/** vm0.c - Core module of VM0
 *
 * Copyright (C) 2023. Hattori, Hiroki. All rights reserved.
 * This file is released under MIT license.  see LICENSE for more detail.
 *
 */
#include <vm0.h>
#if !(defined(VM0_ENABLE_DEBUG) && VM0_ENABLE_DEBUG)
#define NDEBUG
#endif
#include <assert.h>
#include <memory.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// //////////////////////////////////////////////////////////////////////////
static struct {
  struct timespec startup_time;
  unsigned int n_IR_objects;
  unsigned int n_runs_gc;
  unsigned int n_active_objects;
} vm0_stat;

static inline void vm0_initialize_stat() {
  clock_gettime(CLOCK_MONOTONIC, &vm0_stat.startup_time);
  vm0_stat.n_IR_objects = 0;
  vm0_stat.n_runs_gc = 0;
  vm0_stat.n_active_objects = 0;
}

void vm0_show_stat() {
  fprintf(stderr, "startup time      : %ld,%06ld\n", vm0_stat.startup_time.tv_sec, vm0_stat.startup_time.tv_nsec);
  fprintf(stderr, "num IR objects    : %d\n", vm0_stat.n_IR_objects);
  fprintf(stderr, "num runs GC       : %d\n", vm0_stat.n_runs_gc);
  fprintf(stderr, "num active objects: %d\n", vm0_stat.n_active_objects);
}

// //////////////////////////////////////////////////////////////////////////
static void vm0_gc_paint_gray_all(vm0_value_t *p, size_t n);
static void vm0_obj_visit(struct vm0_obj_hdr *obj, void (*proc)(vm0_value_t child, void *ctxt), void *ctxt);

static void vm0_release_integer(vm0_value_t obj) {
  struct vm0_obj_integer *p = vm0_value_to_objref(integer, obj);
  for (int i = 0; i < p->hdr.capacity; ++i) mpz_clear(p->i[i]);
}

static void vm0_release_child(vm0_value_t obj, void * /*ctxt*/) { vm0_obj_unref(obj); }
static void vm0_release_children(vm0_value_t obj) {
  vm0_obj_visit(vm0_value_to_objref(hdr, obj), vm0_release_child, NULL);
}

static void vm0_visitt_obarray(vm0_value_t obj, void (*proc)(vm0_value_t, void *), void *ctxt) {
  struct vm0_obj_obarray *p = vm0_value_to_objref(obarray, obj);
  for (int i = 0; i < p->hdr.capacity; ++i) proc(p->arr[i], ctxt);
}

static void vm0_visit_data(vm0_value_t obj, void (*proc)(vm0_value_t, void *), void *ctxt) {
  struct vm0_obj_data *p = vm0_value_to_objref(data, obj);
  for (int i = 0; i < p->hdr.capacity; ++i) proc(p->args[i], ctxt);
}

static void vm0_visit_closure(vm0_value_t obj, void (*proc)(vm0_value_t, void *), void *ctxt) {
  struct vm0_obj_closure *p = vm0_value_to_objref(closure, obj);
  for (int i = 0; i < p->hdr.capacity; ++i) proc(p->args[i], ctxt);
}

static struct {
  void (*releaseproc)(vm0_value_t obj);
  void (*visit)(vm0_value_t obj, void (*proc)(vm0_value_t, void *), void *ctxt);
  uint8_t basesize;
  uint8_t unitsize;
} const vm0_object_info[] = {
    [VM0_OBJ_TYPE_INT] = {.releaseproc = NULL,
                          .visit = NULL,
                          .basesize = sizeof(struct vm0_obj_int),
                          .unitsize = sizeof(int)},
    [VM0_OBJ_TYPE_INT64] = {.releaseproc = NULL,
                            .visit = NULL,
                            .basesize = sizeof(struct vm0_obj_int64),
                            .unitsize = sizeof(int64_t)},
    [VM0_OBJ_TYPE_INT32] = {.releaseproc = NULL,
                            .visit = NULL,
                            .basesize = sizeof(struct vm0_obj_int32),
                            .unitsize = sizeof(int32_t)},
    [VM0_OBJ_TYPE_INT16] = {.releaseproc = NULL,
                            .visit = NULL,
                            .basesize = sizeof(struct vm0_obj_int16),
                            .unitsize = sizeof(int16_t)},
    [VM0_OBJ_TYPE_INT8] = {.releaseproc = NULL,
                           .visit = NULL,
                           .basesize = sizeof(struct vm0_obj_int8),
                           .unitsize = sizeof(int8_t)},
    [VM0_OBJ_TYPE_UINT] = {.releaseproc = NULL,
                           .visit = NULL,
                           .basesize = sizeof(struct vm0_obj_uint),
                           .unitsize = sizeof(unsigned int)},
    [VM0_OBJ_TYPE_UINT64] = {.releaseproc = NULL,
                             .visit = NULL,
                             .basesize = sizeof(struct vm0_obj_uint64),
                             .unitsize = sizeof(uint64_t)},
    [VM0_OBJ_TYPE_UINT32] = {.releaseproc = NULL,
                             .visit = NULL,
                             .basesize = sizeof(struct vm0_obj_uint32),
                             .unitsize = sizeof(uint32_t)},
    [VM0_OBJ_TYPE_UINT16] = {.releaseproc = NULL,
                             .visit = NULL,
                             .basesize = sizeof(struct vm0_obj_uint16),
                             .unitsize = sizeof(uint16_t)},
    [VM0_OBJ_TYPE_UINT8] = {.releaseproc = NULL,
                            .visit = NULL,
                            .basesize = sizeof(struct vm0_obj_uint8),
                            .unitsize = sizeof(uint8_t)},
    [VM0_OBJ_TYPE_DOUBLE] = {.releaseproc = NULL,
                             .visit = NULL,
                             .basesize = sizeof(struct vm0_obj_double),
                             .unitsize = sizeof(double)},
    [VM0_OBJ_TYPE_FLOAT] = {.releaseproc = NULL,
                            .visit = NULL,
                            .basesize = sizeof(struct vm0_obj_float),
                            .unitsize = sizeof(float)},
    [VM0_OBJ_TYPE_INTEGER] = {.releaseproc = vm0_release_integer,
                              .visit = NULL,
                              .basesize = sizeof(struct vm0_obj_integer),
                              .unitsize = sizeof(mpz_t)},
    [VM0_OBJ_TYPE_OBJ] = {.releaseproc = vm0_release_children,
                          .visit = vm0_visitt_obarray,
                          .basesize = sizeof(struct vm0_obj_obarray),
                          .unitsize = sizeof(vm0_value_t)},
    [VM0_OBJ_TYPE_DATA] = {.releaseproc = vm0_release_children,
                           .visit = vm0_visit_data,
                           .basesize = sizeof(struct vm0_obj_data),
                           .unitsize = sizeof(vm0_value_t)},
    [VM0_OBJ_TYPE_CLOSURE] = {.releaseproc = vm0_release_children,
                              .visit = vm0_visit_closure,
                              .basesize = sizeof(struct vm0_obj_closure),
                              .unitsize = sizeof(vm0_value_t)},
    [VM0_OBJ_TYPE_BUFFER] = {.releaseproc = NULL,
                             .visit = NULL,
                             .basesize = sizeof(struct vm0_obj_buffer),
                             .unitsize = sizeof(uint8_t)},
};

#if defined(VM0_ENABLE_DEBUG) && VM0_ENABLE_DEBUG
#define vm0_assert_object_type(typ) \
  do { assert((typ) >= 0 && (ty) < (sizeof(vm0_object_info) / sizeof(vm0_object_info[0]))); } while (0)
#else

#define vm0_assert_obj_type(typ) \
  do {                           \
  } while (0)

#endif

/** Allocate memory for new object.
 * @param typ       Type of new object.
 * @param capacity  Containner capacity.
 */
vm0_value_t vm0_allocate_objmem(uint8_t typ, uint32_t capacity) {
  assert(capacity > 0);
  vm0_assert_obj_type(typ);

  size_t unit = vm0_object_info[typ].unitsize;
  size_t n = vm0_object_info[typ].basesize + unit * capacity;
  ++vm0_stat.n_active_objects;

  struct vm0_obj_hdr *p = malloc(n);
  p->capacity = capacity;
  p->refcount = 1;
  p->type = typ;
  p->color = VM0_OBJ_COLOR_BLACK;

  return vm0_value_from_objref(p);
}

static void vm0_obj_visit(struct vm0_obj_hdr *obj, void (*proc)(vm0_value_t child, void *ctxt), void *ctxt) {
  if (vm0_object_info[obj->type].visit != NULL)
    vm0_object_info[obj->type].visit(vm0_value_from_objref(obj), proc, ctxt);
}

// //////////////////////////////////////////////////////////////////////////
// garbage collector

static uint64_t vm0_gc_purple_object_flags;
static struct vm0_obj_hdr *vm0_gc_purple_objects[64];

static void vm0_gc_paint_gray(vm0_value_t objs, void * /* ctxt */) {
  struct vm0_obj_hdr *p = vm0_value_to_objref(hdr, objs);
  if (p->color != VM0_OBJ_COLOR_IR && p->color != VM0_OBJ_COLOR_GREEN) {
    --p->refcount;
    if (p->color != VM0_OBJ_COLOR_GRAY) {
      p->color = VM0_OBJ_COLOR_GRAY;
      vm0_obj_visit(p, vm0_gc_paint_gray, NULL);
    }
  }
}

static void vm0_gc_paint_black(vm0_value_t objs, void * /* ctxt */) {
  struct vm0_obj_hdr *p = vm0_value_to_objref(hdr, objs);
  if (p->color == VM0_OBJ_COLOR_GRAY) {
    p->color = VM0_OBJ_COLOR_BLACK;
    ++p->refcount;
    vm0_obj_visit(p, vm0_gc_paint_black, NULL);
  }
}

static void vm0_gc_unpaint_purple(struct vm0_obj_hdr *obj) {
  obj->color = VM0_OBJ_COLOR_BLACK;
  for (int i = 0; i < 64; ++i) {
    if (vm0_gc_purple_objects[i] == obj) vm0_gc_purple_objects[i] = NULL;
    vm0_gc_purple_object_flags &= ~(1ull << i);
  }
}

/** Increase reference counter.
 *
 */
vm0_value_t vm0_obj_addref(vm0_value_t obj) {
  if (vm0_value_type(obj) == VM0_VALUE_TYPE_OBJREF) {
    struct vm0_obj_hdr *p = vm0_value_to_objref(hdr, obj);
    if (p->color != VM0_OBJ_COLOR_GREEN && p->color != VM0_OBJ_COLOR_IR) {
      if (p->color != VM0_OBJ_COLOR_PURPLE) vm0_gc_unpaint_purple(p);
      if (++p->refcount == 0) {
        p->color = VM0_OBJ_COLOR_IR;
        ++vm0_stat.n_IR_objects;
      }
    }
  }
  return obj;
}

void vm0_obj_unref(vm0_value_t obj) {
  struct vm0_obj_hdr *p = vm0_value_to_objref(hdr, obj);
  if (p->color == VM0_OBJ_COLOR_GREEN || p->color == VM0_OBJ_COLOR_IR) return;
  if (--p->refcount == 0) {
    if (p->color == VM0_OBJ_COLOR_PURPLE) vm0_gc_unpaint_purple(p);
    vm0_object_info[p->type].releaseproc(obj);
    free(p);
  } else if (p->color == VM0_OBJ_COLOR_BLACK) {
    //
    // Paint the object purple.
    //
    p->color = VM0_OBJ_COLOR_PURPLE;
    unsigned int n = __builtin_ffs(vm0_gc_purple_object_flags);
    if (n < 64) {
      vm0_gc_purple_object_flags &= ~(1ul << n);
      vm0_gc_purple_objects[n] = p;

      if (vm0_gc_purple_object_flags == 0) {
        // It seems need GC.
        // Paint the purple objects gray
        for (int i = 0; i < 64; ++i) {
          struct vm0_obj_hdr *p = vm0_gc_purple_objects[i];
          if (p != NULL && p->color == VM0_OBJ_COLOR_PURPLE) {
            p->color = VM0_OBJ_COLOR_GRAY;
            vm0_obj_visit(p, vm0_gc_paint_gray, NULL);

            // Paint objects black.
            bool is_cyclic = p->refcount == 0;
            p->color = VM0_OBJ_COLOR_BLACK;
            ++p->refcount;
            vm0_obj_visit(p, vm0_gc_paint_black, NULL);

            if (is_cyclic) vm0_obj_unref(vm0_value_from_objref(p));
          }
          vm0_gc_purple_object_flags |= (1ul << i);
          vm0_gc_purple_objects[i] = NULL;
        }
      }
    }
  }
}

// //////////////////////////////////////////////////////////////////////////
// Boxed primitive number

#define VM0_GEN_ALLOCATOR(objtyp, typ, primtyp, mbr)                         \
  vm0_value_t vm0_new_##objtyp##_array(primtyp initval, uint32_t capacity) { \
    vm0_value_t r = vm0_allocate_objmem(typ, capacity);                      \
    primtyp *p = vm0_value_to_objref(objtyp, r)->mbr;                        \
    while (capacity--) *(p++) = initval;                                     \
    return r;                                                                \
  }                                                                          \
  vm0_value_t vm0_new_##objtyp(primtyp initval) { return vm0_new_##objtyp##_array(initval, 1); }

VM0_GEN_ALLOCATOR(int, VM0_OBJ_TYPE_INT, int, i)
VM0_GEN_ALLOCATOR(uint, VM0_OBJ_TYPE_UINT, unsigned int, ui)
VM0_GEN_ALLOCATOR(int64, VM0_OBJ_TYPE_INT64, int64_t, i64)
VM0_GEN_ALLOCATOR(uint64, VM0_OBJ_TYPE_UINT64, uint64_t, ui64)
VM0_GEN_ALLOCATOR(int32, VM0_OBJ_TYPE_INT32, int32_t, i32)
VM0_GEN_ALLOCATOR(uint32, VM0_OBJ_TYPE_UINT32, uint32_t, ui32)
VM0_GEN_ALLOCATOR(int16, VM0_OBJ_TYPE_INT16, int16_t, i16)
VM0_GEN_ALLOCATOR(uint16, VM0_OBJ_TYPE_UINT16, uint16_t, ui16)
VM0_GEN_ALLOCATOR(int8, VM0_OBJ_TYPE_INT16, int8_t, i8)
VM0_GEN_ALLOCATOR(uint8, VM0_OBJ_TYPE_UINT16, uint8_t, ui8)
VM0_GEN_ALLOCATOR(double, VM0_OBJ_TYPE_DOUBLE, double, dbl)
VM0_GEN_ALLOCATOR(float, VM0_OBJ_TYPE_FLOAT, float, flt)

// //////////////////////////////////////////////////////////////////////////
// Integer

vm0_value_t vm0_new_integer_array_with_zero(uint32_t capacity) {
  vm0_value_t r = vm0_allocate_objmem(VM0_OBJ_TYPE_INTEGER, capacity);
  mpz_t *p = vm0_value_to_objref(integer, r)->i;
  while (capacity--) mpz_init(*(p++));
}
vm0_value_t vm0_new_integer_with_zero(uint32_t capacity) { return vm0_new_integer_array_with_zero(1); }

vm0_value_t vm0_new_integer_array_with_int(int initval, uint32_t capacity) {
  vm0_value_t r = vm0_allocate_objmem(VM0_OBJ_TYPE_INTEGER, capacity);
  mpz_t *p = vm0_value_to_objref(integer, r)->i;
  while (capacity--) mpz_init_set_si(*(p++), initval);
}
vm0_value_t vm0_new_integer_with_int(int initval) { return vm0_new_integer_array_with_int(initval, 1); }

vm0_value_t vm0_new_integer_array_with_str(char const *initval, uint32_t capacity) {
  vm0_value_t r = vm0_allocate_objmem(VM0_OBJ_TYPE_INTEGER, capacity);
  mpz_t *p = vm0_value_to_objref(integer, r)->i;
  mpz_t *q = p;

  mpz_init_set_str(*(p++), initval, 10);
  while (--capacity) mpz_init_set(*(p++), *q);
  return r;
}
vm0_value_t vm0_new_integer_with_str(char const *initval) { return vm0_new_integer_array_with_str(initval, 1); }

// //////////////////////////////////////////////////////////////////////////
// Array of object or IORef

vm0_value_t vm0_new_obarray(vm0_value_t initval, uint32_t capacity) {
  vm0_value_t r = vm0_allocate_objmem(VM0_OBJ_TYPE_OBJ, capacity);
  vm0_value_t *p = vm0_value_to_objref(obarray, r);
  if (vm0_value_type(initval) == VM0_VALUE_TYPE_OBJREF)
    while (capacity--) *(p++) = vm0_obj_addref(initval);
  else
    while (capacity--) *(p++) = initval;
}

vm0_value_t vm0_new_ioref(vm0_value_t initval) { return vm0_new_obarray(initval, 1); }

// //////////////////////////////////////////////////////////////////////////
// Constructor and argments.

vm0_value_t vm0_new_data(struct vm0_constructor const *constr, uint32_t nargs) {
  vm0_value_t r = vm0_allocate_objmem(VM0_OBJ_TYPE_DATA, nargs);
  struct vm0_obj_data *p = vm0_value_to_objref(data, r);
  p->constructor = constr;
}

// //////////////////////////////////////////////////////////////////////////
// Closure

vm0_value_t vm0_new_closure(struct vm0_function const *fnc, uint32_t nargs) {
  vm0_value_t r = vm0_allocate_objmem(VM0_OBJ_TYPE_CLOSURE, nargs);
  struct vm0_obj_closure *p = vm0_value_to_objref(closure, r);
  p->fnc = fnc;
  return r;
}

vm0_value_t vm0_expand_closure(vm0_value_t clos, uint32_t nargs) {
  struct vm0_obj_closure *orig = vm0_value_to_objref(closure, clos);

  vm0_value_t r = vm0_new_closure(orig->fnc, orig->hdr.capacity + nargs);
  struct vm0_obj_closure *p = vm0_value_to_objref(closure, r);
  for (int i = 0; i < orig->hdr.capacity; i++) p->args[i] = vm0_obj_addref(orig->args[i]);

  return r;
}

vm0_value_t vm0_reduce_closure(vm0_value_t clos) {
  struct vm0_obj_closure *p = vm0_value_to_objref(closure, clos);
  while (p->hdr.capacity >= vm0_fnc_arity(p->fnc)) {
    //
  }
}

// //////////////////////////////////////////////////////////////////////////
void vm0_initialize() {
  vm0_initialize_stat();
  vm0_gc_purple_object_flags = (uint64_t)(-1ll);
  for (int i = 0; i < 64; ++i) vm0_gc_purple_objects[i] = NULL;
}
