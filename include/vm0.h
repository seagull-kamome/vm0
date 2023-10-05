/** vm0.h - Header file of VM0
 *
 * Copyright (C) 2023. Hattori, Hiroki. All rights reserved.
 * This file is released under MIT license.  see LICENSE for more detail.
 *
 */
#pragma once
#if !defined(__INCLUDE__VM0__H__)
#define __INCLUDE__VM0__H__

#include <gmp.h>
#include <stdint.h>

#include "vm0.config.h"

// //////////////////////////////////////////////////////////////////////////
void vm0_initialize();

// //////////////////////////////////////////////////////////////////////////
typedef union {
  uintptr_t boxed;
} vm0_value_t;

union vm0_float_and_uint32_tag {
  uint32_t ui32;
  float flt;
};

#define VM0_VALUE_TYPE_MASK (0x07)
#define VM0_VALUE_TYPE_OBJREF (0)
#define VM0_VALUE_TYPE_ARITH (1)
#define VM0_VALUE_TYPE_CONSTR (2)
#define VM0_VALUE_TYPE_FUNCTION (3)
#define VM0_VALUE_TYPE_CLOSURE (3)

#define vm0_value_type(x) (x.boxed & VM0_VALUE_TYPE_MASK)
#define vm0_value_to_objref(typ, x) ((struct vm0_obj_##typ *)((x).boxed))
#define vm0_value_to_int(w, x) ((int##w##_t)((x).boxed >> 32))
#define vm0_value_to_uint(w, x) ((uint##w##_t)((x).boxed >> 32))
#define vm0_value_to_float(x)  ({ union { uint32_t u32; float flt; } tmp = vm0_value_to uint(32, x); tmp.flt; } \
  }
#define vm0_value_from_objref(x) ((vm0_value_t)((uintptr_t)(x)))
#define vm0_value_from_int(x) ((vm0_value_t)((((uintptr_t)(x)) << 32) | VM_VALUE_TYPE_ARITH))
#define vm0_value_from_uint(x) ((vm0_value_t)((((uintptr_t)(x)) << 32) | VM_VALUE_TYPE_ARITH))
#define vm0_value_from_float(x)   \
  ({                              \
    union {                       \
      float flt;                  \
      uint32_t u32;               \
    } tmp = (x);                  \
    vm0_value_from_uint(tmp.u32); \
  })

// //////////////////////////////////////////////////////////////////////////
struct vm0_constructor {
  char const *name;
  uint16_t tag;
  uint16_t reserved_0;
  uint8_t reserved_1;
  uint8_t arity;
};

struct vm0_function {
  vm0_value_t (*proc)();
  uint32_t arg_boxing;  // when bitN is 1, argN is objref.
  uint16_t reserved_0;
  uint8_t flags;  // bit0..4 indicate arity. when bit7 is 1,result is boxed.
};
#define vm0_fnc_arity(x) ((x)->flags & 0x1f)

// //////////////////////////////////////////////////////////////////////////

// Header part of all objects.
struct vm0_obj_hdr {
  uint32_t capacity;  // Capacities of payload.
  uint16_t refcount;  // Reference count for GC.
  uint8_t type;       // Object type.
#define VM0_OBJ_TYPE_INT (0)
#define VM0_OBJ_TYPE_INT64 (1)
#define VM0_OBJ_TYPE_INT32 (2)
#define VM0_OBJ_TYPE_INT16 (3)
#define VM0_OBJ_TYPE_INT8 (4)
#define VM0_OBJ_TYPE_UINT (5)
#define VM0_OBJ_TYPE_UINT64 (6)
#define VM0_OBJ_TYPE_UINT32 (7)
#define VM0_OBJ_TYPE_UINT16 (8)
#define VM0_OBJ_TYPE_UINT8 (9)
#define VM0_OBJ_TYPE_DOUBLE (10)
#define VM0_OBJ_TYPE_FLOAT (11)
#define VM0_OBJ_TYPE_INTEGER (12)
#define VM0_OBJ_TYPE_OBJ (13)
#define VM0_OBJ_TYPE_DATA (14)
#define VM0_OBJ_TYPE_CLOSURE (15)
#define VM0_OBJ_TYPE_BUFFER (16)
#define VM0_OBJ_TYPE_GC_PTR (17)
// TODO: slice

  uint8_t color;
#define VM0_OBJ_COLOR_BLACK (0 /* Normal actrive cell */)
#define VM0_OBJ_COLOR_PURPLE (1 /* Partial mark & sweep required. */)
#define VM0_OBJ_COLOR_GRAY (2 /* Partial marked. */)
#define VM0_OBJ_COLOR_GREEN (3 /* Stock object. Never free the memory. */)
#define VM0_OBJ_COLOR_IR (4 /* Sticky object cause refcnt overflowed. */)
};

vm0_value_t vm0_allocate_objmem(uint8_t typ, uint32_t capacity);
vm0_value_t vm0_obj_addref(vm0_value_t obj);
void vm0_obj_unref(vm0_value_t obj);

// //////////////////////////////////////////////////////////////////////////
struct vm0_obj_int {
  struct vm0_obj_hdr hdr;
  int i[];
};
vm0_value_t vm0_new_int_array(int initval, uint32_t capacity);
vm0_value_t vm0_new_int(int initval);

struct vm0_obj_uint {
  struct vm0_obj_hdr hdr;
  unsigned int ui[];
};
vm0_value_t vm0_new_uint_array(unsigned int initval, uint32_t capacity);
vm0_value_t vm0_new_uint(unsigned int initval);

struct vm0_obj_int64 {
  struct vm0_obj_hdr hdr;
  int64_t i64[];
};
vm0_value_t vm0_new_int64_array(int64_t initval, uint32_t capacity);
vm0_value_t vm0_new_int64(int64_t initval);

struct vm0_obj_uint64 {
  struct vm0_obj_hdr hdr;
  uint64_t ui64[];
};
vm0_value_t vm0_new_uint64_array(uint64_t initval, uint32_t capacity);
vm0_value_t vm0_new_uint64(uint64_t initval);

struct vm0_obj_int32 {
  struct vm0_obj_hdr hdr;
  int32_t i32[];
};
vm0_value_t vm0_new_int32_array(int32_t initval, uint32_t capacity);
vm0_value_t vm0_new_int32(int32_t initval);

struct vm0_obj_uint32 {
  struct vm0_obj_hdr hdr;
  uint32_t ui32[];
};
vm0_value_t vm0_new_uint32_array(uint32_t initval, uint32_t capacity);
vm0_value_t vm0_new_uint32(uint32_t initval);

struct vm0_obj_int16 {
  struct vm0_obj_hdr hdr;
  int16_t i16[];
};
vm0_value_t vm0_new_int16_array(int16_t initval, uint32_t capacity);
vm0_value_t vm0_new_int16(int16_t initval);

struct vm0_obj_uint16 {
  struct vm0_obj_hdr hdr;
  uint16_t ui16[];
};
vm0_value_t vm0_new_uint16_array(uint16_t initval, uint32_t capacity);
vm0_value_t vm0_new_uint16(uint16_t initval);

struct vm0_obj_int8 {
  struct vm0_obj_hdr hdr;
  int8_t i8[];
};
vm0_value_t vm0_new_int8_array(int8_t initval, uint32_t capacity);
vm0_value_t vm0_new_int8(int8_t initval);

struct vm0_obj_uint8 {
  struct vm0_obj_hdr hdr;
  uint8_t ui8[];
};
vm0_value_t vm0_new_uint8_array(uint8_t initval, uint32_t capacity);
vm0_value_t vm0_new_uint8(uint8_t initval);

struct vm0_obj_double {
  struct vm0_obj_hdr hdr;
  double dbl[];
};
vm0_value_t vm0_new_double_array(double initval, uint32_t capacity);
vm0_value_t vm0_new_double(double initval);

struct vm0_obj_float {
  struct vm0_obj_hdr hdr;
  float flt[];
};
vm0_value_t vm0_new_float_array(float initval, uint32_t capacity);
vm0_value_t vm0_new_float(float initval);

struct vm0_obj_integer {
  struct vm0_obj_hdr hdr;
  mpz_t i[];
};
vm0_value_t vm0_new_integer_array_with_zero(uint32_t capcacity);
vm0_value_t vm0_new_integer_array_with_int(int initval, uint32_t capacity);
vm0_value_t vm0_new_integer_array_with_str(char const *initval, uint32_t capacity);

vm0_value_t vm0_new_integer_with_zero();
vm0_value_t vm0_new_integer_with_int(int initval);
vm0_value_t vm0_new_integer_with_str(char const *initval);

struct vm0_obj_obarray {
  struct vm0_obj_hdr hdr;
  vm0_value_t arr[];
};
vm0_value_t vm0_new_obarray(vm0_value_t initval, uint32_t capacity);
vm0_value_t vm0_new_ioref(vm0_value_t initval);

struct vm0_obj_data {
  struct vm0_obj_hdr hdr;
  struct vm0_constructor const *constructor;
  vm0_value_t args[];
};
vm0_value_t vm0_new_data(struct vm0_constructor const *constr, uint32_t nargs);

struct vm0_obj_closure {
  struct vm0_obj_hdr hdr;
  struct vm0_function const *fnc;
  vm0_value_t args[];
};
vm0_value_t vm0_new_closure(struct vm0_function const *fnc, uint32_t nargs);
vm0_value_t vm0_expand_closure(vm0_value_t x, uint32_t nargs);
vm0_value_t vm0_reduce_closure(vm0_value_t x);

struct vm0_obj_buffer {
  struct vm0_obj_hdr hdr;
  uint32_t reserved_0;
  uint32_t used;
  uint8_t buff[];
};
vm0_value_t vm0_new_buffer(size_t capacity);

struct vm0_obj_gcptr {
  struct vm0_obj_hdr hdr;
  struct vm0_obj_closure on_release;
  void *ptr[];
};

#endif
