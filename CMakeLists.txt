cmake_minimum_required(VERSION 3.13)
project(vm0 C)

set(VM0_VERSION_MAJOR 0)
set(VM0_VERSION_MINOR 1)
set(VM0_VERSION_PATCHLEVEL 0)
option(VM0_ENABLE_DEBUG "Enable debug output to stderr." OFF)

configure_file(
    ${PROJECT_SOURCE_DIR}/src/vm0.config.h.in
    ${PROJECT_BINARY_DIR}/vm0.config.h
    )

add_library(vm0 STATIC src/vm0.c)
set_property(TARGET vm0 PROPERTY C_STANDARD 23)
target_include_directories(vm0
  PUBLIC include/
  PUBLIC ${PROJECT_BINARY_DIR}/)


