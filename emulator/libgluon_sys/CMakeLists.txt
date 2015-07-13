#
# System abstraction layer for GluonVM
#
project(gluon_sys CXX C)
cmake_minimum_required(VERSION 2.8)

include_directories(
    ${CMAKE_SOURCE_DIR}/include
    )
add_definitions(
    -O0
    -g -ggdb
    )
set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS}
    -std=c++11
    )
set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS}
    -std=c99
    )
set(SRC_LIST
    gsys_file.cpp     gsys_file.h
    gsys_mem.cpp      gsys_mem.h
    gsys_stdlib.cpp   gsys_stdlib.h
    )

add_library(${PROJECT_NAME} ${SRC_LIST})
