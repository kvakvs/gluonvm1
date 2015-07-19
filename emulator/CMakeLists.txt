project(gluon CXX C)
cmake_minimum_required(VERSION 2.8)

#find_package (Threads)

#SET (CMAKE_C_COMPILER   "clang")
#SET (CMAKE_CXX_COMPILER "clang++")

add_subdirectory(libgluon_sys)

include_directories(
    #${CMAKE_SOURCE_DIR}/generated/
    ${CMAKE_SOURCE_DIR}/include/
    ${CMAKE_SOURCE_DIR}/src/
    )
add_definitions(
    -O0 -g -ggdb
    #-Os
    -Weverything -Werror
    -Wno-c++98-compat-pedantic -Wno-missing-noreturn
    -Wno-c++98-compat -Wno-padded -Wno-format-nonliteral
    -Wno-old-style-cast -Wno-exit-time-destructors -Wno-global-constructors
    -Wno-unused-parameter
    -Wno-gnu-label-as-value -Wno-unused-label
    )
set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS}
    -std=c++11
    )
set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS}
    -std=c99
    )
set(SRC_LIST
    include/g_defs.h
    include/g_error.h
    include/g_reader.h
    include/g_sys_fs.h
    include/g_sys_mem.h
    include/g_sys_stdlib.h

    src/g_code.cpp       include/g_code.h
    src/g_codeserver.cpp include/g_codeserver.h
    src/g_dist.cpp       include/g_dist.h
    src/g_ext_term.cpp   include/g_ext_term.h
    src/g_genop.h # generate using Makefile in codegen/
    src/g_gleam_loader.cpp
    src/g_heap.cpp       include/g_heap.h
    src/g_main.cpp
    src/g_module.cpp     include/g_module.h
    src/g_process.cpp    include/g_process.h
    src/g_term.cpp       include/g_term.h
    src/g_vm.cpp         include/g_vm.h
    src/g_vm_loop.cpp
    src/g_vm_impl.h # because all stuff inline here

    src/bif/g_bif_misc.cpp include/bif/g_bif_misc.h

    # Header-only test framework
    src/fructose/fructose.h
    src/fructose/test_base.h
    src/fructose/test_root.h
    src/fructose/double_compare.h
    )
set(SRC_LIBTOMMATH
#    deps/libtommath/tommath.h
    )

#SET_SOURCE_FILES_PROPERTIES(src/beam/bw_beam_load.cpp PROPERTIES COMPILE_FLAGS -emit-llvm)

add_executable(${PROJECT_NAME} ${SRC_LIST} ${SRC_LIBTOMMATH})

set(G_LINK_LIBS m dl z)
target_link_libraries(${PROJECT_NAME} ${CMAKE_THREAD_LIBS_INIT}
                      ${G_LINK_LIBS}
                      gluon_sys)
