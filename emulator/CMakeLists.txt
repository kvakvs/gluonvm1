project(gluon CXX C)
cmake_minimum_required(VERSION 2.8)

#find_package (Threads)

SET (CMAKE_C_COMPILER   "clang")
SET (CMAKE_CXX_COMPILER "clang++")

include_directories(
    ${CMAKE_SOURCE_DIR}/include/
    ${CMAKE_SOURCE_DIR}/src/
    ${CMAKE_SOURCE_DIR}/test/
    )

# Enables lot of Std::fmt debugging features as well as assertions everywhere in
# the code. Makes things run slow.
add_definitions(-DG_DEBUG=1)

set(G_TEST 0)

if(G_TEST)
  add_definitions(-DG_TEST=1) # to run tests
else()
  add_definitions(-DG_TEST=0 -fno-rtti) # normal compilation no test
endif()

add_definitions(-O0 -g -ggdb)
#add_definitions(-Os)
# optimize for size AND generate asm, build will fail though
#add_definitions(-S -g -Os) # generate asm in .o files (bad file extension)
#add_definitions(-S -Os) # same but no debug
add_definitions(#-m32
    -Weverything #-Werror
    -Wno-c++98-compat-pedantic -Wno-missing-noreturn
    -Wno-c++98-compat -Wno-padded -Wno-format-nonliteral
    -Wno-old-style-cast -Wno-exit-time-destructors -Wno-global-constructors
    -Wno-unused-parameter
    -Wno-gnu-label-as-value -Wno-unused-label
    )
#-fno-exceptions -fno-rtti
set(CMAKE_CXX_FLAGS "-std=c++14 ${CMAKE_CXX_FLAGS}")
set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS}
    -std=c99
    )
set(SRC_LIST
    include/FEATURE.h   # edit this to configure stuff
    include/defs.h
    include/error.h
    include/functional.h

    include/struct/array.h
    include/struct/dict.h
    include/struct/list.h
    include/struct/str.h
    include/struct/set.h

    src/beam_loader.cpp       src/beam.h
    src/beam_prepare.cpp

    src/bif_tab.cpp           src/bif_tab.h
    src/binary.cpp            include/binary.h
    src/code.cpp              include/code.h
    src/code_index.cpp        include/code_index.h
    src/code_server.cpp       include/code_server.h
    src/dist.cpp              include/dist.h
    src/ext_term.cpp          include/ext_term.h
    src/genop.cpp             include/genop.h        # generated via codegen/ script
    src/fun.cpp               include/fun.h
    src/heap.cpp              include/heap.h
    src/mailbox.cpp           include/mailbox.h
    src/main.cpp
    src/module.cpp            include/module.h
    src/pointer.h
    src/predef_atoms.cpp include/predef_atoms.h
    src/process.cpp           include/process.h
    src/process_fail.cpp      include/process_fail.h
    src/reader.h
    src/runtime_ctx.cpp       src/runtime_ctx.h
    src/scheduler.cpp         include/scheduler.h
    src/stack.cpp             src/stack.h
    src/term.cpp              include/term.h
    src/term_helpers.cpp      include/term_helpers.h
    src/term_layout.cpp       src/term_layout.h
    src/term_tag.h
    src/vm.cpp                include/vm.h
    src/vm_loop.cpp
    src/vm_ctx.cpp            src/vm_ctx.h              src/vm_impl.h
    src/wrap.h

    src/bif/bif_misc.cpp include/bif/bif_misc.h
    src/bif/bif_proc.cpp include/bif/bif_proc.h

    # Zlib replacement
    src/miniz/miniz.cpp

    include/gsl/gsl.h include/gsl/array_view.h include/gsl/fail_fast.h

    src/platf/gsys_file.cpp    src/platf/gsys_file.h
    src/platf/gsys_stdlib.cpp  src/platf/gsys_stdlib.h
    src/platf/gsys_mem.cpp     src/platf/gsys_mem.h
    )

if(G_TEST)
  set(SRC_LIST ${SRC_LIST}
      test/test.cpp test/test.h
      test/test_code_index.cpp
      test/test_process.cpp
      test/test_term.cpp

      # Header-only test framework
      test/fructose/fructose.h
      test/fructose/test_base.h
      test/fructose/test_root.h
      test/fructose/double_compare.h
      )
endif()

set(SRC_LIBTOMMATH
#    deps/libtommath/tommath.h
    )

#SET_SOURCE_FILES_PROPERTIES(src/beam/bw_beam_load.cpp PROPERTIES COMPILE_FLAGS -emit-llvm)

add_executable(${PROJECT_NAME} ${SRC_LIST} ${SRC_LIBTOMMATH})
## Enable this to get 32-bit build
## Remember to install gcc-multilib and g++-multilib and libc6-{dev-}i386
#set_target_properties(${PROJECT_NAME} PROPERTIES
#                      COMPILE_FLAGS "-m32" LINK_FLAGS "-m32")

set(G_LINK_LIBS m dl)
target_link_libraries(${PROJECT_NAME} ${CMAKE_THREAD_LIBS_INIT}
                      ${G_LINK_LIBS}
                      )
