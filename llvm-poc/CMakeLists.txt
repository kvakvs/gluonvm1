project(llvm-gluon CXX C)
cmake_minimum_required(VERSION 2.8)

#find_package (Threads)
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

execute_process(COMMAND llvm-config-3.8 --cppflags OUTPUT_VARIABLE LLVM_DEFINITIONS)

#SET (CMAKE_C_COMPILER   "clang-3.8")
#SET (CMAKE_CXX_COMPILER "clang++-3.8")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

include_directories(
    ${CMAKE_SOURCE_DIR}/src/
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
#add_definitions(#-m32
#    -Weverything #-Werror
#    -Wno-c++98-compat-pedantic -Wno-missing-noreturn
#    -Wno-c++98-compat -Wno-padded -Wno-format-nonliteral
#    -Wno-old-style-cast -Wno-exit-time-destructors -Wno-global-constructors
#    -Wno-unused-parameter
#    -Wno-gnu-label-as-value -Wno-unused-label
#    )
set(CMAKE_CXX_FLAGS "-std=c++14 ${CMAKE_CXX_FLAGS}")
set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS}
    -std=c99
    )
set(SRC_LIST
    src/jit.h
    src/codegen.cpp
    )

add_executable(${PROJECT_NAME} ${SRC_LIST} ${SRC_LIBTOMMATH})
## Enable this to get 32-bit build
## Remember to install gcc-multilib and g++-multilib and libc6-{dev-}i386
#set_target_properties(${PROJECT_NAME} PROPERTIES
#                      COMPILE_FLAGS "-m32" LINK_FLAGS "-m32")

set(G_LINK_LIBS m dl)
# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core mcjit native)

target_link_libraries(${PROJECT_NAME} ${CMAKE_THREAD_LIBS_INIT}
                      ${G_LINK_LIBS} ${llvm_libs}
                      )
