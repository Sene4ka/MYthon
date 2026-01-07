cmake_minimum_required(VERSION 3.10)
project(mython C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

option(DEBUG_MEMORY "Enable memory debugging" OFF)

set(SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)

set(SOURCE_FILES
        ${SRC_DIR}/cli/main.c
        ${SRC_DIR}/core/lexer.c
        ${SRC_DIR}/core/parser.c
        ${SRC_DIR}/utils/memory.c
        ${SRC_DIR}/core/bytecode.c
        ${SRC_DIR}/core/ast.c
        ${SRC_DIR}/runtime/vm.c
        ${SRC_DIR}/runtime/gc.c
        ${SRC_DIR}/runtime/native.c
        ${SRC_DIR}/cli/cli.c
        ${SRC_DIR}/core/debug.c
        ${SRC_DIR}/core/compiler.c
        ${SRC_DIR}/utils/utils.c
)

set(HEADER_FILES
        ${SRC_DIR}/core/lexer.h
        ${SRC_DIR}/core/parser.h
        ${SRC_DIR}/utils/memory.h
        ${SRC_DIR}/core/bytecode.h
        ${SRC_DIR}/core/ast.h
        ${SRC_DIR}/core/compiler.h
        ${SRC_DIR}/runtime/vm.h
        ${SRC_DIR}/runtime/gc.h
        ${SRC_DIR}/runtime/native.h
        ${SRC_DIR}/cli/cli.h
        ${SRC_DIR}/core/debug.h
        ${SRC_DIR}/utils/utils.h
)

add_executable(mython ${SOURCE_FILES} ${HEADER_FILES})

source_group("Source" FILES ${SOURCE_FILES})
source_group("Headers" FILES ${HEADER_FILES})

target_include_directories(mython PRIVATE
        ${SRC_DIR}
        ${SRC_DIR}/core
        ${SRC_DIR}/cli
        ${SRC_DIR}/runtime
        ${SRC_DIR}/utils
)

if(DEBUG_MEMORY)
    target_compile_definitions(mython PRIVATE DEBUG_MEMORY=1)
    message(STATUS "Memory debugging enabled")
endif()

option(ENABLE_SANITIZERS "Enable AddressSanitizer and UndefinedBehaviorSanitizer" OFF)

if(ENABLE_SANITIZERS)
    include(CheckCCompilerFlag)
    check_c_compiler_flag("-fsanitize=address" COMPILER_SUPPORTS_ASAN)
    
    if(COMPILER_SUPPORTS_ASAN)
        set(SANITIZER_FLAGS
            -fsanitize=address
            -fsanitize=undefined
            -fno-omit-frame-pointer
            -fno-optimize-sibling-calls
        )
        message(STATUS "Sanitizers enabled (AddressSanitizer + UBSan)")
        set(COMPILE_WARNINGS -Wall -Wextra -pedantic)
    else()
        message(WARNING "Compiler does not support sanitizers. Install Clang or GCC with sanitizer support.")
        message(WARNING "For Windows: Download LLVM from https://llvm.org/builds/ and set C/C++ compiler to clang")
        set(SANITIZER_FLAGS "")
        set(COMPILE_WARNINGS -Wall -Wextra -Werror -pedantic)
    endif()
else()
    set(SANITIZER_FLAGS "")
    set(COMPILE_WARNINGS -Wall -Wextra -Werror -pedantic)
endif()

target_compile_options(mython PRIVATE -g -O0 ${COMPILE_WARNINGS} ${SANITIZER_FLAGS})
target_compile_definitions(mython PRIVATE DEBUG=1)
target_link_options(mython PRIVATE ${SANITIZER_FLAGS})
