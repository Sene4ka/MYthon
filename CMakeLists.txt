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
        ${SRC_DIR}/core/vm.c
        ${SRC_DIR}/runtime/native.c
        ${SRC_DIR}/cli/cli.c
        ${SRC_DIR}/core/debug.c
        ${SRC_DIR}/core/compiler.c
        ${SRC_DIR}/cli/repl.c
)

set(HEADER_FILES
        ${SRC_DIR}/core/lexer.h
        ${SRC_DIR}/core/parser.h
        ${SRC_DIR}/utils/memory.h
        ${SRC_DIR}/core/bytecode.h
        ${SRC_DIR}/core/ast.h
        ${SRC_DIR}/core/compiler.h
        ${SRC_DIR}/core/vm.h
        ${SRC_DIR}/runtime/native.h
        ${SRC_DIR}/cli/cli.h
        ${SRC_DIR}/cli/repl.h
        ${SRC_DIR}/core/debug.h
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

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(mython PRIVATE
            -g -O0 -Wall -Wextra -Werror -pedantic
    )
    target_compile_definitions(mython PRIVATE DEBUG=1)
else()
    target_compile_options(mython PRIVATE -O2)
endif()
