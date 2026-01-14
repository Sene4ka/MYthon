cmake_minimum_required(VERSION 3.10)

project(mython C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

find_program(LUA_EXECUTABLE
        NAMES luajit luajit.exe lua.exe lua
        PATHS
        C:/msys64/clang64/bin
        C:/msys64/mingw64/bin
        C:/msys64/usr/bin
        NO_DEFAULT_PATH
)

if(NOT LUA_EXECUTABLE)
    find_program(LUA_EXECUTABLE NAMES luajit lua)
endif()

if(NOT LUA_EXECUTABLE)
    message(FATAL_ERROR "Lua/LuaJIT not found! Install: pacman -S mingw-w64-clang-x86_64-luajit")
endif()

message(STATUS "Found Lua: ${LUA_EXECUTABLE}")

execute_process(
        COMMAND ${LUA_EXECUTABLE} -v
        OUTPUT_VARIABLE LUA_VERSION_STRING
        ERROR_VARIABLE LUA_VERSION_STRING
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_STRIP_TRAILING_WHITESPACE
)
message(STATUS "Lua version: ${LUA_VERSION_STRING}")

option(DEBUG_MEMORY "Enable memory debugging" OFF)

set(SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)

set(DASC_FILE ${CMAKE_CURRENT_SOURCE_DIR}/src/runtime/jit_codegen.dasc)
set(GENERATED_C ${CMAKE_CURRENT_SOURCE_DIR}/src/runtime/jit_codegen.c)

add_custom_command(
        OUTPUT ${GENERATED_C}
        COMMAND ${LUA_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/dynasm/dynasm.lua
        -LN
        -D X64
        -D WIN32=1
        -I ${CMAKE_CURRENT_SOURCE_DIR}/dynasm
        -o ${GENERATED_C}
        ${DASC_FILE}
        DEPENDS ${DASC_FILE}
        COMMENT "Generating jit_codegen.c from DynASM template"
        VERBATIM
)

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
        ${SRC_DIR}/runtime/jit.c
        ${SRC_DIR}/cli/cli.c
        ${SRC_DIR}/core/debug.c
        ${SRC_DIR}/core/compiler.c
        ${SRC_DIR}/utils/utils.c
        ${SRC_DIR}/runtime/jit_vm_bridge.c
        ${GENERATED_C}
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
        ${SRC_DIR}/runtime/jit.h
        ${SRC_DIR}/cli/cli.h
        ${SRC_DIR}/core/debug.h
        ${SRC_DIR}/utils/utils.h
        ${SRC_DIR}/runtime/jit_vm_bridge.h
        ${SRC_DIR}/runtime/jit_codegen.h
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
        ${CMAKE_CURRENT_SOURCE_DIR}/dynasm
)

target_compile_definitions(mython PRIVATE DASM_VERSION=10500)

if(DEBUG_MEMORY)
    target_compile_definitions(mython PRIVATE DEBUG_MEMORY=1)
    message(STATUS "Memory debugging enabled")
endif()

set(ENABLE_SANITIZERS OFF)

if(CMAKE_C_COMPILER MATCHES "clang" OR CMAKE_CXX_COMPILER MATCHES "clang" AND ENABLE_SANITIZERS)
    message(STATUS "Clang DETECTED: ${CMAKE_C_COMPILER}")
    message(STATUS "ASan/UBSan activated")

    target_compile_options(mython PRIVATE
            -fsanitize=address
            -g3 -O0 -fno-omit-frame-pointer
    )

    target_link_libraries(mython
            "C:/msys64/clang64/lib/clang/21/lib/windows/libclang_rt.asan_dynamic-x86_64.dll.a"
    )

    target_link_libraries(mython
            "C:/msys64/clang64/lib/clang/21/lib/windows/libclang_rt.asan_dynamic_runtime_thunk-x86_64.a"
    )
else()
    target_compile_options(mython PRIVATE
            -g3 -O0 -fno-omit-frame-pointer
    )
    message(WARNING "ASan not enabled")
endif()

target_compile_definitions(mython PRIVATE DEBUG=1)
