cmake_minimum_required(VERSION 3.15)
project(trash_compiler)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXE_LINKER_FLAGS "-fuse-ld=lld")
# LLVM configuration
find_package(LLVM REQUIRED CONFIG)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Include definitions
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/definitions)

# Subdirectories
add_subdirectory(scheme-tokenizer)
add_subdirectory(scheme-parser)
add_subdirectory(scheme-compiler)
add_subdirectory(ir1lisp)

# Executable
add_executable(trash_compiler 
    src/main.cpp
    src/Optimizer.cpp
    src/Optimizer.h
)

# List the LLVM components your project depends on
set(LLVM_LINK_COMPONENTS
    core
    support
    irreader
    passes
    transformutils
    scalaropts
    analysis
    instcombine
    nativecodegen
    orcjit

    # Add the native target component
    ${LLVM_TARGETS_TO_BUILD}
    )

# Map the LLVM components to library names
llvm_map_components_to_libnames(llvm_libs ${LLVM_LINK_COMPONENTS})

# Link libraries
target_link_libraries(trash_compiler
    scheme_tokenizer
    scheme_parser
    scheme_compiler
    ir1lisp
    ${llvm_libs}
)

target_include_directories(trash_compiler PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src)