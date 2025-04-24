cmake_minimum_required(VERSION 3.15)
project(lightlisp LANGUAGES CXX)

# Use C++20
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXE_LINKER_FLAGS "-fuse-ld=lld")

# — LLVM configuration —
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Pull in LLVM headers & definitions
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Choose the LLVM components you depend on
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

# Map those component names to actual library names
llvm_map_components_to_libnames(llvm_libs ${LLVM_LINK_COMPONENTS})

# Your own include tree
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Now that llvm_libs is defined, bring in all the subprojects
add_subdirectory(definitions)
add_subdirectory(tokenizer)
add_subdirectory(parser)
add_subdirectory(compiler)
add_subdirectory(ir1lisp)
add_subdirectory(optimizer)
add_subdirectory(src)