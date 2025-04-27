# definitions: meta.cpp, types.cpp
add_library(definitions STATIC
  meta.cpp
  types.cpp
  prepare.cpp
)

target_include_directories(definitions
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_SOURCE_DIR}/compiler
  ${CMAKE_SOURCE_DIR}/optimizer
  ${CMAKE_SOURCE_DIR}/ir1lisp
)