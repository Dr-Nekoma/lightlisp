add_library(compiler STATIC
  compiler.cpp
  util.cpp
)

target_include_directories(compiler
  PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/definitions
    ${CMAKE_SOURCE_DIR}/parser
    ${CMAKE_SOURCE_DIR}/optimizer
)

target_link_libraries(compiler
  PUBLIC parser definitions
)
