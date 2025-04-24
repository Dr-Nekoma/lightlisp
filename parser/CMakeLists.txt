add_library(parser STATIC
  parser.cpp
)

target_include_directories(parser
  PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/definitions
    ${CMAKE_SOURCE_DIR}/tokenizer
)

target_link_libraries(parser
  PUBLIC tokenizer definitions
)