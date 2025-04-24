# header-only tokenizer
add_library(tokenizer INTERFACE)

target_include_directories(tokenizer
  INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}
)