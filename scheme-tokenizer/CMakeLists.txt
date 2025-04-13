# Create an interface library for header-only setup
add_library(scheme_tokenizer INTERFACE)

target_include_directories(scheme_tokenizer INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
