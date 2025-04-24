# definitions: meta.cpp, types.cpp
add_library(definitions STATIC
  meta.cpp
  types.cpp
)

target_include_directories(definitions
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
)