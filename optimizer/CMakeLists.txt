add_library(optimizer STATIC
  Optimizer.cpp
)

target_include_directories(optimizer
  PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/definitions
)

target_link_libraries(optimizer
  PUBLIC definitions
)