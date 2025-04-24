add_library(ir1lisp STATIC
  ir1lisp.cpp
)

target_include_directories(ir1lisp
  PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/definitions
)

target_link_libraries(ir1lisp
  PUBLIC parser definitions
)