add_library(ir2lisp STATIC
  ir2lisp.cpp
)

target_include_directories(ir2lisp
  PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/definitions
)

target_link_libraries(ir2lisp
  PUBLIC definitions
)