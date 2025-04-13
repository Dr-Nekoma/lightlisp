add_library(ir1lisp STATIC
    ir1lisp.cpp
    ir1lisp.h
)

target_include_directories(ir1lisp PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/../definitions
)
