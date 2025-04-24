add_executable(lightlisp
  main.cpp
)

target_include_directories(lightlisp
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/definitions
)

target_link_libraries(lightlisp
  PRIVATE
    tokenizer
    parser
    compiler
    ir1lisp
    optimizer
    definitions
    ${llvm_libs}
)