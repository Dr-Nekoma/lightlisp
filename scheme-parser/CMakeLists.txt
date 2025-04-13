add_library(scheme_parser STATIC
    parser.cpp
    parser.h
)

target_include_directories(scheme_parser PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(scheme_parser PUBLIC scheme_tokenizer)
