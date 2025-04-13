add_library(scheme_compiler STATIC
    compiler.cpp
)

target_include_directories(scheme_compiler PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(scheme_compiler PUBLIC scheme_parser)
