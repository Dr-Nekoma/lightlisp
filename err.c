#include <stdio.h>
#include <stdlib.h>

void panic_code(int code) {
    fprintf(stderr, "PANIC: error code %d\n", code);
    exit(code);
}


void print_int(int64_t num) {
    fprintf(stderr, "Result: %ld\n", num);
}