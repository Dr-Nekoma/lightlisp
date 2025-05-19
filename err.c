#include <stdio.h>
#include <stdlib.h>

void panic_code(int code, int proper_code) {
    fprintf(stderr,
        "Panic - type %d, while should have been %d",
        code, proper_code);
    exit(code);
}


void print_int(int64_t num) {
    fprintf(stderr, "Result: %ld\n", num);
}