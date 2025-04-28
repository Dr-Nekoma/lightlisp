# LightLisp

Notitia linguarum est prima porta sapientiae.

Човекът е толкова пъти човек, колкото езика знае.

學而時習之 不亦說乎

Kolik jazyků znáš, tolikrát jsi člověkem.

Die Grenzen meiner Sprache bedeuten die Grenzen meiner Welt.

I ka 'ōlelo no ke ola; i ka 'ōlelo no ka make

Beatha teanga í a labhairt

..okay, you got the idea.

## Prerequisites

- CMake 3.15 or newer
- Ninja (optional)
- LLVM (with development headers)
- A C++20-capable compiler (e.g. `clang++`)

## Building

From the project root:

```bash
./build.sh
```

This produces the compiler executable at:

```
build/src/lightlisp
```

## Compiling a Lisp program

Given a file `myprog.lisp`, run:

```bash
./build/src/lightlisp myprog.lisp
```

This will emit two files in the working directory:

- `myprog.lisp.ll` – the generated LLVM IR
- `myprog.o` – the compiled object file

To turn that into a native executable:

```bash
clang++ myprog.o -o myprog
```

Now you can run your program:

```bash
./myprog
```

## About LightLisp

LightLisp is a minimalist Lisp dialect (integers, cons cells, `car`/`cdr`, `+`, `-`, `*`, `<`, `if`, etc.) with a AOT compiler front end. It's designed purely for learning and experimentation--feel free to clone, hack on it, and study how code generation works. I hope I would manage to expand this into a full tutorial one day.
