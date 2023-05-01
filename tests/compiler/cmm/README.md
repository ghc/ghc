# The Cmm tests

These tests are primarily targetted towards writing a codegeneration backend. The
`driver.sh` will pickup all `.cmm` files and run them by looking for the `RUN:`
marker in them.  The commands will then be executed in sequence.

A typical file will contain the following header:
```
// RUN: "$HC" -cpp -dcmm-lint -keep-s-file -c "$1" && cat "${1%%.*}.s" | FileCheck "$1" -check-prefix=CHECK-A64
```
this instructs the driver, to compie the cmm file `$1`; and pipe the assembly
output into the `FileCheck` tool from the llvm project, which allows us to write
basic assertions about the generated assembly.  The `FileCheck` tool will scan
the file for `CHECK-A64:` marker, as well as `CHECK-A64-NEXT:` marker and verify
those in the output (see the `-check-prefix` argument to `FileCheck`).  The
`FileCheck` tool has many more options, that might come in handy.

Some files will also verify that they execute with a zero exit code, or print
some value to stdout.  Again we can use the `RUN:` marker for this as the following
example demonstrates:
```
// RUN: "$HC" -cpp -dcmm-lint -keep-s-file -c "$1" && cat "${1%%.*}.s" | FileCheck "$1" -check-prefix=CHECK-A64
// RUN: "$CC" "${1%%.*}.o" -o "${1%%.*}.exe"
// RUN: "$EXEC" "${1%%.cmm}.exe"
```
this will not only compile the assembly, verify that some instructions are
contained, but also convert the produced object file into an executable using
a c compiler, and then run the executable.

You will most likely also want to include the following:
```
#include "Cmm.h"
#include "Types.h"
```
to be able to copy the cmm dumps ghc produces more closely. Those will still
need quite a bit of munging, as the pretty-printer does not roundtrip.

## Environment Variables:

- `HC` the haskell compiler to use.
- `CC` the c compiler to use, should be targetting the same architecture as `HC`.
- `EXEC` if we are cross compiling, we might need a tool to execute the foreign
  executable. E.g. `wine` or `qemu-<arch>`.

That is, to execute the whole test-suite (when cross compiling) you may run
```
EXEC=qemu-aarch64 CC="$TARGET_CC" bash ./driver.sh
```

## Missing features

- [ ] Run selective tests.
- [ ] Pipe command output into log, and cat log on failure.
- [x] Better reporting on which command failed if multiple commands are given.
- [ ] RUN commands should have pipeset fail set.

## Cmm ppr/parser bugs
- [ ] [Parser] Cannot assing to a fixed value (F1) = ... fails to parse
- [ ] [Parser] F1 + F2, will produce MO_Add F1 F2 instead of MO_F_Add F1 F2
- [ ] [Parser] Can not construct f(y(x)), the parser will turn that into z = y(x); f(z).
- [ ] [Ppr] keeps pring types all over the place even though the parser doesn't allow for those.
- [ ] [Ppr] will print MO_Add, ... instead of the symbols or primitives understood by the parser.