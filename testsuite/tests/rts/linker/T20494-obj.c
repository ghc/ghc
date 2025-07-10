#include <stdio.h>
#include <unistd.h>

#define CONSTRUCTOR(prio) __attribute__((constructor(prio)))
#define DESTRUCTOR(prio)  __attribute__((destructor(prio)))
// don't use "stdout" variable here as it is not properly defined when loading
// this object in a statically linked GHC.
#define PRINT(str) dprintf(1,str); fsync(1)

CONSTRUCTOR(1000) void constr_a(void) { PRINT("constr a\n"); }
CONSTRUCTOR(2000) void constr_b(void) { PRINT("constr b\n"); }
DESTRUCTOR(2000)  void destr_b(void)  { PRINT("destr b\n"); }
DESTRUCTOR(1000)  void destr_a(void)  { PRINT("destr a\n"); }

void hello() {
    PRINT("hello\n");
}
