#include <stdio.h>

#define CONSTRUCTOR(prio) __attribute__((constructor(prio)))
#define DESTRUCTOR(prio)  __attribute__((destructor(prio)))
#define PRINT(str) printf(str); fflush(stdout)

CONSTRUCTOR(1000) void constr_a(void) { PRINT("constr a\n"); }
CONSTRUCTOR(2000) void constr_b(void) { PRINT("constr b\n"); }
DESTRUCTOR(2000)  void destr_b(void)  { PRINT("destr b\n"); }
DESTRUCTOR(1000)  void destr_a(void)  { PRINT("destr a\n"); }

void hello() {
    PRINT("hello\n");
}
