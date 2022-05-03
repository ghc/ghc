#include <stdio.h>

// Haskell function
extern void helper();

void test_c() {
    unsigned char blah[16] = {0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10};
    unsigned char foo[16] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
    // Copy blah into xmm6
    asm (
        "movups %[blah], %%xmm6"
        :
        : [blah] "m" (blah)
        : "cc", "memory", "xmm6", "xmm7"
    );
    // Call to Haskell
    helper();
    // Copy xmm6 to foo
    asm (
        "movups %%xmm6, %[foo]"
        : [foo] "=m" (foo)
        :
        : "cc", "memory", "xmm6", "xmm7"
    );
    for (int i = 0; i < 16; i++) {
        printf("%2i: %02x %02x\n", i, blah[i], foo[i]);
    }
}