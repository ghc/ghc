#include<stdint.h>
#include<stdio.h>

extern int foo32_1, foo32_2;

// The bug in #23066 was that we wouldn't correctly align 32-bytes aligned
// sections, except by chance (we were always aligning on 16 bytes).
//
// Hence we intersperse two 16-bytes aligned sections with two 32-bytes aligned
// sections to ensure that at least one of the 32-bytes aligned section
// triggers the bug (the order of the sections seems to be preserved).

__asm__(
"	 .section	pad16_1,\"aM\",@progbits,16\n\t"
"        .align 16\n\t"
"        .byte 0\n\t"
"\n\t"
"        .global foo32_1\n\t"
"	 .section	sfoo32_1,\"aM\",@progbits,32\n\t"
"        .align 32\n\t"
"foo32_1:\n\t"
"        .byte 0\n\t"
"\n\t"
"	 .section	pad16_2,\"aM\",@progbits,16\n\t"
"        .align 16\n\t"
"        .byte 0\n\t"
"\n\t"
"        .global foo32_2\n\t"
"	 .section	sfoo32_2,\"aM\",@progbits,32\n\t"
"        .align 32\n\t"
"foo32_2:\n\t"
"        .byte 0\n\t"
);


#define ALIGN32(x) (((intptr_t)(&x) & 0x1F) == 0)

int isAligned() {
  //printf("%p\n", &foo32_1);
  //printf("%p\n", &foo32_2);
  return (ALIGN32(foo32_1) && ALIGN32(foo32_2));
}
