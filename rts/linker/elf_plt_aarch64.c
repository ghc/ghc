#include "Rts.h"
#include "elf_compat.h"
#include "elf_plt_aarch64.h"

#include <stdlib.h>

#if defined(aarch64_HOST_ARCH)

#if defined(OBJFORMAT_ELF)

/* five 4 byte instructions */
const size_t instSizeAarch64 = 4;
const size_t stubSizeAarch64 = 5 * 4;

/*
 * Compute the number of stub (PLT entries) for a given section by iterating
 * over the relocations and relocations with explicit addend and counting those
 * relocations that might require a PLT relocation.
 *
 * This will be an upper bound, and we might not use all stubs.  However by
 * calculating the number of potential stubs beforehand, we can allocate enough
 * space adjacent to the section, such that the PLT is rather close to the
 * section, and the risk of the stubs being out of reach for the instruction to
 * be relocated is minimal.
 */
bool needStubForRelAarch64(Elf_Rel * rel) {
    switch(ELF64_R_TYPE(rel->r_info)) {
        case COMPAT_R_AARCH64_CALL26:
        case COMPAT_R_AARCH64_JUMP26:
            return true;
        default:
            return false;
    }
}
bool needStubForRelaAarch64(Elf_Rela * rela) {
    switch(ELF64_R_TYPE(rela->r_info)) {
        case COMPAT_R_AARCH64_CALL26:
        case COMPAT_R_AARCH64_JUMP26:
            return true;
        default:
            return false;
    }
}


bool
makeStubAarch64(Stub * s) {
    // We (the linker) may corrupt registers x16 (IP0) and x17 (IP1) [AAPCS64]
    // and the condition flags, according to the "ELF for the ARM64
    // Architecture".
    //
    // [Special purpose regs]
    // X16 and X17 are IP0 and IP1, intra-procedure-call temporary registers.
    // These can be used by call veneers and similar code, or as temporary
    // registers for intermediate values between subroutine calls. They are
    // corruptible by a function. Veneers are small pieces of code which are
    // automatically inserted by the linker, for example when the branch target
    // is out of range of the branch instruction.
    // (Sect 9.9.1 of ARM Cortex-A Series Programmer's Guide for ARMv8-A, V1.0)

    // Move wide
    // mov <Wd>, #<imm16> (sf == 0)
    // mov <Xd>, #<imm16> (sf == 1) looks like:
    // 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16
    // sf  1  0  1  0  0  1  0  1 [hw ] [   imm16 ...
    //
    // 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
    // ...        imm16               ] [     Rd     ]
    // hw is the half word shift.

    // Move keep
    // movk <Wd>, #<imm16> (sf == 0)
    // movk <Xd>, #<imm16> (sf == 1) looks like:
    // 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16
    // sf  1  1  1  0  0  1  0  1 [hw ] [   imm16 ...
    //
    // 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
    // ...        imm16               ] [     Rd     ]
    // hw is the half word shift.

    // br <Xn> (Encoding A1) looks like:
    // 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16
    //  1  1  0  1  0  1  1  0  0  0  0  1  1  1  1  1
    //
    // 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
    //  0  0  0  0  0  0 [    Rd      ]  0  0  0  0  0
    //
    // We'll use 0b1110 for the condition.


    uint32_t mov__hw0_x16 = 0xd2800000 | 16;
    uint32_t movk_hw0_x16 = mov__hw0_x16 | (1 << 29);

    uint32_t mov__hw3_x16 = mov__hw0_x16 | (3 << 21);
    uint32_t movk_hw2_x16 = movk_hw0_x16 | (2 << 21);
    uint32_t movk_hw1_x16 = movk_hw0_x16 | (1 << 21);


    uint32_t br_x16 = 0xd61f0000 | 16 << 5;

    uint32_t *P = (uint32_t*)s->addr;

    /* target address */
    uint64_t addr = (uint64_t)s->target;
    uint16_t  addr_hw0 = (uint16_t)(addr >>  0);
    uint16_t  addr_hw1 = (uint16_t)(addr >> 16);
    uint16_t  addr_hw2 = (uint16_t)(addr >> 32);
    uint16_t  addr_hw3 = (uint16_t)(addr >> 48);

    P[0] = mov__hw3_x16 | ((uint32_t)addr_hw3 << 5);
    P[1] = movk_hw2_x16 | ((uint32_t)addr_hw2 << 5);
    P[2] = movk_hw1_x16 | ((uint32_t)addr_hw1 << 5);
    P[3] = movk_hw0_x16 | ((uint32_t)addr_hw0 << 5);
    P[4] = br_x16;

    return EXIT_SUCCESS;
}
#endif // OBJECTFORMAT_ELF

#endif // aarch64_HOST_ARCH
