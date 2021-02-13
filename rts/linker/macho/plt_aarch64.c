#include "Rts.h"
#include "plt_aarch64.h"

#include <stdlib.h>

#if defined(aarch64_HOST_ARCH)

#if defined(OBJFORMAT_MACHO)

#include <mach/machine.h>
#include <mach-o/fat.h>
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/reloc.h>

/* five 4 byte instructions */
const size_t instSizeAarch64 = 4;
const size_t stubSizeAarch64 = 5 * 4;

bool needStubForRelAarch64(MachORelocationInfo * rel) {
    switch(rel->r_type) {
        case ARM64_RELOC_BRANCH26:
            return true;
        default:
            return false;
    }
}

/* see the elf_plt_aarch64.c for the discussion on this */
bool
makeStubAarch64(Stub * s) {
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

#endif
#endif

