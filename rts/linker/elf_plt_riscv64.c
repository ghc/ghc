#include "Rts.h"
#include "elf_compat.h"
#include "elf_plt_riscv64.h"

#include <stdlib.h>

#if defined(riscv64_HOST_ARCH)

#if defined(OBJFORMAT_ELF)

const size_t instSizeRISCV64 = 2;
const size_t stubSizeRISCV64 = 5 * instSizeRISCV64;

bool needStubForRelRISCV64(Elf_Rel * rel) {
    switch(ELF64_R_TYPE(rel->r_info)) {
        case R_RISCV_CALL_PLT:
            return true;
        default:
            return false;
    }
}

bool needStubForRelaIRISCV64(Elf_Rela * rela) {
    switch(ELF64_R_TYPE(rela->r_info)) {
        case R_RISCV_CALL_PLT:
            return true;
        default:
            return false;
    }
}

// The stub is just a long jump to the target address.
bool makeStubRISCV64(Stub * s) {
    uint32_t *P = (uint32_t*)s->addr;

    /* target address */
    uint64_t addr = (uint64_t)s->target;

    // LUI ip, %hi(addr)
    uint32_t luiInst = 0x37; // opcode
    luiInst |= 0x1f << 7; // rd = ip (x31)
    luiInst |= ((addr >> 12) & 0xfffff) << 12; // imm[31:12]

    // JALR x0, ip, %lo(addr)
    uint32_t jalrInst = 0x67; // opcode
    jalrInst |= 0x00 << 7; // rd = x0
    jalrInst |= 0x1f << 15; // rs1 = ip (x31)
    jalrInst |= (addr & 0xfff) << 20; // imm[11:0]

    P[0] = luiInst;
    P[1] = jalrInst;

    return EXIT_SUCCESS;
}
#endif // OBJECTFORMAT_ELF

#endif // riscv64_HOST_ARCH
