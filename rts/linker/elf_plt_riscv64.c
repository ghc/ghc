#include "Rts.h"
#include "elf_compat.h"
#include "elf_plt_riscv64.h"
#include "rts/Messages.h"
#include "linker/ElfTypes.h"

#include <stdint.h>
#include <stdlib.h>

#if defined(riscv64_HOST_ARCH)

#if defined(OBJFORMAT_ELF)

const size_t instSizeRISCV64 = 4;
const size_t stubSizeRISCV64 = 3 * instSizeRISCV64;

bool needStubForRelRISCV64(Elf_Rel *rel) {
  switch (ELF64_R_TYPE(rel->r_info)) {
  case R_RISCV_CALL:
  case R_RISCV_CALL_PLT:
    return true;
  default:
    return false;
  }
}

bool needStubForRelaRISCV64(Elf_Rela *rela) {
  switch (ELF64_R_TYPE(rela->r_info)) {
  case R_RISCV_CALL:
  case R_RISCV_CALL_PLT:
    return true;
  default:
    return false;
  }
}

// After the global offset table (GOT) has been set up, we can use these three
// instructions to jump to the target address / function:
//
//  1. AUIPC ip, %pcrel_hi(addr)
//  2. LD ip, %pcrel_lo(addr)(ip)
//  3. JARL x0, ip, 0
//
// We could use the absolute address of the target (because we know it), but
// that would require loading a 64-bit constant which is a nightmare to do in
// riscv64 assembly. (See
// https://github.com/riscv-non-isa/riscv-elf-psabi-doc/blob/5ffe5b5aeedb37b1c1c0c3d94641267d9ad4795a/riscv-elf.adoc#procedure-linkage-table)
//
// So far, PC-relative addressing seems to be good enough. If it ever turns out
// to be not, one could (additionally for out-of-range cases?) encode absolute
// addressing here.
bool makeStubRISCV64(Stub *s) {
  uint32_t *P = (uint32_t *)s->addr;
  int32_t addr = (uint64_t)s->got_addr - (uint64_t)P;

  uint64_t hi = (addr + 0x800) >> 12;
  uint64_t lo = addr - (hi << 12);

  IF_DEBUG(
      linker,
      debugBelch(
          "makeStubRISCV64: P = %p, got_addr = %p, target = %p, addr = 0x%x "
          ", hi = 0x%lx, lo = 0x%lx\n",
          P, s->got_addr, s->target, addr, hi, lo));

  // AUIPC ip, %pcrel_hi(addr)
  uint32_t auipcInst = 0b0010111; // opcode
  auipcInst |= 0x1f << 7;         // rd = ip (x31)
  auipcInst |= hi << 12;          // imm[31:12]

  // LD ip, %pcrel_lo(addr)(ip)
  uint32_t ldInst = 0b0000011; // opcode
  ldInst |= 0x1f << 7;         // rd = ip (x31)
  ldInst |= 0x1f << 15;        // rs = ip (x31)
  ldInst |= 0b11 << 12;        // funct3 = 0x3 (LD)
  ldInst |= lo << 20;          // imm[11:0]

  // JARL x0, ip, 0
  uint32_t jalrInst = 0b1100111; // opcode
  jalrInst |= 0x1f << 15;        // rs = ip (x31)

  P[0] = auipcInst;
  P[1] = ldInst;
  P[2] = jalrInst;

  return EXIT_SUCCESS;
}

#endif
#endif
