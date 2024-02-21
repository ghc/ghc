#include "elf_reloc_riscv64.h"
#include "Rts.h"
#include "elf.h"
#include "elf_plt.h"
#include "elf_util.h"
#include "rts/Messages.h"
#include "util.h"

#include <stdint.h>
#include <stdlib.h>

#if defined(riscv64_HOST_ARCH)

#if defined(OBJFORMAT_ELF)

char *relocationTypeToString(Elf64_Xword type) {
  switch (ELF64_R_TYPE(type)) {
  case R_RISCV_NONE:
    return "R_RISCV_NONE";
  case R_RISCV_32:
    return "R_RISCV_32";
  case R_RISCV_64:
    return "R_RISCV_64";
  case R_RISCV_RELATIVE:
    return "R_RISCV_RELATIVE";
  case R_RISCV_COPY:
    return "R_RISCV_COPY";
  case R_RISCV_JUMP_SLOT:
    return "R_RISCV_JUMP_SLOT";
  case R_RISCV_TLS_DTPMOD32:
    return "R_RISCV_TLS_DTPMOD32";
  case R_RISCV_TLS_DTPMOD64:
    return "R_RISCV_TLS_DTPMOD64";
  case R_RISCV_TLS_DTPREL32:
    return "R_RISCV_TLS_DTPREL32";
  case R_RISCV_TLS_DTPREL64:
    return "R_RISCV_TLS_DTPREL64";
  case R_RISCV_TLS_TPREL32:
    return "R_RISCV_TLS_TPREL32";
  case R_RISCV_TLS_TPREL64:
    return "R_RISCV_TLS_TPREL64";
  case R_RISCV_BRANCH:
    return "R_RISCV_BRANCH";
  case R_RISCV_JAL:
    return "R_RISCV_JAL";
  case R_RISCV_CALL:
    return "R_RISCV_CALL";
  case R_RISCV_CALL_PLT:
    return "R_RISCV_CALL_PLT";
  case R_RISCV_GOT_HI20:
    return "R_RISCV_GOT_HI20";
  case R_RISCV_PCREL_HI20:
    return "R_RISCV_PCREL_HI20";
  case R_RISCV_LO12_I:
    return "R_RISCV_LO12_I";
  case R_RISCV_PCREL_LO12_I:
    return "R_RISCV_PCREL_LO12_I";
  case R_RISCV_HI20:
    return "R_RISCV_HI20";
  case R_RISCV_LO12_S:
    return "R_RISCV_LO12_S";
  case R_RISCV_PCREL_LO12_S:
    return "R_RISCV_PCREL_LO12_S";
  case R_RISCV_RELAX:
    return "R_RISCV_RELAX";
  default:
    return "Unknown relocation type";
  }
}

#define Page(x) ((x) & ~0xFFF)

typedef uint64_t addr_t;

int64_t decodeAddendRISCV64(Section *section, Elf_Rel *rel) STG_NORETURN;
bool encodeAddendRISCV64(Section *section, Elf_Rel *rel, int64_t addend);

/* regular instructions are 32bit */
typedef uint32_t inst_t;

/* compressed instructions are 16bit */
typedef uint16_t cinst_t;

// TODO: Decide which functions should be static and/or inlined.
int64_t decodeAddendRISCV64(Section *section STG_UNUSED,
                            Elf_Rel *rel STG_UNUSED) {
  debugBelch("decodeAddendRISCV64: Relocations with explicit addend are not "
             "supported.");
  abort(/* we don't support Rel locations yet. */);
}

// Sign-extend the number in the bottom B bits of X to a 64-bit integer.
// Requires 0 < B <= 64.
int64_t SignExtend64(uint64_t X, unsigned B) {
  assert(B > 0 && "Bit width can't be 0.");
  assert(B <= 64 && "Bit width out of range.");
  return (int64_t)(X << (64 - B)) >> (64 - B);
}

// Make sure that V can be represented as an N bit signed integer.
void checkInt(inst_t *loc, int64_t v, int n) {
  if (v != SignExtend64(v, n)) {
    debugBelch("Relocation at 0x%x is out of range. value: 0x%lx (%ld), "
               "sign-extended value: 0x%lx (%ld), max bits 0x%x (%d)\n",
               *loc, v, v, SignExtend64(v, n), SignExtend64(v, n), n, n);
  }
}
// RISCV is little-endian by definition.
void write8le(uint8_t *p, uint8_t v) { *p = v; }

// RISCV is little-endian by definition.
uint8_t read8le(const uint8_t *P) { return *P; }

// RISCV is little-endian by definition.
void write16le(cinst_t *p, uint16_t v) { *p = v; }

// RISCV is little-endian by definition.
uint16_t read16le(const cinst_t *P) { return *P; }

// RISCV is little-endian by definition.
uint32_t read32le(const inst_t *P) { return *P; }

// RISCV is little-endian by definition.
void write32le(inst_t *p, uint32_t v) { *p = v; }

// RISCV is little-endian by definition.
uint64_t read64le(const uint64_t *P) { return *P; }

// RISCV is little-endian by definition.
void write64le(uint64_t *p, uint64_t v) { *p = v; }

uint32_t extractBits(uint64_t v, uint32_t begin, uint32_t end) {
  return (v & ((1ULL << (begin + 1)) - 1)) >> end;
}

uint32_t setLO12_I(uint32_t insn, uint32_t imm) {
  debugBelch(
      "setLO12_I: insn 0x%x imm 0x%x (insn & 0xfffff) 0x%x (imm << 20) 0x%x \n",
      insn, imm, (insn & 0xfffff), (imm << 20));
  return (insn & 0xfffff) | (imm << 20);
}

uint32_t setLO12_S(uint32_t insn, uint32_t imm) {
  return (insn & 0x1fff07f) | (extractBits(imm, 11, 5) << 25) |
         (extractBits(imm, 4, 0) << 7);
}

void setUType(inst_t *loc, uint32_t val) {
  const unsigned bits = 32;
  uint64_t hi = val + 0x800;
  checkInt(loc, SignExtend64(hi, bits) >> 12, 20);
  write32le(loc, (read32le(loc) & 0xFFF) | (hi & 0xFFFFF000));
}

void setIType(inst_t *loc, uint32_t val) {
  uint64_t hi = (val + 0x800) >> 12;
  uint64_t lo = val - (hi << 12);
  debugBelch("setIType: hi 0x%lx lo 0x%lx\n", hi, lo);
  debugBelch("setIType: loc %p  *loc 0x%x  val 0x%x\n", loc, *loc, val);
  uint32_t insn = setLO12_I(read32le(loc), lo & 0xfff);
  debugBelch("setIType: insn 0x%x\n", insn);
  write32le(loc, insn);
  debugBelch("setIType: loc %p  *loc' 0x%x  val 0x%x\n", loc, *loc, val);
}

void setSType(inst_t *loc, uint32_t val) {
  uint64_t hi = (val + 0x800) >> 12;
  uint64_t lo = val - (hi << 12);
  write32le(loc, setLO12_S(read32le(loc), lo));
}

void setJType(inst_t *loc, uint32_t val) {
  checkInt(loc, val, 21);

  uint32_t insn = read32le(loc) & 0xFFF;
  uint32_t imm20 = extractBits(val, 20, 20) << 31;
  uint32_t imm10_1 = extractBits(val, 10, 1) << 21;
  uint32_t imm11 = extractBits(val, 11, 11) << 20;
  uint32_t imm19_12 = extractBits(val, 19, 12) << 12;
  insn |= imm20 | imm10_1 | imm11 | imm19_12;

  write32le(loc, insn);
}

void setBType(inst_t *loc, uint32_t val) {
  checkInt(loc, val, 13);

  uint32_t insn = read32le(loc) & 0x1FFF07F;
  uint32_t imm12 = extractBits(val, 12, 12) << 31;
  uint32_t imm10_5 = extractBits(val, 10, 5) << 25;
  uint32_t imm4_1 = extractBits(val, 4, 1) << 8;
  uint32_t imm11 = extractBits(val, 11, 11) << 7;
  insn |= imm12 | imm10_5 | imm4_1 | imm11;

  write32le(loc, insn);
}

void setCBType(cinst_t *loc, uint32_t val) {
  checkInt((inst_t *)loc, val, 9);
  uint16_t insn = read16le(loc) & 0xE383;
  uint16_t imm8 = extractBits(val, 8, 8) << 12;
  uint16_t imm4_3 = extractBits(val, 4, 3) << 10;
  uint16_t imm7_6 = extractBits(val, 7, 6) << 5;
  uint16_t imm2_1 = extractBits(val, 2, 1) << 3;
  uint16_t imm5 = extractBits(val, 5, 5) << 2;
  insn |= imm8 | imm4_3 | imm7_6 | imm2_1 | imm5;

  write16le(loc, insn);
}

void setCJType(cinst_t *loc, uint32_t val) {
  checkInt((inst_t *)loc, val, 12);
  uint16_t insn = read16le(loc) & 0xE003;
  uint16_t imm11 = extractBits(val, 11, 11) << 12;
  uint16_t imm4 = extractBits(val, 4, 4) << 11;
  uint16_t imm9_8 = extractBits(val, 9, 8) << 9;
  uint16_t imm10 = extractBits(val, 10, 10) << 8;
  uint16_t imm6 = extractBits(val, 6, 6) << 7;
  uint16_t imm7 = extractBits(val, 7, 7) << 6;
  uint16_t imm3_1 = extractBits(val, 3, 1) << 3;
  uint16_t imm5 = extractBits(val, 5, 5) << 2;
  insn |= imm11 | imm4 | imm9_8 | imm10 | imm6 | imm7 | imm3_1 | imm5;

  write16le(loc, insn);
}

bool encodeAddendRISCV64(Section *section, Elf_Rel *rel, int64_t addend) {
  addr_t P = (addr_t)((uint8_t *)section->start + rel->r_offset);
  debugBelch("Relocation type %s 0x%lx (%lu) symbol 0x%lx addend 0x%lx (%lu / "
             "%ld) P 0x%lx\n",
             relocationTypeToString(rel->r_info), ELF64_R_TYPE(rel->r_info),
             ELF64_R_TYPE(rel->r_info), ELF64_R_SYM(rel->r_info), addend,
             addend, addend, P);
  switch (ELF64_R_TYPE(rel->r_info)) {
  case R_RISCV_32_PCREL:
  case R_RISCV_32:
    write32le((inst_t *)P, addend);
    break;
  case R_RISCV_64:
    write64le((uint64_t *)P, addend);
    break;
  case R_RISCV_GOT_HI20:
  case R_RISCV_PCREL_HI20:
  case R_RISCV_HI20: {
    setUType((inst_t *)P, addend);
    break;
  }
  case R_RISCV_PCREL_LO12_I:
  case R_RISCV_LO12_I: {
    setIType((inst_t *)P, addend);
    break;
  }
  case R_RISCV_RVC_JUMP: {
    setCJType((cinst_t *)P, addend);
    break;
  }
  case R_RISCV_RVC_BRANCH: {
    setCBType((cinst_t *)P, addend);
    break;
  }
  case R_RISCV_BRANCH: {
    setBType((inst_t *)P, addend);
    break;
  }
  case R_RISCV_CALL:
  case R_RISCV_CALL_PLT: {
    // We could relax more (in some cases) but right now most important is to
    // make it work.
    setUType((inst_t *)P, addend);
    setIType(((inst_t *)P) + 1, addend);
    break;
  }
  case R_RISCV_JAL: {
    setJType((inst_t *)P, addend);
    break;
  }
  case R_RISCV_ADD8:
    write8le((uint8_t *)P, read8le((uint8_t *)P) + addend);
    break;
  case R_RISCV_ADD16:
    write16le((cinst_t *)P, read16le((cinst_t *)P) + addend);
    break;
  case R_RISCV_ADD32:
    write32le((inst_t *)P, read32le((inst_t *)P) + addend);
    break;
  case R_RISCV_ADD64:
    write64le((uint64_t *)P, read64le((uint64_t *)P) + addend);
    break;
  case R_RISCV_SUB6: {
    uint8_t keep = *((uint8_t *)P) & 0xc0;
    uint8_t imm = (((*(uint8_t *)P) & 0x3f) - addend) & 0x3f;

    write8le((uint8_t *)P, keep | imm);
    break;
  }
  case R_RISCV_SUB8:
    write8le((uint8_t *)P, read8le((uint8_t *)P) - addend);
    break;
  case R_RISCV_SUB16:
    write16le((cinst_t *)P, read16le((cinst_t *)P) - addend);
    break;
  case R_RISCV_SUB32:
    write32le((inst_t *)P, read32le((inst_t *)P) - addend);
    break;
  case R_RISCV_SUB64:
    write64le((uint64_t *)P, read64le((uint64_t *)P) - addend);
    break;
  case R_RISCV_SET6: {
    uint8_t keep = *((uint8_t *)P) & 0xc0;
    uint8_t imm = (addend & 0x3f) & 0x3f;

    write8le((uint8_t *)P, keep | imm);
    break;
  }
  case R_RISCV_SET8:
    write8le((uint8_t *)P, addend);
    break;
  case R_RISCV_SET16:
    write16le((cinst_t *)P, addend);
    break;
  case R_RISCV_SET32:
    write32le((inst_t *)P, addend);
    break;
  case R_RISCV_PCREL_LO12_S:
  case R_RISCV_TPREL_LO12_S:
  case R_RISCV_LO12_S: {
    setSType((inst_t *)P, addend);
    break;
  }
  case R_RISCV_RELAX:
  case R_RISCV_ALIGN:
    // I guess we don't need to implement these relaxations (optimizations).
    break;
  default:
    debugBelch("Missing relocation 0x%lx\n", ELF64_R_TYPE(rel->r_info));
    abort();
  }
  return EXIT_SUCCESS;
}

/**
 * Compute the *new* addend for a relocation, given a pre-existing addend.
 * @param section The section the relocation is in.
 * @param rel     The Relocation struct.
 * @param symbol  The target symbol.
 * @param addend  The existing addend. Either explicit or implicit.
 * @return The new computed addend.
 */
int64_t computeAddend(Section *section, Elf_Rel *rel, ElfSymbol *symbol,
                      int64_t addend) {

  /* Position where something is relocated */
  addr_t P = (addr_t)((uint8_t *)section->start + rel->r_offset);

  CHECK(0x0 != P);
  CHECK((uint64_t)section->start <= P);
  CHECK(P <= (uint64_t)section->start + section->size);
  /* Address of the symbol */
  addr_t S = (addr_t)symbol->addr;
  /* GOT slot for the symbol */
  addr_t GOT_S = (addr_t)symbol->got_addr;

  int64_t A = addend;

  debugBelch("%s: P 0x%lx S 0x%lx %s GOT_S 0x%lx A 0x%lx\n",
             relocationTypeToString(rel->r_info), P, S, symbol->name, GOT_S, A);
  switch (ELF64_R_TYPE(rel->r_info)) {
  case R_RISCV_32:
    return S + A;
  case R_RISCV_64:
    return S + A;
  case R_RISCV_HI20:
    return S + A;
  case R_RISCV_JUMP_SLOT:
    return S;
  case R_RISCV_JAL:
    return S + A - P;
  case R_RISCV_PCREL_HI20:
    return S + A - P;
  case R_RISCV_LO12_I:
    return S + A;
  case R_RISCV_PCREL_LO12_I:
    return S - P;
  case R_RISCV_RVC_JUMP:
    return S + A - P;
  case R_RISCV_RVC_BRANCH:
    return S + A - P;
  case R_RISCV_BRANCH:
    return S + A - P;
  case R_RISCV_CALL:
  case R_RISCV_CALL_PLT: {
    if (findStub(section, (void **)&S, 0)) {
      /* did not find it. Crete a new stub. */
      if (makeStub(section, (void **)&S, (void*) GOT_S, 0)) {
        abort(/* could not find or make stub */);
      }
    }
    debugBelch("S = 0x%lx  A = 0x%lx  P = 0x%lx  (S + A) - P = 0x%lx \n", S, A,
               P, (S + A) - P);
    return (S + A) - P;
  }
  case R_RISCV_ADD8:
  case R_RISCV_ADD16:
  case R_RISCV_ADD32:
  case R_RISCV_ADD64:
    return S + A; // Add V when the value is set
  case R_RISCV_SUB6:
  case R_RISCV_SUB8:
  case R_RISCV_SUB16:
  case R_RISCV_SUB32:
  case R_RISCV_SUB64:
    return S + A; // Subtract from V when value is set
  case R_RISCV_SET6:
  case R_RISCV_SET8:
  case R_RISCV_SET16:
  case R_RISCV_SET32:
    return S + A;
  case R_RISCV_RELAX:
  case R_RISCV_ALIGN:
    // I guess we don't need to implement this relaxation. Otherwise, this
    // should return the number of blank bytes to insert via NOPs.
    return 0;
  case R_RISCV_32_PCREL:
    return S + A - P;
  case R_RISCV_GOT_HI20:
    // reduced G + GOT to GOT_S - This might be wrong!
    return GOT_S + A - P;
  case R_RISCV_PCREL_LO12_S:
    return S - P;
  default:
    debugBelch("Unimplemented relocation: 0x%lx\n (%lu)",
               ELF64_R_TYPE(rel->r_info), ELF64_R_TYPE(rel->r_info));
    abort(/* unhandled rel */);
  }
  debugBelch("This should never happen!");
  abort(/* unhandled rel */);
}

// TODO: This is duplicated from elf_reloc_aarch64.c
bool relocateObjectCodeRISCV64(ObjectCode *oc) {
  for (ElfRelocationTable *relTab = oc->info->relTable; relTab != NULL;
       relTab = relTab->next) {
    /* only relocate interesting sections */
    if (SECTIONKIND_OTHER == oc->sections[relTab->targetSectionIndex].kind)
      continue;

    Section *targetSection = &oc->sections[relTab->targetSectionIndex];

    for (unsigned i = 0; i < relTab->n_relocations; i++) {
      Elf_Rel *rel = &relTab->relocations[i];

      ElfSymbol *symbol = findSymbol(oc, relTab->sectionHeader->sh_link,
                                     ELF64_R_SYM((Elf64_Xword)rel->r_info));

      CHECK(0x0 != symbol);

      // TODO: This always fails, because we don't support Rel locations: Do
      // we need this case?
      /* decode implicit addend */
      int64_t addend = decodeAddendRISCV64(targetSection, rel);

      addend = computeAddend(targetSection, rel, symbol, addend);
      encodeAddendRISCV64(targetSection, rel, addend);
    }
  }
  for (ElfRelocationATable *relaTab = oc->info->relaTable; relaTab != NULL;
       relaTab = relaTab->next) {
    /* only relocate interesting sections */
    if (SECTIONKIND_OTHER == oc->sections[relaTab->targetSectionIndex].kind)
      continue;

    Section *targetSection = &oc->sections[relaTab->targetSectionIndex];

    for (unsigned i = 0; i < relaTab->n_relocations; i++) {

      Elf_Rela *rel = &relaTab->relocations[i];

      ElfSymbol *symbol = findSymbol(oc, relaTab->sectionHeader->sh_link,
                                     ELF64_R_SYM((Elf64_Xword)rel->r_info));

      CHECK(0x0 != symbol);

      /* take explicit addend */
      int64_t addend = rel->r_addend;

      addend = computeAddend(targetSection, (Elf_Rel *)rel, symbol, addend);
      encodeAddendRISCV64(targetSection, (Elf_Rel *)rel, addend);
    }
  }
  return EXIT_SUCCESS;
}

#endif /* OBJECTFORMAT_ELF */
#endif /* riscv64_HOST_ARCH */
