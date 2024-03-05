#include "elf_reloc_riscv64.h"
#include "LinkerInternals.h"
#include "Rts.h"
#include "Stg.h"
#include "SymbolExtras.h"
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

int32_t decodeAddendRISCV64(Section *section, Elf_Rel *rel) STG_NORETURN;
bool encodeAddendRISCV64(Section *section, Elf_Rel *rel, int32_t addend);

/* regular instructions are 32bit */
typedef uint32_t inst_t;

/* compressed instructions are 16bit */
typedef uint16_t cinst_t;

// TODO: Decide which functions should be static and/or inlined.
int32_t decodeAddendRISCV64(Section *section STG_UNUSED,
                            Elf_Rel *rel STG_UNUSED) {
  debugBelch("decodeAddendRISCV64: Relocations with explicit addend are not "
             "supported.");
  abort(/* we don't support Rel locations yet. */);
}

// Sign-extend the number in the bottom B bits of X to a 32-bit integer.
// Requires 0 < B <= 32. (32 bit is sufficient as we can only encode 20 + 12 =
// 32 bit in a relocation pair.)
int32_t SignExtend32(uint32_t X, unsigned B) {
  assert(B > 0 && "Bit width can't be 0.");
  assert(B <= 32 && "Bit width out of range.");
  return (int32_t)(X << (32 - B)) >> (32 - B);
}

// Make sure that V can be represented as an N bit signed integer.
void checkInt(inst_t *loc, int32_t v, int n) {
  if (v != SignExtend32(v, n)) {
    debugBelch("Relocation at 0x%x is out of range. value: 0x%x (%d), "
               "sign-extended value: 0x%x (%d), max bits 0x%x (%d)\n",
               *loc, v, v, SignExtend32(v, n), SignExtend32(v, n), n, n);
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
  IF_DEBUG(linker, debugBelch("setLO12_I: insn 0x%x imm 0x%x (insn & 0xfffff) "
                              "0x%x (imm << 20) 0x%x \n",
                              insn, imm, (insn & 0xfffff), (imm << 20)));
  return (insn & 0xfffff) | (imm << 20);
}

uint32_t setLO12_S(uint32_t insn, uint32_t imm) {
  return (insn & 0x1fff07f) | (extractBits(imm, 11, 5) << 25) |
         (extractBits(imm, 4, 0) << 7);
}

void setUType(inst_t *loc, int32_t val) {
  const unsigned bits = 32;
  uint32_t hi = val + 0x800;
  checkInt(loc, SignExtend32(hi, bits) >> 12, 20);
  IF_DEBUG(linker, debugBelch("setUType: hi 0x%x val 0x%x\n", hi, val));
  write32le(loc, (read32le(loc) & 0xFFF) | (hi & 0xFFFFF000));
}

void setIType(inst_t *loc, int32_t val) {
  uint64_t hi = (val + 0x800) >> 12;
  uint64_t lo = val - (hi << 12);
  IF_DEBUG(linker, debugBelch("setIType: hi 0x%lx lo 0x%lx\n", hi, lo));
  IF_DEBUG(linker, debugBelch("setIType: loc %p  *loc 0x%x  val 0x%x\n", loc,
                              *loc, val));
  uint32_t insn = setLO12_I(read32le(loc), lo & 0xfff);
  IF_DEBUG(linker, debugBelch("setIType: insn 0x%x\n", insn));
  write32le(loc, insn);
  IF_DEBUG(linker, debugBelch("setIType: loc %p  *loc' 0x%x  val 0x%x\n", loc,
                              *loc, val));
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

bool encodeAddendRISCV64(Section *section, Elf_Rel *rel, int32_t addend) {
  addr_t P = (addr_t)((uint8_t *)section->start + rel->r_offset);
  IF_DEBUG(linker,
           debugBelch(
               "Relocation type %s 0x%lx (%lu) symbol 0x%lx addend 0x%x (%u / "
               "%d) P 0x%lx\n",
               relocationTypeToString(rel->r_info), ELF64_R_TYPE(rel->r_info),
               ELF64_R_TYPE(rel->r_info), ELF64_R_SYM(rel->r_info), addend,
               addend, addend, P));
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
int32_t computeAddend(ElfRelocationATable * relaTab, unsigned relNo, Elf_Rel *rel, ElfSymbol *symbol,
                      int64_t addend, ObjectCode *oc) {
  Section * section = &oc->sections[relaTab->targetSectionIndex];

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

  IF_DEBUG(linker, debugBelch("%s: P 0x%lx S 0x%lx %s GOT_S 0x%lx A 0x%lx relNo %u\n",
                              relocationTypeToString(rel->r_info), P, S,
                              symbol->name, GOT_S, A, relNo));
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
    // Quoting LLVM docs: For R_RISCV_PC_INDIRECT (R_RISCV_PCREL_LO12_{I,S}),
    // the symbol actually points the corresponding R_RISCV_PCREL_HI20
    // relocation, and the target VA is calculated using PCREL_HI20's symbol.
  case R_RISCV_PCREL_LO12_S:
    FALLTHROUGH;
  case R_RISCV_PCREL_LO12_I: {
    // Lookup related HI20 relocation and use that value. I'm still confused why
    // relocations aren't pure, but this is how LLVM does it. And, calculating
    // the lower 12 bit without any relation ship to the GOT entry's address
    // makes no sense either.
      for (unsigned i = relNo; i >= 0 ; i--) {
        Elf_Rela *rel_prime = &relaTab->relocations[i];

        addr_t P_prime =
            (addr_t)((uint8_t *)section->start + rel_prime->r_offset);

        if (P_prime != S) {
          // S points to the P of the corresponding *_HI20 relocation.
          continue;
        }

        ElfSymbol *symbol_prime =
            findSymbol(oc, relaTab->sectionHeader->sh_link,
                       ELF64_R_SYM((Elf64_Xword)rel_prime->r_info));

        CHECK(0x0 != symbol_prime);

        /* take explicit addend */
        int64_t addend_prime = rel_prime->r_addend;

        uint64_t type_prime = ELF64_R_TYPE(rel_prime->r_info);

        if (type_prime == R_RISCV_PCREL_HI20 ||
            type_prime == R_RISCV_GOT_HI20 ||
            type_prime == R_RISCV_TLS_GD_HI20 ||
            type_prime == R_RISCV_TLS_GOT_HI20) {
          IF_DEBUG(linker,
                   debugBelch(
                       "Found matching relocation: %s (P: 0x%lx, S: 0x%lx, "
                       "sym-name: %s) -> %s (P: 0x%lx, S: 0x%lx, sym-name: %s, relNo: %u)",
                       relocationTypeToString(rel->r_info), P, S, symbol->name,
                       relocationTypeToString(rel_prime->r_info), P_prime,
                       symbol_prime->addr, symbol_prime->name, i));
          int32_t result = computeAddend(relaTab, i, (Elf_Rel *)rel_prime,
                                         symbol_prime, addend_prime, oc);
          IF_DEBUG(linker, debugBelch("Result of computeAddend: 0x%x (%d)\n",
                                      result, result));
          return result;
        }
    }
    debugBelch("Missing HI relocation for %s: P 0x%lx S 0x%lx %s\n",
               relocationTypeToString(rel->r_info), P, S, symbol->name);
    abort();
  }

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
      if (makeStub(section, (void **)&S, (void *)GOT_S, 0)) {
        abort(/* could not find or make stub */);
      }
    }
    IF_DEBUG(linker, debugBelch("R_RISCV_CALL_PLT: S = 0x%lx A = 0x%lx P = "
                                "0x%lx (S + A) - P = 0x%lx \n",
                                S, A, P, (S + A) - P));
    return (S + A) - P;
  }
  case R_RISCV_ADD8:
    FALLTHROUGH;
  case R_RISCV_ADD16:
    FALLTHROUGH;
  case R_RISCV_ADD32:
    FALLTHROUGH;
  case R_RISCV_ADD64:
    FALLTHROUGH;
    return S + A; // Add V when the value is set
  case R_RISCV_SUB6:
    FALLTHROUGH;
  case R_RISCV_SUB8:
    FALLTHROUGH;
  case R_RISCV_SUB16:
    FALLTHROUGH;
  case R_RISCV_SUB32:
    FALLTHROUGH;
  case R_RISCV_SUB64:
    FALLTHROUGH;
    return S + A; // Subtract from V when value is set
  case R_RISCV_SET6:
    FALLTHROUGH;
  case R_RISCV_SET8:
    FALLTHROUGH;
  case R_RISCV_SET16:
    FALLTHROUGH;
  case R_RISCV_SET32:
    FALLTHROUGH;
    return S + A;
  case R_RISCV_RELAX:
  case R_RISCV_ALIGN:
    // I guess we don't need to implement this relaxation. Otherwise, this
    // should return the number of blank bytes to insert via NOPs.
    return 0;
  case R_RISCV_32_PCREL:
    return S + A - P;
  case R_RISCV_GOT_HI20: {
    // TODO: Allocating extra memory for every symbol just to play this trick
    // seems to be a bit obscene. (GOT relocations hitting local symbols
    // happens, but not very often.) It would be better to allocate only what we
    // really need.

    // There are two cases here: 1. The symbol is public and has an entry in the
    // GOT. 2. It's local and has no corresponding GOT entry. The first case is
    // easy: We simply calculate the addend with the GOT address. In the second
    // case we create a symbol extra entry and pretend it's the GOT.
    if (GOT_S != 0) {
      // 1. Public symbol with GOT entry.
      return GOT_S + A - P;
    } else {
      // 2. Fake GOT entry with symbol extra entry.
      SymbolExtra *symbolExtra = makeSymbolExtra(oc, ELF_R_SYM(rel->r_info), S);
      addr_t* FAKE_GOT_S = &symbolExtra->addr;
      addr_t res = (addr_t) FAKE_GOT_S + A - P;
      IF_DEBUG(linker, debugBelch("R_RISCV_GOT_HI20 w/ SymbolExtra = %p , "
                                  "entry = 0x%lx , reloc-addend = 0x%lu ",
                                  symbolExtra, FAKE_GOT_S, res));
      return res;
    }
  }
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

      addend = computeAddend((ElfRelocationATable*) relTab, i, rel, symbol, addend, oc);
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

      addend = computeAddend(relaTab, i, (Elf_Rel *)rel, symbol, addend, oc);
      encodeAddendRISCV64(targetSection, (Elf_Rel *)rel, addend);
    }
  }
  return EXIT_SUCCESS;
}

#endif /* OBJECTFORMAT_ELF */
#endif /* riscv64_HOST_ARCH */