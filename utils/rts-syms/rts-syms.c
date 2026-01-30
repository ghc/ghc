/* A utility to export the symbol table of the RTS. The RTS has a built-in
 * linker, and has a pre-populated table of known RTS symbols.
 *
 * This is used primarily to generate input files for linkers, to limit the
 * symbols exported from the RTS to those we want to export.
 *
 * This utility can generate Windows .def files (for making DLLs), or GNU ld
 * linker scripts (used by GNU ld and LLVM ld for .so libs). We also support
 * a raw dump format for curiosity or debugging.
 */

#include "RtsSymbols.h"

/* RtsSymbols.h is an internal header file.
 * It defines a symbol table (reordered and simplified for clarity):

extern RtsSymbolVal rtsSyms;

typedef struct _RtsSymbolVal {
      const SymbolName* lbl;
      SymbolAddr* addr;
      SymStrength strength;
      SymType type;
  } RtsSymbolVal;

typedef enum _SymType {
      SYM_TYPE_CODE,
      SYM_TYPE_DATA,
      SYM_TYPE_INDIRECT_DATA,
      SYM_TYPE_DUP_DISCARD,
      SYM_TYPE_HIDDEN,
  } SymType;

  typedef enum _SymStrength {
      STRENGTH_NORMAL,
      STRENGTH_WEAK,
      STRENGTH_STRONG,
  } SymStrength;

 */

#include <stdio.h>

void dump_nm_bsd(void);
void dump_nm_posix(void);
void init_ghc_hs_iface(void);

int main (int argc, char *argv[]) {
  //TODO: formats: raw, map and def
  dump_nm_posix();
}

char *format_sym_type(SymType type);
char *format_sym_strength(SymStrength strength);

void dump_nm_bsd() {
  for (int i = 0; rtsSyms[i].addr != 0; i++) {
    RtsSymbolVal *sym = &rtsSyms[i];
    printf("%.16lx %s%s %s\n", (unsigned long)(sym->addr),
                               format_sym_strength(sym->strength),
                               format_sym_type(sym->type),
                               sym->lbl);
  }
}

void dump_nm_posix() {
  for (int i = 0; rtsSyms[i].addr != 0; i++) {
    RtsSymbolVal *sym = &rtsSyms[i];
    printf("%s %s\n", sym->lbl, format_sym_type(sym->type));
  }
}

char *format_sym_type(SymType type) {
  /* Ignore SYM_TYPE_DUP_DISCARD, SYM_TYPE_HIDDEN as they do not occur in
   * the RTS built-in symbol table. (They can occur in other loaded libraries).
   */
  switch (type & ~(SYM_TYPE_DUP_DISCARD | SYM_TYPE_HIDDEN | SYM_TYPE_RTS_DEF)) {
    case SYM_TYPE_CODE: return "T";
    case SYM_TYPE_DATA: return "D";
    case SYM_TYPE_INDIRECT_DATA: return "I";
    default: return " ";
  }
}

char *format_sym_strength(SymStrength strength) {
  switch (strength) {
    case STRENGTH_NORMAL: return " ";
    case STRENGTH_WEAK:   return "W";
    case STRENGTH_STRONG: return "S";
    default:              return " ";
  }
}

void init_ghc_hs_iface(void) {
  return;
};

