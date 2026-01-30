/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2015
 *
 * RTS Symbols
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "ghcautoconf.h"
#include "Hash.h"

#if defined(LEADING_UNDERSCORE)
#define MAYBE_LEADING_UNDERSCORE_STR(s) ("_" s)
#else
#define MAYBE_LEADING_UNDERSCORE_STR(s) (s)
#endif

typedef void SymbolAddr;
typedef char SymbolName;

/* What kind of thing a symbol identifies. We need to know this to determine how
 * to process overflowing relocations. See Note [Processing overflowed relocations].
 * This is bitfield however only the option SYM_TYPE_DUP_DISCARD and
 * SYM_TYPE_RTS_DEF can be combined with the other values. */
typedef enum _SymType {
    SYM_TYPE_CODE = 1 << 0, /* the symbol is a function and can be relocated via a jump island */
    SYM_TYPE_DATA = 1 << 1, /* the symbol is data */
    SYM_TYPE_INDIRECT_DATA = 1 << 2, /* see Note [_iob_func symbol] */
    SYM_TYPE_DUP_DISCARD = 1 << 3, /* the symbol is a symbol in a BFD import library
                                      however if a duplicate is found with a mismatching
                                      SymType then discard this one.  */
    SYM_TYPE_HIDDEN = 1 << 4, /* the symbol is hidden and should not be exported */
    SYM_TYPE_RTS_DEF = 1 << 5, /* the symbol is defined in the RTS DSO */
} SymType;

/* Note [RTS symbol exports]
 * SymType and SymStrength are used by the RTS's internal (aka GHCi) linker.
 * They're also used by the rtsSyms array, which is used to pre-populate the
 * GHCi linker symbol table (see ghciInsertSymbolTable calls in initLinker_).
 * The rtsSyms array has a secondary purpose: to be the source of truth for
 * which symbols are supposed to be exported from the RTS, when the RTS is
 * built as a shared object (i.e. .so, .dll), which is handled by the native
 * system linker.
 *
 * This is related but different to the GHCi linker. The GHCi linker's symbol
 * table is pre-populated with RTS exported symbols but also additional symbols
 * from dependent libraries and a few platform specific symbols and hacks (see
 * for example Note [Strong symbols], and Note [Symbols for MinGW's printf],
 * Note [Extra RTS symbols]). The GHCi linker does not need to distinguish
 * known symbols that are defined within the RTS from known symbols from other
 * libs. All of them are available to resolve against.
 *
 * So to serve the secondary purpose, we use the SYM_TYPE_RTS_DEF flag, which
 * we combine with the other flags (CODE, DATA etc). We arrange to ignore this
 * flag when pre-populating the GHCi linker symbol table. But we make use of it
 * to dump the symbols that are intended to be exported from the RTS. This can
 * be used by the build system and native linker to limit the symbols exported
 * from the RTS shared object. See utils/rts-sym/rts-sym.c
 */

typedef enum _SymStrength {
    STRENGTH_NORMAL,
    STRENGTH_WEAK,
    STRENGTH_STRONG,
} SymStrength;

typedef struct _RtsSymbolVal {
    const SymbolName* lbl;
    SymbolAddr* addr;
    SymStrength strength;
    SymType type;
} RtsSymbolVal;

void initLinkerRtsSyms (StrHashTable *symhash);

extern RtsSymbolVal* __attribute__((weak)) rtsExtraSyms(void);

/* See Note [_iob_func symbol].  */
#if defined(mingw32_HOST_OS)
extern const void* __rts_iob_func;
#endif
