/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2015
 *
 * RTS Symbols
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "ghcautoconf.h"

#if defined(LEADING_UNDERSCORE)
#define MAYBE_LEADING_UNDERSCORE_STR(s) ("_" s)
#else
#define MAYBE_LEADING_UNDERSCORE_STR(s) (s)
#endif

typedef void SymbolAddr;
typedef char SymbolName;

/* What kind of thing a symbol identifies. We need to know this to determine how
 * to process overflowing relocations. See Note [Processing overflowed relocations].
 * This is bitfield however only the option SYM_TYPE_DUP_DISCARD can be combined
 * with the other values. */
typedef enum _SymType {
    SYM_TYPE_CODE = 1 << 0, /* the symbol is a function and can be relocated via a jump island */
    SYM_TYPE_DATA = 1 << 1, /* the symbol is data */
    SYM_TYPE_INDIRECT_DATA = 1 << 2, /* see Note [_iob_func symbol] */
    SYM_TYPE_DUP_DISCARD = 1 << 3, /* the symbol is a symbol in a BFD import library
                                      however if a duplicate is found with a mismatching
                                      SymType then discard this one.  */
    SYM_TYPE_HIDDEN = 1 << 4, /* the symbol is hidden and should not be exported */
} SymType;

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

extern RtsSymbolVal rtsSyms[];

extern RtsSymbolVal* __attribute__((weak)) rtsExtraSyms(void);

/* See Note [_iob_func symbol].  */
#if defined(mingw32_HOST_OS)
extern const void* __rts_iob_func;
#endif
