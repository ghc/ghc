/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2015
 *
 * RTS Symbol Info
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "LinkerInternals.h"
#include <stdbool.h>

/* See Note [BFD import library].  */
typedef enum _SymbolKind {
    KIND_NORMAL = 0x01,
    KIND_WEAK   = 0x02,
    KIND_IMPORT = 0x04
} SymbolKind;

typedef struct _SymbolInfo {
    /* Determines what kind of symbol we are storing.  */
    SymbolKind kind;
} SymbolInfo;

RTS_PUBLIC bool isSymbolWeak(ObjectCode *owner, const void *label);
RTS_PUBLIC bool isSymbolImport(ObjectCode *owner, const void *label);
RTS_PUBLIC void setWeakSymbol(ObjectCode *owner, const void *label);
RTS_PUBLIC void setImportSymbol(ObjectCode *owner, const void *label);
RTS_PUBLIC void clearImportSymbol(ObjectCode *owner, const void *label);

typedef void (*symbolUpdater)(SymbolInfo*);
RTS_PUBLIC void setSymbolInfo(ObjectCode *owner, const void *label, symbolUpdater updater);
