/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2021
 *
 * Support for IPE
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "Rts.h"
#include <stdio.h>

#include "BeginPrivate.h"

#define IPE_LIST_NODE_BUFFER_SIZE 126

typedef struct IpeBufferListNode_ {
    InfoProvEnt **buffer[IPE_LIST_NODE_BUFFER_SIZE];
    StgWord8 count;
    struct IpeBufferListNode_ *next;
} IpeBufferListNode;

void dumpIPEToEventLog(void);
void updateIpeMap(void);
void initIpeMapLock(void);
void closeIpeMapLock(void);

#if defined(TRACING)
void traceIPEFromHashTable(void *data STG_UNUSED, StgWord key STG_UNUSED,
                           const void *value);
#endif

#include "EndPrivate.h"
