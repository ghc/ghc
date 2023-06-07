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

void dumpIPEToEventLog(void);
void updateIpeMap(void);
void initIpe(void);
void exitIpe(void);
void decompressIPEBufferListNodeIfCompressed(IpeBufferListNode*, IpeBufferEntry**, char**);

#include "EndPrivate.h"
