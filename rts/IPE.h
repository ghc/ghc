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
void initIpe(void);
void exitIpe(void);

#include "EndPrivate.h"
