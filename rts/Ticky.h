/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1999
 *
 * Header for Ticky.c
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

void PrintTickyInfo(void);

void emitTickyCounterSamples(void);
void emitTickyCounterDefs(void);

#include "EndPrivate.h"
