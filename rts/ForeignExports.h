/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2020
 *
 * Management of foreign exports.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"
#include "Rts.h"
#include "LinkerInternals.h"

void foreignExportsLoadingObject(ObjectCode *oc);
void foreignExportsFinishedLoadingObject(void);
void processForeignExports(void);

#include "EndPrivate.h"

