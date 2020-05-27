/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2013-
 *
 * Check whether dynamically-loaded object code can be safely
 * unloaded, by searching for references to it from the heap and RTS
 * data structures.
 *
 * --------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

#include "LinkerInternals.h"

// Current loaded objects
extern ObjectCode *objects;

void initUnloadCheck(void);
void exitUnloadCheck(void);
void checkUnload(StgClosure *static_objects);

// Call on loaded object codes
void insertOCSectionIndices(ObjectCode *oc);

#include "EndPrivate.h"
