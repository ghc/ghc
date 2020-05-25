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

// Currently live objects
extern ObjectCode *objects;

// Root set for object collection
extern ObjectCode *loaded_objects;

// Mark bit for live objects
extern uint8_t object_code_mark_bit;

void initUnloadCheck(void);
void exitUnloadCheck(void);

// Call before major GC to prepare section index table for marking
void prepareUnloadCheck(void);

// Mark object code of a static closure address as 'live'
void markObjectCode(const void *addr);

// Call after major GC to unload unused and unmarked object code
void checkUnload(void);

// Call on loaded object code
void insertOCSectionIndices(ObjectCode *oc);

#include "EndPrivate.h"
