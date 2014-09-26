/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2013-
 *
 * Check whether dynamically-loaded object code can be safely
 * unloaded, by searching for references to it from the heap and RTS
 * data structures.
 *
 * --------------------------------------------------------------------------*/

#ifndef CHECKUNLOAD_H
#define CHECKUNLOAD_H

#include "BeginPrivate.h"

void checkUnload (StgClosure *static_objects);

#include "EndPrivate.h"

#endif // CHECKUNLOAD_H
