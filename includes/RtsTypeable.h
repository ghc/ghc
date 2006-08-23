/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006
 *
 *  Support for shared Typeable
 *
 * ---------------------------------------------------------------------------*/

#ifndef GHC_RTS_TYPEABLE_H
#define GHC_RTS_TYPEABLE_H

#include "Stg.h"

void initTypeableStore(void);
void exitTypeableStore(void);

StgPtr getOrSetTypeableStore(StgPtr);

#endif
