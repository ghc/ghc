/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2025
 *
 * Prototypes for functions in AllocArray.c
 *
 * RTS internal utilities for allocating arrays of pointers (StgMutArrPtrs) and
 * arrays of bytes (StgArrBytes).
 * -------------------------------------------------------------------------*/

#pragma once

#include "Capability.h"

#include "BeginPrivate.h"

/* All these allocation functions return NULL on failure. */

/* Allocate a StgMutArrPtrs for a given number of elements. It is allocated in
 * the DIRTY state.
 */
StgMutArrPtrs *allocateMutArrPtrs (Capability *cap,
                                   StgWord nelements,
                                   CostCentreStack *ccs);

/* Allocate a StgSmallMutArrPtrs for a given number of elements.
 */
StgSmallMutArrPtrs *allocateSmallMutArrPtrs (Capability *cap,
                                             StgWord nelements,
                                             CostCentreStack *ccs);

/* Allocate a StgArrBytes for a given number of bytes.
 */
StgArrBytes *allocateArrBytes (Capability *cap,
                               StgWord nbytes,
                               CostCentreStack *ccs);

/* Allocate a pinned (and optionally aligned) StgArrBytes for a given number
 * of bytes.
 */
StgArrBytes *allocateArrBytesPinned (Capability *cap,
                                     StgWord nbytes,
                                     StgWord alignment,
                                     CostCentreStack *ccs);

#include "EndPrivate.h"
