/* -----------------------------------------------------------------------------
 * Common utilities used in adjustor implementations
 * ---------------------------------------------------------------------------*/

#pragma once

/* Number of allocated adjustors for accounting purposes. Accessed via atomics. */
extern StgWord n_allocd_adjustors;

int totalArgumentSize(const char *typeString);

void initAdjustors(void);
