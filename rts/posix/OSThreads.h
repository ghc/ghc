/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2009
 *
 * Extra posix-specific thread functions
 *
 * See rts/include/rts/OSThreads.h for the OS-independent API
 *
 * --------------------------------------------------------------------------*/

#pragma once

// Like waitConditionTimedRelative but the expiry time is given as an absolute
// time in the system's monotonic clock.
// Returns false on timeout, true otherwise.
bool waitConditionTimedAbsolute (Condition* cond, Mutex* mutex, Time expiry);

