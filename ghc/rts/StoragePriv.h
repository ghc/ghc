/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Storage manager bits visible to the rest of the RTS only
 *
 * ---------------------------------------------------------------------------*/

#ifndef STORAGEPRIV_H
#define STORAGEPRIV_H

/*
 * Storage manager mutex
 */
#if defined(SMP)
extern Mutex sm_mutex;
#define ACQUIRE_SM_LOCK   ACQUIRE_LOCK(&sm_mutex)
#define RELEASE_SM_LOCK   RELEASE_LOCK(&sm_mutex)
#else
#define ACQUIRE_SM_LOCK
#define RELEASE_SM_LOCK
#endif

#endif /* STORAGEPRIV_H */
