/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "sm/GC.h" // for evac_fn below

#include "BeginPrivate.h"

void    initStableNameTable   ( void );
void    freeSnEntry           ( snEntry *sn );
void    exitStableNameTable   ( void );
StgWord lookupStableName      ( StgPtr p );

void    rememberOldStableNameAddresses ( void );

void    threadStableNameTable ( evac_fn evac, void *user );
void    gcStableNameTable     ( void );
void    updateStableNameTable ( bool full );

void    stableNameLock            ( void );
void    stableNameUnlock          ( void );

extern unsigned int SNT_size;

#define FOR_EACH_STABLE_NAME(p, CODE)                                   \
    do {                                                                \
        snEntry *p;                                                     \
        snEntry *__end_ptr = &stable_name_table[SNT_size];              \
        for (p = stable_name_table + 1; p < __end_ptr; p++) {           \
            /* Internal pointers are free slots.  */                    \
            /* If p->addr == NULL, it's a */                            \
            /* stable name where the object has been GC'd, but the */   \
            /* StableName object (sn_obj) is still alive. */            \
            if ((p->addr < (P_)stable_name_table ||                     \
                 p->addr >= (P_)__end_ptr))                             \
            {                                                           \
                /* NOTE: There is an ambiguity here if p->addr == NULL */ \
                /* it is either the last item in the free list or it */ \
                /* is a stable name whose pointee died. sn_obj == NULL */ \
                /* disambiguates as last free list item. */             \
                do { CODE } while(0);                                   \
            }                                                           \
        }                                                               \
    } while(0)

#if defined(THREADED_RTS)
// needed by Schedule.c:forkProcess()
extern Mutex stable_name_mutex;
#endif

#include "EndPrivate.h"
