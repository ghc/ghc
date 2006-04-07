/* -----------------------------------------------------------------------------
 * ParInit.h,1
 * 
 * Phil Trinder
 * July 1998
 *
 * External Parallel Initialisation Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef PARINIT_H
#define PARINIT_H

extern void RunParallelSystem (P_);
extern void initParallelSystem(void);
extern void SynchroniseSystem(void);
extern void par_exit(I_);

#endif /* PARINIT_H */
