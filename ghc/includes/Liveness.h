/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2004
 *
 * Building liveness masks for RET_DYN stack frames.
 * A few macros that are used in both .cmm and .c sources.
 *
 * A liveness mask is constructed like so:
 *
 *    R1_PTR & R2_PTR & R3_PTR
 *
 * -------------------------------------------------------------------------- */

#ifndef LIVENESS_H
#define LIVENESS_H

#define NO_PTRS   0xff
#define R1_PTR	  (NO_PTRS ^ (1<<0))
#define R2_PTR	  (NO_PTRS ^ (1<<1))
#define R3_PTR	  (NO_PTRS ^ (1<<2))
#define R4_PTR	  (NO_PTRS ^ (1<<3))
#define R5_PTR	  (NO_PTRS ^ (1<<4))
#define R6_PTR	  (NO_PTRS ^ (1<<5))
#define R7_PTR	  (NO_PTRS ^ (1<<6))
#define R8_PTR	  (NO_PTRS ^ (1<<7))

#define N_NONPTRS(n)  ((n)<<16)
#define N_PTRS(n)     ((n)<<24)

#define RET_DYN_NONPTRS(l) ((l)>>16 & 0xff)
#define RET_DYN_PTRS(l)    ((l)>>24 & 0xff)
#define RET_DYN_LIVENESS(l) ((l) & 0xffff)

#endif /* LIVENESS_H */
