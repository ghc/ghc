/*
 * (c) Juan Quintela, Universidade da Corunha 1998
 * 
 * wrappers for signal funcions
 * 
 * sigset_t is a struct in some UNIXes (LINUX/glibc for instance)
 * and it is not posible to do the inline (_casm_). These functions 
 * aren't inline because it causes gcc to run out of registers on x86.
 *
 * Ugly casting added by SUP to avoid C compiler warnings about
 * incompatible pointer types.
 */

#include "Rts.h"
#include "libposix.h"

void
stg_sigaddset(StgByteArray newset, StgByteArray oldset, int signum)
{
	*((sigset_t *)newset) = *((sigset_t *)oldset);
	sigaddset((sigset_t *)newset, signum);
}

void
stg_sigdelset(StgByteArray newset, StgByteArray oldset, int signum)
{
	*((sigset_t *)newset) = *((sigset_t *)oldset);
	sigdelset((sigset_t *)newset, signum);
}
