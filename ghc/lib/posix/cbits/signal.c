/*
 * (c) Juan Quintela, Universidade da Corunha 1998
 * 
 * wrappers for signal funcions
 * 
 * sigset_t is a struct in some UNIXes (LINUX/glibc for instance)
 * and it is not posible to do the inline (_casm_). These functions 
 * aren't inline because it causes gcc to run out of registers on x86.
 *
 */

#include "Rts.h"
#include "libposix.h"

void
stg_sigaddset(sigset_t *newset, sigset_t *oldset, int signum)
{
	*newset = *oldset;
	sigaddset(newset, signum);
}

void
stg_sigdelset(sigset_t *newset, sigset_t *oldset, int signum)
{
	*newset = *oldset;
	sigdelset(newset, signum);
}
