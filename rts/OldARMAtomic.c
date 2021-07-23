/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2011
 *
 * Inefficient but necessary atomic locks used for implementing atomic
 * operations on ARM architectures pre-ARMv6.
 *
 * These operations are not only referenced in the threaded RTS, but also in
 * ghc (the library), via the operations in compiler/cbits/genSym.c.
 * They are not actually called in a non-threaded environment, but we still
 * need them in every RTS to make the linker happy, hence no
 * #if defined(THREADED_RTS) here. See #8951.
 *
 * -------------------------------------------------------------------------- */

#include "rts/PosixSource.h"
#include "Rts.h"

#if defined(HAVE_SCHED_H)
#include <sched.h>
#endif

#if defined(arm_HOST_ARCH) && defined(arm_HOST_ARCH_PRE_ARMv6)

static volatile int atomic_spin = 0;

static int arm_atomic_spin_trylock (void)
{
  int result;

  asm volatile (
    "swp %0, %1, [%2]\n"
    : "=&r,&r" (result)
    : "r,0" (1), "r,r" (&atomic_spin)
    : "memory");
  if (result == 0)
    return 0;
  else
    return -1;
}

void arm_atomic_spin_lock()
{
  while (arm_atomic_spin_trylock())
#if defined(HAVE_SCHED_H)
    sched_yield();
#else
    ;  // inefficient on non-POSIX.
#endif
}

void arm_atomic_spin_unlock()
{
  atomic_spin = 0;
}

#endif  /* arm_HOST_ARCH && defined(arm_HOST_ARCH_PRE_ARMv6) */
