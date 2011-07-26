/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2011
 *
 * Architecture-specific defines for ARM
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

#ifndef ARCH_ARM_H
#define ARCH_ARM_H

#if defined(__ARM_ARCH_2__) || defined(__ARM_ARCH_3__) || defined(__ARM_ARCH_3M__) || \
    defined(__ARM_ARCH_4__) || defined(__ARM_ARCH_4T__) || defined(__ARM_ARCH_5__) || \
    defined(__ARM_ARCH_5T__) || defined(__ARM_ARCH_5E__) || defined(__ARM_ARCH_5TE__)
#define arm_HOST_ARCH_PRE_ARMv6
#endif

#if defined(arm_HOST_ARCH_PRE_ARMv6) || defined(__ARM_ARCH_6__) || defined(__ARM_ARCH_6J__) || \
    defined(__ARM_ARCH_6T2__) || defined(__ARM_ARCH_6Z__) || defined(__ARM_ARCH_6ZK__) || \
    defined(__ARM_ARCH_6M__)
#define arm_HOST_ARCH_PRE_ARMv7
#endif

#endif

