#ifndef STGMACHDEPS_H
#define STGMACHDEPS_H

#define COMMENT__(a)

COMMENT__(The COMMON_ITBLS macro determines whether we use commoned-up
  info tables and rep tables instead of the old info table system.)

#define COMMON_ITBLS 1


COMMENT__(This code has to go through a Haskell compiler too)

COMMENT__(We assume 8 bit bytes.)
#define BITS_PER_BYTE 8
#define BITS_IN(x) (BITS_PER_BYTE * sizeof(x))

#ifdef __STDC__
#define PROTO(x)	x
#define NON_PROTO(x)	/* nothing */
#define STG_VOLATILE	volatile
#define STG_NO_ARGS	void
#define CAT2(a,b) a##b
#define CAT3(a,b,c) a##b##c
#define CAT4(a,b,c,d) a##b##c##d
#define CAT5(a,b,c,d,e) a##b##c##d##e
#define CAT6(a,b,c,d,e,f) a##b##c##d##e##f

#else
#define PROTO(x)	()
#define NON_PROTO(x)	x
#define STG_VOLATILE	/* no volatile */
#define STG_NO_ARGS	/* no such thing either */
#define CAT2(a,b) a/**/b
#define CAT3(a,b,c) a/**/b/**/c
#define CAT4(a,b,c,d) a/**/b/**/c/**/d
#define CAT5(a,b,c,d,e) a/**/b/**/c/**/d/**/e
#define CAT6(a,b,c,d,e,f) a/**/b/**/c/**/d/**/e/**/f
#endif /* ! __STDC__ */

#ifdef __GNUC__
#define STG_NORETURN	__attribute__((noreturn))
#define STG_INLINE __inline__
#else
#define STG_NORETURN	/* no such thing */
#define STG_INLINE /* no inline functions */
#endif

#if 0
------------------------------------------------------------------------
  Steve Maguires "Writing Solid Code" suggests that (in the debugging
  version) we should work hard to make bugs show themselves at the
  earliest possible moment.  

  In particular, it suggests that the memory management system should
  trash memory when it is allocated and when it is deallocated so that
  references to uninitialised memory or to released memory will show up
  as the bugs they are.

  By "trashing", I mean writing easily recognisable "nonsense" bit
  patterns over the block of memory.  It is worth taking some care to
  choose values which:

  1) Are meaningless pointers (ideally causing memory exceptions)

     (eg not word-aligned)

  2) Are "weird-looking" integers (whether treated as 8, 16, 32 or 64
     bits) (A large (definately non-zero) value).

  3) Make strange-looking strings when concatenated.

  4) Are meaningless machine code (ideally causing exceptions)

  We should also choose different values for initialisation and
  deallocation to make it easier to identify the source of the bug.

  ADR
------------------------------------------------------------------------
#endif /* 0 */


#if alpha_TARGET_ARCH
#define DEALLOCATED_TRASH   0xdeadbeefdeadbeef
#else
#define DEALLOCATED_TRASH   0xdeadbeef
#endif

#endif /* ! STGMACHDEPS_H */
