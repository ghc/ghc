#ifndef HSVERSIONS_H
#define HSVERSIONS_H

#if 0

IMPORTANT!  If you put extra tabs/spaces in these macro definitions,
you will screw up the layout where they are used in case expressions!

(This is cpp-dependent, of course)

#endif

/* Useful in the headers that we share with the RTS */
#define COMPILING_GHC 1

/* Pull in all the platform defines for this build (foo_TARGET_ARCH etc.) */
#include "ghc_boot_platform.h"

/* Pull in the autoconf defines (HAVE_FOO), but don't include
 * ghcconfig.h, because that will include ghcplatform.h which has the
 * wrong platform settings for the compiler (it has the platform
 * settings for the target plat instead). */
#include "../includes/ghcautoconf.h"

#define GLOBAL_VAR(name,value,ty)  \
{-# NOINLINE name #-};             \
name :: IORef (ty);                \
name = Util.global (value);

#define GLOBAL_VAR_M(name,value,ty) \
{-# NOINLINE name #-};              \
name :: IORef (ty);                 \
name = Util.globalM (value);

#define ASSERT(e)      if debugIsOn && not (e) then (assertPanic __FILE__ __LINE__) else
#define ASSERT2(e,msg) if debugIsOn && not (e) then (assertPprPanic __FILE__ __LINE__ (msg)) else
#define WARN( e, msg ) (warnPprTrace (e) __FILE__ __LINE__ (msg)) $

-- Examples:   Assuming   flagSet :: String -> m Bool
--
--    do { c   <- getChar; MASSERT( isUpper c ); ... }
--    do { c   <- getChar; MASSERT2( isUpper c, text "Bad" ); ... }
--    do { str <- getStr;  ASSERTM( flagSet str ); .. }
--    do { str <- getStr;  ASSERTM2( flagSet str, text "Bad" ); .. }
--    do { str <- getStr;  WARNM2( flagSet str, text "Flag is set" ); .. }
#define MASSERT(e)      ASSERT(e) return ()
#define MASSERT2(e,msg) ASSERT2(e,msg) return ()
#define ASSERTM(e)      do { bool <- e; MASSERT(bool) }
#define ASSERTM2(e,msg) do { bool <- e; MASSERT2(bool,msg) }
#define WARNM2(e,msg)   do { bool <- e; WARN(bool, msg) return () }

#endif /* HsVersions.h */

