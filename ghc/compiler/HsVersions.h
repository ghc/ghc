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

#if __GLASGOW_HASKELL__ >= 504

#define CONCURRENT  Control.Concurrent
#define EXCEPTION   Control.Exception
     /* If you want Control.Exception.try, get it as Panic.try, which
        deals with the shift from 'tryAllIO' to 'try'.  */
#define DYNAMIC     Data.Dynamic
#define GLAEXTS     GHC.Exts
#define DATA_BITS   Data.Bits
#define DATA_INT    Data.Int
#define DATA_WORD   Data.Word
#define UNSAFE_IO   System.IO.Unsafe
#define TRACE       Debug.Trace
#define DATA_IOREF  Data.IORef
#define FIX_IO      System.IO
#define MONAD_ST    Control.Monad.ST
#define ST_ARRAY    Data.Array.ST

#else

#define CONCURRENT  Concurrent
#define EXCEPTION   Exception
#define DYNAMIC     Dynamic
#define GLAEXTS     GlaExts
#define DATA_BITS   Bits
#define DATA_INT    Int
#define DATA_WORD   Word
#define UNSAFE_IO   IOExts
#define TRACE       IOExts
#define DATA_IOREF  IOExts
#define FIX_IO      IOExts
#define MONAD_ST    ST
#define ST_ARRAY    ST

#endif

#ifdef __GLASGOW_HASKELL__
#define GLOBAL_VAR(name,value,ty)  \
name = Util.global (value) :: IORef (ty); \
{-# NOINLINE name #-}
#endif

#if __GLASGOW_HASKELL__ >= 620
#define UNBOX_FIELD !!
#else
#define UNBOX_FIELD !
#endif

#define COMMA ,

#ifdef DEBUG
#define ASSERT(e) if (not (e)) then (assertPanic __FILE__ __LINE__) else
#define ASSERT2(e,msg) if (not (e)) then (assertPprPanic __FILE__ __LINE__ (msg)) else
#define WARN( e, msg ) (warnPprTrace (e) __FILE__ __LINE__ (msg))
#define ASSERTM(mbool) do { bool <- mbool; ASSERT(bool) return () }
#define ASSERTM2(mbool,msg) do { bool <- mbool; ASSERT2(bool,msg) return () }
#else
#define ASSERT(e)      if False then error "ASSERT"  else
#define ASSERT2(e,msg) if False then error "ASSERT2" else
#define ASSERTM(e)
#define ASSERTM2(e,msg)
#define WARN(e,msg)    if False then error "WARN"    else
#endif

-- This #ifndef lets us switch off the "import FastString"
-- when compiling FastString itself
#ifndef COMPILING_FAST_STRING
-- 
import qualified FastString 
#endif

#define SLIT(x)	 (FastString.mkLitString# (x#))
#define FSLIT(x) (FastString.mkFastString# (x#))

#endif /* HsVersions.h */

