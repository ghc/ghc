#ifndef HSVERSIONS_H
#define HSVERSIONS_H

#if 0

IMPORTANT!  If you put extra tabs/spaces in these macro definitions,
you will screw up the layout where they are used in case expressions!

(This is cpp-dependent, of course)

#endif

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

#define COMMA ,

#ifdef DEBUG
#define ASSERT(e) if (not (e)) then (assertPanic __FILE__ __LINE__) else
#define ASSERT2(e,msg) if (not (e)) then (assertPprPanic __FILE__ __LINE__ (msg)) else
#define WARN( e, msg ) (warnPprTrace (e) __FILE__ __LINE__ (msg))
#define ASSERTM(e) ASSERT(e) do
#else
#define ASSERT(e)
#define ASSERT2(e,msg)
#define ASSERTM(e)
#define WARN(e,msg)
#endif

-- temporary usage assertion control KSW 2000-10
#ifdef DO_USAGES
#define UASSERT(e) ASSERT(e)
#define UASSERT2(e,msg) ASSERT2(e,msg)
#else
#define UASSERT(e)
#define UASSERT2(e,msg)
#endif

-- This #ifndef lets us switch off the "import FastString"
-- when compiling FastString itself
#ifndef COMPILING_FAST_STRING
-- 
import qualified FastString 
#endif

#define SLIT(x)	 (FastString.mkLitString# (x#))
#define FSLIT(x) (FastString.mkFastString# (x#))

#endif // HSVERSIONS_H
