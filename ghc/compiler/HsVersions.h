#ifndef HSVERSIONS_H
#define HSVERSIONS_H

#if 0

IMPORTANT!  If you put extra tabs/spaces in these macro definitions,
you will screw up the layout where they are used in case expressions!

(This is cpp-dependent, of course)

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
#else
#define ASSERT(e)
#define ASSERT2(e,msg)
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
