#ifndef HSVERSIONS_H
#define HSVERSIONS_H

#if 0

IMPORTANT!  If you put extra tabs/spaces in these macro definitions,
you will screw up the layout where they are used in case expressions!

(This is cpp-dependent, of course)

#endif

#define COMMA ,

#ifdef DEBUG
#define ASSERT(e) if (not (e)) then (assertPanic __FILE__ __LINE__) else
#define ASSERT2(e,msg) if (not (e)) then (assertPprPanic __FILE__ __LINE__ (msg)) else
#else
#define ASSERT(e)
#define ASSERT2(e,msg)
#endif

#if __STDC__
#define CAT2(a,b)a##b
#else
#define CAT2(a,b)a/**/b
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 202
# define REALLY_HASKELL_1_3
# define SYN_IE(a) a
# define EXP_MODULE(a) module a
# define IMPORT_DELOOPER(mod) import mod
# define IMPORT_1_3(mod) import mod
# define _Addr Addr
# define _ByteArray GlaExts.ByteArray
# define _MutableByteArray GlaExts.MutableByteArray
# define _MutableArray GlaExts.MutableArray
# define _RealWorld GlaExts.RealWorld
# define _ST GlaExts.ST
# define _ForeignObj Foreign.ForeignObj
# define _runST ST.runST
# define seqStrictlyST seqST
# define thenStrictlyST thenST
# define returnStrictlyST return
# define _readHandle IOHandle.readHandle
# define _writeHandle IOHandle.writeHandle
# define _newHandle   IOHandle.newdHandle
# define MkST ST
# if __GLASGOW_HASKELL__ >= 209
#  define STATE_TOK(x)    x
#  define ST_RET(x,y)     STret (y) (x)
#  define unsafePerformST(x)  runST (x)
#  define ST_TO_PrimIO(x) (stToIO (x))
# else
#  define STATE_TOK(x)  (S# x)
#  define ST_RET(x,y)   (x,y)
#  define unsafePerformST(x) unsafePerformPrimIO(x)
#  define ST_TO_PrimIO(x) x
# endif
# define failWith fail
# define MkIOError(h,errt,msg) (IOError (Just h) errt msg)
# define CCALL_THEN thenIO_Prim
# define _filePtr IOHandle.filePtr
# define Text Show
# define IMP_FASTSTRING() import FastString
# if __GLASGOW_HASKELL__ >= 209
#  define IMP_Ubiq() import GlaExts ; import Addr(Addr(..)); import FastString
#  define CHK_Ubiq() import GlaExts ; import Addr(Addr(..)); import FastString
# else
#  define IMP_Ubiq() import GlaExts ; import FastString
#  define CHK_Ubiq() import GlaExts ; import FastString
# endif
# define minInt (minBound::Int)
# define maxInt (maxBound::Int)
#else
# define STATE_TOK(x)  (S# x)
# define ST_RET(x,y)   (x,y)
# define unsafePerformST(x) unsafePerformPrimIO(x)
# define ST_TO_PrimIO(x) x
# define SYN_IE(a) a(..)
# define EXP_MODULE(a) a..
# define IMPORT_DELOOPER(mod) import mod
# define IMPORT_1_3(mod) {--}
# define IMP_FASTSTRING() import FastString
# define IMP_Ubiq() IMPORT_DELOOPER(Ubiq) ; import FastString
# define CHK_Ubiq() IMPORT_DELOOPER(Ubiq) ; import FastString
# define MkST
# define CCALL_THEN thenPrimIO
# define MkIOError(h,errt,msg) (errt msg)
#endif

#if defined(__GLASGOW_HASKELL__)

-- Import the beggars
import GlaExts	( Int(..), Int#, (+#), (-#), (*#), 
		  quotInt#, negateInt#, (==#), (<#), (<=#), (>=#), (>#)
		)

#define FAST_INT Int#
#define ILIT(x) (x#)
#define IBOX(x) (I# (x))
#define _ADD_ +#
#define _SUB_ -#
#define _MUL_ *#
#define _QUOT_ `quotInt#`
#define _NEG_ negateInt#
#define _EQ_ ==#
#define _LT_ <#
#define _LE_ <=#
#define _GE_ >=#
#define _GT_ >#

#define FAST_BOOL Int#
#define _TRUE_ 1#
#define _FALSE_ 0#
#define _IS_TRUE_(x) ((x) _EQ_ 1#)

#else {- ! __GLASGOW_HASKELL__ -}

#define FAST_INT Int
#define ILIT(x) (x)
#define IBOX(x) (x)
#define _ADD_ +
#define _SUB_ -
#define _MUL_ *
#define _DIV_ `div`
#define _QUOT_ `quot`
#define _NEG_ -
#define _EQ_ ==
#define _LT_ <
#define _LE_ <=
#define _GE_ >=
#define _GT_ >

#define FAST_BOOL Bool
#define _TRUE_ True
#define _FALSE_ False
#define _IS_TRUE_(x) (x)

#endif  {- ! __GLASGOW_HASKELL__ -}

#if __GLASGOW_HASKELL__ >= 23

-- This #ifndef lets us switch off the "import FastString"
-- when compiling FastString itself
#ifndef COMPILING_FAST_STRING
-- 
import FastString	( FastString, mkFastString, mkFastCharString#, nullFastString, 
			  consFS, headFS, tailFS, lengthFS, unpackFS, appendFS, concatFS
			)
#endif

# define USE_FAST_STRINGS 1
# define FAST_STRING	FastString
# define SLIT(x)	(mkFastCharString# (x#))
# define _NULL_		nullFastString
# define _NIL_		(mkFastString "")
# define _CONS_		consFS
# define _HEAD_		headFS
# define _TAIL_		tailFS
# define _LENGTH_	lengthFS
# define _PK_		mkFastString
# define _UNPK_		unpackFS
# define _APPEND_	`appendFS`
# define _CONCAT_	concatFS
#else
# define FAST_STRING String
# define SLIT(x)      (x)
# define _CMP_STRING_ cmpString
# define _NULL_	      null
# define _NIL_	      ""
# define _CONS_	      (:)
# define _HEAD_	      head
# define _TAIL_	      tail
# define _LENGTH_     length
# define _PK_	      (\x->x)
# define _UNPK_	      (\x->x)
# define _SUBSTR_     substr{-from Utils-}
# define _APPEND_     ++
# define _CONCAT_     concat
#endif

#endif
