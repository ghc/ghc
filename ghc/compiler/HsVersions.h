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
#else
#define ASSERT(e)
#endif

#if __STDC__
#define CAT2(a,b)a##b
#else
#define CAT2(a,b)a/**/b
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ == 201
# define REALLY_HASKELL_1_3
# define SYN_IE(a) a
# define EXP_MODULE(a) module a
# define IMPORT_DELOOPER(mod) import mod
# define IMPORT_1_3(mod) import mod
# define _tagCmp compare
# define _LT LT
# define _EQ EQ
# define _GT GT
# define _Addr GHCbase.Addr
# define _ByteArray GHCbase.ByteArray
# define _MutableByteArray GHCbase.MutableByteArray
# define _MutableArray GHCbase.MutableArray
# define _RealWorld GHCbase.RealWorld
# define _ST GHCbase.ST
# define _ForeignObj GHCbase.ForeignObj
# define _runST STbase.runST
# define failWith fail
# define MkST ST
# define MkIOError(h,errt,msg) (errt msg)
# define Text Show
# define IMP_FASTSTRING()
# define IMP_Ubiq() IMPORT_DELOOPER(Ubiq); import qualified GHCbase
# define CHK_Ubiq() IMPORT_DELOOPER(Ubiq); import qualified GHCbase
# define minInt (minBound::Int)
# define maxInt (maxBound::Int)
#elif defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 202
# define REALLY_HASKELL_1_3
# define SYN_IE(a) a
# define EXP_MODULE(a) module a
# define IMPORT_DELOOPER(mod) import mod
# define IMPORT_1_3(mod) import mod
# define _CMP_TAG Ordering
# define _tagCmp compare
# define _LT LT
# define _EQ EQ
# define _GT GT
# define _Addr GlaExts.Addr
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
# define failWith fail
# define MkIOError(h,errt,msg) (IOError (Just h) errt msg)
# define CCALL_THEN thenIO_Prim
# define _filePtr IOHandle.filePtr
# define Text Show
# define IMP_FASTSTRING() import FastString
# define IMP_Ubiq() import GlaExts ; import FastString
# define CHK_Ubiq() import GlaExts ; import FastString
# define minInt (minBound::Int)
# define maxInt (maxBound::Int)
#else
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

#if __GLASGOW_HASKELL__ >= 26 && __GLASGOW_HASKELL__ < 200
#define trace _trace
#endif

#define TAG_ Int#
#define LT_ -1#
#define EQ_ 0#
#define GT_ 1#
#define GT__ _

#if defined(__GLASGOW_HASKELL__)
#define FAST_INT Int#
#define ILIT(x) (x#)
#define IBOX(x) (I# (x))
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
#define _ADD_ `plusInt#`
#define _SUB_ `minusInt#`
#define _MUL_ `timesInt#`
#define _DIV_ `divInt#`
#define _QUOT_ `quotInt#`
#define _NEG_ negateInt#
#define _EQ_ `eqInt#`
#define _LT_ `ltInt#`
#define _LE_ `leInt#`
#define _GE_ `geInt#`
#define _GT_ `gtInt#`
#else
#define _ADD_ +#
#define _SUB_ -#
#define _MUL_ *#
#define _DIV_ /#
#define _QUOT_ `quotInt#`
#define _NEG_ negateInt#
#define _EQ_ ==#
#define _LT_ <#
#define _LE_ <=#
#define _GE_ >=#
#define _GT_ >#
#endif

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
# define USE_FAST_STRINGS 1
# if __GLASGOW_HASKELL__ < 200 || __GLASGOW_HASKELL__ >= 202
#  define FAST_STRING	FastString {-_PackedString -}
#  if __GLASGOW_HASKELL__ < 200
#    define SLIT(x)	(mkFastCharString (A# (x#)))
#  else
#    define SLIT(x)	(mkFastCharString (GlaExts.A# (x#)))
#  endif
#  define _CMP_STRING_	cmpPString
	/* cmpPString defined in utils/Util.lhs */
#  define _NULL_	nullFastString {-_nullPS-}
#  define _NIL_		(mkFastString "") {-_nilPS -}
#  define _CONS_	consFS {-_consPS-}
#  define _HEAD_	headFS {-_headPS-}
#  define _TAIL_	tailFS {-_tailPS-} 
#  define _LENGTH_	lengthFS {-_lengthPS-}
#  define _PK_		mkFastString {-_packString-}
#  define _UNPK_	unpackFS {-_unpackPS-}
     /* #  define _SUBSTR_	_substrPS */
#  define _APPEND_	`appendFS` {-`_appendPS`-}
#  define _CONCAT_	concatFS {-_concatPS-}
# else
#  define FAST_STRING	GHCbase.PackedString
#  define SLIT(x)	(packCString (GHCbase.A# x#))
#  define _CMP_STRING_	cmpPString
#  define _NULL_	nullPS
#  define _NIL_		nilPS
#  define _CONS_	consPS
#  define _HEAD_	headPS
#  define _TAIL_	tailPS
#  define _LENGTH_	lengthPS
#  define _PK_		packString
#  define _UNPK_	unpackPS
#  define _SUBSTR_	substrPS
#  define _APPEND_	`appendPS`
#  define _CONCAT_	concatPS
# endif
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
