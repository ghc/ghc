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

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 200
# define REALLY_HASKELL_1_3
# define SYN_IE(a) a
# define EXP_MODULE(a) module a
# define IMPORT_DELOOPER(mod) import CAT2(mod,_1_3)
# define IMPORT_1_3(mod) import mod
# define _tagCmp compare
# define _LT LT
# define _EQ EQ
# define _GT GT
# define _Addr GHCbase.Addr
# define Text Show
# define IMP_Ubiq() IMPORT_DELOOPER(Ubiq); import qualified GHCbase
# define CHK_Ubiq() IMPORT_DELOOPER(Ubiq); import qualified GHCbase
# define minInt (minBound::Int)
# define maxInt (maxBound::Int)
#else
# define SYN_IE(a) a(..)
# define EXP_MODULE(a) a..
# define IMPORT_DELOOPER(mod) import mod
# define IMPORT_1_3(mod) {--}
# define IMP_Ubiq() IMPORT_DELOOPER(Ubiq)
# define CHK_Ubiq() IMPORT_DELOOPER(Ubiq)
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

#define FAST_BOOL Int#
#define _TRUE_ 1#
#define _FALSE_ 0#
#define _IS_TRUE_(x) ((x) `eqInt#` 1#)

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
# if __GLASGOW_HASKELL__ < 200
#  define FAST_STRING	_PackedString
#  define SLIT(x)	(_packCString (A# x#))
#  define _CMP_STRING_	cmpPString
#  define _NULL_	_nullPS
#  define _NIL_		_nilPS
#  define _CONS_	_consPS
#  define _HEAD_	_headPS
#  define _TAIL_	_tailPS
#  define _LENGTH_	_lengthPS
#  define _PK_		_packString
#  define _UNPK_	_unpackPS
#  define _SUBSTR_	_substrPS
#  define _APPEND_	`_appendPS`
#  define _CONCAT_	_concatPS
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
