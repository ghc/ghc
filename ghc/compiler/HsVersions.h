#ifndef HSVERSIONS_H
#define HSVERSIONS_H

#if 0

IMPORTANT!  If you put extra tabs/spaces in these macro definitions,
you will screw up the layout where they are used in case expressions!

(This is cpp-dependent, of course)

#endif

#define MkInt I#
#define MkChar C#
#define MkArray _Array

#ifdef __GLASGOW_HASKELL__
#define TAG_ Int#
#define LT_ -1#
#define EQ_ 0#
#define GT_ 1#
#endif
#define GT__ _

#ifdef __HBC__
#define IMPORT_Trace import Trace
#define BSCC(l) (
#define ESCC 	)
#else
#define IMPORT_Trace {--}
#define BSCC(l) (_scc_ l (
#define ESCC 	))
#endif

-- these are overridable
#ifndef BIND
#define BIND case
#endif /* BIND */
#ifndef _TO_
#define _TO_ of {
#endif /* _TO_ */
#ifndef BEND
#define BEND }
#endif /* BEND */
#ifndef RETN
#define RETN {--}
#endif /* RETN */
#ifndef RETN_TYPE
#define RETN_TYPE {--}
#endif /* RETN_TYPE */

#define COMMA ,

#ifdef DEBUG
#define ASSERT(e) if (not (e)) then (assertPanic __FILE__ __LINE__) else
#else
#define ASSERT(e)
#endif

-- ToDo: ghci needs to load far too many bits of the backend because
--       this ATTACK_PRAGMA stuff encourages Utils.lhs to tell
--       everyone about everyone else.  I guess we need to add some
--       more conditional stuff in.
#ifdef USE_ATTACK_PRAGMAS
#define IF_ATTACK_PRAGMAS(x) x
#else
#define IF_ATTACK_PRAGMAS(x) {--}
#endif

#if GHCI
#define IF_GHCI(stuff) stuff
#else 
#define IF_GHCI(stuff) {-nothing-}
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 26
#define trace _trace
#endif

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
#define USE_FAST_STRINGS 1
#define FAST_STRING _PackedString
#define SLIT(x)	    (_packCString (A# x#))
#define _CMP_STRING_ cmpPString
#define _NULL_	    _nullPS
#define _NIL_	    _nilPS
#define _CONS_	    _consPS
#define _HEAD_	    _headPS
#define _TAIL_	    _tailPS
#define _LENGTH_    _lengthPS
#define _PK_	    _packString
#define _UNPK_	    _unpackPS
#define _SUBSTR_    _substrPS
#define _APPEND_    `_appendPS`
#define _CONCAT_    _concatPS
#else
#define FAST_STRING String
#define SLIT(x)	    (x)
#define _CMP_STRING_ cmpString
#define _NULL_	    null
#define _NIL_	    ""
#define _CONS_	    (:)
#define _HEAD_	    head
#define _TAIL_	    tail
#define _LENGTH_    length
#define _PK_	    (\x->x)
#define _UNPK_	    (\x->x)
#define _SUBSTR_    substr{-from Utils-}
#define _APPEND_    ++
#define _CONCAT_    concat
#endif

#if __HASKELL1__ < 3
{- To avoid confusion with Haskell 1.3, we use Swahili.

   data Maybe a = Nothing | Just a
   data Labda a = Hamna   | Ni   a

   Should we ever need to increase confusion with HBC, we will
   use Swedish:

   data Kanske a = Ingenting | Bara a
-}
# define Maybe Labda
# define Just Ni
# define Nothing Hamna
#else
# define MAYBE Labda
# define JUST Ni
# define NOTHING Hamna
#endif

#endif
