%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

@Uniques@ are used to distinguish entities in the compiler (@Ids@,
@Classes@, etc.) from each other.  Thus, @Uniques@ are the basic
comparison key in the compiler.

If there is any single operation that needs to be fast, it is @Unique@
comparison.  Unsurprisingly, there is quite a bit of huff-and-puff
directed to that end.

Some of the other hair in this code is to be able to use a
``splittable @UniqueSupply@'' if requested/possible (not standard
Haskell).

\begin{code}
{-# LANGUAGE CPP, BangPatterns, MagicHash #-}

module Unique (
        -- * Main data types
        Unique, Uniquable(..),

        -- ** Constructors, desctructors and operations on 'Unique's
        hasKey,

        pprUnique,

        mkUniqueGrimily,                -- Used in UniqSupply only!
        getKey, getKeyFastInt,          -- Used in Var, UniqFM, Name only!
        mkUnique, unpkUnique,           -- Used in BinIface only

        incrUnique,                     -- Used for renumbering
        deriveUnique,                   -- Ditto
        newTagUnique,                   -- Used in CgCase
        initTyVarUnique,

        -- ** Making built-in uniques

        -- now all the built-in Uniques (and functions to make them)
        -- [the Oh-So-Wonderful Haskell module system wins again...]
        mkAlphaTyVarUnique,
        mkPrimOpIdUnique,
        mkTupleTyConUnique, mkTupleDataConUnique,
        mkPreludeMiscIdUnique, mkPreludeDataConUnique,
        mkPreludeTyConUnique, mkPreludeClassUnique,
        mkPArrDataConUnique,

    mkVarOccUnique, mkDataOccUnique, mkTvOccUnique, mkTcOccUnique,
        mkRegSingleUnique, mkRegPairUnique, mkRegClassUnique, mkRegSubUnique,
        mkCostCentreUnique,

        mkBuiltinUnique,
        mkPseudoUniqueD,
        mkPseudoUniqueE,
        mkPseudoUniqueH
    ) where

#include "HsVersions.h"

import BasicTypes
import FastTypes
import FastString
import Outputable
-- import StaticFlags
import Util

#if defined(__GLASGOW_HASKELL__)
--just for implementing a fast [0,61) -> Char function
import GHC.Exts (indexCharOffAddr#, Char(..))
#else
import Data.Array
#endif
import Data.Char        ( chr, ord )
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Unique-type]{@Unique@ type and operations}
%*                                                                      *
%************************************************************************

The @Chars@ are ``tag letters'' that identify the @UniqueSupply@.
Fast comparison is everything on @Uniques@:

\begin{code}
--why not newtype Int?

-- | The type of unique identifiers that are used in many places in GHC
-- for fast ordering and equality tests. You should generate these with
-- the functions from the 'UniqSupply' module
data Unique = MkUnique FastInt
\end{code}

Now come the functions which construct uniques from their pieces, and vice versa.
The stuff about unique *supplies* is handled further down this module.

\begin{code}
unpkUnique      :: Unique -> (Char, Int)        -- The reverse

mkUniqueGrimily :: Int -> Unique                -- A trap-door for UniqSupply
getKey          :: Unique -> Int                -- for Var
getKeyFastInt   :: Unique -> FastInt            -- for Var

incrUnique      :: Unique -> Unique
deriveUnique    :: Unique -> Int -> Unique
newTagUnique    :: Unique -> Char -> Unique
\end{code}


\begin{code}
mkUniqueGrimily x = MkUnique (iUnbox x)

{-# INLINE getKey #-}
getKey (MkUnique x) = iBox x
{-# INLINE getKeyFastInt #-}
getKeyFastInt (MkUnique x) = x

incrUnique (MkUnique i) = MkUnique (i +# _ILIT(1))

-- deriveUnique uses an 'X' tag so that it won't clash with
-- any of the uniques produced any other way
deriveUnique (MkUnique i) delta = mkUnique 'X' (iBox i + delta)

-- newTagUnique changes the "domain" of a unique to a different char
newTagUnique u c = mkUnique c i where (_,i) = unpkUnique u

-- pop the Char in the top 8 bits of the Unique(Supply)

-- No 64-bit bugs here, as long as we have at least 32 bits. --JSM

-- and as long as the Char fits in 8 bits, which we assume anyway!

mkUnique :: Char -> Int -> Unique       -- Builds a unique from pieces
-- NOT EXPORTED, so that we can see all the Chars that
--               are used in this one module
mkUnique c i
  = MkUnique (tag `bitOrFastInt` bits)
  where
    !tag  = fastOrd (cUnbox c) `shiftLFastInt` _ILIT(24)
    !bits = iUnbox i `bitAndFastInt` _ILIT(16777215){-``0x00ffffff''-}

unpkUnique (MkUnique u)
  = let
        -- as long as the Char may have its eighth bit set, we
        -- really do need the logical right-shift here!
        tag = cBox (fastChr (u `shiftRLFastInt` _ILIT(24)))
        i   = iBox (u `bitAndFastInt` _ILIT(16777215){-``0x00ffffff''-})
    in
    (tag, i)
\end{code}



%************************************************************************
%*                                                                      *
\subsection[Uniquable-class]{The @Uniquable@ class}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Class of things that we can obtain a 'Unique' from
class Uniquable a where
    getUnique :: a -> Unique

hasKey          :: Uniquable a => a -> Unique -> Bool
x `hasKey` k    = getUnique x == k

instance Uniquable FastString where
 getUnique fs = mkUniqueGrimily (iBox (uniqueOfFS fs))

instance Uniquable Int where
 getUnique i = mkUniqueGrimily i
\end{code}


%************************************************************************
%*                                                                      *
\subsection[Unique-instances]{Instance declarations for @Unique@}
%*                                                                      *
%************************************************************************

And the whole point (besides uniqueness) is fast equality.  We don't
use `deriving' because we want {\em precise} control of ordering
(equality on @Uniques@ is v common).

\begin{code}
eqUnique, ltUnique, leUnique :: Unique -> Unique -> Bool
eqUnique (MkUnique u1) (MkUnique u2) = u1 ==# u2
ltUnique (MkUnique u1) (MkUnique u2) = u1 <#  u2
leUnique (MkUnique u1) (MkUnique u2) = u1 <=# u2

cmpUnique :: Unique -> Unique -> Ordering
cmpUnique (MkUnique u1) (MkUnique u2)
  = if u1 ==# u2 then EQ else if u1 <# u2 then LT else GT

instance Eq Unique where
    a == b = eqUnique a b
    a /= b = not (eqUnique a b)

instance Ord Unique where
    a  < b = ltUnique a b
    a <= b = leUnique a b
    a  > b = not (leUnique a b)
    a >= b = not (ltUnique a b)
    compare a b = cmpUnique a b

-----------------
instance Uniquable Unique where
    getUnique u = u
\end{code}

We do sometimes make strings with @Uniques@ in them:
\begin{code}
showUnique :: Unique -> String
showUnique uniq
  = case unpkUnique uniq of
      (tag, u) -> finish_show tag u (iToBase62 u)

finish_show :: Char -> Int -> String -> String
finish_show 't' u _pp_u | u < 26
  = -- Special case to make v common tyvars, t1, t2, ...
    -- come out as a, b, ... (shorter, easier to read)
    [chr (ord 'a' + u)]
finish_show tag _ pp_u = tag : pp_u

pprUnique :: Unique -> SDoc
pprUnique u = text (showUnique u)

instance Outputable Unique where
    ppr = pprUnique

instance Show Unique where
    show uniq = showUnique uniq
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Utils-base62]{Base-62 numbers}
%*                                                                      *
%************************************************************************

A character-stingy way to read/write numbers (notably Uniques).
The ``62-its'' are \tr{[0-9a-zA-Z]}.  We don't handle negative Ints.
Code stolen from Lennart.

\begin{code}
iToBase62 :: Int -> String
iToBase62 n_
  = ASSERT(n_ >= 0) go (iUnbox n_) ""
  where
    go n cs | n <# _ILIT(62)
             = case chooseChar62 n of { c -> c `seq` (c : cs) }
             | otherwise
             =  case (quotRem (iBox n) 62) of { (q_, r_) ->
                case iUnbox q_ of { q -> case iUnbox r_ of { r ->
                case (chooseChar62 r) of { c -> c `seq`
                (go q (c : cs)) }}}}

    chooseChar62 :: FastInt -> Char
    {-# INLINE chooseChar62 #-}
#if defined(__GLASGOW_HASKELL__)
    --then FastInt == Int#
    chooseChar62 n = C# (indexCharOffAddr# chars62 n)
    !chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"#
#else
    --Haskell98 arrays are portable
    chooseChar62 n = (!) chars62 n
    chars62 = listArray (0,61) "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
#endif
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Uniques-prelude]{@Uniques@ for wired-in Prelude things}
%*                                                                      *
%************************************************************************

Allocation of unique supply characters:
        v,t,u : for renumbering value-, type- and usage- vars.
        B:   builtin
        C-E: pseudo uniques     (used in native-code generator)
        X:   uniques derived by deriveUnique
        _:   unifiable tyvars   (above)
        0-9: prelude things below
             (no numbers left any more..)
        ::   (prelude) parallel array data constructors

        other a-z: lower case chars for unique supplies.  Used so far:

        d       desugarer
        f       AbsC flattener
        g       SimplStg
        n       Native codegen
        r       Hsc name cache
        s       simplifier

\begin{code}
mkAlphaTyVarUnique     :: Int -> Unique
mkPreludeClassUnique   :: Int -> Unique
mkPreludeTyConUnique   :: Int -> Unique
mkTupleTyConUnique     :: TupleSort -> Int -> Unique
mkPreludeDataConUnique :: Int -> Unique
mkTupleDataConUnique   :: TupleSort -> Int -> Unique
mkPrimOpIdUnique       :: Int -> Unique
mkPreludeMiscIdUnique  :: Int -> Unique
mkPArrDataConUnique    :: Int -> Unique

mkAlphaTyVarUnique i            = mkUnique '1' i

mkPreludeClassUnique i          = mkUnique '2' i

-- Prelude type constructors occupy *three* slots.
-- The first is for the tycon itself; the latter two
-- are for the generic to/from Ids.  See TysWiredIn.mk_tc_gen_info.

mkPreludeTyConUnique i          = mkUnique '3' (3*i)
mkTupleTyConUnique BoxedTuple   a       = mkUnique '4' (3*a)
mkTupleTyConUnique UnboxedTuple a       = mkUnique '5' (3*a)
mkTupleTyConUnique ConstraintTuple a    = mkUnique 'k' (3*a)

-- Data constructor keys occupy *two* slots.  The first is used for the
-- data constructor itself and its wrapper function (the function that
-- evaluates arguments as necessary and calls the worker). The second is
-- used for the worker function (the function that builds the constructor
-- representation).

mkPreludeDataConUnique i        = mkUnique '6' (2*i)    -- Must be alphabetic
mkTupleDataConUnique BoxedTuple   a = mkUnique '7' (2*a)        -- ditto (*may* be used in C labels)
mkTupleDataConUnique UnboxedTuple    a = mkUnique '8' (2*a)
mkTupleDataConUnique ConstraintTuple a = mkUnique 'h' (2*a)

mkPrimOpIdUnique op         = mkUnique '9' op
mkPreludeMiscIdUnique  i    = mkUnique '0' i

-- No numbers left anymore, so I pick something different for the character tag
mkPArrDataConUnique a           = mkUnique ':' (2*a)

-- The "tyvar uniques" print specially nicely: a, b, c, etc.
-- See pprUnique for details

initTyVarUnique :: Unique
initTyVarUnique = mkUnique 't' 0

mkPseudoUniqueD, mkPseudoUniqueE, mkPseudoUniqueH,
   mkBuiltinUnique :: Int -> Unique

mkBuiltinUnique i = mkUnique 'B' i
mkPseudoUniqueD i = mkUnique 'D' i -- used in NCG for getUnique on RealRegs
mkPseudoUniqueE i = mkUnique 'E' i -- used in NCG spiller to create spill VirtualRegs
mkPseudoUniqueH i = mkUnique 'H' i -- used in NCG spiller to create spill VirtualRegs

mkRegSingleUnique, mkRegPairUnique, mkRegSubUnique, mkRegClassUnique :: Int -> Unique
mkRegSingleUnique = mkUnique 'R'
mkRegSubUnique    = mkUnique 'S'
mkRegPairUnique   = mkUnique 'P'
mkRegClassUnique  = mkUnique 'L'

mkCostCentreUnique :: Int -> Unique
mkCostCentreUnique = mkUnique 'C'

mkVarOccUnique, mkDataOccUnique, mkTvOccUnique, mkTcOccUnique :: FastString -> Unique
-- See Note [The Unique of an OccName] in OccName
mkVarOccUnique  fs = mkUnique 'i' (iBox (uniqueOfFS fs))
mkDataOccUnique fs = mkUnique 'd' (iBox (uniqueOfFS fs))
mkTvOccUnique   fs = mkUnique 'v' (iBox (uniqueOfFS fs))
mkTcOccUnique   fs = mkUnique 'c' (iBox (uniqueOfFS fs))
\end{code}

