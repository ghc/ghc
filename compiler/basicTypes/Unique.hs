{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


@Uniques@ are used to distinguish entities in the compiler (@Ids@,
@Classes@, etc.) from each other.  Thus, @Uniques@ are the basic
comparison key in the compiler.

If there is any single operation that needs to be fast, it is @Unique@
comparison.  Unsurprisingly, there is quite a bit of huff-and-puff
directed to that end.

Some of the other hair in this code is to be able to use a
``splittable @UniqueSupply@'' if requested/possible (not standard
Haskell).
-}

{-# LANGUAGE CPP, BangPatterns, MagicHash #-}

module Unique (
        -- * Main data types
        Unique, Uniquable(..),

        -- ** Constructors, destructors and operations on 'Unique's
        hasKey,

        pprUnique,

        mkUniqueGrimily,                -- Used in UniqSupply only!
        getKey,                         -- Used in Var, UniqFM, Name only!
        mkUnique, unpkUnique,           -- Used in BinIface only

        deriveUnique,                   -- Ditto
        newTagUnique,                   -- Used in CgCase
        initTyVarUnique,
        nonDetCmpUnique,

        -- ** Making built-in uniques

        -- now all the built-in Uniques (and functions to make them)
        -- [the Oh-So-Wonderful Haskell module system wins again...]
        mkAlphaTyVarUnique,
        mkPrimOpIdUnique,
        mkTupleTyConUnique, mkTupleDataConUnique,
        mkSumTyConUnique, mkSumDataConUnique,
        mkCTupleTyConUnique,
        mkPreludeMiscIdUnique, mkPreludeDataConUnique,
        mkPreludeTyConUnique, mkPreludeClassUnique,
        mkPArrDataConUnique, mkCoVarUnique,
        mkCoreConAppUnique,

        mkVarOccUnique, mkDataOccUnique, mkTvOccUnique, mkTcOccUnique,
        mkRegSingleUnique, mkRegPairUnique, mkRegClassUnique, mkRegSubUnique,
        mkCostCentreUnique,

        tyConRepNameUnique,
        dataConWorkerUnique, dataConWrapperUnique, dataConRepNameUnique,

        mkBuiltinUnique,
        mkPseudoUniqueD,
        mkPseudoUniqueE,
        mkPseudoUniqueH
    ) where

#include "HsVersions.h"

import BasicTypes
import FastString
import Outputable
import Util

-- just for implementing a fast [0,61) -> Char function
import GHC.Exts (indexCharOffAddr#, Char(..), Int(..))

import Data.Char        ( chr, ord )
import Data.Bits

{-
************************************************************************
*                                                                      *
\subsection[Unique-type]{@Unique@ type and operations}
*                                                                      *
************************************************************************

The @Chars@ are ``tag letters'' that identify the @UniqueSupply@.
Fast comparison is everything on @Uniques@:
-}

-- | Unique identifier.
--
-- The type of unique identifiers that are used in many places in GHC
-- for fast ordering and equality tests. You should generate these with
-- the functions from the 'UniqSupply' module
newtype Unique = MkUnique Int

{-
Now come the functions which construct uniques from their pieces, and vice versa.
The stuff about unique *supplies* is handled further down this module.
-}

unpkUnique      :: Unique -> (Char, Int)        -- The reverse

mkUniqueGrimily :: Int -> Unique                -- A trap-door for UniqSupply
getKey          :: Unique -> Int                -- for Var

incrUnique   :: Unique -> Unique
stepUnique   :: Unique -> Int -> Unique
deriveUnique :: Unique -> Int -> Unique
newTagUnique :: Unique -> Char -> Unique

mkUniqueGrimily = MkUnique

{-# INLINE getKey #-}
getKey (MkUnique x) = x

incrUnique (MkUnique i) = MkUnique (i + 1)
stepUnique (MkUnique i) n = MkUnique (i + n)

-- deriveUnique uses an 'X' tag so that it won't clash with
-- any of the uniques produced any other way
-- SPJ says: this looks terribly smelly to me!
deriveUnique (MkUnique i) delta = mkUnique 'X' (i + delta)

-- newTagUnique changes the "domain" of a unique to a different char
newTagUnique u c = mkUnique c i where (_,i) = unpkUnique u

-- pop the Char in the top 8 bits of the Unique(Supply)

-- No 64-bit bugs here, as long as we have at least 32 bits. --JSM

-- and as long as the Char fits in 8 bits, which we assume anyway!

mkUnique :: Char -> Int -> Unique       -- Builds a unique from pieces
-- NOT EXPORTED, so that we can see all the Chars that
--               are used in this one module
mkUnique c i
  = MkUnique (tag .|. bits)
  where
    tag  = ord c `shiftL` 24
    bits = i .&. 16777215 {-``0x00ffffff''-}

unpkUnique (MkUnique u)
  = let
        -- as long as the Char may have its eighth bit set, we
        -- really do need the logical right-shift here!
        tag = chr (u `shiftR` 24)
        i   = u .&. 16777215 {-``0x00ffffff''-}
    in
    (tag, i)

{-
************************************************************************
*                                                                      *
\subsection[Uniquable-class]{The @Uniquable@ class}
*                                                                      *
************************************************************************
-}

-- | Class of things that we can obtain a 'Unique' from
class Uniquable a where
    getUnique :: a -> Unique

hasKey          :: Uniquable a => a -> Unique -> Bool
x `hasKey` k    = getUnique x == k

instance Uniquable FastString where
 getUnique fs = mkUniqueGrimily (uniqueOfFS fs)

instance Uniquable Int where
 getUnique i = mkUniqueGrimily i

{-
************************************************************************
*                                                                      *
\subsection[Unique-instances]{Instance declarations for @Unique@}
*                                                                      *
************************************************************************

And the whole point (besides uniqueness) is fast equality.  We don't
use `deriving' because we want {\em precise} control of ordering
(equality on @Uniques@ is v common).
-}

-- Note [Unique Determinism]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- The order of allocated @Uniques@ is not stable across rebuilds.
-- The main reason for that is that typechecking interface files pulls
-- @Uniques@ from @UniqSupply@ and the interface file for the module being
-- currently compiled can, but doesn't have to exist.
--
-- It gets more complicated if you take into account that the interface
-- files are loaded lazily and that building multiple files at once has to
-- work for any subset of interface files present. When you add parallelism
-- this makes @Uniques@ hopelessly random.
--
-- As such, to get deterministic builds, the order of the allocated
-- @Uniques@ should not affect the final result.
-- see also wiki/DeterministicBuilds
--
-- Note [Unique Determinism and code generation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The goal of the deterministic builds (wiki/DeterministicBuilds, #4012)
-- is to get ABI compatible binaries given the same inputs and environment.
-- The motivation behind that is that if the ABI doesn't change the
-- binaries can be safely reused.
-- Note that this is weaker than bit-for-bit identical binaries and getting
-- bit-for-bit identical binaries is not a goal for now.
-- This means that we don't care about nondeterminism that happens after
-- the interface files are created, in particular we don't care about
-- register allocation and code generation.
-- To track progress on bit-for-bit determinism see #12262.

eqUnique :: Unique -> Unique -> Bool
eqUnique (MkUnique u1) (MkUnique u2) = u1 == u2

-- Provided here to make it explicit at the call-site that it can
-- introduce non-determinism.
-- See Note [Unique Determinism]
-- See Note [No Ord for Unique]
nonDetCmpUnique :: Unique -> Unique -> Ordering
nonDetCmpUnique (MkUnique u1) (MkUnique u2)
  = if u1 == u2 then EQ else if u1 < u2 then LT else GT

{-
Note [No Ord for Unique]
~~~~~~~~~~~~~~~~~~~~~~~~~~
As explained in Note [Unique Determinism] the relative order of Uniques
is nondeterministic. To prevent from accidental use the Ord Unique
instance has been removed.
This makes it easier to maintain deterministic builds, but comes with some
drawbacks.
The biggest drawback is that Maps keyed by Uniques can't directly be used.
The alternatives are:

  1) Use UniqFM or UniqDFM, see Note [Deterministic UniqFM] to decide which
  2) Create a newtype wrapper based on Unique ordering where nondeterminism
     is controlled. See Module.ModuleEnv
  3) Change the algorithm to use nonDetCmpUnique and document why it's still
     deterministic
  4) Use TrieMap as done in CmmCommonBlockElim.groupByLabel
-}

instance Eq Unique where
    a == b = eqUnique a b
    a /= b = not (eqUnique a b)

instance Uniquable Unique where
    getUnique u = u

-- We do sometimes make strings with @Uniques@ in them:

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

{-
************************************************************************
*                                                                      *
\subsection[Utils-base62]{Base-62 numbers}
*                                                                      *
************************************************************************

A character-stingy way to read/write numbers (notably Uniques).
The ``62-its'' are \tr{[0-9a-zA-Z]}.  We don't handle negative Ints.
Code stolen from Lennart.
-}

iToBase62 :: Int -> String
iToBase62 n_
  = ASSERT(n_ >= 0) go n_ ""
  where
    go n cs | n < 62
            = let !c = chooseChar62 n in c : cs
            | otherwise
            = go q (c : cs) where (q, r) = quotRem n 62
                                  !c = chooseChar62 r

    chooseChar62 :: Int -> Char
    {-# INLINE chooseChar62 #-}
    chooseChar62 (I# n) = C# (indexCharOffAddr# chars62 n)
    chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"#

{-
************************************************************************
*                                                                      *
\subsection[Uniques-prelude]{@Uniques@ for wired-in Prelude things}
*                                                                      *
************************************************************************

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
        c       typechecker
        a       binders for case expressions in mkCoreConApp (desugarer)
-}

mkAlphaTyVarUnique     :: Int -> Unique
mkPreludeClassUnique   :: Int -> Unique
mkPreludeTyConUnique   :: Int -> Unique
mkTupleTyConUnique     :: Boxity -> Arity -> Unique
mkCTupleTyConUnique    :: Arity -> Unique
mkPreludeDataConUnique :: Arity -> Unique
mkTupleDataConUnique   :: Boxity -> Arity -> Unique
mkPrimOpIdUnique       :: Int -> Unique
mkPreludeMiscIdUnique  :: Int -> Unique
mkPArrDataConUnique    :: Int -> Unique
mkCoVarUnique          :: Int -> Unique
mkCoreConAppUnique     :: Int -> Unique

mkAlphaTyVarUnique   i = mkUnique '1' i
mkCoVarUnique        i = mkUnique 'g' i
mkPreludeClassUnique i = mkUnique '2' i
mkCoreConAppUnique   i = mkUnique 'a' i

--------------------------------------------------
-- Wired-in type constructor keys occupy *two* slots:
--    * u: the TyCon itself
--    * u+1: the TyConRepName of the TyCon
mkPreludeTyConUnique i                = mkUnique '3' (2*i)
mkTupleTyConUnique Boxed           a  = mkUnique '4' (2*a)
mkTupleTyConUnique Unboxed         a  = mkUnique '5' (2*a)
mkCTupleTyConUnique                a  = mkUnique 'k' (2*a)

tyConRepNameUnique :: Unique -> Unique
tyConRepNameUnique  u = incrUnique u

-- Data constructor keys occupy *two* slots.  The first is used for the
-- data constructor itself and its wrapper function (the function that
-- evaluates arguments as necessary and calls the worker). The second is
-- used for the worker function (the function that builds the constructor
-- representation).

--------------------------------------------------
-- Wired-in data constructor keys occupy *three* slots:
--    * u: the DataCon itself
--    * u+1: its worker Id
--    * u+2: its wrapper Id
--    * u+3: the TyConRepName of the promoted TyCon

mkPreludeDataConUnique i              = mkUnique '6' (4*i)    -- Must be alphabetic
mkTupleDataConUnique Boxed          a = mkUnique '7' (4*a)    -- ditto (*may* be used in C labels)
mkTupleDataConUnique Unboxed        a = mkUnique '8' (4*a)

--------------------------------------------------
-- Sum arities start from 2. A sum of arity N has N data constructors, so it
-- occupies N+1 slots: 1 TyCon + N DataCons.
--
-- So arity 2 sum takes uniques 0 (tycon), 1, 2  (2 data cons)
--    arity 3 sum takes uniques 3 (tycon), 4, 5, 6 (3 data cons)
-- etc.

mkSumTyConUnique :: Arity -> Unique
mkSumTyConUnique arity = mkUnique 'z' (sumUniqsOccupied arity)

mkSumDataConUnique :: ConTagZ -> Arity -> Unique
mkSumDataConUnique alt arity
  | alt >= arity
  = panic ("mkSumDataConUnique: " ++ show alt ++ " >= " ++ show arity)
  | otherwise
  = mkUnique 'z' (sumUniqsOccupied arity + alt + 1 {- skip the tycon -})

-- How many unique slots occupied by sum types (including constructors) up to
-- the given arity?
sumUniqsOccupied :: Arity -> Int
sumUniqsOccupied arity
  = ASSERT(arity >= 2)
    -- 3 + 4 + ... + arity
    ((arity * (arity + 1)) `div` 2) - 3
{-# INLINE sumUniqsOccupied #-}

--------------------------------------------------
dataConRepNameUnique, dataConWrapperUnique, dataConWorkerUnique :: Unique -> Unique
dataConWorkerUnique  u = incrUnique u
dataConWrapperUnique u = stepUnique u 2
dataConRepNameUnique u = stepUnique u 3

--------------------------------------------------
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
mkVarOccUnique  fs = mkUnique 'i' (uniqueOfFS fs)
mkDataOccUnique fs = mkUnique 'd' (uniqueOfFS fs)
mkTvOccUnique   fs = mkUnique 'v' (uniqueOfFS fs)
mkTcOccUnique   fs = mkUnique 'c' (uniqueOfFS fs)
