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

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns, MagicHash #-}

module GHC.Types.Unique (
        -- * Main data types
        Unique, Uniquable(..),
        uNIQUE_BITS,

        -- ** Constructors, destructors and operations on 'Unique's
        hasKey,

        pprUniqueAlways,

        mkUniqueGrimily,
        getKey,
        mkUnique, unpkUnique,
        eqUnique, ltUnique,
        incrUnique, stepUnique,

        newTagUnique,
        nonDetCmpUnique,
        isValidKnownKeyUnique,

        -- ** Local uniques
        -- | These are exposed exclusively for use by 'GHC.Types.Var.Env.uniqAway', which
        -- has rather peculiar needs. See Note [Local uniques].
        mkLocalUnique, minLocalUnique, maxLocalUnique,
    ) where

#include "Unique.h"

import GHC.Prelude

import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Utils.Panic.Plain

-- just for implementing a fast [0,61) -> Char function
import GHC.Exts (indexCharOffAddr#, Char(..), Int(..))

import Data.Char        ( chr, ord )

{-
************************************************************************
*                                                                      *
\subsection[Unique-type]{@Unique@ type and operations}
*                                                                      *
************************************************************************

Note [Uniques and masks]
~~~~~~~~~~~~~~~~~~~~~~~~
A `Unique` in GHC is a Word-sized value composed of two pieces:
* A "mask", of width `UNIQUE_TAG_BITS`, in the high order bits
* A number, of width `uNIQUE_BITS`, which fills up the remainder of the Word

The mask is typically an ASCII character.  It is typically used to make it easier
to distinguish uniques constructed by different parts of the compiler.
There is a (potentially incomplete) list of unique masks used given in
GHC.Builtin.Uniques. See Note [Uniques-prelude - Uniques for wired-in Prelude things]

`mkUnique` constructs a `Unique` from its pieces
  mkUnique :: Char -> Int -> Unique

-}

-- | Unique identifier.
--
-- The type of unique identifiers that are used in many places in GHC
-- for fast ordering and equality tests. You should generate these with
-- the functions from the 'UniqSupply' module
--
-- These are sometimes also referred to as \"keys\" in comments in GHC.
newtype Unique = MkUnique Int

{-# INLINE uNIQUE_BITS #-}
uNIQUE_BITS :: Int
uNIQUE_BITS = finiteBitSize (0 :: Int) - UNIQUE_TAG_BITS

{-
Now come the functions which construct uniques from their pieces, and vice versa.
The stuff about unique *supplies* is handled further down this module.
-}

unpkUnique      :: Unique -> (Char, Int)        -- The reverse

mkUniqueGrimily :: Int -> Unique                -- A trap-door for UniqSupply
getKey          :: Unique -> Int                -- for Var

incrUnique   :: Unique -> Unique
stepUnique   :: Unique -> Int -> Unique
newTagUnique :: Unique -> Char -> Unique

mkUniqueGrimily = MkUnique

{-# INLINE getKey #-}
getKey (MkUnique x) = x

incrUnique (MkUnique i) = MkUnique (i + 1)
stepUnique (MkUnique i) n = MkUnique (i + n)

mkLocalUnique :: Int -> Unique
mkLocalUnique i = mkUnique 'X' i

minLocalUnique :: Unique
minLocalUnique = mkLocalUnique 0

maxLocalUnique :: Unique
maxLocalUnique = mkLocalUnique uniqueMask

-- newTagUnique changes the "domain" of a unique to a different char
newTagUnique u c = mkUnique c i where (_,i) = unpkUnique u

-- | How many bits are devoted to the unique index (as opposed to the class
-- character).
uniqueMask :: Int
uniqueMask = (1 `shiftL` uNIQUE_BITS) - 1

-- pop the Char in the top 8 bits of the Unique(Supply)

-- No 64-bit bugs here, as long as we have at least 32 bits. --JSM

-- and as long as the Char fits in 8 bits, which we assume anyway!

mkUnique :: Char -> Int -> Unique       -- Builds a unique from pieces
-- EXPORTED and used only in GHC.Builtin.Uniques
mkUnique c i
  = MkUnique (tag .|. bits)
  where
    tag  = ord c `shiftL` uNIQUE_BITS
    bits = i .&. uniqueMask

unpkUnique (MkUnique u)
  = let
        -- as long as the Char may have its eighth bit set, we
        -- really do need the logical right-shift here!
        tag = chr (u `shiftR` uNIQUE_BITS)
        i   = u .&. uniqueMask
    in
    (tag, i)

-- | The interface file symbol-table encoding assumes that known-key uniques fit
-- in 30-bits; verify this.
--
-- See Note [Symbol table representation of names] in "GHC.Iface.Binary" for details.
isValidKnownKeyUnique :: Unique -> Bool
isValidKnownKeyUnique u =
    case unpkUnique u of
      (c, x) -> ord c < 0xff && x <= (1 `shiftL` 22)

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
-- see also wiki/deterministic-builds
--
-- Note [Unique Determinism and code generation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The goal of the deterministic builds (wiki/deterministic-builds, #4012)
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

ltUnique :: Unique -> Unique -> Bool
ltUnique (MkUnique u1) (MkUnique u2) = u1 < u2

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
  4) Use TrieMap as done in GHC.Cmm.CommonBlockElim.groupByLabel
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
      (tag, u) -> tag : iToBase62 u

pprUniqueAlways :: Unique -> SDoc
-- The "always" means regardless of -dsuppress-uniques
-- It replaces the old pprUnique to remind callers that
-- they should consider whether they want to consult
-- Opt_SuppressUniques
pprUniqueAlways u
  = text (showUnique u)

instance Outputable Unique where
    ppr = pprUniqueAlways

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
  = assert (n_ >= 0) $ go n_ ""
  where
    go n cs | n < 62
            = let !c = chooseChar62 n in c : cs
            | otherwise
            = go q (c : cs) where (!q, r) = quotRem n 62
                                  !c = chooseChar62 r

    chooseChar62 :: Int -> Char
    {-# INLINE chooseChar62 #-}
    chooseChar62 (I# n) = C# (indexCharOffAddr# chars62 n)
    chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"#
