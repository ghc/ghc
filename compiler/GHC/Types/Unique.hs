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
{-# LANGUAGE MagicHash #-}

module GHC.Types.Unique (
        -- * Main data types
        Unique, Uniquable(..),
        UniqueTag(..), uniqueTag, charToUniqueTag,
        uNIQUE_BITS,

        -- ** Constructors, destructors and operations on 'Unique's
        hasKey,

        showUnique,
        pprUniqueAlways,

        mkTag,
        mkUniqueGrimily,
        mkUniqueGrimilyWithTag,
        mkUniqueIntGrimily,
        getKey,
        mkUnique, unpkUnique,
        unpkUniqueGrimly,
        mkUniqueInt,
        eqUnique, ltUnique,
        incrUnique, stepUnique,

        newTagUnique, newTagUniqueGrimly,
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
import GHC.Utils.Panic.Plain (panic)
import GHC.Utils.Word64 (intToWord64, word64ToInt)

-- just for implementing a fast [0,61) -> Char function
import GHC.Exts (indexCharOffAddr#, Char(..), Int(..))

import GHC.Word         ( Word64 )
import Data.Char        ( chr, ord, isPrint )

import Language.Haskell.Syntax.Module.Name

{-
************************************************************************
*                                                                      *
\subsection[Unique-type]{@Unique@ type and operations}
*                                                                      *
************************************************************************

Note [Uniques and tags]
~~~~~~~~~~~~~~~~~~~~~~~~
A `Unique` in GHC is a 64 bit value composed of two pieces:
* A "tag", of width `UNIQUE_TAG_BITS`, in the high order bits
* A number, of width `uNIQUE_BITS`, which fills up the remainder of the Word64

The tag is typically stored as an ASCII character.  It is typically used to make it easier
to distinguish uniques constructed by different parts of the compiler.
To ensure that we assign distinct tags for each purpose, we represent tags in the compiler
via the UniqueTag ADT.

The bidirectional mapping from the UniqueTag ADT to the corresponding character is witnessed
by the uniqueTag and charToUniqueTag functions.
See Note [Uniques for wired-in prelude things and known tags]

`mkUnique` constructs a `Unique` from its pieces
  mkUnique :: UniqueTag -> Word64 -> Unique

Note [Performance implications of UniqueTag]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The UniqueTag ADT is meant to be ephemeral and eliminated by the simplifier,
so for long term storage (i.e. in monadic environments or data structures) we
want to store the raw 'Char's. Working with the raw tags is done via the *Grimly
class of functions

For instance, if we are generating a unique for a concrete tag, we should use
functions like `mkUniqueInt` which take a `UniqueTag` which will be eliminated by inlining.

However, if the unique is unknown and comes from a stored location, we should prefer using
the 'Char' directly. This avoids multiple conversions using `uniqueTag` at runtime.

For instance, instead of `env_ut :: UniqueTag` and

newUnique
  = do { env <- getEnv
       ; let tag = env_ut env
       ; liftIO $! uniqFromTag tag }

Prefer `env_ut :: Char` and
       ; liftIO $! uniqFromTagGrimly tag }

-}

-- | Unique identifier.
--
-- The type of unique identifiers that are used in many places in GHC
-- for fast ordering and equality tests. You should generate these with
-- the functions from the 'UniqSupply' module
--
-- These are sometimes also referred to as \"keys\" in comments in GHC.
newtype Unique = MkUnique Word64

data UniqueTag
  = AlphaTyVarTag
  | BcoTag
  | BlockIdTag
  | BoxedTupleDataTag
  | BoxedTupleTyConTag
  | BoxingTyConTag
  | BuiltinTag
  | CmmTag
  | CodeGenTag
  | CostCentreTag
  | CTupleDataTag
  | CTupleSelTag
  | CTupleTag
  | DataNSTag
  | DsTag
  | FldNSTag
  | HscTag
  | IfaceTag
  | JsTag
  | LocalTag
  | PluginTag
  | PreludeClassTag
  | PreludeDataConTag
  | PreludeMiscIdTag
  | PreludeTyConTag
  | PrimOpTag
  | PseudoTag
  | RegClassTag
  | RegPairTag
  | RegSingleTag
  | RegSubTag
  | RnIfaceTag
  | SimplTag
  | SkolemTag
  | SrtTag
  | StgPTag
  | StgTag
  | SumTag
  | TcNSTag
  | TcTag
  | TsanTag
  | TvNSTag
  | UnboxedTupleDataTag
  | UnboxedTupleTyConTag
  | UniqueRenamerTag
  | VarNSTag
  | VirtualRegTag
  | NullTag
  deriving (Eq, Show, Enum, Bounded)

uniqueTag :: UniqueTag -> Char
uniqueTag AlphaTyVarTag        = '1'
uniqueTag BcoTag               = 'I'
uniqueTag BlockIdTag           = 'L'
uniqueTag BoxedTupleDataTag    = '7'
uniqueTag BoxedTupleTyConTag   = '4'
uniqueTag BoxingTyConTag       = 'b'
uniqueTag BuiltinTag           = 'B'
uniqueTag CmmTag               = 'c'
uniqueTag CodeGenTag           = 'n'
uniqueTag CostCentreTag        = 'C'
uniqueTag CTupleDataTag        = 'm'
uniqueTag CTupleSelTag         = 'j'
uniqueTag CTupleTag            = 'k'
uniqueTag DataNSTag            = 'd'
uniqueTag DsTag                = 'D'
uniqueTag FldNSTag             = 'f'
uniqueTag HscTag               = 'r'
uniqueTag IfaceTag             = 'i'
uniqueTag JsTag                = 'J'
uniqueTag LocalTag             = 'X'
uniqueTag PluginTag            = 'p'
uniqueTag PreludeClassTag      = '2'
uniqueTag PreludeDataConTag    = '6'
uniqueTag PreludeMiscIdTag     = '0'
uniqueTag PreludeTyConTag      = '3'
uniqueTag PrimOpTag            = '9'
uniqueTag PseudoTag            = 'E'
uniqueTag RegClassTag          = 'A'
uniqueTag RegPairTag           = 'P'
uniqueTag RegSingleTag         = 'R'
uniqueTag RegSubTag            = 'S'
uniqueTag RnIfaceTag           = 'M'
uniqueTag SimplTag             = 's'
uniqueTag SkolemTag            = 'K'
uniqueTag SrtTag               = 'u'
uniqueTag StgPTag              = 'g'
uniqueTag StgTag               = 't'
uniqueTag SumTag               = 'z'
uniqueTag TcNSTag              = 'N'
uniqueTag TcTag                = 'a'
uniqueTag TsanTag              = 'T'
uniqueTag TvNSTag              = 'v'
uniqueTag UnboxedTupleDataTag  = '8'
uniqueTag UnboxedTupleTyConTag = '5'
uniqueTag UniqueRenamerTag     = 'Q'
uniqueTag VarNSTag             = 'V'
uniqueTag VirtualRegTag        = 'H'
uniqueTag NullTag              = chr 0
{-# INLINE uniqueTag #-}

charToUniqueTag :: Char -> UniqueTag
charToUniqueTag '0' = PreludeMiscIdTag
charToUniqueTag '1' = AlphaTyVarTag
charToUniqueTag '2' = PreludeClassTag
charToUniqueTag '3' = PreludeTyConTag
charToUniqueTag '4' = BoxedTupleTyConTag
charToUniqueTag '5' = UnboxedTupleTyConTag
charToUniqueTag '6' = PreludeDataConTag
charToUniqueTag '7' = BoxedTupleDataTag
charToUniqueTag '8' = UnboxedTupleDataTag
charToUniqueTag '9' = PrimOpTag
charToUniqueTag 'a' = TcTag
charToUniqueTag 'A' = RegClassTag
charToUniqueTag 'b' = BoxingTyConTag
charToUniqueTag 'B' = BuiltinTag
charToUniqueTag 'c' = CmmTag
charToUniqueTag 'C' = CostCentreTag
charToUniqueTag 'd' = DataNSTag
charToUniqueTag 'D' = DsTag
charToUniqueTag 'E' = PseudoTag
charToUniqueTag 'f' = FldNSTag
charToUniqueTag 'g' = StgPTag
charToUniqueTag 'H' = VirtualRegTag
charToUniqueTag 'i' = IfaceTag
charToUniqueTag 'I' = BcoTag
charToUniqueTag 'j' = CTupleSelTag
charToUniqueTag 'J' = JsTag
charToUniqueTag 'k' = CTupleTag
charToUniqueTag 'K' = SkolemTag
charToUniqueTag 'L' = BlockIdTag
charToUniqueTag 'm' = CTupleDataTag
charToUniqueTag 'M' = RnIfaceTag
charToUniqueTag 'n' = CodeGenTag
charToUniqueTag 'N' = TcNSTag
charToUniqueTag 'p' = PluginTag
charToUniqueTag 'P' = RegPairTag
charToUniqueTag 'Q' = UniqueRenamerTag
charToUniqueTag 'r' = HscTag
charToUniqueTag 'R' = RegSingleTag
charToUniqueTag 's' = SimplTag
charToUniqueTag 'S' = RegSubTag
charToUniqueTag 't' = StgTag
charToUniqueTag 'T' = TsanTag
charToUniqueTag 'u' = SrtTag
charToUniqueTag 'v' = TvNSTag
charToUniqueTag 'V' = VarNSTag
charToUniqueTag 'X' = LocalTag
charToUniqueTag 'z' = SumTag
charToUniqueTag c
  | ord c == 0 = NullTag
  | otherwise = panic $ "charToUniqueTag: unknown tag for char " ++ show c
{-# INLINE charToUniqueTag #-}


{-# INLINE uNIQUE_BITS #-}
uNIQUE_BITS :: Int
uNIQUE_BITS = 64 - UNIQUE_TAG_BITS

{-
Now come the functions which construct uniques from their pieces, and vice versa.
The stuff about unique *supplies* is handled further down this module.
-}

unpkUnique :: Unique -> (UniqueTag, Word64)        -- The reverse
unpkUniqueGrimly :: Unique -> (Char, Word64)        -- The reverse

mkUniqueGrimily :: Word64 -> Unique                -- A trap-door for UniqSupply
getKey          :: Unique -> Word64                -- for Var

incrUnique   :: Unique -> Unique
stepUnique   :: Unique -> Word64 -> Unique
newTagUnique :: Unique -> UniqueTag -> Unique
newTagUniqueGrimly :: Unique -> Char -> Unique

mkUniqueGrimily = MkUnique

{-# INLINE getKey #-}
getKey (MkUnique x) = x

incrUnique (MkUnique i) = MkUnique (i + 1)
stepUnique (MkUnique i) n = MkUnique (i + n)

mkLocalUnique :: Word64 -> Unique
mkLocalUnique i = mkUnique LocalTag i

minLocalUnique :: Unique
minLocalUnique = mkLocalUnique 0

maxLocalUnique :: Unique
maxLocalUnique = mkLocalUnique uniqueMask

-- newTagUnique changes the "domain" of a unique to a different char
newTagUnique u c = newTagUniqueGrimly u (uniqueTag c)

newTagUniqueGrimly u c = mkUniqueGrimilyWithTag c i where (_,i) = unpkUniqueGrimly u

-- | Bitmask that has zeros for the tag bits and ones for the rest.
uniqueMask :: Word64
uniqueMask = (1 `shiftL` uNIQUE_BITS) - 1

-- | Put the character in the highest bits of the Word64.
-- This may truncate the character to UNIQUE_TAG_BITS.
-- This function is used in @`mkSplitUniqSupply`@ so that it can
-- precompute and share the tag part of the uniques it generates.
mkTag :: Char -> Word64
mkTag c = intToWord64 (ord c) `shiftL` uNIQUE_BITS

-- pop the Char in the top 8 bits of the Unique(Supply)

-- No 64-bit bugs here, as long as we have at least 32 bits. --JSM

-- and as long as the Char fits in 8 bits, which we assume anyway!

mkUniqueGrimilyWithTag :: Char -> Word64 -> Unique       -- Builds a unique from pieces
mkUniqueGrimilyWithTag c i
  = MkUnique (tag .|. bits)
  where
    tag  = mkTag c
    bits = i .&. uniqueMask

{-# INLINE mkUniqueGrimilyWithTag #-}

mkUnique :: UniqueTag -> Word64 -> Unique       -- Builds a unique from pieces
mkUnique c i = mkUniqueGrimilyWithTag (uniqueTag c) i

{-# INLINE mkUnique #-}

mkUniqueInt :: UniqueTag -> Int -> Unique
mkUniqueInt c i = mkUnique c (intToWord64 i)

{-# INLINE mkUniqueInt #-}

mkUniqueIntGrimily :: Int -> Unique
mkUniqueIntGrimily = MkUnique . intToWord64

{-# INLINE mkUniqueIntGrimily #-}

unpkUniqueGrimly (MkUnique u)
  = let
        -- The potentially truncating use of fromIntegral here is safe
        -- because the argument is just the tag bits after shifting.
        tag = chr (word64ToInt (u `shiftR` uNIQUE_BITS))
        i   = u .&. uniqueMask
    in
    (tag, i)
{-# INLINE unpkUniqueGrimly #-}


unpkUnique u = case unpkUniqueGrimly u of
  (c, i) -> ( charToUniqueTag c, i)
{-# INLINE unpkUnique #-}

-- | The interface file symbol-table encoding assumes that known-key uniques fit
-- in 30-bits; verify this.
--
-- See Note [Symbol table representation of names] in "GHC.Iface.Binary" for details.
isValidKnownKeyUnique :: Unique -> Bool
isValidKnownKeyUnique u =
    case unpkUniqueGrimly u of
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
 getUnique fs = mkUniqueIntGrimily (uniqueOfFS fs)

instance Uniquable Int where
  getUnique i = mkUniqueIntGrimily i

instance Uniquable Word64 where
  getUnique i = mkUniqueGrimily i

instance Uniquable ModuleName where
  getUnique (ModuleName nm) = getUnique nm


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
--
-- Besides ABI/interface determinism, we also guarantee bit-for-bit identical
-- binaries (when -fobject-determinism is given), also known as object
-- determinism (#12935)
--
-- To achieve this, we must take care to non-determinism in the code
-- generation, and, in particular, guarantee that the existing uniques are
-- renamed deterministically and new ones are produced deterministically too.
-- The overview of object determinism is given by Note [Object determinism].
-- References to this note identify code where the unique determinism may
-- impact object determinism more specifically.

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
     is controlled. See GHC.Unit.Module.Env.ModuleEnv
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
  = tagStr ++ w64ToBase62 u
  where
    (tag, u) = unpkUniqueGrimly uniq
    -- Avoid emitting non-printable characters in pretty uniques.
    -- See #25989.
    tagStr
      | not (isPrint tag)  = show (ord tag) ++ "_"
      | otherwise          = [tag]

pprUniqueAlways :: IsLine doc => Unique -> doc
-- The "always" means regardless of -dsuppress-uniques
-- It replaces the old pprUnique to remind callers that
-- they should consider whether they want to consult
-- Opt_SuppressUniques
pprUniqueAlways u
  = text (showUnique u)
{-# SPECIALIZE pprUniqueAlways :: Unique -> SDoc #-}
{-# SPECIALIZE pprUniqueAlways :: Unique -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

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
The ``62-its'' are \tr{[0-9a-zA-Z]}.
Code stolen from Lennart.
-}

w64ToBase62 :: Word64 -> String
w64ToBase62 n_ = go n_ ""
  where
    -- The potentially truncating uses of fromIntegral here are safe
    -- because the argument is guaranteed to be less than 62 in both cases.
    go n cs | n < 62
            = let !c = chooseChar62 (word64ToInt n) in c : cs
            | otherwise
            = go q (c : cs) where (!q, r) = quotRem n 62
                                  !c = chooseChar62 (word64ToInt r)

    chooseChar62 :: Int -> Char
    {-# INLINE chooseChar62 #-}
    chooseChar62 (I# n) = C# (indexCharOffAddr# chars62 n)
    chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"#
