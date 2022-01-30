

-- | This is where we define a mapping from Uniques to their associated
-- known-key Names for things associated with tuples and sums. We use this
-- mapping while deserializing known-key Names in interface file symbol tables,
-- which are encoded as their Unique. See Note [Symbol table representation of
-- names] for details.
--

module GHC.Builtin.Uniques
    ( -- * Looking up known-key names
      knownUniqueName

      -- * Getting the 'Unique's of 'Name's
      -- ** Anonymous sums
    , mkSumTyConUnique
    , mkSumDataConUnique
      -- ** Tuples
      -- *** Vanilla
    , mkTupleTyConUnique
    , mkTupleDataConUnique
      -- *** Constraint
    , mkCTupleTyConUnique
    , mkCTupleDataConUnique
    , mkCTupleSelIdUnique

      -- ** Making built-in uniques
    , mkAlphaTyVarUnique
    , mkPrimOpIdUnique, mkPrimOpWrapperUnique
    , mkPreludeMiscIdUnique, mkPreludeDataConUnique
    , mkPreludeTyConUnique, mkPreludeClassUnique

    , mkVarOccUnique, mkDataOccUnique, mkTvOccUnique, mkTcOccUnique
    , mkRegSingleUnique, mkRegPairUnique, mkRegClassUnique, mkRegSubUnique
    , mkCostCentreUnique

    , mkBuiltinUnique
    , mkPseudoUniqueE

      -- ** Deriving uniquesc
      -- *** From TyCon name uniques
    , tyConRepNameUnique
      -- *** From DataCon name uniques
    , dataConWorkerUnique, dataConTyRepNameUnique

    , initExitJoinUnique

    ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Builtin.Types
import {-# SOURCE #-} GHC.Core.TyCon
import {-# SOURCE #-} GHC.Core.DataCon
import {-# SOURCE #-} GHC.Types.Id
import {-# SOURCE #-} GHC.Types.Name
import GHC.Types.Basic
import GHC.Types.Unique
import GHC.Data.FastString

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain

import Data.Maybe

-- | Get the 'Name' associated with a known-key 'Unique'.
knownUniqueName :: Unique -> Maybe Name
knownUniqueName u =
    case tag of
      'z' -> Just $ getUnboxedSumName n
      '4' -> Just $ getTupleTyConName Boxed n
      '5' -> Just $ getTupleTyConName Unboxed n
      '7' -> Just $ getTupleDataConName Boxed n
      '8' -> Just $ getTupleDataConName Unboxed n
      'j' -> Just $ getCTupleSelIdName n
      'k' -> Just $ getCTupleTyConName n
      'm' -> Just $ getCTupleDataConName n
      _   -> Nothing
  where
    (tag, n) = unpkUnique u

{-
Note [Unique layout for unboxed sums]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sum arities start from 2. The encoding is a bit funny: we break up the
integral part into bitfields for the arity, an alternative index (which is
taken to be 0xfc in the case of the TyCon), and, in the case of a datacon, a
tag (used to identify the sum's TypeRep binding).

This layout is chosen to remain compatible with the usual unique allocation
for wired-in data constructors described in GHC.Types.Unique

TyCon for sum of arity k:
  00000000 kkkkkkkk 11111100

TypeRep of TyCon for sum of arity k:
  00000000 kkkkkkkk 11111101

DataCon for sum of arity k and alternative n (zero-based):
  00000000 kkkkkkkk nnnnnn00

TypeRep for sum DataCon of arity k and alternative n (zero-based):
  00000000 kkkkkkkk nnnnnn10
-}

mkSumTyConUnique :: Arity -> Unique
mkSumTyConUnique arity =
    assert (arity < 0x3f) $ -- 0x3f since we only have 6 bits to encode the
                            -- alternative
    mkUnique 'z' (arity `shiftL` 8 .|. 0xfc)

mkSumDataConUnique :: ConTagZ -> Arity -> Unique
mkSumDataConUnique alt arity
  | alt >= arity
  = panic ("mkSumDataConUnique: " ++ show alt ++ " >= " ++ show arity)
  | otherwise
  = mkUnique 'z' (arity `shiftL` 8 + alt `shiftL` 2) {- skip the tycon -}

getUnboxedSumName :: Int -> Name
getUnboxedSumName n
  | n .&. 0xfc == 0xfc
  = case tag of
      0x0 -> tyConName $ sumTyCon arity
      0x1 -> getRep $ sumTyCon arity
      _   -> pprPanic "getUnboxedSumName: invalid tag" (ppr tag)
  | tag == 0x0
  = dataConName $ sumDataCon (alt + 1) arity
  | tag == 0x1
  = getName $ dataConWrapId $ sumDataCon (alt + 1) arity
  | tag == 0x2
  = getRep $ promoteDataCon $ sumDataCon (alt + 1) arity
  | otherwise
  = pprPanic "getUnboxedSumName" (ppr n)
  where
    arity = n `shiftR` 8
    alt = (n .&. 0xfc) `shiftR` 2
    tag = 0x3 .&. n
    getRep tycon =
        fromMaybe (pprPanic "getUnboxedSumName(getRep)" (ppr tycon))
        $ tyConRepName_maybe tycon

-- Note [Uniques for tuple type and data constructors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Wired-in type constructor keys occupy *two* slots:
--    * u: the TyCon itself
--    * u+1: the TyConRepName of the TyCon
--
-- Wired-in tuple data constructor keys occupy *three* slots:
--    * u: the DataCon itself
--    * u+1: its worker Id
--    * u+2: the TyConRepName of the promoted TyCon

{-
Note [Unique layout for constraint tuple selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Constraint tuples, like boxed and unboxed tuples, have their type and data
constructor Uniques wired in (see
Note [Uniques for tuple type and data constructors]). Constraint tuples are
somewhat more involved, however. For a boxed or unboxed n-tuple, we need:

* A Unique for the type constructor, and
* A Unique for the data constructor

With a constraint n-tuple, however, we need:

* A Unique for the type constructor,
* A Unique for the data constructor, and
* A Unique for each of the n superclass selectors

To pick a concrete example (n = 2), the binary constraint tuple has a type
constructor and data constructor (%,%) along with superclass selectors
$p1(%,%) and $p2(%,%).

Just as we wire in the Uniques for constraint tuple type constructors and data
constructors, we wish to wire in the Uniques for the superclass selectors as
well. Not only does this make everything consistent, it also avoids a
compile-time performance penalty whenever GHC.Classes is loaded from an
interface file. This is because GHC.Classes defines constraint tuples as class
definitions, and if these classes weren't wired in, then loading GHC.Classes
would also load every single constraint tuple type constructor, data
constructor, and superclass selector. See #18635.

We encode the Uniques for constraint tuple superclass selectors as follows. The
integral part of the Unique is broken up into bitfields for the arity and the
position of the superclass. Given a selector for a constraint tuple with
arity n (zero-based) and position k (where 1 <= k <= n), its Unique will look
like:

  00000000 nnnnnnnn kkkkkkkk

We can use bit-twiddling tricks to access the arity and position with
cTupleSelIdArityBits and cTupleSelIdPosBitmask, respectively.

This pattern bears a certain resemblance to the way that the Uniques for
unboxed sums are encoded. This is because for a unboxed sum of arity n, there
are n corresponding data constructors, each with an alternative position k.
Similarly, for a constraint tuple of arity n, there are n corresponding
superclass selectors. Reading Note [Unique layout for unboxed sums] will
instill an appreciation for how the encoding for constraint tuple superclass
selector Uniques takes inspiration from the encoding for unboxed sum Uniques.
-}

mkCTupleTyConUnique :: Arity -> Unique
mkCTupleTyConUnique a = mkUnique 'k' (2*a)

mkCTupleDataConUnique :: Arity -> Unique
mkCTupleDataConUnique a = mkUnique 'm' (3*a)

mkCTupleSelIdUnique :: ConTagZ -> Arity -> Unique
mkCTupleSelIdUnique sc_pos arity
  | sc_pos >= arity
  = panic ("mkCTupleSelIdUnique: " ++ show sc_pos ++ " >= " ++ show arity)
  | otherwise
  = mkUnique 'j' (arity `shiftL` cTupleSelIdArityBits + sc_pos)

getCTupleTyConName :: Int -> Name
getCTupleTyConName n =
    case n `divMod` 2 of
      (arity, 0) -> cTupleTyConName arity
      (arity, 1) -> mkPrelTyConRepName $ cTupleTyConName arity
      _          -> panic "getCTupleTyConName: impossible"

getCTupleDataConName :: Int -> Name
getCTupleDataConName n =
    case n `divMod` 3 of
      (arity,  0) -> cTupleDataConName arity
      (arity,  1) -> getName $ dataConWrapId $ cTupleDataCon arity
      (arity,  2) -> mkPrelTyConRepName $ cTupleDataConName arity
      _           -> panic "getCTupleDataConName: impossible"

getCTupleSelIdName :: Int -> Name
getCTupleSelIdName n = cTupleSelIdName (sc_pos + 1) arity
  where
    arity  = n `shiftR` cTupleSelIdArityBits
    sc_pos = n .&. cTupleSelIdPosBitmask

-- Given the arity of a constraint tuple, this is the number of bits by which
-- one must shift it to the left in order to encode the arity in the Unique
-- of a superclass selector for that constraint tuple. Alternatively, given the
-- Unique for a constraint tuple superclass selector, this is the number of
-- bits by which one must shift it to the right to retrieve the arity of the
-- constraint tuple. See Note [Unique layout for constraint tuple selectors].
cTupleSelIdArityBits :: Int
cTupleSelIdArityBits = 8

-- Given the Unique for a constraint tuple superclass selector, one can
-- retrieve the position of the selector by ANDing this mask, which will
-- clear all but the eight least significant bits.
-- See Note [Unique layout for constraint tuple selectors].
cTupleSelIdPosBitmask :: Int
cTupleSelIdPosBitmask = 0xff

--------------------------------------------------
-- Normal tuples

mkTupleDataConUnique :: Boxity -> Arity -> Unique
mkTupleDataConUnique Boxed          a = mkUnique '7' (3*a)    -- may be used in C labels
mkTupleDataConUnique Unboxed        a = mkUnique '8' (3*a)

mkTupleTyConUnique :: Boxity -> Arity -> Unique
mkTupleTyConUnique Boxed           a  = mkUnique '4' (2*a)
mkTupleTyConUnique Unboxed         a  = mkUnique '5' (2*a)

getTupleTyConName :: Boxity -> Int -> Name
getTupleTyConName boxity n =
    case n `divMod` 2 of
      (arity, 0) -> tyConName $ tupleTyCon boxity arity
      (arity, 1) -> fromMaybe (panic "getTupleTyConName")
                    $ tyConRepName_maybe $ tupleTyCon boxity arity
      _          -> panic "getTupleTyConName: impossible"

getTupleDataConName :: Boxity -> Int -> Name
getTupleDataConName boxity n =
    case n `divMod` 3 of
      (arity, 0) -> dataConName $ tupleDataCon boxity arity
      (arity, 1) -> idName $ dataConWorkId $ tupleDataCon boxity arity
      (arity, 2) -> fromMaybe (panic "getTupleDataCon")
                    $ tyConRepName_maybe $ promotedTupleDataCon boxity arity
      _          -> panic "getTupleDataConName: impossible"

{-
Note [Uniques for wired-in prelude things and known masks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Allocation of unique supply characters:
        v,u: for renumbering value-, and usage- vars.
        B:   builtin
        C-E: pseudo uniques     (used in native-code generator)
        I:   GHCi evaluation
        X:   uniques from mkLocalUnique
        _:   unifiable tyvars   (above)
        0-9: prelude things below
             (no numbers left any more..)
        ::   (prelude) parallel array data constructors

        other a-z: lower case chars for unique supplies.  Used so far:

        a       TypeChecking?
        c       StgToCmm/Renamer
        d       desugarer
        f       AbsC flattener
        i       TypeChecking interface files
        j       constraint tuple superclass selectors
        k       constraint tuple tycons
        m       constraint tuple datacons
        n       Native/LLVM codegen
        r       Hsc name cache
        s       simplifier
        u       Cmm pipeline
        y       GHCi bytecode generator
        z       anonymous sums
-}

mkAlphaTyVarUnique     :: Int -> Unique
mkPreludeClassUnique   :: Int -> Unique
mkPrimOpIdUnique       :: Int -> Unique
-- See Note [Primop wrappers] in GHC.Builtin.PrimOps.
mkPrimOpWrapperUnique  :: Int -> Unique
mkPreludeMiscIdUnique  :: Int -> Unique

mkAlphaTyVarUnique   i = mkUnique '1' i
mkPreludeClassUnique i = mkUnique '2' i

--------------------------------------------------
mkPrimOpIdUnique op         = mkUnique '9' (2*op)
mkPrimOpWrapperUnique op    = mkUnique '9' (2*op+1)
mkPreludeMiscIdUnique  i    = mkUnique '0' i

mkPseudoUniqueE, mkBuiltinUnique :: Int -> Unique

mkBuiltinUnique i = mkUnique 'B' i
mkPseudoUniqueE i = mkUnique 'E' i -- used in NCG spiller to create spill VirtualRegs

mkRegSingleUnique, mkRegPairUnique, mkRegSubUnique, mkRegClassUnique :: Int -> Unique
mkRegSingleUnique = mkUnique 'R'
mkRegSubUnique    = mkUnique 'S'
mkRegPairUnique   = mkUnique 'P'
mkRegClassUnique  = mkUnique 'L'

mkCostCentreUnique :: Int -> Unique
mkCostCentreUnique = mkUnique 'C'

mkVarOccUnique, mkDataOccUnique, mkTvOccUnique, mkTcOccUnique :: FastString -> Unique
-- See Note [The Unique of an OccName] in GHC.Types.Name.Occurrence
mkVarOccUnique  fs = mkUnique 'i' (uniqueOfFS fs)
mkDataOccUnique fs = mkUnique 'd' (uniqueOfFS fs)
mkTvOccUnique   fs = mkUnique 'v' (uniqueOfFS fs)
mkTcOccUnique   fs = mkUnique 'c' (uniqueOfFS fs)

initExitJoinUnique :: Unique
initExitJoinUnique = mkUnique 's' 0


--------------------------------------------------
-- Wired-in type constructor keys occupy *two* slots:
--    * u: the TyCon itself
--    * u+1: the TyConRepName of the TyCon

mkPreludeTyConUnique   :: Int -> Unique
mkPreludeTyConUnique i                = mkUnique '3' (2*i)

tyConRepNameUnique :: Unique -> Unique
tyConRepNameUnique  u = incrUnique u

--------------------------------------------------
-- Wired-in data constructor keys occupy *three* slots:
--    * u: the DataCon itself
--    * u+1: its worker Id
--    * u+2: the TyConRepName of the promoted TyCon
-- Prelude data constructors are too simple to need wrappers.

mkPreludeDataConUnique :: Int -> Unique
mkPreludeDataConUnique i              = mkUnique '6' (3*i)    -- Must be alphabetic

--------------------------------------------------
dataConTyRepNameUnique, dataConWorkerUnique :: Unique -> Unique
dataConWorkerUnique  u = incrUnique u
dataConTyRepNameUnique u = stepUnique u 2
