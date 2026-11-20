{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, MagicHash, Modifiers, PolyKinds, RankNTypes, RequiredTypeArguments, StandaloneKindSignatures, TypeFamilies #-}
module NativeNestedCase
  ( Pick, Pair(..), LeftFirst, IgnoreLeft, Uniform, SameFallback
  , Indexed(..), IndexedPick, TypeHead, UniformTypeHead, Wrapped, WrappedBool
  , PickFlipped, DiscardConstant, CaptureFlipped, WrittenFirst
  , AliasFirst, MixedAlias, ArrowResult
  , CaseFlag(..), ForallResult, ForallRepResult, forallResult
  , FamilyGate(..), FamilyGatePick, WrittenGatePick, HiddenGatePick, ApartGatePick
  , DependentFieldGate(..), DependentFieldPick
  , IndexedFieldBox(..), IndexedFieldPayload(..), IndexedFieldPick, IndexedFieldOrder
  , OpenCaseGate(..), OpenCaseIndex, OpenIndexPick, OpenIndexAtOrdering
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Exts (Int#, LiftedRep, RuntimeRep(IntRep), TYPE, U)

type Pick d = case d of
  Just (Just field) -> Proxy field
  Just Nothing -> Int
  Nothing -> Char

data Pair = Pair (Maybe Bool) (Maybe Bool)

type LeftFirst d = case d of
  Pair (Just _) Nothing -> Int
  Pair _ (Just _) -> Bool

type IgnoreLeft d = case d of
  Pair _ (Just _) -> Bool
  Pair (Just _) Nothing -> Int

type SameFallback d = case d of
  Just (Just _) -> Int
  Just _ -> Int
  Nothing -> Char

data Indexed k where
  BooleanTag :: Bool -> Indexed Bool
  OrderingTag :: Ordering -> Indexed Ordering

type IndexedPick =
  ((\tag -> case tag of
      Just (BooleanTag field) -> Proxy field
      Just (OrderingTag field) -> Proxy field
      Nothing -> Char) :: forall k. @U Maybe (Indexed k) %U -> Type)

type Uniform =
  ((\tag -> case tag of
      Just (BooleanTag _) -> Int
      Just (OrderingTag _) -> Int
      Nothing -> Int) :: forall k. @U Maybe (Indexed k) %U -> Type)

type TypeHead (m :: Maybe Type) = case m of
  Just Bool -> Int
  Just Ordering -> Char
  Just _ -> ()
  Nothing -> ()

type Flag = Bool
type UniformTypeHead (m :: Maybe Type) = case m of
  Just Flag -> Int
  Just Ordering -> Int
  Just _ -> Int
  Nothing -> Int

type Optional a = Maybe a
type Wrapped (m :: Maybe Type) = case m of
  Just (Optional a) -> Proxy a
  Just _ -> ()
  Nothing -> ()

type WrappedBool (m :: Maybe Type) = case m of
  Just (Optional Bool) -> Int
  Just _ -> ()
  Nothing -> ()

type Flip a b = Either b a
type Discard a b = Maybe b

type PickFlipped (m :: Maybe Type) = case m of
  Just (Maybe (Flip Bool left)) -> Proxy left
  Just _ -> ()
  Nothing -> ()

type DiscardConstant (m :: Maybe Type) = case m of
  Just (Discard Bool a) -> Proxy a
  Just _ -> ()
  Nothing -> ()

type CaptureFlipped (tag :: k) (m :: Maybe Type) = case m of
  Just (Flip a b) -> '(Proxy tag, Proxy a, Proxy b)
  Nothing -> '((), (), ())

type WrittenFirst (m :: Maybe Type) = case m of
  Just (Flip Bool Ordering) -> Int
  Just _ -> Int
  Nothing -> Int

type BothPresent = 'Pair ('Just 'True) ('Just 'False)

type AliasFirst d = case d of
  BothPresent -> Int
  Pair _ _ -> Int

type WrapLeft left right = 'Pair ('Just left) right

type MixedAlias d = case d of
  WrapLeft True (Just False) -> Int
  Pair _ _ -> Int

type family GateIndex where
  GateIndex = Bool

type GateIndexAlias = GateIndex

data FamilyGate k where
  PlainGate :: Bool -> FamilyGate k
  IndexedGate :: FamilyGate GateIndexAlias

type GateInput = Maybe (FamilyGate Bool)

type FamilyGatePick (d :: GateInput) = case d of
  Just (PlainGate _) -> Int
  Just _ -> Bool
  Nothing -> Char

type WrittenGatePick (d :: GateInput) = case d of
  Just IndexedGate -> Int
  Just _ -> Bool
  Nothing -> Char

type HiddenIndexedGate = 'Just ('IndexedGate :: FamilyGate Bool)

type HiddenGatePick (d :: GateInput) = case d of
  HiddenIndexedGate -> Int
  Just _ -> Int
  Nothing -> Char

newtype GateIndexOther = GateIndexOther Bool

type ApartGatePick (d :: Maybe (FamilyGate GateIndexOther)) = case d of
  Just (PlainGate True) -> Int
  Just _ -> Bool
  Nothing -> Char

type family OpenCaseIndex (b :: Bool) :: Type

data OpenCaseGate k where
  OpenPlain :: Bool -> OpenCaseGate k
  OpenIndexed :: Proxy b -> OpenCaseGate (OpenCaseIndex b)

type OpenIndexPick (k :: Type) (d :: Maybe (OpenCaseGate k)) = case d of
  Just (OpenPlain True) -> Int
  Just _ -> Int
  Nothing -> Char

type OpenIndexAtOrdering (d :: Maybe (OpenCaseGate Ordering)) = case d of
  Just (OpenPlain True) -> Int
  Just _ -> Bool
  Nothing -> Char

type family FieldIndex (seed :: Bool) (active :: Bool) :: Type where
  FieldIndex seed active = Bool

-- | Refine an index and transport two later arguments to their new kinds.
type DependentFieldGate :: forall k -> forall (x :: k) -> Proxy x -> Type
data DependentFieldGate k value marker where
  DefaultField :: Bool -> DependentFieldGate k value marker
  IndexedField :: forall (v :: FieldIndex 'False 'True) (mark :: Proxy v).
                  Proxy mark -> DependentFieldGate (FieldIndex 'False 'True) v mark

-- | Select a branch after checking transport for both dependent arguments.
type DependentFieldPick (d :: Maybe (DependentFieldGate Bool 'True 'Proxy)) = case d of
  Just (IndexedField _) -> Char
  Just _ -> Int
  Nothing -> ()

data IndexedFieldPayload (active :: Bool) where
  FieldOn :: Bool -> IndexedFieldPayload 'True
  FieldOff :: Ordering -> IndexedFieldPayload 'False

data IndexedFieldBox (k :: Type) (active :: Bool) where
  PlainFieldBox :: Bool -> IndexedFieldBox k active
  RefinedFieldBox :: Proxy seed -> IndexedFieldPayload active
                  -> IndexedFieldBox (FieldIndex seed active) active

type IndexedFieldPick (d :: Maybe (IndexedFieldBox Bool 'True)) = case d of
  Just (RefinedFieldBox _ (FieldOn flag)) -> Proxy flag
  Just _ -> Char
  Nothing -> ()

type IndexedFieldOrder (d :: Maybe (IndexedFieldBox Bool 'True)) = case d of
  Just (RefinedFieldBox _ (FieldOn True)) -> Int
  Just _ -> Int
  Nothing -> Char

type ArrowResult (tag :: k) (d :: Maybe (Maybe Bool)) =
  () -> (case d of
    Just (Just field) -> (Proxy tag, Proxy field)
    Just _ -> (Proxy tag, Proxy 'False)
    Nothing -> ())

data CaseFlag (d :: Maybe Bool) where
  CaseOn :: CaseFlag ('Just 'True)
  CaseOff :: CaseFlag ('Just 'False)
  CaseMissing :: CaseFlag 'Nothing

-- | Select the result from the flag and keep the quantified tag.
type ForallResult = forall k (tag :: k) (d :: Maybe Bool).
  Proxy tag -> CaseFlag d -> (case d of
    Just True -> (Proxy tag, Int)
    Just _ -> (Proxy tag, Int)
    Nothing -> (Proxy tag, Char))

-- | Use the result type selected by the index of the flag.
forallResult :: ForallResult
forallResult tag CaseOn = (tag, 1)
forallResult tag CaseOff = (tag, 0)
forallResult tag CaseMissing = (tag, '?')

type AtRep :: forall (r :: RuntimeRep) -> TYPE r
type family AtRep r where
  AtRep LiftedRep = Int
  AtRep 'IntRep = Int#

-- | The result representation depends on the quantified representation.
type ForallRepResult = forall k (tag :: k) (r :: RuntimeRep) (d :: Maybe Bool).
  Proxy tag -> CaseFlag d ->
    ((case d of
        Just True -> AtRep r
        Just _ -> AtRep r
        Nothing -> AtRep r) :: TYPE r)
