{-# LANGUAGE UndecidableSuperClasses, FunctionalDependencies, RoleAnnotations, ExplicitNamespaces, TypeFamilies, RankNTypes, TypeApplications, LambdaCase, DerivingStrategies, ScopedTypeVariables, TypeOperators, DataKinds, PolyKinds, GADTs, TypeFamilyDependencies, ConstraintKinds, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, AllowAmbiguousTypes, StrictData #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -fforce-recomp -Wincomplete-patterns #-}

{- | Module, containing restrictions imposed by instruction or value scope.

Michelson have multiple restrictions on values, examples:
* @operation@ type cannot appear in parameter.
* @big_map@ type cannot appear in @PUSH@-able constants.
* @contract@ type cannot appear in type we @UNPACK@ to.

Thus we declare multiple "scopes" - constraints applied in corresponding
situations, for instance
* 'ParameterScope';
* 'StorageScope';
* 'ConstantScope'.

Also we separate multiple "classes" of scope-related constraints.

* 'ParameterScope' and similar ones are used within Michelson engine,
they are understandable by GHC but produce not very clarifying errors.

* 'ProperParameterBetterErrors' and similar ones are middle-layer constraints,
they produce human-readable errors but GHC cannot make conclusions from them.
They are supposed to be used only by eDSLs to define their own high-level
constraints.

* Lorentz and other eDSLs may declare their own constraints, in most cases
you should use them. For example see 'Lorentz.Constraints' module.

-}

module T18478
  ( -- * Scopes
    ConstantScope
  , StorageScope
  , PackedValScope
  , ParameterScope
  , PrintedValScope
  , UnpackedValScope

  , ProperParameterBetterErrors
  , ProperStorageBetterErrors
  , ProperConstantBetterErrors
  , ProperPackedValBetterErrors
  , ProperUnpackedValBetterErrors
  , ProperPrintedValBetterErrors

  , properParameterEvi
  , properStorageEvi
  , properConstantEvi
  , properPackedValEvi
  , properUnpackedValEvi
  , properPrintedValEvi
  , (:-)(..)

  , BadTypeForScope (..)
  , CheckScope (..)

    -- * Implementation internals
  , HasNoBigMap
  , HasNoNestedBigMaps
  , HasNoOp
  , HasNoContract
  , ContainsBigMap
  , ContainsNestedBigMaps

  , ForbidOp
  , ForbidContract
  , ForbidBigMap
  , ForbidNestedBigMaps
  , FailOnBigMapFound
  , FailOnNestedBigMapsFound
  , FailOnOperationFound

  , OpPresence (..)
  , ContractPresence (..)
  , BigMapPresence (..)
  , NestedBigMapsPresence (..)
  , checkOpPresence
  , checkContractTypePresence
  , checkBigMapPresence
  , checkNestedBigMapsPresence
  , opAbsense
  , contractTypeAbsense
  , bigMapAbsense
  , nestedBigMapsAbsense
  , forbiddenOp
  , forbiddenContractType
  , forbiddenBigMap
  , forbiddenNestedBigMaps

    -- * Re-exports
  , withDict
  , SingI (..)
  ) where

import Data.Type.Bool (type (||))
import Data.Type.Coercion
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Data.Typeable
import GHC.Generics
import GHC.Exts hiding (withDict)
import Data.Kind

data T =
    TKey
  | TUnit
  | TSignature
  | TChainId
  | TOption T
  | TList T
  | TSet T
  | TOperation
  | TContract T
  | TPair T T
  | TOr T T
  | TLambda T T
  | TMap T T
  | TBigMap T T
  | TInt
  | TNat
  | TString
  | TBytes
  | TMutez
  | TBool
  | TKeyHash
  | TTimestamp
  | TAddress
  deriving stock (Eq, Show)

type family Sing :: k -> Type

class SingI a where
  sing :: Sing a

class SingKind (k :: Type) where
  -- | Get a base type from the promoted kind. For example,
  -- @Demote Bool@ will be the type @Bool@. Rarely, the type and kind do not
  -- match. For example, @Demote Nat@ is @Natural@.
  type Demote k = (r :: Type) | r -> k

  -- | Convert a singleton to its unrefined version.
  fromSing :: Sing (a :: k) -> Demote k

  -- | Convert an unrefined type to an existentially-quantified singleton type.
  toSing   :: Demote k -> SomeSing k

data SomeSing (k :: Type) :: Type where
  SomeSing :: Sing (a :: k) -> SomeSing k


-- | Instance of data family 'Sing' for 'T'.
-- Custom instance is implemented in order to inject 'Typeable'
-- constraint for some of constructors.
data SingT :: T -> Type where
  STKey :: SingT  'TKey
  STUnit :: SingT  'TUnit
  STSignature :: SingT  'TSignature
  STChainId :: SingT  'TChainId
  STOption :: (SingI a, Typeable a) => Sing a -> SingT ( 'TOption a)
  STList :: (SingI a, Typeable a) => Sing a -> SingT ( 'TList a )
  STSet :: (SingI a, Typeable a) => Sing a -> SingT ( 'TSet a )
  STOperation  :: SingT 'TOperation
  STContract   :: (SingI a, Typeable a)
                => Sing a -> SingT ( 'TContract a )
  STPair       :: (SingI a, SingI b, Typeable a, Typeable b)
                => Sing a -> Sing b -> SingT ('TPair a b)
  STOr         :: (SingI a, SingI b, Typeable a, Typeable b)
                => Sing a -> Sing b -> SingT ('TOr a b)
  STLambda     :: (SingI a, SingI b, Typeable a, Typeable b)
                => Sing a -> Sing b -> SingT ('TLambda a b)
  STMap        :: (SingI a, SingI b, Typeable a, Typeable b)
                => Sing a -> Sing b -> SingT ('TMap a b)
  STBigMap    :: (SingI a, SingI b, Typeable a, Typeable b)
                => Sing a -> Sing b -> SingT ('TBigMap a b)
  STInt :: SingT  'TInt
  STNat :: SingT  'TNat
  STString :: SingT  'TString
  STBytes :: SingT  'TBytes
  STMutez :: SingT  'TMutez
  STBool :: SingT  'TBool
  STKeyHash :: SingT  'TKeyHash
  STTimestamp :: SingT  'TTimestamp
  STAddress :: SingT  'TAddress

type instance Sing = SingT

---------------------------------------------
-- Singleton-related helpers for T
--------------------------------------------

-- | Version of 'SomeSing' with 'Typeable' constraint,
-- specialized for use with 'T' kind.
data SomeSingT where
  SomeSingT :: forall (a :: T). (Typeable a, SingI a)
            => Sing a -> SomeSingT

-- | Version of 'withSomeSing' with 'Typeable' constraint
-- provided to processing function.
--
-- Required for not to erase these useful constraints when doing
-- conversion from value of type 'T' to its singleton representation.
withSomeSingT
  :: T
  -> (forall (a :: T). (Typeable a, SingI a) => Sing a -> r)
  -> r
withSomeSingT t f = (\(SomeSingT s) -> f s) (toSingT t)

-- | Version of 'fromSing' specialized for use with
-- @data instance Sing :: T -> Type@ which requires 'Typeable'
-- constraint for some of its constructors
fromSingT :: Sing (a :: T) -> T
fromSingT = \case
  STKey -> TKey
  STUnit -> TUnit
  STSignature -> TSignature
  STChainId -> TChainId
  STOption t -> TOption (fromSingT t)
  STList t -> TList (fromSingT t)
  STSet t -> TSet (fromSingT t)
  STOperation -> TOperation
  STContract t -> TContract (fromSingT t)
  STPair a b -> TPair (fromSingT a) (fromSingT b)
  STOr a b -> TOr (fromSingT a) (fromSingT b)
  STLambda a b -> TLambda (fromSingT a) (fromSingT b)
  STMap a b -> TMap (fromSingT a) (fromSingT b)
  STBigMap a b -> TBigMap (fromSingT a) (fromSingT b)
  STInt -> TInt
  STNat -> TNat
  STString -> TString
  STBytes -> TBytes
  STMutez -> TMutez
  STBool -> TBool
  STKeyHash -> TKeyHash
  STTimestamp -> TTimestamp
  STAddress -> TAddress

-- | Version of 'toSing' which creates 'SomeSingT'.
toSingT :: T -> SomeSingT
toSingT = \case
  TKey -> SomeSingT STKey
  TUnit -> SomeSingT STUnit
  TSignature -> SomeSingT STSignature
  TChainId -> SomeSingT STChainId
  TOption t -> withSomeSingT t $ \tSing -> SomeSingT $ STOption tSing
  TList t -> withSomeSingT t $ \tSing -> SomeSingT $ STList tSing
  TSet ct -> withSomeSingT ct $ \ctSing -> SomeSingT $ STSet ctSing
  TOperation -> SomeSingT STOperation
  TContract t -> withSomeSingT t $ \tSing -> SomeSingT $ STContract tSing
  TPair l r ->
    withSomeSingT l $ \lSing ->
    withSomeSingT r $ \rSing ->
      SomeSingT $ STPair lSing rSing
  TOr l r ->
    withSomeSingT l $ \lSing ->
    withSomeSingT r $ \rSing ->
      SomeSingT $ STOr lSing rSing
  TLambda l r ->
    withSomeSingT l $ \lSing ->
    withSomeSingT r $ \rSing ->
      SomeSingT $ STLambda lSing rSing
  TMap l r ->
    withSomeSingT l $ \lSing ->
    withSomeSingT r $ \rSing ->
      SomeSingT $ STMap lSing rSing
  TBigMap l r ->
    withSomeSingT l $ \lSing ->
    withSomeSingT r $ \rSing ->
      SomeSingT $ STBigMap lSing rSing
  TInt -> SomeSingT STInt
  TNat -> SomeSingT STNat
  TString -> SomeSingT STString
  TBytes -> SomeSingT STBytes
  TMutez -> SomeSingT STMutez
  TBool -> SomeSingT STBool
  TKeyHash -> SomeSingT STKeyHash
  TTimestamp -> SomeSingT STTimestamp
  TAddress -> SomeSingT STAddress

instance SingKind T where
  type Demote T = T
  fromSing  = fromSingT
  toSing t = case toSingT t of SomeSingT s -> SomeSing s

instance SingI  'TKey where
  sing = STKey
instance SingI  'TUnit where
  sing = STUnit
instance SingI  'TSignature where
  sing = STSignature
instance SingI  'TChainId where
  sing = STChainId
instance (SingI a, Typeable a) => SingI ( 'TOption (a :: T)) where
  sing = STOption sing
instance (SingI a, Typeable a) => SingI ( 'TList (a :: T)) where
  sing = STList sing
instance (SingI a, Typeable a) => SingI ( 'TSet (a :: T)) where
  sing = STSet sing
instance SingI 'TOperation where
  sing = STOperation
instance (SingI a, Typeable a) =>
          SingI ( 'TContract (a :: T)) where
  sing = STContract sing
instance (SingI a, Typeable a, Typeable b, SingI b) =>
          SingI ( 'TPair a b) where
  sing = STPair sing sing
instance (SingI a, Typeable a, Typeable b, SingI b) =>
          SingI ( 'TOr a b) where
  sing = STOr sing sing
instance (SingI a, Typeable a, Typeable b, SingI b) =>
          SingI ( 'TLambda a b) where
  sing = STLambda sing sing
instance (SingI a, Typeable a, Typeable b, SingI b) =>
          SingI ( 'TMap a b) where
  sing = STMap sing sing
instance (SingI a, Typeable a, Typeable b, SingI b) =>
          SingI ( 'TBigMap a b) where
  sing = STBigMap sing sing
instance SingI  'TInt where
  sing = STInt
instance SingI  'TNat where
  sing = STNat
instance SingI  'TString where
  sing = STString
instance SingI  'TBytes where
  sing = STBytes
instance SingI  'TMutez where
  sing = STMutez
instance SingI  'TBool where
  sing = STBool
instance SingI  'TKeyHash where
  sing = STKeyHash
instance SingI  'TTimestamp where
  sing = STTimestamp
instance SingI  'TAddress where
  sing = STAddress

data Dict :: Constraint -> Type where
  Dict :: a => Dict a
  deriving Typeable

withDict :: HasDict c e => e -> (c => r) -> r
withDict d r = case evidence d of
  Dict -> r

class HasDict c e | e -> c where
  evidence :: e -> Dict c

instance HasDict a (Dict a) where
  evidence = Prelude.id

instance a => HasDict b (a :- b) where
  evidence (Sub x) = x

instance HasDict (Coercible a b) (Coercion a b) where
  evidence Coercion = Dict

instance HasDict (a ~ b) (a :~: b) where
  evidence Refl = Dict

infixl 1 \\ -- required comment

(\\) :: HasDict c e => (c => r) -> e -> r
r \\ d = withDict d r
infixr 9 :-
newtype a :- b = Sub (a => Dict b)
  deriving Typeable
type role (:-) nominal nominal
(***) :: (a :- b) -> (c :- d) -> (a, c) :- (b, d)
f *** g = Sub $ Dict \\ f \\ g

type f $ a = f a
infixr 2 $

maybeToRight a Nothing = Left a
maybeToRight _ (Just a) = Right a

----------------------------------------------------------------------------
-- Constraints
----------------------------------------------------------------------------
-- | Whether this type contains 'TOperation' type.
--
-- In some scopes (constants, parameters, storage) appearing for operation type
-- is prohibited.
-- Operations in input/output of lambdas are allowed without limits though.
type family ContainsOp (t :: T) :: Bool where
  ContainsOp 'TKey = 'False
  ContainsOp 'TUnit = 'False
  ContainsOp 'TSignature = 'False
  ContainsOp 'TChainId = 'False
  ContainsOp ('TOption t) = ContainsOp t
  ContainsOp ('TList t) = ContainsOp t
  ContainsOp ('TSet t) = ContainsOp t
  ContainsOp 'TOperation = 'True
  ContainsOp ('TContract t) = ContainsOp t
  ContainsOp ('TPair a b) = ContainsOp a || ContainsOp b
  ContainsOp ('TOr a b) = ContainsOp a || ContainsOp b
  ContainsOp ('TLambda _ _) = 'False
  ContainsOp ('TMap k v) = ContainsOp k || ContainsOp v
  ContainsOp ('TBigMap k v) = ContainsOp k || ContainsOp v
  ContainsOp _ = 'False

-- | Whether this type contains 'TContract' type.
--
-- In some scopes (constants, storage) appearing for contract type
-- is prohibited.
-- Contracts in input/output of lambdas are allowed without limits though.
type family ContainsContract (t :: T) :: Bool where
  ContainsContract 'TKey = 'False
  ContainsContract 'TUnit = 'False
  ContainsContract 'TSignature = 'False
  ContainsContract 'TChainId = 'False
  ContainsContract ('TOption t) = ContainsContract t
  ContainsContract ('TList t) = ContainsContract t
  ContainsContract ('TSet _) = 'False
  ContainsContract 'TOperation = 'False
  ContainsContract ('TContract _) = 'True
  ContainsContract ('TPair a b) = ContainsContract a || ContainsContract b
  ContainsContract ('TOr a b) = ContainsContract a || ContainsContract b
  ContainsContract ('TLambda _ _) = 'False
  ContainsContract ('TMap _ v) = ContainsContract v
  ContainsContract ('TBigMap _ v) = ContainsContract v
  ContainsContract _ = 'False

-- | Whether this type contains 'TBigMap' type.
type family ContainsBigMap (t :: T) :: Bool where
  ContainsBigMap 'TKey = 'False
  ContainsBigMap 'TUnit = 'False
  ContainsBigMap 'TSignature = 'False
  ContainsBigMap 'TChainId = 'False
  ContainsBigMap ('TOption t) = ContainsBigMap t
  ContainsBigMap ('TList t) = ContainsBigMap t
  ContainsBigMap ('TSet _) = 'False
  ContainsBigMap 'TOperation = 'False
  ContainsBigMap ('TContract t) = ContainsBigMap t
  ContainsBigMap ('TPair a b) = ContainsBigMap a || ContainsBigMap b
  ContainsBigMap ('TOr a b) = ContainsBigMap a || ContainsBigMap b
  ContainsBigMap ('TLambda _ _) = 'False
  ContainsBigMap ('TMap _ v) = ContainsBigMap v
  ContainsBigMap ('TBigMap _ _) = 'True
  ContainsBigMap _ = 'False

-- | Whether this type contains a type with nested 'TBigMap's .
--
-- Nested big_maps (i.e. big_map which contains another big_map inside of it's value type). Are
-- prohibited in all contexts. Some context such as PUSH, APPLY, PACK/UNPACK instructions are more
-- strict because they doesn't work with big_map at all.
type family ContainsNestedBigMaps (t :: T) :: Bool where
  ContainsNestedBigMaps 'TKey = 'False
  ContainsNestedBigMaps 'TUnit = 'False
  ContainsNestedBigMaps 'TSignature = 'False
  ContainsNestedBigMaps 'TChainId = 'False
  ContainsNestedBigMaps ('TOption t) = ContainsNestedBigMaps t
  ContainsNestedBigMaps ('TList t) = ContainsNestedBigMaps t
  ContainsNestedBigMaps ('TSet _) = 'False
  ContainsNestedBigMaps 'TOperation = 'False
  ContainsNestedBigMaps ('TContract t) = ContainsNestedBigMaps t
  ContainsNestedBigMaps ('TPair a b) = ContainsNestedBigMaps a || ContainsNestedBigMaps b
  ContainsNestedBigMaps ('TOr a b) = ContainsNestedBigMaps a || ContainsNestedBigMaps b
  ContainsNestedBigMaps ('TLambda _ _) = 'False
  ContainsNestedBigMaps ('TMap _ v) = ContainsNestedBigMaps v
  ContainsNestedBigMaps ('TBigMap _ v) = ContainsBigMap v
  ContainsNestedBigMaps _ = 'False

-- | Constraint which ensures that operation type does not appear in a given type.
--
-- Not just a type alias in order to be able to partially apply it
-- (e.g. in 'Each').
class (ContainsOp t ~ 'False) => HasNoOp t
instance (ContainsOp t ~ 'False) => HasNoOp t

-- | Constraint which ensures that contract type does not appear in a given type.
class (ContainsContract t ~ 'False) => HasNoContract t
instance (ContainsContract t ~ 'False) => HasNoContract t

-- | Constraint which ensures that bigmap does not appear in a given type.
class (ContainsBigMap t ~ 'False) => HasNoBigMap t
instance (ContainsBigMap t ~ 'False) => HasNoBigMap t

-- | Constraint which ensures that there are no nested bigmaps.
class (ContainsNestedBigMaps t ~ 'False) => HasNoNestedBigMaps t
instance (ContainsNestedBigMaps t ~ 'False) => HasNoNestedBigMaps t

-- | Report a human-readable error about 'TOperation' at a wrong place.
type family FailOnOperationFound (enabled :: Bool) :: Constraint where
  FailOnOperationFound 'True =
    TypeError ('Text "Operations are not allowed in this scope")
  FailOnOperationFound 'False = ()

-- | Report a human-readable error about 'TContract' at a wrong place.
type family FailOnContractFound (enabled :: Bool) :: Constraint where
  FailOnContractFound 'True =
    TypeError ('Text "Type `contract` is not allowed in this scope")
  FailOnContractFound 'False = ()

-- | Report a human-readable error about 'TBigMap' at a wrong place.
type family FailOnBigMapFound (enabled :: Bool) :: Constraint where
  FailOnBigMapFound 'True =
    TypeError ('Text "BigMaps are not allowed in this scope")
  FailOnBigMapFound 'False = ()

-- | Report a human-readable error that 'TBigMap' contains another 'TBigMap'
type family FailOnNestedBigMapsFound (enabled :: Bool) :: Constraint where
  FailOnNestedBigMapsFound 'True =
    TypeError ('Text "Nested BigMaps are not allowed")
  FailOnNestedBigMapsFound 'False = ()

-- | This is like 'HasNoOp', but raises a more human-readable error
-- when the @t@ type is concrete.
--
-- Use this constraint in our eDSL.
type ForbidOp t = FailOnOperationFound (ContainsOp t)

type ForbidContract t = FailOnContractFound (ContainsContract t)

type ForbidBigMap t = FailOnBigMapFound (ContainsBigMap t)

type ForbidNestedBigMaps t = FailOnNestedBigMapsFound (ContainsNestedBigMaps t)

-- | Evidence of that 'HasNoOp' is deducable from 'ForbidOp'.
forbiddenOpEvi :: forall t. (SingI t, ForbidOp t) :- HasNoOp t
forbiddenOpEvi = Sub $
  case checkOpPresence (sing @t) of
    OpAbsent -> Dict

-- | Reify 'HasNoOp' constraint from 'ForbidOp'.
--
-- Left for backward compatibility.
forbiddenOp
  :: forall t a.
     (SingI t, ForbidOp t)
  => (HasNoOp t => a)
  -> a
forbiddenOp = withDict $ forbiddenOpEvi @t

forbiddenBigMapEvi :: forall t. (SingI t, ForbidBigMap t) :- HasNoBigMap t
forbiddenBigMapEvi = Sub $
  case checkBigMapPresence (sing @t) of
    BigMapAbsent -> Dict

forbiddenNestedBigMapsEvi :: forall t. (SingI t, ForbidNestedBigMaps t) :- HasNoNestedBigMaps t
forbiddenNestedBigMapsEvi = Sub $
  case checkNestedBigMapsPresence (sing @t) of
    NestedBigMapsAbsent -> Dict

forbiddenBigMap
  :: forall t a.
     (SingI t, ForbidBigMap t)
  => (HasNoBigMap t => a)
  -> a
forbiddenBigMap = withDict $ forbiddenBigMapEvi @t

forbiddenNestedBigMaps
  :: forall t a.
     (SingI t, ForbidNestedBigMaps t)
  => (HasNoNestedBigMaps t => a)
  -> a
forbiddenNestedBigMaps = withDict $ forbiddenNestedBigMapsEvi @t

-- | Reify 'HasNoContract' constraint from 'ForbidContract'.
forbiddenContractTypeEvi
  :: forall t. (SingI t, ForbidContract t) :- HasNoContract t
forbiddenContractTypeEvi = Sub $
  case checkContractTypePresence (sing @t) of
    ContractAbsent -> Dict

-- | Reify 'HasNoContract' constraint from 'ForbidContract'.
forbiddenContractType
  :: forall t a.
     (SingI t, ForbidContract t)
  => (HasNoContract t => a)
  -> a
forbiddenContractType = withDict $ forbiddenContractTypeEvi @t

-- | Whether the type contains 'TOperation', with proof.
data OpPresence (t :: T)
  = ContainsOp t ~ 'True => OpPresent
  | ContainsOp t ~ 'False => OpAbsent

data ContractPresence (t :: T)
  = ContainsContract t ~ 'True => ContractPresent
  | ContainsContract t ~ 'False => ContractAbsent

data BigMapPresence (t :: T)
  = ContainsBigMap t ~ 'True => BigMapPresent
  | ContainsBigMap t ~ 'False => BigMapAbsent

data NestedBigMapsPresence (t :: T)
  = ContainsNestedBigMaps t ~ 'True => NestedBigMapsPresent
  | ContainsNestedBigMaps t ~ 'False => NestedBigMapsAbsent

-- @rvem: IMO, generalization of OpPresence and BigMapPresence to
-- TPresence is not worth it, due to the fact that
-- it will require more boilerplate in checkTPresence implementation
-- than it is already done in checkOpPresence and checkBigMapPresence

-- | Check at runtime whether the given type contains 'TOperation'.
checkOpPresence :: Sing (ty :: T) -> OpPresence ty
checkOpPresence = \case
  -- This is a sad amount of boilerplate, but at least
  -- there is no chance to make a mistake in it.
  -- We can't do in a simpler way while requiring only @Sing ty@ / @SingI ty@,
  -- and a more complex constraint would be too unpleasant and confusing to
  -- propagate everywhere.
  STKey -> OpAbsent
  STSignature -> OpAbsent
  STChainId -> OpAbsent
  STUnit -> OpAbsent
  STOption t -> case checkOpPresence t of
    OpPresent -> OpPresent
    OpAbsent -> OpAbsent
  STList t -> case checkOpPresence t of
    OpPresent -> OpPresent
    OpAbsent -> OpAbsent
  STSet a -> case checkOpPresence a of
    OpPresent -> OpPresent
    OpAbsent -> OpAbsent
  STOperation -> OpPresent
  STContract t -> case checkOpPresence t of
    OpPresent -> OpPresent
    OpAbsent -> OpAbsent
  STPair a b -> case (checkOpPresence a, checkOpPresence b) of
    (OpPresent, _) -> OpPresent
    (_, OpPresent) -> OpPresent
    (OpAbsent, OpAbsent) -> OpAbsent
  STOr a b -> case (checkOpPresence a, checkOpPresence b) of
    (OpPresent, _) -> OpPresent
    (_, OpPresent) -> OpPresent
    (OpAbsent, OpAbsent) -> OpAbsent
  STLambda _ _ -> OpAbsent
  STMap k v -> case (checkOpPresence k, checkOpPresence v) of
    (OpAbsent, OpAbsent) -> OpAbsent
    (OpPresent, _) -> OpPresent
    (_, OpPresent) -> OpPresent
  STBigMap k v -> case (checkOpPresence k, checkOpPresence v) of
    (OpAbsent, OpAbsent) -> OpAbsent
    (OpPresent, _) -> OpPresent
    (_, OpPresent) -> OpPresent
  STInt -> OpAbsent
  STNat -> OpAbsent
  STString -> OpAbsent
  STBytes -> OpAbsent
  STMutez -> OpAbsent
  STBool -> OpAbsent
  STKeyHash -> OpAbsent
  STTimestamp -> OpAbsent
  STAddress -> OpAbsent

-- | Check at runtime whether the given type contains 'TContract'.
checkContractTypePresence :: Sing (ty :: T) -> ContractPresence ty
checkContractTypePresence = \case
  STKey -> ContractAbsent
  STSignature -> ContractAbsent
  STChainId -> ContractAbsent
  STUnit -> ContractAbsent
  STOption t -> case checkContractTypePresence t of
    ContractPresent -> ContractPresent
    ContractAbsent -> ContractAbsent
  STList t -> case checkContractTypePresence t of
    ContractPresent -> ContractPresent
    ContractAbsent -> ContractAbsent
  STSet _ -> ContractAbsent
  STOperation -> ContractAbsent
  STContract _ -> ContractPresent
  STPair a b -> case (checkContractTypePresence a, checkContractTypePresence b) of
    (ContractPresent, _) -> ContractPresent
    (_, ContractPresent) -> ContractPresent
    (ContractAbsent, ContractAbsent) -> ContractAbsent
  STOr a b -> case (checkContractTypePresence a, checkContractTypePresence b) of
    (ContractPresent, _) -> ContractPresent
    (_, ContractPresent) -> ContractPresent
    (ContractAbsent, ContractAbsent) -> ContractAbsent
  STLambda _ _ -> ContractAbsent
  STMap _ v -> case checkContractTypePresence v of
    ContractPresent -> ContractPresent
    ContractAbsent -> ContractAbsent
  STBigMap _ v -> case checkContractTypePresence v of
    ContractPresent -> ContractPresent
    ContractAbsent -> ContractAbsent
  STInt -> ContractAbsent
  STNat -> ContractAbsent
  STString -> ContractAbsent
  STBytes -> ContractAbsent
  STMutez -> ContractAbsent
  STBool -> ContractAbsent
  STKeyHash -> ContractAbsent
  STTimestamp -> ContractAbsent
  STAddress -> ContractAbsent

-- | Check at runtime whether the given type contains 'TBigMap'.
checkBigMapPresence :: Sing (ty :: T) -> BigMapPresence ty
checkBigMapPresence = \case
  -- More boilerplate to boilerplate god.
  STKey -> BigMapAbsent
  STSignature -> BigMapAbsent
  STChainId -> BigMapAbsent
  STUnit -> BigMapAbsent
  STOption t -> case checkBigMapPresence t of
    BigMapPresent -> BigMapPresent
    BigMapAbsent -> BigMapAbsent
  STList t -> case checkBigMapPresence t of
    BigMapPresent -> BigMapPresent
    BigMapAbsent -> BigMapAbsent
  STSet _ -> BigMapAbsent
  STOperation -> BigMapAbsent
  STContract t -> case checkBigMapPresence t of
    BigMapPresent -> BigMapPresent
    BigMapAbsent -> BigMapAbsent
  STPair a b -> case (checkBigMapPresence a, checkBigMapPresence b) of
    (BigMapPresent, _) -> BigMapPresent
    (_, BigMapPresent) -> BigMapPresent
    (BigMapAbsent, BigMapAbsent) -> BigMapAbsent
  STOr a b -> case (checkBigMapPresence a, checkBigMapPresence b) of
    (BigMapPresent, _) -> BigMapPresent
    (_, BigMapPresent) -> BigMapPresent
    (BigMapAbsent, BigMapAbsent) -> BigMapAbsent
  STLambda _ _ -> BigMapAbsent
  STMap _ v -> case checkBigMapPresence v of
    BigMapPresent -> BigMapPresent
    BigMapAbsent -> BigMapAbsent
  STBigMap _ _ ->
    BigMapPresent
  STInt -> BigMapAbsent
  STNat -> BigMapAbsent
  STString -> BigMapAbsent
  STBytes -> BigMapAbsent
  STMutez -> BigMapAbsent
  STBool -> BigMapAbsent
  STKeyHash -> BigMapAbsent
  STTimestamp -> BigMapAbsent
  STAddress -> BigMapAbsent

-- | Check at runtime whether the given type contains 'TBigMap'.
checkNestedBigMapsPresence :: Sing (ty :: T) -> NestedBigMapsPresence ty
checkNestedBigMapsPresence = \case
  -- More boilerplate to boilerplate god.
  STKey -> NestedBigMapsAbsent
  STSignature -> NestedBigMapsAbsent
  STChainId -> NestedBigMapsAbsent
  STUnit -> NestedBigMapsAbsent
  STOption t -> case checkNestedBigMapsPresence t of
    NestedBigMapsPresent -> NestedBigMapsPresent
    NestedBigMapsAbsent -> NestedBigMapsAbsent
  STList                   t -> case checkNestedBigMapsPresence t of
    NestedBigMapsPresent -> NestedBigMapsPresent
    NestedBigMapsAbsent -> NestedBigMapsAbsent
  STSet _ -> NestedBigMapsAbsent
  STOperation -> NestedBigMapsAbsent
  STContract t -> case checkNestedBigMapsPresence t of
    NestedBigMapsPresent -> NestedBigMapsPresent
    NestedBigMapsAbsent -> NestedBigMapsAbsent
  STPair a b -> case (checkNestedBigMapsPresence a, checkNestedBigMapsPresence b) of
    (NestedBigMapsPresent, _) -> NestedBigMapsPresent
    (_, NestedBigMapsPresent) -> NestedBigMapsPresent
    (NestedBigMapsAbsent, NestedBigMapsAbsent) -> NestedBigMapsAbsent
  STOr a b -> case (checkNestedBigMapsPresence a, checkNestedBigMapsPresence b) of
    (NestedBigMapsPresent, _) -> NestedBigMapsPresent
    (_, NestedBigMapsPresent) -> NestedBigMapsPresent
    (NestedBigMapsAbsent, NestedBigMapsAbsent) -> NestedBigMapsAbsent
  STLambda _ _ -> NestedBigMapsAbsent
  STMap _ v -> case checkNestedBigMapsPresence v of
    NestedBigMapsPresent -> NestedBigMapsPresent
    NestedBigMapsAbsent -> NestedBigMapsAbsent
  STBigMap _ v -> case checkBigMapPresence v of
    BigMapPresent -> NestedBigMapsPresent
    BigMapAbsent -> NestedBigMapsAbsent
  STInt -> NestedBigMapsAbsent
  STNat -> NestedBigMapsAbsent
  STString -> NestedBigMapsAbsent
  STBytes -> NestedBigMapsAbsent
  STMutez -> NestedBigMapsAbsent
  STBool -> NestedBigMapsAbsent
  STKeyHash -> NestedBigMapsAbsent
  STTimestamp -> NestedBigMapsAbsent
  STAddress -> NestedBigMapsAbsent

-- | Check at runtime that the given type does not contain 'TOperation'.
opAbsense :: Sing (t :: T) -> Maybe (Dict $ HasNoOp t)
opAbsense s = case checkOpPresence s of
  OpPresent -> Nothing
  OpAbsent -> Just Dict

-- | Check at runtime that the given type does not contain 'TContract'.
contractTypeAbsense :: Sing (t :: T) -> Maybe (Dict $ HasNoContract t)
contractTypeAbsense s = case checkContractTypePresence s of
  ContractPresent -> Nothing
  ContractAbsent -> Just Dict

-- | Check at runtime that the given type does not containt 'TBigMap'
bigMapAbsense :: Sing (t :: T) -> Maybe (Dict $ HasNoBigMap t)
bigMapAbsense s = case checkBigMapPresence s of
  BigMapPresent -> Nothing
  BigMapAbsent -> Just Dict

-- | Check at runtime that the given type does not contain nested 'TBigMap'
nestedBigMapsAbsense :: Sing (t :: T) -> Maybe (Dict $ HasNoNestedBigMaps t)
nestedBigMapsAbsense s = case checkNestedBigMapsPresence s of
  NestedBigMapsPresent -> Nothing
  NestedBigMapsAbsent -> Just Dict

----------------------------------------------------------------------------
-- Scopes
----------------------------------------------------------------------------

data BadTypeForScope
  = BtNotComparable
  | BtIsOperation
  | BtHasBigMap
  | BtHasNestedBigMap
  | BtHasContract
  deriving stock (Show, Eq, Generic)

-- | Alias for constraints which Michelson applies to parameter.
type ParameterScope t =
  (Typeable t, SingI t, HasNoOp t, HasNoNestedBigMaps t)

-- | Alias for constraints which Michelson applies to contract storage.
type StorageScope t =
  (Typeable t, SingI t, HasNoOp t, HasNoNestedBigMaps t, HasNoContract t)

-- | Alias for constraints which Michelson applies to pushed constants.
type ConstantScope t =
  (SingI t, HasNoOp t, HasNoBigMap t, HasNoContract t)

-- | Alias for constraints which Michelson applies to packed values.
type PackedValScope t =
  (SingI t, HasNoOp t, HasNoBigMap t)

-- | Alias for constraints which Michelson applies to unpacked values.
--
-- It is different from 'PackedValScope', e.g. @contract@ type cannot appear
-- in a value we unpack to.
type UnpackedValScope t =
  (PackedValScope t, ConstantScope t)

-- | Alias for constraints which are required for printing.
type PrintedValScope t = (SingI t, HasNoOp t)

----------------------------------------------------------------------------
-- Conveniences
----------------------------------------------------------------------------

-- | Should be present for common scopes.
class CheckScope (c :: Constraint) where
  -- | Check that constraint hold for a given type.
  checkScope :: Either BadTypeForScope (Dict c)

instance SingI t => CheckScope (HasNoOp t) where
  checkScope = maybeToRight BtIsOperation $ opAbsense sing
instance SingI t => CheckScope (HasNoBigMap t) where
  checkScope = maybeToRight BtHasBigMap $ bigMapAbsense sing
instance SingI t => CheckScope (HasNoNestedBigMaps t) where
  checkScope = maybeToRight BtHasNestedBigMap $ nestedBigMapsAbsense sing
instance SingI t => CheckScope (HasNoContract t) where
  checkScope = maybeToRight BtHasContract $ contractTypeAbsense sing

instance (Typeable t, SingI t) => CheckScope (ParameterScope t) where
  checkScope =
    (\Dict Dict -> Dict)
      <$> checkScope @(HasNoOp t)
      <*> checkScope @(HasNoNestedBigMaps t)

instance (Typeable t, SingI t) => CheckScope (StorageScope t) where
  checkScope =
    (\Dict Dict Dict -> Dict)
      <$> checkScope @(HasNoOp t)
      <*> checkScope @(HasNoNestedBigMaps t)
      <*> checkScope @(HasNoContract t)

instance (Typeable t, SingI t) => CheckScope (ConstantScope t) where
  checkScope =
    (\Dict Dict Dict -> Dict)
      <$> checkScope @(HasNoOp t)
      <*> checkScope @(HasNoBigMap t)
      <*> checkScope @(HasNoContract t)

instance (Typeable t, SingI t) => CheckScope (PackedValScope t) where
  checkScope =
    (\Dict Dict -> Dict)
      <$> checkScope @(HasNoOp t)
      <*> checkScope @(HasNoBigMap t)

instance (Typeable t, SingI t) => CheckScope (UnpackedValScope t) where
  checkScope =
    (\Dict Dict -> Dict)
      <$> checkScope @(PackedValScope t)
      <*> checkScope @(ConstantScope t)

-- Versions for eDSL
----------------------------------------------------------------------------

{- These constraints are supposed to be used only in eDSL code and eDSL should
define its own wrapers over it.
-}

type ProperParameterBetterErrors t =
  (Typeable t, SingI t, ForbidOp t, ForbidNestedBigMaps t)

type ProperStorageBetterErrors t =
  (Typeable t, SingI t, ForbidOp t, ForbidNestedBigMaps t, ForbidContract t)

type ProperConstantBetterErrors t =
  (SingI t, ForbidOp t, ForbidBigMap t, ForbidContract t)

type ProperPackedValBetterErrors t =
  (SingI t, ForbidOp t, ForbidBigMap t)

type ProperUnpackedValBetterErrors t =
  (ProperPackedValBetterErrors t, ProperConstantBetterErrors t)

type ProperPrintedValBetterErrors t =
  (SingI t, ForbidOp t)

properParameterEvi :: forall t. ProperParameterBetterErrors t :- ParameterScope t
properParameterEvi = Sub $
  Dict \\ forbiddenOpEvi @t \\ forbiddenNestedBigMapsEvi @t

properStorageEvi :: forall t. ProperStorageBetterErrors t :- StorageScope t
properStorageEvi = Sub $
  Dict \\ forbiddenOpEvi @t
       \\ forbiddenContractTypeEvi @t
       \\ forbiddenNestedBigMapsEvi @t
       \\ forbiddenContractTypeEvi @t

properConstantEvi :: forall t. ProperConstantBetterErrors t :- ConstantScope t
properConstantEvi = Sub $
  Dict \\ forbiddenOpEvi @t
       \\ forbiddenBigMapEvi @t
       \\ forbiddenContractTypeEvi @t

properPackedValEvi :: forall t. ProperPackedValBetterErrors t :- PackedValScope t
properPackedValEvi = Sub $
  Dict \\ forbiddenOpEvi @t
       \\ forbiddenBigMapEvi @t

properUnpackedValEvi :: forall t. ProperUnpackedValBetterErrors t :- UnpackedValScope t
properUnpackedValEvi = properPackedValEvi @t *** properConstantEvi @t

properPrintedValEvi :: forall t. ProperPrintedValBetterErrors t :- PrintedValScope t
properPrintedValEvi = Sub $
  Dict \\ forbiddenOpEvi @t
