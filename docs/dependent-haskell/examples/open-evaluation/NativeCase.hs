{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ExplicitForAll, PolyKinds, RankNTypes, TypeFamilies #-}
-- | Select a type from the constructor of a type-level scrutinee.
module NativeCase
  ( F, ByType, ElementOrSelf, Field, Same, Capture, Braces, roadmap
  , PayloadKind, Fixed, Captured, Retain, PatternShadow, ScrutineeShadow, NestedScope
  , Paired, InferredCapture, ContextKind, SeparateKinds
  , NestedLetCase, NestedLetShadow, NestedLetKind
  , TupleKindLet, AliasTupleKindLet, NestedTupleKindLet, PolyNestedTupleKindLet
  , AscribedCaseKind, SignedCaseRhs
  , CaseRhsAliasKind, CaseBodyAliasKind
  , caseIdentity, nestedCaseIdentity, retainedPick, renderRetainedPick, retainedKeep
  , retainedDoubleNot, renderRetainedDoubleNot, retainedPermission
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeNats (Nat)
import qualified OpenMapWorker as Open
import Prelude
import qualified Prelude as P

-- | Choose an ordinary result type from a Boolean.
type F a = case a of
  True -> Int
  False -> Bool

-- | Select a result from the input type itself.
type ByType (a :: Type) = case a of
  Bool     -> Int
  Ordering -> Char

-- | Remove one Maybe layer and keep any other type.
type ElementOrSelf (a :: Type) = case a of
  Maybe b -> b
  other -> other

-- | Bind the field of Just in its alternative.
type Field x = case x of
  Just x -> '(x,x)
  Nothing -> '( 'False, 'False)

-- | Keep the case stuck until a constructor is known.
type Same a = case a of
  True -> Int
  False -> Int

-- | Refer to both an enclosing parameter and a pattern-bound field.
type Capture outer input = case input of
  Just x -> '(outer,x)
  Nothing -> '(outer, 'False)

-- | Use explicit braces for the alternatives.
type Braces a = case a of { True -> Int; False -> Bool; }

-- | Observe the two selected result types.
roadmap :: (F 'True, Braces 'False)
roadmap = (7, False)

-- | Return the argument at the same selected type.
caseIdentity :: forall (b :: Bool). Proxy b ->
  (case b of { True -> Int; False -> Bool }) ->
  (case b of { True -> Int; False -> Bool })
caseIdentity _ input = input

-- | Return the input selected by the inner flag.
nestedCaseIdentity :: forall (b :: Bool). Proxy b ->
  (case b of { True -> Int; False -> Bool }) ->
  (forall (b :: Bool). Proxy b ->
    (case b of { True -> Int; False -> Bool }) ->
    (case b of { True -> Int; False -> Bool }))
nestedCaseIdentity _ _ _ input = input

-- | Select the result type from a runtime Boolean.
retainedPick :: foreach (b :: Bool) -> (case b of { True -> Int; False -> Char })
retainedPick True = 7
retainedPick False = 'f'

-- | Render a result selected by the same runtime Boolean.
renderRetainedPick :: foreach (flag :: Bool) -> String
renderRetainedPick flag = render flag (retainedPick flag)

-- | Render each selected type with its own Show instance.
render :: foreach (selected :: Bool) -> (case selected of { True -> Int; False -> Char }) -> String
render True value = show value
render False value = show value

-- | Return a constant without forcing either argument.
retainedKeep :: foreach (flag :: Bool) -> (case flag of { True -> Int; False -> Char }) -> Int
retainedKeep _ _ = 42

-- | Select the result after two checked Boolean negations.
retainedDoubleNot :: foreach (b :: Bool)
                  -> (case not (not b) of { True -> Int; False -> Bool })
retainedDoubleNot True = 42
retainedDoubleNot False = False

-- | Render the selected type through a qualified spelling of the same call.
renderRetainedDoubleNot :: foreach (b :: Bool) -> String
renderRetainedDoubleNot b = renderDoubleNot b (retainedDoubleNot b)

renderDoubleNot :: foreach (b :: Bool)
                -> (case P.not (P.not b) of { True -> Int; False -> Bool }) -> String
renderDoubleNot True value = show value
renderDoubleNot False value = show value

-- | Select a type from two runtime operands.
retainedPermission :: foreach (light :: Open.Lamp) -> foreach (allowed :: Bool)
                   -> Proxy (case Open.mayProceed light allowed of { True -> Int; False -> Char })
retainedPermission _ _ = Proxy

-- | Reuse a local kind while retaining the kind of an outer parameter.
type Fixed (k :: Type) (a :: k) = let k = Bool in case 'False of
  'False -> (Proxy a, Proxy ('True :: k))
  'True  -> (Proxy a, Proxy ('False :: k))

-- | Select the type named by the local binding.
type family PayloadKind (a :: Bool) :: Type where
  PayloadKind 'False = Bool
  PayloadKind 'True = Ordering

type Captured (a :: Bool) (b :: Bool) = let k = PayloadKind a in case b of
  'False -> Maybe k
  'True  -> [k]

type Retain (k :: Type) (a :: k) = let x = a in case 'False of
  'False -> Proxy x
  'True  -> Maybe (Proxy x)

type PatternShadow (a :: Bool) = let k = PayloadKind a in case 'Just 'False of
  'Just a  -> (Proxy a, Maybe k)
  'Nothing -> (Proxy 'True, Maybe k)

-- The inner case must read the local False, even when the outer k is True.
type ScrutineeShadow (k :: Bool) = let k = 'False in
  case (case 'True of { 'True -> k; 'False -> 'True }) of
    'False -> Int
    'True  -> Bool

-- A let in the scrutinee shadows x only in that scrutinee.
type NestedScope = let x = 'False in
  case (let x = 'True in case 'True of { 'True -> x; 'False -> 'False }) of
    'True  -> Proxy x
    'False -> Proxy 'True

-- | Capture two values along with the kinds that scope them.
type Paired (k :: Type) (x :: k) (l :: Type) (y :: l) b =
  let p = '(x, y) in case b of
    'True  -> Proxy p
    'False -> Maybe (Proxy p)

-- | Infer the kind of a visible capture before checking either branch.
type InferredCapture a b = let x = a in case b of
  'True  -> Proxy x
  'False -> Maybe (Proxy x)

type BoolUse (m :: Maybe Bool) = Bool
type TypeFlag (m :: Maybe Type) = 'True

-- | Both alternatives constrain the kind of the one local Nothing.
type ContextKind b = let m = 'Nothing in case b of
  'True  -> BoolUse m
  'False -> BoolUse m

-- | The inner and outer definitions have different inferred kinds.
type SeparateKinds b = let m = 'Nothing in
  case (let n = 'Nothing in case b of
    'True  -> TypeFlag n
    'False -> TypeFlag n) of
    'True  -> BoolUse m
    'False -> Maybe (BoolUse m)


-- | Use two earlier aliases before shadowing one of them.
type NestedLetCase (a :: Type) (d :: Maybe Bool) =
  let a = Maybe a in
  let b = Either a a in
  let a = '(a, b) in
  case d of
    'Just 'True -> Proxy a
    'Just _ -> Proxy a
    'Nothing -> Proxy a

-- | A pattern shadows the written name only in its own branch.
type NestedLetShadow (b :: Bool) (d :: Maybe Bool) =
  let b = 'Just b in
  let inner = b in
  case d of
    'Just b -> (Proxy b, Proxy inner)
    'Nothing -> (Proxy b, Proxy inner)

-- | Both aliases share the inferred kind of the outer RHS.
type NestedLetKind (b :: Bool) =
  let outer = 'Nothing in
  let inner = outer in
  case b of
    'True  -> BoolUse inner
    'False -> BoolUse outer

-- | Select a type after a let binds a field of a parameter with a tuple kind.
type TupleKindLet (dims :: (Bool, Nat)) =
  let (flag, _) = dims
  in case flag of
    'True -> Int
    'False -> Bool

type Dimensions = (Bool, Nat)

-- | Use a named kind for the same selection.
type AliasTupleKindLet (dims :: Dimensions) =
  let (flag, _) = dims
  in case flag of
    'True -> Int
    'False -> Bool

-- | Capture a parameter with a nested tuple kind.
type NestedTupleKindLet (dims :: ((Bool, Nat), Nat)) =
  let ((flag, _), _) = dims
  in case flag of
    'True -> Int
    'False -> Ordering

-- | Capture the nested projection and its enclosing kind variable.
type PolyNestedTupleKindLet (k :: Type) (dims :: ((Bool, k), Nat)) =
  let ((flag, _), _) = dims
  in case flag of
    'True -> Int
    'False -> k

-- | Use the enclosing kind even when a branch binds the same name.
type AscribedCaseKind (k :: Type) (input :: Maybe Bool) =
  (case input of
    'Just k -> 'Nothing
    'Nothing -> 'Nothing) :: Maybe k

-- | Check both branches of a case RHS at the local binding's kind.
type SignedCaseRhs (b :: Bool) =
  let result :: Maybe Bool
      result = case b of
        'True -> 'Nothing
        'False -> 'Nothing
  in Proxy result

-- | The result signature uses both the original and the shadowing kind.
type CaseRhsAliasKind (payload :: Type) (b :: Bool) =
  let base = Maybe payload
      payload = Bool
      result :: (base, Maybe payload)
      result = case b of
        'True -> '( 'Nothing, 'Just 'True)
        'False -> '( 'Nothing, 'Nothing)
  in Proxy result

-- | The constructor field can use the same name in its own branch.
type CaseBodyAliasKind (input :: Maybe Bool) =
  let payload = Bool in
  ((case input of
    'Just payload -> 'Just payload
    'Nothing -> 'Nothing) :: Maybe payload)
