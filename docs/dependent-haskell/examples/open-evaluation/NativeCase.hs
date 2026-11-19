{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, TypeFamilies #-}
-- | Select a type from the constructor of a type-level scrutinee.
module NativeCase
  ( F, Field, Same, Capture, Braces, roadmap
  , PayloadKind, Fixed, Captured, Retain, PatternShadow, ScrutineeShadow, NestedScope
  , Paired, InferredCapture, ContextKind, SeparateKinds
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy)

-- | Choose an ordinary result type from a Boolean.
type F a = case a of
  True -> Int
  False -> Bool

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
