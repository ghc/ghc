{-# LANGUAGE DataKinds, LinearTypes, Modifiers, PolyKinds, NoExperimentalDependentTypes #-}
{-# LANGUAGE TemplateHaskell, TypeApplications, TypeOperators #-}
module NativePercentArrowQuote where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, M, U, Matchability, Multiplicity(One))
import Language.Haskell.TH
import NativePercentArrowProvider (LeftOrder)

type Checked = $([t| Int %U %One -> Bool |])
checked :: Checked :~: FUN @U 'One Int Bool
checked = Refl

type Callback = $([t| forall (m :: Matchability) (p :: Multiplicity) a b. a %m %p -> b |])
type Old = $([t| forall (p :: Multiplicity) a b. a %p -> b |])
type Linear = $([t| Int %1 -> Bool |])

type Identity (m :: Matchability) = m
type Alias = $([t| forall (m :: Matchability) a b. a %(Identity m :: Matchability) -> b |])

type FromInterface = $(do
    info <- reify ''LeftOrder
    case info of
      TyConI (TySynD _ [KindedTV a _ _, KindedTV b _ _] rhs) ->
        pure (ForallT [PlainTV a SpecifiedSpec, PlainTV b SpecifiedSpec] [] rhs)
      TyConI (TySynD _ [PlainTV a _, PlainTV b _] rhs) ->
        pure (ForallT [PlainTV a SpecifiedSpec, PlainTV b SpecifiedSpec] [] rhs)
      _ -> fail (pprint info))
