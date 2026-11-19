{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module NativeTypeCaseReify where
import Data.Type.Equality ((:~:)(Refl))
import Language.Haskell.TH
import NativeCase

-- Reuse the private family occurrence returned by reify under its exact Name.
type Seen = $(do
  info <- reify ''F
  case info of
    TyConI (TySynD _ [_] (AppT (AppT family _) _)) ->
      pure (AppT (AppT family (PromotedT 'True)) (PromotedT 'True))
    _ -> fail (pprint info))

checked :: Seen :~: Int
checked = Refl
