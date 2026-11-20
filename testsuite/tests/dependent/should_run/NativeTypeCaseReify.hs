{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module NativeTypeCaseReify where
import Data.Type.Equality ((:~:)(Refl))
import Data.Proxy (Proxy)
import Language.Haskell.TH
import NativeCase
import qualified NativeNestedCase as N

-- Reuse the private family occurrence returned by reify under its exact Name.
type Seen = $(do
  info <- reify ''F
  case info of
    TyConI (TySynD _ [_] (AppT (AppT family _) _)) ->
      pure (AppT (AppT family (PromotedT 'True)) (PromotedT 'True))
    _ -> fail (pprint info))

checked :: Seen :~: Int
checked = Refl

type SeenNested = $(do
  info <- reify ''N.Pick
  case info of
    TyConI (TySynD _ [_] (AppT (AppT family _) _)) ->
      let scrutinee = AppT (PromotedT 'Just) (AppT (PromotedT 'Just) (PromotedT 'True))
      in pure (AppT (AppT family scrutinee) scrutinee)
    _ -> fail (pprint info))

checkedNested :: SeenNested :~: Proxy 'True
checkedNested = Refl
