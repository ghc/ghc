{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeFamilies #-}

module T26256 (go) where

import Data.Kind

class Cat k where (<<<) :: k a b -> k x a -> k x b
instance Cat (->) where (<<<) = (.)
class Pro k p where pro :: k a b s t -> p a b -> p s t
data Hiding o a b s t = forall e. Hiding (s -> o e a)
newtype Apply e a = Apply (e a)

type (:->) :: Type -> Type -> Type
type family (:->) where
  (:->) = (->)

go :: (Pro (Hiding Apply) p) => (s :-> e a) -> p a b -> p s t
go sea = pro (Hiding (Apply <<< sea))
