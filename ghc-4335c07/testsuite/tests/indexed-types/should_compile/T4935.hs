{-# LANGUAGE TypeFamilies, RankNTypes, ScopedTypeVariables #-}
module T4935 where

import Control.Applicative

data TFalse
data TTrue

data Tagged b a = Tagged {at :: a}
type At b = forall a. Tagged b a -> a

class TBool b where onTBool :: (b ~ TFalse => c) -> (b ~ TTrue => c) -> Tagged b c
instance TBool TFalse where onTBool f _ = Tagged $ f
instance TBool TTrue where onTBool _ t = Tagged $ t

type family CondV c f t
type instance CondV TFalse f t = f
type instance CondV TTrue f t = t

newtype Cond c f a = Cond {getCond :: CondV c a (f a)}
cond :: forall c f a g. (TBool c, Functor g) => (c ~ TFalse => g a) -> (c ~ TTrue => g (f a)) -> g (Cond c f a)
cond f t = (at :: At c) $ onTBool (fmap Cond f) (fmap Cond t)
condMap :: (TBool c, Functor f) => (a -> b) -> Cond c f a -> Cond c f b
condMap g (Cond n) = cond g (fmap g) n
