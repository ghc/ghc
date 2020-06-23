{-# LANGUAGE RankNTypes #-}

module MiniLens ((^.), Getting, Lens', lens, view) where

import Data.Functor.Const (Const(..))

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

type Getting r s a = (a -> Const r a) -> s -> Const r s

view :: Getting a s a -> s -> a
view l = getConst . l Const
{-# INLINE view #-}

(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}
