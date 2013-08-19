{-# LANGUAGE TypeFamilies #-}

module T8142 where

tracer :: (Functor f, Coinductive f) => (c -> f c) -> (c -> f c)
tracer = h where h = (\(_, b) -> ((outI . fmap h) b)) . out

class Functor g => Coinductive g where
  type Nu g :: *
  out     :: Nu g -> g (Nu g)
  outI    :: g (Nu g) -> Nu g
