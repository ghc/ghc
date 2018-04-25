{-# LANGUAGE PartialTypeSignatures, RankNTypes #-}

module ExprSigLocal where

-- We expect this to compile fine,
-- reporting that '_' stands 'a'

y :: forall b. b->b
y  = ((\x -> x) :: forall a. a -> _)

g :: forall a. a -> _
g x = x
