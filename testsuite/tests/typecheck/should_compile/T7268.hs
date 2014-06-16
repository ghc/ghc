{-# LANGUAGE MonoLocalBinds, NoMonomorphismRestriction, RankNTypes #-}
module T7268 where

data X = X { a :: forall a . a -> Bool }

ida :: forall b. b -> Bool
X { a = ida } = error "urk"

bar :: c -> Bool
bar = ida

