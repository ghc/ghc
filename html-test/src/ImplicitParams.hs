{-# LANGUAGE ImplicitParams, RankNTypes #-}
module ImplicitParams where

data X = X

c :: (?x :: X) => X
c = ?x

d :: (?x :: X, ?y :: X) => (X, X)
d = (?x, ?y)

f :: ((?x :: X) => a) -> a
f a = let ?x = X in a
