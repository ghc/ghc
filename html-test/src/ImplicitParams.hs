{-# LANGUAGE ImplicitParams #-}
module ImplicitParams where

data X

c :: (?x :: X) => X
c = ?x

d :: (?x :: X, ?y :: X) => (X, X)
d = (?x, ?y)
