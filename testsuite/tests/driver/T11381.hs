{-# LANGUAGE TypeFamilies #-}

module T11381 where

-- ensure that this code does not compile without TypeFamilyDependencies and that
-- injectivity error is not reported.
type family F a = r | r -> a
type instance F Bool = Int
type instance F Char = Int
