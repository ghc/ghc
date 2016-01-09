{-# LANGUAGE TypeFamilies #-}

module T11381 where

-- ensure that this code does not compile without InjectiveTypeFamilies and that
-- injectivity error is not reported.
type family F a = r | r -> a
type instance F Int = Bool
type instance F Int = Char
