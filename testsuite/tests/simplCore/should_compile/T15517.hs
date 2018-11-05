{-# LANGUAGE PatternSynonyms #-}
module T15517 where

data Nat = Z | S Nat

pattern Zpat = Z

sfrom :: Nat -> () -> Bool
sfrom Zpat  = \_ -> False
sfrom (S Z) = \_ -> False
