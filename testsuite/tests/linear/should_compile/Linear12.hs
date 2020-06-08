{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs #-}
module Linear12 where

type N a = a ⊸ ()

consume :: a ⊸ N a ⊸ ()
consume x k = k x

data N' a where N :: N a ⊸ N' a

consume' :: a ⊸ N' a ⊸ ()
consume' x (N k) = k x

data W = W (W ⊸ ())

wPlusTwo :: W ⊸ W
wPlusTwo n = W (\(W k) -> k n)

data Nat = S Nat

natPlusOne :: Nat ⊸ Nat
natPlusOne n = S n

data D = D ()

mkD :: () ⊸ D
mkD x = D x

data Odd = E Even
data Even = O Odd

evenPlusOne :: Even ⊸ Odd
evenPlusOne e = E e
