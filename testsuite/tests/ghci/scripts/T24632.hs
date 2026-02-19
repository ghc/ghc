{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DataKinds             #-}

module T24632 where

import GHC.TypeLits
import Data.Proxy

data N (n :: Nat) where
    O :: (n ~ 0) => N n
    S :: (1 <= n) => N n

f :: forall n. KnownNat n => (1 <= n => N n) -> N n
f c = case cmpNat @1 @n Proxy Proxy of EQI -> c

g :: KnownNat n => N n
g = f S

--x :: N 1 -- With type signature: no issue
x = g @1

-- ghci> :force x --> internal error: stg_ap_p_ret
