{-# Language DataKinds             #-}
{-# Language TypeApplications         #-}
{-# Language PolyKinds             #-}

module T12045e where

import Data.Kind

data Nat = Zero | Succ Nat
data T (n :: k) = MkT
data D1 n = T @Nat n :! ()
data D2 n = () :!! T @Nat n
data D3 n = T @Nat n :!!! T @Nat n
