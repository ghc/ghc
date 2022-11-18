{-# LANGUAGE DataKinds #-}

module T20531_defs where

-- Definitions to be used in GHCi scripts
data T = MkT
type L0 = '[] :: [T]
type L1 = '[ 'MkT]
type L2 = '[ 'MkT, 'MkT ]
type Tup0 = '()
type Tup2 = '( 'MkT, 'MkT )
type S = MkT
data And a b = a :& b
type I = T :& MkT