{-# LANGUAGE TypeFamilies, GADTs, DataKinds, PolyKinds, NamedWildCards #-}
module NamedWildcardInDataFamilyInstanceLHS where

data MyKind = A | B

data family Sing (a :: k)

data instance Sing (_a :: MyKind) where
    SingA :: Sing A
    SingB :: Sing B
