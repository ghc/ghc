{-# LANGUAGE TypeFamilies, GADTs, DataKinds, PolyKinds #-}
module DataFamilyInstanceLHS where
-- Test case from #10586
data MyKind = A | B

data family Sing (a :: k)

data instance Sing (_ :: MyKind) where
    SingA :: Sing A
    SingB :: Sing B

foo :: Sing A
foo = SingA
