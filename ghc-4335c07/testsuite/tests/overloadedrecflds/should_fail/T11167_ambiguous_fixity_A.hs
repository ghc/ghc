{-# LANGUAGE DuplicateRecordFields #-}
module T11167_ambiguous_fixity_A where
data A = MkA { foo :: Int -> Int }
data C = MkC { foo :: Int -> Int }
infixr 3 `foo`
