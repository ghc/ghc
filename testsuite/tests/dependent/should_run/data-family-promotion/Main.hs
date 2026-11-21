{-# LANGUAGE Haskell2010, DataKinds, GADTs, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import TupleProvider (Tuple(Tup2), At(..), fields)
import TuplePromotion

proofAfterImport :: Selected :~: 'True
proofAfterImport = Refl

refinementAfterImport :: Refined :~: 'True
refinementAfterImport = Refl

main :: IO ()
main = case (proof, proofAfterImport, refinementProof, refinementAfterImport, pairIndex) of
    (Refl, Refl, Refl, Refl, At) -> do
        print (fields (Tup2 True False))
        putStrLn reflected
