{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedEnumA as A (choose, score, chooseGadt, scoreGadt)
import qualified NativeClosedEnumB as B
import qualified NativeClosedEnumReexport

first :: A.score (A.choose 0) :~: 7
first = Refl

largeTag :: A.score (A.choose 2) :~: 11
largeTag = Refl

second :: B.choose 0 :~: 'B.Ready
second = Refl

nestedSecond :: B.score (B.choose 0) :~: 17
nestedSecond = Refl

gadtSyntax :: A.scoreGadt (A.chooseGadt 0) :~: 6
gadtSyntax = Refl

ordering :: Prelude.compare 3 5 :~: 'LT
ordering = Refl

main :: IO ()
main = case (first, largeTag, second, nestedSecond, gadtSyntax, ordering) of
  (Refl, Refl, Refl, Refl, Refl, Refl) -> putStrLn "closed enumerations"
