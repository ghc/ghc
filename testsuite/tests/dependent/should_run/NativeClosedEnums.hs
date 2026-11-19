{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Main where

import qualified FiniteTree as T
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedEnumA as A (choose, score, chooseGadt, scoreGadt)
import qualified NativeClosedEnumA as Partial (Token(Ready), score, Growth(Stop), depth)
import qualified NativeClosedEnumA as F (Strict(..), strictValue, readStrict, A(..), B(..), mutual, readMutual)
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

tree :: T.sample 3 :~: 'T.Fork ('T.Leaf 3) ('T.Fork ('T.Leaf 4) ('T.Leaf 5))
tree = Refl

treeOperand :: T.leaves (T.sample 3) :~: '[3, 4, 5]
treeOperand = Refl

strictField :: F.strictValue 4 :~: 'F.Field 4
strictField = Refl

strictOperand :: F.readStrict (F.strictValue 4) :~: 4
strictOperand = Refl

mutual :: F.mutual 5 :~: 'F.Step ('F.Back ('F.End 5))
mutual = Refl

mutualOperand :: F.readMutual (F.mutual 5) :~: 5
mutualOperand = Refl

selected :: Partial.score 'Partial.Ready :~: 7
selected = Refl

unselectedExpansion :: Partial.depth 'Partial.Stop :~: 1
unselectedExpansion = Refl

main :: IO ()
main = case (first, largeTag, second, nestedSecond, gadtSyntax, ordering) of
  (Refl, Refl, Refl, Refl, Refl, Refl) -> putStrLn "closed enumerations"
