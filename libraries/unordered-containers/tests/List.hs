module Main (main) where

import Data.HashMap.Internal.List
import Data.List (nub, sort, sortBy)
import Data.Ord (comparing)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>), (===), property, Property)

tests :: Test
tests = testGroup "Data.HashMap.Internal.List"
    [ testProperty "isPermutationBy" pIsPermutation
    , testProperty "isPermutationBy of different length" pIsPermutationDiffLength
    , testProperty "pUnorderedCompare" pUnorderedCompare
    , testGroup "modelUnorderedCompare"
        [ testProperty "reflexive" modelUnorderedCompareRefl
        , testProperty "anti-symmetric" modelUnorderedCompareAntiSymm
        , testProperty "transitive" modelUnorderedCompareTrans
        ]
    ]

pIsPermutation :: [Char] -> [Int] -> Bool
pIsPermutation xs is = isPermutationBy (==) xs xs'
  where
    is' = nub is ++ [maximum (0:is) + 1 ..]
    xs' = map fst . sortBy (comparing snd) $ zip xs is'

pIsPermutationDiffLength :: [Int] -> [Int] -> Property
pIsPermutationDiffLength xs ys =
    length xs /= length ys ==> isPermutationBy (==) xs ys === False

-- | Homogenous version of 'unorderedCompare'
--
-- *Compare smallest non-equal elements of the two lists*.
modelUnorderedCompare :: Ord a => [a] -> [a] -> Ordering
modelUnorderedCompare as bs = compare (sort as) (sort bs)

modelUnorderedCompareRefl :: [Int] -> Property
modelUnorderedCompareRefl xs = modelUnorderedCompare xs xs === EQ

modelUnorderedCompareAntiSymm :: [Int] -> [Int] -> Property
modelUnorderedCompareAntiSymm xs ys = case a of
    EQ -> b === EQ
    LT -> b === GT
    GT -> b === LT
  where
    a = modelUnorderedCompare xs ys
    b = modelUnorderedCompare ys xs

modelUnorderedCompareTrans :: [Int] -> [Int] -> [Int] -> Property
modelUnorderedCompareTrans xs ys zs =
    case (modelUnorderedCompare xs ys, modelUnorderedCompare ys zs) of
        (EQ, yz) -> xz === yz
        (xy, EQ) -> xz === xy
        (LT, LT) -> xz === LT
        (GT, GT) -> xz === GT
        (LT, GT) -> property True
        (GT, LT) -> property True
  where
    xz = modelUnorderedCompare xs zs

pUnorderedCompare :: [Int] -> [Int] -> Property
pUnorderedCompare xs ys =
    unorderedCompare compare xs ys === modelUnorderedCompare xs ys

main :: IO ()
main = defaultMain [tests]
