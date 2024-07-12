{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Main where

import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label.NonDet

import Data.Maybe

data TestBlock (e :: Extensibility) (x :: Extensibility) = TB { label_ :: Label, successors_ :: [Label] }
    deriving (Eq, Show)

instance NonLocal TestBlock where
  entryLabel = label_
  successors = successors_

-- Test the classical diamond shape graph.
test_diamond :: LabelMap (TestBlock C C)
test_diamond = mapFromList $ map (\b -> (label_ b, b)) blocks
  where
    blocks =
        [ TB (mkHooplLabel 1) [mkHooplLabel 2, mkHooplLabel 3]
        , TB (mkHooplLabel 2) [mkHooplLabel 4]
        , TB (mkHooplLabel 3) [mkHooplLabel 4]
        , TB (mkHooplLabel 4) []
        ]

-- Test that the backedge doesn't change anything.
test_diamond_backedge :: LabelMap (TestBlock C C)
test_diamond_backedge = mapFromList $ map (\b -> (label_ b, b)) blocks
  where
    blocks =
        [ TB (mkHooplLabel 1) [mkHooplLabel 2, mkHooplLabel 3]
        , TB (mkHooplLabel 2) [mkHooplLabel 4]
        , TB (mkHooplLabel 3) [mkHooplLabel 4]
        , TB (mkHooplLabel 4) [mkHooplLabel 1]
        ]

-- Test that the "bypass" edge from 1 to 4 doesn't change anything.
test_3 :: LabelMap (TestBlock C C)
test_3 = mapFromList $ map (\b -> (label_ b, b)) blocks
  where
    blocks =
        [ TB (mkHooplLabel 1) [mkHooplLabel 2, mkHooplLabel 4]
        , TB (mkHooplLabel 2) [mkHooplLabel 4]
        , TB (mkHooplLabel 4) []
        ]

-- Like test_3 but with different order of successors for the entry point.
test_4 :: LabelMap (TestBlock C C)
test_4 = mapFromList $ map (\b -> (label_ b, b)) blocks
  where
    blocks =
        [ TB (mkHooplLabel 1) [mkHooplLabel 4, mkHooplLabel 2]
        , TB (mkHooplLabel 2) [mkHooplLabel 4]
        , TB (mkHooplLabel 4) []
        ]


main :: IO ()
main = do
    let result = revPostorderFrom test_diamond (mkHooplLabel 1)
    putStrLn (show $ map label_ result)
    let result = revPostorderFrom test_diamond_backedge (mkHooplLabel 1)
    putStrLn (show $ map label_ result)
    let result = revPostorderFrom test_3 (mkHooplLabel 1)
    putStrLn (show $ map label_ result)
    let result = revPostorderFrom test_4 (mkHooplLabel 1)
    putStrLn (show $ map label_ result)
