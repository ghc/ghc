{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import TestUtils
import GHC.Records
import GHC.TypeLits
import Data.Tree

data Thing = Thing {field1 :: Char, field2 :: Bool}
  deriving (Show, Eq)

foo :: Thing -> String
foo t = show t.field1 ++ show t.field2
--              ^ this is the point

testing2 (x :: Thing) = x.field1
--                      ^ this is the point
--                        ^ this is the point

data NestedThing = NestedThing { nested1 :: Thing }

nestedSig :: NestedThing -> Char
nestedSig n = n.nested1.field1
--              ^ this is the point
--                      ^ this is the point

nestedNoSig n = n.nested1.field2 :: Bool
--                 ^ this is the point
--                         ^ this is the point



points =
  [ (13,17)
  , (16,25)
  , (16,27)
  , (23,17)
  , (23,25)
  , (27,20)
  , (27,28)
  ]

main = do
  (df, hf) <- readTestHie "HasFieldQueries.hie"
  let refmap = generateReferencesMap $ getAsts $ hie_asts hf

  traverse (explainEv df hf refmap) points
  return ()
