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


-- Multi-label projection sections: each getField gets the SrcSpan of its own
-- field label, so both HasField dictionaries are reachable.  Before that, no
-- node inside the section had a usable span and hovering it found nothing at
-- all.  The points below are on 'nested1' and 'field1' respectively.
projSection :: NestedThing -> Char
projSection = (.nested1.field1)

projSectionApplied :: NestedThing -> Char
projSectionApplied n = (.nested1.field1) n


points =
  [ (13,17)
  , (16,25)
  , (16,27)
  , (23,17)
  , (23,25)
  , (27,20)
  , (27,28)
  , (37,17)
  , (37,25)
  , (40,26)
  , (40,34)
  ]

main = do
  (df, hf) <- readTestHie "HasFieldQueries.hie"
  let refmap = generateReferencesMap $ getAsts $ hie_asts hf

  traverse (explainEv df hf refmap) points
  return ()
