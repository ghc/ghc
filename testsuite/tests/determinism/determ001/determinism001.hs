module Main where

import GHC.Data.Graph.Directed

main = mapM_ print
  [ test001
  , test002
  , test003
  , test004
  ]

-- These check that the result of SCCs doesn't depend on the order of the key
-- type (Int here).

test001 = testSCC [("a", 1, []), ("b", 2, []), ("c", 3, [])]

test002 = testSCC [("a", 2, []), ("b", 3, []), ("c", 1, [])]

test003 = testSCC [("b", 1, []), ("c", 2, []), ("a", 3, [])]

test004 = testSCC [("b", 2, []), ("c", 3, []), ("a", 1, [])]

testSCC = flattenSCCs . stronglyConnCompFromEdgedVerticesOrd . map toNode
  where
    toNode (a, b, c) = DigraphNode a b c
