{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import TestUtils
import GHC.Records

data A = A { a :: Int, b :: Char }
data B = B { a :: Int, n :: A }
data C
  = C { c :: Float }
  | D { c :: Float }
  | E { e :: Int }

valA = A { a = 1, b = 'b' }
valB = B { a = 1, n = valA }
valC = C { c = 1.0 }
valD = D { c = 2.0 }
valE = E { e = 1 }

accessA :: Int
accessA = valA.a
--             ^ 1

accessField :: HasField "a" r Int => r -> Int
accessField r = r.a
--                ^ 2

nested = valB.n.b
--            ^ ^
--            3 4
multi1a = valC.c
--             ^
--             5
multi1b = valD.c
--             ^
--             6
multi1c :: C -> Float
multi1c someC = someC.c -- Where does this point to?
--                    ^
--                    7
multi3 = valE.e
--            ^
--            8
multi4 = valD.e -- This is a run-time error, but where does it point to?
--            ^
--            9

points =
  [ (23, 16)
  , (27, 19)
  , (30, 15)
  , (30, 17)
  , (33, 16)
  , (36, 16)
  , (40, 23)
  , (43, 15)
  , (46, 15)
  ]

main = do
  (df, hf) <- readTestHie "NoFieldSelectorQueries.hie"
  let refmap = generateReferencesMap $ getAsts $ hie_asts hf

  traverse (explainEv df hf refmap) points
  return ()
