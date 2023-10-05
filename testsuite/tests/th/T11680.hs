{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}


module T11680 where


import qualified Data.List                  as List
import           Language.Haskell.TH.Syntax         (addTopDecls)


------------------------------------------------------------------------------
-- # No suggestions or exact match
ex1 :: Bool
ex1 = noMatches

------------------------------------------------------------------------------
-- # One suggestion (abcd), but no exact match
ex2 :: [a]
ex2 = abce

abcd :: String
abcd = "abcd"

------------------------------------------------------------------------------
-- # No suggestions, but an exact match after a splice
-- # Exact match following a single-line splice
-- # One splice separating use and binding
-- # Annotation does not affect identification of top-level splice
ex3 :: Int
ex3 = foo

------------------------------------------------------------------------------
-- # Suggestions (bat, baz) and an exact match after a splice
-- # Out-of-scope suggestion (bad) NOT reported
-- # Exact match following a multi-line splice
-- # Multiple splices separating use and binding
ex4 :: ()
ex4 = bar

bat :: [Double]
bat = [1.2, 3.4]

baz :: Bool
baz = True

------------------------------------------------------------------------------
-- # No suggestions, but an exact match IN a splice
ex5 :: [Double]
ex5 = ns

------------------------------------------------------------------------------
-- # Qualified suggestion and an exact local match after a splice
-- # Inline bracket does not affect identification of top-level splice
ex6 = intercalate

------------------------------------------------------------------------------
-- # Qualified suggestion, but no exact local match
ex7 = nub

------------------------------------------------------------------------------
-- # No suggestions or matches for an out-of-scope duplicate field selector
ex8 :: Int
ex8 = x undefined

------------------------------------------------------------------------------
-- # Exact match to binding added by addTopDecls in next inter-splice group
ex9 :: ()
ex9 = cat

----------------------------------------------------------------------------
-- END OF INTER-SPLICE GROUP
----------------------------------------------------------------------------

------------------------------------------------------------------------------
-- # In a splice, suggestions for out-of-scope variable (cab) dependent upon
-- #   whether that variable is used in a binding added by addTopDecls:
-- #   cat and cap both suggested for f's cab, only cap suggested for g's cab
$(do
    ds <- [d| f = cab
              cat = ()
            |]
    addTopDecls ds
    [d| g = cab
        cap = True
      |])

------------------------------------------------------------------------------
-- # Exact match to binding added by addTopDecls in current inter-splice group
ex10 :: ()
ex10 = cat

----------------------------------------------------------------------------
-- END OF INTER-SPLICE GROUP
----------------------------------------------------------------------------
$(return [])

{-# ANN foo (Just "hello") #-}
foo :: Int
foo = 23


----------------------------------------------------------------------------
-- END OF INTER-SPLICE GROUP
----------------------------------------------------------------------------
[d| ms = 3 : ns :: [Int]
    ns = 4 : ms
  |]

bar = [| 23.6 : bat |]

bad = False

intercalate = undefined

data D = D { x :: Int
           , y :: Double
           }

data E = E { x :: Bool
           , y :: String
           }
