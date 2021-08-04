{-# OPTIONS -fglasgow-exts #-}

module Tree (tests) where

{-

This example illustrates serialisation and de-serialisation,
but we replace *series* by *trees* so to say.

-}

import Test.Tasty.HUnit

import Control.Monad.Reader
import Data.Generics
import Data.Maybe
import Data.Tree
import CompanyDatatypes


-- Trealise Data to Tree
data2tree :: Data a => a -> Tree String
data2tree = gdefault `extQ` atString
  where
    atString (x::String) = Node x []
    gdefault x = Node (showConstr (toConstr x)) (gmapQ data2tree x)


-- De-trealise Tree to Data
tree2data :: Data a => Tree String -> Maybe a
tree2data = gdefault `extR` atString
  where
    atString (Node x []) = Just x
    gdefault (Node x ts) = res
      where

	-- a helper for type capture
        res  = maybe Nothing (kids . fromConstr) con

	-- the type to constructed
        ta   = fromJust res

	-- construct constructor
        con  = readConstr (dataTypeOf ta) x

        -- recursion per kid with accumulation
        perkid ts = const (tail ts, tree2data (head ts))

        -- recurse into kids
        kids x =
          do guard (glength x == length ts)
             snd (gmapAccumM perkid ts x)


-- Main function for testing
tests = (   genCom
        , ( data2tree genCom
        , ( (tree2data (data2tree genCom)) :: Maybe Company
        , ( Just genCom == tree2data (data2tree genCom)
        )))) @=? output

output = (C [D "Research" (E (P "Laemmel" "Amsterdam") (S 8000.0)) [PU (E (P "Joost" "Amsterdam") (S 1000.0)),PU (E (P "Marlow" "Cambridge") (S 2000.0))],D "Strategy" (E (P "Blair" "London") (S 100000.0)) []],(Node {rootLabel = "C", subForest = [Node {rootLabel = "(:)", subForest = [Node {rootLabel = "D", subForest = [Node {rootLabel = "Research", subForest = []},Node {rootLabel = "E", subForest = [Node {rootLabel = "P", subForest = [Node {rootLabel = "Laemmel", subForest = []},Node {rootLabel = "Amsterdam", subForest = []}]},Node {rootLabel = "S", subForest = [Node {rootLabel = "8000.0", subForest = []}]}]},Node {rootLabel = "(:)", subForest = [Node {rootLabel = "PU", subForest = [Node {rootLabel = "E", subForest = [Node {rootLabel = "P", subForest = [Node {rootLabel = "Joost", subForest = []},Node {rootLabel = "Amsterdam", subForest = []}]},Node {rootLabel = "S", subForest = [Node {rootLabel = "1000.0", subForest = []}]}]}]},Node {rootLabel = "(:)", subForest = [Node {rootLabel = "PU", subForest = [Node {rootLabel = "E", subForest = [Node {rootLabel = "P", subForest = [Node {rootLabel = "Marlow", subForest = []},Node {rootLabel = "Cambridge", subForest = []}]},Node {rootLabel = "S", subForest = [Node {rootLabel = "2000.0", subForest = []}]}]}]},Node {rootLabel = "[]", subForest = []}]}]}]},Node {rootLabel = "(:)", subForest = [Node {rootLabel = "D", subForest = [Node {rootLabel = "Strategy", subForest = []},Node {rootLabel = "E", subForest = [Node {rootLabel = "P", subForest = [Node {rootLabel = "Blair", subForest = []},Node {rootLabel = "London", subForest = []}]},Node {rootLabel = "S", subForest = [Node {rootLabel = "100000.0", subForest = []}]}]},Node {rootLabel = "[]", subForest = []}]},Node {rootLabel = "[]", subForest = []}]}]}]},(Just (C [D "Research" (E (P "Laemmel" "Amsterdam") (S 8000.0)) [PU (E (P "Joost" "Amsterdam") (S 1000.0)),PU (E (P "Marlow" "Cambridge") (S 2000.0))],D "Strategy" (E (P "Blair" "London") (S 100000.0)) []]),True)))
