{-# OPTIONS -fglasgow-exts #-}

{-

This example illustrates serialisation and de-serialisation,
but we replace *series* by *trees* so to say.

-}

module Main where
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
main = print $ (   genCom
               , ( data2tree genCom 
               , ( (tree2data (data2tree genCom)) :: Maybe Company 
               , ( Just genCom == tree2data (data2tree genCom)
               ))))
