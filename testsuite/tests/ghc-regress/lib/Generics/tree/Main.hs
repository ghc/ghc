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
gtree :: forall a. Data a => a -> Tree String
gtree = gdefault `extQ` string
  where
    string (x::String) = Node x []
    gdefault x = Node (conString (toConstr x)) (gmapQ gtree x)


-- De-trealise Tree to Data
gdetree :: forall a. Data a => Tree String -> Maybe a
gdetree = gdefault `extR` string
  where
    string (Node x []) = Just x
    gdefault (Node x ts) = res
      where

	-- a helper for type capture
        res  = maybe Nothing (kids . fromConstr) con

	-- the type to constructed
        ta   = fromJust res

	-- construct constructor
        con  = stringCon (dataTypeOf ta) x

        -- recursion per kid with accumulation
        perkid ts = const (tail ts, gdetree (head ts)) 

        -- recurse into kids
        kids x =
          do guard (glength x == length ts)
             snd (gmapAccumM perkid ts x)


-- Main function for testing
main = print $ (   genCom
               , ( gtree genCom 
               , ( (gdetree (gtree genCom)) :: Maybe Company 
               , ( Just genCom == gdetree (gtree genCom)
               ))))
