{-# OPTIONS -fglasgow-exts #-}

{-

This examples serialisation and de-serialisation,
but we replace *series* by *trees*.

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
        res    = maybe Nothing (kids . fromConstr) con
        ta     = fromJust res
        con    = stringCon (dataTypeOf ta) x
        kids x =
          do guard (glength x == length ts)
             x' <- gzipWithM (\t -> const (gdetree t)) ts x
             return x'


-- Main function for testing
main = print $ (   genCom
               , ( gtree genCom 
               , ( (gdetree (gtree genCom)) :: Maybe Company 
               , ( Just genCom == gdetree (gtree genCom)
               ))))
