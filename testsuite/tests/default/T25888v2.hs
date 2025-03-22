{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Main  where
import Data.Foldable (Foldable(..))
import Data.Functor.Identity (Identity(..))

default Show ()
-- This is ensured we cleanup defaults for Show,
-- we do not want Show to effect the defaulting of `Num, Foldable`.
-- since we are using Show to print out the result.

class (Foldable t) => Build t where
  polyFold :: forall a -> t a

instance Build Maybe where
  polyFold x = Nothing

instance Build [] where
  polyFold x = []

-- With ExtendedDefaultRules enabled, we implicitly have the following defaults:
--    default Foldable ([]), Num (Integer, Double)
-- These defaults are not overridden in this module.

-- According to the defaulting rules:
-- 1. For an unresolved type variable 't' with constraint 'Foldable t', 't' defaults to []
-- 2. For an unresolved type variable 'a' with constraint 'Num a', 'a' defaults to Integer

-- Therefore:
-- * polyFold Int should evaluate to [] (empty list)
-- * The literal 1 should be of type Integer

-- See also T25888v1.hs for the same test with overridden defaults
main :: IO ()
main = do
  print $ polyFold Int
  print $ 1

