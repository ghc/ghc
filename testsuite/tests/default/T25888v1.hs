{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Main  where
import Data.Foldable (Foldable(..))
import Data.Functor.Identity (Identity(..))

default Foldable (Maybe)
default Num (Double)
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


-- This test demonstrates the functionality of ExtendedDefaultRules and NamedDefaults.
--
-- By default, ExtendedDefaultRules implicitly provides:
--   default Foldable ([])
--   default Num (Integer, Double)
--
-- However, we override these with our explicit declarations:
--   default Foldable (Maybe)
--   default Num (Double)
--
-- This test verifies that our overrides work correctly:
--   1. For an unresolved type variable 't' with a 'Foldable t' constraint,
--      't' defaults to 'Maybe' (not '[]')
--   2. For an unresolved type variable 'a' with a 'Num a' constraint,
--      'a' defaults to 'Double' (not 'Integer')
--
-- Expected outcomes:
--   * 'polyFold Int' evaluates to 'Nothing' (not '[]')
--   * '1' is a 'Double' (1.0) (not an 'Integer')
--
-- See also T25888v2.hs for a companion test without overriding the defaults

main :: IO ()
main = do
  print $ polyFold Int
  print $ 1

