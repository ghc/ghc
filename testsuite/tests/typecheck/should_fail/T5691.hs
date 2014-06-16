{-# LANGUAGE ScopedTypeVariables #-}

module GHCBug where

import Control.Applicative as Ap
import Control.Monad (MonadPlus, mplus, mzero)
import Data.Functor.Identity (Identity, runIdentity)

newtype PrintRuleInterp v = MkPRI { printRule_ :: Int -> String }
class Test p where
  test :: p a -> p a

instance Test PrintRuleInterp where
  test (f :: p a) =
    MkPRI $ printRule_ f 


newtype RecDecParser a = MkRD {
  parseRD :: String -> [(String, a)]
  }

pure_ v = MkRD $ \s -> pure (s , v)

instance MonadPlus RecDecParser where
  mzero = MkRD $ const Ap.empty
  mplus a b = MkRD $ const Ap.empty



