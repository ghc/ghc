

{-# LANGUAGE RebindableSyntax #-}

module T16618 where

import Prelude hiding (fail)
import Data.Functor.Identity

foo :: Identity Char
foo = do
  () <- pure ()
  pure 'z'
