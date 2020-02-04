{-# LANGUAGE ApplicativeComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}
module Test where

import Control.Applicative
import Control.Monad
import Prelude

-- Caused a -dcore-lint failure with an earlier version of
-- ApplicativeDo due to the polymorphic let binding.
test :: IO [Char]
test = [map f [] | x <- pure 'a', y <- pure 'b', let f | y == 'c' = id | otherwise = id]
