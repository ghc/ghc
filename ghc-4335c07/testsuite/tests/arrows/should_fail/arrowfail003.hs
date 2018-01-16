{-# LANGUAGE Arrows #-}
-- Arrow commands where an expression is expected

module ShouldFail where

import Control.Arrow

foo = returnA -< []
bar = (|zeroArrow|)
