{-# LANGUAGE Arrows, ExistentialQuantification #-}

-- Trac #1662

module ShouldFail where

import Control.Arrow

data T = forall a. T a

panic :: (Arrow arrow) => arrow T T
panic = proc (T x) -> do returnA -< T x