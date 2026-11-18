{-# LANGUAGE NoExperimentalDependentTypes #-}
module ClosedLengthProducer (length) where

import Numeric.Natural (Natural)
import Prelude hiding (length)

-- | Count cells without inspecting the elements.
length :: [Natural] -> Natural
length xs = foldr (\_ n -> n + 1) 0 xs
