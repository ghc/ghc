{-# OPTIONS_GHC -fspecialize-aggressively -fexpose-all-unfoldings  #-}
module T23024 (testPolyn) where

import T23024a

testPolyn :: (forall r. Tensor r => r) -> Vector Double
testPolyn f = gradientFromDelta f
