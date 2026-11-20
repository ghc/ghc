{-# LANGUAGE ExperimentalDependentTypes #-}
module Fake where

import GHC.Exists

normal :: Int
normal = case Pack 7 of Pack value -> value
