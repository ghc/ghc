{-# LANGUAGE GADTs, ViewPatterns #-}
module GadtRefinedOrderedScope where

import Data.Proxy
import GadtRefinedOrderedProvider

useEq :: Eq a => Proxy a -> Bool
useEq _ = True

observe :: TrailingContext x y -> Bool
observe (TrailingContext (useEq -> answer)) = answer
