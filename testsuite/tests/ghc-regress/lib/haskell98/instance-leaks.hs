-- Check that the instances in Control.Monad.Instances do not leak
-- into any Haskell 98 modules.
module Main where

-- import all Haskell 98 modules
import Array
import Char
import Complex
import CPUTime
import Directory
import IO
import Ix
import List
import Locale
import Maybe
import Monad
import Numeric
import Random
import Ratio
import System
import Time

-- This will fail if any of the Haskell 98 modules indirectly import
-- Control.Monad.Instances
instance Functor ((->) r) where fmap = (.)

main = undefined
