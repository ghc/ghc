{-# LANGUAGE Unsafe #-}
-- | Should fail compilation because we import Data.Word as safe and unsafe
module Mixed03 where

import safe qualified Data.Word as DW
import System.IO
import Data.Word

f :: Int
f = 1

