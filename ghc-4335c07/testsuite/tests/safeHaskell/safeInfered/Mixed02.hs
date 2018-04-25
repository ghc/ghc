{-# LANGUAGE Unsafe #-}
-- | Should fail compilation because we import Data.Word as safe and unsafe
module Mixed02 where

import safe qualified Data.Word as DW
import Data.Word

f :: Int
f = 1

