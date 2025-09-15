{-# LANGUAGE Safe #-}
module M_SafePkg4 where

import qualified M_SafePkg3 as M3
import Data.Word

bigInt :: Int
bigInt = M3.bigInt

type MyWord = Word

