{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
module T12513 where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

f :: $([t| (# Int #) |]) -> Int
f x = x

g :: $(unboxedTupleT 1 `appT` conT ''Int) -> Int
g x = x
