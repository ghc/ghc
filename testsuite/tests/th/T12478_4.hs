{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedSums #-}
module T12478_4 where

import Language.Haskell.TH

f :: $(unboxedSumT 1 `appT` conT ''()) -> Int
f _ = 42
