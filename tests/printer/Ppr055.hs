{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedSums #-}
module Ppr055 where

import Language.Haskell.TH

foo :: $(conT (unboxedSumTypeName 2) `appT` conT ''() `appT` conT ''())
    -> $(conT (unboxedSumTypeName 2) `appT` conT ''() `appT` conT ''())
foo $(conP (unboxedSumDataName 1 2) [conP '() []])
  = $(conE (unboxedSumDataName 2 2) `appE` conE '())
foo $(conP (unboxedSumDataName 2 2) [conP '() []])
  = $(conE (unboxedSumDataName 2 2) `appE` conE '())

foo2 :: (# () | () #)
     -> $(conT (unboxedSumTypeName 2) `appT` conT ''() `appT` conT ''())
foo2 (# () | #) = $(conE (unboxedSumDataName 2 2) `appE` conE '())
foo2 $(conP (unboxedSumDataName 2 2) [conP '() []]) = (# | () #)


foo3 :: (# () | () | () | () #) -> Int
foo3 (# |  |  ()   |  #) = 3
