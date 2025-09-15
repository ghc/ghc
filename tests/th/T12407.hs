{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
module T12407 where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

$(do let ubxTup = conT (unboxedTupleTypeName 2) `appT` conT ''Int
                                                `appT` conT ''Int
     x <- newName "x"
     y <- newName "y"

     [d| f :: $(ubxTup) -> $(ubxTup)
         f $(conP (unboxedTupleDataName 2) [varP x, varP y])
             = $(conE (unboxedTupleDataName 2) `appE` varE x
                                               `appE` varE y)
      |])
