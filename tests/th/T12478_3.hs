{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedSums #-}
module T12478_3 where

import Language.Haskell.TH

$(do let ubxSum = unboxedSumT 2 `appT` conT ''Int `appT` conT ''Int
     x <- newName "x"
     y <- newName "y"

     [d| swap :: $(ubxSum) -> $(ubxSum)
         swap $(unboxedSumP (varP x) 1 2) = $(unboxedSumE (varE x) 2 2)
         swap $(unboxedSumP (varP y) 2 2) = $(unboxedSumE (varE y) 1 2)
      |])
