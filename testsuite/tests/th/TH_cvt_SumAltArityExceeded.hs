{-# LANGUAGE TemplateHaskell #-}

module TH_cvt_SumAltArityExceeded where

import Language.Haskell.TH

$(return [ValD (VarP (mkName "x"))
    (NormalB (UnboxedSumE (LitE (IntegerL 1)) 3 2)) []])
