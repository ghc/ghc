{-# LANGUAGE TemplateHaskell #-}

module TH_cvt_IllegalSumAlt where

import Language.Haskell.TH

$(return [ValD (VarP (mkName "x"))
    (NormalB (UnboxedSumE (LitE (IntegerL 1)) 0 2)) []])
