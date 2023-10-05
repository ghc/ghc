{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module T13587A where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

importDoubleToDouble :: String -> Q (TExp (Double -> Double))
importDoubleToDouble fname = do
    n <- newName fname
    d <- forImpD CCall unsafe fname n [t|Double -> Double|]
    addTopDecls [d]
    unsafeTExpCoerce (varE n)
