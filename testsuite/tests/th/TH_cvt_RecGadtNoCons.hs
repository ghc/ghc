{-# LANGUAGE TemplateHaskell #-}

module TH_cvt_RecGadtNoCons where

import Language.Haskell.TH

$(return [DataD [] (mkName "T") [] Nothing
    [RecGadtC [] [] (ConT (mkName "T"))] []])
