{-# LANGUAGE TemplateHaskell #-}

module TH_cvt_GadtNoCons where

import Language.Haskell.TH

$(return [DataD [] (mkName "T") [] Nothing
    [GadtC [] [] (ConT (mkName "T"))] []])
