{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTSyntax #-}

module GadtConstructors where

import Language.Haskell.TH

$( do
    typ <- mkName "T"
    pure
      [ DataD [] typ [] Nothing [GadtC [] [] (ConT typ)] []
      ]
)
