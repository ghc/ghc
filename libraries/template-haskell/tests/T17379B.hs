{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTSyntax #-}

module T17379B where

import Language.Haskell.TH

$( do
    typ <- mkName "T"
    pure
      [ DataD [] typ [] Nothing [RecGadtC [] [] (ConT typ)] []
      ]
)
