{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTSyntax #-}

module T17379b where

import Language.Haskell.TH

$(let typ = mkName "T" in pure [ DataD [] typ [] Nothing [RecGadtC [] [] (ConT typ)] [] ])
