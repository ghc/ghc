{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTSyntax #-}

module T17379a where

import Language.Haskell.TH

$(let typ = mkName "T" in pure [ DataD [] typ [] Nothing [GadtC [] [] (ConT typ)] [] ])
