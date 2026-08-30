{-# LANGUAGE TemplateHaskell #-}
module T27034 where

import Language.Haskell.TH

f $(tupP [wildP]) = ()
g $(tupP [varP (mkName "x")]) = x
