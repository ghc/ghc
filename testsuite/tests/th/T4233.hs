{-# LANGUAGE TemplateHaskell #-}
module T4233 where
import Language.Haskell.TH

w :: Q Type
w = varT (mkName "w")

f :: Q Type
f = [t| $w -> $w |]
