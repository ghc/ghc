{-# LANGUAGE TemplateHaskell, PartialTypeSignatures  #-}
module ExtraConstraintsWildcardInTypeSplice2 where

import Language.Haskell.TH.Lib (wildCardT)

show' :: $(wildCardT) => a -> String
show' x = show x
