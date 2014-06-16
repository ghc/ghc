{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}  

module T5371 where
import Language.Haskell.TH

f :: a -> Name
f (x :: a) = ''a
