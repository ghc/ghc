{-# LANGUAGE TemplateHaskell #-}
module TH_finalizer2M( f, g) where

import Language.Haskell.TH.Syntax

g :: IO ()
g = $(do addModFinalizer (do d <- [d| f x = (2 :: Int) |]; addTopDecls d)
         [| return ()|]
     )
