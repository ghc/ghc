{-# LANGUAGE TemplateHaskell #-}

module T10946 where

import Language.Haskell.TH

m :: a -> a
m x = $$([||_||])
