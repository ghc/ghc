{-# LANGUAGE PolyKinds #-}

module Test where

import GHC.Exts

data Test (a :: TYPE r) = Test !a
