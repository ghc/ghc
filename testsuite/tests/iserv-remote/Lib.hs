{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Language.Haskell.TH

x :: Int -> ExpQ
x n = [| 3 + n |]
