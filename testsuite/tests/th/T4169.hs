{-# LANGUAGE TemplateHaskell #-}

-- Crashed GHC 6.12

module T4165 where

import Language.Haskell.TH
class Numeric a where
    fromIntegerNum :: a
    fromIntegerNum = undefined

ast :: Q [Dec]
ast = [d|
    instance Numeric Int
    |]
