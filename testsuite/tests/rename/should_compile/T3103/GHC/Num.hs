{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Num (fromInteger) where

import GHC.Base
import GHC.Num.Integer

fromInteger :: Num a => Integer -> a
fromInteger = fromInteger
