{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

module GHC.Word (
    Word(..),
    ) where

import GHC.Base

import {-# SOURCE #-} GHC.Unicode ()

data Word = W# Word# deriving Eq

instance Num Word where
    signum 0               = 0
    signum _               = 1

