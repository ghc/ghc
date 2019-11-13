{-# language MagicHash #-}

module CStringLengthCore
  ( ozymandias
  ) where

import GHC.Exts

ozymandias :: Int
ozymandias =
  I# (cstringLength# "I met a traveller from an antique land"#)
