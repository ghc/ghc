{-# LANGUAGE NoImplicitPrelude #-}

module Data.Version
  ( Version
  , makeVersion
  ) where

import GHC.Base

data Version

makeVersion :: [Int] -> Version
