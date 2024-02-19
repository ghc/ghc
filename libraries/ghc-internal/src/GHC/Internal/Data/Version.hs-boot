{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.Data.Version
  ( Version
  , makeVersion
  ) where

import GHC.Internal.Base

data Version

makeVersion :: [Int] -> Version
