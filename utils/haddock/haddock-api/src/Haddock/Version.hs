{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.Version
-- Copyright   :  (c) Simon Marlow 2003
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
module Haddock.Version
  ( projectName
  , projectVersion
  , projectUrl
  ) where

import Data.Version (showVersion)

import Paths_haddock_api ( version )

projectName :: String
projectName = "Haddock"

projectUrl :: String
projectUrl = "http://www.haskell.org/haddock/"

projectVersion :: String
projectVersion = showVersion version
