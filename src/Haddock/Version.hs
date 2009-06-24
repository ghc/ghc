-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Version
-- Copyright   :  (c) Simon Marlow 2003
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Haddock.Version ( 
  projectName, projectVersion, projectUrl
) where

import Paths_haddock ( version )
import Data.Version  ( showVersion )

projectName, projectUrl :: String
projectName = "Haddock"
projectUrl  = "http://www.haskell.org/haddock/"

projectVersion :: String
projectVersion = showVersion version
