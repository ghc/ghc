--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--

module HaddockVersion ( 
	projectName, projectVersion, projectUrl
   ) where

import Version ( version )

projectName, projectUrl :: String
projectName = "Haddock"
projectUrl = "http://www.haskell.org/haddock/"

projectVersion = version
