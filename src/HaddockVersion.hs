--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--

module HaddockVersion ( 
	projectName, projectVersion, projectUrl
   ) where

projectName, projectUrl :: String
projectName = "Haddock"
projectUrl = "http://www.haskell.org/haddock/"

-- The version comes in via CPP from mk/version.mk
projectVersion :: String
projectVersion = HADDOCK_VERSION
