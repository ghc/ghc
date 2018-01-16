-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription.Utils
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Common utils used by modules under Distribution.PackageDescription.*.

module Distribution.PackageDescription.Utils (
  cabalBug, userBug
  ) where

-- ----------------------------------------------------------------------------
-- Exception and logging utils

userBug :: String -> a
userBug msg = error $ msg ++ ". This is a bug in your .cabal file."

cabalBug :: String -> a
cabalBug msg = error $ msg ++ ". This is possibly a bug in Cabal.\n"
               ++ "Please report it to the developers: "
               ++ "https://github.com/haskell/cabal/issues/new"
