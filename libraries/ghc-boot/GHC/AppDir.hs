{-# LANGUAGE LambdaCase #-}
module GHC.AppDir where

import Prelude
import GHC.Platform.ArchOS
import Control.Monad.Trans.Maybe
import Control.Monad
import System.Directory
import System.FilePath
import GHC.UniqueSubdir
import Control.Exception

-- | The directory for this version of ghc in the user's app directory
-- The appdir used to be in ~/.ghc but to respect the XDG specification
-- we want to move it under $XDG_DATA_HOME/
-- However, old tooling (like cabal) might still write package environments
-- to the old directory, so we prefer that if a subdirectory of ~/.ghc
-- with the correct target and GHC version suffix exists.
--
-- i.e. if ~/.ghc/$UNIQUE_SUBDIR exists we use that
-- otherwise we use $XDG_DATA_HOME/$UNIQUE_SUBDIR
--
-- UNIQUE_SUBDIR is typically a combination of the target platform and GHC version
versionedAppDir :: String -> ArchOS -> MaybeT IO FilePath
versionedAppDir appname platform = do
  -- Make sure we handle the case the HOME isn't set (see #11678)
  -- We need to fallback to the old scheme if the subdirectory exists.
  msum $ map (checkIfExists <=< fmap (</> versionedFilePath platform))
       [ tryMaybeT $ getAppUserDataDirectory appname  -- this is ~/.ghc/
       , tryMaybeT $ getXdgDirectory XdgData appname -- this is $XDG_DATA_HOME/
       ]
  where
    checkIfExists dir = tryMaybeT (doesDirectoryExist dir) >>= \case
      True -> pure dir
      False -> MaybeT (pure Nothing)

versionedFilePath :: ArchOS -> FilePath
versionedFilePath platform = uniqueSubdir platform

-- | Try performing an 'IO' action, failing on error.
tryMaybeT :: IO a -> MaybeT IO a
tryMaybeT action = MaybeT $ catch (Just `fmap` action) handler
  where
    handler (SomeException _) = return Nothing
