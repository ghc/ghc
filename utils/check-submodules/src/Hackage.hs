{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hackage
    ( PackageState(..)
    , getVersions
    ) where

import qualified Data.Map.Strict as M
import Lens.Micro
import Network.Wreq
import Distribution.Types.PackageName
import qualified Data.Aeson as JSON
import Data.Version

data PackageState = Normal | Deprecated
    deriving (Show)

instance JSON.FromJSON PackageState where
    parseJSON = JSON.withText "package state" $ \case
        "normal" -> pure Normal
        "deprecated" -> pure Deprecated
        _ -> fail "unknown PackageState"

getVersions :: PackageName -> IO (M.Map Version PackageState)
getVersions pn = do
    r <- asJSON =<< getWith opts url
    maybe (fail "getVersions: failed") pure (r ^? responseBody)
  where
    opts = defaults & header "Accept" .~ ["application/json"]
    url = "https://hackage.haskell.org/package/" <> unPackageName pn

