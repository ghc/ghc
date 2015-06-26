module Haddock.Backends.Hyperlinker.Utils
    ( srcModUrl
    , srcNameUrlMap
    ) where

import Haddock.Utils
import Haddock.Backends.Xhtml.Types

import GHC

import Data.Maybe
import Data.Map (Map)

srcModUrl :: SourceURLs -> String
srcModUrl (_, mModUrl, _, _) = fromMaybe defaultModuleSourceUrl mModUrl

srcNameUrlMap :: SourceURLs -> Map PackageKey FilePath
srcNameUrlMap (_, _, nameUrlMap, _) = nameUrlMap
