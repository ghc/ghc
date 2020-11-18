module GHC.Types.IPE(ClosureMap, InfoTableProvMap(..)
                    , emptyInfoTableProvMap) where

import GHC.Prelude

import GHC.Types.Name
import GHC.Types.SrcLoc

import GHC.Types.Unique.Map

type ClosureMap = UniqMap Name (String, RealSrcSpan, String)

data InfoTableProvMap = InfoTableProvMap
                          { provClosure :: ClosureMap }

emptyInfoTableProvMap :: InfoTableProvMap
emptyInfoTableProvMap = InfoTableProvMap emptyUniqMap