module GHC.Types.IPE(DCMap, ClosureMap, InfoTableProvMap(..)
                    , emptyInfoTableProvMap) where

import GHC.Prelude

import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Core.DataCon

import GHC.Types.Unique.Map

type ClosureMap = UniqMap Name (String, RealSrcSpan, String)

type DCMap = UniqMap DataCon [(Int, Maybe (RealSrcSpan, String))]

data InfoTableProvMap = InfoTableProvMap
                          { provDC :: DCMap
                          , provClosure :: ClosureMap }

emptyInfoTableProvMap :: InfoTableProvMap
emptyInfoTableProvMap = InfoTableProvMap emptyUniqMap emptyUniqMap