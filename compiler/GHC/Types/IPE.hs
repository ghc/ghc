module GHC.Types.IPE(ClosureMap, InfoTableProvMap(..)
                    , emptyInfoTableProvMap) where

import GHC.Prelude

import GHC.Types.Name
import GHC.Types.SrcLoc

import GHC.Types.Unique.Map

-- | A map from a 'Name' to the best approximate source position that
-- name arose from.
type ClosureMap = UniqMap
                                        Name  -- The binding
                                        (String, RealSrcSpan, String) -- The best approximate source position.

data InfoTableProvMap = InfoTableProvMap
                          { provClosure :: ClosureMap }

emptyInfoTableProvMap :: InfoTableProvMap
emptyInfoTableProvMap = InfoTableProvMap emptyUniqMap
