module GHC.Types.IPE(DCMap, ClosureMap, InfoTableProvMap(..)
                    , emptyInfoTableProvMap) where

import GHC.Prelude

import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Core.DataCon

import GHC.Types.Unique.Map
import GHC.Core.Type
import Data.List.NonEmpty

-- | A map from a 'Name' to the best approximate source position that
-- name arose from.
type ClosureMap = UniqMap Name  -- The binding
                          (Type, Maybe (RealSrcSpan, String))
                          -- The best approximate source position.
                          -- (rendered type, source position, source note
                          -- label)

-- | A map storing all the different uses of a specific data constructor and the
-- approximate source position that usage arose from.
-- The 'Int' is an incrementing identifier which distinguishes each usage
-- of a constructor in a module. It is paired with the source position
-- the constructor was used at, if possible and a string which names
-- the source location. This is the same information as is the payload
-- for the 'GHC.Core.SourceNote' constructor.
type DCMap = UniqMap DataCon (NonEmpty (Int, Maybe (RealSrcSpan, String)))

data InfoTableProvMap = InfoTableProvMap
                          { provDC :: DCMap
                          , provClosure :: ClosureMap }

emptyInfoTableProvMap :: InfoTableProvMap
emptyInfoTableProvMap = InfoTableProvMap emptyUniqMap emptyUniqMap
