module GHC.Stg.Debug.Types where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Types.SrcLoc

import Data.Set (Set)
import qualified Data.Set as Set

data SpanWithLabel = SpanWithLabel RealSrcSpan LexicalFastString

data StgDebugOpts = StgDebugOpts
  { stgDebug_infoTableMap              :: !Bool
  , stgDebug_distinctConstructorTables :: !StgDebugDctConfig
  }

-- | Configuration describing which constructors should be given distinct info
-- tables for each usage.
data StgDebugDctConfig =
    -- | Create distinct constructor tables for each usage of any data
    -- constructor.
    --
    -- This is the behavior if just @-fdistinct-constructor-tables@ is supplied.
    All

    -- | Create distinct constructor tables for each usage of only these data
    -- constructors.
    --
    -- This is the behavior if @-fdistinct-constructor-tables-only=C1,...,CN@ is
    -- supplied.
  | Only !(Set String)

    -- | Do not create distinct constructor tables for any data constructor.
    --
    -- This is the behavior if @-fno-distinct-constructor-tables@ is given.
  | None

-- | Given a distinct constructor tables configuration and a set of constructor
-- names that we want to generate distinct info tables for, create a new
-- configuration which includes those constructors.
--
-- If the given set is empty, that means the user has entered
-- @-fdistinct-constructor-tables@ with no constructor names specified, and
-- therefore we consider that an 'All' configuration.
dctConfigOnly :: StgDebugDctConfig -> Set String -> StgDebugDctConfig
dctConfigOnly cfg cs
    | Set.null cs = All
    | otherwise =
        case cfg of
          All -> Only cs
          Only cs' -> Only $ Set.union cs' cs
          None -> Only cs
