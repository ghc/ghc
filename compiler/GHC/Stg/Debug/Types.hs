module GHC.Stg.Debug.Types where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Types.SrcLoc
import GHC.Utils.Binary (Binary)
import qualified GHC.Utils.Binary as B

import Data.Set (Set)
import qualified Data.Set as Set

data SpanWithLabel = SpanWithLabel RealSrcSpan LexicalFastString

data StgDebugOpts = StgDebugOpts
  { stgDebug_infoTableMap              :: !Bool
  , stgDebug_distinctConstructorTables :: !StgDebugDctConfig
  }

data StgDebugDctConfig =
    StgDebugDctConfig
      { dctConfig_perModule :: !Bool
      , dctConfig_whichConstructors :: !StgDebugDctConfigConstrs
      }

-- | Necessary for 'StgDebugDctConfig' to be included in the dynflags
-- fingerprint
instance Binary StgDebugDctConfig where
  put_ bh (StgDebugDctConfig pm cs) = do
    B.put_ bh pm
    B.put_ bh cs

  get bh = StgDebugDctConfig <$> B.get bh <*> B.get bh

-- | Configuration describing which constructors should be given distinct info
-- tables for each usage.
data StgDebugDctConfigConstrs =
    -- | Create distinct constructor tables for each usage of any data
    -- constructor.
    --
    -- This is the behavior if just @-fdistinct-constructor-tables@ is supplied.
    All

    -- | Create distinct constructor tables for each usage of only these data
    -- constructors.
    --
    -- This is the behavior if @-fdistinct-constructor-tables=C1,...,CN@ is
    -- supplied.
  | Only !(Set String)

    -- | Create distinct constructor tables for each usage of any data
    -- constructor except these ones.
    --
    -- This is the behavior if @-fdistinct-constructor-tables@ and
    -- @-fno-distinct-constructor-tables=C1,...,CN@ is given.
  | AllExcept !(Set String)

    -- | Do not create distinct constructor tables for any data constructor.
    --
    -- This is the behavior if no @-fdistinct-constructor-tables@ is given (or
    -- @-fno-distinct-constructor-tables@ is given).
  | None

-- | Necessary for 'StgDebugDctConfig' to be included in the dynflags
-- fingerprint
instance Binary StgDebugDctConfigConstrs where
  put_ bh All = B.putByte bh 0
  put_ bh (Only cs) = do
    B.putByte bh 1
    B.put_ bh cs
  put_ bh (AllExcept cs) = do
    B.putByte bh 2
    B.put_ bh cs
  put_ bh None = B.putByte bh 3

  get bh = do
    h <- B.getByte bh
    case h of
      0 -> pure All
      1 -> Only <$> B.get bh
      2 -> AllExcept <$> B.get bh
      _ -> pure None

-- | Given a distinct constructor tables configuration and a set of constructor
-- names that we want to generate distinct info tables for, create a new
-- configuration which includes those constructors.
--
-- If the given set is empty, that means the user has entered
-- @-fdistinct-constructor-tables@ with no constructor names specified, and
-- therefore we consider that an 'All' configuration.
dctConfigConstrsPlus :: StgDebugDctConfig -> Set String -> StgDebugDctConfig
dctConfigConstrsPlus cfg cs
    | Set.null cs = cfg { dctConfig_whichConstructors = All }
    | otherwise =
        case dctConfig_whichConstructors cfg of
          All -> cfg { dctConfig_whichConstructors = All }
          Only cs' -> cfg { dctConfig_whichConstructors = Only $ Set.union cs' cs }
          AllExcept cs' -> cfg { dctConfig_whichConstructors = AllExcept $ Set.difference cs' cs }
          None -> cfg { dctConfig_whichConstructors = Only cs }

-- | Given a distinct constructor tables configuration and a set of constructor
-- names that we /do not/ want to generate distinct info tables for, create a
-- new configuration which excludes those constructors.
--
-- If the given set is empty, that means the user has entered
-- @-fno-distinct-constructor-tables@ with no constructor names specified, and
-- therefore we consider that a 'None' configuration.
dctConfigConstrsMinus :: StgDebugDctConfig -> Set String -> StgDebugDctConfig
dctConfigConstrsMinus cfg cs
    | Set.null cs = cfg { dctConfig_whichConstructors = None }
    | otherwise =
        case dctConfig_whichConstructors cfg of
          All -> cfg { dctConfig_whichConstructors = AllExcept cs }
          Only cs' -> cfg { dctConfig_whichConstructors = Only $ Set.difference cs' cs }
          AllExcept cs' -> cfg { dctConfig_whichConstructors = AllExcept $ Set.union cs' cs }
          None -> cfg { dctConfig_whichConstructors = None }

