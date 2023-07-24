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
instance Binary StgDebugDctConfig where
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
dctConfigPlus :: StgDebugDctConfig -> Set String -> StgDebugDctConfig
dctConfigPlus cfg cs
    | Set.null cs = All
    | otherwise =
        case cfg of
          All -> All
          Only cs' -> Only $ Set.union cs' cs
          AllExcept cs' -> AllExcept $ Set.difference cs' cs
          None -> Only cs

-- | Given a distinct constructor tables configuration and a set of constructor
-- names that we /do not/ want to generate distinct info tables for, create a
-- new configuration which excludes those constructors.
--
-- If the given set is empty, that means the user has entered
-- @-fno-distinct-constructor-tables@ with no constructor names specified, and
-- therefore we consider that a 'None' configuration.
dctConfigMinus :: StgDebugDctConfig -> Set String -> StgDebugDctConfig
dctConfigMinus cfg cs
    | Set.null cs = None
    | otherwise =
        case cfg of
          All -> AllExcept cs
          Only cs' -> Only $ Set.difference cs' cs
          AllExcept cs' -> AllExcept $ Set.union cs' cs
          None -> None

