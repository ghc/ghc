{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Types.PkgQual where

import GHC.Prelude
import GHC.Types.SourceText
import GHC.Unit.Types
import GHC.Utils.Outputable

import Data.Data

-- | Package-qualifier as it was parsed
data RawPkgQual
  = NoRawPkgQual             -- ^ No package qualifier
  | RawPkgQual StringLiteral -- ^ Raw package qualifier string.
  deriving (Data)

-- | Package-qualifier after renaming
--
-- Renaming detects if "this" or the unit-id of the home-unit was used as a
-- package qualifier.
data PkgQual
  = NoPkgQual       -- ^ No package qualifier
  | ThisPkg  UnitId -- ^ Import from home-unit
  | OtherPkg UnitId -- ^ Import from another unit
  deriving (Data, Ord, Eq)

instance Outputable RawPkgQual where
  ppr = \case
    NoRawPkgQual -> empty
    RawPkgQual (StringLiteral st p _)
      -> pprWithSourceText st (doubleQuotes (ftext p))

instance Outputable PkgQual where
  ppr = \case
    NoPkgQual  -> empty
    ThisPkg u  -> doubleQuotes (ppr u)
    OtherPkg u -> doubleQuotes (ppr u)


