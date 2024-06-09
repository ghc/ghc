{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Types.PkgQual where

import GHC.Prelude
import GHC.Types.SourceText
import GHC.Unit.Types
import GHC.Utils.Outputable

import Language.Haskell.Syntax.Extension

import Data.Data

-- | Package-qualifier as it was parsed
data RawPkgQual pass
  = NoRawPkgQual                -- ^ No package qualifier
  | RawPkgQual (StringLit pass) -- ^ Raw package qualifier string.

deriving instance
  (Data pass, XStringLit pass ~ (SourceText, Maybe NoCommentsLocation))
  => Data (RawPkgQual pass)

-- | Package-qualifier after renaming
--
-- Renaming detects if "this" or the unit-id of the home-unit was used as a
-- package qualifier.
data PkgQual
  = NoPkgQual       -- ^ No package qualifier
  | ThisPkg  !UnitId -- ^ Import from home-unit
  | OtherPkg !UnitId -- ^ Import from another unit
  deriving (Data, Ord, Eq)

instance (XStringLit pass ~ (SourceText, Maybe NoCommentsLocation))
  => Outputable (RawPkgQual pass) where
  ppr = \case
    NoRawPkgQual -> empty
    RawPkgQual (SL st p)
      -> pprWithSourceText (fst st) (doubleQuotes (ftext p))

instance Outputable PkgQual where
  ppr = \case
    NoPkgQual  -> empty
    ThisPkg u  -> doubleQuotes (ppr u)
    OtherPkg u -> doubleQuotes (ppr u)


