{-# LANGUAGE DerivingVia #-}

module GHC.Types.ImportLevel (
        -- * ImportLevel
        -- ** Data-type
        ImportLevel(..),
        -- ** Enumeration
        allImportLevels,
        -- ** Conversion
        convImportLevel,
        convImportLevelSpec
   ) where

import GHC.Prelude

import GHC.Utils.Binary
import GHC.Utils.Outputable

import Language.Haskell.Syntax.ImpExp

import Data.Data

-- | ImportLevel
data ImportLevel = NormalLevel | SpliceLevel | QuoteLevel
  deriving (Eq, Ord, Data, Show, Enum, Bounded)

instance Outputable ImportLevel where
  ppr NormalLevel = text "normal"
  ppr SpliceLevel = text "splice"
  ppr QuoteLevel = text "quote"

deriving via (EnumBinary ImportLevel) instance Binary ImportLevel

allImportLevels :: [ImportLevel]
allImportLevels = [minBound .. maxBound]

convImportLevel :: ImportDeclLevelStyle -> ImportLevel
convImportLevel (LevelStylePre level) = convImportLevelSpec level
convImportLevel (LevelStylePost level) = convImportLevelSpec level
convImportLevel NotLevelled = NormalLevel

convImportLevelSpec :: ImportDeclLevel -> ImportLevel
convImportLevelSpec ImportDeclQuote = QuoteLevel
convImportLevelSpec ImportDeclSplice = SpliceLevel
