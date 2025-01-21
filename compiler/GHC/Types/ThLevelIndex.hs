module GHC.Types.ThLevelIndex where

import GHC.Prelude
import GHC.Utils.Outputable
import GHC.Types.Basic ( ImportLevel(..) )

-- | The integer which represents the level
newtype ThLevelIndex = ThLevelIndex Int deriving (Eq, Ord)
    -- NB: see Note [Template Haskell levels] in GHC.Tc.Gen.Splice
    -- Incremented when going inside a bracket,
    -- decremented when going inside a splice

instance Outputable ThLevelIndex where
    ppr (ThLevelIndex i) = int i

incThLevelIndex :: ThLevelIndex -> ThLevelIndex
incThLevelIndex (ThLevelIndex i) = ThLevelIndex (i + 1)

decThLevelIndex :: ThLevelIndex -> ThLevelIndex
decThLevelIndex (ThLevelIndex i) = ThLevelIndex (i - 1)

topLevelIndex :: ThLevelIndex
topLevelIndex = ThLevelIndex 0

spliceLevelIndex :: ThLevelIndex
spliceLevelIndex = decThLevelIndex topLevelIndex

quoteLevelIndex :: ThLevelIndex
quoteLevelIndex = incThLevelIndex topLevelIndex

-- | Convert a 'GHC.Types.Basic.ImportLevel' to a 'ThLevelIndex'
thLevelIndexFromImportLevel :: ImportLevel -> ThLevelIndex
thLevelIndexFromImportLevel NormalLevel = topLevelIndex
thLevelIndexFromImportLevel SpliceLevel = spliceLevelIndex
thLevelIndexFromImportLevel QuoteLevel  = quoteLevelIndex