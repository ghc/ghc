{-# LANGUAGE UnboxedTuples, PatternSynonyms, DerivingVia #-}
module GHC.Types.Unique.DSM where

import GHC.Exts (oneShot)
import GHC.Prelude
import GHC.Word
import Control.Monad.Fix
import GHC.Types.Unique
import qualified GHC.Utils.Monad.State.Strict as Strict
import qualified GHC.Types.Unique.Supply as USM

-- todo: Do I need to use the one-shot state monad trick? Probably yes.

-- check: UniqSM is only used before Cmm (grep for it), afterwards only UniqDSM is used.

-- todo: use UniqSM for UniqRenamable? We've basically re-implemented this logic
-- there, but without the unboxing it feels? Maybe not, since we carry the
-- mappings too.

newtype DUniqSupply = DUS Word64 -- supply uniques iteratively
type DUniqResult result = (# result, DUniqSupply #)

pattern DUniqResult :: a -> b -> (# a, b #)
pattern DUniqResult x y = (# x, y #)
{-# COMPLETE DUniqResult #-}

-- | A monad which just gives the ability to obtain 'Unique's deterministically.
-- There's no splitting.
newtype UniqDSM result = UDSM' { unUDSM :: DUniqSupply -> DUniqResult result }
  deriving (Functor, Applicative, Monad) via (Strict.State DUniqSupply)

instance MonadFix UniqDSM where
  mfix m = UDSM (\us0 -> let (r,us1) = runUniqueDSM us0 (m r) in DUniqResult r us1)

-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
pattern UDSM :: (DUniqSupply -> DUniqResult a) -> UniqDSM a
pattern UDSM m <- UDSM' m
  where
    UDSM m = UDSM' (oneShot $ \s -> m s)
{-# COMPLETE UDSM #-}

getUniqueDSM :: UniqDSM Unique
getUniqueDSM = UDSM (\(DUS us0) -> DUniqResult (mkUniqueGrimily us0) (DUS $ us0+1))

takeUniqueFromDSupply :: DUniqSupply -> (Unique, DUniqSupply)
takeUniqueFromDSupply d =
  case unUDSM getUniqueDSM d of
    DUniqResult x y -> (x, y)

-- Write Note about the importance of locality in uniques that are deterministic
--
-- If you use a tag which collides with other names, you'll get a uniques
-- deterministically colliding with existing symbols.
--
-- (e.g. easy to observe if you do this wrong)
--
-- Ideally, we'd thread the same deterministic unique supply all the way
-- throughout the Cmm pipeline, starting off from hte deterministic rename
-- pass.
initDUniqSupply :: Char -> Word64 -> DUniqSupply
initDUniqSupply c firstUniq =
  let !tag = mkTag c
  in DUS (tag .|. firstUniq)

newTagDUniqSupply :: Char -> DUniqSupply -> DUniqSupply
newTagDUniqSupply c (DUS w) = DUS $ getKey $ newTagUnique (mkUniqueGrimily w) c

runUniqueDSM :: DUniqSupply -> UniqDSM a -> (a, DUniqSupply)
runUniqueDSM ds (UDSM f) =
  case f ds of
    DUniqResult uq us -> (uq, us)

-- Add explanation on how this gives you a deterministic way of getting uniques
-- if the instance uses a deterministic unique supply.
class Monad m => MonadGetUnique m where
  getUniqueM :: m Unique

instance MonadGetUnique UniqDSM where
  getUniqueM = getUniqueDSM

instance MonadGetUnique USM.UniqSM where
  getUniqueM = USM.getUniqueM

{-
Note [Cmm Local Deterministic Uniques]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TODO!!!!!
TODO!!!!!
-}
