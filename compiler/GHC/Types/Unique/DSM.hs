{-# LANGUAGE UnboxedTuples, PatternSynonyms, DerivingVia #-}
module GHC.Types.Unique.DSM
  (
  -- * Threading a deterministic supply
    DUniqSupply
  , UniqDSM(UDSM)
  , DUniqResult
  , pattern DUniqResult

  -- ** UniqDSM and DUniqSupply operations
  , getUniqueDSM
  , runUniqueDSM
  , takeUniqueFromDSupply
  , initDUniqSupply

  -- ** Tag operations
  , newTagDUniqSupply
  , getTagDUniqSupply

  -- * A transfomer threading a deterministic supply
  , UniqDSMT(UDSMT)

  -- ** UniqDSMT operations
  , runUDSMT
  , withDUS
  , hoistUDSMT
  , liftUDSMT

  -- ** Tags
  , setTagUDSMT

  -- * Monad class for deterministic supply threading
  , MonadGetUnique(..)
  , MonadUniqDSM(..)

  )
  where

import GHC.Exts (oneShot)
import GHC.Prelude
import GHC.Word
import Control.Monad.Fix
import GHC.Types.Unique
import qualified GHC.Utils.Monad.State.Strict as Strict
import qualified GHC.Types.Unique.Supply as USM
import Control.Monad.IO.Class

{-
Note [Deterministic Uniques in the CG]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC produces fully deterministic object code. To achieve this, there is a key
pass (detRenameCmmGroup) which renames all non-deterministic uniques in
the Cmm code right after StgToCmm. See Note [Object determinism] for the big
picture and some details.

The code generation pipeline that processes this renamed, deterministic, Cmm,
however, may still need to generate new uniques. If we were to resort to the
non-deterministic unique supply used in the rest of the compiler, our renaming
efforts would be for naught.

Therefore, after having renamed Cmm deterministically, we must ensure that all
uniques created by the code generation pipeline use a deterministic source of uniques.
Most often, this means don't use `UniqSM` in the Cmm passes, use `UniqDSM`:

`UniqDSM` is a pure state monad with an incrementing counter from which we
source new uniques. Unlike `UniqSM`, there's no way to `split` the supply, but
it turns out this was rarely really needed for code generation and migrating
from UniqSM to UniqDSM was easy.

Secondly, the `DUniqSupply` used to run a `UniqDSM` must be threaded through
all passes to guarantee uniques in different passes are unique amongst them
altogether.
Specifically, the same `DUniqSupply` must be threaded through the CG Streaming
pipeline, starting with Driver.Main calling `StgToCmm.codeGen`, `cmmPipeline`,
`cmmToRawCmm`, and `codeOutput` in sequence.

To thread resources through the `Stream` abstraction, we use the `UniqDSMT`
transformer on top of `IO` as the Monad underlying the Stream. `UniqDSMT` will
thread the `DUniqSupply` through every pass applied to the `Stream`, for every
element. We use @type CgStream = Stream (UniqDSMT IO)@ for the Stream used in
code generation which that carries through the deterministic unique supply.

Unlike non-deterministic unique supplies which can be split into supplies using
different tags, or where a new supply with a new tag can be brought from the
void, a `DUniqSupply` needs to be sampled iteratively. To use a different tag
during a specific pass (to more easily identify uniques created in it), the tag
should be manually set and then reset on the unique supply. There's also the
auxiliary `setTagUDSMT` which sets the tag for all uniques supplied in the given
action, and resets it implicitly.

See also Note [Object determinism] in GHC.StgToCmm
-}

-- See Note [Deterministic Uniques in the CG]
newtype DUniqSupply = DUS Word64 -- supply uniques iteratively
type DUniqResult result = (# result, DUniqSupply #)

pattern DUniqResult :: a -> DUniqSupply -> (# a, DUniqSupply #)
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

-- | Initialize a deterministic unique supply with the given Tag and initial unique.
initDUniqSupply :: Char -> Word64 -> DUniqSupply
initDUniqSupply c firstUniq =
  let !tag = mkTag c
  in DUS (tag .|. firstUniq)

runUniqueDSM :: DUniqSupply -> UniqDSM a -> (a, DUniqSupply)
runUniqueDSM ds (UDSM f) =
  case f ds of
    DUniqResult uq us -> (uq, us)

-- | Set the tag of uniques generated from this deterministic unique supply
newTagDUniqSupply :: Char -> DUniqSupply -> DUniqSupply
newTagDUniqSupply c (DUS w) = DUS $ getKey $ newTagUnique (mkUniqueGrimily w) c

-- | Get the tag uniques generated from this deterministic unique supply would have
getTagDUniqSupply :: DUniqSupply -> Char
getTagDUniqSupply (DUS w) = fst $ unpkUnique (mkUniqueGrimily w)

-- | Get a unique from a monad that can access a unique supply.
--
-- Crucially, because 'MonadGetUnique' doesn't allow you to get the
-- 'UniqSupply' (unlike 'MonadUnique'), an instance such as 'UniqDSM' can use a
-- deterministic unique supply to return deterministic uniques without allowing
-- for the 'UniqSupply' to be shared.
class Monad m => MonadGetUnique m where
  getUniqueM :: m Unique

instance MonadGetUnique UniqDSM where
  getUniqueM = getUniqueDSM

-- non deterministic instance
instance MonadGetUnique USM.UniqSM where
  getUniqueM = USM.getUniqueM

--------------------------------------------------------------------------------
-- UniqDSMT
--------------------------------------------------------------------------------

-- | Transformer version of 'UniqDSM' to use when threading a deterministic
-- uniq supply over a Monad. Specifically, it is used in the `Stream` of Cmm
-- decls.
newtype UniqDSMT m result = UDSMT' (DUniqSupply -> m (result, DUniqSupply))
  deriving (Functor)

-- Similar to GHC.Utils.Monad.State.Strict, using Note [The one-shot state monad trick]
-- Using the one-shot trick is necessary for performance.
-- Using transfomer's strict `StateT` regressed some performance tests in 1-2%.
-- The one-shot trick here fixes those regressions.

pattern UDSMT :: (DUniqSupply -> m (result, DUniqSupply)) -> UniqDSMT m result
pattern UDSMT m <- UDSMT' m
  where
    UDSMT m = UDSMT' (oneShot $ \s -> m s)
{-# COMPLETE UDSMT #-}

instance Monad m => Applicative (UniqDSMT m) where
  pure x = UDSMT $ \s -> pure (x, s)
  UDSMT f <*> UDSMT x = UDSMT $ \s0 -> do
    (f', s1) <- f s0
    (x', s2) <- x s1
    pure (f' x', s2)

instance Monad m => Monad (UniqDSMT m) where
  UDSMT x >>= f = UDSMT $ \s0 -> do
    (x', s1) <- x s0
    case f x' of UDSMT y -> y s1

instance MonadIO m => MonadIO (UniqDSMT m) where
  liftIO x = UDSMT $ \s -> (,s) <$> liftIO x

instance Monad m => MonadGetUnique (UniqDSMT m) where
  getUniqueM = UDSMT $ \us -> do
    let (u, us') = takeUniqueFromDSupply us
    return (u, us')

-- | Set the tag of the running @UniqDSMT@ supply to the given tag and run an action with it.
-- All uniques produced in the given action will use this tag, until the tag is changed
-- again.
setTagUDSMT :: Monad m => Char {-^ Tag -} -> UniqDSMT m a -> UniqDSMT m a
setTagUDSMT tag (UDSMT act) = UDSMT $ \us -> do
  let origtag = getTagDUniqSupply us
      new_us  = newTagDUniqSupply tag us
  (a, us') <- act new_us
  let us'_origtag = newTagDUniqSupply origtag us'
      -- restore original tag
  return (a, us'_origtag)

-- | Like 'runUniqueDSM' but for 'UniqDSMT'
runUDSMT :: DUniqSupply -> UniqDSMT m a -> m (a, DUniqSupply)
runUDSMT dus (UDSMT st) = st dus

-- | Lift an IO action that depends on, and threads through, a unique supply
-- into UniqDSMT IO.
withDUS :: (DUniqSupply -> IO (a, DUniqSupply)) -> UniqDSMT IO a
withDUS f = UDSMT $ \us -> do
  (a, us') <- liftIO (f us)
  return (a, us')

-- | Change the monad underyling an applied @UniqDSMT@, i.e. transform a
-- @UniqDSMT m@ into a @UniqDSMT n@ given @m ~> n@.
hoistUDSMT :: (forall x. m x -> n x) -> UniqDSMT m a -> UniqDSMT n a
hoistUDSMT nt (UDSMT m) = UDSMT $ \s -> nt (m s)

-- | Lift a monadic action @m a@ into an @UniqDSMT m a@
liftUDSMT :: Functor m => m a -> UniqDSMT m a
liftUDSMT m = UDSMT $ \s -> (,s) <$> m

--------------------------------------------------------------------------------
-- MonadUniqDSM
--------------------------------------------------------------------------------

class (Monad m) => MonadUniqDSM m where
  -- | Lift a pure 'UniqDSM' action into a 'MonadUniqDSM' such as 'UniqDSMT'
  liftUniqDSM :: UniqDSM a -> m a

instance MonadUniqDSM UniqDSM where
  liftUniqDSM = id

instance Monad m => MonadUniqDSM (UniqDSMT m) where
  liftUniqDSM act = UDSMT $ \us -> pure $ runUniqueDSM us act
