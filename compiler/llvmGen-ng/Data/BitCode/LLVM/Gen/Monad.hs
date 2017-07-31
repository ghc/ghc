{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.BitCode.LLVM.Gen.Monad where

import GhcPrelude

import DynFlags
import qualified Stream

import Outputable as Outp

import ErrUtils

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Fix (MonadFix(..))

import UniqSupply (MonadUnique(..), UniqSupply, splitUniqSupply, takeUniqFromSupply)


-- Note: this is basically taken verbatim from the LlvmGen/Base.hs of ghc 8.0.1

-- | Llvm environment
data LlvmEnv = LlvmEnv
  { envDynFlags :: DynFlags   -- ^ Dynamic flags
  , envUniq :: UniqSupply     -- ^ Supply of unique values
  }

-- | The Llvm monad. Wrapping @LlvmEnv@ state as well as the @IO@ monad.
newtype LlvmT m a = LlvmT { runLlvmT :: StateT LlvmEnv m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

instance Monad m => HasDynFlags (LlvmT m) where
    getDynFlags = getEnv envDynFlags

instance Monad m => MonadUnique (LlvmT m) where
    getUniqueSupplyM = do
        us <- getEnv envUniq
        let (us1, us2) = splitUniqSupply us
        modifyEnv (\s -> s { envUniq = us2 })
        return us1

    getUniqueM = do
        us <- getEnv envUniq
        let (u,us') = takeUniqFromSupply us
        modifyEnv (\s -> s { envUniq = us' })
        return u

-- type LlvmM a = LlvmT IO a

-- -- | Get initial Llvm environment.
-- runLlvm :: DynFlags -> LlvmM () -> IO ()
-- runLlvm dflags m = do
--     _ <- flip runStateT env (runLlvmT m)
--     return ()
--   where env = LlvmEnv { envDynFlags = dflags
--                       }

-- | Get environment (internal)
getEnv :: Monad m => (LlvmEnv -> a) -> LlvmT m a
getEnv = LlvmT . gets

-- | Modify environment (internal)
modifyEnv :: Monad m => (LlvmEnv -> LlvmEnv) -> LlvmT m ()
modifyEnv = LlvmT . modify

-- | Lift a stream into the LlvmM monad
liftStream :: (MonadIO m) => Stream.Stream IO a x -> Stream.Stream m a x
liftStream s = Stream.Stream $ do
  r <- liftIO $ Stream.runStream s
  case r of
    Left b        -> return (Left b)
    Right (a, r2) -> return (Right (a, liftStream r2))

-- | Get the platform we are generating code for
getDynFlag :: Monad m => (DynFlags -> a) -> LlvmT m a
getDynFlag f = getEnv (f . envDynFlags)

-- | Dumps the document if the corresponding flag has been set by the user
dumpIfSetLlvm :: MonadIO m => DumpFlag -> String -> Outp.SDoc -> LlvmT m ()
dumpIfSetLlvm flag hdr doc = do
  dflags <- getDynFlags
  liftIO $ dumpIfSet_dyn dflags flag hdr doc
