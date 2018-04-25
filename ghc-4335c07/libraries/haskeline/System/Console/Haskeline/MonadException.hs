{- | This module redefines some of the functions in "Control.Exception" to
work for more general monads built on top of 'IO'.
-}

module System.Console.Haskeline.MonadException(
    -- * The MonadException class
    MonadException(..),
    -- * Generalizations of Control.Exception
    catch,
    handle,
    catches,
    Handler(..),
    finally,
    throwIO,
    throwTo,
    bracket,
    -- * Helpers for defining \"wrapper\" functions
    liftIOOp,
    liftIOOp_,
    -- * Internal implementation
    RunIO(..),
    -- * Extensible Exceptions
    Exception,
    SomeException(..),
    E.IOException(),
    )
     where

import qualified Control.Exception as E
import Control.Exception (Exception,SomeException)
#if __GLASGOW_HASKELL__ < 705
import Prelude hiding (catch)
#endif
import Control.Monad(liftM, join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Writer
import Data.Monoid
import Control.Concurrent(ThreadId)

-- This approach is based on that of the monad-control package.
-- Since we want to use haskeline to bootstrap GHC, we reimplement
-- a simplified version here.  
-- Additionally, we avoid TypeFamilies (which are used in the latest version of
-- monad-control) so that we're still compatible with older versions of GHC.

-- | A 'RunIO' function takes a monadic action @m@ as input,
-- and outputs an IO action which performs the underlying impure part of @m@
-- and returns the ''pure'' part of @m@.
--
-- Note that @(RunIO return)@ is an incorrect implementation, since it does not
-- separate the pure and impure parts of the monadic action.  This module defines
-- implementations for several common monad transformers.
newtype RunIO m = RunIO (forall b . m b -> IO (m b))
-- Uses a newtype so we don't need RankNTypes.

-- | An instance of 'MonadException' is generally made up of monad transformers
-- layered on top of the IO monad.  
-- 
-- The 'controlIO' method enables us to \"lift\" a function that manages IO actions (such
-- as 'bracket' or 'catch') into a function that wraps arbitrary monadic actions.
class MonadIO m => MonadException m where
    controlIO :: (RunIO m -> IO (m a)) -> m a

-- | Lift a IO operation
-- 
-- > wrap :: (a -> IO b) -> IO b
-- 
-- to a more general monadic operation
-- 
-- > liftIOOp wrap :: MonadException m => (a -> m b) -> m b
--
-- For example: 
--
-- @
--  'liftIOOp' ('System.IO.withFile' f m) :: MonadException m => (Handle -> m r) -> m r
--  'liftIOOp' 'Foreign.Marshal.Alloc.alloca' :: (MonadException m, Storable a) => (Ptr a -> m b) -> m b
--  'liftIOOp' (`Foreign.ForeignPtr.withForeignPtr` fp) :: MonadException m => (Ptr a -> m b) -> m b
-- @
liftIOOp :: MonadException m => ((a -> IO (m b)) -> IO (m c)) -> (a -> m b) -> m c
liftIOOp f g = controlIO $ \(RunIO run) -> f (run . g)

-- | Lift an IO operation
-- 
-- > wrap :: IO a -> IO a
-- 
-- to a more general monadic operation
-- 
-- > liftIOOp_ wrap :: MonadException m => m a -> m a
liftIOOp_ :: MonadException m => (IO (m a) -> IO (m a)) -> m a -> m a
liftIOOp_ f act = controlIO $ \(RunIO run) -> f (run act)


catch :: (MonadException m, E.Exception e) => m a -> (e -> m a) -> m a
catch act handler = controlIO $ \(RunIO run) -> E.catch
                                                    (run act)
                                                    (run . handler)

handle :: (MonadException m, Exception e) => (e -> m a) -> m a -> m a
handle = flip catch
 
catches :: (MonadException m) => m a -> [Handler m a] -> m a
catches act handlers = controlIO $ \(RunIO run) ->
                           let catchesHandler e = foldr tryHandler (E.throw e) handlers
                                   where tryHandler (Handler handler) res =
                                             case E.fromException e of
                                               Just e' -> run $ handler e'
                                               Nothing -> res
                           in E.catch (run act) catchesHandler

data Handler m a = forall e . Exception e => Handler (e -> m a)


bracket :: MonadException m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket before after thing 
    = controlIO $ \(RunIO run) -> E.bracket
                                    (run before)
                                    (\m -> run (m >>= after))
                                    (\m -> run (m >>= thing))

finally :: MonadException m => m a -> m b -> m a
finally thing ender = controlIO $ \(RunIO run) -> E.finally (run thing) (run ender)

throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . E.throwIO

throwTo :: (MonadIO m, Exception e) => ThreadId -> e -> m ()
throwTo tid = liftIO . E.throwTo tid

----------
-- Instances of MonadException.
-- Since implementations of this class are non-obvious to a casual user,
-- we provide instances for nearly everything in the transformers package.

instance MonadException IO where
    controlIO f = join $ f (RunIO (liftM return))
    -- Note: it's crucial that we use "liftM return" instead of "return" here.
    -- For example, in "finally thing end", this ensures that "end" will always run, 
    -- regardless of whether an mzero occurred inside of "thing".

instance MonadException m => MonadException (ReaderT r m) where
    controlIO f = ReaderT $ \r -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (ReaderT . const) . run . flip runReaderT r)
                    in fmap (flip runReaderT r) $ f run'

instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'

instance MonadException m => MonadException (MaybeT m) where
    controlIO f = MaybeT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap MaybeT . run . runMaybeT)
                    in fmap runMaybeT $ f run' 

instance (MonadException m, Error e) => MonadException (ErrorT e m) where
    controlIO f = ErrorT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ErrorT . run . runErrorT)
                    in fmap runErrorT $ f run'

instance MonadException m => MonadException (ListT m) where
    controlIO f = ListT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ListT . run . runListT)
                    in fmap runListT $ f run'

instance (Monoid w, MonadException m) => MonadException (WriterT w m) where
    controlIO f = WriterT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap WriterT . run . runWriterT)
                    in fmap runWriterT $ f run'

instance (Monoid w, MonadException m) => MonadException (RWST r w s m) where
    controlIO f = RWST $ \r s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (\act -> RWST (\_ _ -> act))
                                    . run . (\m -> runRWST m r s))
                    in fmap (\m -> runRWST m r s) $ f run'

deriving instance MonadException m => MonadException (IdentityT m)
