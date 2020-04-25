{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
--
-- (c) The University of Glasgow 2002-2006
--

-- | The IO Monad with an environment
--
-- The environment is passed around as a Reader monad but
-- as its in the IO monad, mutable references can be used
-- for updating state.
--
module GHC.Data.IOEnv (
        IOEnv, -- Instance of Monad

        -- Monad utilities
        module GHC.Utils.Monad,

        -- Errors
        failM, failWithM,
        IOEnvFailure(..),

        -- Getting at the environment
        getEnv, setEnv, updEnv,

        runIOEnv, unsafeInterleaveM, uninterruptibleMaskM_,
        tryM, tryAllM, tryMostM, fixM,

        -- I/O operations
        IORef, newMutVar, readMutVar, writeMutVar, updMutVar,
        atomicUpdMutVar, atomicUpdMutVar'
  ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Utils.Exception
import GHC.Unit.Module
import GHC.Utils.Panic

import Data.IORef       ( IORef, newIORef, readIORef, writeIORef, modifyIORef,
                          atomicModifyIORef, atomicModifyIORef' )
import System.IO.Unsafe ( unsafeInterleaveIO )
import System.IO        ( fixIO )
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import GHC.Utils.Monad
import Control.Applicative (Alternative(..))

----------------------------------------------------------------------
-- Defining the monad type
----------------------------------------------------------------------


newtype IOEnv env a = IOEnv (env -> IO a)
  deriving (Functor)
  deriving (MonadThrow, MonadCatch, MonadMask, MonadIO) via (ReaderT env IO)

unIOEnv :: IOEnv env a -> (env -> IO a)
unIOEnv (IOEnv m) = m

instance Monad (IOEnv m) where
    (>>=)  = thenM
    (>>)   = (*>)

instance MonadFail (IOEnv m) where
    fail _ = failM -- Ignore the string

instance Applicative (IOEnv m) where
    pure = returnM
    IOEnv f <*> IOEnv x = IOEnv (\ env -> f env <*> x env )
    (*>) = thenM_

returnM :: a -> IOEnv env a
returnM a = IOEnv (\ _ -> return a)

thenM :: IOEnv env a -> (a -> IOEnv env b) -> IOEnv env b
thenM (IOEnv m) f = IOEnv (\ env -> do { r <- m env ;
                                         unIOEnv (f r) env })

thenM_ :: IOEnv env a -> IOEnv env b -> IOEnv env b
thenM_ (IOEnv m) f = IOEnv (\ env -> do { _ <- m env ; unIOEnv f env })

failM :: IOEnv env a
failM = IOEnv (\ _ -> throwIO IOEnvFailure)

failWithM :: String -> IOEnv env a
failWithM s = IOEnv (\ _ -> ioError (userError s))

data IOEnvFailure = IOEnvFailure

instance Show IOEnvFailure where
    show IOEnvFailure = "IOEnv failure"

instance Exception IOEnvFailure

instance ContainsDynFlags env => HasDynFlags (IOEnv env) where
    getDynFlags = do env <- getEnv
                     return $! extractDynFlags env

instance ContainsModule env => HasModule (IOEnv env) where
    getModule = do env <- getEnv
                   return $ extractModule env

----------------------------------------------------------------------
-- Fundamental combinators specific to the monad
----------------------------------------------------------------------


---------------------------
runIOEnv :: env -> IOEnv env a -> IO a
runIOEnv env (IOEnv m) = m env


---------------------------
{-# NOINLINE fixM #-}
  -- Aargh!  Not inlining fixM alleviates a space leak problem.
  -- Normally fixM is used with a lazy tuple match: if the optimiser is
  -- shown the definition of fixM, it occasionally transforms the code
  -- in such a way that the code generator doesn't spot the selector
  -- thunks.  Sigh.

fixM :: (a -> IOEnv env a) -> IOEnv env a
fixM f = IOEnv (\ env -> fixIO (\ r -> unIOEnv (f r) env))


---------------------------
tryM :: IOEnv env r -> IOEnv env (Either IOEnvFailure r)
-- Reflect UserError exceptions (only) into IOEnv monad
-- Other exceptions are not caught; they are simply propagated as exns
--
-- The idea is that errors in the program being compiled will give rise
-- to UserErrors.  But, say, pattern-match failures in GHC itself should
-- not be caught here, else they'll be reported as errors in the program
-- begin compiled!
tryM (IOEnv thing) = IOEnv (\ env -> tryIOEnvFailure (thing env))

tryIOEnvFailure :: IO a -> IO (Either IOEnvFailure a)
tryIOEnvFailure = try

-- XXX We shouldn't be catching everything, e.g. timeouts
tryAllM :: IOEnv env r -> IOEnv env (Either SomeException r)
-- Catch *all* exceptions
-- This is used when running a Template-Haskell splice, when
-- even a pattern-match failure is a programmer error
tryAllM (IOEnv thing) = IOEnv (\ env -> try (thing env))

tryMostM :: IOEnv env r -> IOEnv env (Either SomeException r)
tryMostM (IOEnv thing) = IOEnv (\ env -> tryMost (thing env))

---------------------------
unsafeInterleaveM :: IOEnv env a -> IOEnv env a
unsafeInterleaveM (IOEnv m) = IOEnv (\ env -> unsafeInterleaveIO (m env))

uninterruptibleMaskM_ :: IOEnv env a -> IOEnv env a
uninterruptibleMaskM_ (IOEnv m) = IOEnv (\ env -> uninterruptibleMask_ (m env))

----------------------------------------------------------------------
-- Alternative/MonadPlus
----------------------------------------------------------------------

instance Alternative (IOEnv env) where
    empty   = IOEnv (const empty)
    m <|> n = IOEnv (\env -> unIOEnv m env <|> unIOEnv n env)

instance MonadPlus (IOEnv env)

----------------------------------------------------------------------
-- Accessing input/output
----------------------------------------------------------------------

newMutVar :: a -> IOEnv env (IORef a)
newMutVar val = liftIO (newIORef val)

writeMutVar :: IORef a -> a -> IOEnv env ()
writeMutVar var val = liftIO (writeIORef var val)

readMutVar :: IORef a -> IOEnv env a
readMutVar var = liftIO (readIORef var)

updMutVar :: IORef a -> (a -> a) -> IOEnv env ()
updMutVar var upd = liftIO (modifyIORef var upd)

-- | Atomically update the reference.  Does not force the evaluation of the
-- new variable contents.  For strict update, use 'atomicUpdMutVar''.
atomicUpdMutVar :: IORef a -> (a -> (a, b)) -> IOEnv env b
atomicUpdMutVar var upd = liftIO (atomicModifyIORef var upd)

-- | Strict variant of 'atomicUpdMutVar'.
atomicUpdMutVar' :: IORef a -> (a -> (a, b)) -> IOEnv env b
atomicUpdMutVar' var upd = liftIO (atomicModifyIORef' var upd)

----------------------------------------------------------------------
-- Accessing the environment
----------------------------------------------------------------------

getEnv :: IOEnv env env
{-# INLINE getEnv #-}
getEnv = IOEnv (\ env -> return env)

-- | Perform a computation with a different environment
setEnv :: env' -> IOEnv env' a -> IOEnv env a
{-# INLINE setEnv #-}
setEnv new_env (IOEnv m) = IOEnv (\ _ -> m new_env)

-- | Perform a computation with an altered environment
updEnv :: (env -> env') -> IOEnv env' a -> IOEnv env a
{-# INLINE updEnv #-}
updEnv upd (IOEnv m) = IOEnv (\ env -> m (upd env))
