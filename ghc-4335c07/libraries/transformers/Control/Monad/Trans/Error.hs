{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
#if !(MIN_VERSION_base(4,9,0))
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Error
-- Copyright   :  (c) Michael Weber <michael.weber@post.rwth-aachen.de> 2001,
--                (c) Jeff Newbern 2003-2006,
--                (c) Andriy Palamarchuk 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- This monad transformer adds the ability to fail or throw exceptions
-- to a monad.
--
-- A sequence of actions succeeds, producing a value, only if all the
-- actions in the sequence are successful.  If one fails with an error,
-- the rest of the sequence is skipped and the composite action fails
-- with that error.
--
-- If the value of the error is not required, the variant in
-- "Control.Monad.Trans.Maybe" may be used instead.
--
-- /Note:/ This module will be removed in a future release.
-- Instead, use "Control.Monad.Trans.Except", which does not restrict
-- the exception type, and also includes a base exception monad.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Error
  {-# DEPRECATED "Use Control.Monad.Trans.Except instead" #-} (
    -- * The ErrorT monad transformer
    Error(..),
    ErrorList(..),
    ErrorT(..),
    mapErrorT,
    -- * Error operations
    throwError,
    catchError,
    -- * Lifting other operations
    liftCallCC,
    liftListen,
    liftPass,
    -- * Examples
    -- $examples
  ) where

import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Data.Functor.Classes

import Control.Applicative
import Control.Exception (IOException)
import Control.Monad
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import Control.Monad.Fix
#if !(MIN_VERSION_base(4,6,0))
import Control.Monad.Instances ()  -- deprecated from base-4.6
#endif
import Data.Foldable (Foldable(foldMap))
import Data.Monoid (mempty)
import Data.Traversable (Traversable(traverse))
import System.IO.Error

#if !(MIN_VERSION_base(4,9,0))
-- These instances are in base-4.9.0

instance MonadPlus IO where
    mzero       = ioError (userError "mzero")
    m `mplus` n = m `catchIOError` \ _ -> n

instance Alternative IO where
    empty = mzero
    (<|>) = mplus

# if !(MIN_VERSION_base(4,4,0))
-- exported by System.IO.Error from base-4.4
catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = catch
# endif
#endif

instance (Error e) => Alternative (Either e) where
    empty        = Left noMsg
    Left _ <|> n = n
    m      <|> _ = m

instance (Error e) => MonadPlus (Either e) where
    mzero            = Left noMsg
    Left _ `mplus` n = n
    m      `mplus` _ = m

#if !(MIN_VERSION_base(4,3,0))
-- These instances are in base-4.3

instance Applicative (Either e) where
    pure          = Right
    Left  e <*> _ = Left e
    Right f <*> r = fmap f r

instance Monad (Either e) where
    return        = Right
    Left  l >>= _ = Left l
    Right r >>= k = k r

instance MonadFix (Either e) where
    mfix f = let
        a = f $ case a of
            Right r -> r
            _       -> error "empty mfix argument"
        in a

#endif /* base to 4.2.0.x */

-- | An exception to be thrown.
--
-- Minimal complete definition: 'noMsg' or 'strMsg'.
class Error a where
    -- | Creates an exception without a message.
    -- The default implementation is @'strMsg' \"\"@.
    noMsg  :: a
    -- | Creates an exception with a message.
    -- The default implementation of @'strMsg' s@ is 'noMsg'.
    strMsg :: String -> a

    noMsg    = strMsg ""
    strMsg _ = noMsg

instance Error IOException where
    strMsg = userError

-- | A string can be thrown as an error.
instance (ErrorList a) => Error [a] where
    strMsg = listMsg

-- | Workaround so that we can have a Haskell 98 instance @'Error' 'String'@.
class ErrorList a where
    listMsg :: String -> [a]

instance ErrorList Char where
    listMsg = id

-- | The error monad transformer. It can be used to add error handling
-- to other monads.
--
-- The @ErrorT@ Monad structure is parameterized over two things:
--
-- * e - The error type.
--
-- * m - The inner monad.
--
-- The 'return' function yields a successful computation, while @>>=@
-- sequences two subcomputations, failing on the first error.
newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

instance (Eq e, Eq1 m) => Eq1 (ErrorT e m) where
    liftEq eq (ErrorT x) (ErrorT y) = liftEq (liftEq eq) x y

instance (Ord e, Ord1 m) => Ord1 (ErrorT e m) where
    liftCompare comp (ErrorT x) (ErrorT y) = liftCompare (liftCompare comp) x y

instance (Read e, Read1 m) => Read1 (ErrorT e m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "ErrorT" ErrorT
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Show e, Show1 m) => Show1 (ErrorT e m) where
    liftShowsPrec sp sl d (ErrorT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "ErrorT" d m
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Eq e, Eq1 m, Eq a) => Eq (ErrorT e m a) where (==) = eq1
instance (Ord e, Ord1 m, Ord a) => Ord (ErrorT e m a) where compare = compare1
instance (Read e, Read1 m, Read a) => Read (ErrorT e m a) where
    readsPrec = readsPrec1
instance (Show e, Show1 m, Show a) => Show (ErrorT e m a) where
    showsPrec = showsPrec1

-- | Map the unwrapped computation using the given function.
--
-- * @'runErrorT' ('mapErrorT' f m) = f ('runErrorT' m)@
mapErrorT :: (m (Either e a) -> n (Either e' b))
          -> ErrorT e m a
          -> ErrorT e' n b
mapErrorT f m = ErrorT $ f (runErrorT m)

instance (Functor m) => Functor (ErrorT e m) where
    fmap f = ErrorT . fmap (fmap f) . runErrorT

instance (Foldable f) => Foldable (ErrorT e f) where
    foldMap f (ErrorT a) = foldMap (either (const mempty) f) a

instance (Traversable f) => Traversable (ErrorT e f) where
    traverse f (ErrorT a) =
        ErrorT <$> traverse (either (pure . Left) (fmap Right . f)) a

instance (Functor m, Monad m) => Applicative (ErrorT e m) where
    pure a  = ErrorT $ return (Right a)
    f <*> v = ErrorT $ do
        mf <- runErrorT f
        case mf of
            Left  e -> return (Left e)
            Right k -> do
                mv <- runErrorT v
                case mv of
                    Left  e -> return (Left e)
                    Right x -> return (Right (k x))

instance (Functor m, Monad m, Error e) => Alternative (ErrorT e m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m, Error e) => Monad (ErrorT e m) where
#if !(MIN_VERSION_base(4,8,0))
    return a = ErrorT $ return (Right a)
#endif
    m >>= k  = ErrorT $ do
        a <- runErrorT m
        case a of
            Left  l -> return (Left l)
            Right r -> runErrorT (k r)
    fail msg = ErrorT $ return (Left (strMsg msg))

#if MIN_VERSION_base(4,9,0)
instance (Monad m, Error e) => Fail.MonadFail (ErrorT e m) where
    fail msg = ErrorT $ return (Left (strMsg msg))
#endif

instance (Monad m, Error e) => MonadPlus (ErrorT e m) where
    mzero       = ErrorT $ return (Left noMsg)
    m `mplus` n = ErrorT $ do
        a <- runErrorT m
        case a of
            Left  _ -> runErrorT n
            Right r -> return (Right r)

instance (MonadFix m, Error e) => MonadFix (ErrorT e m) where
    mfix f = ErrorT $ mfix $ \ a -> runErrorT $ f $ case a of
        Right r -> r
        _       -> error "empty mfix argument"

instance MonadTrans (ErrorT e) where
    lift m = ErrorT $ do
        a <- m
        return (Right a)

instance (Error e, MonadIO m) => MonadIO (ErrorT e m) where
    liftIO = lift . liftIO

-- | Signal an error value @e@.
--
-- * @'runErrorT' ('throwError' e) = 'return' ('Left' e)@
--
-- * @'throwError' e >>= m = 'throwError' e@
throwError :: (Monad m) => e -> ErrorT e m a
throwError l = ErrorT $ return (Left l)

-- | Handle an error.
--
-- * @'catchError' h ('lift' m) = 'lift' m@
--
-- * @'catchError' h ('throwError' e) = h e@
catchError :: (Monad m) =>
    ErrorT e m a                -- ^ the inner computation
    -> (e -> ErrorT e m a)      -- ^ a handler for errors in the inner
                                -- computation
    -> ErrorT e m a
m `catchError` h = ErrorT $ do
    a <- runErrorT m
    case a of
        Left  l -> runErrorT (h l)
        Right r -> return (Right r)

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: CallCC m (Either e a) (Either e b) -> CallCC (ErrorT e m) a b
liftCallCC callCC f = ErrorT $
    callCC $ \ c ->
    runErrorT (f (\ a -> ErrorT $ c (Right a)))

-- | Lift a @listen@ operation to the new monad.
liftListen :: (Monad m) => Listen w m (Either e a) -> Listen w (ErrorT e m) a
liftListen listen = mapErrorT $ \ m -> do
    (a, w) <- listen m
    return $! fmap (\ r -> (r, w)) a

-- | Lift a @pass@ operation to the new monad.
liftPass :: (Monad m) => Pass w m (Either e a) -> Pass w (ErrorT e m) a
liftPass pass = mapErrorT $ \ m -> pass $ do
    a <- m
    return $! case a of
        Left  l      -> (Left  l, id)
        Right (r, f) -> (Right r, f)

{- $examples

Wrapping an IO action that can throw an error @e@:

> type ErrorWithIO e a = ErrorT e IO a
> ==> ErrorT (IO (Either e a))

An IO monad wrapped in @StateT@ inside of @ErrorT@:

> type ErrorAndStateWithIO e s a = ErrorT e (StateT s IO) a
> ==> ErrorT (StateT s IO (Either e a))
> ==> ErrorT (StateT (s -> IO (Either e a,s)))

-}
