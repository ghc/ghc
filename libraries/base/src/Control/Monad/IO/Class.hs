{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.IO.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  stable
-- Portability :  portable
--
-- Class of monads based on @IO@.
-----------------------------------------------------------------------------

module Control.Monad.IO.Class (
    MonadIO(..)
  ) where

-- | Monads in which 'IO' computations may be embedded.
-- Any monad built by applying a sequence of monad transformers to the
-- 'IO' monad will be an instance of this class.
--
-- Instances should satisfy the following laws, which state that 'liftIO'
-- is a transformer of monads:
--
-- * @'liftIO' . 'return' = 'return'@
--
-- * @'liftIO' (m >>= f) = 'liftIO' m >>= ('liftIO' . f)@

class (Monad m) => MonadIO m where
    -- | Lift a computation from the 'IO' monad.
    -- This allows us to run IO computations in any monadic stack, so long as it supports these kinds of operations
    -- (i.e. 'IO' is the base monad for the stack).
    --
    -- === __Example__
    --
    --
    -- > import Control.Monad.Trans.State -- from the "transformers" library
    -- >
    -- > printState :: Show s => StateT s IO ()
    -- > printState = do
    -- >   state <- get
    -- >   liftIO $ print state
    --
    --
    -- Had we omitted @'liftIO'@, we would have ended up with this error:
    --
    -- > • Couldn't match type ‘IO’ with ‘StateT s IO’
    -- >  Expected type: StateT s IO ()
    -- >    Actual type: IO ()
    --
    -- The important part here is the mismatch between @StateT s IO ()@ and @'IO' ()@.
    --
    -- Luckily, we know of a function that takes an @'IO' a@ and returns an @(m a)@: @'liftIO'@,
    -- enabling us to run the program and see the expected results:
    --
    -- @
    -- > evalStateT printState "hello"
    -- "hello"
    --
    -- > evalStateT printState 3
    -- 3
    -- @
    --
    liftIO :: IO a -> m a

-- | @since 4.9.0.0
instance MonadIO IO where
    liftIO = id

