{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.IO.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
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
    -- Let us take for example a program that makes use of monad stacks.
    -- While the point of this example is not to teach monad stacks,
    -- know that they are useful to combine different capabilities
    -- provided by different monads: IO operations such as output functions
    -- for the 'IO' Monad, and state manipulation for the 'State' Monad.
    --
    -- The following program shows basic state manipulation and IO output sewn together
    -- by the use of a monad stack:
    --
    -- @
    -- {-\# LANGUAGE ConstraintKinds   \#-}
    -- {-\# LANGUAGE FlexibleContexts  \#-}
    -- {-\# LANGUAGE OverloadedStrings \#-}
    --
    -- module LiftExample
    --     ( someFunc
    --     ) where
    --
    -- import           Control.Monad.IO.Class
    -- import           Control.Monad.State
    -- import           Data.Text              (Text)
    -- import qualified Data.Text.IO           as T
    --
    -- data AppState = AppState {msg :: Text}
    --
    -- type StateIO m = (MonadState AppState m, MonadIO m)
    --
    -- initialState = AppState "Hello"
    --
    -- runExample :: MonadIO m => m AppState
    -- runExample = evalStateT interpreter initialState
    --
    -- interpreter :: StateIO m => m AppState
    -- interpreter = do
    --   outputMessage
    --   put $ AppState{msg = "World"}
    --   outputMessage
    --   get >>= return
    --
    -- outputMessage :: (StateIO m) => m ()
    -- outputMessage = do
    --   message <- gets msg
    --   liftIO $ T.putStrLn message
    -- @
    --
    -- Although certainly more complex than a one-liner, the main focus of this example
    -- is its last line. Had we ommitted @'liftIO'@, we would have ended up with this error:
    --
    -- @
    --     • Couldn't match type ‘m’ with ‘IO’
    --      ‘m’ is a rigid type variable bound by
    --        the type signature for:
    --          outputMessage :: forall (m :: * -> *). StateIO m => m ()
    --      Expected type: m ()
    --        Actual type: IO ()
    -- @
    --
    -- The important part here is the mismatch between @m ()@ and @'IO' ()@.
    -- Luckily, we know of a function that takes an @'IO' a@ and returns a @m a@: @'liftIO'@.
    -- Which enables us to run the program and see the expect results:
    --
    -- @
    -- λ❯ someFunc
    -- Hello
    -- World
    -- @
    --
    liftIO :: IO a -> m a

-- | @since 4.9.0.0
instance MonadIO IO where
    liftIO = id

