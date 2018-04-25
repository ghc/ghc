{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif
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

#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
#endif

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
    liftIO :: IO a -> m a

#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable MonadIO
#endif

instance MonadIO IO where
    liftIO = id
