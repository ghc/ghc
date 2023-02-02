{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.GHCi
-- Copyright   :  (c) The University of Glasgow 2012
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The GHCi Monad lifting interface.
--
-- EXPERIMENTAL! DON'T USE.
--
-----------------------------------------------------------------------------

module GHC.GHCi {-# WARNING "This is an unstable interface." #-} (
        GHCiSandboxIO(..), NoIO()
    ) where

import GHC.Base (IO(), Monad, Functor(fmap), Applicative(..), (>>=), id, (.), ap)

-- | A monad that can execute GHCi statements by lifting them out of
-- m into the IO monad. (e.g state monads)
class (Monad m) => GHCiSandboxIO m where
    ghciStepIO :: m a -> IO a

-- | @since 4.4.0.0
instance GHCiSandboxIO IO where
    ghciStepIO = id

-- | A monad that doesn't allow any IO.
newtype NoIO a = NoIO { noio :: IO a }

-- | @since 4.8.0.0
instance Functor NoIO where
  fmap f (NoIO a) = NoIO (fmap f a)

-- | @since 4.8.0.0
instance Applicative NoIO where
  pure a = NoIO (pure a)
  (<*>) = ap

-- | @since 4.4.0.0
instance Monad NoIO where
    (>>=) k f = NoIO (noio k >>= noio . f)

-- | @since 4.4.0.0
instance GHCiSandboxIO NoIO where
    ghciStepIO = noio

