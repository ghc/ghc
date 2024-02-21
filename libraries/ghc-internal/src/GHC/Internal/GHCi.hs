{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.GHCi
-- Copyright   :  (c) The University of Glasgow 2012
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The GHCi Monad lifting interface.
--
-- EXPERIMENTAL! DON'T USE.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-----------------------------------------------------------------------------

module GHC.Internal.GHCi (
        GHCiSandboxIO(..), NoIO()
    ) where

import GHC.Internal.Base (IO(), Monad, Functor(fmap), Applicative(..), (>>=), id, (.), ap)

-- | A monad that can execute GHCi statements by lifting them out of
-- m into the IO monad. (e.g state monads)
class (Monad m) => GHCiSandboxIO m where
    ghciStepIO :: m a -> IO a

-- | @since base-4.4.0.0
instance GHCiSandboxIO IO where
    ghciStepIO = id

-- | A monad that doesn't allow any IO.
newtype NoIO a = NoIO { noio :: IO a }

-- | @since base-4.8.0.0
instance Functor NoIO where
  fmap f (NoIO a) = NoIO (fmap f a)

-- | @since base-4.8.0.0
instance Applicative NoIO where
  pure a = NoIO (pure a)
  (<*>) = ap

-- | @since base-4.4.0.0
instance Monad NoIO where
    (>>=) k f = NoIO (noio k >>= noio . f)

-- | @since base-4.4.0.0
instance GHCiSandboxIO NoIO where
    ghciStepIO = noio

