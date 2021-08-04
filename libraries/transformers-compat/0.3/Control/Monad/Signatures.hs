{-# LANGUAGE CPP #-}

#ifndef HASKELL98
# if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
# elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
# endif
# if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
# endif
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Signatures
-- Copyright   :  (c) Ross Paterson 2012
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Signatures for monad operations that require specialized lifting.
-- Each signature has a uniformity property that the lifting should satisfy.
-----------------------------------------------------------------------------

module Control.Monad.Signatures (
    CallCC, Catch, Listen, Pass
  ) where

-- | Signature of the @callCC@ operation,
-- introduced in "Control.Monad.Trans.Cont".
-- Any lifting function @liftCallCC@ should satisfy
--
-- * @'lift' (f k) = f' ('lift' . k) => 'lift' (cf f) = liftCallCC cf f'@
--
type CallCC m a b = ((a -> m b) -> m a) -> m a

-- | Signature of the @catchE@ operation,
-- introduced in "Control.Monad.Trans.Except".
-- Any lifting function @liftCatch@ should satisfy
--
-- * @'lift' (cf m f) = liftCatch ('lift' . cf) ('lift' f)@
--
type Catch e m a = m a -> (e -> m a) -> m a

-- | Signature of the @listen@ operation,
-- introduced in "Control.Monad.Trans.Writer".
-- Any lifting function @liftListen@ should satisfy
--
-- * @'lift' . liftListen = liftListen . 'lift'@
--
type Listen w m a = m a -> m (a, w)

-- | Signature of the @pass@ operation,
-- introduced in "Control.Monad.Trans.Writer".
-- Any lifting function @liftPass@ should satisfy
--
-- * @'lift' . liftPass = liftPass . 'lift'@
--
type Pass w m a =  m (a, w -> w) -> m a
