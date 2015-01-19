{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Zip
-- Copyright   :  (c) Nils Schweinsberg 2011,
--                (c) George Giorgidze 2011
--                (c) University Tuebingen 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Monadic zipping (used for monad comprehensions)
--
-----------------------------------------------------------------------------

module Control.Monad.Zip where

import Control.Monad (liftM)

-- | `MonadZip` type class. Minimal definition: `mzip` or `mzipWith`
--
-- Instances should satisfy the laws:
--
-- * Naturality :
--
--   > liftM (f *** g) (mzip ma mb) = mzip (liftM f ma) (liftM g mb)
--
-- * Information Preservation:
--
--   > liftM (const ()) ma = liftM (const ()) mb
--   > ==>
--   > munzip (mzip ma mb) = (ma, mb)
--
class Monad m => MonadZip m where
    {-# MINIMAL mzip | mzipWith #-}

    mzip :: m a -> m b -> m (a,b)
    mzip = mzipWith (,)

    mzipWith :: (a -> b -> c) -> m a -> m b -> m c
    mzipWith f ma mb = liftM (uncurry f) (mzip ma mb)

    munzip :: m (a,b) -> (m a, m b)
    munzip mab = (liftM fst mab, liftM snd mab)
    -- munzip is a member of the class because sometimes
    -- you can implement it more efficiently than the
    -- above default code.  See Trac #4370 comment by giorgidze

instance MonadZip [] where
    mzip     = zip
    mzipWith = zipWith
    munzip   = unzip

