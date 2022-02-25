{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Zip
-- Copyright   :  (c) Nils Schweinsberg 2011,
--                (c) George Giorgidze 2011
--                (c) University Tuebingen 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Monadic zipping (used for monad comprehensions)
--
-----------------------------------------------------------------------------

module Control.Monad.Zip where

import Control.Monad (liftM, liftM2)
import Data.Functor.Identity
import Data.Monoid
import Data.Ord ( Down(..) )
import Data.Proxy
import qualified Data.List.NonEmpty as NE
import GHC.Generics
import GHC.Tuple (Solo (..))

-- | Instances should satisfy the laws:
--
-- [Naturality]
--
--     @'liftM' (f 'Control.Arrow.***' g) ('mzip' ma mb)
--         = 'mzip' ('liftM' f ma) ('liftM' g mb)@
--
-- [Information Preservation]
--
--     @'liftM' ('Prelude.const' ()) ma = 'liftM' ('Prelude.const' ()) mb@
--         implies
--     @'munzip' ('mzip' ma mb) = (ma, mb)@
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
    -- above default code.  See #4370 comment by giorgidze

-- | @since 4.3.1.0
instance MonadZip [] where
    mzip     = zip
    mzipWith = zipWith
    munzip   = unzip

-- | @since 4.9.0.0
instance MonadZip NE.NonEmpty where
  mzip     = NE.zip
  mzipWith = NE.zipWith
  munzip   = NE.unzip

-- | @since 4.8.0.0
instance MonadZip Identity where
    mzipWith                 = liftM2
    munzip (Identity (a, b)) = (Identity a, Identity b)

-- | @since 4.15.0.0
instance MonadZip Solo where
    mzipWith = liftM2
    munzip (Solo (a, b)) = (Solo a, Solo b)

-- | @since 4.8.0.0
instance MonadZip Dual where
    -- Cannot use coerce, it's unsafe
    mzipWith = liftM2

-- | @since 4.8.0.0
instance MonadZip Sum where
    mzipWith = liftM2

-- | @since 4.8.0.0
instance MonadZip Product where
    mzipWith = liftM2

-- | @since 4.8.0.0
instance MonadZip Maybe where
    mzipWith = liftM2

-- | @since 4.8.0.0
instance MonadZip First where
    mzipWith = liftM2

-- | @since 4.8.0.0
instance MonadZip Last where
    mzipWith = liftM2

-- | @since 4.8.0.0
instance MonadZip f => MonadZip (Alt f) where
    mzipWith f (Alt ma) (Alt mb) = Alt (mzipWith f ma mb)

-- | @since 4.9.0.0
instance MonadZip Proxy where
    mzipWith _ _ _ = Proxy

-- Instances for GHC.Generics
-- | @since 4.9.0.0
instance MonadZip U1 where
    mzipWith _ _ _ = U1

-- | @since 4.9.0.0
instance MonadZip Par1 where
    mzipWith = liftM2

-- | @since 4.9.0.0
instance MonadZip f => MonadZip (Rec1 f) where
    mzipWith f (Rec1 fa) (Rec1 fb) = Rec1 (mzipWith f fa fb)

-- | @since 4.9.0.0
instance MonadZip f => MonadZip (M1 i c f) where
    mzipWith f (M1 fa) (M1 fb) = M1 (mzipWith f fa fb)

-- | @since 4.9.0.0
instance (MonadZip f, MonadZip g) => MonadZip (f :*: g) where
    mzipWith f (x1 :*: y1) (x2 :*: y2) = mzipWith f x1 x2 :*: mzipWith f y1 y2

-- instances for Data.Ord

-- | @since 4.12.0.0
instance MonadZip Down where
    mzipWith = liftM2
