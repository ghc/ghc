{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Monad (
        MonadPlus(mzero, mplus),
        join, guard, when, unless, ap,
        msum,
        filterM, mapAndUnzipM, zipWithM, zipWithM_, foldM,
        liftM, liftM2, liftM3, liftM4, liftM5,

        -- ...and what the Prelude exports
        Monad((>>=), (>>), return, fail),
        Functor(fmap),
        mapM, mapM_, sequence, sequence_, (=<<),
    ) where

import Control.Monad
