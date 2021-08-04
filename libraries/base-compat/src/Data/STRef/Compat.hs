{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.STRef.Compat (
  module Base
, modifySTRef'
) where

import Data.STRef as Base

#if !(MIN_VERSION_base(4,6,0))
import Control.Monad.ST (ST)
import Prelude (seq)

-- | Strict version of 'modifySTRef'
--
-- /Since: 4.6.0.0/
modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' ref f = do
    x <- readSTRef ref
    let x' = f x
    x' `seq` writeSTRef ref x'
#endif
