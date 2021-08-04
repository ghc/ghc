{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.IORef.Compat (
  module Base
, modifyIORef'
, atomicModifyIORef'
, atomicWriteIORef
) where

import Data.IORef as Base

#if !(MIN_VERSION_base(4,6,0))
import Prelude

-- |Strict version of 'modifyIORef'
--
-- /Since: 4.6.0.0/
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'

-- | Strict version of 'atomicModifyIORef'.  This forces both the value stored
-- in the 'IORef' as well as the value returned.
--
-- /Since: 4.6.0.0/
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref $ \a ->
            case f a of
                v@(a',_) -> a' `seq` v
    b `seq` return b

-- | Variant of 'writeIORef' with the \"barrier to reordering\" property that
-- 'atomicModifyIORef' has.
--
-- /Since: 4.6.0.0/
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef ref a = do
    x <- atomicModifyIORef ref (\_ -> (a, ()))
    x `seq` return ()
#endif
