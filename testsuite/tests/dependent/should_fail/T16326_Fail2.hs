{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module VisibleDependentQuantificationFail2 where

foreign import ccall "blah" blah :: forall a -> a -> IO ()
