{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module T16326_Fail2 where

foreign import ccall "blah" blah :: forall a -> a -> IO ()
