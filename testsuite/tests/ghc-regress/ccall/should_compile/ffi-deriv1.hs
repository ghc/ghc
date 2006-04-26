{-# OPTIONS -fglasgow-exts #-}

-- Tests newtype unwrapping for the IO monad itself
-- Notice the RenderM monad, which is used in the
-- type of the callback function

module ShouldCompile where

import Foreign.Ptr
newtype RenderM a = RenderM (IO a) deriving (Functor, Monad)

type RenderCallback = Int -> Int -> RenderM ()

foreign import ccall duma_onRender :: FunPtr RenderCallback -> RenderM ()

foreign import ccall "wrapper" mkRenderCallback
     :: RenderCallback -> RenderM (FunPtr RenderCallback)

onRender :: RenderCallback -> RenderM ()
onRender f = mkRenderCallback f >>= duma_onRender



