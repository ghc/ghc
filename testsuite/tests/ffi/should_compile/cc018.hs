module ShouldCompile where

import Control.Applicative (Applicative)
import Foreign.Ptr

-- simple functions

foreign import winapi unsafe "a" a :: IO Int

foreign import winapi unsafe "b" b :: Int -> IO Int

foreign import winapi unsafe "c"
  c :: Int -> Char -> Float -> Double -> IO Float

-- Complex and wrapper functions

newtype RenderM a = RenderM (IO a) deriving (Functor, Applicative, Monad)

type RenderCallback = Int -> Int -> RenderM ()

foreign import winapi duma_onRender :: FunPtr RenderCallback -> RenderM ()

foreign import winapi "wrapper" mkRenderCallback
    :: RenderCallback -> RenderM (FunPtr RenderCallback)

onRender :: RenderCallback -> RenderM ()
onRender f = mkRenderCallback f >>= duma_onRender
