module T8037 where

import Unsafe.Coerce
import Foreign.C.Types
import System.IO.Unsafe

data D4 = D4 CInt CInt CInt
data Color3 = Color3 CInt CInt

crash :: D4 -> IO ()
crash x = color (unsafeCoerce x)

color :: Color3 -> IO ()
color (Color3 r g) = f (unsafePerformIO undefined) r g

foreign import ccall f :: CInt -> CInt -> CInt -> IO ()

