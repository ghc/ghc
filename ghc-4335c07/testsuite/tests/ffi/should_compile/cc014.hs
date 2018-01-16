module Blah ( foo ) where
import Foreign.Ptr ( FunPtr )
type Bar = Int -> Double -> Double -> Double -> IO ()
foreign import ccall unsafe "dynamic" foo :: FunPtr Bar -> Bar
