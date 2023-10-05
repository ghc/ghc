module Main where
import Foreign.C.Types
foreign import ccall func :: IO CInt
main :: IO ()
main = print =<< func
