{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Make sure FFI must be IO type
module Main where

import Foreign.C

foreign import ccall "SafeLang08_A" c_sin :: CDouble -> CDouble

sinx :: Double -> Double
sinx d = realToFrac $ c_sin $ realToFrac d

x :: Double
x = 0.8932

main :: IO ()
main = do
    putStrLn "Hello World"
    putStrLn $ "Sin of " ++ (show x) ++ " is "  ++ (show $ sinx x)

