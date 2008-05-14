-- trac #2059

{-# LANGUAGE ForeignFunctionInterface #-}

module Main(main) where

import Control.Monad

main = do let d = 1e20 :: Double
              f = 1e20 :: Float
          test "sind" sind sin 1e20
          test "sinf" sinf sin 1e20
          test "cosd" cosd cos 1e20
          test "cosf" cosf cos 1e20
          test "tand" tand tan 1e20
          test "tanf" tanf tan 1e20
          putStrLn "Done"

test :: Floating a => String -> (a -> a) -> (a -> a) -> a -> IO ()
test s f g x = do let y = f x
                      z = g x
                  unless (y == z) $ do
                      putStrLn s
                      print y
                      print z

foreign import ccall "math.h sin"  sind :: Double -> Double
foreign import ccall "math.h sinf" sinf :: Float  -> Float

foreign import ccall "math.h cos"  cosd :: Double -> Double
foreign import ccall "math.h cosf" cosf :: Float  -> Float

foreign import ccall "math.h tan"  tand :: Double -> Double
foreign import ccall "math.h tanf" tanf :: Float  -> Float

