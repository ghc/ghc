-- trac #2059

{-# LANGUAGE ForeignFunctionInterface #-}

module Main(main) where

import Control.Monad
import Foreign.C

main = do let d = 1e20 :: Double
              f = 1e20 :: Float
          test "sind" sind sin d
          test "sinf" sinf sin f
          test "cosd" cosd cos d
          test "cosf" cosf cos f
          test "tand" tand tan d
          test "tanf" tanf tan f
          putStrLn "Done"

test :: (RealFloat a, Floating a, RealFloat b, Floating b, Show b)
     => String -> (a -> a) -> (b -> b) -> b -> IO ()
test s f g x = do let y = realToFrac (f (realToFrac x))
                      z = g x
                  unless (y == z) $ do
                      putStrLn s
                      print y
                      print z
                      print $ decodeFloat y
                      print $ decodeFloat z

foreign import ccall "math.h sin"  sind :: CDouble -> CDouble
foreign import ccall "math.h sinf" sinf :: CFloat  -> CFloat

foreign import ccall "math.h cos"  cosd :: CDouble -> CDouble
foreign import ccall "math.h cosf" cosf :: CFloat  -> CFloat

foreign import ccall "math.h tan"  tand :: CDouble -> CDouble
foreign import ccall "math.h tanf" tanf :: CFloat  -> CFloat

