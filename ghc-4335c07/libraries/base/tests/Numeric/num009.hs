-- trac #2059
--
-- Note that this test fails miserably when compiled to use X87 floating point.
-- For instance, in the case of (sin 1e20) the X86 FSIN instruction doesn't even
-- get the sign right on my machine.

module Main(main) where

import Control.Monad
import Foreign.C

main = do let d = [0, pi, pi/2, pi/3, 1e10, 1e20] :: [Double]
              f = [0, pi, pi/2, pi/3, 1e10, 1e20] :: [Float]
          mapM_ (test "sind" sind sin) d
          mapM_ (test "sinf" sinf sin) f
          mapM_ (test "cosd" cosd cos) d
          mapM_ (test "cosf" cosf cos) f
          mapM_ (test "tand" tand tan) d
          mapM_ (test "tanf" tanf tan) f
          -- added to test #13629
          mapM_ (test "sqrtd" sqrtd sqrt) f
          mapM_ (test "sqrtf" sqrtf sqrt) f
          putStrLn "Done"

test :: (RealFloat a, Floating a, RealFloat b, Floating b, Show b)
     => String -> (a -> a) -> (b -> b) -> b -> IO ()
test s f g x = do let y = realToFrac (f (realToFrac x))
                      z = g x
                  unless (y == z) $ do
                      putStrLn ("uh oh! " ++ s ++ ' ':show x)
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

foreign import ccall "math.h sqrt"  sqrtd :: CDouble -> CDouble
foreign import ccall "math.h sqrtf" sqrtf :: CFloat  -> CFloat
