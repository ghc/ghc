{-# OPTIONS_GHC -O1 #-}

{-
  On windows some xmm registers are callee saved. This means
  they can't be used as scratch registers before a call to C.

  In #14619 this wasn't respected which lead to a wrong value
  ending up in xmm6 and being returned in the final result.

  This code compiles to a non trivial fp computation followed
  by a call to sqrt at O1+. If xmm6 isn't properly handled it
  will be used as a scratch register failing the test.

  The original code used regular sqrt which on 8.2 generated
  a C call in the backend. To imitate this behaviour on 8.4+
  we force a call to a C function instead.
-}

module Main (main) where



import Prelude hiding((*>), (<*))
import Foreign.C
import Unsafe.Coerce

foreign import ccall unsafe "sqrt" call_sqrt :: CDouble -> CDouble

type V3 = (Double, Double, Double)

absf :: V3 -> V3 -> Double
absf (x, y, z) (x', y', z') = x*x' +y*y'+z*z'


{-# NOINLINE sphereIntersection #-}
sphereIntersection :: V3 -> V3 -> (V3)
sphereIntersection orig dir@(_, _, dirz)
  | b < 0  = undefined
  | t1   > 0  = dir
  | t1   < 0  = orig
  | otherwise = undefined
    where b  = orig `absf` dir
          sqrtDisc = realToFrac . call_sqrt $ CDouble b
          t1 = b - sqrtDisc

main = print $ sphereIntersection (11, 22, 33) (44, 55, 66)
