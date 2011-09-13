-- Test gcdInt/gcdInteger

import GHC.Real ( gcdInt )
import GHC.Integer.GMP.Internals ( gcdInteger )

main :: IO ()
main = do
   test gcdInt     [                       -42, 0, 105             ]
   test gcdInteger [-12193263111263526900, -42, 0, 105, 1234567890 ]


test :: Integral a => (a -> a -> a) -> [a] -> IO ()
test f xs = mapM_ print [ (a, b, f a b) | a <- xs, b <- reverse xs, a /= 0  || b /= 0 ]
