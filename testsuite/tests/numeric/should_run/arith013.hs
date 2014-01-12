-- Test gcd

main :: IO ()
main = do
   test gcd ([                       -42, 0, 105             ] :: [Int])
   test gcd ([-12193263111263526900, -42, 0, 105, 1234567890 ] :: [Integer])


test :: (Show a, Integral a) => (a -> a -> a) -> [a] -> IO ()
test f xs = mapM_ print [ (a, b, f a b) | a <- xs, b <- reverse xs, a /= 0  || b /= 0 ]
