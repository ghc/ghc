-- exposes a bug in the native code generator in GHC 6.4.1.  Division by
-- a power of 2 was being mis-optimsed to a direct shift.

main = do
  print (map f4 [(-20) .. (-1)])
  print (map f8 [(-20) .. (-1)])

f4 :: Int -> Int
f4 x = x `quot` 4

f8 :: Int -> Int
f8 x = x `quot` 8
