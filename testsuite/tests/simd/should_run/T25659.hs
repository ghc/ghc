{-# LANGUAGE MagicHash, UnboxedTuples, ExtendedLiterals #-}
import GHC.Int
import GHC.Prim

test :: (Int64X2# -> Int64X2# -> Int64X2#) -> IO ()
test f = do
  let a = packInt64X2# (# 0#Int64, 11#Int64 #)
      b = packInt64X2# (# 22#Int64, 33#Int64 #)
      c = f a b
      (# x0, x1 #) = unpackInt64X2# a
      (# y0, y1 #) = unpackInt64X2# b
      (# z0, z1 #) = unpackInt64X2# c
  putStrLn $ "a = " ++ show (I64# x0, I64# x1)
  putStrLn $ "b = " ++ show (I64# y0, I64# y1)
  putStrLn $ "c = " ++ show (I64# z0, I64# z1)
{-# NOINLINE test #-}

main :: IO ()
main = test (\_ b -> b)
