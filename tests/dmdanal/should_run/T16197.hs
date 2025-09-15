import System.IO.Unsafe (unsafePerformIO)

data T = T !Bool
data Box a = Box a

f :: Int -> T -> Box Bool
f n t
  | n <= 0 = case t of
      T b -> Box b
  | otherwise = f (n-2) t

f1 :: Int -> Bool -> Box Bool
f1 n t
  | n <= 0 = f (-n) $! T t
  | otherwise = f1 (n-2) t

g :: Int -> Bool
g k = if k <= 0
        then unsafePerformIO (putStrLn "Evaluated True" >> return True)
        else unsafePerformIO (putStrLn "Evaluated False" >> return False)
{-# NOINLINE g #-}

main :: IO ()
main = case f1 4 (g 0) of
    Box _ -> return ()
