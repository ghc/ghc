import ClosureSizeUtils

pap :: Int -> Char -> Int
pap x _ = x
{-# NOINLINE pap #-}

main :: IO ()
main = do
  assertSize (id :: Int -> Int) 1
  assertSize (fst :: (Int,Int) -> Int) 1
  assertSize (pap 1) 2

