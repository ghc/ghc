{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Type.Reflection

import GHC.Exts.Heap.Closures

assertSize :: forall a. Typeable a => a -> Int -> IO ()
assertSize !x n = do
  let !size = closureSize (asBox x)
  when (size != expected) $ do
    putStrLn $ "closureSize ("++show (typeRep @a)++") == "++show size++", expected "++show expected
{-# NOINLINE assertSize #-}

pap :: Int -> Char -> Int
pap x _ = x
{-# NOINLINE pap #-}

main :: IO ()
main = do
  assertSize 'a' 2
  assertSize (Just ()) 2
  assertSize (1,2) 3
  assertSize (1,2,3) 4
  assertSize id 1
  assertSize fst 1
  assertSize (pap 1) 2

