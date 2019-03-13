{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Type.Reflection
import GHC.Stack

import GHC.Exts.Heap.Closures

assertSize :: forall a. (HasCallStack, Typeable a)
           => a -> Int -> IO ()
assertSize !x expected = do
  let !size = closureSize (asBox x)
  when (size /= expected) $ do
    putStrLn $ "closureSize ("++show (typeRep @a)++") == "++show size++", expected "++show expected
    putStrLn $ prettyCallStack callStack
{-# NOINLINE assertSize #-}

pap :: Int -> Char -> Int
pap x _ = x
{-# NOINLINE pap #-}

main :: IO ()
main = do
  assertSize 'a' 2
  assertSize (Just ()) 2
  assertSize (Nothing :: Maybe ()) 2
  assertSize ((1,2) :: (Int,Int)) 3
  assertSize ((1,2,3) :: (Int,Int,Int)) 4
  assertSize (id :: Int -> Int) 1
  assertSize (fst :: (Int,Int) -> Int) 1
  assertSize (pap 1) 2

