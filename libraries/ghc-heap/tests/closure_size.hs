{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

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

pap :: Int -> Maybe Char -> Int
pap x _ = x
{-# NOINLINE pap #-}

main :: IO ()
main = do
  -- Ensure that GHC can't turn PAP into a FUN (see #16531)
  let x :: Int
      x = 42
      {-# NOINLINE x #-}

  assertSize 'a' 2
  assertSize (Just ()) 2
  assertSize (Nothing :: Maybe ()) 2
  assertSize ((1,2) :: (Int,Int)) 3
  assertSize ((1,2,3) :: (Int,Int,Int)) 4
  assertSize (id :: Int -> Int) 1
  assertSize (fst :: (Int,Int) -> Int) 1
  assertSize (pap x) 2

