{-# LANGUAGE CPP,PatternGuards #-}
module ImmList where

import Control.Concurrent
import Data.IORef
import qualified Data.Sequence as S
import Data.Sequence (Seq, (|>), (<|), (><), ViewL(..))
import Data.Maybe
import Data.Foldable

#if 0
newtype ListHandle a = ListHandle (IORef (Seq a))

newList :: IO (ListHandle a)
newList = do
  r <- newIORef S.empty
  return (ListHandle r)

addToTail :: Eq a => ListHandle a -> a -> IO ()
addToTail (ListHandle r) x = 
  atomicModifyIORef r $ \s -> (s |> x, ())

find :: Eq a => ListHandle a -> a -> IO Bool
find (ListHandle r) x = do
  s <- readIORef r
  return (isJust (S.elemIndexL x s))

delete :: Eq a => ListHandle a -> a -> IO Bool
delete (ListHandle r) x = atomicModifyIORef r $ \s ->
  case S.breakl (== x) s of
    (xs, ys) | c :< zs <- S.viewl ys, c == x -> (xs >< zs, True)
             | otherwise                     -> (xs >< ys, False)

printList :: Show a => ListHandle a -> IO ()
printList (ListHandle r) = do
  s <- readIORef r
  print (toList s)

cntList :: Show a => ListHandle a -> IO Int
cntList (ListHandle r) = readIORef r >>= return . S.length
#else
newtype ListHandle a = ListHandle (IORef [a])

newList :: IO (ListHandle a)
newList = do
  r <- newIORef []
  return (ListHandle r)

addToTail :: Eq a => ListHandle a -> a -> IO ()
addToTail (ListHandle r) x = 
  atomicModifyIORef r $ \s -> (s ++ [x], ())

find :: Eq a => ListHandle a -> a -> IO Bool
find (ListHandle r) x = do
  s <- readIORef r
  return (x `Prelude.elem` s)

delete :: Eq a => ListHandle a -> a -> IO Bool
delete (ListHandle r) x = atomicModifyIORef r $ \s ->
  case break (== x) s of
    (xs, ys) | (c:zs) <- ys, c == x -> (xs ++ zs, True)
             | otherwise            -> (xs ++ ys, False)

printList :: Show a => ListHandle a -> IO ()
printList (ListHandle r) = do
  s <- readIORef r
  print s

cntList :: Show a => ListHandle a -> IO Int
cntList (ListHandle r) = readIORef r >>= return . length
#endif
