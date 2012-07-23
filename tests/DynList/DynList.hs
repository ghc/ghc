{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables#-}

module DynList
(
DynList,
newDynList,
readDynList,
updateDynList,
addToDynList
) where

import Data.Dynamic
import Data.Maybe
import Data.IORef
import Data.List

newtype DynList = DynList ([IORef Dynamic])

newDynList :: IO (DynList)
newDynList = return $ DynList []

readDynList :: Typeable a => DynList -> IO a
readDynList (DynList []) = error "Error: readDynList"
readDynList (DynList (i:tail)) = do
  v <- readIORef i
  case fromDynamic v of
    Nothing -> readDynList (DynList tail)
    Just x -> return x

updateDynList :: Typeable a => DynList -> a -> IO a
updateDynList (DynList []) x = error "Error: updateDynList"
updateDynList (DynList (ref:tail)) x = do
  dv <- readIORef ref
  case fromDynamic dv of
    Nothing -> do
      updateDynList (DynList tail) x
    Just v -> do
      writeIORef ref $ toDyn x
      return v

addToDynList :: Typeable a => DynList -> a -> IO DynList
addToDynList (DynList l) v = do
  iv <- newIORef $ toDyn v
  let !r = DynList $ iv:l
  return r
