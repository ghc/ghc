module T15907A
  ( memo
  , memoSized
  ) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Data.Array.IO (IOArray, newArray, readArray, writeArray)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, hashStableName, makeStableName)
import System.Mem.Weak (Weak, deRefWeak, finalize, mkWeak, mkWeakPtr)

type MemoTable key val =
  MVar
    ( Int
    , IOArray Int [MemoEntry key val]
    )

data MemoEntry key val = MemoEntry !(StableName key) !(Weak val)

memo :: (a -> b) -> a -> b
memo f = memoSized defaultTableSize f

defaultTableSize :: Int
defaultTableSize = 1001

memoSized :: Int -> (a -> b) -> a -> b
memoSized size f = strict (lazyMemoSized size f)

strict :: (a -> b) -> a -> b
strict = ($!)

lazyMemoSized :: Int -> (a -> b) -> a -> b
lazyMemoSized size f =
  let (table, weak) =
        unsafePerformIO $ do
          tbl <- newArray (0, size) []
          mvar <- newMVar (size, tbl)
          weak <- mkWeakPtr mvar (Just (tableFinalizer tbl size))
          pure (mvar, weak)
   in memo' f table weak

tableFinalizer :: IOArray Int [MemoEntry key val] -> Int -> IO ()
tableFinalizer table size =
  sequence_ [finalizeBucket i | i <- [0 .. size]]
  where
    finalizeBucket i = do
      bucket <- readArray table i
      sequence_ [finalize w | MemoEntry _ w <- bucket]

memo' :: (a -> b) -> MemoTable a b -> Weak (MemoTable a b) -> a -> b
memo' f ref weakRef = \k -> unsafePerformIO $ do
  stableKey <- makeStableName k
  (size, table) <- takeMVar ref
  let hashKey = hashStableName stableKey `mod` size
  bucket <- readArray table hashKey
  lkp <- lookupSN stableKey bucket
  case lkp of
    Just result -> do
      putMVar ref (size, table)
      pure result
    Nothing -> do
      let result = f k
      weak <- mkWeak k result (Just (finalizer hashKey stableKey weakRef))
      writeArray table hashKey (MemoEntry stableKey weak : bucket)
      putMVar ref (size, table)
      pure result

finalizer :: Int -> StableName a -> Weak (MemoTable a b) -> IO ()
finalizer hashKey stableKey weakRef = do
  r <- deRefWeak weakRef
  case r of
    Nothing -> pure ()
    Just mvar -> do
      (size, table) <- takeMVar mvar
      bucket <- readArray table hashKey
      let newBucket =
            [ e
            | e@(MemoEntry sn _) <- bucket
            , sn /= stableKey
            ]
      writeArray table hashKey newBucket
      putMVar mvar (size, table)

lookupSN :: StableName key -> [MemoEntry key val] -> IO (Maybe val)
lookupSN sn [] = sn `seq` pure Nothing
lookupSN sn (MemoEntry sn' weak : xs)
  | sn == sn' = do
      maybeItem <- deRefWeak weak
      case maybeItem of
        Nothing -> error ("dead weak pair: " ++ show (hashStableName sn))
        Just v -> pure (Just v)
  | otherwise = lookupSN sn xs
