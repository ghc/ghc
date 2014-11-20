{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, NoImplicitPrelude, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module GHC.Event.IntTable
    (
      IntTable
    , new
    , lookup
    , insertWith
    , reset
    , delete
    , updateWith
    ) where

import Data.Bits ((.&.), shiftL, shiftR)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Storable (peek, poke)
import GHC.Base (Monad(..), (=<<), ($), const, liftM, otherwise, when)
import GHC.Classes (Eq(..), Ord(..))
import GHC.Event.Arr (Arr)
import GHC.Num (Num(..))
import GHC.Prim (seq)
import GHC.Types (Bool(..), IO(..), Int(..))
import qualified GHC.Event.Arr as Arr

-- A very simple chained integer-keyed mutable hash table. We use
-- power-of-two sizing, grow at a load factor of 0.75, and never
-- shrink. The "hash function" is the identity function.

newtype IntTable a = IntTable (IORef (IT a))

data IT a = IT {
      tabArr  :: {-# UNPACK #-} !(Arr (Bucket a))
    , tabSize :: {-# UNPACK #-} !(ForeignPtr Int)
    }

data Bucket a = Empty
              | Bucket {
      bucketKey   :: {-# UNPACK #-} !Int
    , bucketValue :: a
    , bucketNext  :: Bucket a
    }

lookup :: Int -> IntTable a -> IO (Maybe a)
lookup k (IntTable ref) = do
  let go Bucket{..}
        | bucketKey == k = return (Just bucketValue)
        | otherwise      = go bucketNext
      go _ = return Nothing
  it@IT{..} <- readIORef ref
  go =<< Arr.read tabArr (indexOf k it)

new :: Int -> IO (IntTable a)
new capacity = IntTable `liftM` (newIORef =<< new_ capacity)

new_ :: Int -> IO (IT a)
new_ capacity = do
  arr <- Arr.new Empty capacity
  size <- mallocForeignPtr
  withForeignPtr size $ \ptr -> poke ptr 0
  return IT { tabArr = arr
            , tabSize = size
            }

grow :: IT a -> IORef (IT a) -> Int -> IO ()
grow oldit ref size = do
  newit <- new_ (Arr.size (tabArr oldit) `shiftL` 1)
  let copySlot n !i
        | n == size = return ()
        | otherwise = do
          let copyBucket !m Empty          = copySlot m (i+1)
              copyBucket  m bkt@Bucket{..} = do
                let idx = indexOf bucketKey newit
                next <- Arr.read (tabArr newit) idx
                Arr.write (tabArr newit) idx bkt { bucketNext = next }
                copyBucket (m+1) bucketNext
          copyBucket n =<< Arr.read (tabArr oldit) i
  copySlot 0 0
  withForeignPtr (tabSize newit) $ \ptr -> poke ptr size
  writeIORef ref newit

insertWith :: (a -> a -> a) -> Int -> a -> IntTable a -> IO (Maybe a)
insertWith f k v inttable@(IntTable ref) = do
  it@IT{..} <- readIORef ref
  let idx = indexOf k it
      go seen bkt@Bucket{..}
        | bucketKey == k = do
          let !v' = f v bucketValue
              !next = seen <> bucketNext
              Empty        <> bs = bs
              b@Bucket{..} <> bs = b { bucketNext = bucketNext <> bs }
          Arr.write tabArr idx (Bucket k v' next)
          return (Just bucketValue)
        | otherwise = go bkt { bucketNext = seen } bucketNext
      go seen _ = withForeignPtr tabSize $ \ptr -> do
        size <- peek ptr
        if size + 1 >= Arr.size tabArr - (Arr.size tabArr `shiftR` 2)
          then grow it ref size >> insertWith f k v inttable
          else do
            v `seq` Arr.write tabArr idx (Bucket k v seen)
            poke ptr (size + 1)
            return Nothing
  go Empty =<< Arr.read tabArr idx
{-# INLINABLE insertWith #-}

-- | Used to undo the effect of a prior insertWith.
reset :: Int -> Maybe a -> IntTable a -> IO ()
reset k (Just v) tbl = insertWith const k v tbl >> return ()
reset k Nothing  tbl = delete k tbl >> return ()

indexOf :: Int -> IT a -> Int
indexOf k IT{..} = k .&. (Arr.size tabArr - 1)

delete :: Int -> IntTable a -> IO (Maybe a)
delete k t = updateWith (const Nothing) k t

updateWith :: (a -> Maybe a) -> Int -> IntTable a -> IO (Maybe a)
updateWith f k (IntTable ref) = do
  it@IT{..} <- readIORef ref
  let idx = indexOf k it
      go changed bkt@Bucket{..}
        | bucketKey == k =
            let fbv = f bucketValue
                !nb = case fbv of
                        Just val -> bkt { bucketValue = val }
                        Nothing  -> bucketNext
            in (fbv, Just bucketValue, nb)
        | otherwise = case go changed bucketNext of
                        (fbv, ov, nb) -> (fbv, ov, bkt { bucketNext = nb })
      go _ e = (Nothing, Nothing, e)
  (fbv, oldVal, newBucket) <- go False `liftM` Arr.read tabArr idx
  when (isJust oldVal) $ do
    Arr.write tabArr idx newBucket
    when (isNothing fbv) $
      withForeignPtr tabSize $ \ptr -> do
        size <- peek ptr
        poke ptr (size - 1)
  return oldVal
