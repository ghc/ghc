{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash          #-}

module T29054 where


------------------------------------------------------------------------------
import           Control.Monad.ST                     (ST)
import           Data.Maybe                           (fromMaybe)
import           Data.STRef
import           GHC.Exts (Any, reallyUnsafePtrEquality#, (==#), isTrue#)
import           Unsafe.Coerce
import           Control.Monad.ST

data MutableArray s a = MutableArray

newArray :: Int -> a -> ST s (MutableArray s a)
newArray = undefined

readArray :: MutableArray s a -> Int -> ST s a
readArray = undefined

writeArray :: MutableArray s a -> Int -> a -> ST s ()
writeArray = undefined


type Key a = Any

------------------------------------------------------------------------------
-- Type signatures
emptyRecord :: Key a
deletedRecord :: Key a
keyIsEmpty :: Key a -> Bool
toKey :: a -> Key a
fromKey :: Key a -> a


data TombStone = EmptyElement
               | DeletedElement

{-# NOINLINE emptyRecord #-}
emptyRecord = unsafeCoerce EmptyElement

{-# NOINLINE deletedRecord #-}
deletedRecord = unsafeCoerce DeletedElement

{-# INLINE keyIsEmpty #-}
keyIsEmpty a = isTrue# (x# ==# 1#)
  where
    !x# = reallyUnsafePtrEquality# a emptyRecord

{-# INLINE toKey #-}
toKey = unsafeCoerce

{-# INLINE fromKey #-}
fromKey = unsafeCoerce


type Bucket s k v = Key (Bucket_ s k v)

------------------------------------------------------------------------------
data Bucket_ s k v = Bucket { _bucketSize :: {-# UNPACK #-} !Int
                            , _highwater  :: {-# UNPACK #-} !(STRef s Int)
                            , _keys       :: {-# UNPACK #-} !(MutableArray s k)
                            , _values     :: {-# UNPACK #-} !(MutableArray s v)
                            }


------------------------------------------------------------------------------
emptyWithSize :: Int -> ST s (Bucket s k v)
emptyWithSize !sz = undefined

------------------------------------------------------------------------------
expandArray  :: a                  -- ^ default value
             -> Int                -- ^ new size
             -> Int                -- ^ number of elements to copy
             -> MutableArray s a   -- ^ old array
             -> ST s (MutableArray s a)
expandArray def !sz !hw !arr = undefined

------------------------------------------------------------------------------
growBucketTo :: Int -> Bucket s k v -> ST s (Bucket s k v)
growBucketTo !sz bk | keyIsEmpty bk = emptyWithSize sz
                    | otherwise = do
    if osz >= sz
      then return bk
      else do
        hw <- readSTRef hwRef
        k' <- expandArray undefined sz hw keys
        v' <- expandArray undefined sz hw values
        return $ toKey $ Bucket sz hwRef k' v'

  where
    bucket = fromKey bk
    osz    = _bucketSize bucket
    hwRef  = _highwater bucket
    keys   = _keys bucket
    values = _values bucket

