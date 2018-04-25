{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables, KindSignatures,
             MultiParamTypeClasses, FunctionalDependencies #-}

-- This one triggered a bug in the indirection-shorting 
-- machinery, which gave a core-lint error

module MHashTable (STHashTable, new, update) where

import Data.Int             (Int32)
import Control.Monad.ST     (ST)
import Data.STRef           (STRef)
import Data.Array.ST        (STArray)
import Data.Array.MArray    (writeArray)

class Monad m => MutHash arr ref m | arr -> m, ref -> m
                                   , arr -> ref, ref -> arr where
    newMHArray  :: (Int32, Int32) -> a -> m (arr Int32 a)
    readMHArray :: arr Int32 a -> Int32 -> m a
    writeMHArray:: arr Int32 a -> Int32 -> a -> m ()

    newMHRef    :: a -> m (ref a)
    readMHRef   :: ref a -> m a
    writeMHRef  :: ref a -> a -> m ()

instance MutHash (STArray s) (STRef s) (ST s) where
    newMHArray  = undefined
    readMHArray = undefined
    writeMHArray= writeArray

    newMHRef    = undefined
    readMHRef   = undefined
    writeMHRef  = undefined

type STHashTable s key val = HashTable key val (STArray s) (STRef s) (ST s)

newtype HashTable key val arr ref m = HashTable (ref (HT key val arr ref m))

data HT key val arr (ref :: * -> *) (m :: * -> *) = HT { dir :: (arr Int32 (arr Int32 [(key,val)])) }

new :: forall arr ref m key val. (MutHash arr ref m) => m (HashTable key val arr ref m)
new = do
  (dir::arr Int32 (arr Int32 [(key,val)]))  <- newMHArray (0,0) undefined
  (segment::arr Int32 [(key,val)])          <- return undefined
  return (undefined :: HashTable key val arr ref m)

{-# RULES "update/ST" update = updateST #-}
updateST:: STHashTable s k v -> k -> v -> ST s Bool
updateST= update'

update  :: (MutHash arr ref m)
        => HashTable key val arr ref m -> key -> val -> m Bool
{-# NOINLINE [1] update #-}
update  = update'

update' :: (MutHash arr ref m)
        => HashTable key val arr ref m -> key -> val -> m Bool
update' _ _ _ = return False
