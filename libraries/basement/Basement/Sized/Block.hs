-- |
-- Module      : Basement.Sized.Block
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- A Nat-sized version of Block
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType               #-}
#endif

module Basement.Sized.Block
    ( BlockN
    , MutableBlockN
    , length
    , lengthBytes
    , toBlockN
    , toBlock
    , new
    , newPinned
    , singleton
    , replicate
    , thaw
    , freeze
    , index
    , indexStatic
    , map
    , foldl'
    , foldr
    , cons
    , snoc
    , elem
    , sub
    , uncons
    , unsnoc
    , splitAt
    , all
    , any
    , find
    , reverse
    , sortBy
    , intersperse
    , withPtr
    , withMutablePtr
    , withMutablePtrHint
    , cast
    , mutableCast
    ) where

import           Data.Proxy (Proxy(..))
import           Basement.Compat.Base
import           Basement.Numerical.Additive (scale)
import           Basement.Block (Block, MutableBlock(..), unsafeIndex)
import qualified Basement.Block as B
import qualified Basement.Block.Base as B
import           Basement.Monad (PrimMonad, PrimState)
import           Basement.Nat
import           Basement.Types.OffsetSize
import           Basement.NormalForm
import           Basement.PrimType (PrimType, PrimSize, primSizeInBytes)

-- | Sized version of 'Block'
--
newtype BlockN (n :: Nat) a = BlockN { unBlock :: Block a }
  deriving (NormalForm, Eq, Show, Data, Ord)

newtype MutableBlockN (n :: Nat) ty st = MutableBlockN { unMBlock :: MutableBlock ty st }

toBlockN :: forall n ty . (PrimType ty, KnownNat n, Countable ty n) => Block ty -> Maybe (BlockN n ty)
toBlockN b
    | expected == B.length b = Just (BlockN b)
    | otherwise = Nothing
  where
    expected = toCount @n

length :: forall n ty
        . (KnownNat n, Countable ty n)
       => BlockN n ty
       -> CountOf ty
length _ = toCount @n

lengthBytes :: forall n ty
             . PrimType ty
            => BlockN n ty
            -> CountOf Word8
lengthBytes = B.lengthBytes . unBlock

toBlock :: BlockN n ty -> Block ty
toBlock = unBlock

cast :: forall n m a b
      . ( PrimType a, PrimType b
        , KnownNat n, KnownNat m
        , ((PrimSize b) * m) ~ ((PrimSize a) * n)
        )
      => BlockN n a
      -> BlockN m b
cast (BlockN b) = BlockN (B.unsafeCast b)

mutableCast :: forall n m a b st
             . ( PrimType a, PrimType b
             , KnownNat n, KnownNat m
             , ((PrimSize b) * m) ~ ((PrimSize a) * n)
             )
            => MutableBlockN n a st
            -> MutableBlockN m b st
mutableCast (MutableBlockN b) = MutableBlockN (B.unsafeRecast b)

-- | Create a new unpinned mutable block of a specific N size of 'ty' elements
--
-- If the size exceeds a GHC-defined threshold, then the memory will be
-- pinned. To be certain about pinning status with small size, use 'newPinned'
new :: forall n ty prim
     . (PrimType ty, KnownNat n, Countable ty n, PrimMonad prim)
    => prim (MutableBlockN n ty (PrimState prim))
new = MutableBlockN <$> B.new (toCount @n)

-- | Create a new pinned mutable block of a specific N size of 'ty' elements
newPinned :: forall n ty prim
           . (PrimType ty, KnownNat n, Countable ty n, PrimMonad prim)
          => prim (MutableBlockN n ty (PrimState prim))
newPinned = MutableBlockN <$> B.newPinned (toCount @n)

singleton :: PrimType ty => ty -> BlockN 1 ty
singleton a = BlockN (B.singleton a)

replicate :: forall n ty . (KnownNat n, Countable ty n, PrimType ty) => ty -> BlockN n ty
replicate a = BlockN (B.replicate (toCount @n) a)

thaw :: (KnownNat n, PrimMonad prim, PrimType ty) => BlockN n ty -> prim (MutableBlockN n ty (PrimState prim))
thaw b = MutableBlockN <$> B.thaw (unBlock b)

freeze ::  (PrimMonad prim, PrimType ty, Countable ty n) => MutableBlockN n ty (PrimState prim) -> prim (BlockN n ty)
freeze b = BlockN <$> B.freeze (unMBlock b)

indexStatic :: forall i n ty . (KnownNat i, CmpNat i n ~ 'LT, PrimType ty, Offsetable ty i) => BlockN n ty -> ty
indexStatic b = unsafeIndex (unBlock b) (toOffset @i)

index :: forall i n ty . PrimType ty => BlockN n ty -> Offset ty -> ty
index b ofs = B.index (unBlock b) ofs

map :: (PrimType a, PrimType b) => (a -> b) -> BlockN n a -> BlockN n b
map f b = BlockN (B.map f (unBlock b))

foldl' :: PrimType ty => (a -> ty -> a) -> a -> BlockN n ty -> a
foldl' f acc b = B.foldl' f acc (unBlock b)

foldr :: PrimType ty => (ty -> a -> a) -> a -> BlockN n ty -> a
foldr f acc b = B.foldr f acc (unBlock b)

cons :: PrimType ty => ty -> BlockN n ty -> BlockN (n+1) ty
cons e = BlockN . B.cons e . unBlock

snoc :: PrimType ty => BlockN n ty -> ty -> BlockN (n+1) ty
snoc b = BlockN . B.snoc (unBlock b)

sub :: forall i j n ty
     . ( (i <=? n) ~ 'True
       , (j <=? n) ~ 'True
       , (i <=? j) ~ 'True
       , PrimType ty
       , KnownNat i
       , KnownNat j
       , Offsetable ty i
       , Offsetable ty j )
    => BlockN n ty
    -> BlockN (j-i) ty
sub block = BlockN (B.sub (unBlock block) (toOffset @i) (toOffset @j))

uncons :: forall n ty . (CmpNat 0 n ~ 'LT, PrimType ty, KnownNat n, Offsetable ty n)
       => BlockN n ty
       -> (ty, BlockN (n-1) ty)
uncons b = (indexStatic @0 b, BlockN (B.sub (unBlock b) 1 (toOffset @n)))

unsnoc :: forall n ty . (CmpNat 0 n ~ 'LT, KnownNat n, PrimType ty, Offsetable ty n)
       => BlockN n ty
       -> (BlockN (n-1) ty, ty)
unsnoc b =
    ( BlockN (B.sub (unBlock b) 0 (toOffset @n `offsetSub` 1))
    , unsafeIndex (unBlock b) (toOffset @n `offsetSub` 1))

splitAt :: forall i n ty . (CmpNat i n ~ 'LT, PrimType ty, KnownNat i, Countable ty i) => BlockN n ty -> (BlockN i ty, BlockN (n-i) ty)
splitAt b =
    let (left, right) = B.splitAt (toCount @i) (unBlock b)
     in (BlockN left, BlockN right)

elem :: PrimType ty => ty -> BlockN n ty -> Bool
elem e b = B.elem e (unBlock b)

all :: PrimType ty => (ty -> Bool) -> BlockN n ty -> Bool
all p b = B.all p (unBlock b)

any :: PrimType ty => (ty -> Bool) -> BlockN n ty -> Bool
any p b = B.any p (unBlock b)

find :: PrimType ty => (ty -> Bool) -> BlockN n ty -> Maybe ty
find p b = B.find p (unBlock b)

reverse :: PrimType ty => BlockN n ty -> BlockN n ty
reverse = BlockN . B.reverse . unBlock

sortBy :: PrimType ty => (ty -> ty -> Ordering) -> BlockN n ty -> BlockN n ty
sortBy f b = BlockN (B.sortBy f (unBlock b))

intersperse :: (CmpNat n 1 ~ 'GT, PrimType ty) => ty -> BlockN n ty -> BlockN (n+n-1) ty
intersperse sep b = BlockN (B.intersperse sep (unBlock b))

toCount :: forall n ty . (KnownNat n, Countable ty n) => CountOf ty
toCount = natValCountOf (Proxy @n)

toOffset :: forall n ty . (KnownNat n, Offsetable ty n) => Offset ty
toOffset = natValOffset (Proxy @n)

-- | Get a Ptr pointing to the data in the Block.
--
-- Since a Block is immutable, this Ptr shouldn't be
-- to use to modify the contents
--
-- If the Block is pinned, then its address is returned as is,
-- however if it's unpinned, a pinned copy of the Block is made
-- before getting the address.
withPtr :: (PrimMonad prim, KnownNat n)
        => BlockN n ty
        -> (Ptr ty -> prim a)
        -> prim a
withPtr b = B.withPtr (unBlock b)

-- | Create a pointer on the beginning of the MutableBlock
-- and call a function 'f'.
--
-- The mutable block can be mutated by the 'f' function
-- and the change will be reflected in the mutable block
--
-- If the mutable block is unpinned, a trampoline buffer
-- is created and the data is only copied when 'f' return.
--
-- it is all-in-all highly inefficient as this cause 2 copies
withMutablePtr :: (PrimMonad prim, KnownNat n)
               => MutableBlockN n ty (PrimState prim)
               -> (Ptr ty -> prim a)
               -> prim a
withMutablePtr mb = B.withMutablePtr (unMBlock mb)

-- | Same as 'withMutablePtr' but allow to specify 2 optimisations
-- which is only useful when the MutableBlock is unpinned and need
-- a pinned trampoline to be called safely.
--
-- If skipCopy is True, then the first copy which happen before
-- the call to 'f', is skipped. The Ptr is now effectively
-- pointing to uninitialized data in a new mutable Block.
--
-- If skipCopyBack is True, then the second copy which happen after
-- the call to 'f', is skipped. Then effectively in the case of a
-- trampoline being used the memory changed by 'f' will not
-- be reflected in the original Mutable Block.
--
-- If using the wrong parameters, it will lead to difficult to
-- debug issue of corrupted buffer which only present themselves
-- with certain Mutable Block that happened to have been allocated
-- unpinned.
--
-- If unsure use 'withMutablePtr', which default to *not* skip
-- any copy.
withMutablePtrHint :: forall n ty prim a . (PrimMonad prim, KnownNat n)
                   => Bool -- ^ hint that the buffer doesn't need to have the same value as the mutable block when calling f
                   -> Bool -- ^ hint that the buffer is not supposed to be modified by call of f
                   -> MutableBlockN n ty (PrimState prim)
                   -> (Ptr ty -> prim a)
                   -> prim a
withMutablePtrHint skipCopy skipCopyBack (MutableBlockN mb) f =
    B.withMutablePtrHint skipCopy skipCopyBack mb f
