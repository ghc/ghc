{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds            #-}
module Basement.Sized.UVect
    ( UVect
    , MUVect
    , unUVect
    , toUVect
    , empty
    , singleton
    , replicate
    , thaw
    , freeze
    , index
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
    ) where

import           Basement.Compat.Base
import           Basement.Nat
import           Basement.NormalForm
import           Basement.Types.OffsetSize
import           Basement.Monad
import           Basement.PrimType (PrimType)
import qualified Basement.UArray as A
import qualified Basement.UArray.Mutable as A hiding (sub)
import           Data.Proxy

newtype UVect (n :: Nat) a = UVect { unUVect :: A.UArray a } deriving (NormalForm, Eq, Show)
newtype MUVect (n :: Nat) ty st = MUVect { unMUVect :: A.MUArray ty st }

toUVect :: forall n ty . (PrimType ty, KnownNat n, Countable ty n) => A.UArray ty -> Maybe (UVect n ty)
toUVect b
    | expected == A.length b = Just (UVect b)
    | otherwise              = Nothing
  where
    expected = toCount @n

empty :: PrimType ty => UVect 0 ty
empty = UVect mempty

singleton :: PrimType ty => ty -> UVect 1 ty
singleton a = UVect (A.singleton a)

create :: forall ty (n :: Nat) . (PrimType ty, Countable ty n, KnownNat n) => (Offset ty -> ty) -> UVect n ty
create f = UVect $ A.create sz f
  where
    sz = natValCountOf (Proxy :: Proxy n)

replicate :: forall n ty . (KnownNat n, Countable ty n, PrimType ty) => ty -> UVect n ty
replicate a = UVect (A.replicate (toCount @n) a)

thaw :: (KnownNat n, PrimMonad prim, PrimType ty) => UVect n ty -> prim (MUVect n ty (PrimState prim))
thaw b = MUVect <$> A.thaw (unUVect b)

freeze ::  (PrimMonad prim, PrimType ty, Countable ty n) => MUVect n ty (PrimState prim) -> prim (UVect n ty)
freeze b = UVect <$> A.freeze (unMUVect b)

write :: (PrimMonad prim, PrimType ty) => MUVect n ty (PrimState prim) -> Offset ty -> ty -> prim ()
write (MUVect ma) ofs v = A.write ma ofs v

read :: (PrimMonad prim, PrimType ty) => MUVect n ty (PrimState prim) -> Offset ty -> prim ty
read (MUVect ma) ofs = A.read ma ofs

indexStatic :: forall i n ty . (KnownNat i, CmpNat i n ~ 'LT, PrimType ty, Offsetable ty i) => UVect n ty -> ty
indexStatic b = A.unsafeIndex (unUVect b) (toOffset @i)

index :: forall i n ty . PrimType ty => UVect n ty -> Offset ty -> ty
index b ofs = A.index (unUVect b) ofs

map :: (PrimType a, PrimType b) => (a -> b) -> UVect n a -> UVect n b
map f b = UVect (A.map f (unUVect b))

foldl' :: PrimType ty => (a -> ty -> a) -> a -> UVect n ty -> a
foldl' f acc b = A.foldl' f acc (unUVect b)

foldr :: PrimType ty => (ty -> a -> a) -> a -> UVect n ty -> a
foldr f acc b = A.foldr f acc (unUVect b)

cons :: PrimType ty => ty -> UVect n ty -> UVect (n+1) ty
cons e = UVect . A.cons e . unUVect

snoc :: PrimType ty => UVect n ty -> ty -> UVect (n+1) ty
snoc b = UVect . A.snoc (unUVect b)

sub :: forall i j n ty
     . ( (i <=? n) ~ 'True
       , (j <=? n) ~ 'True
       , (i <=? j) ~ 'True
       , PrimType ty
       , KnownNat i
       , KnownNat j
       , Offsetable ty i
       , Offsetable ty j )
    => UVect n ty
    -> UVect (j-i) ty
sub block = UVect (A.sub (unUVect block) (toOffset @i) (toOffset @j))

uncons :: forall n ty . (CmpNat 0 n ~ 'LT, PrimType ty, KnownNat n, Offsetable ty n)
       => UVect n ty
       -> (ty, UVect (n-1) ty)
uncons b = (indexStatic @0 b, UVect (A.sub (unUVect b) 1 (toOffset @n)))

unsnoc :: forall n ty . (CmpNat 0 n ~ 'LT, KnownNat n, PrimType ty, Offsetable ty n)
       => UVect n ty
       -> (UVect (n-1) ty, ty)
unsnoc b =
    ( UVect (A.sub (unUVect b) 0 (toOffset @n `offsetSub` 1))
    , A.unsafeIndex (unUVect b) (toOffset @n `offsetSub` 1))

splitAt :: forall i n ty . (CmpNat i n ~ 'LT, PrimType ty, KnownNat i, Countable ty i) => UVect n ty -> (UVect i ty, UVect (n-i) ty)
splitAt b =
    let (left, right) = A.splitAt (toCount @i) (unUVect b)
     in (UVect left, UVect right)

elem :: PrimType ty => ty -> UVect n ty -> Bool
elem e b = A.elem e (unUVect b)

all :: PrimType ty => (ty -> Bool) -> UVect n ty -> Bool
all p b = A.all p (unUVect b)

any :: PrimType ty => (ty -> Bool) -> UVect n ty -> Bool
any p b = A.any p (unUVect b)

find :: PrimType ty => (ty -> Bool) -> UVect n ty -> Maybe ty
find p b = A.find p (unUVect b)

reverse :: PrimType ty => UVect n ty -> UVect n ty
reverse = UVect . A.reverse . unUVect

sortBy :: PrimType ty => (ty -> ty -> Ordering) -> UVect n ty -> UVect n ty
sortBy f b = UVect (A.sortBy f (unUVect b))

intersperse :: (CmpNat n 1 ~ 'GT, PrimType ty) => ty -> UVect n ty -> UVect (n+n-1) ty
intersperse sep b = UVect (A.intersperse sep (unUVect b))

toCount :: forall n ty . (KnownNat n, Countable ty n) => CountOf ty
toCount = natValCountOf (Proxy @n)

toOffset :: forall n ty . (KnownNat n, Offsetable ty n) => Offset ty
toOffset = natValOffset (Proxy @n)
