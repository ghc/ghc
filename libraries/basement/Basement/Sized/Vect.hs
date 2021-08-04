{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds            #-}
module Basement.Sized.Vect
    ( Vect
    , MVect
    , unVect
    , toVect
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
import qualified Basement.BoxedArray as A
--import qualified Basement.BoxedArray.Mutable as A hiding (sub)
import           Data.Proxy

newtype Vect (n :: Nat) a = Vect { unVect :: A.Array a } deriving (NormalForm, Eq, Show)
newtype MVect (n :: Nat) ty st = MVect { unMVect :: A.MArray ty st }

instance Functor (Vect n) where
    fmap = map

toVect :: forall n ty . (KnownNat n, Countable ty n) => A.Array ty -> Maybe (Vect n ty)
toVect b
    | expected == A.length b = Just (Vect b)
    | otherwise = Nothing
  where
    expected = toCount @n

empty :: Vect 0 ty
empty = Vect A.empty

singleton :: ty -> Vect 1 ty
singleton a = Vect (A.singleton a)

create :: forall a (n :: Nat) . (Countable a n, KnownNat n) => (Offset a -> a) -> Vect n a
create f = Vect $ A.create sz f
  where
    sz = natValCountOf (Proxy :: Proxy n)

replicate :: forall n ty . (KnownNat n, Countable ty n) => ty -> Vect n ty
replicate a = Vect (A.replicate (toCount @n) a)

thaw :: (KnownNat n, PrimMonad prim) => Vect n ty -> prim (MVect n ty (PrimState prim))
thaw b = MVect <$> A.thaw (unVect b)

freeze ::  (PrimMonad prim, Countable ty n) => MVect n ty (PrimState prim) -> prim (Vect n ty)
freeze b = Vect <$> A.freeze (unMVect b)

write :: PrimMonad prim => MVect n ty (PrimState prim) -> Offset ty -> ty -> prim ()
write (MVect ma) ofs v = A.write ma ofs v

read :: PrimMonad prim => MVect n ty (PrimState prim) -> Offset ty -> prim ty
read (MVect ma) ofs = A.read ma ofs

indexStatic :: forall i n ty . (KnownNat i, CmpNat i n ~ 'LT, Offsetable ty i) => Vect n ty -> ty
indexStatic b = A.unsafeIndex (unVect b) (toOffset @i)

index :: Vect n ty -> Offset ty -> ty
index b ofs = A.index (unVect b) ofs

map :: (a -> b) -> Vect n a -> Vect n b
map f b = Vect (fmap f (unVect b))

foldl' :: (a -> ty -> a) -> a -> Vect n ty -> a
foldl' f acc b = A.foldl' f acc (unVect b)

foldr :: (ty -> a -> a) -> a -> Vect n ty -> a
foldr f acc b = A.foldr f acc (unVect b)

cons :: ty -> Vect n ty -> Vect (n+1) ty
cons e = Vect . A.cons e . unVect

snoc :: Vect n ty -> ty -> Vect (n+1) ty
snoc b = Vect . A.snoc (unVect b)

sub :: forall i j n ty
     . ( (i <=? n) ~ 'True
       , (j <=? n) ~ 'True
       , (i <=? j) ~ 'True
       , KnownNat i
       , KnownNat j
       , Offsetable ty i
       , Offsetable ty j )
    => Vect n ty
    -> Vect (j-i) ty
sub block = Vect (A.sub (unVect block) (toOffset @i) (toOffset @j))

uncons :: forall n ty . (CmpNat 0 n ~ 'LT, KnownNat n, Offsetable ty n)
       => Vect n ty
       -> (ty, Vect (n-1) ty)
uncons b = (indexStatic @0 b, Vect (A.sub (unVect b) 1 (toOffset @n)))

unsnoc :: forall n ty . (CmpNat 0 n ~ 'LT, KnownNat n, Offsetable ty n)
       => Vect n ty
       -> (Vect (n-1) ty, ty)
unsnoc b =
    ( Vect (A.sub (unVect b) 0 (toOffset @n `offsetSub` 1))
    , A.unsafeIndex (unVect b) (toOffset @n `offsetSub` 1))

splitAt :: forall i n ty . (CmpNat i n ~ 'LT, KnownNat i, Countable ty i) => Vect n ty -> (Vect i ty, Vect (n-i) ty)
splitAt b =
    let (left, right) = A.splitAt (toCount @i) (unVect b)
     in (Vect left, Vect right)

elem :: Eq ty => ty -> Vect n ty -> Bool
elem e b = A.elem e (unVect b)

all :: (ty -> Bool) -> Vect n ty -> Bool
all p b = A.all p (unVect b)

any :: (ty -> Bool) -> Vect n ty -> Bool
any p b = A.any p (unVect b)

find :: (ty -> Bool) -> Vect n ty -> Maybe ty
find p b = A.find p (unVect b)

reverse :: Vect n ty -> Vect n ty
reverse = Vect . A.reverse . unVect

sortBy :: (ty -> ty -> Ordering) -> Vect n ty -> Vect n ty
sortBy f b = Vect (A.sortBy f (unVect b))

intersperse :: (CmpNat n 1 ~ 'GT) => ty -> Vect n ty -> Vect (n+n-1) ty
intersperse sep b = Vect (A.intersperse sep (unVect b))

toCount :: forall n ty . (KnownNat n, Countable ty n) => CountOf ty
toCount = natValCountOf (Proxy @n)

toOffset :: forall n ty . (KnownNat n, Offsetable ty n) => Offset ty
toOffset = natValOffset (Proxy @n)
