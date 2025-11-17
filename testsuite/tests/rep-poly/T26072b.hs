{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module T26072b where

-- base
import Data.Kind
import GHC.TypeNats
import GHC.Exts
  ( TYPE, RuntimeRep(..), LiftedRep
  , proxy#
  )

--------------------------------------------------------------------------------

-- Stub for functions in 'primitive' (to avoid dependency)
type PrimArray :: Type -> Type
data PrimArray a = MkPrimArray

indexPrimArray :: PrimArray a -> Int -> a
indexPrimArray _ _ = error "unimplemented"
{-# NOINLINE indexPrimArray #-}

--------------------------------------------------------------------------------

int :: forall n. KnownNat n => Int
int = fromIntegral ( natVal' @n proxy# )

type Fin :: Nat -> Type
newtype Fin n = Fin { getFin :: Int }

-- Vector
type V :: Nat -> Type -> Type
newtype V n a = V ( PrimArray a )

-- Matrix
type M :: Nat -> Type -> Type
newtype M n a = M ( PrimArray a )

type IndexRep :: (Type -> Type) -> RuntimeRep
type family IndexRep f
class Ix f where
  type Index f :: TYPE (IndexRep f)
  (!) :: f a -> Index f -> a
  infixl 9 !

type instance IndexRep ( V n ) = LiftedRep
instance Ix ( V n ) where
  type Index ( V n ) = Fin n
  V v ! Fin !i = indexPrimArray v i
  {-# INLINE (!) #-}

type instance IndexRep ( M m ) = TupleRep [ LiftedRep, LiftedRep ]

instance KnownNat n => Ix ( M n ) where
  type Index ( M n ) = (# Fin n, Fin n #)
  M m ! (# Fin !i, Fin !j #) = indexPrimArray m ( i + j * int @n )
  {-# INLINE (!) #-}

rowCol :: forall n a. ( KnownNat n, Num a ) => Fin n -> M n a -> V n a -> a
rowCol i m v = go 0 ( Fin 0 )
  where
    n = int @n
    go :: a -> Fin n -> a
    go !acc j@( Fin !j_ )
      | j_ >= n
      = acc
      | otherwise
      = go ( acc + m ! (# i , j #) * v ! j ) ( Fin ( j_ + 1 ) )
