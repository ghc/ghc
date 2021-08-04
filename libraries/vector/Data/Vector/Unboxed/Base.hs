{-# LANGUAGE BangPatterns, CPP, MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module      : Data.Vector.Unboxed.Base
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Adaptive unboxed vectors: basic implementation
--

module Data.Vector.Unboxed.Base (
  MVector(..), IOVector, STVector, Vector(..), Unbox
) where

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Vector.Primitive as P

import Control.Applicative (Const(..))

import Control.DeepSeq ( NFData(rnf)
#if MIN_VERSION_deepseq(1,4,3)
                       , NFData1(liftRnf)
#endif
                       )

import Control.Monad.Primitive
import Control.Monad ( liftM )

#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Compose
#endif

import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Int  ( Int8, Int16, Int32, Int64 )
import Data.Complex
import Data.Monoid (Dual(..),Sum(..),Product(..),All(..),Any(..))
#if MIN_VERSION_base(4,8,0)
import Data.Monoid (Alt(..))
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Min(..),Max(..),First(..),Last(..),WrappedMonoid(..),Arg(..))
#endif
#if !MIN_VERSION_base(4,8,0)
import Data.Word ( Word )
#endif

#if __GLASGOW_HASKELL__ >= 707
import Data.Typeable ( Typeable )
#else
import Data.Typeable ( Typeable1(..), Typeable2(..), mkTyConApp,
                       mkTyCon3
                     )
#endif
import Data.Data     ( Data(..) )
import GHC.Exts      ( Down(..) )

-- Data.Vector.Internal.Check is unused
#define NOT_VECTOR_MODULE
#include "vector.h"

data family MVector s a
data family Vector    a

type IOVector = MVector RealWorld
type STVector s = MVector s

type instance G.Mutable Vector = MVector

class (G.Vector Vector a, M.MVector MVector a) => Unbox a

instance NFData (Vector a) where rnf !_ = ()
instance NFData (MVector s a) where rnf !_ = ()

#if MIN_VERSION_deepseq(1,4,3)
-- | @since 0.12.1.0
instance NFData1 Vector where
  liftRnf _ !_ = ()
-- | @since 0.12.1.0
instance NFData1 (MVector s) where
  liftRnf _ !_ = ()
#endif

-- -----------------
-- Data and Typeable
-- -----------------
#if __GLASGOW_HASKELL__ >= 707
deriving instance Typeable Vector
deriving instance Typeable MVector
#else
vectorTyCon = mkTyCon3 "vector"

instance Typeable1 Vector where
  typeOf1 _ = mkTyConApp (vectorTyCon "Data.Vector.Unboxed" "Vector") []

instance Typeable2 MVector where
  typeOf2 _ = mkTyConApp (vectorTyCon "Data.Vector.Unboxed.Mutable" "MVector") []
#endif

instance (Data a, Unbox a) => Data (Vector a) where
  gfoldl       = G.gfoldl
  toConstr _   = G.mkVecConstr "Data.Vector.Unboxed.Vector"
  gunfold      = G.gunfold
  dataTypeOf _ = G.mkVecType "Data.Vector.Unboxed.Vector"
  dataCast1    = G.dataCast

-- ----
-- Unit
-- ----

newtype instance MVector s () = MV_Unit Int
newtype instance Vector    () = V_Unit Int

instance Unbox ()

instance M.MVector MVector () where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}

  basicLength (MV_Unit n) = n

  basicUnsafeSlice _ m (MV_Unit _) = MV_Unit m

  basicOverlaps _ _ = False

  basicUnsafeNew n = return (MV_Unit n)

  -- Nothing to initialize
  basicInitialize _ = return ()

  basicUnsafeRead (MV_Unit _) _ = return ()

  basicUnsafeWrite (MV_Unit _) _ () = return ()

  basicClear _ = return ()

  basicSet (MV_Unit _) () = return ()

  basicUnsafeCopy (MV_Unit _) (MV_Unit _) = return ()

  basicUnsafeGrow (MV_Unit n) m = return $ MV_Unit (n+m)

instance G.Vector Vector () where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Unit n) = return $ V_Unit n

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Unit n) = return $ MV_Unit n

  {-# INLINE basicLength #-}
  basicLength (V_Unit n) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice _ m (V_Unit _) = V_Unit m

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Unit _) _ = return ()

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Unit _) (V_Unit _) = return ()

  {-# INLINE elemseq #-}
  elemseq _ = seq


-- ---------------
-- Primitive types
-- ---------------

#define primMVector(ty,con)                                             \
instance M.MVector MVector ty where {                                   \
  {-# INLINE basicLength #-}                                            \
; {-# INLINE basicUnsafeSlice #-}                                       \
; {-# INLINE basicOverlaps #-}                                          \
; {-# INLINE basicUnsafeNew #-}                                         \
; {-# INLINE basicInitialize #-}                                        \
; {-# INLINE basicUnsafeReplicate #-}                                   \
; {-# INLINE basicUnsafeRead #-}                                        \
; {-# INLINE basicUnsafeWrite #-}                                       \
; {-# INLINE basicClear #-}                                             \
; {-# INLINE basicSet #-}                                               \
; {-# INLINE basicUnsafeCopy #-}                                        \
; {-# INLINE basicUnsafeGrow #-}                                        \
; basicLength (con v) = M.basicLength v                                 \
; basicUnsafeSlice i n (con v) = con $ M.basicUnsafeSlice i n v         \
; basicOverlaps (con v1) (con v2) = M.basicOverlaps v1 v2               \
; basicUnsafeNew n = con `liftM` M.basicUnsafeNew n                     \
; basicInitialize (con v) = M.basicInitialize v                         \
; basicUnsafeReplicate n x = con `liftM` M.basicUnsafeReplicate n x     \
; basicUnsafeRead (con v) i = M.basicUnsafeRead v i                     \
; basicUnsafeWrite (con v) i x = M.basicUnsafeWrite v i x               \
; basicClear (con v) = M.basicClear v                                   \
; basicSet (con v) x = M.basicSet v x                                   \
; basicUnsafeCopy (con v1) (con v2) = M.basicUnsafeCopy v1 v2           \
; basicUnsafeMove (con v1) (con v2) = M.basicUnsafeMove v1 v2           \
; basicUnsafeGrow (con v) n = con `liftM` M.basicUnsafeGrow v n }

#define primVector(ty,con,mcon)                                         \
instance G.Vector Vector ty where {                                     \
  {-# INLINE basicUnsafeFreeze #-}                                      \
; {-# INLINE basicUnsafeThaw #-}                                        \
; {-# INLINE basicLength #-}                                            \
; {-# INLINE basicUnsafeSlice #-}                                       \
; {-# INLINE basicUnsafeIndexM #-}                                      \
; {-# INLINE elemseq #-}                                                \
; basicUnsafeFreeze (mcon v) = con `liftM` G.basicUnsafeFreeze v        \
; basicUnsafeThaw (con v) = mcon `liftM` G.basicUnsafeThaw v            \
; basicLength (con v) = G.basicLength v                                 \
; basicUnsafeSlice i n (con v) = con $ G.basicUnsafeSlice i n v         \
; basicUnsafeIndexM (con v) i = G.basicUnsafeIndexM v i                 \
; basicUnsafeCopy (mcon mv) (con v) = G.basicUnsafeCopy mv v            \
; elemseq _ = seq }

newtype instance MVector s Int = MV_Int (P.MVector s Int)
newtype instance Vector    Int = V_Int  (P.Vector    Int)
instance Unbox Int
primMVector(Int, MV_Int)
primVector(Int, V_Int, MV_Int)

newtype instance MVector s Int8 = MV_Int8 (P.MVector s Int8)
newtype instance Vector    Int8 = V_Int8  (P.Vector    Int8)
instance Unbox Int8
primMVector(Int8, MV_Int8)
primVector(Int8, V_Int8, MV_Int8)

newtype instance MVector s Int16 = MV_Int16 (P.MVector s Int16)
newtype instance Vector    Int16 = V_Int16  (P.Vector    Int16)
instance Unbox Int16
primMVector(Int16, MV_Int16)
primVector(Int16, V_Int16, MV_Int16)

newtype instance MVector s Int32 = MV_Int32 (P.MVector s Int32)
newtype instance Vector    Int32 = V_Int32  (P.Vector    Int32)
instance Unbox Int32
primMVector(Int32, MV_Int32)
primVector(Int32, V_Int32, MV_Int32)

newtype instance MVector s Int64 = MV_Int64 (P.MVector s Int64)
newtype instance Vector    Int64 = V_Int64  (P.Vector    Int64)
instance Unbox Int64
primMVector(Int64, MV_Int64)
primVector(Int64, V_Int64, MV_Int64)


newtype instance MVector s Word = MV_Word (P.MVector s Word)
newtype instance Vector    Word = V_Word  (P.Vector    Word)
instance Unbox Word
primMVector(Word, MV_Word)
primVector(Word, V_Word, MV_Word)

newtype instance MVector s Word8 = MV_Word8 (P.MVector s Word8)
newtype instance Vector    Word8 = V_Word8  (P.Vector    Word8)
instance Unbox Word8
primMVector(Word8, MV_Word8)
primVector(Word8, V_Word8, MV_Word8)

newtype instance MVector s Word16 = MV_Word16 (P.MVector s Word16)
newtype instance Vector    Word16 = V_Word16  (P.Vector    Word16)
instance Unbox Word16
primMVector(Word16, MV_Word16)
primVector(Word16, V_Word16, MV_Word16)

newtype instance MVector s Word32 = MV_Word32 (P.MVector s Word32)
newtype instance Vector    Word32 = V_Word32  (P.Vector    Word32)
instance Unbox Word32
primMVector(Word32, MV_Word32)
primVector(Word32, V_Word32, MV_Word32)

newtype instance MVector s Word64 = MV_Word64 (P.MVector s Word64)
newtype instance Vector    Word64 = V_Word64  (P.Vector    Word64)
instance Unbox Word64
primMVector(Word64, MV_Word64)
primVector(Word64, V_Word64, MV_Word64)


newtype instance MVector s Float = MV_Float (P.MVector s Float)
newtype instance Vector    Float = V_Float  (P.Vector    Float)
instance Unbox Float
primMVector(Float, MV_Float)
primVector(Float, V_Float, MV_Float)

newtype instance MVector s Double = MV_Double (P.MVector s Double)
newtype instance Vector    Double = V_Double  (P.Vector    Double)
instance Unbox Double
primMVector(Double, MV_Double)
primVector(Double, V_Double, MV_Double)


newtype instance MVector s Char = MV_Char (P.MVector s Char)
newtype instance Vector    Char = V_Char  (P.Vector    Char)
instance Unbox Char
primMVector(Char, MV_Char)
primVector(Char, V_Char, MV_Char)

-- ----
-- Bool
-- ----

fromBool :: Bool -> Word8
{-# INLINE fromBool #-}
fromBool True = 1
fromBool False = 0

toBool :: Word8 -> Bool
{-# INLINE toBool #-}
toBool 0 = False
toBool _ = True

newtype instance MVector s Bool = MV_Bool (P.MVector s Word8)
newtype instance Vector    Bool = V_Bool  (P.Vector    Word8)

instance Unbox Bool

instance M.MVector MVector Bool where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Bool v) = M.basicLength v
  basicUnsafeSlice i n (MV_Bool v) = MV_Bool $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Bool v1) (MV_Bool v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Bool `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Bool v) = M.basicInitialize v
  basicUnsafeReplicate n x = MV_Bool `liftM` M.basicUnsafeReplicate n (fromBool x)
  basicUnsafeRead (MV_Bool v) i = toBool `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Bool v) i x = M.basicUnsafeWrite v i (fromBool x)
  basicClear (MV_Bool v) = M.basicClear v
  basicSet (MV_Bool v) x = M.basicSet v (fromBool x)
  basicUnsafeCopy (MV_Bool v1) (MV_Bool v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Bool v1) (MV_Bool v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Bool v) n = MV_Bool `liftM` M.basicUnsafeGrow v n

instance G.Vector Vector Bool where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Bool v) = V_Bool `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Bool v) = MV_Bool `liftM` G.basicUnsafeThaw v
  basicLength (V_Bool v) = G.basicLength v
  basicUnsafeSlice i n (V_Bool v) = V_Bool $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Bool v) i = toBool `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Bool mv) (V_Bool v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

-- -------
-- Complex
-- -------

newtype instance MVector s (Complex a) = MV_Complex (MVector s (a,a))
newtype instance Vector    (Complex a) = V_Complex  (Vector    (a,a))

instance (Unbox a) => Unbox (Complex a)

instance (Unbox a) => M.MVector MVector (Complex a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Complex v) = M.basicLength v
  basicUnsafeSlice i n (MV_Complex v) = MV_Complex $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Complex v1) (MV_Complex v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Complex `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Complex v) = M.basicInitialize v
  basicUnsafeReplicate n (x :+ y) = MV_Complex `liftM` M.basicUnsafeReplicate n (x,y)
  basicUnsafeRead (MV_Complex v) i = uncurry (:+) `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Complex v) i (x :+ y) = M.basicUnsafeWrite v i (x,y)
  basicClear (MV_Complex v) = M.basicClear v
  basicSet (MV_Complex v) (x :+ y) = M.basicSet v (x,y)
  basicUnsafeCopy (MV_Complex v1) (MV_Complex v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Complex v1) (MV_Complex v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Complex v) n = MV_Complex `liftM` M.basicUnsafeGrow v n

instance (Unbox a) => G.Vector Vector (Complex a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Complex v) = V_Complex `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Complex v) = MV_Complex `liftM` G.basicUnsafeThaw v
  basicLength (V_Complex v) = G.basicLength v
  basicUnsafeSlice i n (V_Complex v) = V_Complex $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Complex v) i
                = uncurry (:+) `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Complex mv) (V_Complex v)
                = G.basicUnsafeCopy mv v
  elemseq _ (x :+ y) z = G.elemseq (undefined :: Vector a) x
                       $ G.elemseq (undefined :: Vector a) y z

-- -------
-- Identity
-- -------
#define newtypeMVector(inst_ctxt,inst_head,tyC,con) \
instance inst_ctxt => M.MVector MVector (inst_head) where { \
; {-# INLINE basicLength          #-}                                         \
; {-# INLINE basicUnsafeSlice     #-}                                         \
; {-# INLINE basicOverlaps        #-}                                         \
; {-# INLINE basicUnsafeNew       #-}                                         \
; {-# INLINE basicInitialize      #-}                                         \
; {-# INLINE basicUnsafeReplicate #-}                                         \
; {-# INLINE basicUnsafeRead      #-}                                         \
; {-# INLINE basicUnsafeWrite     #-}                                         \
; {-# INLINE basicClear           #-}                                         \
; {-# INLINE basicSet             #-}                                         \
; {-# INLINE basicUnsafeCopy      #-}                                         \
; {-# INLINE basicUnsafeGrow      #-}                                         \
; basicLength (con v)                = M.basicLength v                        \
; basicUnsafeSlice i n (con v)       = con $ M.basicUnsafeSlice i n v         \
; basicOverlaps (con v1) (con v2)    = M.basicOverlaps v1 v2                  \
; basicUnsafeNew n                   = con `liftM` M.basicUnsafeNew n         \
; basicInitialize (con v)            = M.basicInitialize v                    \
; basicUnsafeReplicate n (tyC x)     = con `liftM` M.basicUnsafeReplicate n x \
; basicUnsafeRead (con v) i          = tyC `liftM` M.basicUnsafeRead v i      \
; basicUnsafeWrite (con v) i (tyC x) = M.basicUnsafeWrite v i x               \
; basicClear (con v)                 = M.basicClear v                         \
; basicSet (con v) (tyC x)           = M.basicSet v x                         \
; basicUnsafeCopy (con v1) (con v2)  = M.basicUnsafeCopy v1 v2                \
; basicUnsafeMove (con v1) (con v2)  = M.basicUnsafeMove v1 v2                \
; basicUnsafeGrow (con v) n          = con `liftM` M.basicUnsafeGrow v n      \
}
#define newtypeVector(inst_ctxt,inst_head,tyC,con,mcon) \
instance inst_ctxt => G.Vector Vector (inst_head) where { \
; {-# INLINE basicUnsafeFreeze  #-}                                       \
; {-# INLINE basicUnsafeThaw    #-}                                       \
; {-# INLINE basicLength        #-}                                       \
; {-# INLINE basicUnsafeSlice   #-}                                       \
; {-# INLINE basicUnsafeIndexM  #-}                                       \
; {-# INLINE elemseq            #-}                                       \
; basicUnsafeFreeze (mcon v)        = con `liftM` G.basicUnsafeFreeze v   \
; basicUnsafeThaw (con v)           = mcon `liftM` G.basicUnsafeThaw v    \
; basicLength (con v)               = G.basicLength v                     \
; basicUnsafeSlice i n (con v)      = con $ G.basicUnsafeSlice i n v      \
; basicUnsafeIndexM (con v) i       = tyC `liftM` G.basicUnsafeIndexM v i \
; basicUnsafeCopy (mcon mv) (con v) = G.basicUnsafeCopy mv v              \
; elemseq _ (tyC a)                 = G.elemseq (undefined :: Vector a) a \
}
#define deriveNewtypeInstances(inst_ctxt,inst_head,rep,tyC,con,mcon) \
newtype instance MVector s (inst_head) = mcon (MVector s (rep)) ;\
newtype instance Vector    (inst_head) = con  (Vector (rep))    ;\
instance inst_ctxt => Unbox (inst_head)                         ;\
newtypeMVector(inst_ctxt, inst_head, tyC, mcon)                 ;\
newtypeVector(inst_ctxt,  inst_head, tyC, con, mcon)

#if MIN_VERSION_base(4,8,0)
deriveNewtypeInstances(Unbox a, Identity a, a, Identity, V_Identity, MV_Identity)
#endif

deriveNewtypeInstances(Unbox a, Down a,    a, Down,    V_Down,    MV_Down)
deriveNewtypeInstances(Unbox a, Dual a,    a, Dual,    V_Dual,    MV_Dual)
deriveNewtypeInstances(Unbox a, Sum a,     a, Sum,     V_Sum,     MV_Sum)
deriveNewtypeInstances(Unbox a, Product a, a, Product, V_Product, MV_Product)


-- --------------
-- Data.Semigroup
-- --------------

#if MIN_VERSION_base(4,9,0)
deriveNewtypeInstances(Unbox a, Min a,   a, Min,   V_Min,   MV_Min)
deriveNewtypeInstances(Unbox a, Max a,   a, Max,   V_Max,   MV_Max)
deriveNewtypeInstances(Unbox a, First a, a, First, V_First, MV_First)
deriveNewtypeInstances(Unbox a, Last a,  a, Last,  V_Last,  MV_Last)
deriveNewtypeInstances(Unbox a, WrappedMonoid a, a, WrapMonoid, V_WrappedMonoid, MV_WrappedMonoid)

-- ------------------
-- Data.Semigroup.Arg
-- ------------------

newtype instance MVector s (Arg a b) = MV_Arg (MVector s (a,b))
newtype instance Vector    (Arg a b) = V_Arg  (Vector    (a,b))

instance (Unbox a, Unbox b) => Unbox (Arg a b)

instance (Unbox a, Unbox b) => M.MVector MVector (Arg a b) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Arg v)                  = M.basicLength v
  basicUnsafeSlice i n (MV_Arg v)         = MV_Arg $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Arg v1) (MV_Arg v2)   = M.basicOverlaps v1 v2
  basicUnsafeNew n                        = MV_Arg `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Arg v)              = M.basicInitialize v
  basicUnsafeReplicate n (Arg x y)        = MV_Arg `liftM` M.basicUnsafeReplicate n (x,y)
  basicUnsafeRead (MV_Arg v) i            = uncurry Arg `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Arg v) i (Arg x y) = M.basicUnsafeWrite v i (x,y)
  basicClear (MV_Arg v)                   = M.basicClear v
  basicSet (MV_Arg v) (Arg x y)           = M.basicSet v (x,y)
  basicUnsafeCopy (MV_Arg v1) (MV_Arg v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Arg v1) (MV_Arg v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Arg v) n            = MV_Arg `liftM` M.basicUnsafeGrow v n

instance (Unbox a, Unbox b) => G.Vector Vector (Arg a b) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Arg v)   = V_Arg `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Arg v)      = MV_Arg `liftM` G.basicUnsafeThaw v
  basicLength (V_Arg v)          = G.basicLength v
  basicUnsafeSlice i n (V_Arg v) = V_Arg $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Arg v) i  = uncurry Arg `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Arg mv) (V_Arg v)
                                 = G.basicUnsafeCopy mv v
  elemseq _ (Arg x y) z          = G.elemseq (undefined :: Vector a) x
                                 $ G.elemseq (undefined :: Vector b) y z
#endif

deriveNewtypeInstances((), Any, Bool, Any, V_Any, MV_Any)
deriveNewtypeInstances((), All, Bool, All, V_All, MV_All)

-- -------
-- Const
-- -------

deriveNewtypeInstances(Unbox a, Const a b, a, Const, V_Const, MV_Const)

-- ---
-- Alt
-- ---

#if MIN_VERSION_base(4,8,0)
deriveNewtypeInstances(Unbox (f a), Alt f a, f a, Alt, V_Alt, MV_Alt)
#endif

-- -------
-- Compose
-- -------

#if MIN_VERSION_base(4,9,0)
deriveNewtypeInstances(Unbox (f (g a)), Compose f g a, f (g a), Compose, V_Compose, MV_Compose)
#endif

-- ------
-- Tuples
-- ------

#define DEFINE_INSTANCES
#include "unbox-tuple-instances"
