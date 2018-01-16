{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-orphans -fno-warn-missing-methods #-}

#include "fusion-phases.h"

-- | Instances for the PRepr class.
--
--   For primitive types these are all trivial, as we represent an array of
--   Ints just as an array of Ints. 
--
--   For algebraic data types defined in the source program, the vectoriser
--   creates the appropriate PRepr instances for those types.
--
--   Note that polymorphic container types like tuples and arrays use the 
--   `Wrap` constructor so we only need to convert one layer of the structure
--   to the generic representation at a time. 
--   See "Data.Array.Parallel.PArray.Types" for details.
--
module Data.Array.Parallel.PArray.PReprInstances where
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PDataInstances
import Data.Array.Parallel.PArray.Base
import Data.Array.Parallel.PArray.ScalarInstances ()
import Data.Array.Parallel.PArray.Types
import qualified Data.Array.Parallel.Unlifted as U
import GHC.Word    ( Word8 )


-- Void -----------------------------------------------------------------------
type instance PRepr Void = Void

instance PA Void where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id


-- Unit -----------------------------------------------------------------------
type instance PRepr () = ()

instance PA () where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id


-- Int ------------------------------------------------------------------------
type instance PRepr Int = Int

instance PA Int where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id



-- Word8 ----------------------------------------------------------------------
type instance PRepr Word8 = Word8

instance PA Word8 where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id


-- Float ----------------------------------------------------------------------
type instance PRepr Float = Float

instance PA Float where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id


-- Double ---------------------------------------------------------------------
type instance PRepr Double = Double

instance PA Double where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id


-- Bool -----------------------------------------------------------------------
data instance PData Bool
  = PBool U.Sel2

type instance PRepr Bool = Sum2 Void Void

instance PA Bool where
  {-# INLINE toPRepr #-}
  toPRepr False         = Alt2_1 void
  toPRepr True          = Alt2_2 void

  {-# INLINE fromPRepr #-}
  fromPRepr (Alt2_1 _)  = False
  fromPRepr (Alt2_2 _)  = True

  {-# INLINE toArrPRepr #-}
  toArrPRepr (PBool sel) = PSum2 sel pvoid pvoid

  {-# INLINE fromArrPRepr #-}
  fromArrPRepr (PSum2 sel _ _) = PBool sel


-- Tuple2 ---------------------------------------------------------------------
type instance PRepr (a,b)
        = (Wrap a, Wrap b)

instance (PA a, PA b) => PA (a,b) where
  toPRepr (a, b)
        = (Wrap a, Wrap b)

  fromPRepr (Wrap a, Wrap b)
        = (a, b)

  toArrPRepr   (P_2 as bs)
        = P_2 (PWrap as) (PWrap bs)

  fromArrPRepr (P_2 (PWrap as) (PWrap bs))
        = P_2 as bs


-- Tuple3 ---------------------------------------------------------------------
type instance PRepr (a,b,c) 
        = (Wrap a, Wrap b, Wrap c)

instance (PA a, PA b, PA c) => PA (a,b,c) where
  toPRepr (a, b, c)                       
        = (Wrap a, Wrap b, Wrap c)

  fromPRepr (Wrap a, Wrap b, Wrap c)
        = (a, b, c)

  toArrPRepr (P_3 as bs cs)
        = P_3 (PWrap as) (PWrap bs) (PWrap cs)

  fromArrPRepr (P_3 (PWrap as) (PWrap bs) (PWrap cs))
        = P_3 as bs cs


-- Tuple4 ---------------------------------------------------------------------
type instance PRepr (a,b,c,d)
        = (Wrap a, Wrap b, Wrap c, Wrap d)

instance (PA a, PA b, PA c, PA d) => PA (a,b,c,d) where
  toPRepr (a, b, c, d)
        = (Wrap a, Wrap b, Wrap c, Wrap d)

  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d)
        = (a, b, c, d)

  toArrPRepr (P_4 as bs cs ds)
        = P_4 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds)

  fromArrPRepr (P_4 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds))
        = P_4 as bs cs ds


-- Tuple5 ---------------------------------------------------------------------
type instance PRepr (a,b,c,d,e)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e)

instance (PA a, PA b, PA c, PA d, PA e) => PA (a,b,c,d,e) where
  toPRepr (a, b, c, d, e)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e)

  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e)
        = (a, b, c, d, e)

  toArrPRepr (P_5 as bs cs ds es)
        = P_5 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) 

  fromArrPRepr (P_5 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es))
        = P_5 as bs cs ds es


-- Tuple6 ---------------------------------------------------------------------
type instance PRepr (a,b,c,d,e,f)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f)

instance (PA a, PA b, PA c, PA d, PA e, PA f) => PA (a,b,c,d,e,f) where
  toPRepr (a, b, c, d, e, f)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f)

  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f)
        = (a, b, c, d, e, f)

  toArrPRepr (P_6 as bs cs ds es fs)
        = P_6 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) 

  fromArrPRepr (P_6 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs))
        = P_6 as bs cs ds es fs


-- Tuple7 ---------------------------------------------------------------------
type instance PRepr (a,b,c,d,e,f,g)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g)

instance (PA a, PA b, PA c, PA d, PA e, PA f, PA g) => PA (a,b,c,d,e,f,g) where
  toPRepr (a, b, c, d, e, f, g)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g)

  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g)
        = (a, b, c, d, e, f, g)

  toArrPRepr (P_7 as bs cs ds es fs gs)
        = P_7 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)

  fromArrPRepr (P_7 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs))
        = P_7 as bs cs ds es fs gs


-- Tuple8 ---------------------------------------------------------------------
type instance PRepr (a,b,c,d,e,f,g,h)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h)

instance (PA a, PA b, PA c, PA d, PA e, PA f, PA g, PA h) => PA (a,b,c,d,e,f,g,h) where
  toPRepr (a, b, c, d, e, f, g, h)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h)

  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h)
        = (a, b, c, d, e, f, g, h)

  toArrPRepr (P_8 as bs cs ds es fs gs hs)
        = P_8 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
              (PWrap hs)

  fromArrPRepr (P_8 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
                    (PWrap hs))
        = P_8 as bs cs ds es fs gs hs


-- Tuple9 ---------------------------------------------------------------------
type instance PRepr (a,b,c,d,e,f,g,h,i)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i)

instance (PA a, PA b, PA c, PA d, PA e, PA f, PA g, PA h, PA i) => PA (a,b,c,d,e,f,g,h,i) where
  toPRepr (a, b, c, d, e, f, g, h, i)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i)

  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i)
        = (a, b, c, d, e, f, g, h, i)

  toArrPRepr (P_9 as bs cs ds es fs gs hs is)
        = P_9 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
              (PWrap hs) (PWrap is)

  fromArrPRepr (P_9 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
                    (PWrap hs) (PWrap is))
        = P_9 as bs cs ds es fs gs hs is


-- Tuple10 ---------------------------------------------------------------------
type instance PRepr (a,b,c,d,e,f,g,h,i,j)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j)

instance (PA a, PA b, PA c, PA d, PA e, PA f, PA g, PA h, PA i, PA j) 
  => PA (a,b,c,d,e,f,g,h,i,j) where
  toPRepr (a, b, c, d, e, f, g, h, i, j)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j)

  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j)
        = (a, b, c, d, e, f, g, h, i, j)

  toArrPRepr (P_10 as bs cs ds es fs gs hs is js)
        = P_10 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
               (PWrap hs) (PWrap is) (PWrap js)

  fromArrPRepr (P_10 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
                     (PWrap hs) (PWrap is) (PWrap js))
        = P_10 as bs cs ds es fs gs hs is js


-- Tuple11 ---------------------------------------------------------------------
type instance PRepr (a,b,c,d,e,f,g,h,i,j,k)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k)

instance (PA a, PA b, PA c, PA d, PA e, PA f, PA g, PA h, PA i, PA j, PA k) 
  => PA (a,b,c,d,e,f,g,h,i,j,k) where
  toPRepr (a, b, c, d, e, f, g, h, i, j, k)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k)

  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k)
        = (a, b, c, d, e, f, g, h, i, j, k)

  toArrPRepr (P_11 as bs cs ds es fs gs hs is js ks)
        = P_11 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
               (PWrap hs) (PWrap is) (PWrap js) (PWrap ks)

  fromArrPRepr (P_11 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
                     (PWrap hs) (PWrap is) (PWrap js) (PWrap ks))
        = P_11 as bs cs ds es fs gs hs is js ks


-- Tuple12 ---------------------------------------------------------------------
type instance PRepr (a,b,c,d,e,f,g,h,i,j,k,l)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k,
           Wrap l)

instance (PA a, PA b, PA c, PA d, PA e, PA f, PA g, PA h, PA i, PA j, PA k, PA l) 
  => PA (a,b,c,d,e,f,g,h,i,j,k,l) where
  toPRepr (a, b, c, d, e, f, g, h, i, j, k, l)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k,
           Wrap l)

  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k,
             Wrap l)
        = (a, b, c, d, e, f, g, h, i, j, k, l)

  toArrPRepr (P_12 as bs cs ds es fs gs hs is js ks ls)
        = P_12 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
               (PWrap hs) (PWrap is) (PWrap js) (PWrap ks) (PWrap ls)

  fromArrPRepr (P_12 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
                     (PWrap hs) (PWrap is) (PWrap js) (PWrap ks) (PWrap ls))
        = P_12 as bs cs ds es fs gs hs is js ks ls


-- Tuple13 ---------------------------------------------------------------------
type instance PRepr (a,b,c,d,e,f,g,h,i,j,k,l,m)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k,
           Wrap l, Wrap m)

instance (PA a, PA b, PA c, PA d, PA e, PA f, PA g, PA h, PA i, PA j, PA k, PA l, PA m) 
  => PA (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  toPRepr (a, b, c, d, e, f, g, h, i, j, k, l, m)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k,
           Wrap l, Wrap m)

  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k,
             Wrap l, Wrap m)
        = (a, b, c, d, e, f, g, h, i, j, k, l, m)

  toArrPRepr (P_13 as bs cs ds es fs gs hs is js ks ls ms)
        = P_13 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
               (PWrap hs) (PWrap is) (PWrap js) (PWrap ks) (PWrap ls) (PWrap ms)

  fromArrPRepr (P_13 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
                     (PWrap hs) (PWrap is) (PWrap js) (PWrap ks) (PWrap ls) (PWrap ms))
        = P_13 as bs cs ds es fs gs hs is js ks ls ms


-- Tuple14 ---------------------------------------------------------------------
type instance PRepr (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k,
           Wrap l, Wrap m, Wrap n)

instance (PA a, PA b, PA c, PA d, PA e, PA f, PA g, PA h, PA i, PA j, PA k, PA l, PA m, PA n) 
  => PA (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  toPRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k,
           Wrap l, Wrap m, Wrap n)

  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k,
             Wrap l, Wrap m, Wrap n)
        = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

  toArrPRepr (P_14 as bs cs ds es fs gs hs is js ks ls ms ns)
        = P_14 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
               (PWrap hs) (PWrap is) (PWrap js) (PWrap ks) (PWrap ls) (PWrap ms) (PWrap ns)

  fromArrPRepr (P_14 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
                     (PWrap hs) (PWrap is) (PWrap js) (PWrap ks) (PWrap ls) (PWrap ms) (PWrap ns))
        = P_14 as bs cs ds es fs gs hs is js ks ls ms ns


-- Tuple15 ---------------------------------------------------------------------
type instance PRepr (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k,
           Wrap l, Wrap m, Wrap n, Wrap o)

instance (PA a, PA b, PA c, PA d, PA e, PA f, PA g, PA h, PA i, PA j, PA k, PA l, PA m, PA n, PA o) 
  => PA (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  toPRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k,
           Wrap l, Wrap m, Wrap n, Wrap o)

  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g, Wrap h, Wrap i, Wrap j, Wrap k,
             Wrap l, Wrap m, Wrap n, Wrap o)
        = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

  toArrPRepr (P_15 as bs cs ds es fs gs hs is js ks ls ms ns os)
        = P_15 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
               (PWrap hs) (PWrap is) (PWrap js) (PWrap ks) (PWrap ls) (PWrap ms) (PWrap ns)
               (PWrap os)

  fromArrPRepr (P_15 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)
                     (PWrap hs) (PWrap is) (PWrap js) (PWrap ks) (PWrap ls) (PWrap ms) (PWrap ns)
                     (PWrap os))
        = P_15 as bs cs ds es fs gs hs is js ks ls ms ns os


-- PArray ---------------------------------------------------------------------
type instance PRepr (PArray a)
        = PArray (PRepr a)

instance PA a => PA (PArray a) where
  {-# INLINE toPRepr #-}
  toPRepr (PArray n# xs) 
        = PArray n# (toArrPRepr xs)

  {-# INLINE fromPRepr #-}
  fromPRepr (PArray n# xs)
        = PArray n# (fromArrPRepr xs)

  {-# INLINE toArrPRepr #-}
  toArrPRepr (PNested segd xs)
        = PNested segd (toArrPRepr xs)

  {-# INLINE fromArrPRepr #-}
  fromArrPRepr (PNested segd xs)
        = PNested segd (fromArrPRepr xs)

