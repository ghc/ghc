{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | PRepr/PA instance for nested arrays, 
--   and PA wrappers for other functions defined in D.A.P.PArray.PData.Nested.
module Data.Array.Parallel.PArray.PRepr.Nested
        ( mkPNestedPA
        , concatPA,  concatlPA
        , unconcatPA
        , appendlPA
        , indexlPA
        , slicelPA)
where
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V


-- PArray ---------------------------------------------------------------------
type instance PRepr (PArray a)
        = PArray (PRepr a)

instance PA a => PA (PArray a) where
  {-# INLINE_PA toPRepr #-}
  toPRepr (PArray n xs) 
        = PArray n $ toArrPRepr xs

  {-# INLINE_PA fromPRepr #-}
  fromPRepr (PArray n xs)
        = PArray n $ fromArrPRepr xs

  {-# INLINE_PA toArrPRepr #-}
  toArrPRepr (PNested vsegd xs segd flat)
        = PNested vsegd (toArrPReprs xs) segd (toArrPRepr flat)

  {-# INLINE_PA fromArrPRepr #-}
  fromArrPRepr (PNested vsegd xs segd flat)
        = PNested vsegd (fromArrPReprs xs) segd (fromArrPRepr flat)

  {-# INLINE_PA toArrPReprs #-}
  toArrPReprs (PNesteds vec)
        = PNesteds $ V.map toArrPRepr vec

  {-# INLINE_PA fromArrPReprs #-}
  fromArrPReprs (PNesteds vec)
        = PNesteds $ V.map fromArrPRepr vec


-- PA Wrappers ----------------------------------------------------------------
-- These wrappers have the same types in the ones in D.A.P.PArray.PData.Nested,
-- except that they take a PA dictionary instead of a PR dictionary.
--
-- See D.A.P.PArray.PRepr.Base   for docs on why we need the wrappers.
-- See D.A.P.PArray.PData.Nested for docs on what the PR versions do.
--
-- | Conatruct a nested array.
mkPNestedPA 
        :: PA a
        => U.VSegd -> PDatas a
        -> U.Segd  -> PData a
        -> PData (PArray a)

mkPNestedPA vsegd pdatas segd pdata
 = let  pdatas' = toArrPReprs pdatas
        pdata'  = toArrPRepr  pdata
   in   fromArrPRepr $ mkPNested vsegd pdatas' segd pdata'


concatPA        :: PA a => PData (PArray a) -> PData a
concatPA arr
 = fromArrPRepr $ concatPR $ toArrPRepr arr
{-# INLINE_PA concatPA #-}
 
 
unconcatPA      :: (PA a, PA b) => PData (PArray a) -> PData b -> PData (PArray b)
unconcatPA arr1 arr2
 = fromArrPRepr $ unconcatPR (toArrPRepr arr1) (toArrPRepr arr2)
{-# INLINE_PA unconcatPA #-}


concatlPA       :: PA a => PData (PArray (PArray a)) -> PData (PArray a)
concatlPA arr
 = fromArrPRepr $ concatlPR (toArrPRepr arr)
{-# INLINE_PA concatlPA #-}


appendlPA       :: PA a => PData (PArray a) -> PData (PArray a) -> PData (PArray a)
appendlPA arr1 arr2
 = fromArrPRepr $ appendlPR (toArrPRepr arr1) (toArrPRepr arr2)
{-# INLINE_PA appendlPA #-}


indexlPA        :: PA a => PData (PArray a) -> PData Int -> PData a
indexlPA arr ixs
 = fromArrPRepr $ indexlPR (toArrPRepr arr) ixs
{-# INLINE_PA indexlPA #-}


slicelPA        :: PA a => PData Int -> PData Int -> PData (PArray a) -> PData (PArray a)
slicelPA starts lens arr
 = fromArrPRepr $ slicelPR starts lens (toArrPRepr arr)
{-# INLINE_PA slicelPA #-}

