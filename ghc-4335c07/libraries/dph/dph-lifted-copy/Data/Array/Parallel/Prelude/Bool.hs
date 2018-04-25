{-# OPTIONS_GHC -fvectorise #-}

module Data.Array.Parallel.Prelude.Bool
  ( Bool(..)

  , otherwise, (&&), (||), not, andP, orP
  , fromBool, toBool
  )
where

import Data.Array.Parallel.Prim ()       -- dependency required by the vectoriser

import Data.Array.Parallel.Prelude.Base

import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.PArray.PReprInstances
import Data.Array.Parallel.Lifted.Scalar
import qualified Data.Array.Parallel.Unlifted as U

import Data.Bits


-- We re-export 'Prelude.otherwise' as is as it is special-cased in the Desugarer
  
{-# VECTORISE (&&) = (&&*) #-}
(&&*) :: Bool :-> Bool :-> Bool
{-# INLINE (&&*) #-}
(&&*) = closure2 (&&) and_l
{-# NOVECTORISE (&&*) #-}

and_l :: PArray Bool -> PArray Bool -> PArray Bool
{-# INLINE and_l #-}
and_l (PArray n# bs) (PArray _ cs)
  = PArray n# $
      case bs of { PBool sel1 ->
      case cs of { PBool sel2 ->
      PBool $ U.tagsToSel2 (U.zipWith (.&.) (U.tagsSel2 sel1) (U.tagsSel2 sel2)) }}
{-# NOVECTORISE and_l #-}

{-# VECTORISE (||) = (||*) #-}
(||*) :: Bool :-> Bool :-> Bool
{-# INLINE (||*) #-}
(||*) = closure2 (||) or_l
{-# NOVECTORISE (||*) #-}

or_l :: PArray Bool -> PArray Bool -> PArray Bool
{-# INLINE or_l #-}
or_l (PArray n# bs) (PArray _ cs)
  = PArray n# $
      case bs of { PBool sel1 ->
      case cs of { PBool sel2 ->
      PBool $ U.tagsToSel2 (U.zipWith (.|.) (U.tagsSel2 sel1) (U.tagsSel2 sel2)) }}
{-# NOVECTORISE or_l #-}

{-# VECTORISE not = not_v #-}
not_v :: Bool :-> Bool
{-# INLINE not_v #-}
not_v = closure1 not not_l
{-# NOVECTORISE not_v #-}

not_l :: PArray Bool -> PArray Bool
{-# INLINE not_l #-}
not_l (PArray n# bs)
  = PArray n# $
      case bs of { PBool sel ->
      PBool $ U.tagsToSel2 (U.map complement (U.tagsSel2 sel)) }
{-# NOVECTORISE not_l #-}

andP:: PArr Bool -> Bool
{-# NOINLINE andP #-}
andP _ = True
{-# VECTORISE andP = andP_v #-}
andP_v :: PArray Bool :-> Bool
{-# INLINE andP_v #-}
andP_v = closure1 (scalar_fold (&&) True) (scalar_folds (&&) True)
{-# NOVECTORISE andP_v #-}

orP:: PArr Bool -> Bool
{-# NOINLINE orP #-}
orP _ = True
{-# VECTORISE orP = orP_v #-}
orP_v :: PArray Bool :-> Bool
{-# INLINE orP_v #-}
orP_v = closure1 (scalar_fold (||) False) (scalar_folds (||) False)
{-# NOVECTORISE orP_v #-}

fromBool :: Bool -> Int
fromBool False = 0
fromBool True  = 1

toBool :: Int -> Bool
toBool 0 = False
toBool _ = True
