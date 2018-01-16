{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | PRepr instance for tuples
--   and PD wrappers for other functions defined in D.A.P.PArray.PData.Tuple.
module Data.Array.Parallel.PArray.PRepr.Tuple
        ( PRepr
        , ziplPA)
where
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Tuple2
import Data.Array.Parallel.PArray.PData.Tuple3
import Data.Array.Parallel.PArray.PData.Tuple4
import Data.Array.Parallel.PArray.PData.Tuple5
import Data.Array.Parallel.PArray.PData.Tuple6
import Data.Array.Parallel.PArray.PData.Tuple7
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.PArray.PData.Wrap


-- Tuple2 --------------------------------------------------------------------
type instance PRepr (a, b)
        = (Wrap a, Wrap b)

instance (PA a, PA b) => PA (a, b) where
  {-# INLINE_PA toPRepr #-}
  toPRepr (a, b)
        = (Wrap a, Wrap b)

  {-# INLINE_PA fromPRepr #-}
  fromPRepr (Wrap a, Wrap b)
        = (a, b)

  {-# INLINE_PA toArrPRepr #-}
  toArrPRepr (PTuple2 as bs)
        = PTuple2 (PWrap as) (PWrap bs)

  {-# INLINE_PA fromArrPRepr #-}
  fromArrPRepr (PTuple2 (PWrap as) (PWrap bs))
        = PTuple2 as bs

  {-# INLINE_PA toArrPReprs #-}
  toArrPReprs (PTuple2s as bs)
        = PTuple2s (PWraps as) (PWraps bs)

  {-# INLINE_PA fromArrPReprs #-}
  fromArrPReprs (PTuple2s (PWraps as) (PWraps bs))
        = PTuple2s as bs


-- | Lifted zip on PData arrays.
ziplPA  :: (PA a, PA b) 
        => PData (PArray a) -> PData (PArray b) -> PData (PArray (a, b))
ziplPA xs ys
 = let  
        -- TODO: can we use the flat version here?
        PNested vsegd (PTuple2s xs' ys') segd _
         = ziplPR (toNestedArrPRepr xs) (toNestedArrPRepr ys)

        pdatas  = PTuple2s (fromArrPReprs xs') (fromArrPReprs ys')
        flat    = fromArrPRepr $ extractvs_delay (toArrPReprs pdatas) vsegd

   in   PNested vsegd pdatas segd flat
                

-- Tuple3 --------------------------------------------------------------------
type instance PRepr (a, b, c)
        = (Wrap a, Wrap b, Wrap c)

instance (PA a, PA b, PA c) => PA (a, b, c) where
  {-# INLINE_PA toPRepr #-}
  toPRepr (a, b, c)
        = (Wrap a, Wrap b, Wrap c)

  {-# INLINE_PA fromPRepr #-}
  fromPRepr (Wrap a, Wrap b, Wrap c)
        = (a, b, c)

  {-# INLINE_PA toArrPRepr #-}
  toArrPRepr (PTuple3 as bs cs)
        = PTuple3 (PWrap as) (PWrap bs) (PWrap cs)

  {-# INLINE_PA fromArrPRepr #-}
  fromArrPRepr (PTuple3 (PWrap as) (PWrap bs) (PWrap cs))
        = PTuple3 as bs cs

  {-# INLINE_PA toArrPReprs #-}
  toArrPReprs (PTuple3s as bs cs)
        = PTuple3s (PWraps as) (PWraps bs) (PWraps cs)

  {-# INLINE_PA fromArrPReprs #-}
  fromArrPReprs (PTuple3s (PWraps as) (PWraps bs) (PWraps cs))
        = PTuple3s as bs cs


-- Tuple4 --------------------------------------------------------------------
type instance PRepr (a, b, c, d)
        = (Wrap a, Wrap b, Wrap c, Wrap d)

instance (PA a, PA b, PA c, PA d) => PA (a, b, c, d) where
  {-# INLINE_PA toPRepr #-}
  toPRepr (a, b, c, d)
        = (Wrap a, Wrap b, Wrap c, Wrap d)

  {-# INLINE_PA fromPRepr #-}
  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d)
        = (a, b, c, d)

  {-# INLINE_PA toArrPRepr #-}
  toArrPRepr (PTuple4 as bs cs ds)
        = PTuple4 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds)

  {-# INLINE_PA fromArrPRepr #-}
  fromArrPRepr (PTuple4 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds))
        = PTuple4 as bs cs ds

  {-# INLINE_PA toArrPReprs #-}
  toArrPReprs (PTuple4s as bs cs ds)
        = PTuple4s (PWraps as) (PWraps bs) (PWraps cs) (PWraps ds)

  {-# INLINE_PA fromArrPReprs #-}
  fromArrPReprs (PTuple4s (PWraps as) (PWraps bs) (PWraps cs) (PWraps ds))
        = PTuple4s as bs cs ds


-- Tuple5 --------------------------------------------------------------------
type instance PRepr (a, b, c, d, e)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e)

instance (PA a, PA b, PA c, PA d, PA e) => PA (a, b, c, d, e) where
  {-# INLINE_PA toPRepr #-}
  toPRepr (a, b, c, d, e)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e)

  {-# INLINE_PA fromPRepr #-}
  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e)
        = (a, b, c, d, e)

  {-# INLINE_PA toArrPRepr #-}
  toArrPRepr (PTuple5 as bs cs ds es)
        = PTuple5 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es)

  {-# INLINE_PA fromArrPRepr #-}
  fromArrPRepr (PTuple5 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es))
        = PTuple5 as bs cs ds es

  {-# INLINE_PA toArrPReprs #-}
  toArrPReprs (PTuple5s as bs cs ds es)
        = PTuple5s (PWraps as) (PWraps bs) (PWraps cs) (PWraps ds) (PWraps es)

  {-# INLINE_PA fromArrPReprs #-}
  fromArrPReprs (PTuple5s (PWraps as) (PWraps bs) (PWraps cs) (PWraps ds) (PWraps es))
        = PTuple5s as bs cs ds es


-- Tuple6 --------------------------------------------------------------------
type instance PRepr (a, b, c, d, e, f)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f)

instance (PA a, PA b, PA c, PA d, PA e, PA f) => PA (a, b, c, d, e, f) where
  {-# INLINE_PA toPRepr #-}
  toPRepr (a, b, c, d, e, f)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f)

  {-# INLINE_PA fromPRepr #-}
  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f)
        = (a, b, c, d, e, f)

  {-# INLINE_PA toArrPRepr #-}
  toArrPRepr (PTuple6 as bs cs ds es fs)
        = PTuple6 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs)

  {-# INLINE_PA fromArrPRepr #-}
  fromArrPRepr (PTuple6 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs))
        = PTuple6 as bs cs ds es fs

  {-# INLINE_PA toArrPReprs #-}
  toArrPReprs (PTuple6s as bs cs ds es fs)
        = PTuple6s (PWraps as) (PWraps bs) (PWraps cs) (PWraps ds) (PWraps es) (PWraps fs) 

  {-# INLINE_PA fromArrPReprs #-}
  fromArrPReprs (PTuple6s (PWraps as) (PWraps bs) (PWraps cs) (PWraps ds) (PWraps es) (PWraps fs))
        = PTuple6s as bs cs ds es fs

-- Tuple7 --------------------------------------------------------------------
type instance PRepr (a, b, c, d, e, f, g)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g)

instance (PA a, PA b, PA c, PA d, PA e, PA f, PA g) => PA (a, b, c, d, e, f, g) where
  {-# INLINE_PA toPRepr #-}
  toPRepr (a, b, c, d, e, f, g)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g)

  {-# INLINE_PA fromPRepr #-}
  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e, Wrap f, Wrap g)
        = (a, b, c, d, e, f, g)

  {-# INLINE_PA toArrPRepr #-}
  toArrPRepr (PTuple7 as bs cs ds es fs gs)
        = PTuple7 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs)

  {-# INLINE_PA fromArrPRepr #-}
  fromArrPRepr (PTuple7 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) (PWrap fs) (PWrap gs))
        = PTuple7 as bs cs ds es fs gs

  {-# INLINE_PA toArrPReprs #-}
  toArrPReprs (PTuple7s as bs cs ds es fs gs)
        = PTuple7s (PWraps as) (PWraps bs) (PWraps cs) (PWraps ds) (PWraps es) (PWraps fs)  (PWraps gs)

  {-# INLINE_PA fromArrPReprs #-}
  fromArrPReprs (PTuple7s (PWraps as) (PWraps bs) (PWraps cs) (PWraps ds) (PWraps es) (PWraps fs) (PWraps gs))
        = PTuple7s as bs cs ds es fs gs
