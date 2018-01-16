{-# OPTIONS_GHC -fvectorise #-}

module Data.Array.Parallel.Prelude.Bool 
        ( Bool(..)
        , P.otherwise
        , (P.&&), (P.||), P.not,  andP, orP
        , fromBool, toBool)
where
-- Primitives needed by the vectoriser.
import Data.Array.Parallel.Prim
import Data.Array.Parallel.PArr
import Data.Array.Parallel.Prelude.Base                 (Bool(..))
import Data.Array.Parallel.Prelude.Int as I             (sumP, (==), (/=))  -- just temporary
import Data.Array.Parallel.Lifted                       (mapPP, lengthPP)   -- just temporary
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.PArray.PData.Base
import qualified Data.Array.Parallel.Unlifted           as U
import Data.Bits
import qualified Prelude as P
import Prelude                                          (Int)


-- and ------------------------------------------------------------------------
{-# VECTORISE (P.&&) = (&&*) #-}

(&&*) :: Bool :-> Bool :-> Bool
(&&*) = closure2 (P.&&) and_l
{-# INLINE      (&&*) #-}
{-# NOVECTORISE (&&*) #-}

and_l :: PArray Bool -> PArray Bool -> PArray Bool
and_l (PArray n# bs) (PArray _ cs)
  = PArray n# P.$
      case bs of { PBool sel1 ->
      case cs of { PBool sel2 ->
      PBool P.$ U.tagsToSel2 (U.zipWith (.&.) (U.tagsSel2 sel1) (U.tagsSel2 sel2)) }}
{-# INLINE      and_l #-}
{-# NOVECTORISE and_l #-}


-- or -------------------------------------------------------------------------
{-# VECTORISE (P.||) = (||*) #-}

(||*) :: Bool :-> Bool :-> Bool
(||*) = closure2 (P.||) or_l
{-# INLINE (||*) #-}
{-# NOVECTORISE (||*) #-}

or_l :: PArray Bool -> PArray Bool -> PArray Bool
or_l (PArray n# bs) (PArray _ cs)
  = PArray n# P.$
      case bs of { PBool sel1 ->
      case cs of { PBool sel2 ->
      PBool P.$ U.tagsToSel2 (U.zipWith (.|.) (U.tagsSel2 sel1) (U.tagsSel2 sel2)) }}
{-# INLINE or_l #-}
{-# NOVECTORISE or_l #-}


-- not ------------------------------------------------------------------------
{-# VECTORISE P.not = notPP #-}

notPP :: Bool :-> Bool
notPP   = closure1 P.not notPP_l
{-# INLINE notPP #-}
{-# NOVECTORISE notPP #-}

notPP_l :: PArray Bool -> PArray Bool
notPP_l (PArray n# bs)
  = PArray n# P.$
      case bs of { PBool sel ->
      PBool P.$ U.tagsToSel2 (U.map negate (U.tagsSel2 sel)) }
 where
  -- We must return 1 for True, 0 for False
  negate i = (complement i) .&. 1
{-# NOVECTORISE notPP_l #-}
{-# INLINE notPP_l #-}


{- TODO: We can't do these because there is no Unboxes instance for Bool.
-- andP -----------------------------------------------------------------------
andP :: PArr Bool -> Bool
andP _ = True
{-# NOINLINE  andP #-}
{-# VECTORISE andP = andPP #-}

andPP :: PArray Bool :-> Bool
andPP  = L.closure1' (SC.fold (&&) True) (SC.folds (&&) True)
{-# INLINE      andPP #-}
{-# NOVECTORISE andPP #-}


-- orP ------------------------------------------------------------------------
orP :: PArr Bool -> Bool
orP _ = True
{-# NOINLINE  orP #-}
{-# VECTORISE orP = orPP #-}

orPP :: PArray Bool :-> Bool
orPP   = L.closure1' (SC.fold (||) False) (SC.folds (||) False)
{-# INLINE      orPP #-}
{-# NOVECTORISE orPP #-}
-}

-- Until we have Unboxes for Bool, we use the following definitions instead.

andP :: PArr Bool -> Bool
andP bs = I.sumP (mapP fromBool bs) I.== lengthP bs

orP :: PArr Bool -> Bool
orP bs = sumP (mapP fromBool bs) I./= 0

-- Defining 'mapP' and 'lengthP' here is just a kludge until the original definitions of
-- 'andP' and 'orP' work again.
mapP :: (a -> b) -> PArr a -> PArr b
mapP !_ !_              = emptyPArr
{-# NOINLINE  mapP #-}
{-# VECTORISE mapP      = mapPP #-}

lengthP :: PArr a -> Int
lengthP = lengthPArr
{-# NOINLINE  lengthP #-}
{-# VECTORISE lengthP   = lengthPP #-}


-- conversion functions --------------------------------------------------------

fromBool :: Bool -> Int
fromBool False = 0
fromBool True  = 1

toBool :: Int -> Bool
toBool 0 = False
toBool _ = True
