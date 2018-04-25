{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | PR instance for tuples.
module Data.Array.Parallel.PArray.PData.Tuple3
        ( PData(..),    PDatas(..)
        , zip3PD)
where
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import GHC.Exts
import Prelude hiding (zip, unzip)
import qualified Data.Typeable                  as T
import qualified Data.Vector                    as V
import qualified Prelude                        as P

-------------------------------------------------------------------------------
data instance PData (a, b, c)
        = PTuple3  (PData a)  (PData b)  (PData c)

data instance PDatas (a, b, c)
        = PTuple3s (PDatas a) (PDatas b) (PDatas c)


-- PR -------------------------------------------------------------------------
instance (PR a, PR b, PR c) => PR (a, b, c) where

  {-# NOINLINE validPR #-}
  validPR (PTuple3 xs ys zs)
        = validPR xs && validPR ys && validPR zs


  {-# NOINLINE nfPR #-}
  nfPR (PTuple3 arr1 arr2 arr3)
        = nfPR arr1 `seq` nfPR arr2 `seq` nfPR arr3 `seq` ()


  {-# NOINLINE similarPR #-}
  similarPR (x1, y1, z1) (x2, y2, z2)
        =  similarPR x1 x2
        && similarPR y1 y2
        && similarPR z1 z2


  {-# NOINLINE coversPR #-}
  coversPR weak (PTuple3 arr1 arr2 arr3) ix
        =  coversPR weak arr1 ix
        && coversPR weak arr2 ix
        && coversPR weak arr3 ix

  {-# NOINLINE pprpPR #-}
  pprpPR (x, y, z)
        = text "Tuple3 "
        <> vcat [ pprpPR x
                , pprpPR y
                , pprpPR z]
        

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PTuple3 xs ys zs)
        = text "PTuple3 " 
        <> vcat [ pprpDataPR xs
                , pprpDataPR ys
                , pprpDataPR zs]

  {-# NOINLINE typeRepPR #-}
  typeRepPR x@(a, b, c)
        = T.typeOf3 x 
                `T.mkAppTy` (typeRepPR a)
                `T.mkAppTy` (typeRepPR b)
                `T.mkAppTy` (typeRepPR c)

  {-# NOINLINE typeRepDataPR #-}
  typeRepDataPR (PTuple3 as bs cs)
        = T.typeOf3 ((), (), ())
                `T.mkAppTy` (typeRepDataPR as)
                `T.mkAppTy` (typeRepDataPR bs)
                `T.mkAppTy` (typeRepDataPR cs)

  {-# NOINLINE typeRepDatasPR #-}
  typeRepDatasPR (PTuple3s as bs cs)
        = T.typeOf3 ((), (), ())
                `T.mkAppTy` (typeRepDatasPR as)
                `T.mkAppTy` (typeRepDatasPR bs)
                `T.mkAppTy` (typeRepDatasPR cs)


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PTuple3 emptyPR emptyPR emptyPR


  {-# INLINE_PDATA replicatePR #-}
  replicatePR len (x, y, z)
        = PTuple3 (replicatePR len x)
                  (replicatePR len y)
                  (replicatePR len z)


  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR lens (PTuple3 arr1 arr2 arr3)
        = PTuple3 (replicatesPR lens arr1)
                  (replicatesPR lens arr2)
                  (replicatesPR lens arr3)


  {-# INLINE_PDATA appendPR #-}
  appendPR (PTuple3 arr11 arr12 arr13) (PTuple3 arr21 arr22 arr23)
        = PTuple3 (arr11 `appendPR` arr21)
                  (arr12 `appendPR` arr22)
                  (arr13 `appendPR` arr23) 


  {-# INLINE_PDATA appendvsPR #-}
  appendvsPR segdResult segd1 (PTuple3s arrs11 arrs12 arrs13) segd2 (PTuple3s arrs21 arrs22 arrs23)
        = PTuple3 (appendvsPR segdResult segd1 arrs11 segd2 arrs21)
                  (appendvsPR segdResult segd1 arrs12 segd2 arrs22)
                  (appendvsPR segdResult segd1 arrs13 segd2 arrs23)


  -- Projections ---------------------------------
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PTuple3 arr1 _ _) 
        = lengthPR arr1
  
  {-# INLINE_PDATA indexPR #-}
  indexPR (PTuple3 arr1 arr2 arr3) ix
        = (indexPR arr1 ix, indexPR arr2 ix, indexPR arr3 ix)

  {-# INLINE_PDATA indexsPR #-}
  indexsPR (PTuple3s xs ys zs) srcixs
        = PTuple3 (indexsPR xs srcixs)
                  (indexsPR ys srcixs)
                  (indexsPR zs srcixs)

  {-# INLINE_PDATA indexvsPR #-}
  indexvsPR (PTuple3s xs ys zs) vsegd srcixs
        = PTuple3 (indexvsPR xs vsegd srcixs)
                  (indexvsPR ys vsegd srcixs)
                  (indexvsPR zs vsegd srcixs)

  {-# INLINE_PDATA extractPR #-}
  extractPR (PTuple3 arr1 arr2 arr3) start len
        = PTuple3 (extractPR arr1 start len) 
                  (extractPR arr2 start len)
                  (extractPR arr3 start len)

  {-# INLINE_PDATA extractssPR #-}
  extractssPR (PTuple3s xs ys zs) ussegd
        = PTuple3 (extractssPR xs ussegd)
                  (extractssPR ys ussegd)
                  (extractssPR zs ussegd)

  {-# INLINE_PDATA extractvsPR #-}
  extractvsPR (PTuple3s xs ys zs) uvsegd
        = PTuple3 (extractvsPR xs uvsegd)
                  (extractvsPR ys uvsegd)
                  (extractvsPR zs uvsegd)


  -- Pack and Combine ---------------------------
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PTuple3 arr1 arr2 arr3) tags tag
        = PTuple3 (packByTagPR arr1 tags tag)
                  (packByTagPR arr2 tags tag)
                  (packByTagPR arr3 tags tag)

  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel (PTuple3 xs1 ys1 zs1) (PTuple3 xs2 ys2 zs2)
        = PTuple3 (combine2PR sel xs1 xs2)
                  (combine2PR sel ys1 ys2)
                  (combine2PR sel zs1 zs2)


  -- Conversions --------------------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR vec
   = let (xs, ys, zs)       = V.unzip3 vec
     in  PTuple3  (fromVectorPR xs)
                  (fromVectorPR ys)
                  (fromVectorPR zs)

  {-# NOINLINE toVectorPR #-}
  toVectorPR (PTuple3 xs ys zs)
        = V.zip3  (toVectorPR xs)
                  (toVectorPR ys)
                  (toVectorPR zs)


  -- PData --------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR      
        = PTuple3s emptydPR
                   emptydPR
                   emptydPR 

  
  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PTuple3 x y z)
        = PTuple3s (singletondPR x)
                   (singletondPR y)
                   (singletondPR z)


  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PTuple3s xs _ _)
        = lengthdPR xs
   
   
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PTuple3s xs ys zs) i
        = PTuple3  (indexdPR xs i)
                   (indexdPR ys i)
                   (indexdPR zs i)

   
  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PTuple3s xs1 ys1 zs1) (PTuple3s xs2 ys2 zs2)
        = PTuple3s (appenddPR xs1 xs2)
                   (appenddPR ys1 ys2)
                   (appenddPR zs1 zs2)
  

  {-# NOINLINE fromVectordPR #-}
  fromVectordPR vec
   = let (xss, yss, zss) = V.unzip3 $ V.map (\(PTuple3 xs ys zs) -> (xs, ys, zs)) vec
     in  PTuple3s  (fromVectordPR xss)
                   (fromVectordPR yss)
                   (fromVectordPR zss)


  {-# NOINLINE toVectordPR #-}
  toVectordPR (PTuple3s pdatas1 pdatas2 pdatas3)
        = V.zipWith3 PTuple3
                   (toVectordPR pdatas1)
                   (toVectordPR pdatas2)
                   (toVectordPR pdatas3)

-- PD Functions ---------------------------------------------------------------
-- | O(1). Zip a pair of arrays into an array of pairs.
zip3PD   :: PData a -> PData b -> PData c -> PData (a, b, c)
zip3PD   = PTuple3
{-# INLINE_PA zip3PD #-}


-- Show -----------------------------------------------------------------------
deriving instance (Show (PData  a), Show (PData  b), Show (PData c))
        => Show (PData  (a, b, c))

deriving instance (Show (PDatas a), Show (PDatas b), Show (PDatas c))
        => Show (PDatas (a, b, c))


instance ( PR a, PR b, PR c, Show a, Show b, Show c
         , PprVirtual (PData a), PprVirtual (PData b), PprVirtual (PData c))
        => PprVirtual (PData (a, b, c)) where
 pprv   (PTuple3 xs ys zs)
        = text $ show 
        $ P.zip3 (V.toList $ toVectorPR xs) 
                 (V.toList $ toVectorPR ys)
                 (V.toList $ toVectorPR zs)
