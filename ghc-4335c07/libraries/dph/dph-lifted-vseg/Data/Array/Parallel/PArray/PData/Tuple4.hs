{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | PR instance for tuples.
module Data.Array.Parallel.PArray.PData.Tuple4
        ( PData(..),    PDatas(..)
        , zip4PD)
where
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import GHC.Exts
import Prelude hiding (zip, unzip)
import qualified Data.Typeable                  as T
import qualified Data.Vector                    as V
import qualified Data.List                      as P

-------------------------------------------------------------------------------
data instance PData (a, b, c, d)
        = PTuple4  (PData a)  (PData b)  (PData c)  (PData d)

data instance PDatas (a, b, c, d)
        = PTuple4s (PDatas a) (PDatas b) (PDatas c) (PDatas d)


-- PR -------------------------------------------------------------------------
instance (PR a, PR b, PR c, PR d) => PR (a, b, c, d) where

  {-# NOINLINE validPR #-}
  validPR (PTuple4 xs ys zs ds)
        = validPR xs && validPR ys && validPR zs && validPR ds


  {-# NOINLINE nfPR #-}
  nfPR (PTuple4 arr1 arr2 arr3 arr4)
        = nfPR arr1 `seq` nfPR arr2 `seq` nfPR arr3 `seq` nfPR arr4 `seq` ()


  {-# NOINLINE similarPR #-}
  similarPR (x1, y1, z1, d1) (x2, y2, z2, d2)
        =  similarPR x1 x2
        && similarPR y1 y2
        && similarPR z1 z2
        && similarPR d1 d2


  {-# NOINLINE coversPR #-}
  coversPR weak (PTuple4 arr1 arr2 arr3 arr4) ix
        =  coversPR weak arr1 ix
        && coversPR weak arr2 ix
        && coversPR weak arr3 ix
        && coversPR weak arr4 ix


  {-# NOINLINE pprpPR #-}
  pprpPR (x, y, z, d)
        = text "Tuple4 "
        <> vcat [ pprpPR x
                , pprpPR y
                , pprpPR z
                , pprpPR d ]
        

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PTuple4 xs ys zs ds)
        = text "PTuple4 " 
        <> vcat [ pprpDataPR xs
                , pprpDataPR ys
                , pprpDataPR zs
                , pprpDataPR ds]

  {-# NOINLINE typeRepPR #-}
  typeRepPR x@(a, b, c, d)
        = T.typeOf4 x 
                `T.mkAppTy` (typeRepPR a)
                `T.mkAppTy` (typeRepPR b)
                `T.mkAppTy` (typeRepPR c)
                `T.mkAppTy` (typeRepPR d)

  {-# NOINLINE typeRepDataPR #-}
  typeRepDataPR (PTuple4 as bs cs ds)
        = T.typeOf4 ((), (), (), ())
                `T.mkAppTy` (typeRepDataPR as)
                `T.mkAppTy` (typeRepDataPR bs)
                `T.mkAppTy` (typeRepDataPR cs)
                `T.mkAppTy` (typeRepDataPR ds)

  {-# NOINLINE typeRepDatasPR #-}
  typeRepDatasPR (PTuple4s as bs cs ds)
        = T.typeOf4 ((), (), (), ())
                `T.mkAppTy` (typeRepDatasPR as)
                `T.mkAppTy` (typeRepDatasPR bs)
                `T.mkAppTy` (typeRepDatasPR cs)
                `T.mkAppTy` (typeRepDatasPR ds)


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PTuple4 emptyPR emptyPR emptyPR emptyPR


  {-# INLINE_PDATA replicatePR #-}
  replicatePR len (x, y, z, d)
        = PTuple4 (replicatePR len x)
                  (replicatePR len y)
                  (replicatePR len z)
                  (replicatePR len d)


  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR lens (PTuple4 arr1 arr2 arr3 arr4)
        = PTuple4 (replicatesPR lens arr1)
                  (replicatesPR lens arr2)
                  (replicatesPR lens arr3)
                  (replicatesPR lens arr4)


  {-# INLINE_PDATA appendPR #-}
  appendPR (PTuple4 arr11 arr12 arr13 arr14)
           (PTuple4 arr21 arr22 arr23 arr24)
        = PTuple4 (arr11 `appendPR` arr21)
                  (arr12 `appendPR` arr22)
                  (arr13 `appendPR` arr23) 
                  (arr14 `appendPR` arr24) 


  {-# INLINE_PDATA appendvsPR #-}
  appendvsPR segdResult segd1 (PTuple4s arrs11 arrs12 arrs13 arrs14)
                       segd2 (PTuple4s arrs21 arrs22 arrs23 arrs24)
        = PTuple4 (appendvsPR segdResult segd1 arrs11 segd2 arrs21)
                  (appendvsPR segdResult segd1 arrs12 segd2 arrs22)
                  (appendvsPR segdResult segd1 arrs13 segd2 arrs23)
                  (appendvsPR segdResult segd1 arrs14 segd2 arrs24)


  -- Projections ---------------------------------
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PTuple4 arr1 _ _ _) 
        = lengthPR arr1
  
  {-# INLINE_PDATA indexPR #-}
  indexPR (PTuple4 arr1 arr2 arr3 arr4) ix
        = ( indexPR arr1 ix
          , indexPR arr2 ix
          , indexPR arr3 ix
          , indexPR arr4 ix)


  {-# INLINE_PDATA indexsPR #-}
  indexsPR (PTuple4s xs ys zs ds) srcixs
        = PTuple4 (indexsPR xs srcixs)
                  (indexsPR ys srcixs)
                  (indexsPR zs srcixs)
                  (indexsPR ds srcixs)

  {-# INLINE_PDATA indexvsPR #-}
  indexvsPR (PTuple4s xs ys zs ds) vsegd srcixs
        = PTuple4 (indexvsPR xs vsegd srcixs)
                  (indexvsPR ys vsegd srcixs)
                  (indexvsPR zs vsegd srcixs)
                  (indexvsPR ds vsegd srcixs)

  {-# INLINE_PDATA extractPR #-}
  extractPR (PTuple4 arr1 arr2 arr3 arr4) start len
        = PTuple4 (extractPR arr1 start len) 
                  (extractPR arr2 start len)
                  (extractPR arr3 start len)
                  (extractPR arr4 start len)

  {-# INLINE_PDATA extractssPR #-}
  extractssPR (PTuple4s xs ys zs ds) ussegd
        = PTuple4 (extractssPR xs ussegd)
                  (extractssPR ys ussegd)
                  (extractssPR zs ussegd)
                  (extractssPR ds ussegd)

  {-# INLINE_PDATA extractvsPR #-}
  extractvsPR (PTuple4s xs ys zs ds) uvsegd
        = PTuple4 (extractvsPR xs uvsegd)
                  (extractvsPR ys uvsegd)
                  (extractvsPR zs uvsegd)
                  (extractvsPR ds uvsegd)


  -- Pack and Combine ---------------------------
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PTuple4 arr1 arr2 arr3 arr4) tags tag
        = PTuple4 (packByTagPR arr1 tags tag)
                  (packByTagPR arr2 tags tag)
                  (packByTagPR arr3 tags tag)
                  (packByTagPR arr4 tags tag)


  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel (PTuple4 xs1 ys1 zs1 ds1) (PTuple4 xs2 ys2 zs2 ds2)
        = PTuple4 (combine2PR sel xs1 xs2)
                  (combine2PR sel ys1 ys2)
                  (combine2PR sel zs1 zs2)
                  (combine2PR sel ds1 ds2)


  -- Conversions --------------------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR vec
   = let (xs, ys, zs, ds)       = V.unzip4 vec
     in  PTuple4  (fromVectorPR xs)
                  (fromVectorPR ys)
                  (fromVectorPR zs)
                  (fromVectorPR ds)

  {-# NOINLINE toVectorPR #-}
  toVectorPR (PTuple4 xs ys zs ds)
        = V.zip4  (toVectorPR xs)
                  (toVectorPR ys)
                  (toVectorPR zs)
                  (toVectorPR ds)


  -- PData --------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR      
        = PTuple4s emptydPR
                   emptydPR
                   emptydPR 
                   emptydPR 

  
  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PTuple4 x y z d)
        = PTuple4s (singletondPR x)
                   (singletondPR y)
                   (singletondPR z)
                   (singletondPR d)


  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PTuple4s xs _ _ _)
        = lengthdPR xs
   
   
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PTuple4s xs ys zs ds) i
        = PTuple4  (indexdPR xs i)
                   (indexdPR ys i)
                   (indexdPR zs i)
                   (indexdPR ds i)

   
  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PTuple4s xs1 ys1 zs1 ds1) (PTuple4s xs2 ys2 zs2 ds2)
        = PTuple4s (appenddPR xs1 xs2)
                   (appenddPR ys1 ys2)
                   (appenddPR zs1 zs2)
                   (appenddPR ds1 ds2)
  

  {-# NOINLINE fromVectordPR #-}
  fromVectordPR vec
   = let (xss, yss, zss, dss) = V.unzip4 $ V.map (\(PTuple4 xs ys zs ds) -> (xs, ys, zs, ds)) vec
     in  PTuple4s  (fromVectordPR xss)
                   (fromVectordPR yss)
                   (fromVectordPR zss)
                   (fromVectordPR dss)


  {-# NOINLINE toVectordPR #-}
  toVectordPR (PTuple4s pdatas1 pdatas2 pdatas3 pdatas4)
        = V.zipWith4 PTuple4
                   (toVectordPR pdatas1)
                   (toVectordPR pdatas2)
                   (toVectordPR pdatas3)
                   (toVectordPR pdatas4)


-- PD Functions ---------------------------------------------------------------
-- | O(1). Zip a pair of arrays into an array of pairs.
zip4PD   :: PData a -> PData b -> PData c -> PData d -> PData (a, b, c, d)
zip4PD   = PTuple4
{-# INLINE_PA zip4PD #-}


-- Show -----------------------------------------------------------------------
deriving instance (Show (PData  a), Show (PData  b), Show (PData c), Show (PData d))
        => Show (PData  (a, b, c, d))

deriving instance (Show (PDatas a), Show (PDatas b), Show (PDatas c), Show (PDatas d))
        => Show (PDatas (a, b, c, d))


instance ( PR a, PR b, PR c, PR d, Show a, Show b, Show c, Show d
         , PprVirtual (PData a), PprVirtual (PData b), PprVirtual (PData c), PprVirtual (PData d))
        => PprVirtual (PData (a, b, c, d)) where
 pprv   (PTuple4 xs ys zs ds)
        = text $ show 
        $ P.zip4 (V.toList $ toVectorPR xs) 
                 (V.toList $ toVectorPR ys)
                 (V.toList $ toVectorPR zs)
                 (V.toList $ toVectorPR ds)

                 
                 
