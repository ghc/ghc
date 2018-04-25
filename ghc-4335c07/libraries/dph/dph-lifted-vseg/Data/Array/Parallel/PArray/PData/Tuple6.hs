{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | PR instance for tuples.
module Data.Array.Parallel.PArray.PData.Tuple6
        ( PData(..),    PDatas(..)
        , zip6PD)
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
data instance PData (a, b, c, d, e, f)
        = PTuple6  (PData a)  (PData b)  (PData c)  (PData d)  (PData e) (PData f)

data instance PDatas (a, b, c, d, e, f)
        = PTuple6s (PDatas a) (PDatas b) (PDatas c) (PDatas d) (PDatas e) (PDatas f)


-- PR -------------------------------------------------------------------------
instance (PR a, PR b, PR c, PR d, PR e, PR f) => PR (a, b, c, d, e, f) where

  {-# NOINLINE validPR #-}
  validPR (PTuple6 xs ys zs ds es fs)
        = validPR xs && validPR ys && validPR zs && validPR ds && validPR es && validPR fs


  {-# NOINLINE nfPR #-}
  nfPR (PTuple6 arr1 arr2 arr3 arr4 arr5 arr6)
        = nfPR arr1 `seq` nfPR arr2 `seq` nfPR arr3 `seq` nfPR arr4 `seq` nfPR arr5 `seq` nfPR arr6 `seq` ()


  {-# NOINLINE similarPR #-}
  similarPR (x1, y1, z1, d1, e1, f1) (x2, y2, z2, d2, e2, f2)
        =  similarPR x1 x2
        && similarPR y1 y2
        && similarPR z1 z2
        && similarPR d1 d2
        && similarPR e1 e2
        && similarPR f1 f2
        


  {-# NOINLINE coversPR #-}
  coversPR weak (PTuple6 arr1 arr2 arr3 arr4 arr5 arr6) ix
        =  coversPR weak arr1 ix
        && coversPR weak arr2 ix
        && coversPR weak arr3 ix
        && coversPR weak arr4 ix
        && coversPR weak arr5 ix
        && coversPR weak arr6 ix


  {-# NOINLINE pprpPR #-}
  pprpPR (x, y, z, d, e, f)
        = text "Tuple6 "
        <> vcat [ pprpPR x
                , pprpPR y
                , pprpPR z
                , pprpPR d
                , pprpPR e
                , pprpPR f ]
        

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PTuple6 xs ys zs ds es fs)
        = text "PTuple6 " 
        <> vcat [ pprpDataPR xs
                , pprpDataPR ys
                , pprpDataPR zs
                , pprpDataPR ds
                , pprpDataPR es
                , pprpDataPR fs]


  {-# NOINLINE typeRepPR #-}
  typeRepPR x@(a, b, c, d, e, f)
        = T.typeOf6 x 
                `T.mkAppTy` (typeRepPR a)
                `T.mkAppTy` (typeRepPR b)
                `T.mkAppTy` (typeRepPR c)
                `T.mkAppTy` (typeRepPR d)
                `T.mkAppTy` (typeRepPR e)
                `T.mkAppTy` (typeRepPR f)

  {-# NOINLINE typeRepDataPR #-}
  typeRepDataPR (PTuple6 as bs cs ds es fs)
        = T.typeOf6 ((), (), (), (), (), ())
                `T.mkAppTy` (typeRepDataPR as)
                `T.mkAppTy` (typeRepDataPR bs)
                `T.mkAppTy` (typeRepDataPR cs)
                `T.mkAppTy` (typeRepDataPR ds)
                `T.mkAppTy` (typeRepDataPR es)
                `T.mkAppTy` (typeRepDataPR fs)

  {-# NOINLINE typeRepDatasPR #-}
  typeRepDatasPR (PTuple6s as bs cs ds es fs)
        = T.typeOf6 ((), (), (), (), (), ())
                `T.mkAppTy` (typeRepDatasPR as)
                `T.mkAppTy` (typeRepDatasPR bs)
                `T.mkAppTy` (typeRepDatasPR cs)
                `T.mkAppTy` (typeRepDatasPR ds)
                `T.mkAppTy` (typeRepDatasPR es)
                `T.mkAppTy` (typeRepDatasPR fs)


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PTuple6 emptyPR emptyPR emptyPR emptyPR emptyPR emptyPR


  {-# INLINE_PDATA replicatePR #-}
  replicatePR len (x, y, z, d, e, f)
        = PTuple6 (replicatePR len x)
                  (replicatePR len y)
                  (replicatePR len z)
                  (replicatePR len d)
                  (replicatePR len e)
                  (replicatePR len f)


  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR lens (PTuple6 arr1 arr2 arr3 arr4 arr5 arr6)
        = PTuple6 (replicatesPR lens arr1)
                  (replicatesPR lens arr2)
                  (replicatesPR lens arr3)
                  (replicatesPR lens arr4)
                  (replicatesPR lens arr5)
                  (replicatesPR lens arr6)


  {-# INLINE_PDATA appendPR #-}
  appendPR (PTuple6 arr11 arr12 arr13 arr14 arr15 arr16)
           (PTuple6 arr21 arr22 arr23 arr24 arr25 arr26)
        = PTuple6 (arr11 `appendPR` arr21)
                  (arr12 `appendPR` arr22)
                  (arr13 `appendPR` arr23) 
                  (arr14 `appendPR` arr24) 
                  (arr15 `appendPR` arr25) 
                  (arr16 `appendPR` arr26) 


  {-# INLINE_PDATA appendvsPR #-}
  appendvsPR segdResult segd1 (PTuple6s arrs11 arrs12 arrs13 arrs14 arrs15 arrs16)
                       segd2 (PTuple6s arrs21 arrs22 arrs23 arrs24 arrs25 arrs26)
        = PTuple6 (appendvsPR segdResult segd1 arrs11 segd2 arrs21)
                  (appendvsPR segdResult segd1 arrs12 segd2 arrs22)
                  (appendvsPR segdResult segd1 arrs13 segd2 arrs23)
                  (appendvsPR segdResult segd1 arrs14 segd2 arrs24)
                  (appendvsPR segdResult segd1 arrs15 segd2 arrs25)
                  (appendvsPR segdResult segd1 arrs16 segd2 arrs26)


  -- Projections ---------------------------------
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PTuple6 arr1 _ _ _ _ _) 
        = lengthPR arr1
  
  {-# INLINE_PDATA indexPR #-}
  indexPR (PTuple6 arr1 arr2 arr3 arr4 arr5 arr6) ix
        = ( indexPR arr1 ix
          , indexPR arr2 ix
          , indexPR arr3 ix
          , indexPR arr4 ix
          , indexPR arr5 ix
          , indexPR arr6 ix)


  {-# INLINE_PDATA indexsPR #-}
  indexsPR (PTuple6s xs ys zs ds es fs) srcixs
        = PTuple6 (indexsPR xs srcixs)
                  (indexsPR ys srcixs)
                  (indexsPR zs srcixs)
                  (indexsPR ds srcixs)
                  (indexsPR es srcixs)
                  (indexsPR fs srcixs)

  {-# INLINE_PDATA indexvsPR #-}
  indexvsPR (PTuple6s xs ys zs ds es fs) vsegd srcixs
        = PTuple6 (indexvsPR xs vsegd srcixs)
                  (indexvsPR ys vsegd srcixs)
                  (indexvsPR zs vsegd srcixs)
                  (indexvsPR ds vsegd srcixs)
                  (indexvsPR es vsegd srcixs)
                  (indexvsPR fs vsegd srcixs)

  {-# INLINE_PDATA extractPR #-}
  extractPR (PTuple6 arr1 arr2 arr3 arr4 arr5 arr6) start len
        = PTuple6 (extractPR arr1 start len) 
                  (extractPR arr2 start len)
                  (extractPR arr3 start len)
                  (extractPR arr4 start len)
                  (extractPR arr5 start len)
                  (extractPR arr6 start len)

  {-# INLINE_PDATA extractssPR #-}
  extractssPR (PTuple6s xs ys zs ds es fs) ussegd
        = PTuple6 (extractssPR xs ussegd)
                  (extractssPR ys ussegd)
                  (extractssPR zs ussegd)
                  (extractssPR ds ussegd)
                  (extractssPR es ussegd)
                  (extractssPR fs ussegd)

  {-# INLINE_PDATA extractvsPR #-}
  extractvsPR (PTuple6s xs ys zs ds es fs) uvsegd
        = PTuple6 (extractvsPR xs uvsegd)
                  (extractvsPR ys uvsegd)
                  (extractvsPR zs uvsegd)
                  (extractvsPR ds uvsegd)
                  (extractvsPR es uvsegd)
                  (extractvsPR fs uvsegd)


  -- Pack and Combine ---------------------------
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PTuple6 arr1 arr2 arr3 arr4 arr5 arr6) tags tag
        = PTuple6 (packByTagPR arr1 tags tag)
                  (packByTagPR arr2 tags tag)
                  (packByTagPR arr3 tags tag)
                  (packByTagPR arr4 tags tag)
                  (packByTagPR arr5 tags tag)
                  (packByTagPR arr6 tags tag)


  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel (PTuple6 xs1 ys1 zs1 ds1 es1 fs1) (PTuple6 xs2 ys2 zs2 ds2 es2 fs2)
        = PTuple6 (combine2PR sel xs1 xs2)
                  (combine2PR sel ys1 ys2)
                  (combine2PR sel zs1 zs2)
                  (combine2PR sel ds1 ds2)
                  (combine2PR sel es1 es2)
                  (combine2PR sel fs1 fs2)


  -- Conversions --------------------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR vec
   = let (xs, ys, zs, ds, es, fs)       = V.unzip6 vec
     in  PTuple6  (fromVectorPR xs)
                  (fromVectorPR ys)
                  (fromVectorPR zs)
                  (fromVectorPR ds)
                  (fromVectorPR es)
                  (fromVectorPR fs)

  {-# NOINLINE toVectorPR #-}
  toVectorPR (PTuple6 xs ys zs ds es fs)
        = V.zip6  (toVectorPR xs)
                  (toVectorPR ys)
                  (toVectorPR zs)
                  (toVectorPR ds)
                  (toVectorPR es)
                  (toVectorPR fs)


  -- PData --------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR      
        = PTuple6s emptydPR
                   emptydPR
                   emptydPR 
                   emptydPR 
                   emptydPR 
                   emptydPR 

  
  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PTuple6 x y z d e f)
        = PTuple6s (singletondPR x)
                   (singletondPR y)
                   (singletondPR z)
                   (singletondPR d)
                   (singletondPR e)
                   (singletondPR f)


  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PTuple6s xs _ _ _ _ _)
        = lengthdPR xs
   
   
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PTuple6s xs ys zs ds es fs) i
        = PTuple6  (indexdPR xs i)
                   (indexdPR ys i)
                   (indexdPR zs i)
                   (indexdPR ds i)
                   (indexdPR es i)
                   (indexdPR fs i)

   
  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PTuple6s xs1 ys1 zs1 ds1 es1 fs1) (PTuple6s xs2 ys2 zs2 ds2 es2 fs2)
        = PTuple6s (appenddPR xs1 xs2)
                   (appenddPR ys1 ys2)
                   (appenddPR zs1 zs2)
                   (appenddPR ds1 ds2)
                   (appenddPR es1 es2)
                   (appenddPR fs1 fs2)
  

  {-# NOINLINE fromVectordPR #-}
  fromVectordPR vec
   = let (xss, yss, zss, dss, ess, fss) = V.unzip6 $ V.map (\(PTuple6 xs ys zs ds es fs) -> (xs, ys, zs, ds, es, fs)) vec
     in  PTuple6s  (fromVectordPR xss)
                   (fromVectordPR yss)
                   (fromVectordPR zss)
                   (fromVectordPR dss)
                   (fromVectordPR ess)
                   (fromVectordPR fss)


  {-# NOINLINE toVectordPR #-}
  toVectordPR (PTuple6s pdatas1 pdatas2 pdatas3 pdatas4 pdatas5 pdatas6)
        = V.zipWith6 PTuple6
                   (toVectordPR pdatas1)
                   (toVectordPR pdatas2)
                   (toVectordPR pdatas3)
                   (toVectordPR pdatas4)
                   (toVectordPR pdatas5)
                   (toVectordPR pdatas6)


-- PD Functions ---------------------------------------------------------------
-- | O(1). Zip a pair of arrays into an array of pairs.
zip6PD   :: PData a -> PData b -> PData c -> PData d -> PData e -> PData f -> PData (a, b, c, d, e, f)
zip6PD   = PTuple6
{-# INLINE_PA zip6PD #-}


-- Show -----------------------------------------------------------------------
deriving instance (Show (PData  a), Show (PData  b), Show (PData c), Show (PData d), Show (PData e), Show (PData f))
        => Show (PData  (a, b, c, d, e, f))

deriving instance (Show (PDatas a), Show (PDatas b), Show (PDatas c), Show (PDatas d), Show (PDatas e), Show (PDatas f))
        => Show (PDatas (a, b, c, d, e, f))


instance ( PR a, PR b, PR c, PR d, PR e, PR f, Show a, Show b, Show c, Show d, Show e, Show f
         , PprVirtual (PData a), PprVirtual (PData b), PprVirtual (PData c), PprVirtual (PData d), PprVirtual (PData e), PprVirtual (PData f))
        => PprVirtual (PData (a, b, c, d, e, f)) where
 pprv   (PTuple6 xs ys zs ds es fs)
        = text $ show 
        $ P.zip6 (V.toList $ toVectorPR xs) 
                 (V.toList $ toVectorPR ys)
                 (V.toList $ toVectorPR zs)
                 (V.toList $ toVectorPR ds)
                 (V.toList $ toVectorPR es)
                 (V.toList $ toVectorPR fs)

                 
                 
