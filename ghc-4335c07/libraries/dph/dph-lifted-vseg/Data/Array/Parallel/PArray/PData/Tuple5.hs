{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | PR instance for tuples.
module Data.Array.Parallel.PArray.PData.Tuple5
        ( PData(..),    PDatas(..)
        , zip5PD)
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
data instance PData (a, b, c, d, e)
        = PTuple5  (PData a)  (PData b)  (PData c)  (PData d)  (PData e)

data instance PDatas (a, b, c, d, e)
        = PTuple5s (PDatas a) (PDatas b) (PDatas c) (PDatas d) (PDatas e)


-- PR -------------------------------------------------------------------------
instance (PR a, PR b, PR c, PR d, PR e) => PR (a, b, c, d, e) where

  {-# NOINLINE validPR #-}
  validPR (PTuple5 xs ys zs ds es)
        = validPR xs && validPR ys && validPR zs && validPR ds && validPR es


  {-# NOINLINE nfPR #-}
  nfPR (PTuple5 arr1 arr2 arr3 arr4 arr5)
        = nfPR arr1 `seq` nfPR arr2 `seq` nfPR arr3 `seq` nfPR arr4 `seq` nfPR arr5 `seq` ()


  {-# NOINLINE similarPR #-}
  similarPR (x1, y1, z1, d1, e1) (x2, y2, z2, d2, e2)
        =  similarPR x1 x2
        && similarPR y1 y2
        && similarPR z1 z2
        && similarPR d1 d2
        && similarPR e1 e2


  {-# NOINLINE coversPR #-}
  coversPR weak (PTuple5 arr1 arr2 arr3 arr4 arr5) ix
        =  coversPR weak arr1 ix
        && coversPR weak arr2 ix
        && coversPR weak arr3 ix
        && coversPR weak arr4 ix
        && coversPR weak arr5 ix


  {-# NOINLINE pprpPR #-}
  pprpPR (x, y, z, d, e)
        = text "Tuple5 "
        <> vcat [ pprpPR x
                , pprpPR y
                , pprpPR z
                , pprpPR d
                , pprpPR e ]
        

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PTuple5 xs ys zs ds es)
        = text "PTuple5 " 
        <> vcat [ pprpDataPR xs
                , pprpDataPR ys
                , pprpDataPR zs
                , pprpDataPR ds
                , pprpDataPR es]

  {-# NOINLINE typeRepPR #-}
  typeRepPR x@(a, b, c, d, e)
        = T.typeOf5 x 
                `T.mkAppTy` (typeRepPR a)
                `T.mkAppTy` (typeRepPR b)
                `T.mkAppTy` (typeRepPR c)
                `T.mkAppTy` (typeRepPR d)
                `T.mkAppTy` (typeRepPR e)

  {-# NOINLINE typeRepDataPR #-}
  typeRepDataPR (PTuple5 as bs cs ds es)
        = T.typeOf5 ((), (), (), (), ())
                `T.mkAppTy` (typeRepDataPR as)
                `T.mkAppTy` (typeRepDataPR bs)
                `T.mkAppTy` (typeRepDataPR cs)
                `T.mkAppTy` (typeRepDataPR ds)
                `T.mkAppTy` (typeRepDataPR es)

  {-# NOINLINE typeRepDatasPR #-}
  typeRepDatasPR (PTuple5s as bs cs ds es)
        = T.typeOf5 ((), (), (), (), ())
                `T.mkAppTy` (typeRepDatasPR as)
                `T.mkAppTy` (typeRepDatasPR bs)
                `T.mkAppTy` (typeRepDatasPR cs)
                `T.mkAppTy` (typeRepDatasPR ds)
                `T.mkAppTy` (typeRepDatasPR es)


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PTuple5 emptyPR emptyPR emptyPR emptyPR emptyPR


  {-# INLINE_PDATA replicatePR #-}
  replicatePR len (x, y, z, d, e)
        = PTuple5 (replicatePR len x)
                  (replicatePR len y)
                  (replicatePR len z)
                  (replicatePR len d)
                  (replicatePR len e)


  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR lens (PTuple5 arr1 arr2 arr3 arr4 arr5)
        = PTuple5 (replicatesPR lens arr1)
                  (replicatesPR lens arr2)
                  (replicatesPR lens arr3)
                  (replicatesPR lens arr4)
                  (replicatesPR lens arr5)


  {-# INLINE_PDATA appendPR #-}
  appendPR (PTuple5 arr11 arr12 arr13 arr14 arr15)
           (PTuple5 arr21 arr22 arr23 arr24 arr25)
        = PTuple5 (arr11 `appendPR` arr21)
                  (arr12 `appendPR` arr22)
                  (arr13 `appendPR` arr23) 
                  (arr14 `appendPR` arr24) 
                  (arr15 `appendPR` arr25) 


  {-# INLINE_PDATA appendvsPR #-}
  appendvsPR segdResult segd1 (PTuple5s arrs11 arrs12 arrs13 arrs14 arrs15)
                       segd2 (PTuple5s arrs21 arrs22 arrs23 arrs24 arrs25)
        = PTuple5 (appendvsPR segdResult segd1 arrs11 segd2 arrs21)
                  (appendvsPR segdResult segd1 arrs12 segd2 arrs22)
                  (appendvsPR segdResult segd1 arrs13 segd2 arrs23)
                  (appendvsPR segdResult segd1 arrs14 segd2 arrs24)
                  (appendvsPR segdResult segd1 arrs15 segd2 arrs25)


  -- Projections ---------------------------------
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PTuple5 arr1 _ _ _ _) 
        = lengthPR arr1
  
  {-# INLINE_PDATA indexPR #-}
  indexPR (PTuple5 arr1 arr2 arr3 arr4 arr5) ix
        = ( indexPR arr1 ix
          , indexPR arr2 ix
          , indexPR arr3 ix
          , indexPR arr4 ix
          , indexPR arr5 ix)


  {-# INLINE_PDATA indexsPR #-}
  indexsPR (PTuple5s xs ys zs ds es) srcixs
        = PTuple5 (indexsPR xs srcixs)
                  (indexsPR ys srcixs)
                  (indexsPR zs srcixs)
                  (indexsPR ds srcixs)
                  (indexsPR es srcixs)

  {-# INLINE_PDATA indexvsPR #-}
  indexvsPR (PTuple5s xs ys zs ds es) vsegd srcixs
        = PTuple5 (indexvsPR xs vsegd srcixs)
                  (indexvsPR ys vsegd srcixs)
                  (indexvsPR zs vsegd srcixs)
                  (indexvsPR ds vsegd srcixs)
                  (indexvsPR es vsegd srcixs)

  {-# INLINE_PDATA extractPR #-}
  extractPR (PTuple5 arr1 arr2 arr3 arr4 arr5) start len
        = PTuple5 (extractPR arr1 start len) 
                  (extractPR arr2 start len)
                  (extractPR arr3 start len)
                  (extractPR arr4 start len)
                  (extractPR arr5 start len)

  {-# INLINE_PDATA extractssPR #-}
  extractssPR (PTuple5s xs ys zs ds es) ussegd
        = PTuple5 (extractssPR xs ussegd)
                  (extractssPR ys ussegd)
                  (extractssPR zs ussegd)
                  (extractssPR ds ussegd)
                  (extractssPR es ussegd)

  {-# INLINE_PDATA extractvsPR #-}
  extractvsPR (PTuple5s xs ys zs ds es) uvsegd
        = PTuple5 (extractvsPR xs uvsegd)
                  (extractvsPR ys uvsegd)
                  (extractvsPR zs uvsegd)
                  (extractvsPR ds uvsegd)
                  (extractvsPR es uvsegd)


  -- Pack and Combine ---------------------------
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PTuple5 arr1 arr2 arr3 arr4 arr5) tags tag
        = PTuple5 (packByTagPR arr1 tags tag)
                  (packByTagPR arr2 tags tag)
                  (packByTagPR arr3 tags tag)
                  (packByTagPR arr4 tags tag)
                  (packByTagPR arr5 tags tag)


  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel (PTuple5 xs1 ys1 zs1 ds1 es1) (PTuple5 xs2 ys2 zs2 ds2 es2)
        = PTuple5 (combine2PR sel xs1 xs2)
                  (combine2PR sel ys1 ys2)
                  (combine2PR sel zs1 zs2)
                  (combine2PR sel ds1 ds2)
                  (combine2PR sel es1 es2)


  -- Conversions --------------------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR vec
   = let (xs, ys, zs, ds, es)       = V.unzip5 vec
     in  PTuple5  (fromVectorPR xs)
                  (fromVectorPR ys)
                  (fromVectorPR zs)
                  (fromVectorPR ds)
                  (fromVectorPR es)

  {-# NOINLINE toVectorPR #-}
  toVectorPR (PTuple5 xs ys zs ds es)
        = V.zip5  (toVectorPR xs)
                  (toVectorPR ys)
                  (toVectorPR zs)
                  (toVectorPR ds)
                  (toVectorPR es)


  -- PData --------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR      
        = PTuple5s emptydPR
                   emptydPR
                   emptydPR 
                   emptydPR 
                   emptydPR 

  
  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PTuple5 x y z d e)
        = PTuple5s (singletondPR x)
                   (singletondPR y)
                   (singletondPR z)
                   (singletondPR d)
                   (singletondPR e)


  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PTuple5s xs _ _ _ _)
        = lengthdPR xs
   
   
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PTuple5s xs ys zs ds es) i
        = PTuple5  (indexdPR xs i)
                   (indexdPR ys i)
                   (indexdPR zs i)
                   (indexdPR ds i)
                   (indexdPR es i)

   
  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PTuple5s xs1 ys1 zs1 ds1 es1) (PTuple5s xs2 ys2 zs2 ds2 es2)
        = PTuple5s (appenddPR xs1 xs2)
                   (appenddPR ys1 ys2)
                   (appenddPR zs1 zs2)
                   (appenddPR ds1 ds2)
                   (appenddPR es1 es2)
  

  {-# NOINLINE fromVectordPR #-}
  fromVectordPR vec
   = let (xss, yss, zss, dss, ess) = V.unzip5 $ V.map (\(PTuple5 xs ys zs ds es) -> (xs, ys, zs, ds, es)) vec
     in  PTuple5s  (fromVectordPR xss)
                   (fromVectordPR yss)
                   (fromVectordPR zss)
                   (fromVectordPR dss)
                   (fromVectordPR ess)


  {-# NOINLINE toVectordPR #-}
  toVectordPR (PTuple5s pdatas1 pdatas2 pdatas3 pdatas4 pdatas5)
        = V.zipWith5 PTuple5
                   (toVectordPR pdatas1)
                   (toVectordPR pdatas2)
                   (toVectordPR pdatas3)
                   (toVectordPR pdatas4)
                   (toVectordPR pdatas5)


-- PD Functions ---------------------------------------------------------------
-- | O(1). Zip a pair of arrays into an array of pairs.
zip5PD   :: PData a -> PData b -> PData c -> PData d -> PData e -> PData (a, b, c, d, e)
zip5PD   = PTuple5
{-# INLINE_PA zip5PD #-}


-- Show -----------------------------------------------------------------------
deriving instance (Show (PData  a), Show (PData  b), Show (PData c), Show (PData d), Show (PData e))
        => Show (PData  (a, b, c, d, e))

deriving instance (Show (PDatas a), Show (PDatas b), Show (PDatas c), Show (PDatas d), Show (PDatas e))
        => Show (PDatas (a, b, c, d, e))


instance ( PR a, PR b, PR c, PR d, PR e, Show a, Show b, Show c, Show d, Show e
         , PprVirtual (PData a), PprVirtual (PData b), PprVirtual (PData c), PprVirtual (PData d), PprVirtual (PData e))
        => PprVirtual (PData (a, b, c, d, e)) where
 pprv   (PTuple5 xs ys zs ds es)
        = text $ show 
        $ P.zip5 (V.toList $ toVectorPR xs) 
                 (V.toList $ toVectorPR ys)
                 (V.toList $ toVectorPR zs)
                 (V.toList $ toVectorPR ds)
                 (V.toList $ toVectorPR es)

                 
                 
