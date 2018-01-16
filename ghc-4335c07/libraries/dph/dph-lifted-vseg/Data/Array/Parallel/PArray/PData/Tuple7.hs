{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | PR instance for tuples.
module Data.Array.Parallel.PArray.PData.Tuple7
        ( PData(..),    PDatas(..)
        , zip7PD)
where
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import GHC.Exts
import Prelude hiding (zip, unzip)
import qualified Data.Typeable                  as T
import qualified Data.Vector                    as V
import qualified Data.List                      as P


-- The following are only imported for zip7,
-- remove once that's where it's supposed to be
import qualified Data.Vector as DV
import qualified Data.Vector.Fusion.Bundle.Monadic  as MB
import qualified Data.Vector.Fusion.Stream.Monadic  as MS
import qualified Data.Vector.Generic                as G
import Data.Vector.Fusion.Bundle (inplace)

-------------------------------------------------------------------------------
data instance PData (a, b, c, d, e, f, g)
        = PTuple7  (PData a)  (PData b)  (PData c)  (PData d)  (PData e) (PData f) (PData g)

data instance PDatas (a, b, c, d, e, f, g)
        = PTuple7s (PDatas a) (PDatas b) (PDatas c) (PDatas d) (PDatas e) (PDatas f) (PDatas g)


-- PR -------------------------------------------------------------------------
instance (PR a, PR b, PR c, PR d, PR e, PR f, PR g) => PR (a, b, c, d, e, f, g) where

  {-# NOINLINE validPR #-}
  validPR (PTuple7 xs ys zs ds es fs gs)
        = validPR xs && validPR ys && validPR zs && validPR ds && validPR es && validPR fs && validPR gs


  {-# NOINLINE nfPR #-}
  nfPR (PTuple7 arr1 arr2 arr3 arr4 arr5 arr6 arr7)
        = nfPR arr1 `seq` nfPR arr2 `seq` nfPR arr3 `seq` nfPR arr4 `seq` nfPR arr5 `seq` 
          nfPR arr6 `seq` nfPR arr7 `seq` ()


  {-# NOINLINE similarPR #-}
  similarPR (x1, y1, z1, d1, e1, f1, g1) (x2, y2, z2, d2, e2, f2, g2)
        =  similarPR x1 x2
        && similarPR y1 y2
        && similarPR z1 z2
        && similarPR d1 d2
        && similarPR e1 e2
        && similarPR f1 f2
        && similarPR g1 g2
        

  {-# NOINLINE coversPR #-}
  coversPR weak (PTuple7 arr1 arr2 arr3 arr4 arr5 arr6 arr7) ix
        =  coversPR weak arr1 ix
        && coversPR weak arr2 ix
        && coversPR weak arr3 ix
        && coversPR weak arr4 ix
        && coversPR weak arr5 ix
        && coversPR weak arr6 ix
        && coversPR weak arr7 ix


  {-# NOINLINE pprpPR #-}
  pprpPR (x, y, z, d, e, f, g)
        = text "Tuple7 "
        <> vcat [ pprpPR x
                , pprpPR y
                , pprpPR z
                , pprpPR d
                , pprpPR e
                , pprpPR f
                , pprpPR g]
        

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PTuple7 xs ys zs ds es fs gs)
        = text "PTuple7 " 
        <> vcat [ pprpDataPR xs
                , pprpDataPR ys
                , pprpDataPR zs
                , pprpDataPR ds
                , pprpDataPR es
                , pprpDataPR fs
                , pprpDataPR gs]


  {-# NOINLINE typeRepPR #-}
  typeRepPR x@(a, b, c, d, e, f, g)
        = T.typeOf7 x 
                `T.mkAppTy` (typeRepPR a)
                `T.mkAppTy` (typeRepPR b)
                `T.mkAppTy` (typeRepPR c)
                `T.mkAppTy` (typeRepPR d)
                `T.mkAppTy` (typeRepPR e)
                `T.mkAppTy` (typeRepPR f)
                `T.mkAppTy` (typeRepPR g)


  {-# NOINLINE typeRepDataPR #-}
  typeRepDataPR (PTuple7 as bs cs ds es fs gs)
        = T.typeOf7 ((), (), (), (), (), (), ())
                `T.mkAppTy` (typeRepDataPR as)
                `T.mkAppTy` (typeRepDataPR bs)
                `T.mkAppTy` (typeRepDataPR cs)
                `T.mkAppTy` (typeRepDataPR ds)
                `T.mkAppTy` (typeRepDataPR es)
                `T.mkAppTy` (typeRepDataPR fs)
                `T.mkAppTy` (typeRepDataPR gs)


  {-# NOINLINE typeRepDatasPR #-}
  typeRepDatasPR (PTuple7s as bs cs ds es fs gs)
        = T.typeOf7 ((), (), (), (), (), (), ())
                `T.mkAppTy` (typeRepDatasPR as)
                `T.mkAppTy` (typeRepDatasPR bs)
                `T.mkAppTy` (typeRepDatasPR cs)
                `T.mkAppTy` (typeRepDatasPR ds)
                `T.mkAppTy` (typeRepDatasPR es)
                `T.mkAppTy` (typeRepDatasPR fs)
                `T.mkAppTy` (typeRepDatasPR gs)


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PTuple7 emptyPR emptyPR emptyPR emptyPR emptyPR emptyPR emptyPR


  {-# INLINE_PDATA replicatePR #-}
  replicatePR len (x, y, z, d, e, f, g)
        = PTuple7 (replicatePR len x)
                  (replicatePR len y)
                  (replicatePR len z)
                  (replicatePR len d)
                  (replicatePR len e)
                  (replicatePR len f)
                  (replicatePR len g)


  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR lens (PTuple7 arr1 arr2 arr3 arr4 arr5 arr6 arr7)
        = PTuple7 (replicatesPR lens arr1)
                  (replicatesPR lens arr2)
                  (replicatesPR lens arr3)
                  (replicatesPR lens arr4)
                  (replicatesPR lens arr5)
                  (replicatesPR lens arr6)
                  (replicatesPR lens arr7)


  {-# INLINE_PDATA appendPR #-}
  appendPR (PTuple7 arr11 arr12 arr13 arr14 arr15 arr16 arr17)
           (PTuple7 arr21 arr22 arr23 arr24 arr25 arr26 arr27)
        = PTuple7 (arr11 `appendPR` arr21)
                  (arr12 `appendPR` arr22)
                  (arr13 `appendPR` arr23) 
                  (arr14 `appendPR` arr24) 
                  (arr15 `appendPR` arr25) 
                  (arr16 `appendPR` arr26) 
                  (arr17 `appendPR` arr27) 


  {-# INLINE_PDATA appendvsPR #-}
  appendvsPR segdResult segd1 (PTuple7s arrs11 arrs12 arrs13 arrs14 arrs15 arrs16 arrs17)
                       segd2 (PTuple7s arrs21 arrs22 arrs23 arrs24 arrs25 arrs26 arrs27)
        = PTuple7 (appendvsPR segdResult segd1 arrs11 segd2 arrs21)
                  (appendvsPR segdResult segd1 arrs12 segd2 arrs22)
                  (appendvsPR segdResult segd1 arrs13 segd2 arrs23)
                  (appendvsPR segdResult segd1 arrs14 segd2 arrs24)
                  (appendvsPR segdResult segd1 arrs15 segd2 arrs25)
                  (appendvsPR segdResult segd1 arrs16 segd2 arrs26)
                  (appendvsPR segdResult segd1 arrs17 segd2 arrs27)

  -- Projections ---------------------------------
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PTuple7 arr1 _ _ _ _ _ _) 
        = lengthPR arr1
  
  {-# INLINE_PDATA indexPR #-}
  indexPR (PTuple7 arr1 arr2 arr3 arr4 arr5 arr6 arr7) ix
        = ( indexPR arr1 ix
          , indexPR arr2 ix
          , indexPR arr3 ix
          , indexPR arr4 ix
          , indexPR arr5 ix
          , indexPR arr6 ix
          , indexPR arr7 ix)


  {-# INLINE_PDATA indexsPR #-}
  indexsPR (PTuple7s xs ys zs ds es fs gs) srcixs
        = PTuple7 (indexsPR xs srcixs)
                  (indexsPR ys srcixs)
                  (indexsPR zs srcixs)
                  (indexsPR ds srcixs)
                  (indexsPR es srcixs)
                  (indexsPR fs srcixs)
                  (indexsPR gs srcixs)

  {-# INLINE_PDATA indexvsPR #-}
  indexvsPR (PTuple7s xs ys zs ds es fs gs) vsegd srcixs
        = PTuple7 (indexvsPR xs vsegd srcixs)
                  (indexvsPR ys vsegd srcixs)
                  (indexvsPR zs vsegd srcixs)
                  (indexvsPR ds vsegd srcixs)
                  (indexvsPR es vsegd srcixs)
                  (indexvsPR fs vsegd srcixs)
                  (indexvsPR gs vsegd srcixs)

  {-# INLINE_PDATA extractPR #-}
  extractPR (PTuple7 arr1 arr2 arr3 arr4 arr5 arr6 arr7) start len
        = PTuple7 (extractPR arr1 start len) 
                  (extractPR arr2 start len)
                  (extractPR arr3 start len)
                  (extractPR arr4 start len)
                  (extractPR arr5 start len)
                  (extractPR arr6 start len)
                  (extractPR arr7 start len)

  {-# INLINE_PDATA extractssPR #-}
  extractssPR (PTuple7s xs ys zs ds es fs gs) ussegd
        = PTuple7 (extractssPR xs ussegd)
                  (extractssPR ys ussegd)
                  (extractssPR zs ussegd)
                  (extractssPR ds ussegd)
                  (extractssPR es ussegd)
                  (extractssPR fs ussegd)
                  (extractssPR gs ussegd)

  {-# INLINE_PDATA extractvsPR #-}
  extractvsPR (PTuple7s xs ys zs ds es fs gs) uvsegd
        = PTuple7 (extractvsPR xs uvsegd)
                  (extractvsPR ys uvsegd)
                  (extractvsPR zs uvsegd)
                  (extractvsPR ds uvsegd)
                  (extractvsPR es uvsegd)
                  (extractvsPR fs uvsegd)
                  (extractvsPR gs uvsegd)


  -- Pack and Combine ---------------------------
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PTuple7 arr1 arr2 arr3 arr4 arr5 arr6 arr7) tags tag
        = PTuple7 (packByTagPR arr1 tags tag)
                  (packByTagPR arr2 tags tag)
                  (packByTagPR arr3 tags tag)
                  (packByTagPR arr4 tags tag)
                  (packByTagPR arr5 tags tag)
                  (packByTagPR arr6 tags tag)
                  (packByTagPR arr7 tags tag)


  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel (PTuple7 xs1 ys1 zs1 ds1 es1 fs1 gs1) (PTuple7 xs2 ys2 zs2 ds2 es2 fs2 gs2)
        = PTuple7 (combine2PR sel xs1 xs2)
                  (combine2PR sel ys1 ys2)
                  (combine2PR sel zs1 zs2)
                  (combine2PR sel ds1 ds2)
                  (combine2PR sel es1 es2)
                  (combine2PR sel fs1 fs2)
                  (combine2PR sel gs1 gs2)


  -- Conversions --------------------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR vec
   = let (xs, ys, zs, ds, es, fs, gs)       = unzip7 vec
     in  PTuple7  (fromVectorPR xs)
                  (fromVectorPR ys)
                  (fromVectorPR zs)
                  (fromVectorPR ds)
                  (fromVectorPR es)
                  (fromVectorPR fs)
                  (fromVectorPR gs)

  {-# NOINLINE toVectorPR #-}
  toVectorPR (PTuple7 xs ys zs ds es fs gs)
        =    zip7  (toVectorPR xs)
                   (toVectorPR ys)
                   (toVectorPR zs)
                   (toVectorPR ds)
                   (toVectorPR es)
                   (toVectorPR fs)
                   (toVectorPR gs)


  -- PData --------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR      
        = PTuple7s emptydPR
                   emptydPR
                   emptydPR 
                   emptydPR 
                   emptydPR 
                   emptydPR
                   emptydPR 

  
  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PTuple7 x y z d e f g)
        = PTuple7s (singletondPR x)
                   (singletondPR y)
                   (singletondPR z)
                   (singletondPR d)
                   (singletondPR e)
                   (singletondPR f)
                   (singletondPR g)


  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PTuple7s xs _ _ _ _ _ _)
        = lengthdPR xs
   
   
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PTuple7s xs ys zs ds es fs gs) i
        = PTuple7  (indexdPR xs i)
                   (indexdPR ys i)
                   (indexdPR zs i)
                   (indexdPR ds i)
                   (indexdPR es i)
                   (indexdPR fs i)
                   (indexdPR gs i)

   
  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PTuple7s xs1 ys1 zs1 ds1 es1 fs1 gs1) (PTuple7s xs2 ys2 zs2 ds2 es2 fs2 gs2)
        = PTuple7s (appenddPR xs1 xs2)
                   (appenddPR ys1 ys2)
                   (appenddPR zs1 zs2)
                   (appenddPR ds1 ds2)
                   (appenddPR es1 es2)
                   (appenddPR fs1 fs2)
                   (appenddPR gs1 gs2)
  

  {-# NOINLINE fromVectordPR #-}
  fromVectordPR vec
   = let (xss, yss, zss, dss, ess, fss, gss) = unzip7 $ V.map (\(PTuple7 xs ys zs ds es fs gs) -> (xs, ys, zs, ds, es, fs, gs)) vec
     in  PTuple7s  (fromVectordPR xss)
                   (fromVectordPR yss)
                   (fromVectordPR zss)
                   (fromVectordPR dss)
                   (fromVectordPR ess)
                   (fromVectordPR fss)
                   (fromVectordPR gss)


  {-# NOINLINE toVectordPR #-}
  toVectordPR (PTuple7s pdatas1 pdatas2 pdatas3 pdatas4 pdatas5 pdatas6 pdatas7)
        = zipWith7 PTuple7
                   (toVectordPR pdatas1)
                   (toVectordPR pdatas2)
                   (toVectordPR pdatas3)
                   (toVectordPR pdatas4)
                   (toVectordPR pdatas5)
                   (toVectordPR pdatas6)
                   (toVectordPR pdatas7)


-- PD Functions ---------------------------------------------------------------
-- | O(1). Zip a pair of arrays into an array of pairs.
zip7PD   :: PData a -> PData b -> PData c -> PData d -> PData e -> PData f -> PData g -> PData (a, b, c, d, e, f, g)
zip7PD   = PTuple7
{-# INLINE_PA zip7PD #-}


-- Show -----------------------------------------------------------------------
deriving instance (Show (PData  a), Show (PData  b), Show (PData c), Show (PData d), Show (PData e), Show (PData f), Show (PData g))
        => Show (PData  (a, b, c, d, e, f, g))

deriving instance (Show (PDatas a), Show (PDatas b), Show (PDatas c), Show (PDatas d), Show (PDatas e), Show (PDatas f), Show (PDatas g))
        => Show (PDatas (a, b, c, d, e, f, g))


instance ( PR a, PR b, PR c, PR d, PR e, PR f, PR g, Show a, Show b, Show c, Show d, Show e, Show f, Show g
         , PprVirtual (PData a), PprVirtual (PData b), PprVirtual (PData c), PprVirtual (PData d), PprVirtual (PData e), PprVirtual (PData f), PprVirtual (PData g))
        => PprVirtual (PData (a, b, c, d, e, f, g)) where
 pprv   (PTuple7 xs ys zs ds es fs gs)
        = text $ show 
        $ P.zip7 (V.toList $ toVectorPR xs) 
                 (V.toList $ toVectorPR ys)
                 (V.toList $ toVectorPR zs)
                 (V.toList $ toVectorPR ds)
                 (V.toList $ toVectorPR es)
                 (V.toList $ toVectorPR fs)
                 (V.toList $ toVectorPR gs)

                 
-- zips and related functions -------------------------------------------------
-- should be in Data.Vector

zipWith7MS :: Monad m => (a -> b -> c -> d -> e -> f -> g -> h)
                    -> MB.Bundle m v a -> MB.Bundle m v b -> MB.Bundle m v c -> MB.Bundle m v d
                    -> MB.Bundle m v e -> MB.Bundle m v f -> MB.Bundle m v g -> MB.Bundle m v h
{-# INLINE zipWith7MS #-}
zipWith7MS fn = zipWith7M (\a b c d e f g -> return (fn a b c d e f g))


zipWith7Generic :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e,
             G.Vector v f, G.Vector v g, G.Vector v h)
         => (a -> b -> c -> d -> e -> f -> g -> h)
         -> v a -> v b -> v c -> v d -> v e -> v f -> v g -> v h
{-# INLINE zipWith7Generic #-}
zipWith7Generic f as bs cs ds es fs gs
  = G.unstream (zipWith7MS f (G.stream as)
                          (G.stream bs)
                          (G.stream cs)
                          (G.stream ds)
                          (G.stream es)
                          (G.stream fs)
                          (G.stream gs))
zip7Generic :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e,
         G.Vector v f, G.Vector v g, G.Vector v (a, b, c, d, e, f, g))
     => v a -> v b -> v c -> v d -> v e -> v f -> v g -> v (a, b, c, d, e, f, g)
{-# INLINE zip7Generic #-}
zip7Generic = zipWith7Generic (,,,,,,)

unzip7Generic :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e,
           G.Vector v f, G.Vector v g, G.Vector v (a, b, c, d, e, f, g))
       => v (a, b, c, d, e, f, g) -> (v a, v b, v c, v d, v e, v f, v g)
{-# INLINE unzip7Generic #-}
unzip7Generic xs = 
            (mapG (\(a, _b, _c, _d, _e, _f, _g) -> a) xs,
             mapG (\(_a, b, _c, _d, _e, _f, _g) -> b) xs,
             mapG (\(_a, _b, c, _d, _e, _f, _g) -> c) xs,
             mapG (\(_a, _b, _c, d, _e, _f, _g) -> d) xs,
             mapG (\(_a, _b, _c, _d, e, _f, _g) -> e) xs,
             mapG (\(_a, _b, _c, _d, _e, f, _g) -> f) xs,
             mapG (\(_a, _b, _c, _d, _e, _f, g) -> g) xs)
  where
    -- | /O(n)/ Map a function over a vector
    mapG :: (G.Vector v a, G.Vector v b) => (a -> b) -> v a -> v b
    {-# INLINE mapG #-}
    mapG f = G.unstream . inplace (MS.map f) id . G.stream

zipWith7M :: Monad m => (a -> b -> c -> d -> e -> f -> g -> m h)
                     -> MB.Bundle m v a -> MB.Bundle m v b -> MB.Bundle m v c -> MB.Bundle m v d
                     -> MB.Bundle m v e -> MB.Bundle m v f -> MB.Bundle m v g -> MB.Bundle m v h
{-# INLINE zipWith7M #-}
zipWith7M fn sa sb sc sd se sf sg
  = MB.zipWithM (\(a,b,c) (d,e,(f, g)) -> fn a b c d e f g) (MB.zip3 sa sb sc)
                                                  (MB.zip3 sd se (MB.zip sf sg))

{-}                                                  
zipWith7Monad :: Monad m => (a -> b -> c -> d -> e -> f -> g -> h)
                    -> MB.Bundle m a -> MB.Bundle m b -> MB.Bundle m c -> MB.Bundle m d
                    -> MB.Bundle m e -> MB.Bundle m f -> MB.Bundle m g -> MB.Bundle m h
{-# INLINE zipWith7Monad #-}
zipWith7Monad fn = zipWith7M (\a b c d e f g -> return (fn a b c d e f g))



zip7Monad :: Monad m => MB.Bundle m a -> MB.Bundle m b -> MB.Bundle m c -> MB.Bundle m d
                -> MB.Bundle m e -> MB.Bundle m f -> MB.Bundle m g -> MB.Bundle m (a,b,c,d,e,f,g)
{-# INLINE zip7Monad #-}
zip7Monad = zipWith7Monad (,,,,,,)
-}

zip7 :: DV.Vector a -> DV.Vector b -> DV.Vector c -> DV.Vector d -> DV.Vector e -> DV.Vector f -> DV.Vector g
     -> DV.Vector (a, b, c, d, e, f, g)
{-# INLINE zip7 #-}
zip7 = zip7Generic

unzip7 :: DV.Vector (a, b, c, d, e, f, g)
       -> (DV.Vector a, DV.Vector b, DV.Vector c, DV.Vector d, DV.Vector e, DV.Vector f, DV.Vector g)
{-# INLINE unzip7 #-}
unzip7 = unzip7Generic

zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h)
         -> DV.Vector a -> DV.Vector b -> DV.Vector c -> DV.Vector d -> DV.Vector e
         -> DV.Vector f -> DV.Vector g -> DV.Vector h
{-# INLINE zipWith7 #-}
zipWith7 = zipWith7Generic
