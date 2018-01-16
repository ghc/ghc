{-# LANGUAGE
        ScopedTypeVariables, BangPatterns, MagicHash, PackageImports,
        TypeSynonymInstances, FlexibleInstances, FlexibleContexts,
        UndecidableInstances, TemplateHaskell, MultiParamTypeClasses #-}

-- | Tests for PR functions.
--   These are the ones that take a PR dictionary, and work on PData arrays.
--
--   TODO: Most of the tests don't use the Exp builder becasuse they take Vectors
--         instead of PArray / PData.
-- 
import DPH.Arbitrary
import DPH.Testsuite
import Data.Array.Parallel.Array
import Data.Array.Parallel.Base                 (Tag)
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.PArray               (PA(..))
import Data.Array.Parallel.PArray.PData.Base    ()
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr
import GHC.Exts
import Control.Monad
import Data.Vector                              (Vector)
import Text.PrettyPrint                         as T
import Prelude                                  as P
import qualified Data.Vector                    as V
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Array.Parallel.PArray     as PA
import qualified DPH.Operators.List             as L
import System.IO.Unsafe
import Debug.Trace

import DPH.Arbitrary.VSegd

-- NOTE:
-- The 'b' element type contains one less level of nesting compared with the
-- 'a' type. We use 'b' when we're checking properties of functions that
--  already require nested arrays, such as "concat".

{-
$(testcases [ ""        <@ [t|  (  Int,        PArray Int,        PArray (PArray Int)
                                ,  (),         PArray ()
                                ,  (Int, Int), PArray (Int, Int), PArray (PArray Int, PArray Int)) |]

            , "b"       <@ [t|  ( Int,         PArray Int ) |]
            ]
-}


$(testcases [ ""        <@ [t|  PArray Int |]
            , "b"       <@ [t|  PArray Int |]
            , "c"       <@ [t|  Int |]
            ]

  [d|
  -- PR Dictionary functions --------------------------------------------------
  -- All the functions defined in the PR dictionary should be tested in this
  -- section. The functions appear in the same order as in the class


  -- Converting arrays to and from lists.
  --  * If this doesn't work then we'll be generating invalid arbitrary arrays
  --    for subsequent tests.
  --  * Note that converting a nested array to and from a list is more involved
  --    than converting a flat array, as we need to construct segment descriptors.
  prop_toFromVector :: (PR a, Eq a) => Vector a -> Bool
  prop_toFromVector vec
   =  let arr    = fromVectorPR vec
      in  validPR arr  && vec == toVectorPR arr


  -- | Check that the arbitrary arrays we're getting are valid.
  ---  * The arbitrary instance constructs arrays by using other array operators
  --     so if they're broken the all the subseqent tests will fail as well.
  prop_valid    :: PR a => PData a -> Bool
  prop_valid pdata      
        = validPR pdata


  -- | Define an array that maps all indices to the same element.
  --   The array size must be > 0.
  prop_replicate :: (PR a, Eq a) => a -> Property
  prop_replicate x
   =  forAll (choose (1, 100)) $ \n
   -> let pdata = replicatePR n x
          vec   = V.replicate n x
      in  validPR pdata  && vec == toVectors1 pdata


  -- | Segmented replicate.
  prop_replicates :: (PR a, Eq a) => PData a -> Int -> Property
  prop_replicates pdata i
   =  forAll (vectorOf (lengthPR pdata) (choose (0, 10 `asTypeOf` i))) $ \reps
   -> let vec'    = join $ V.zipWith V.replicate (toVector reps) (toVectors1 pdata)

          segd    = U.lengthsToSegd (U.fromList reps)
          pdata'  = replicatesPR segd pdata

      in  validPR pdata' && vec' == toVectors1 pdata'


  -- | Take a single element from an array.
  prop_index :: (PR a, Eq a) => PData a -> Property
  prop_index pdata
    =   lengthPR pdata > 0
    ==> forAll (choose (0, lengthPR pdata - 1)) $ \ix 
    ->  toVectorPR pdata V.! ix
     == indexPR pdata ix


  ---------------------------------------------------------
  -- TODO: indexl
  ---------------------------------------------------------


  -- | Extract a single slice from a single array.
  prop_extract :: (PR a, Eq a) => PData a -> Property
  prop_extract pdata
    =  forAll (arbitrarySliceSpec (lengthPR pdata)) $ \(SliceSpec ixStart lenSlice)  
    -> let vec'   = V.slice ixStart lenSlice (toVector pdata)
           pdata' = extractPR pdata ixStart lenSlice

       in  validPR pdata' && vec' == toVector pdata'


  -- | Extract many slices from a single array.
  prop_extracts1 :: (PR a, Eq a) => PData a -> Property
  prop_extracts1 pdata
   =    lengthPR pdata > 0 
    ==> forAll (choose (1, 10)) $ \sliceCount
     -> forAll (replicateM sliceCount (arbitrarySliceSpec1 (lengthPR pdata))) $ \sliceSpecs'
     -> let sliceSpecs  = V.fromList sliceSpecs'
            lens        = V.map sliceSpecLen    sliceSpecs
            starts      = V.map sliceSpecStart  sliceSpecs
            sources     = V.replicate (V.length sliceSpecs) 0

            vec         = toVectorPR pdata
            vec'        = V.concat $ V.toList
                        $ V.zipWith (\len start -> V.slice start len vec)
                                lens
                                starts

            segd        = U.lengthsToSegd $ V.convert lens
            ssegd       = U.mkSSegd  (V.convert starts) (V.convert sources) segd
            pdata'      = extractssPR (singletondPR pdata) ssegd

        in  validPR pdata' && vec' == toVector pdata'


  ---------------------------------------------------------
  -- TODO: extracts_n, extract from multiple vectors
  ---------------------------------------------------------


  -- | Append two arrays.  
  prop_append :: (PR a, Eq a) => PData a -> PData a -> Bool
  prop_append xs ys
    = let vec'   = toVector xs V.++ toVector ys
          pdata' = xs `appendPR` ys

      in  validPR pdata'  && vec' == toVectorPR pdata'


  ---------------------------------------------------------
  -- TODO: appends, segmented append
  ---------------------------------------------------------


  -- | Filter an array based on some tags.
  prop_packByTag
    :: (PR a, Eq a, Arbitrary a, Show a)
    => PData a -> Property
  prop_packByTag pdata
   =   forAll (liftM V.fromList $ vectorOf (lengthPR pdata) (choose (0, 1))) $ \tags
    -> forAll (choose (0, 1))                                                $ \tag
    -> let vec'    = V.fromList
                   $ L.packByTag  (V.toList $ toVector pdata)
                                  (V.toList tags)
                                  tag

           pdata'  = packByTagPR  pdata
                                  (U.fromList $ V.toList tags)
                                  tag

       in  validPR pdata' && vec' == toVector pdata'


  -- TODO: more interesting input data.
  -- | Combine two arrays based on a selector.
  prop_combine2 
     :: (PR a, Eq a, Arbitrary a, Show a) 
     => Selector -> Vector a -> Property
  prop_combine2 (Selector vecTags) zz
   =    V.length vecTags >= 2
    ==> even (V.length vecTags)
    ==> forAll (liftM V.fromList $ vectorOf (V.length vecTags `div` 2) arbitrary) $ \vec1
     -> forAll (liftM V.fromList $ vectorOf (V.length vecTags `div` 2) arbitrary) $ \vec2
     -> let vec'        = V.fromList
                        $ L.combine2 (V.toList vecTags) 
                                     (V.toList $ vec1 `asTypeOf` zz) 
                                     (V.toList $ vec2 `asTypeOf` zz)

            sel2        = U.tagsToSel2 (U.fromList $ V.toList vecTags)
            pdata'      = combine2PR  sel2 (fromVectorPR vec1) (fromVectorPR vec2)

        in  validPR pdata'
         && vec' == toVectorPR pdata'


  -- TODO: more interesting input data.
  -- TODO: use sanely sized nested vector
  -- | Concatenate arrays that have been produced via combine.
  --   When an nested array has been produced with combine, it's guaranteed to contain
  --   multiple flat data arrays in its psegdata field. By concatenating it we test
  --   that extractsPR handles this representation.
  prop_combine2_concat
     :: (PR b, PA b, Eq b, Arbitrary b, Show b) 
     => Selector -> Vector (Vector b) -> Property
  prop_combine2_concat (Selector vecTags) zz
   =    V.length vecTags >= 2
    ==> even (V.length vecTags)
    ==> forAll (liftM V.fromList $ vectorOf (V.length vecTags `div` 2) arbitrary) $ \vec1
     -> forAll (liftM V.fromList $ vectorOf (V.length vecTags `div` 2) arbitrary) $ \vec2
     -> let vec'        = V.fromList
                        $ L.combine2 (V.toList vecTags) 
                                     (V.toList $ vec1 `asTypeOf` zz) 
                                     (V.toList $ vec2 `asTypeOf` zz)
            vec''       = V.concat (V.toList vec')

            sel2        = U.tagsToSel2 (U.fromList $ V.toList vecTags)
            pdata'      = combine2PR sel2 
                                (fromVectorPR $ V.map PA.fromVector vec1) 
                                (fromVectorPR $ V.map PA.fromVector vec2)
            pdata''     = concatPR pdata'

        in  validPR pdata''
         && vec'' == toVectorPR pdata''


  -- TODO: more interesting input data
  -- | Packing an array then immediately combining it should yield the original array.
  prop_combine2_packByTag
   :: (PR a, Eq a, Arbitrary a, Show a)
   => Selector -> Vector a -> Property
  prop_combine2_packByTag (Selector vecTags) zz
   =    V.length vecTags >= 2
    ==> even (V.length vecTags)
    ==> forAll (liftM V.fromList $ vectorOf (V.length vecTags) arbitrary) $ \vec
     -> let 
            uarrTags    = U.fromList $ V.toList vecTags
            sel2        = U.tagsToSel2 uarrTags

            pdata         = fromVectorPR (vec `asTypeOf` zz)
            pdata'        = combine2PR sel2
                                (packByTagPR pdata uarrTags 0)
                                (packByTagPR pdata uarrTags 1)

        in  validPR pdata' && toVector pdata == toVector pdata'


  -- Derived Functions --------------------------------------------------------
  -- These are PR functions that are not in the PR dictionary.
  -- | Concatenate arrays
  prop_concat
        :: (PR b, PA b, Eq b)
        => AArray (PData (PArray b)) -> Bool
  prop_concat (AArray pdata)
   = let  vec'   = join $ toVectors2 pdata
          pdata' = concatPR pdata
          
     in   validPR pdata' &&  vec' == toVector pdata'


  -- | Lifted concat
  prop_concatl
        :: (PR c, PA c, Eq c, PprPhysical c)
        => AAArray (PData (PArray (PArray c))) -> Property
  prop_concatl arr@(AAArray pdata)
   = trace (render $ pprp pdata)
   $  lengthPR pdata >= 1
    ==> let vec'   = V.map join $ toVectors3 pdata
            pdata' = concatlPR pdata

        in  validPR pdata' && vec' == toVectors2 pdata'


  -- | Concat then unconcat
  prop_concat_unconcat 
        :: (PR b, PA b, Eq b)
        => AArray (PData (PArray b)) -> Bool
  prop_concat_unconcat (AArray pdata)
   = let  pdata'  = concatPR   pdata  
          pdata'' = unconcatPR pdata pdata'

     in   validPR pdata'' && toVectors2 pdata == toVectors2 pdata''


  -- | Lifted append
  prop_appendl
        :: (PR b, PA b, Eq b)
        => Joint2n (PData (PArray b)) (PData (PArray b)) ->  Bool
  prop_appendl (Joint2n pdata1 pdata2)
   = let vec'   = V.zipWith (V.++) (toVectors2 pdata1) (toVectors2 pdata2)
         pdata' = appendlPR pdata1 pdata2 

     in  validPR pdata'  && vec' == toVectors2 pdata'

  -- | Lifted append, but with more interesting kinds of data (more sharing, separate sources, etc)
  prop_appendlV
        :: (PR b, PA b, Eq b, Arbitrary b, Show b, Show (PData b), Show (PDatas b))
        => b -> Property
  prop_appendlV phantom
   = forAll (sized (\n -> return n)) $ \len ->
     forAll (vsegdOfLength len) $ \segd1 ->
     forAll (pdataForVSegd segd1 phantom) $ \pdata1 ->
     forAll (vsegdOfLength len) $ \segd2 ->
     forAll (pdataForVSegd segd2 phantom) $ \pdata2 ->

     let vec'   = V.zipWith (V.++) (toVectors2 pdata1) (toVectors2 pdata2)
         pdata' = appendlPR pdata1 pdata2 

     in  validPR pdata'  && vec' == toVectors2 pdata'

  prop_fromvec1 :: Vector Int -> Bool
  prop_fromvec1 pdata
   = let vec'   = toVector $ (fromVector pdata :: PData Int)
     in  vec' == pdata

  prop_fromvec2 :: Vector (PArray Int) -> Bool
  prop_fromvec2 pdata
   = let vec'   = toVector $ (fromVector pdata :: PData (PArray Int))
     in  vec' == pdata


  ---------------------------------------------------------
  -- TODO: slicelPD
  ---------------------------------------------------------
  |])


-- Nesting --------------------------------------------------------------------
instance PR a => Array PData a where
 length       = lengthPR
 index        = indexPR
 append       = appendPR
 toVector     = toVectorPR
 fromVector   = fromVectorPR


-- Arbitrary PArrays ----------------------------------------------------------
instance (PprPhysical (PArray a), Arbitrary a, PR a) 
       => ArbitraryLen (PArray a) where
 arbitraryLen n
  = do  plan    <- arbitraryLen n
        pdata   <- arbitraryArrayFromExp plan
        return  $ wrapPDataAsPArray pdata

instance ArbitraryLen (PArray a) => Arbitrary (PArray a) where
 arbitrary = sized arbitraryLen

wrapPDataAsPArray :: PR a => PData a -> PArray a
wrapPDataAsPArray pdata
 = let  !(I# n#)        = lengthPR pdata
   in   PArray n# pdata


-- Arbitrary PData ------------------------------------------------------------
instance (PprPhysical (PData a), Arbitrary a, PR a) 
       => ArbitraryLen (PData a) where
 arbitraryLen n 
  = do  plan    <- arbitraryLen n
        arbitraryArrayFromExp plan

instance ArbitraryLen (PData a) => Arbitrary (PData a) where
 arbitrary = sized arbitraryLen

