{-# LANGUAGE UndecidableInstances #-}
-- | Tests for PArray functions.
--   These are the ones that take PA dictionaries and work on PArrays.
--   They're all exported from D.A.P.PArray.
--   Keep these tests in the same order as D.A.P.PArray
--
import DPH.Arbitrary
import DPH.Testsuite
import Data.Array.Parallel.Array
import Data.Array.Parallel.Base                 (Tag)
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.PArray               (PA, PArray)
import GHC.Exts
import Control.Monad
import Data.Vector                              (Vector)
import Prelude                                  as P
import Text.PrettyPrint                         as T
import qualified Data.Vector                    as V
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Array.Parallel.PArray     as PA
import qualified DPH.Operators.List             as L


$(testcases [ ""        <@ [t|  PArray Int |]
            , "b"       <@ [t|  PArray Int |]
            , "c"       <@ [t|  Int |]
            ]

  [d|
  -- Converting arrays to and from lists.
  prop_toFromVector :: (PA a, Eq a) => Vector a -> Bool
  prop_toFromVector vec
   =  let arr    = PA.fromVector vec
      in  PA.valid arr 
       && vec == PA.toVector arr

  
  -- | Check that the arbitrary arrays we're getting are valid.
  ---  * The arbitrary instance constructs arrays by using other array operators
  --     so if they're broken the all the subseqent tests will fail as well.
  prop_valid    :: PA a => PArray a -> Bool
  prop_valid arr      
        = PA.valid arr


  -- | Define an array that maps all indices to the same element.
  --   The array size must be > 0.
  prop_replicate
        :: (PA a, Eq a)
        => a -> Property
  prop_replicate x
   =  forAll (choose (1, 100)) $ \n
   -> let arr   = PA.replicate n x
          vec   = V.replicate  n x
      in  PA.valid arr  && vec == toVectors1 arr


  -- | Segmented replicate.
  prop_replicates 
        :: (PA a, Eq a) 
        => PArray a -> Int -> Property
  prop_replicates arr i
   =  forAll (vectorOf (PA.length arr) (choose (0, 10 `asTypeOf` i))) $ \reps
   -> let vec'    = join $ V.zipWith V.replicate (toVector reps) (toVectors1 arr)

          segd    = U.lengthsToSegd (U.fromList reps)
          arr'    = PA.replicates segd arr

      in  PA.valid arr' && vec' == toVectors1 arr'


  -- | Lifted replicate.
  prop_replicatel
        :: (PA a, Eq a)
        => PArray a -> Int -> Property
  prop_replicatel arr i
   =  forAll (vectorOf (PA.length arr) (choose (0, 10 `asTypeOf` i))) $ \reps
   -> let vec'    = V.zipWith V.replicate (toVector reps) (toVectors1 arr)
          arr'    = PA.replicatel (PA.fromList reps) arr

      in  PA.valid arr' && vec' == toVectors2 arr'
        

  -- | Append two arrays.  
  prop_append 
        :: (PA a, Eq a)
        => PArray a -> PArray a -> Bool
  prop_append xs ys
    = let vec'   = toVector xs V.++ toVector ys
          arr'   = xs `append` ys

      in  PA.valid arr'  && vec' == toVector arr'


  -- | Lifted append
  -- TODO: Joint22 requires second level lengths to be the same, but this isn't nessesary.
  --       Want to allow this to vary, while still constraining level size.
  prop_appendl
        :: (PA b, Eq b)
        => Joint22 (PArray (PArray b)) (PArray (PArray b)) -> Bool
  prop_appendl (Joint22 arr1 arr2)
   = let vec'   = V.zipWith (V.++) (toVectors2 arr1) (toVectors2 arr2)
         arr'   = PA.appendl arr1 arr2 

     in  PA.valid arr'  && vec' == toVectors2 arr'


  -- | Concatenate a nested array.
  prop_concat
        :: (PA b, Eq b)
        => AArray (PArray (PArray b)) -> Bool
  prop_concat (AArray arr)
   = let  vec'   = join $ toVectors2 arr
          arr'   = PA.concat arr
          
     in   PA.valid arr' &&  vec' == toVector arr'


  -- | Lifted concat
  prop_concatl
        :: (PA c, Eq c)
        => AAArray (PArray (PArray (PArray c))) -> Property
  prop_concatl (AAArray arr)
   =  PA.length arr >= 1
    ==> let vec'   = V.map join $ toVectors3 arr
            arr'   = PA.concatl arr

        in  PA.valid arr' && vec' == toVectors2 arr'


  -- | Concat then unconcat
  prop_concat_unconcat 
        :: (PA b, Eq b)
        => AArray (PArray (PArray b)) -> Bool
  prop_concat_unconcat (AArray arr)
   = let  arr'  = PA.concat arr
          arr'' = PA.unconcat arr arr'

     in   PA.valid arr'' && toVectors2 arr == toVectors2 arr''


  ---------------------------------------------------------
  -- TODO: nestUSegd
  ---------------------------------------------------------
  
  ---------------------------------------------------------
  -- TODO: lengthl
  ---------------------------------------------------------


  -- | Take a single element from an array.
  prop_index :: (PA a, Eq a) => PArray a -> Property
  prop_index arr
    =   PA.length arr > 0
    ==> forAll (choose (0, PA.length arr - 1)) $ \ix 
    ->  toVector arr V.! ix
     == PA.index arr ix


  ---------------------------------------------------------
  -- TODO: indexl
  ---------------------------------------------------------

  -- | Extract a single slice from a single array.
  prop_extract :: (PA a, Eq a) => PArray a -> Property
  prop_extract arr
    =  forAll (arbitrarySliceSpec (PA.length arr)) $ \(SliceSpec ixStart lenSlice)  
    -> let vec'   = V.slice ixStart lenSlice (toVector arr)
           arr'   = PA.extract arr ixStart lenSlice

       in  PA.valid arr' && vec' == toVector arr'


  -- | Extract many slices from a single array.
  prop_extracts1 :: (PA a, Eq a) => PArray a -> Property
  prop_extracts1 arr
   =    PA.length arr > 0 
    ==> forAll (choose (1, 10)) $ \sliceCount
     -> forAll (replicateM sliceCount (arbitrarySliceSpec1 (PA.length arr))) $ \sliceSpecs'
     -> let sliceSpecs  = V.fromList sliceSpecs'
            lens        = V.map sliceSpecLen    sliceSpecs
            starts      = V.map sliceSpecStart  sliceSpecs
            sources     = V.replicate (V.length sliceSpecs) 0

            vec         = PA.toVector arr
            vec'        = V.concat $ V.toList
                        $ V.zipWith (\len start -> V.slice start len vec)
                                lens
                                starts

            segd        = U.lengthsToSegd $ V.convert lens
            ssegd       = U.mkSSegd  (V.convert starts) (V.convert sources) segd
            arr'        = PA.extracts (V.singleton arr) ssegd

        in  PA.valid arr' && vec' == toVector arr'


  ---------------------------------------------------------
  -- TODO: extracts_n, extract from multiple vectors
  ---------------------------------------------------------

  ---------------------------------------------------------
  -- TODO: slice
  ---------------------------------------------------------

  ---------------------------------------------------------
  -- TODO: slicel
  ---------------------------------------------------------

  ---------------------------------------------------------
  -- TODO: pack
  ---------------------------------------------------------

  ---------------------------------------------------------
  -- TODO: packl
  ---------------------------------------------------------


  -- | Filter an array based on some tags.
  prop_packByTag
    :: (PA a, Eq a, Arbitrary a, Show a)
    => PArray a -> Property
  prop_packByTag arr
   =   forAll (liftM V.fromList $ vectorOf (PA.length arr) (choose (0, 1))) $ \tags
    -> forAll (choose (0, 1))                                               $ \tag
    -> let vec'    = V.fromList
                   $ L.packByTag  (V.toList $ toVector arr)
                                  (V.toList tags)
                                  tag

           arr'    = PA.packByTag arr
                                  (U.fromList $ V.toList tags)
                                  tag

       in  PA.valid arr' && vec' == toVector arr'


  -- TODO: more interesting input data.
  -- | Combine two arrays based on a selector.
  prop_combine2 
     :: (PA a, Eq a, Arbitrary a, Show a) 
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
            arr'        = PA.combine2  sel2 (PA.fromVector vec1) (PA.fromVector vec2)

        in  PA.valid arr' && vec' == toVector arr'


  |])

instance (PprVirtual a, PprVirtual b) => PprVirtual (Either a b) where
 pprv (Left  x) = text "Left"  <+> pprv x
 pprv (Right y) = text "Right" <+> pprv y
  

-- Arbitrary PArrays ----------------------------------------------------------
instance (PprPhysical (PArray a), Arbitrary a, PA a) 
       => ArbitraryLen (PArray a) where
 arbitraryLen n
  = do  plan    <- arbitraryLen n
        arbitraryArrayFromExp plan
        
instance ArbitraryLen (PArray a) => Arbitrary (PArray a) where
 arbitrary 
  = sized $ \s ->
  do    s'      <- choose (0, s)
        arbitraryLen s'


