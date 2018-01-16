
-- | Generation of arbitrary array slice specifications.
module DPH.Arbitrary.SliceSpec
        ( SliceSpec (..)
        , arbitrarySliceSpec
        , arbitrarySliceSpec1)
where
import Test.QuickCheck


-- | An in-bounds slice of a vector
data SliceSpec
        = SliceSpec 
        { sliceSpecStart        :: Int 
        , sliceSpecLen          :: Int }
        deriving (Eq)


instance Show SliceSpec where
 show (SliceSpec start len)
        = show (start, len)


-- | Generate an slice specification for an array of the given size.
arbitrarySliceSpec :: Int -> Gen SliceSpec
arbitrarySliceSpec len
 = do   lenSlice  <- choose (0, len)

        -- If the length is 0 then it's valid to slice 0 0,
        -- even though there is no element with index 0
        let maxIx     = if len == 0      then 0     else len - 1 
        let maxStart  = if lenSlice == 0 then maxIx else len - lenSlice

        ixStart   <- choose (0, maxStart)
        return   $  SliceSpec ixStart lenSlice


-- | Generate a slice specification for an array of the given size, 
--   where the slice contains at least one element.
arbitrarySliceSpec1 :: Int -> Gen SliceSpec
arbitrarySliceSpec1 len
 = do   lenSlice  <- choose (1, len)

        -- If the length is 0 then it's valid to slice 0 0,
        -- even though there is no element with index 0
        let maxIx     = if len == 0      then 0     else len - 1 
        let maxStart  = if lenSlice == 0 then maxIx else len - lenSlice

        ixStart   <- choose (0, maxStart)
        return   $  SliceSpec ixStart lenSlice
