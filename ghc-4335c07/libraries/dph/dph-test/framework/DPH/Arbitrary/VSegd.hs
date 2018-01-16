
-- | Generation of arbitrary segment descriptors.
module DPH.Arbitrary.VSegd
        ( pdatasForVSegd
        , pdataForVSegd
        , arraysForVSegd
        , vsegdOfLength
        )
where
import Test.QuickCheck
import Data.Array.Parallel.Unlifted as U hiding ( update )
import Data.List
import Prelude as P

import qualified Data.Array.Parallel.PArray.PData        as PD
import qualified Data.Array.Parallel.PArray.PData.Nested as PDN

import qualified Data.Vector as V

import Debug.Trace


instance Arbitrary VSegd where
  arbitrary = sized vsegdOfLength

-- | Generate a virtual segment descriptor with @len@ elements in vsegmap, ie logical data is @len@-long.
vsegdOfLength :: Int -> Gen VSegd
vsegdOfLength len
 = sized $ \size -> do
        -- stop data getting too big
        let smaller = truncate $ sqrt $ fromIntegral size
        let len' = max 1 len

        -- number of elements in flattened data
        NonNegative n' <- resize smaller arbitrary
        let n = max 1 n'

        -- indices
        ids <- inrange n (len'-1)
        let ids' = 0 : sort ids

        -- flat segment descriptor
        let lens = indicesToLengths ids' n
        let segd = mkSegd (fromList lens) (fromList ids') n

        -- virtual segment map and starts
        vsegs  <- inrange len' len'
        starts <- inrange len' len'

        -- choose maximum number of source arrays
        NonNegative nsrc' <- resize smaller arbitrary
        let nsrc = max 1 nsrc'
        sources <- inrange nsrc len'

        let ssegd = mkSSegd (fromList starts) (fromList sources) segd
        return $ mkVSegd (fromList vsegs) ssegd
 where 
        indicesToLengths ids n = P.zipWith (-) (tail $ ids ++ [n]) ids

        inrange n len = P.map (`mod` n) `fmap` vector len

-- | Generate some data for a given virtual segment descriptor, eg for use with append_vs.
-- Takes a phantom/unused argument of type @a@ to make tests easier..
arraysForVSegd :: (Arbitrary a, Elt a, Elts a) => VSegd -> a -> Gen (Arrays a)
arraysForVSegd vsegd _
 = do   d <- mapM pdata [0..maxsrc]
        return $ fromVectors $ V.fromList d
 where
        vsegids = toList $ U.takeVSegidsOfVSegd  vsegd
        ssegd   =          U.takeSSegdOfVSegd    vsegd
        starts  = toList $ U.startsOfSSegd       ssegd
        sources = toList $ U.sourcesOfSSegd      ssegd
        lens    = toList $ U.lengthsOfSSegd      ssegd

        maxsrc    = maxo sources
        ends    = P.zipWith (+) starts lens

        maxo xs | null xs       = 0
                | otherwise     = maximum xs

        pdata n = do
                let minlen = maxo $ P.map fst $ P.filter ((==n).snd) $ P.zip ends sources
                v <- vector minlen
                return $ fromList v

-- | Generate PDatas for a given virtual segment descriptor, eg for use with appendlPR
-- Takes a phantom/unused argument of type @a@ to make tests easier..
pdatasForVSegd :: (Arbitrary a, PD.PR a) => VSegd -> a -> Gen (PD.PDatas a)
pdatasForVSegd vsegd _
 = do   d <- mapM pdata [0..maxsrc]
        return $ PD.fromVectordPR $ V.fromList d
 where
        vsegids = toList $ U.takeVSegidsOfVSegd  vsegd
        ssegd   =          U.takeSSegdOfVSegd    vsegd
        starts  = toList $ U.startsOfSSegd       ssegd
        sources = toList $ U.sourcesOfSSegd      ssegd
        lens    = toList $ U.lengthsOfSSegd      ssegd

        maxsrc    = maxo sources
        ends    = P.zipWith (+) starts lens

        maxo xs | null xs       = 0
                | otherwise     = maximum xs

        pdata n = do
                let minlen = maxo $ P.map fst $ P.filter ((==n).snd) $ P.zip ends sources
                v <- vector minlen
                return $ PD.fromVectorPR $ V.fromList v

pdataForVSegd :: (Arbitrary a, PD.PR a) => VSegd -> a -> Gen (PD.PData (PD.PArray a))
pdataForVSegd vsegd phantom = do
    pdatas <- pdatasForVSegd vsegd phantom
    let segd' = U.unsafeDemoteToSegdOfVSegd vsegd
    let flat' = PDN.extractvs_delay pdatas vsegd
    return $ PDN.mkPNested vsegd pdatas segd' flat'


