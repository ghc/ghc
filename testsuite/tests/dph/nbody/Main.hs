{-# LANGUAGE ParallelListComp, BangPatterns #-}

import Config
import Dump
import World
import Body
import Util
import Solver
import Generate
import Control.Monad
import Data.Maybe
import qualified Data.Vector.Unboxed            as V
import qualified Data.Array.Parallel    as P
import qualified Data.Array.Parallel.PArray     as P


main :: IO ()
main 
 = let  config          = defaultConfig
        calcAccels      = calcAccels_nb
        
        -- Setup initial world
        vPoints         = genPointsDisc 
                                (configBodyCount config)
                                (0, 0) 
                                (configStartDiscSize config)

        vBodies         = V.map (setStartVelOfBody $ configStartSpeed config)
                        $ V.map (setMassOfBody     $ configBodyMass   config)
                        $ V.map (uncurry unitBody) 
                        $ vPoints

        worldStart      = World
                        { worldBodies   = vBodies
                        , worldSteps    = 0 }

    in  mainBatch config calcAccels worldStart 


-- | Run the simulation in batch mode.
mainBatch :: Config -> Solver -> World -> IO ()
mainBatch config calcAccels worldStart
 = do   let world' = mainBatchRun config calcAccels worldStart
        mainEnd (configDumpFinal config) 
                (configPrintFinal config)
                world'


mainBatchRun config calcAccels worldStart 
 = go worldStart
 where  go !world
          = let world' = advanceWorld
                                (calcAccels $ configEpsilon config)
                                (configTimeStep config)
                                world

            in if worldSteps world' < configMaxSteps config
                        then go world'
                        else world'


-- | Called at end of run to dump final world state.
mainEnd :: Maybe FilePath       -- ^ Write final bodies to this file.
        -> Bool                 -- ^ Print final bodies to stdout
        -> World                -- ^ Final world state.
        -> IO ()

mainEnd mDumpFinal printFinal world
 = do   -- Dump the final world state to file if requested.
        maybe   (return ())  (dumpWorld world) mDumpFinal
        when    printFinal   (printWorld world)


-- Solver ---------------------------------------------------------------------
type Solver     = Double -> V.Vector MassPoint -> V.Vector Accel

-- | Nested Data Parallelism + Barnes-Hut algorithm.
calcAccels_nb   :: Solver
calcAccels_nb epsilon mpts
 = let  
        -- bounds finding isn't vectorised yet.
        (llx, lly, rux, ruy)    = findBounds mpts

        mpts'   = P.fromList $ V.toList mpts
        accels' = calcAccelsWithBoxPA epsilon llx lly rux ruy mpts'
        
   in   V.fromList $ P.toList accels'


-- | Find the coordinates of the bounding box that contains these points.
findBounds :: V.Vector MassPoint -> (Double, Double, Double, Double)
{-# INLINE findBounds #-}
findBounds bounds
 = V.foldl' acc (x1, y1, x1, y1) bounds
 where
        (x1, y1, _)     = bounds V.! 0

        acc (!llx, !lly, !rux, !ruy) (x, y, _)
         = let  !llx'   = min llx  x
                !lly'   = min lly  y
                !rux'   = max rux  x
                !ruy'   = max ruy  y
           in   (llx', lly', rux', ruy')
