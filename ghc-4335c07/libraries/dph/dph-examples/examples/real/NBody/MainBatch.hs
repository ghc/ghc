{-# LANGUAGE ParallelListComp, BangPatterns #-}

import Batch.MainArgs
import Batch.Config
import Common.Dump
import Common.World
import Common.Body
import Common.Util
import Solver
import Timing
import Points2D.Generate
import System.Environment
import System.Console.ParseArgs
import System.IO.Unsafe
import Control.Monad
import Data.Maybe
import qualified Data.Vector.Unboxed		as V


main :: IO ()
main  
 = do	args	<- parseArgsIO ArgsComplete mainArgs

	when (gotArg args ArgHelp)
	 $ usageError args ""

	mainWithArgs args
	

mainWithArgs :: Args MainArg -> IO ()
mainWithArgs args
 = let	config		= loadConfig args

	-- The solver we're using to calculate the acclerations.
	solverName	= configSolverName config
	calcAccels	= fromMaybe (error $ unlines
					[ "unknown solver " ++ show solverName
					, "choose one of "  ++ (show $ map fst solvers) ])
			$ lookup solverName solvers
	
	-- Setup initial world
	vPoints 	= genPointsDisc 
				(configBodyCount config)
	 			(0, 0) 
				(configStartDiscSize config)

	vBodies		= V.map (setStartVelOfBody $ configStartSpeed config)
			$ V.map (setMassOfBody     $ configBodyMass   config)
			$ V.map (uncurry unitBody) 
			$ vPoints

	worldStart	= World
			{ worldBodies	= vBodies
			, worldSteps	= 0 }

    in	mainBatch config calcAccels worldStart 


-- | Run the simulation in batch mode.
mainBatch :: Config -> Solver -> World -> IO ()
mainBatch config calcAccels worldStart
 = do
	worldStart `seq` return ()

	(world', tElapsed)
		<- time 
		$  let 	world	= mainBatchRun config calcAccels worldStart
		   in	world `seq` return world
	
	when (configPrintTimings config)
	 $ putStr $ prettyTime tElapsed

	mainEnd (configDumpFinal config) 
	        (configPrintFinal config)
	        world'


mainBatchRun config calcAccels worldStart 
 = go worldStart
 where	go !world
 	  = let world' = advanceWorld
				(calcAccels $ configEpsilon config)
				(configTimeStep config)
				world

	    in if worldSteps world' < configMaxSteps config
			then go world'
			else world'


-- | Called at end of run to dump final world state.
mainEnd :: Maybe FilePath	-- ^ Write final bodies to this file.
        -> Bool                 -- ^ Print final bodies to stdout
	-> World		-- ^ Final world state.
	-> IO ()

mainEnd mDumpFinal printFinal world
 = do	-- Dump the final world state to file if requested.
	maybe 	(return ())  (dumpWorld world) mDumpFinal
	when    printFinal   (printWorld world)


