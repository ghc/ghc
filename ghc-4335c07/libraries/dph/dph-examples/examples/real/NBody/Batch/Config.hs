
module Batch.Config
	( Config(..)
	, loadConfig)
where
import Batch.MainArgs
import System.Console.ParseArgs
import Data.Maybe


-- | Program config
data Config
	= Config {
	-- What solver to use
	  configSolverName	:: String

	-- System setup
	, configBodyCount	:: Int
	, configBodyMass	:: Double
	, configTimeStep	:: Double
	, configEpsilon		:: Double

	-- Initial conditions.
	, configStartDiscSize	:: Double
	, configStartSpeed	:: Double

	-- Terminating conditions.
	, configMaxSteps	:: Int
	
	-- Dump points to file
	, configDumpFinal	:: Maybe FilePath 

        -- Print points to stdout
	, configPrintFinal      :: Bool
	
	-- Print timings to stdout
	, configPrintTimings    :: Bool }
	

-- | Load program config from its command line arguments.	
loadConfig :: Args MainArg -> Config
loadConfig args
 = let	Just solverName	= getArgString	args ArgSolver
	Just timeStep	= getArgDouble	args ArgTimeStep
	Just bodyCount	= getArgInt	args ArgBodyCount
	Just bodyMass	= getArgDouble  args ArgBodyMass
	Just epsilon	= getArgDouble	args ArgEpsilon
	Just discSize	= getArgDouble	args ArgDiscSize
	Just startSpeed	= getArgDouble	args ArgStartSpeed

	Just maxSteps	= getArgInt	args ArgMaxSteps
	mFilePath	= getArgString	args ArgDumpFinal

   in	Config
	{ configSolverName	= solverName
	, configBodyCount	= bodyCount
	, configBodyMass	= bodyMass
	, configTimeStep	= timeStep
	, configEpsilon		= epsilon
	, configStartDiscSize	= discSize
	, configStartSpeed	= startSpeed
	, configMaxSteps	= maxSteps
	, configDumpFinal	= mFilePath 
	, configPrintFinal      = gotArg args ArgPrintFinal 
	, configPrintTimings    = gotArg args ArgPrintTimings }