{-# LANGUAGE PatternGuards #-}

import qualified QuickHullVector
import qualified QuickHullIO
import qualified QuickHullSplit
import Points2D.Types
import Points2D.Generate
import Timing
import SVG

import System.Environment
import Data.Function
import qualified Data.Vector.Unboxed	as V
import Data.Vector.Unboxed		(Vector)
import Control.Monad

algs = 	[ ("vector",	(\v -> return $ QuickHullVector.quickHull v))
	, ("io",	QuickHullIO.quickHull)
	, ("split",	(\v -> return $ QuickHullSplit.quickHull v)) ]

parseArgs args
	| [alg, strCount]	<- args
	, Just fun 		<- lookup alg algs
	= Just (fun, read strCount, Nothing)

	| [alg, strCount, file]	<- args
	, Just fun 		<- lookup alg algs
	= Just (fun, read strCount, Just file)

	| otherwise
	= Nothing


main :: IO ()
main
 = do	argStrs		<- getArgs
	case parseArgs argStrs of
	 Just args	-> run args
	 _ 		-> putStr $ unlines
				[ "usage: quickhull <alg> <points> [out.svg]"
				, "   algs: " ++ (show $ map fst algs) ++ "\n" ]

run ::	( Vector Point -> IO (Vector Point)
	, Int
	, Maybe FilePath)
    ->	IO ()
	
run (fun, pointCount, mFileSVG) 
 = do
	let vPoints	= genPointsDisc pointCount (400, 400) 350 

	-- Force points to create the input vector.
	V.force vPoints `seq` return ()

	-- Compute the convex hull.
	(vHull, tElapsed)
	 	<- time
		$  do	vHull	<- fun vPoints
		     	vHull `seq` return vHull
		
	-- Print how long it took	
	putStr $ prettyTime tElapsed

	-- If we were asked for an SVG then write it out to file.
	maybe 	(return ())
	 	(\fileSVG -> 
			writeFile fileSVG
			 $ makeSVG 	(roundPoints $ V.toList vPoints) 
					(roundPoints $ V.toList vHull))
		mFileSVG

