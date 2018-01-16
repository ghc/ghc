{-# LANGUAGE ParallelListComp, BangPatterns #-}

import Solver

import Graphics.Gloss

import System.Environment
import Data.Maybe
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as VU


main :: IO ()
main  
 = do   args    <- getArgs
        mainWithArgs args
        

mainWithArgs :: [String] -> IO ()
mainWithArgs [solverName,depthStr]
 = let  -- The solver we're using to calculate the acclerations.
        solver      = fromMaybe (error $ unlines
                                        [ "unknown solver " ++ show solverName
                                        , "choose one of "  ++ (show $ map fst solvers) ])
                        $ lookup solverName solvers
        
        depth = read depthStr
    in  mainGloss depth solver 400

mainWithArgs [solverName] = mainWithArgs [solverName,"4"]

mainWithArgs _ = putStrLn "Usage: rotations <vector|vectorised> <depth>"


-- | Run the simulation in a gloss window.
mainGloss 
        :: Int          -- ^ Depth
        -> Solver       -- ^ Fn to calculate accels of each point.
        -> Int          -- ^ Size of window.
        -> IO ()
        
mainGloss depth solver windowSize
 = let  draw t
         = let  pts = solver depth (realToFrac t)
           in   Color white $ Pictures $ map drawPoint $ VU.toList pts

   in   animate 
                (InWindow  "Silly"                    -- window name
                           (windowSize, windowSize)   -- window size
                           (10, 10))                  -- window position
                black                                 -- background color
                draw                                  -- fn to convert a world to a picture



pointSize = 4

drawPoint :: (Double, Double) -> Picture
drawPoint (x, y)
	= Translate (realToFrac x * 50) (realToFrac y * 50) 
	$ ThickCircle (pointSize / 2) pointSize
