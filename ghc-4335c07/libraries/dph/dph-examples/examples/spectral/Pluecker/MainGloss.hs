{-# LANGUAGE BangPatterns #-}

import Common
import qualified Common as C
import Solver
import ObjImport

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate

import System.Environment
import Data.Maybe
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as VU

import System.Random

main :: IO ()
main  
 = do   args    <- getArgs
        mainWithArgs args
        

mainWithArgs :: [String] -> IO ()
mainWithArgs [solverName,numr]
 = let  -- The solver we're using to calculate the acclerations.
        solver      = fromMaybe (error $ unlines
                                        [ "unknown solver " ++ show solverName
                                        , "choose one of "  ++ (show $ map fst solvers) ])
                        $ lookup solverName solvers
    in do
        (v,t)   <- model
        mainGloss v t (read numr) solver 400

mainWithArgs [solverName] = mainWithArgs [solverName,"1000"]

mainWithArgs _ = putStrLn "Usage: pluecker <vector|vectorised> <rays=1000>"


-- | Run the simulation in a gloss window.
mainGloss 
        :: VU.Vector Vec3
        -> VU.Vector (Int,Int,Int,Colour)
        -> Int          -- ^ number of rays
        -> Solver       -- ^ Fn to calculate accels of each point.
        -> Int          -- ^ Size of window.
        -> IO ()
        
mainGloss v t numr solver windowSize
 = let  screenSize = (640,480)
        mkray _
         = do   x <- randomRIO (0,fst screenSize)
                y <- randomRIO (0,snd screenSize)
                return (x,y)
            
        draw time
         = do   rays <- VU.generateM numr mkray
                let rays' = VU.map (rayOfScreen screenSize) rays
                let pts = solver v t rays' (realToFrac time)
                return $ Pictures $ map drawPoint $ (VU.toList $ rays `VU.zip` pts)

   in   animateIO
                (InWindow  "Raytracer"                -- window name
                           (windowSize, windowSize)   -- window size
                           (10, 10))                  -- window position
                black                                 -- background color
                draw                                  -- fn to convert a world to a picture



pointSize = 5

drawPoint :: ((Double,Double), Colour) -> Picture
drawPoint ((x,y), (cR,cG,cB))
	= Translate (c x) (c y) 
    $ Color (makeColor (c cR) (c cG) (c cB) 0.5)
    $ Polygon [ (-pointSize, -pointSize)
              , ( pointSize, -pointSize)
              , ( pointSize,  pointSize)
              , (-pointSize,  pointSize) ]
	-- $ ThickCircle (pointSize / 2) pointSize
 where c = realToFrac
