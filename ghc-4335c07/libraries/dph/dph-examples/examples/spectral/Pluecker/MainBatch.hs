{-# LANGUAGE BangPatterns #-}

import Common
import qualified Common as C
import Solver
import ObjImport

import Timing

import System.Environment
import Data.Maybe
import Data.Word
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as VU

import qualified Data.ByteString as B
import Codec.BMP

main :: IO ()
main  
 = do   args    <- getArgs
        mainWithArgs args
        

mainWithArgs :: [String] -> IO ()
mainWithArgs [solverName,mx,my]
 = let  -- The solver we're using to calculate the acclerations.
        solver      = fromMaybe (error $ unlines
                                        [ "unknown solver " ++ show solverName
                                        , "choose one of "  ++ (show $ map fst solvers) ])
                        $ lookup solverName solvers
    in do
        (!v,!t)   <- model
        let !rays = VU.generate (width*height) mkray

        (!result,!tm) <- time
         $ do   let !pts = solver v t rays 0
                return pts

        putStrLn (prettyTime tm) 

        putStrLn "writing image"
        output width height result
                
 where
    mkray i
     = let (y,x) = i `divMod` width
           fi = fromIntegral
       in  rayOfScreen (fi width, fi height) (fi x, fi y)

    width       = read mx
    height      = read my
mainWithArgs [solverName] = mainWithArgs [solverName,"640","480"]

mainWithArgs _ = putStrLn "Usage: pluecker <vector|vectorised> <x=640> <y=480>"



output :: Int -> Int -> VU.Vector Colour -> IO ()
output width height v
 = do   let words = VU.concatMap mkwords v
        let rgba  = B.pack (VU.toList words)
        let bmp   = packRGBA32ToBMP width height rgba
        writeBMP "out.bmp" bmp
 where

  mkwords (r,g,b) = VU.fromList [w r, w g, w b, 0]
  w c = truncate (c * 255) :: Word8


