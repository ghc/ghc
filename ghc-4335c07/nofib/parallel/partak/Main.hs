-- tak benchmark program 
-- Divide-and-conquer structure with tertiary parallelism.
-----------------------------------------------------------------------------

module Main(main) where

import System.Environment (getArgs)
import Control.Parallel

main = do [x,y,z] <- map read `fmap` getArgs
          let res = partak x y z
          putStrLn ("tak " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " = " ++ (show res))

partak :: Int -> Int -> Int -> Int
partak x y z 
    | x <= y     = z
    | otherwise  = x' `par` y' `par` z' `par`
                   res
                   where res = partak x' y' z'
                         x' = partak (x-1) y z
                         y' = partak (y-1) z x
                         z' = partak (z-1) x y
                         -- g =  gran x y z	 
                         -- gran x y z = abs (z-y) + abs (y-x) + abs (z-x)
