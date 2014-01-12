{-# LANGUAGE TypeOperators #-}
module Generate 
        ( genPointsUniform
        , genPointsUniformWithSeed
        , genPointsDisc
        , genPointsCombo
        , pointsPArrayOfUArray )
where
import Types
import Randomish
import qualified Data.Array.Parallel.Unlifted       as U
import qualified Data.Array.Parallel        as P
import qualified Data.Array.Parallel.PArray         as P
import Data.Array.Parallel.PArray                   (PArray)
import Control.Exception

-- Random points generation
-- IMPORTANT: We use the same seed with the same random generator in all
--            quickhull codes.  The asymptotic work complexity of quickhull
--            is between O (N) and O (N^2) depending on the input.
--            To compare benchmark results, they always need to use the same
--            input.
seed :: Int
seed    = 42742

-- | Some uniformly distributed points
genPointsUniform 
        :: Int                  -- ^ number of points
        -> Double               -- ^ minimum coordinate
        -> Double               -- ^ maximum coordinate
        -> U.Array (Double, Double)

genPointsUniform n pointMin pointMax
 = let  pts             = randomishDoubles (n*2) pointMin pointMax seed
        xs              = U.extract pts 0 n
        ys              = U.extract pts n n
   in   U.zip xs ys


-- | Some uniformly distributed points
genPointsUniformWithSeed
        :: Int                  -- ^ seed
        -> Int                  -- ^ number of points
        -> Double               -- ^ minimum coordinate
        -> Double               -- ^ maximum coordinate
        -> U.Array (Double, Double)

genPointsUniformWithSeed seed' n pointMin pointMax
 = let  pts             = randomishDoubles (n*2) pointMin pointMax seed'
        xs              = U.extract pts 0 n
        ys              = U.extract pts n n
   in   U.zip xs ys


-- | Some points distributed as a disc
genPointsDisc
        :: Int                  -- ^ number of points
        -> (Double, Double)     -- ^ center of disc
        -> Double               -- ^ radius of disc
        -> U.Array (Double, Double)

genPointsDisc n (originX, originY) radiusMax
 = let  radius = randomishDoubles n 0     radiusMax seed
        angle  = randomishDoubles n (-pi) pi        (seed + 1234)

        makeXY r a      
                = ( originX + r * cos a
                  , originY + r * sin a)        

    in  originX `seq` originY `seq` U.zipWith makeXY radius angle


-- | A point cloud with areas of high an low density
genPointsCombo 
        :: Int                  -- ^ number of points
        -> U.Array (Double, Double)

genPointsCombo n
        =  genPointsDisc    (n `div` 5) (250, 250) 200
        U.+:+ genPointsDisc (n `div` 5) (100, 100) 80 
        U.+:+ genPointsDisc (n `div` 5) (150, 300) 30 
        U.+:+ genPointsDisc (n `div` 5) (500, 120) 30 
        U.+:+ genPointsDisc (n `div` 5) (300, 200) 150


-- | Convert a list of points to a PArray
pointsPArrayOfUArray 
        :: U.Array (Double, Double)
        -> IO (PArray Point)

pointsPArrayOfUArray ps
  = do
      let pts = makePointsPA 
                        (P.fromUArray (U.fsts ps))
                        (P.fromUArray (U.snds ps))
      evaluate $ P.nf pts
      return pts

