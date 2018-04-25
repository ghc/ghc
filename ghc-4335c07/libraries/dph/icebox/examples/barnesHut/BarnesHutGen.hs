{-# LANGUAGE TypeOperators #-}
module BarnesHutGen where

import Monad   (liftM)

import List   (nubBy)
import IO
import System (ExitCode(..), getArgs, exitWith)
import Random (Random, RandomGen, getStdGen, randoms, randomRs)
import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Base ( (:*:)(..) )

type Vector = (Double :*: Double) 

type Point     = Vector
type Accel     = Vector
type Velocity  = Vector
type MassPoint = Point :*: Double
type Particle  = MassPoint :*: Velocity

type BoundingBox = Point :*: Point

type  BHTree      = [BHTreeLevel]
type  BHTreeLevel = (UArr MassPoint, USegd) -- centroids

epsilon = 0.05
eClose  = 0.5

-- particle generation
-- -------------------

randomTo, randomFrom :: Integer
randomTo    = 2^30
randomFrom  = - randomTo

randomRIOs       :: Random a => (a, a) -> IO [a]
randomRIOs range  = liftM (randomRs range) getStdGen 

randomIOs :: Random a => IO [a]
randomIOs  = liftM randoms getStdGen 

--  generate a stream of random numbers in [0, 1)
--
randomDoubleIO :: IO [Double]
randomDoubleIO  = randomIOs

-- generate an infinite list of random mass points located with a homogeneous
-- distribution around the origin within the given bounds
--
randomMassPointsIO       :: Double -> Double -> IO [MassPoint]
randomMassPointsIO dx dy  = do
			    rs <- randomRIOs (randomFrom, randomTo)
			    return (massPnts rs)
		            	  where
			    to    = fromIntegral randomTo
			    from  = fromIntegral randomFrom
			    xmin  = - (dx / 2.0)
			    ymin  = - (dy / 2.0)
			    xfrac = (to - from) / dx
			    yfrac = (to - from) / dy

			    massPnts               :: [Integer] -> [MassPoint]
			    massPnts (xb:yb:mb:rs)  = 
			      ((x :*: y) :*: m) : massPnts rs
			      where
				m = (fromInteger . abs) mb + epsilon
				x = xmin + (fromInteger xb) / xfrac
				y = ymin + (fromInteger yb) / yfrac

-- The mass of the generated particle cloud is standardized to about 
-- 5.0e7 g/m^2.  The mass of individual particles may deviate by a factor of
-- ten from the average.
--
smoothMass           :: Double -> Double -> [MassPoint] -> [MassPoint]
smoothMass dx dy mps  = let
			  avmass = 5.0e7
			  area   = dx * dy
			  middle = avmass * area / fromIntegral (length mps)
			  range  = fromIntegral (randomTo - randomFrom)
			  factor = (middle * 10 - middle / 10) / range

			  adjust (xy :*: m) = 
			    xy :*: (middle + factor * m)
			in
			  map adjust mps

-- Given the number of particles to generate and the horizontal and vertical
-- extensions of the area where the generated particles should occur, generate
-- a particle set according to a function specific strategy.
--
asymTwinParticles, 
  sphereParticles, 
  plummerParticles, 
  homParticles    :: Int -> Double -> Double -> IO ([Particle])

asymTwinParticles n dx dy = error "asymTwinPrticles not implemented yet\n"

sphereParticles n dx dy = 
  do
    let rad = dx `min` dy
    mps <- randomMassPointsIO dx dy
    return ((  map (\mp -> mp :*: (0.0 :*: 0.0))
	     . smoothMass dx dy
	     . head 
	     . filter ((== n) . length) 
	     . map fst 
	     . iterate refine
	    )  ([], filter (inside rad) mps)
	   )
  where
    --
    -- move suitable mass points from the second list to the first (i.e., those
    -- not conflicting with points that are already in the first list)
    --
    refine :: ([MassPoint], [MassPoint]) -> ([MassPoint], [MassPoint])
    refine (ds, rs) = let
		        (ns, rs') = splitAt (n - length ds) rs
		      in
		        (nubMassPoints (ds ++ ns), rs')

    -- check whether inside the given radius
    --
    inside                          :: Double -> MassPoint -> Bool
    inside rad ((dx :*: dy) :*: _)  = dx * dx + dy * dy <= rad * rad

plummerParticles n _ _ =
  do
    rs <- randomDoubleIO
    return ((   normalize
	      . head 
	      . filter ((== n) . length) 
	      . map fst 
	      . iterate refine
	     ) ([], particles rs)
	    )
  where
    particles (w:preY:rs') = let
			       s_i = rsc * r_i
			       rsc = (3 * pi) / 16
			       r_i = sqrt' ((0.999 * w)`power`(-2/3) - 1)
			       --
			       u_i = vsc * v_i
			       vsc = 1 / sqrt rsc
			       v_i = (x * sqrt 2) / (1 + r_i^2)**(1/4)
			       --
			       (pos :*: rs''' ) = rndVec s_i rs''
			       (vel :*: rs'''') = rndVec u_i rs'''
			     in
			     ((pos :*: m) :*: vel) : particles rs''''
			     where
			       y	 = preY / 101
						  -- !!!should be 10, but then
						  -- !!!findX gets problems
			       (x, rs'') = findX y rs'
			       --
			       m         = 1 / fromIntegral n
			       --
			       x`power`y | x == 0.0  = 0.0
					 | otherwise = x**y
			       sqrt' x   | x < 0     = 0
					 | otherwise = sqrt x

    findX :: Double -> [Double] -> (Double, [Double])
    findX y (x:rs) | y <= x^2 * (1 - x^2)**(7/2) = (x, rs)
		   | otherwise			  = findX y rs

    rndVec len (x:y:rs) = let r = len / sqrt (x^2 + y^2)
			  in
			  ((r * x :*: r * y) :*: rs)

    -- move suitable mass points from the second list to the first (i.e., those
    -- not conflicting with points that are already in the first list)
    --
    refine :: ([Particle], [Particle]) -> ([Particle], [Particle])
    refine (ds, rs) = let
		        (ns, rs') = splitAt (n - length ds) rs
		      in
		        (nubParticles (ds ++ ns), rs')

    -- translate positions and velocities such that they are at the origin
    --
    normalize    :: [Particle] -> [Particle]
    normalize ps  = 
      let (dx :*: dy) :*: _       = centroid [mp | mp :*: _  <- ps]
	  ((dvx:*: dvy) :*: _)    = totalMomentum ps
      in
      (map (translateVel (-dvx :*: -dvy)) . map (translate (-dx :*: -dy))) ps


homParticles n dx dy = 
  do
    mps <- randomMassPointsIO dx dy
    return ((  map (\mp -> mp :*: (0.0 :*: 0.0))
	     . smoothMass dx dy
	     . head 
	     . filter ((== n) . length) 
	     . map fst 
	     . iterate refine
	    )  ([], mps)
	   )
  where
    --
    -- move suitable mass points from the second list to the first (i.e., those
    -- not conflicting with points that are already in the first list)
    --
    refine :: ([MassPoint], [MassPoint]) -> ([MassPoint], [MassPoint])
    refine (ds, rs) = let
		        (ns, rs') = splitAt (n - length ds) rs
		      in
		        (nubMassPoints (ds ++ ns), rs')


-- Drop all mass points that are too close to another.
--
nubMassPoints :: [MassPoint] -> [MassPoint]
nubMassPoints  = nubBy (\(p1 :*: _) (p2 :*: _) -> epsilonEqual p1 p2)

-- Same for particles.
--
nubParticles :: [Particle] -> [Particle]
nubParticles  = nubBy (\((p1 :*: _) :*: _) ->
                        \((p2 :*: _) :*: _) -> epsilonEqual p1 p2)


-- Test whether the Manhattan distance between two points is smaller than
-- `epsilon'. 
--
epsilonEqual                    :: Point -> Point -> Bool
epsilonEqual  (x1 :*: y1) (x2 :*: y2)  = abs (x1 - x2) + abs (y1 - y2) < epsilon


--  Calculates the centroid of a list of mass points. 
--
centroid     :: [MassPoint] -> MassPoint
centroid mps  = let
		  m          = sum [m | _ :*:  m <- mps]
		  (wxs, wys) = unzip [(m * x, m * y) | (x :*: y) :*: m <- mps]
		in
		  ((sum wxs / m) :*: (sum wys / m)) :*: m
--  Calculates the total momentum.
--
totalMomentum    :: [Particle] -> (Point :*: Double)
totalMomentum ps  = 
  let
    m          = sum [m | ((_ :*: m) :*: _) <- ps]
    (wxs, wys) = unzip [(m * x, m * y) | (_ :*: m) :*: (x:*: y) <- ps]
  in
    ((sum wxs / m :*: sum wys / m) :*: m)

-- translate a particle
--
translate :: Point -> Particle -> Particle
translate (dx :*: dy) (((x :*: y) :*: m) :*: vxy) =
  ((x + dx :*: y + dy) :*: m) :*: vxy

-- translate the velocity of particle
--
translateVel :: Point -> Particle -> Particle
translateVel (dvx :*: dvy) (mp :*: (vx :*: vy)) =
  mp :*: (vx + dvx :*: vy + dvy)



showBHTree:: BHTree -> String
showBHTree treeLevels = "Tree:" ++ concat (map showBHTreeLevel treeLevels)

showBHTreeLevel (massPnts, cents) = "\t" ++ show massPnts ++ "\n\t" ++
                                     show cents   ++ "\n" ++ "\t\t|\n\t\t|\n"
