
-- | Wrappers for the various solvers.
module Solver
	( Solver
	, solvers)
where
import Common.Body

import qualified Solver.ListBH.Solver		as SolverLB

import qualified Data.Vector.Unboxed		as V
import qualified Solver.VectorBH.Solver		as SolverVB
import qualified Solver.VectorNaive.Solver	as SolverVN

import qualified Data.Array.Parallel	as P
import qualified Data.Array.Parallel.PArray	as P
import qualified Solver.NestedBH.Solver		as SolverNB

type Solver	= Double -> V.Vector MassPoint -> V.Vector Accel

solvers :: [(String, Solver)]
solvers
 = 	[ ("list-bh",		calcAccels_lb)
	, ("vector-naive",	calcAccels_vn)
	, ("vector-bh",		calcAccels_vb)
	, ("nested-bh",		calcAccels_nb) ]


-- | Lists + Barnes-Hut algorithm.
calcAccels_lb	:: Solver
calcAccels_lb epsilon mpts
	= V.fromList
	$ SolverLB.calcAccels epsilon
	$ V.toList mpts


-- | Vector + Naive algorithm.
calcAccels_vn	:: Solver
calcAccels_vn epsilon
	= SolverVN.calcAccels epsilon 
	

-- | Vector + Barnes-Hut algorithm.
calcAccels_vb 	:: Solver
calcAccels_vb epsilon mpts
	= SolverVB.calcAccels epsilon mpts


-- | Nested Data Parallelism + Barnes-Hut algorithm.
calcAccels_nb	:: Solver
calcAccels_nb epsilon mpts
 = let	
	-- bounds finding isn't vectorised yet.
	(llx, lly, rux, ruy)	= SolverVB.findBounds mpts

	mpts'	= P.fromList $ V.toList mpts
	accels'	= SolverNB.calcAccelsWithBoxPA epsilon llx lly rux ruy mpts'
	
   in	V.fromList $ P.toList accels'