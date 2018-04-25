
-- | Wrappers for the various solvers.
module Solver
	( Solver
	, solvers)
where
import qualified Vector		    as SV
import qualified Vectorised		as SPA

import qualified Data.Vector        		as V
import qualified Data.Vector.Unboxed		as VU

import qualified Data.Array.Parallel	as P
import qualified Data.Array.Parallel.PArray	as P

type Solver	= Int -> Double -> VU.Vector (Double,Double)

solvers :: [(String, Solver)]
solvers
 = 	[ ("vectorised",		solver_spa)
    , ("vector",		    SV.solveV)
    ]

-- | Nested Data Parallelism + Barnes-Hut algorithm.
solver_spa	:: Solver
solver_spa depth time
 = let	
	pts'	= SPA.solvePA depth time
   in	VU.fromList $ P.toList pts'

