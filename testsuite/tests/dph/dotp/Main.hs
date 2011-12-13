import DotPVect ( dotp )

import Control.Exception (evaluate)
import System.Console.GetOpt
import qualified System.Random as R

import qualified Data.Array.Parallel.Unlifted as U
import qualified Data.Array.Parallel.PArray   as P
import Data.Array.Parallel.PArray (PArray)



generateVectorU :: Int -> IO (U.Array Double)
generateVectorU n =
  do
    let seed = 42742
    let rg   = R.mkStdGen seed
    let -- The std random function is too slow to generate really big vectors
        -- with.  Instead, we generate a short random vector and repeat that.
        randvec = U.randomRs k (-100, 100) rg
        vec     = U.map (\i -> U.index "generateVectorU" randvec (i `mod` k)) (U.enumFromTo 0 (n-1))
    evaluate vec
    return vec
  where
    k = 1000

generateVector :: Int -> IO (PArray Double)
generateVector n 
  = do
      vec <- generateVectorU n
      return $ P.fromUArray vec

generateVectors :: Int -> IO (PArray Double, PArray Double)
generateVectors n =
  do
    v <- generateVector n
    w <- generateVector n
    return (v,w)

main 
 = do	-- compute dot product with NDP
	vectors			<- generateVectors 100000
	let resultViaNDP	= (uncurry dotp) vectors

	-- compute with lists
	let (aVecX, aVecY) = vectors
	let vecX		= P.toList aVecX
	let vecY		= P.toList aVecY
	let resultViaList	= sum $ zipWith (*) vecX vecY

	-- ignore wibbles in low order bits
	putStr $ (take 12 $ show resultViaNDP) ++ "\n"
	putStr $ (take 12 $ show resultViaList) ++ "\n"
	
