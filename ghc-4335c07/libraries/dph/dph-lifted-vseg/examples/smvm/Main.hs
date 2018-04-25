{-# LANGUAGE ScopedTypeVariables,
        NoMonomorphismRestriction,
        TypeFamilies #-}

import Timing
import SMVMVectorised
import System.IO
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Array.Parallel.PArray
import Data.Array.Parallel.PArray.PData
import qualified Data.Array.Parallel.PArray	as P
import qualified Data.Array.Parallel.Unlifted   as U
import System.Environment
import Control.Exception (evaluate)
import Text.PrettyPrint

-------------------
{-
matrix :: PArray (PArray (Int, Double))
matrix  = P.fromListPA  [ P.fromListPA [(1, 1), (10, 5), (80, 20)]
                        , P.fromListPA [(1, 2), (20, 6), (90,  30)] ]

vector  :: PArray Double
vector  = P.fromListPA    [0..99 :: Double]

ex_smvm = smvmPA matrix vector
-}


------------------
main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [fileName] -> run fileName
	  _	     -> usage
	
usage	
 = putStr $ unlines
	[ "usage: smvm <file>" ]

run fileName
 = do	(matrix, vector) <- loadPA fileName

	matrix `seq` return ()
	vector `seq` return ()

	-- Multiply sparse matrix by the dense vector.
	(vResult, tElapsed)
	 <- time $ let result	= smvmPA matrix vector
		   in  P.nfPA result `seq` return result
					
	-- Print how long it took.
	putStr $ prettyTime tElapsed

	-- Print some info about the test setup.
	putStrLn $ "vector length   = " ++ show (P.lengthPA (vector :: PArray Double))

	-- Print checksum of resulting vector.
	putStrLn $ "result sum      = " ++ show (U.sum (P.toUArrayPA vResult))


-- | Load a test file containing a sparse matrix and dense vector.
loadPA 	:: String 				-- ^ filename.
	-> IO  ( PArray (PArray (Int, Double))	-- sparse matrix
	       , PArray Double)			-- dense vector

loadPA fileName
 = do 	(segd, arrMatrixElems, arrVector) <- loadUArr fileName

    	let paMatrix	= P.nestUSegdPA segd (P.fromUArray2PA arrMatrixElems)

	let paVector :: PArray Double
	    paVector	= P.fromUArrayPA arrVector

	return (paMatrix, paVector)


-- | Load a test file containing a sparse matrix and dense vector.
loadUArr :: String				-- ^ filename
	 -> IO ( U.Segd				-- segment descriptor saying what array elements
						--    belong to each row of the matrix.
	       , U.Array (Int, Double)		-- column indices and matrix elements
	       , U.Array Double)		-- the dense vector

loadUArr fname 
 = do	h <- openBinaryFile fname ReadMode

	-- check magic numbers at start of file to guard against word-size screwups.
	alloca $ \ptr -> do
		hGetBuf h ptr (sizeOf (undefined :: Int))
		magic1 :: Int	<- peek ptr
		hGetBuf h ptr (sizeOf (undefined :: Int))
		magic2	:: Int <- peek ptr
		if magic1 == 0xc0ffee00 Prelude.&& magic2 == 0x12345678 
			then return ()
			else error $ "bad magic in " ++ fname

	-- number of elements in each row of the matrix.
	lengths <- U.hGet h

	-- indices of all the elements.
	indices <- U.hGet h

	-- values of the matrix elements.
	values  <- U.hGet h

	-- the dense vector.
	vector  <- U.hGet h

	evaluate lengths
	evaluate indices
	evaluate values
	evaluate vector

	let segd    = U.lengthsToSegd lengths
	    matrix  = U.zip indices values

	return (segd, matrix, vector)
