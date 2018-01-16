-- Time-stamp: <2010-11-03 09:27:34 simonmar>
-- $Id: Matrix.hs,v 1.4.2.5 2002/06/15 01:34:29 hwloidl Exp $

-- Data Encapsulation of the ADT Matrix.
-- Internal representation is a list of lists.
--
-- LinSolv remark: default determinant is parallel (d&c over 1st line)
-----------------------------------------------------------------------------

-- @node ADT Matrix, , ,
-- @chapter ADT Matrix

module Matrix(SqMatrix, Vector, {- MatBounds, -}
              (!!-), (!-), sqMatrix, 
              vecBounds, matBounds, vecCont, matCont,
              listSqMatrix, lolSqMatrix, unSqMatrix, vector, unvector,
              determinant, transp, replaceColumn, size,
              maxElem, maxElemVec, scalarMult, vecScalarQuot, 
              matGcd, vecGcd, matHom, vecHom, matBounds, matCont,
	       matMult, matCompact
              ) 
              {- showsMatrix, matEqual, matSum, matMult)
                 matSum',matSum'',showIt,makeUnique) -}                   where

-- @menu
-- * Imports::			
-- * Data Types::		
-- * Constants::		
-- * Aux functions::		
-- * Data type constructors::	
-- * H.o. fcts::		
-- * Misc operations::		
-- * Arithmetic Operations::	
-- * I/O Operations::		
-- * Instances::		
-- @end menu

-- @node Imports, Data Types, ADT Matrix, ADT Matrix
-- @section Imports

import Data.List(transpose)
import Data.Array

import ModArithm ({- Hom(hom), -} modHom)
-- only needed if we use array based LU Decomp later

#if defined(STRATEGIES)
import Control.Parallel.Strategies
#endif
import Control.DeepSeq

infixl 5 !!-
infixl 5 !-

m!!-(i,j) = (m'!!i')!!j'
            where bds@((rl,cl),(rh,ch)) = matBounds m
                  i' = i - rl
                  j' = j - cl
                  m' = matCont m

v!-i = v'!!i'
       where bds@(rl,rh) = vecBounds v
             i' = i - rl
             v' = vecCont v


-- ----------------------------------------------------------------------------
-- @node Data Types, Constants, Imports, ADT Matrix
-- @section Data Types
--
-- Data Type definitions
-- ----------------------------------------------------------------------------

data  (Integral a)  =>  SqMatrix a = SqMatrixC MatBounds [[a]]
data  (Integral a)  =>  Vector a   = VectorC VecBounds [a]

type  MatBounds = ((Int,Int),(Int,Int))
type  VecBounds = ((Int),(Int))

instance (NFData a, Integral a) => NFData (Vector a) where
  -- rnf x@(VectorC b l) = rnf b >> rnf l >>  return x
  rnf (VectorC b l) = rnf b `seq` rnf l

instance (NFData a, Integral a) => NFData (SqMatrix a) where
  -- rnf x@(SqMatrixC b m) = rnf b >> rnf m >> return x
  rnf (SqMatrixC b m) = rnf b `seq` rnf m

-- ----------------------------------------------------------------------------
-- @node Aux functions, Data type constructors, Constants, ADT Matrix
-- @section Aux functions
-- ----------------------------------------------------------------------------

lol :: (Integral a) => Int -> [a] -> [[a]]
lol _ [] = []
lol n l = let 
	    (line, rest) = splitAt n l
          in
	    line : (lol n rest)


mat_map = map

listCompwiseComp :: (a -> b -> c) -> [a] -> [b] -> [c]
listCompwiseComp = zipWith 
			  		{- map f' (zip l l')
			  		where f' (a,b) = a `f` b -}

-- ----------------------------------------------------------------------------
-- @node Data type constructors, H.o. fcts, Aux functions, ADT Matrix
-- @section Data type constructors
-- ----------------------------------------------------------------------------

sqMatrix :: (Integral a)  =>  Array (Int,Int) a -> SqMatrix a
sqMatrix arr = SqMatrixC b [ [ (arr!(i,j)) | j <- [jLo..jHi] ]
                           | i <- [iLo..iHi] ] 
	       where b@((iLo,jLo),(iHi,jHi)) = (bounds arr)

unSqMatrix :: (Integral a)  =>  SqMatrix a -> Array (Int,Int) a 
unSqMatrix (SqMatrixC b@((iLo,jLo),(iHi,jHi)) m)  
 = array b (concat [ [ ((i,j), (m!!(i-1))!!(j-1)) | j <- [jLo..jHi] ]
                   | i <- [iLo..iHi] ])

listSqMatrix :: (Integral a)  =>  MatBounds -> [a] -> SqMatrix a
listSqMatrix b@((iLo,jLo),(iHi,jHi)) l = SqMatrixC b (take m (lol n l))
					 where m = iHi - iLo +1
					       n = jHi - jLo + 1

lolSqMatrix :: (Integral a)  =>  MatBounds -> [[a]] -> SqMatrix a
lolSqMatrix b l = SqMatrixC b l

matBounds (SqMatrixC b _) = b
matCont   (SqMatrixC _ m) = m

vector :: (Integral a)  =>  [a] -> Vector a
vector l = VectorC ((1),(n)) l
           where n = length l

vecBounds (VectorC b _) = b
vecCont   (VectorC _ v) = v

unvector :: (Integral a)  =>  Vector a -> Array (Int) a
unvector (VectorC b@(x,y) l) = array b (zip [x..y] l)

-- ----------------------------------------------------------------------------
-- @node H.o. fcts, Misc operations, Data type constructors, ADT Matrix
-- @section H.o. fcts
--
-- Mapping and other general operations
-- ----------------------------------------------------------------------------

#if defined(STRATEGIES)
matMapUnary :: (Integral a, NFData a)  =>  
#else
matMapUnary :: (Integral a)  =>  
#endif
               (a -> a) -> SqMatrix a -> SqMatrix a

matMapUnary f (SqMatrixC b mat) =
	SqMatrixC b (mat_map (mat_map f) mat)

matCompwiseComp :: (Integral a, Integral b, Integral c
#if defined(STRATEGIES)
		      ,NFData a, NFData b, NFData c
#endif
                   )  =>  
                   (a -> b -> c) -> SqMatrix a -> SqMatrix b -> SqMatrix c
matCompwiseComp f (SqMatrixC bnds@((iLo,jLo),(iHi,jHi)) mat) (SqMatrixC bnds' mat') = 
       if (bnds==bnds')
         then SqMatrixC bnds [ listCompwiseComp f (mat!!(k-1)) (mat'!!(k-1))
			     | k <- [iLo..iHi] ]
         else error "matCompwiseComp: Matrices have different bounds\n"

#if defined(STRATEGIES)
matFold :: (Integral a, NFData a)  =>  (a -> a -> a) -> a -> SqMatrix a -> a
#else
matFold :: (Integral a)  =>  (a -> a -> a) -> a -> SqMatrix a -> a
#endif
matFold f init (SqMatrixC _ mat) = foldl f init (mat_map (foldl f init) mat)


#if defined(STRATEGIES)
vecFold :: (Integral a, NFData a)  =>  (a -> a -> a) -> a -> Vector a -> a
#else
vecFold :: (Integral a)  =>  (a -> a -> a) -> a -> Vector a -> a
#endif
vecFold f init (VectorC _ mat) = foldl f init mat 

-- ----------------------------------------------------------------------------
-- @node Misc operations, Arithmetic Operations, H.o. fcts, ADT Matrix
-- @section Misc operations
--
-- Misc operations
-- ----------------------------------------------------------------------------
 
-- Just for testing; demands computation of all elems of the matrix

matCompact x = matFold max 0 (matMapUnary signum x)

-- ---------------------------------------------------------------------------

size :: (Integral a)  =>  SqMatrix a -> Int
size (SqMatrixC ((iLo,jLo),(iHi,jHi)) mat) = 
			if (iLo==jLo) && (iHi==jHi)
                         then iHi-iLo+1
                         else error "size: Matrix doesn't have size ((1,1),(n,n))\n"


-- replaceColumn :: (Ix a, Ix b) => a -> Array (a,b) c -> Array b c -> Array (a,b) c

replaceColumn :: (Integral a) => Int -> SqMatrix a -> Vector a -> SqMatrix a

-- This is definitely more elegant. But is it as efficient?

replaceColumn j (SqMatrixC b m)(VectorC _ v) = 
		SqMatrixC b (transpose (replaceLine j v (transpose m)))
		where   replaceLine :: Int -> [a] -> [[a]] -> [[a]]
			replaceLine j v m = ( take (j-1) m ) ++
					  [v] ++
					  ( drop (j) m )
			
{-
replaceColumn j (SqMatrixC b@((iLo,jLo),(iHi,jHi)) mat) (VectorC _ v) = 
     if (not (inRange (jLo,jHi) j)) 
       then error "Error in replaceColumn: column index not in range"
       else SqMatrixC b [ replaceElem j i | i <- [iLo..iHi] ]
	    where replaceElem j i = [ line !! (k-1) | k <- [jLo..j-1] ] ++
				    [ v !! (i-1) ] ++
				    [ line !! (k-1) | k <- [j+1..jHi] ]
		  		    where line = mat !! (i-1)
-}

-- transp :: (Ix a, Ix b) => Array (a,b) c -> Array (b,a) c

transp :: (Integral a) => SqMatrix a -> SqMatrix a
transp (SqMatrixC b@((iLo,jLo),(iHi,jHi)) mat) = SqMatrixC b (transpose mat)
	{- 
	SqMatrixC b [ [ line !! (j-1) | line <- mat ] | j <- [jLo..jHi] ]
	-}

-- maxElem :: (Ix a, Ix b, Ord c) => Array (a,b) c -> c

#if defined(STRATEGIES)
maxElem :: (Integral a, NFData a) => SqMatrix a -> a
#else
maxElem :: (Integral a) => SqMatrix a -> a
#endif
maxElem (SqMatrixC _ mat) = maximum ( mat_map maximum mat )

#if defined(STRATEGIES)
maxElemVec :: (Integral a, NFData a) => Vector a -> a
#else
maxElemVec :: (Integral a) => Vector a -> a
#endif
maxElemVec (VectorC _ vec) = maximum vec

-- ----------------------------------------------------------------------------
-- @node Arithmetic Operations, I/O Operations, Misc operations, ADT Matrix
-- @section Arithmetic Operations
-- ----------------------------------------------------------------------------
 
-- scalarMult :: (Ix a, Ix b, Num c) => c -> Array (a,b) c -> Array (a,b) c

#if defined(STRATEGIES)
scalarMult :: (Integral a, NFData a) => a -> SqMatrix a -> SqMatrix a
#else
scalarMult :: (Integral a) => a -> SqMatrix a -> SqMatrix a
#endif
scalarMult x = matMapUnary (x*)
		{-
	      	SqMatrixC b [ mat_map (x*) line | line <- mat ]
		-}

#if defined(STRATEGIES)
vecScalarQuot :: (Integral a, NFData a) => a -> Vector a -> Vector a
#else
vecScalarQuot :: (Integral a) => a -> Vector a -> Vector a
#endif
vecScalarQuot x (VectorC b vec) = 
              VectorC b (mat_map (`div` x) vec)

#if defined(STRATEGIES)
crossProd :: (Integral a, NFData a) => Vector a -> Vector a -> a
#else
crossProd :: (Integral a) => Vector a -> Vector a -> a
#endif
crossProd (VectorC _ vec) (VectorC _ vec') = sum (zipWith (+) vec vec')
	-- foldl (+) 0 (listCompwiseComp (*) vec vec')

-- @cindex determinant

-- determinant :: (Ix a, Ix b, Num c) => Array (a,b) c -> c

determinant :: (
		  Integral a 
                , NFData a
               ) => SqMatrix a -> a

determinant (SqMatrixC ((iLo,jLo),(iHi,jHi)) mat) 
	| jHi-jLo+1 == 1 =  let 
			      [[mat_1_1]] = mat 
			    in 
			      mat_1_1
	| jHi-jLo+1 == 2 =  let  
			      [[mat_1_1,mat_1_2],
			       [mat_2_1,mat_2_2] ] = mat
			    in
			      mat_1_1 * mat_2_2 -  mat_1_2 * mat_2_1
	| otherwise      =  sum l_par 
	     where
	      l_par =   map determine1 [jLo..jHi]
	      determine1 j = 
	                 (if pivot > 0 then
	     		   sign*pivot*det' 
	     		 else
	     		   0) -- `sparking` rnf sign 
	     		 where
	     		    sign = if (even (j-jLo)) then 1 else -1
	     		    pivot = (head mat) !! (j-1)
			    mat_h' = (map (newLine j) (tail mat))
	     		    mat' = SqMatrixC ((iLo,jLo),(iHi-1,jHi-1))
	     				     mat_h'
	     		    det' = determinant mat'

#if 0
	     		    strategyD r = 
	     		      parList (parList rnf) mat_h'  `par`
	     		      rnf det'         `par` 
	     		      r0 r
#endif
	      tree_sum [] = 0
	      tree_sum [x] = x
	      tree_sum xs = (left+right)
			    where (l,r) = splitAt (length xs `div` 2) xs
				  left = tree_sum l
				  right = tree_sum r
	      newLine _ [] = []
	      newLine j line = (pre ++ post)
                               where				  
                                pre  = [ line !! (k-1) | k <- [jLo..j-1] ]
	      			post = [ line !! (k-1) | k <- [j+1..jHi] ]

{- seq determinant! -}

-- matEqual :: (Ix a, Ix b, Eq c) => Array (a,b) c -> Array (a,b) c -> Bool

matEqual :: (Integral a, NFData a) => SqMatrix a -> SqMatrix a -> Bool
matEqual (SqMatrixC bnds@((iLo,jLo),(iHi,jHi)) mat) (SqMatrixC bnds' mat') = 
       if (bnds==bnds')
         then foldl (&&) True 
                    [ foldl (&&) True 
                            (listCompwiseComp (==) (mat !! (k-1)) (mat' !! (k-1)))
                    | k <- [iLo..iHi] ]
         else error "matEqual: Matrices have different bounds\n"


vecEqual :: (Integral a, NFData a) => Vector a -> Vector a -> Bool
vecEqual (VectorC bnds vec) (VectorC bnds' vec') = 
       if (bnds==bnds')
         then foldl (&&) True (listCompwiseComp (==) vec vec')
         else error "vecEqual: Matrices have different bounds\n"

-- matSum :: (Ix a, Ix b, Num c) -> Array (a,b) c -> Array (a,b) c -> Array (a,b) c

matSum :: (Integral a, NFData a) => SqMatrix a -> SqMatrix a -> SqMatrix a
matSum = matCompwiseComp (+)

matDif :: (Integral a, NFData a) => SqMatrix a -> SqMatrix a -> SqMatrix a
matDif = matCompwiseComp (-)

-- @cindex mat mult

{- parallel matrix multiplication -}
matMult (SqMatrixC bnds mat) (SqMatrixC bnds' mat') = 
        SqMatrixC resultBounds 
#if defined(__PARALLEL_HASKELL__) || defined(__GRANSIM__)
        (parMap rwhnf
	   (\i -> 
	    parMap rnf
	      (\j ->
#else
	(map (\i -> map (\j ->
#endif
	      let
		line =    (VectorC ((jLo),(jHi)) (getLine i mat))
		column =  (VectorC ((iLo'),(iHi')) (getColumn j mat'))
	      in
		crossProd line column
              )
              [iLo..iHi]
           )
           [jLo..jHi] 
        )
	where getLine i mat = mat !! (i-1)
	      getColumn j mat = [ line !! (j-1) | line <- mat ]
              size = iHi - iLo + 1	      
              ((iLo,jLo),(iHi,jHi)) = bnds
              ((iLo',jLo'),(iHi',jHi')) = bnds'
              resultBounds 
               | (jLo,jHi)==(iLo',iHi')  = ((iLo,jLo'),(iHi,jHi'))
               | otherwise               = error "matMult: incompatible bounds"

matAbs :: (Integral a, NFData a) => SqMatrix a -> SqMatrix a
matAbs = matMapUnary abs


matSignum :: (Integral a, NFData a) => SqMatrix a -> SqMatrix a
matSignum = matMapUnary signum


matGcd :: (Integral a, NFData a)  =>  SqMatrix a -> a
matGcd m = matFold gcd (maxElem m) m


vecGcd :: (Integral a, NFData a)  =>  Vector a -> a
vecGcd m = vecFold gcd (maxElemVec m) m


-- matHom :: (Integral a) =>  Integer -> SqMatrix a -> SqMatrix a
-- matHom :: (Integral a) => Integer -> SqMatrix a -> SqMatrix a

matHom p = matMapUnary (modHom p)

-- vecHom :: (Integral a) => Integer -> Vector a -> Vector a
-- vecHom :: (Integral a) => Integer -> Vector a -> Vector a

vecHom p (VectorC _ v) = vector (mat_map (modHom p) v) 

{-
matBounds :: (Integral a) => SqMatrix a -> MatBounds

matBounds (SqMatrixC mat) = bounds mat
-}

matFromInteger :: Integer -> SqMatrix Integer
matFromInteger n = SqMatrixC ((1,1),(1,1)) [[n]]

-- ----------------------------------------------------------------------------
-- @node I/O Operations, Instances, Arithmetic Operations, ADT Matrix
-- @section I/O Operations
-- ----------------------------------------------------------------------------

-- showsMatrix :: (Ix a, Ix b, Text c) => Array (a,b) c -> ShowS

showsMatrix :: (Integral a) => SqMatrix a -> ShowS
showsMatrix (SqMatrixC _ mat) = ( (++) ("Matrix: \n" ++
                                  (foldl (++) "" [ show line ++ "\n" 
                                                 | line <- mat ] ) ) )


showsVector :: (Integral a) => Vector a -> ShowS
showsVector (VectorC _ vec) = 
	( (++) ("Vector: " ++ show vec) ) 

-- ----------------------------------------------------------------------------
-- @node Instances,  , I/O Operations, ADT Matrix
-- @section Instances
--
-- Instance definitions for the ADT of Square Matrices and Vectors
-- ----------------------------------------------------------------------------

{-
instance (Eq a) => Eq [a] where
 l == l' = foldl (&&) True (listCompwiseComp (==) l l')
-}

instance (Integral a, NFData a) => Eq (SqMatrix a) where
 (==) = matEqual

instance (Integral a) => Read (SqMatrix a) where
 readsPrec p  = error "readsPrec of Matrix: Not yet implemented!\n"
instance (Integral a) => Show (SqMatrix a) where
 showsPrec p  = showsMatrix

instance (Integral a, NFData a) => Num (SqMatrix a) where                
 (+) = matSum
 (-) = matDif
 (*) = matMult
 negate = scalarMult (-1)
 abs = matAbs
 signum = matSignum
 fromInteger = error "fromInteger of Matrix: Not yet implemented\n"
               {- matFromInteger -}


instance (Integral a, NFData a) => Eq (Vector a) where
 (==) = vecEqual

instance (Integral a, NFData a) => Read (Vector a) where
 readsPrec p  = error "readsPrec of Vector: Not yet implemented!\n"

instance (Integral a, NFData a) => Show (Vector a) where
 showsPrec p  = showsVector

