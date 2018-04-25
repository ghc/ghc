\section{Matrix}

 =======================================================================
 This file has been created entirely by Brian D. Moe (Summer 1990)
 It contains all the "include" statements for the sparse and dense
 matrix operations.
 Converted to Haskell by Ian R McLelland and Cordelia V Hall (Nov 1992)
 =======================================================================

\begin{code}

module Matrix 
         (Matrix, Vector, Block , Vec ,
          Block_list , Row_pos, Col_pos, 
          mmult, mvmult, svmult,
          madd, msub,
          vadd, vsub,
          vdot, vouter,
          mneg, vneg,
          norm,    
          mkmatrix,
          mkvector,
          mergevectors,
          mupdate, vupdate,
          msubscript, vsubscript,
          getrow,
          getcol,
          numrows,
          msize, vsize,
          showmatrix, showvector) where


import AbsDensematrix
import Utils

\end{code}

 We have replaced include of "densematrix" with import of
 AbsDensematrix, which imports Densematrix and exports the
 named objects with the new names (excluding some). The original
 include appears in comments in AbsDensematrix (cvh).


 The beginning of Sparsematrix_kludge,
 textually inserted because Matrix and Sparsematrix_kludge
 are now mutually recursive, and it doesn't seem worth 
 maintaining them separately for now (cvh).

  ================================================================
  ================ A SPARSE MATRIX IMPLEMENTATION ================

  Written by Brian D. Moe (Summer 1990)
  Note:  
      mupdate and vupdate have been included by Kamini Shenoi 

  ================================================================


     This is a matrix/vector abstract type implementation designed
     for sparse, block stuctured matrices.  Included are several
     functions specific to solving linear systems for oil reservoir
     modelling. (e.g. precond & uncondition)


Implementation of Types


\begin{code}

mmult   :: Matrix -> Matrix -> Matrix
madd    :: Matrix -> Matrix -> Matrix
msub    :: Matrix -> Matrix -> Matrix
mvmult  :: Matrix -> Vector -> Vector
vadd    :: Vector -> Vector -> Vector
vsub    :: Vector -> Vector -> Vector
vdot    :: Vector -> Vector -> Scalar
vouter  :: Vector -> Vector -> Matrix
norm    :: Vector -> Scalar
mneg    :: Matrix -> Matrix
vneg    :: Vector -> Vector
svmult  :: Scalar -> Vector -> Vector

mupdate :: Matrix -> (Int,Int) -> Block -> Matrix
vupdate :: Vector -> Int -> Vec -> Vector

msubscript  :: Int -> Int -> Matrix -> Block
msubscript' :: Int -> Int -> Matrix -> [Block]
vsubscript  :: Int -> Vector -> Vec
getrow      :: Int -> Matrix -> [Block_tuple]
getcol      :: Int -> Matrix -> [Block_tuple]
numrows     :: Matrix -> Int
msize       :: Matrix -> (Int,Int)
vsize       :: Vector -> Int

mkmatrix   :: [[(Int,Int,Block)]] -> Matrix
mkvector   :: [Vec] -> Vector
mergevectors :: Vector -> Vector -> Vector

showmatrix :: Matrix -> [Char]
showvector :: Vector -> [Char]

type Row_pos        = Int
type Col_pos        = Int
type Block_tuple    = (Row_pos, Col_pos, Block)
type Block_list     = [Block_tuple]

type Matrix = Matrix_type
type Vector = Vector_type

type Matrix_type = [Block_list]
type Vector_type = [Vec]

type Scalar = Float
sadd       = (+)

mmult m1 m2 = error "unsupported matrix operation"

madd m1 m2 = error "unsupported matrix operation"   

msub m1 m2 = error "unsupported matrix operation"


mneg m = map (map negtuple) m
                    where
                       negtuple (r, c, b) = (r, c, bneg b)


vadd v1 v2   = map2 vecadd v1 v2

vsub v1 v2   = map2 vecsub v1 v2

vdot v1 v2   = foldl1 sadd (map2 vecdot v1 v2)

vouter v1 v2   = error "unsupported vector operation"

norm v   = foldl1 sadd (map vecnorm v)

vneg v   = map vecneg v

svmult s v = map (svecmult s) v

mupdate m (i,j) val
   = [getrow k m |k <- [0..i-1]] ++ [(f(m!!i))]
      ++ [getrow l m | l <- [(i+1) .. (numrows m)-1]]
     where
        f xs = (take j xs) ++ [(i, (j+1), val)] ++ (drop (j+1) xs)
   
	
vupdate v i vc = (take i v) ++ [vc] ++ (drop (i+1) v)


showmatrix m
   = concat [ (showrow i)++"\n" | i<-[0..length(m)-1] ]
     where
	showrow i = concat [status i j | j<-[0..length(m)-1]]
	status i j
	   = if exist i j then "#"
	     else "."
	exist i j = j `elem` (row i)
	row i = [ col | (r,col,b) <- (m!!i) ]

showvector vs =  concat (map showvec vs)
                  


mkmatrix = id


mkvector = id

mergevectors = (++)

\end{code}




 ================================================
 ================ MISC FUNCTIONS ================ 
 ================================================



 Roger Wainwrights's mvmult:  (used in mvmult elsewhere)

      For multiplying a sparse matrix by its
      vector counterpart.  (a list of vectors)

	mvmult :: Matrix -> Vector -> Vector

\begin{code}

mvmult rows v
   =  if ok then [ rvdot row v | row <- rows ]
      else error "Incompatible operands to large mvmult"
     where
        ok = (length rows) == (length v)

rvdot row v
     = foldl1 vecadd [bvecmult b (vsubscript c v) | (r,c,b) <- row]

\end{code}
     
 Notes:

   okindex checks that the number given won't be out of range for the
   given list. WARNING:  WILL HANG IF LIST IS INFINITE.

   msubscript' gets the block at a given location in a sparse matrix.

   msubscript gets the block at a given location in s sparse matrix,
   even if it isn't present.

   getrow gets the list of block tuples from the given row of a matrix

   getcol gets the list of block tuples from the given column of a matrix

   numrows returns the number of rows of blocks in a sparse matrix.

   strip_block returns the block part of a block tuple.

   vsubscript gets a piece of one of those chopped up vectors.


\begin{code}

okindex :: Int -> [a] -> Bool
okindex n m = (0<=n) && (n<=((length m) - 1)) -- testing (irm)


iscol :: Int -> Block_tuple -> Bool
iscol k (r,c,b) = (k==c)


msubscript' r c m
   = map strip_block (filter (iscol c) (getrow r m))

msubscript r c m
   = if thingee /= [] then (head thingee)
    else zero_block r c m
     where
        thingee = msubscript' r c m


getrow n m
   = if okindex n m then m !! n
     else error "getrow: index out of bounds"



getcol n m
   = concat [ filter (iscol n) row | row <- m ]


numrows m = length m

msize m = (length m,length (head m))

vsize v = length v


  
strip_block :: Block_tuple -> Block
strip_block (r,c,b) = b


vsubscript n v
   = if okindex n v then v!!n
     else error "vsubscript in matrix"

\end{code}

========== end of Sparsematrix_kludge ========================

\begin{code}

zero_block :: Int -> Int -> Matrix -> Block
zero_block i j m
   = mkblock (rep nrows (rep ncols 0))
     where
       (nrows,junk1) = block_size (head (getrow i m))
       (junk2,ncols) = block_size (head (getcol j m))
       block_size (r,c,b) = bsize b

\end{code}
