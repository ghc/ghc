
\begin{code}
module Arr (
  module Data.Array,

  safezipWith, safezip,
  row,
  sum1, map2, map3,
  mapat, mapat2, mapat3,
  mapindexed, mapindexed2, mapindexed3,
--  zipArr, sumArr, scaleArr,
  arraySize,

  matvec, inner, 
  outerVector,
  
  Vector (Vector), toVector, fromVector, listVector, vectorList, vector, 
  zipVector, scaleVector, sumVector, vectorNorm2, vectorSize,
  
  Matrix (Matrix), toMatrix, fromMatrix, listMatrix, matrixList, matrix, 
  zipMatrix, scaleMatrix, sumMatrix,

  augment,
  trMatrix,

--   showsVector,
--   showsMatrix,
-- showsVecList, showsMatList
--  spy,
) where
import Data.Array
import Numeric
--import Trace
--import IOExtensions(unsafePerformIO)
\end{code}

@Vector@ and @Matrix@ are 1-based arrays with read/show in form of Lists.

\begin{code}
data Vector a = Vector (Array Int a) deriving (Eq) --, Show)

toVector :: Array Int a -> Vector a
toVector x = Vector x

fromVector :: Vector a -> Array Int a
fromVector (Vector x) = x

instance Functor (Vector) where
  fmap fn x = toVector (fmap fn (fromVector x))    

{-instance Eq a => Eq (Vector a) where
--  (Vector x) == (Vector y) = x == y
-}

instance Show a => Show (Vector a) where
  showsPrec p x = showsPrec p (elems (fromVector x))

instance Read a => Read (Vector a) where
  readsPrec p = readParen False 
                  (\r -> [(listVector s, t) | (s, t) <- reads r])

instance Num b => Num (Vector b) where
  (+) = zipVector "+" (+)
  (-) = zipVector "-" (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
--   (*) = matMult -- works only for matrices!
--  fromInteger = fmap fromInteger
\end{code}


Convert a list to 1-based vector.

\begin{code}
listVector :: [a] -> Vector a
listVector x = toVector (listArray (1,length x) x)

vectorList :: Vector a -> [a]
vectorList = elems . fromVector

vector (l,u) x | l == 1 = toVector (array (l,u) x)
               | otherwise = error "vector: l != 1"
               
zipVector :: String -> (b -> c -> d) -> Vector b -> Vector c -> Vector d
zipVector s f (Vector a) (Vector b) 
  | bounds a == bounds b = vector (bounds a) [(i, f (a!i) (b!i)) | i <- indices a]
  | otherwise            = error ("zipVector: " ++ s ++ ": unconformable arrays")

scaleVector :: Num a => a -> Vector a -> Vector a
scaleVector a = fmap (* a)

sumVector :: Num a => Vector a -> a
sumVector = sum . elems . fromVector

vectorNorm2 :: Num a => Vector a -> a
vectorNorm2 x = inner x x

vectorSize :: Vector a -> Int
vectorSize (Vector x) = rangeSize (bounds x)

\end{code}

==============

\begin{code}
data Matrix a = Matrix (Array (Int, Int) a) deriving Eq

toMatrix :: Array (Int, Int) a -> Matrix a
toMatrix x = Matrix x

fromMatrix :: Matrix a -> Array (Int, Int) a
fromMatrix (Matrix x) = x

instance Functor (Matrix) where
  fmap fn x = toMatrix (fmap fn (fromMatrix x))    

--instance Eq a => Eq (Matrix a) where
--  (Matrix x) == (Matrix y) = x == y

instance Show a => Show (Matrix a) where
  showsPrec p x = vertl (matrixList x)
  
vertl [] = showString "[]"
vertl (x:xs) = showChar '[' . shows x . showl xs 
    where showl [] = showChar ']'
          showl (x:xs) = showString ",\n" . shows x . showl xs

instance Read a => Read (Matrix a) where
    readsPrec p = readParen False
                  (\r -> [(listMatrix s, t) | (s, t) <- reads r])

instance Num b => Num (Matrix b) where
  (+) = zipMatrix "+" (+)
  (-) = zipMatrix "-" (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  x * y = toMatrix (matMult (fromMatrix x) (fromMatrix y)) -- works only for matrices!
--  fromInteger = fmap fromInteger
\end{code}

Convert a nested list to a matrix.

\begin{code}
listMatrix :: [[a]] -> Matrix a
listMatrix x = Matrix (listArray ((1, 1), (length x, length (x!!0))) (concat x))

matrixList :: Matrix a -> [[a]]
matrixList (Matrix x) = [ [x!(i,j) | j <- range (l',u')] | i <- range (l,u)]
         where ((l,l'),(u,u')) = bounds x

matrix ((l,l'),(u,u')) x | l == 1 && l' == 1 = toMatrix (array ((l,l'),(u,u')) x)
                         | otherwise = error "matrix: l != 1"

zipMatrix :: String -> (b -> c -> d) -> Matrix b -> Matrix c -> Matrix d
zipMatrix s f (Matrix a) (Matrix b) 
  | bounds a == bounds b = matrix (bounds a) [(i, f (a!i) (b!i)) | i <- indices a]
  | otherwise            = error ("zipMatrix: " ++ s ++ ": unconformable arrays")

scaleMatrix :: Num a => a -> Matrix a -> Matrix a
scaleMatrix a = fmap (* a)

sumMatrix :: Num a => Matrix a -> a
sumMatrix = sum . elems . fromMatrix

\end{code}


============

\begin{code}
safezipWith :: String -> (a -> b -> c) -> [a] -> [b] -> [c]
safezipWith _ _ [] [] = []
safezipWith s f (x:xs) (y:ys) = f x y : safezipWith s f xs ys
safezipWith s _ _ _ = error ("safezipWith: " ++ s ++ ": unconformable vectors")

safezip :: [a] -> [b] -> [(a,b)]
safezip = safezipWith "(,)" (,)

trMatrix :: Matrix a -> Matrix a
trMatrix (Matrix x) = matrix ((l,l'),(u',u)) [((j,i), x!(i,j)) | j <- range (l',u'), i <- range (l,u)]
         where ((l,l'),(u,u')) = bounds x

row :: (Ix a, Ix b) => a -> Array (a,b) c -> Array b c
row i x = ixmap (l',u') (\j->(i,j)) x where ((l,l'),(u,u')) = bounds x

zipArr :: (Ix a) => String -> (b -> c -> d) -> Array a b -> Array a c -> Array a d
zipArr s f a b | bounds a == bounds b = array (bounds a) [(i, f (a!i) (b!i)) | i <- indices a]
               | otherwise            = error ("zipArr: " ++ s ++ ": unconformable arrays")
\end{code}

Valid only for b -> c -> b functions.

\begin{code}
zipArr' :: (Ix a) => String -> (b -> c -> b) -> Array a b -> Array a c -> Array a b
zipArr' s f a b | bounds a == bounds b = accum f a (assocs b)
                | otherwise            = error ("zipArr': " ++ s ++ ": unconformable arrays")
\end{code}

Overload arithmetical operators to work on lists.

\begin{code}
instance Num a => Num [a] where
  (+) = safezipWith "+" (+)
  (-) = safezipWith "-" (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
--   (*) = undefined
--   fromInteger = undefined
\end{code}

\begin{code}
sum1 :: (Num a) => [a] -> a
sum1 = foldl1 (+)

--main = print (sum1 [[4,1,1], [5,1,2], [6,1,3,4]])
\end{code}

\begin{code}
map2 f = fmap (fmap f) 
map3 f = fmap (map2 f) 
\end{code}

Map function f at position n only.  Out of range indices are silently
ignored.

\begin{code}
mapat n f x = mapat1 0 f x where
    mapat1 _ _ [] = []
    mapat1 i f (x:xs) = (if i == n then f x else x) : mapat1 (i + 1) f xs

mapat2 (i,j) = mapat i . mapat j
mapat3 (i,j,k) = mapat i . mapat j . mapat k

-- main = print (mapat 2 (10+) [1,2,3,4])
-- main = print (mapat2 (1,0) (1000+) ginp)
-- main = print (mapat3 (1,0,1) (1000+) gw)
\end{code}

\begin{code}
mapindexed f x = mapindexed1 f 0 x where
    mapindexed1 _ _ [] = []
    mapindexed1 f n (x:xs) = f n x : mapindexed1 f (n + 1) xs

mapindexed2 f = mapindexed (\i -> mapindexed (\j -> f (i, j))) 
mapindexed3 f = mapindexed (\i -> mapindexed (\j -> mapindexed (\k -> f (i, j, k))))

-- main = print (mapindexed (\x y -> mapat (10+) [1,2,3,4] y) [1,2,3,4])
-- main = print (mapindexed2 (\(i,j) x -> 100*i + 10*j + x) ginp)
-- main = print (mapindexed3 (\(i,j,k) x -> 1000*i + 100*j + 10*k + x) gw)
\end{code}



Overload arithmetical operators to work on arrays.

\begin{code}
instance (Ix a, Show a, Num b) => Num (Array a b) where
  (+) = zipArr "+" (+)
  (-) = zipArr "-" (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
--   (*) = matMult -- works only for matrices!
--   fromInteger = map fromInteger
\end{code}

\begin{xcode}
scaleArr :: (Ix i, Num a) => a -> Array i a -> Array i a
scaleArr a = fmap (*a)

sumArr :: (Ix i, Num a) => Array i a -> a
sumArr = sum . elems
\end{xcode}

\begin{code}
arraySize :: (Ix i) => Array i a -> Int
arraySize = rangeSize . bounds
\end{code}

\begin{code}
matMult         :: (Ix a, Ix b, Ix c, Num d) =>
                   Array (a,b) d -> Array (b,c) d -> Array (a,c) d
matMult x y     =  array resultBounds
                         [((i,j), sum [x!(i,k) * y!(k,j) | k <- range (lj,uj)])
                                       | i <- range (li,ui),
                                         j <- range (lj',uj') ]
        where ((li,lj),(ui,uj))         =  bounds x
              ((li',lj'),(ui',uj'))     =  bounds y
              resultBounds
                | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj'))
                | otherwise             = error "matMult: incompatible bounds"
\end{code}


Inner product of two vectors.

\begin{code}
inner :: Num a => Vector a -> Vector a -> a
inner (Vector v) (Vector w) = if b == bounds w
               then sum [v!i * w!i | i <- range b]
               else error "nn.inner: inconformable vectors"
            where b = bounds v
\end{code}

Outer product of two vectors $v \dot w^\mathrm{T}$.

\begin{code}
outerVector :: Num b => Vector b -> Vector b -> Matrix b
outerVector (Vector v) (Vector w) = if (l,u) == (l',u')
               then matrix ((l,l'),(u,u')) [((i,j), v!i * w!j) | i <- range (l,u), j <- range (l',u')]
               else error "nn.outer: inconformable vectors"
            where ((l,u),(l',u')) = (bounds v, bounds w)
\end{code}

\begin{code}
outerArr :: (Ix a, Num b) => Array a b -> Array a b -> Array (a,a) b
outerArr v w = if (l,u) == (l',u')
               then array ((l,l'),(u,u')) [((i,j), v!i * w!j) | i <- range (l,u), j <- range (l',u')]
               else error "nn.outer: inconformable vectors"
            where ((l,u),(l',u')) = (bounds v, bounds w)
\end{code}

Inner product of a matrix and a vector.

\begin{code}
matvec :: (Ix a, Num b) => Array (a,a) b -> Array a b -> Array a b
matvec w x | bounds x == (l',u') =
                array (l,u) [(i, sum [w!(i,j) * x!j | j <- range (l',u')]) 
                                | i <- range (l,u)]
           | otherwise           = error "nn.matvec: inconformable arrays"
         where ((l,l'),(u,u')) = bounds w
\end{code}

Append to a vector.

\begin{code}
augment :: (Num a) => Vector a -> a -> Vector a
augment (Vector x) y = Vector (array (a,b') ((b',y) : assocs x))
            where (a,b) = bounds x
                  b' = b + 1
\end{code}

Older approach (x!!i!!j fails in ghc-2.03).

\begin{code}
toMatrix' :: [[a]] -> Matrix a
toMatrix' x = Matrix (array ((1,1),(u,u')) [((i,j), (x!!(i-1))!!(j-1)) 
                             | i <- range (1,u), j <- range (1,u')])
          where (u,u') = (length x,length (x!!0))
\end{code}

Matrix 2D printout.

\begin{code}
padleft :: Int -> String -> String
padleft n x | n <= length x = x
            | otherwise = replicate (n - length x) ' ' ++ x
\end{code}

\begin{code}
padMatrix :: RealFloat a => Int -> Matrix a -> Matrix String
padMatrix n x = let ss = fmap (\a -> showFFloat (Just n) a "") x 
                    maxw = maximum (fmap length (elems (fromMatrix ss)))
              in fmap (padleft maxw) ss
\end{code}

\begin{xcode}
showsVector :: (RealFloat a) => Int -> Vector a -> ShowS
showsVector n x z1 = let x' = padArr n x
                         (l,u) = bounds x' in
                  concat (fmap (\ (i, s) -> if i == u then s ++ "\n" else s ++ " ") (assocs x')) ++ z1
\end{xcode}

\begin{xcode}
showsMatrix :: RealFloat a => Int -> Matrix a -> ShowS
showsMatrix n x z1 = let x' = padMatrix n x
                         ((l,l'),(u,u')) = bounds x' in
                   concat (fmap (\ ((i,j), s) -> if j == u' then s ++ "\n" else s ++ " ") (assocs x')) ++ z1
\end{xcode}

{-
showsVecList n x s = foldr (showsVector n) s x
showsMatList n x s = foldr (showsMatrix n) s x
-}


\begin{code}
--spy :: Show a => String -> a -> a
--spy msg x = trace ('<' : msg ++ ": " ++ shows x ">\n") x
--spy x  = seq (unsafePerformIO (putStr ('<' : shows x ">\n"))) x
--spy x  = traceShow "z" x
\end{code}
