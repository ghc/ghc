module Vectors(Vector,vec,x,y,z,inpr,mulv,len,norm) where
import Numbers
{- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- section 2: Vectors

Vectors can be added to each other (+), subtracted (-),
you can build the outer product of two vectors (*)
and there is a denotation for the zero vector (0).
To be able to use the symbols + - * and 0 we make
vectors an instance of Num.

Further we have the operations
- vec, to build a vector from a list of coordinates
- coordinate selection x,y and z
- inner product
- multiply a vector by a scalar (mulv)
- length
- norm

-}

data Vector = Vec [Number] deriving (Eq)
-- if you are not using the Chalmers Haskell B compiler then remove the @
{- needed for GOFER
   remove the deriving and add:
instance Eq Vector where
	v == w	= len(v-w) == 0
-}
instance Num Vector where
	Vec v + Vec w	= Vec (zipWith (+) v w)
	Vec v - Vec w	= Vec (zipWith (-) v w)
	v * w	= Vec	[y(v)*z(w) - y(w)*z(v)
			,z(v)*x(w) - z(w)*x(v)
			,x(v)*y(w) - x(w)*y(v)]
	negate (Vec v)	= Vec (map negate v)
	abs v		= Vec [len v]
	signum v	= norm v
	fromInteger 0	= Vec [0,0,0]

instance Show Vector where
	showsPrec p (Vec v) = showParen (p>9) (showString "vec ". showList v)
instance Read Vector where
	readsPrec p = readParen (p>9) rd
		      where rd s = [(Vec ns,u) | ("vec",t) <- lex s,
						 (ns,u)    <- readList t,
						 length ns >= 2]

vec :: [Number] -> Vector
vec = Vec

x,y,z :: Vector -> Number
x(Vec v) = v !! 0
y(Vec v) = v !! 1
z(Vec v) = v !! 2

inpr :: Vector -> Vector -> Number
Vec v1 `inpr` Vec v2 = sum (zipWith (*) v1 v2)

mulv :: Number -> Vector -> Vector
c `mulv` (Vec v) = Vec (map (c*) v)

len :: Vector -> Number
len v = sqrt (v `inpr` v)

norm :: Vector -> Vector
norm v = (1/len v) `mulv` v
