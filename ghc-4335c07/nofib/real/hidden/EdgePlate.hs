module EdgePlate(Edge,edgeT,edgeH, s,h,t, Plate(Plt),n,
		 Input,Object,makeObject) where
import Numbers
import Vectors
{- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- section 3: Lines and edges

An edge and a line are defined by
- a support vector (s) and a termination vector (t) differing from s   or
- a support vector (s) and a non-zero heading vector (h)
Below we have chosen for the latter.
-}

data Edge = Edg Vector Vector

edgeT, edgeH :: Vector -> Vector -> Edge
edgeH v w = Edg v w
edgeT v w = Edg v (w-v)

s,h,t :: Edge -> Vector
s (Edg v w)   = v
h (Edg v w)   = w
t (Edg v w)   = v+w

type Input = [[Vector]]

{- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- section 3: Planes and plates

A plate is a sequence of edges $e_{0},\ldots,e_{n-1}$ such that
- t(e_{i}) == s(e_{i+1}) && not (e_{i} ||| e_{i+1}) for 0<=i<=n-2
- t(e_{n-1}) == s(e_{0}) && not (e_{n-1} ||| e_{0})
(|||) means `is parralel to`, see Geometric.hs

-}

data Plate = Plt Int [Edge]

n :: Plate -> Vector
n(Plt _ (l1:l2:ls)) = norm( h(l1) * h(l2) )

type Object = [Plate]

-- `makeObject' transforms the input data to a proper object
makeObject :: Input -> Object
makeObject = zipWith borders [1..]
	     where borders :: Int -> [Vector] -> Plate
		   borders n ps = Plt n (zipWith edgeT ps (ror 1 ps))

-- rotate right
ror :: Int -> [a] -> [a]
ror n xs =
	reverse (take n rvxs) ++ reverse (drop n rvxs)
	where rvxs = reverse xs
