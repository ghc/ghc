--!! Make sure that state threads don't escape
--!! (example from Neil Ashton at York)
--
module IndTree(IndTree(..), itgen, itiap, itrap, itrapstate) where

--partain: import Auxiliary
import PreludeGlaST

type IndTree s t = MutableArray s (Int,Int) t

itgen :: Constructed a => (Int,Int) -> a -> IndTree s a
itgen n x = 
	runST (
	newArray ((1,1),n) x)

itiap :: Constructed a => (Int,Int) -> (a->a) -> IndTree s a -> IndTree s a
itiap i f arr =
	runST (
	readArray arr i `thenStrictlyST` \val ->
	writeArray arr i (f val) `seqStrictlyST`
	returnStrictlyST arr)

itrap :: Constructed a => ((Int,Int),(Int,Int)) -> (a->a) -> IndTree s a -> IndTree s a
itrap ((i,k),(j,l)) f arr = runST(itrap' i k)
	where
	itrap' i k = if k > l then returnStrictlyST arr
		     else (itrapsnd i k `seqStrictlyST`
			itrap' i (k+1))
	itrapsnd i k = if i > j then returnStrictlyST arr
                     else (readArray arr (i,k) `thenStrictlyST` \val ->
		        writeArray arr (i,k) (f val) `seqStrictlyST`
		        itrapsnd (i+1) k)

itrapstate :: Constructed b => ((Int,Int),(Int,Int)) -> (a->b->(a,b)) -> ((Int,Int)->c->a) ->
		(a->c) -> c -> IndTree s b -> (c, IndTree s b)
itrapstate ((i,k),(j,l)) f c d s arr = runST(itrapstate' i k s)
	where
	itrapstate' i k s = if k > l then returnStrictlyST (s,arr)
			    else (itrapstatesnd i k s `thenStrictlyST` \(s,arr) ->
				itrapstate' i (k+1) s)
	itrapstatesnd i k s = if i > j then returnStrictlyST (s,arr)
                            else (readArray arr (i,k) `thenStrictlyST` \val ->
		               let (newstate, newval) = f (c (i,k) s) val
		               in writeArray arr (i,k) newval `seqStrictlyST`
		               itrapstatesnd (i+1) k (d newstate))

-- stuff from Auxiliary: copied here (partain)

sap :: (a->b) -> (c,a) -> (c,b)
sap f (x,y) = (x, f y)

fap :: (a->b) -> (a,c) -> (b,c)
fap f (x,y) = (f x, y)

nonempty :: [a] -> Bool
nonempty []    = False
nonempty (_:_) = True

-- const :: a -> b -> a
-- const k x = k

-- id :: a -> a
-- id x = x

compose :: [a->a] -> a -> a
compose = foldr (.) id

data Maybe t = Just t | Nothing

class Constructed a where
   normal :: a -> Bool

instance Constructed Bool where
   normal True = True
   normal False = True

instance Constructed Int where
   normal 0 = True
   normal n = True

instance (Constructed a, Constructed b) => Constructed (a,b) where
   normal (x,y) = normal x && normal y

-- pair :: (Constructed a, Constructed b) => a -> b -> (a,b)
-- pair x y | normal x && normal y = (x,y)

instance Constructed (Maybe a) where
   normal Nothing = True
   normal (Just _) = True

just :: Constructed a => a -> Maybe a
just x | normal x = Just x
