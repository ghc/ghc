{-# LANGUAGE Arrows #-}

-- Homogeneous (or depth-preserving) functions over perfectly balanced trees.

module Main where

import Control.Arrow
import Control.Category
import Data.Complex
import Prelude hiding (id, (.))

infixr 4 :&:

-- Consider the following non-regular type of perfectly balanced trees,
-- or `powertrees' (cf Jayadev Misra's powerlists):

data Pow a = Zero a | Succ (Pow (Pair a))
	deriving Show

type Pair a = (a, a)

-- Here are some example elements:

tree0 = Zero 1
tree1 = Succ (Zero (1, 2))
tree2 = Succ (Succ (Zero ((1, 2), (3, 4))))
tree3 = Succ (Succ (Succ (Zero (((1, 2), (3, 4)), ((5, 6), (7, 8))))))

-- The elements of this type have a string of constructors expressing
-- a depth n as a Peano numeral, enclosing a nested pair tree of 2^n
-- elements.  The type definition ensures that all elements of this type
-- are perfectly balanced binary trees of this form.  (Such things arise
-- in circuit design, eg Ruby, and descriptions of parallel algorithms.)
-- And the type system will ensure that all legal programs preserve
-- this structural invariant.
--  
-- The only problem is that the type constraint is too restrictive, rejecting
-- many of the standard operations on these trees.  Typically you want to
-- split a tree into two subtrees, do some processing on the subtrees and
-- combine the results.  But the type system cannot discover that the two
-- results are of the same depth (and thus combinable).  We need a type
-- that says a function preserves depth.  Here it is:

data Hom a b = (a -> b) :&: Hom (Pair a) (Pair b)

-- A homogeneous (or depth-preserving) function is an infinite sequence of
-- functions of type Pair^n a -> Pair^n b, one for each depth n.  We can
-- apply a homogeneous function to a powertree by selecting the function
-- for the required depth:

apply :: Hom a b -> Pow a -> Pow b
apply (f :&: fs) (Zero x) = Zero (f x)
apply (f :&: fs) (Succ t) = Succ (apply fs t)

-- Having defined apply, we can forget about powertrees and do all our
-- programming with Hom's.  Firstly, Hom is an arrow:

instance Category Hom where
	id = id :&: id
	(f :&: fs) . (g :&: gs) = (f . g) :&: (fs . gs)

instance Arrow Hom where
	arr f = f :&: arr (f *** f)
	first (f :&: fs) =
		first f :&: (arr transpose >>> first fs >>> arr transpose)

transpose :: ((a,b), (c,d)) -> ((a,c), (b,d))
transpose ((a,b), (c,d)) = ((a,c), (b,d))

-- arr maps f over the leaves of a powertree.

-- The composition >>> composes sequences of functions pairwise.
--  
-- The *** operator unriffles a powertree of pairs into a pair of powertrees,
-- applies the appropriate function to each and riffles the results.
-- It defines a categorical product for this arrow category.

-- When describing algorithms, one often provides a pure function for the
-- base case (trees of one element) and a (usually recursive) expression
-- for trees of pairs.

-- For example, a common divide-and-conquer pattern is the butterfly, where
-- one recursive call processes the odd-numbered elements and the other
-- processes the even ones (cf Geraint Jones and Mary Sheeran's Ruby papers):

butterfly :: (Pair a -> Pair a) -> Hom a a
butterfly f = id :&: proc (x, y) -> do
		x' <- butterfly f -< x
		y' <- butterfly f -< y
		returnA -< f (x', y')

-- The recursive calls operate on halves of the original tree, so the
-- recursion is well-defined.

-- Some examples of butterflies:

rev :: Hom a a
rev = butterfly swap
	where	swap (x, y) = (y, x)

unriffle :: Hom (Pair a) (Pair a)
unriffle = butterfly transpose

-- Batcher's sorter for bitonic sequences:

bisort :: Ord a => Hom a a
bisort = butterfly cmp
	where	cmp (x, y) = (min x y, max x y)

-- This can be used (with rev) as the merge phase of a merge sort.
--  
sort :: Ord a => Hom a a
sort = id :&: proc (x, y) -> do
		x' <- sort -< x
		y' <- sort -< y
		yr <- rev -< y'
		p <- unriffle -< (x', yr)
		bisort2 -< p
	where _ :&: bisort2 = bisort

-- Here is the scan operation, using the algorithm of Ladner and Fischer:

scan :: (a -> a -> a) -> a -> Hom a a
scan op b = id :&: proc (x, y) -> do
		y' <- scan op b -< op x y
		l <- rsh b -< y'
		returnA -< (op l x, y')

-- The auxiliary function rsh b shifts each element in the tree one place to
-- the right, placing b in the now-vacant leftmost position, and discarding
-- the old rightmost element:

rsh :: a -> Hom a a
rsh b = const b :&: proc (x, y) -> do
		w <- rsh b -< y
		returnA -< (w, x)

-- Finally, here is the Fast Fourier Transform:

type C = Complex Double

fft :: Hom C C
fft = id :&: proc (x, y) -> do
		x' <- fft -< x
		y' <- fft -< y
		r <- roots (-1) -< ()
		let z = r*y'
		unriffle -< (x' + z, x' - z)

-- The auxiliary function roots r (where r is typically a root of unity)
-- populates a tree of size n (necessarily a power of 2) with the values
-- 1, w, w^2, ..., w^(n-1), where w^n = r.

roots :: C -> Hom () C
roots r = const 1 :&: proc _ -> do
		x <- roots r' -< ()
		unriffle -< (x, x*r')
	where	r' = if imagPart s >= 0 then -s else s
		s = sqrt r

-- Miscellaneous functions:

rrot :: Hom a a
rrot = id :&: proc (x, y) -> do
		w <- rrot -< y
		returnA -< (w, x)

ilv :: Hom a a -> Hom (Pair a) (Pair a)
ilv f = proc (x, y) -> do
		x' <- f -< x
		y' <- f -< y
		returnA -< (x', y')

scan' :: (a -> a -> a) -> a -> Hom a a
scan' op b = proc x -> do
		l <- rsh b -< x
		(id :&: ilv (scan' op b)) -< op l x

riffle :: Hom (Pair a) (Pair a)
riffle = id :&: proc ((x1, y1), (x2, y2)) -> do
		x <- riffle -< (x1, x2)
		y <- riffle -< (y1, y2)
		returnA -< (x, y)

invert :: Hom a a
invert = id :&: proc (x, y) -> do
		x' <- invert -< x
		y' <- invert -< y
		unriffle -< (x', y')

carryLookaheadAdder :: Hom (Bool, Bool) Bool
carryLookaheadAdder = proc (x, y) -> do
		carryOut <- rsh (Just False) -<
			if x == y then Just x else Nothing
		Just carryIn <- scan plusMaybe Nothing -< carryOut
		returnA -< x `xor` y `xor` carryIn
	where	plusMaybe x Nothing = x
		plusMaybe x (Just y) = Just y
		False `xor` b = b
		True `xor` b = not b

-- Global conditional for SIMD

ifAll :: Hom a b -> Hom a b -> Hom (a, Bool) b
ifAll fs gs = ifAllAux snd (arr fst >>> fs) (arr fst >>> gs)
	where	ifAllAux :: (a -> Bool) -> Hom a b -> Hom a b -> Hom a b
		ifAllAux p (f :&: fs) (g :&: gs) =
			liftIf p f g :&: ifAllAux (liftAnd p) fs gs
		liftIf p f g x = if p x then f x else g x
		liftAnd p (x, y) = p x && p y

maybeAll :: Hom a c -> Hom (a, b) c -> Hom (a, Maybe b) c
maybeAll (n :&: ns) (j :&: js) =
	choose :&: (arr dist >>> maybeAll ns (arr transpose >>> js))
	where	choose (a, Nothing) = n a
		choose (a, Just b) = j (a, b)
		dist ((a1, b1), (a2, b2)) = ((a1, a2), zipMaybe b1 b2)
		zipMaybe (Just x) (Just y) = Just (x, y)
		zipMaybe _ _ = Nothing

main = do
	print (apply rev tree3)
	print (apply invert tree3)
	print (apply (invert >>> sort) tree3)
	print (apply (scan (+) 0) tree3)
