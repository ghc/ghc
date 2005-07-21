{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence
-- Copyright   :  (c) Ross Paterson 2005
-- License     :  BSD-style
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- General purpose finite sequences.
-- Apart from being finite and having strict operations, sequences
-- also differ from lists in supporting a wider variety of operations
-- efficiently.
--
-- An amortized running time is given for each operation, with /n/ referring
-- to the length of the sequence and /i/ being the integral index used by
-- some operations.  These bounds hold even in a persistent (shared) setting.
--
-- The implementation uses 2-3 finger trees annotated with sizes,
-- as described in section 4.2 of
--
--    * Ralf Hinze and Ross Paterson,
--	\"Finger trees: a simple general-purpose data structure\",
--	to appear in /Journal of Functional Programming/.
--	<http://www.soi.city.ac.uk/~ross/papers/FingerTree.html>
--
-- /Note/: Many of these operations have the same names as similar
-- operations on lists in the "Prelude".  The ambiguity may be resolved
-- using either qualification or the @hiding@ clause.
--
-----------------------------------------------------------------------------

module Data.Sequence (
	Seq,
	-- * Construction
	empty,		-- :: Seq a
	singleton,	-- :: a -> Seq a
	(<|),		-- :: a -> Seq a -> Seq a
	(|>),		-- :: Seq a -> a -> Seq a
	(><),		-- :: Seq a -> Seq a -> Seq a
	-- * Deconstruction
	-- ** Queries
	null,		-- :: Seq a -> Bool
	length,		-- :: Seq a -> Int
	-- ** Views
	ViewL(..),
	viewl,		-- :: Seq a -> ViewL a
	ViewR(..),
	viewr,		-- :: Seq a -> ViewR a
	-- ** Indexing
	index,		-- :: Seq a -> Int -> a
	adjust,		-- :: (a -> a) -> Int -> Seq a -> Seq a
	update,		-- :: Int -> a -> Seq a -> Seq a
	take,		-- :: Int -> Seq a -> Seq a
	drop,		-- :: Int -> Seq a -> Seq a
	splitAt,	-- :: Int -> Seq a -> (Seq a, Seq a)
	-- * Lists
	fromList,	-- :: [a] -> Seq a
	toList,		-- :: Seq a -> [a]
	-- * Folds
	-- ** Right associative
	foldr,		-- :: (a -> b -> b) -> b -> Seq a -> b
	foldr1,		-- :: (a -> a -> a) -> Seq a -> a
	foldr',		-- :: (a -> b -> b) -> b -> Seq a -> b
	foldrM,		-- :: Monad m => (a -> b -> m b) -> b -> Seq a -> m b
	-- ** Left associative
	foldl,		-- :: (a -> b -> a) -> a -> Seq b -> a
	foldl1,		-- :: (a -> a -> a) -> Seq a -> a
	foldl',		-- :: (a -> b -> a) -> a -> Seq b -> a
	foldlM,		-- :: Monad m => (a -> b -> m a) -> a -> Seq b -> m a
	-- * Transformations
	reverse,	-- :: Seq a -> Seq a
#if TESTING
	valid,
#endif
	) where

import Prelude hiding (
	null, length, take, drop, splitAt, foldl, foldl1, foldr, foldr1,
	reverse)
import qualified Prelude (foldr)
import qualified Data.List (foldl', intersperse)
import Data.FunctorM
import Data.Typeable

#if TESTING
import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Test.QuickCheck
#endif

#if __GLASGOW_HASKELL__
import Data.Generics.Basics (Data(..), Fixity(..),
			constrIndex, mkConstr, mkDataType)
#endif

infixr 5 `consTree`
infixl 5 `snocTree`

infixr 5 ><
infixr 5 <|, :<
infixl 5 |>, :>

class Sized a where
	size :: a -> Int

------------------------------------------------------------------------
-- Random access sequences
------------------------------------------------------------------------

-- | General-purpose finite sequences.
newtype Seq a = Seq (FingerTree (Elem a))

instance Functor Seq where
	fmap f (Seq xs) = Seq (fmap (fmap f) xs)

instance Eq a => Eq (Seq a) where
	xs == ys = length xs == length ys && toList xs == toList ys

instance Ord a => Ord (Seq a) where
	compare xs ys = compare (toList xs) (toList ys)

#if TESTING
instance Show a => Show (Seq a) where
	showsPrec p (Seq x) = showsPrec p x
#else
instance Show a => Show (Seq a) where
	showsPrec _ xs = showChar '<' .
		flip (Prelude.foldr ($)) (Data.List.intersperse (showChar ',')
						(map shows (toList xs))) .
		showChar '>'
#endif

instance FunctorM Seq where
	fmapM f = foldlM f' empty
	  where f' ys x = do
			y <- f x
			return $! (ys |> y)
	fmapM_ f = foldlM f' ()
	  where f' _ x = f x >> return ()

#include "Typeable.h"
INSTANCE_TYPEABLE1(Seq,seqTc,"Seq")

#if __GLASGOW_HASKELL__
instance Data a => Data (Seq a) where
	gfoldl f z s	= case viewl s of
		EmptyL	-> z empty
		x :< xs -> z (<|) `f` x `f` xs

	gunfold k z c	= case constrIndex c of
		1 -> z empty
		2 -> k (k (z (<|)))
		_ -> error "gunfold"

	toConstr xs
	  | null xs	= emptyConstr
	  | otherwise	= consConstr

	dataTypeOf _	= seqDataType

	dataCast1	= gcast1

emptyConstr = mkConstr seqDataType "empty" [] Prefix
consConstr  = mkConstr seqDataType "<|" [] Infix
seqDataType = mkDataType "Data.Sequence.Seq" [emptyConstr, consConstr]
#endif

-- Finger trees

data FingerTree a
	= Empty
	| Single a
	| Deep {-# UNPACK #-} !Int !(Digit a) (FingerTree (Node a)) !(Digit a)
#if TESTING
	deriving Show
#endif

instance Sized a => Sized (FingerTree a) where
	{-# SPECIALIZE instance Sized (FingerTree (Elem a)) #-}
	{-# SPECIALIZE instance Sized (FingerTree (Node a)) #-}
	size Empty		= 0
	size (Single x)		= size x
	size (Deep v _ _ _)	= v

instance Functor FingerTree where
	fmap _ Empty = Empty
	fmap f (Single x) = Single (f x)
	fmap f (Deep v pr m sf) =
		Deep v (fmap f pr) (fmap (fmap f) m) (fmap f sf)

{-# INLINE deep #-}
{-# SPECIALIZE deep :: Digit (Elem a) -> FingerTree (Node (Elem a)) -> Digit (Elem a) -> FingerTree (Elem a) #-}
{-# SPECIALIZE deep :: Digit (Node a) -> FingerTree (Node (Node a)) -> Digit (Node a) -> FingerTree (Node a) #-}
deep		:: Sized a => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
deep pr m sf	=  Deep (size pr + size m + size sf) pr m sf

-- Digits

data Digit a
	= One a
	| Two a a
	| Three a a a
	| Four a a a a
#if TESTING
	deriving Show
#endif

instance Functor Digit where
	fmap f (One a) = One (f a)
	fmap f (Two a b) = Two (f a) (f b)
	fmap f (Three a b c) = Three (f a) (f b) (f c)
	fmap f (Four a b c d) = Four (f a) (f b) (f c) (f d)

instance Sized a => Sized (Digit a) where
	{-# SPECIALIZE instance Sized (Digit (Elem a)) #-}
	{-# SPECIALIZE instance Sized (Digit (Node a)) #-}
	size xs = foldlDigit (\ i x -> i + size x) 0 xs

{-# SPECIALIZE digitToTree :: Digit (Elem a) -> FingerTree (Elem a) #-}
{-# SPECIALIZE digitToTree :: Digit (Node a) -> FingerTree (Node a) #-}
digitToTree	:: Sized a => Digit a -> FingerTree a
digitToTree (One a) = Single a
digitToTree (Two a b) = deep (One a) Empty (One b)
digitToTree (Three a b c) = deep (Two a b) Empty (One c)
digitToTree (Four a b c d) = deep (Two a b) Empty (Two c d)

-- Nodes

data Node a
	= Node2 {-# UNPACK #-} !Int a a
	| Node3 {-# UNPACK #-} !Int a a a
#if TESTING
	deriving Show
#endif

instance Functor (Node) where
	fmap f (Node2 v a b) = Node2 v (f a) (f b)
	fmap f (Node3 v a b c) = Node3 v (f a) (f b) (f c)

instance Sized (Node a) where
	size (Node2 v _ _)	= v
	size (Node3 v _ _ _)	= v

{-# INLINE node2 #-}
{-# SPECIALIZE node2 :: Elem a -> Elem a -> Node (Elem a) #-}
{-# SPECIALIZE node2 :: Node a -> Node a -> Node (Node a) #-}
node2		:: Sized a => a -> a -> Node a
node2 a b	=  Node2 (size a + size b) a b

{-# INLINE node3 #-}
{-# SPECIALIZE node3 :: Elem a -> Elem a -> Elem a -> Node (Elem a) #-}
{-# SPECIALIZE node3 :: Node a -> Node a -> Node a -> Node (Node a) #-}
node3		:: Sized a => a -> a -> a -> Node a
node3 a b c	=  Node3 (size a + size b + size c) a b c

nodeToDigit :: Node a -> Digit a
nodeToDigit (Node2 _ a b) = Two a b
nodeToDigit (Node3 _ a b c) = Three a b c

-- Elements

newtype Elem a  =  Elem { getElem :: a }

instance Sized (Elem a) where
	size _ = 1

instance Functor Elem where
	fmap f (Elem x) = Elem (f x)

#ifdef TESTING
instance (Show a) => Show (Elem a) where
	showsPrec p (Elem x) = showsPrec p x
#endif

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

-- | /O(1)/. The empty sequence.
empty		:: Seq a
empty		=  Seq Empty

-- | /O(1)/. A singleton sequence.
singleton	:: a -> Seq a
singleton x	=  Seq (Single (Elem x))

-- | /O(1)/. Add an element to the left end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(<|)		:: a -> Seq a -> Seq a
x <| Seq xs	=  Seq (Elem x `consTree` xs)

{-# SPECIALIZE consTree :: Elem a -> FingerTree (Elem a) -> FingerTree (Elem a) #-}
{-# SPECIALIZE consTree :: Node a -> FingerTree (Node a) -> FingerTree (Node a) #-}
consTree	:: Sized a => a -> FingerTree a -> FingerTree a
consTree a Empty	= Single a
consTree a (Single b)	= deep (One a) Empty (One b)
consTree a (Deep s (Four b c d e) m sf) = m `seq`
	Deep (size a + s) (Two a b) (node3 c d e `consTree` m) sf
consTree a (Deep s (Three b c d) m sf) =
	Deep (size a + s) (Four a b c d) m sf
consTree a (Deep s (Two b c) m sf) =
	Deep (size a + s) (Three a b c) m sf
consTree a (Deep s (One b) m sf) =
	Deep (size a + s) (Two a b) m sf

-- | /O(1)/. Add an element to the right end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(|>)		:: Seq a -> a -> Seq a
Seq xs |> x	=  Seq (xs `snocTree` Elem x)

{-# SPECIALIZE snocTree :: FingerTree (Elem a) -> Elem a -> FingerTree (Elem a) #-}
{-# SPECIALIZE snocTree :: FingerTree (Node a) -> Node a -> FingerTree (Node a) #-}
snocTree	:: Sized a => FingerTree a -> a -> FingerTree a
snocTree Empty a	=  Single a
snocTree (Single a) b	=  deep (One a) Empty (One b)
snocTree (Deep s pr m (Four a b c d)) e = m `seq`
	Deep (s + size e) pr (m `snocTree` node3 a b c) (Two d e)
snocTree (Deep s pr m (Three a b c)) d =
	Deep (s + size d) pr m (Four a b c d)
snocTree (Deep s pr m (Two a b)) c =
	Deep (s + size c) pr m (Three a b c)
snocTree (Deep s pr m (One a)) b =
	Deep (s + size b) pr m (Two a b)

-- | /O(log(min(n1,n2)))/. Concatenate two sequences.
(><)		:: Seq a -> Seq a -> Seq a
Seq xs >< Seq ys = Seq (appendTree0 xs ys)

-- The appendTree/addDigits gunk below is machine generated

appendTree0 :: FingerTree (Elem a) -> FingerTree (Elem a) -> FingerTree (Elem a)
appendTree0 Empty xs =
	xs
appendTree0 xs Empty =
	xs
appendTree0 (Single x) xs =
	x `consTree` xs
appendTree0 xs (Single x) =
	xs `snocTree` x
appendTree0 (Deep s1 pr1 m1 sf1) (Deep s2 pr2 m2 sf2) =
	Deep (s1 + s2) pr1 (addDigits0 m1 sf1 pr2 m2) sf2

addDigits0 :: FingerTree (Node (Elem a)) -> Digit (Elem a) -> Digit (Elem a) -> FingerTree (Node (Elem a)) -> FingerTree (Node (Elem a))
addDigits0 m1 (One a) (One b) m2 =
	appendTree1 m1 (node2 a b) m2
addDigits0 m1 (One a) (Two b c) m2 =
	appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (One a) (Three b c d) m2 =
	appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (One a) (Four b c d e) m2 =
	appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Two a b) (One c) m2 =
	appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (Two a b) (Two c d) m2 =
	appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (Two a b) (Three c d e) m2 =
	appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Two a b) (Four c d e f) m2 =
	appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c) (One d) m2 =
	appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (Three a b c) (Two d e) m2 =
	appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Three a b c) (Three d e f) m2 =
	appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c) (Four d e f g) m2 =
	appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits0 m1 (Four a b c d) (One e) m2 =
	appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Four a b c d) (Two e f) m2 =
	appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Four a b c d) (Three e f g) m2 =
	appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits0 m1 (Four a b c d) (Four e f g h) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2

appendTree1 :: FingerTree (Node a) -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree1 Empty a xs =
	a `consTree` xs
appendTree1 xs a Empty =
	xs `snocTree` a
appendTree1 (Single x) a xs =
	x `consTree` a `consTree` xs
appendTree1 xs a (Single x) =
	xs `snocTree` a `snocTree` x
appendTree1 (Deep s1 pr1 m1 sf1) a (Deep s2 pr2 m2 sf2) =
	Deep (s1 + size a + s2) pr1 (addDigits1 m1 sf1 a pr2 m2) sf2

addDigits1 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits1 m1 (One a) b (One c) m2 =
	appendTree1 m1 (node3 a b c) m2
addDigits1 m1 (One a) b (Two c d) m2 =
	appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (One a) b (Three c d e) m2 =
	appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (One a) b (Four c d e f) m2 =
	appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (One d) m2 =
	appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (Two a b) c (Two d e) m2 =
	appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Two a b) c (Three d e f) m2 =
	appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (Four d e f g) m2 =
	appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (One e) m2 =
	appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Three a b c) d (Two e f) m2 =
	appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Three a b c) d (Three e f g) m2 =
	appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (Four e f g h) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (One f) m2 =
	appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Four a b c d) e (Two f g) m2 =
	appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Four a b c d) e (Three f g h) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (Four f g h i) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2

appendTree2 :: FingerTree (Node a) -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree2 Empty a b xs =
	a `consTree` b `consTree` xs
appendTree2 xs a b Empty =
	xs `snocTree` a `snocTree` b
appendTree2 (Single x) a b xs =
	x `consTree` a `consTree` b `consTree` xs
appendTree2 xs a b (Single x) =
	xs `snocTree` a `snocTree` b `snocTree` x
appendTree2 (Deep s1 pr1 m1 sf1) a b (Deep s2 pr2 m2 sf2) =
	Deep (s1 + size a + size b + s2) pr1 (addDigits2 m1 sf1 a b pr2 m2) sf2

addDigits2 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits2 m1 (One a) b c (One d) m2 =
	appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits2 m1 (One a) b c (Two d e) m2 =
	appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (One a) b c (Three d e f) m2 =
	appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (One a) b c (Four d e f g) m2 =
	appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (One e) m2 =
	appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (Two a b) c d (Two e f) m2 =
	appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Two a b) c d (Three e f g) m2 =
	appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (Four e f g h) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (One f) m2 =
	appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Three a b c) d e (Two f g) m2 =
	appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Three a b c) d e (Three f g h) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (Four f g h i) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (One g) m2 =
	appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Four a b c d) e f (Two g h) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Four a b c d) e f (Three g h i) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (Four g h i j) m2 =
	appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2

appendTree3 :: FingerTree (Node a) -> Node a -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree3 Empty a b c xs =
	a `consTree` b `consTree` c `consTree` xs
appendTree3 xs a b c Empty =
	xs `snocTree` a `snocTree` b `snocTree` c
appendTree3 (Single x) a b c xs =
	x `consTree` a `consTree` b `consTree` c `consTree` xs
appendTree3 xs a b c (Single x) =
	xs `snocTree` a `snocTree` b `snocTree` c `snocTree` x
appendTree3 (Deep s1 pr1 m1 sf1) a b c (Deep s2 pr2 m2 sf2) =
	Deep (s1 + size a + size b + size c + s2) pr1 (addDigits3 m1 sf1 a b c pr2 m2) sf2

addDigits3 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits3 m1 (One a) b c d (One e) m2 =
	appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits3 m1 (One a) b c d (Two e f) m2 =
	appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (One a) b c d (Three e f g) m2 =
	appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (One a) b c d (Four e f g h) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (One f) m2 =
	appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (Two a b) c d e (Two f g) m2 =
	appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Two a b) c d e (Three f g h) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (Four f g h i) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (One g) m2 =
	appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Three a b c) d e f (Two g h) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Three a b c) d e f (Three g h i) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (Four g h i j) m2 =
	appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (One h) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Four a b c d) e f g (Two h i) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Four a b c d) e f g (Three h i j) m2 =
	appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (Four h i j k) m2 =
	appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2

appendTree4 :: FingerTree (Node a) -> Node a -> Node a -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree4 Empty a b c d xs =
	a `consTree` b `consTree` c `consTree` d `consTree` xs
appendTree4 xs a b c d Empty =
	xs `snocTree` a `snocTree` b `snocTree` c `snocTree` d
appendTree4 (Single x) a b c d xs =
	x `consTree` a `consTree` b `consTree` c `consTree` d `consTree` xs
appendTree4 xs a b c d (Single x) =
	xs `snocTree` a `snocTree` b `snocTree` c `snocTree` d `snocTree` x
appendTree4 (Deep s1 pr1 m1 sf1) a b c d (Deep s2 pr2 m2 sf2) =
	Deep (s1 + size a + size b + size c + size d + s2) pr1 (addDigits4 m1 sf1 a b c d pr2 m2) sf2

addDigits4 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits4 m1 (One a) b c d e (One f) m2 =
	appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits4 m1 (One a) b c d e (Two f g) m2 =
	appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (One a) b c d e (Three f g h) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (One a) b c d e (Four f g h i) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (One g) m2 =
	appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (Two a b) c d e f (Two g h) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Two a b) c d e f (Three g h i) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (Four g h i j) m2 =
	appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (One h) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Three a b c) d e f g (Two h i) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Three a b c) d e f g (Three h i j) m2 =
	appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (Four h i j k) m2 =
	appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (One i) m2 =
	appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Four a b c d) e f g h (Two i j) m2 =
	appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Four a b c d) e f g h (Three i j k) m2 =
	appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (Four i j k l) m2 =
	appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node3 j k l) m2

------------------------------------------------------------------------
-- Deconstruction
------------------------------------------------------------------------

-- | /O(1)/. Is this the empty sequence?
null		:: Seq a -> Bool
null (Seq Empty) = True
null _		=  False

-- | /O(1)/. The number of elements in the sequence.
length		:: Seq a -> Int
length (Seq xs) =  size xs

-- Views

data Maybe2 a b = Nothing2 | Just2 a b

-- | View of the left end of a sequence.
data ViewL a
	= EmptyL	-- ^ empty sequence
	| a :< Seq a	-- ^ leftmost element and the rest of the sequence
#ifndef __HADDOCK__
	deriving (Eq, Show)
#else
instance Eq a => Eq (ViewL a)
instance Show a => Show (ViewL a)
#endif


instance Functor ViewL where
	fmap _ EmptyL		= EmptyL
	fmap f (x :< xs)	= f x :< fmap f xs

-- | /O(1)/. Analyse the left end of a sequence.
viewl		::  Seq a -> ViewL a
viewl (Seq xs)	=  case viewLTree xs of
	Nothing2 -> EmptyL
	Just2 (Elem x) xs' -> x :< Seq xs'

{-# SPECIALIZE viewLTree :: FingerTree (Elem a) -> Maybe2 (Elem a) (FingerTree (Elem a)) #-}
{-# SPECIALIZE viewLTree :: FingerTree (Node a) -> Maybe2 (Node a) (FingerTree (Node a)) #-}
viewLTree	:: Sized a => FingerTree a -> Maybe2 a (FingerTree a)
viewLTree Empty			= Nothing2
viewLTree (Single a)		= Just2 a Empty
viewLTree (Deep s (One a) m sf) = Just2 a (case viewLTree m of
	Nothing2	-> digitToTree sf
	Just2 b m'	-> Deep (s - size a) (nodeToDigit b) m' sf)
viewLTree (Deep s (Two a b) m sf) =
	Just2 a (Deep (s - size a) (One b) m sf)
viewLTree (Deep s (Three a b c) m sf) =
	Just2 a (Deep (s - size a) (Two b c) m sf)
viewLTree (Deep s (Four a b c d) m sf) =
	Just2 a (Deep (s - size a) (Three b c d) m sf)

-- | View of the right end of a sequence.
data ViewR a
	= EmptyR	-- ^ empty sequence
	| Seq a :> a	-- ^ the sequence minus the rightmost element,
			-- and the rightmost element
#ifndef __HADDOCK__
	deriving (Eq, Show)
#else
instance Eq a => Eq (ViewR a)
instance Show a => Show (ViewR a)
#endif

instance Functor ViewR where
	fmap _ EmptyR		= EmptyR
	fmap f (xs :> x)	= fmap f xs :> f x

-- | /O(1)/. Analyse the right end of a sequence.
viewr		::  Seq a -> ViewR a
viewr (Seq xs)	=  case viewRTree xs of
	Nothing2 -> EmptyR
	Just2 xs' (Elem x) -> Seq xs' :> x

{-# SPECIALIZE viewRTree :: FingerTree (Elem a) -> Maybe2 (FingerTree (Elem a)) (Elem a) #-}
{-# SPECIALIZE viewRTree :: FingerTree (Node a) -> Maybe2 (FingerTree (Node a)) (Node a) #-}
viewRTree	:: Sized a => FingerTree a -> Maybe2 (FingerTree a) a
viewRTree Empty			= Nothing2
viewRTree (Single z)		= Just2 Empty z
viewRTree (Deep s pr m (One z)) = Just2 (case viewRTree m of
	Nothing2	->  digitToTree pr
	Just2 m' y	->  Deep (s - size z) pr m' (nodeToDigit y)) z
viewRTree (Deep s pr m (Two y z)) =
	Just2 (Deep (s - size z) pr m (One y)) z
viewRTree (Deep s pr m (Three x y z)) =
	Just2 (Deep (s - size z) pr m (Two x y)) z
viewRTree (Deep s pr m (Four w x y z)) =
	Just2 (Deep (s - size z) pr m (Three w x y)) z

-- Indexing

-- | /O(log(min(i,n-i)))/. The element at the specified position
index		:: Seq a -> Int -> a
index (Seq xs) i
  | 0 <= i && i < size xs = case lookupTree (-i) xs of
				Place _ (Elem x) -> x
  | otherwise	= error "index out of bounds"

data Place a = Place {-# UNPACK #-} !Int a
#if TESTING
	deriving Show
#endif

{-# SPECIALIZE lookupTree :: Int -> FingerTree (Elem a) -> Place (Elem a) #-}
{-# SPECIALIZE lookupTree :: Int -> FingerTree (Node a) -> Place (Node a) #-}
lookupTree :: Sized a => Int -> FingerTree a -> Place a
lookupTree _ Empty = error "lookupTree of empty tree"
lookupTree i (Single x) = Place i x
lookupTree i (Deep _ pr m sf)
  | vpr > 0	=  lookupDigit i pr
  | vm > 0	=  case lookupTree vpr m of
			Place i' xs -> lookupNode i' xs
  | otherwise	=  lookupDigit vm sf
  where	vpr	=  i + size pr
	vm	=  vpr + size m

{-# SPECIALIZE lookupNode :: Int -> Node (Elem a) -> Place (Elem a) #-}
{-# SPECIALIZE lookupNode :: Int -> Node (Node a) -> Place (Node a) #-}
lookupNode :: Sized a => Int -> Node a -> Place a
lookupNode i (Node2 _ a b)
  | va > 0	= Place i a
  | otherwise	= Place va b
  where	va	= i + size a
lookupNode i (Node3 _ a b c)
  | va > 0	= Place i a
  | vab > 0	= Place va b
  | otherwise	= Place vab c
  where	va	= i + size a
	vab	= va + size b

{-# SPECIALIZE lookupDigit :: Int -> Digit (Elem a) -> Place (Elem a) #-}
{-# SPECIALIZE lookupDigit :: Int -> Digit (Node a) -> Place (Node a) #-}
lookupDigit :: Sized a => Int -> Digit a -> Place a
lookupDigit i (One a) = Place i a
lookupDigit i (Two a b)
  | va > 0	= Place i a
  | otherwise	= Place va b
  where	va	= i + size a
lookupDigit i (Three a b c)
  | va > 0	= Place i a
  | vab > 0	= Place va b
  | otherwise	= Place vab c
  where	va	= i + size a
	vab	= va + size b
lookupDigit i (Four a b c d)
  | va > 0	= Place i a
  | vab > 0	= Place va b
  | vabc > 0	= Place vab c
  | otherwise	= Place vabc d
  where	va	= i + size a
	vab	= va + size b
	vabc	= vab + size c

-- | /O(log(min(i,n-i)))/. Replace the element at the specified position
update		:: Int -> a -> Seq a -> Seq a
update i x	= adjust (const x) i

-- | /O(log(min(i,n-i)))/. Update the element at the specified position
adjust		:: (a -> a) -> Int -> Seq a -> Seq a
adjust f i (Seq xs)
  | 0 <= i && i < size xs = Seq (adjustTree (const (fmap f)) (-i) xs)
  | otherwise	= Seq xs

{-# SPECIALIZE adjustTree :: (Int -> Elem a -> Elem a) -> Int -> FingerTree (Elem a) -> FingerTree (Elem a) #-}
{-# SPECIALIZE adjustTree :: (Int -> Node a -> Node a) -> Int -> FingerTree (Node a) -> FingerTree (Node a) #-}
adjustTree	:: Sized a => (Int -> a -> a) ->
			Int -> FingerTree a -> FingerTree a
adjustTree _ _ Empty = error "adjustTree of empty tree"
adjustTree f i (Single x) = Single (f i x)
adjustTree f i (Deep s pr m sf)
  | vpr > 0	= Deep s (adjustDigit f i pr) m sf
  | vm > 0	= Deep s pr (adjustTree (adjustNode f) vpr m) sf
  | otherwise	= Deep s pr m (adjustDigit f vm sf)
  where	vpr	= i + size pr
	vm	= vpr + size m

{-# SPECIALIZE adjustNode :: (Int -> Elem a -> Elem a) -> Int -> Node (Elem a) -> Node (Elem a) #-}
{-# SPECIALIZE adjustNode :: (Int -> Node a -> Node a) -> Int -> Node (Node a) -> Node (Node a) #-}
adjustNode	:: Sized a => (Int -> a -> a) -> Int -> Node a -> Node a
adjustNode f i (Node2 s a b)
  | va > 0	= Node2 s (f i a) b
  | otherwise	= Node2 s a (f va b)
  where	va	= i + size a
adjustNode f i (Node3 s a b c)
  | va > 0	= Node3 s (f i a) b c
  | vab > 0	= Node3 s a (f va b) c
  | otherwise	= Node3 s a b (f vab c)
  where	va	= i + size a
	vab	= va + size b

{-# SPECIALIZE adjustDigit :: (Int -> Elem a -> Elem a) -> Int -> Digit (Elem a) -> Digit (Elem a) #-}
{-# SPECIALIZE adjustDigit :: (Int -> Node a -> Node a) -> Int -> Digit (Node a) -> Digit (Node a) #-}
adjustDigit	:: Sized a => (Int -> a -> a) -> Int -> Digit a -> Digit a
adjustDigit f i (One a) = One (f i a)
adjustDigit f i (Two a b)
  | va > 0	= Two (f i a) b
  | otherwise	= Two a (f va b)
  where	va	= i + size a
adjustDigit f i (Three a b c)
  | va > 0	= Three (f i a) b c
  | vab > 0	= Three a (f va b) c
  | otherwise	= Three a b (f vab c)
  where	va	= i + size a
	vab	= va + size b
adjustDigit f i (Four a b c d)
  | va > 0	= Four (f i a) b c d
  | vab > 0	= Four a (f va b) c d
  | vabc > 0	= Four a b (f vab c) d
  | otherwise	= Four a b c (f vabc d)
  where	va	= i + size a
	vab	= va + size b
	vabc	= vab + size c

-- Splitting

-- | /O(log(min(i,n-i)))/. The first @i@ elements of a sequence.
take		:: Int -> Seq a -> Seq a
take i		=  fst . splitAt i

-- | /O(log(min(i,n-i)))/. Elements of a sequence after the first @i@.
drop		:: Int -> Seq a -> Seq a
drop i		=  snd . splitAt i

-- | /O(log(min(i,n-i)))/. Split a sequence at a given position.
splitAt			:: Int -> Seq a -> (Seq a, Seq a)
splitAt i (Seq xs)	=  (Seq l, Seq r)
  where	(l, r)		=  split i xs

split :: Int -> FingerTree (Elem a) ->
	(FingerTree (Elem a), FingerTree (Elem a))
split i Empty	= i `seq` (Empty, Empty)
split i xs
  | size xs > i	= (l, consTree x r)
  | otherwise	= (xs, Empty)
  where Split l x r = splitTree (-i) xs

data Split t a = Split t a t
#if TESTING
	deriving Show
#endif

{-# SPECIALIZE splitTree :: Int -> FingerTree (Elem a) -> Split (FingerTree (Elem a)) (Elem a) #-}
{-# SPECIALIZE splitTree :: Int -> FingerTree (Node a) -> Split (FingerTree (Node a)) (Node a) #-}
splitTree :: Sized a => Int -> FingerTree a -> Split (FingerTree a) a
splitTree _ Empty = error "splitTree of empty tree"
splitTree i (Single x) = i `seq` Split Empty x Empty
splitTree i (Deep _ pr m sf)
  | vpr > 0	= case splitDigit i pr of
			Split l x r -> Split (maybe Empty digitToTree l) x (deepL r m sf)
  | vm > 0	= case splitTree vpr m of
			Split ml xs mr -> case splitNode (vpr + size ml) xs of
			    Split l x r -> Split (deepR pr  ml l) x (deepL r mr sf)
  | otherwise	= case splitDigit vm sf of
			Split l x r -> Split (deepR pr  m  l) x (maybe Empty digitToTree r)
  where	vpr	= i + size pr
	vm	= vpr + size m

{-# SPECIALIZE deepL :: Maybe (Digit (Elem a)) -> FingerTree (Node (Elem a)) -> Digit (Elem a) -> FingerTree (Elem a) #-}
{-# SPECIALIZE deepL :: Maybe (Digit (Node a)) -> FingerTree (Node (Node a)) -> Digit (Node a) -> FingerTree (Node a) #-}
deepL :: Sized a => Maybe (Digit a) -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL Nothing m sf	= case viewLTree m of
	Nothing2	-> digitToTree sf
	Just2 a m'	-> deep (nodeToDigit a) m' sf
deepL (Just pr) m sf	= deep pr m sf

{-# SPECIALIZE deepR :: Digit (Elem a) -> FingerTree (Node (Elem a)) -> Maybe (Digit (Elem a)) -> FingerTree (Elem a) #-}
{-# SPECIALIZE deepR :: Digit (Node a) -> FingerTree (Node (Node a)) -> Maybe (Digit (Node a)) -> FingerTree (Node a) #-}
deepR :: Sized a => Digit a -> FingerTree (Node a) -> Maybe (Digit a) -> FingerTree a
deepR pr m Nothing	= case viewRTree m of
	Nothing2	-> digitToTree pr
	Just2 m' a	-> deep pr m' (nodeToDigit a)
deepR pr m (Just sf)	= deep pr m sf

{-# SPECIALIZE splitNode :: Int -> Node (Elem a) -> Split (Maybe (Digit (Elem a))) (Elem a) #-}
{-# SPECIALIZE splitNode :: Int -> Node (Node a) -> Split (Maybe (Digit (Node a))) (Node a) #-}
splitNode :: Sized a => Int -> Node a -> Split (Maybe (Digit a)) a
splitNode i (Node2 _ a b)
  | va > 0	= Split Nothing a (Just (One b))
  | otherwise	= Split (Just (One a)) b Nothing
  where	va	= i + size a
splitNode i (Node3 _ a b c)
  | va > 0	= Split Nothing a (Just (Two b c))
  | vab > 0	= Split (Just (One a)) b (Just (One c))
  | otherwise	= Split (Just (Two a b)) c Nothing
  where	va	= i + size a
	vab	= va + size b

{-# SPECIALIZE splitDigit :: Int -> Digit (Elem a) -> Split (Maybe (Digit (Elem a))) (Elem a) #-}
{-# SPECIALIZE splitDigit :: Int -> Digit (Node a) -> Split (Maybe (Digit (Node a))) (Node a) #-}
splitDigit :: Sized a => Int -> Digit a -> Split (Maybe (Digit a)) a
splitDigit i (One a) = i `seq` Split Nothing a Nothing
splitDigit i (Two a b)
  | va > 0	= Split Nothing a (Just (One b))
  | otherwise	= Split (Just (One a)) b Nothing
  where	va	= i + size a
splitDigit i (Three a b c)
  | va > 0	= Split Nothing a (Just (Two b c))
  | vab > 0	= Split (Just (One a)) b (Just (One c))
  | otherwise	= Split (Just (Two a b)) c Nothing
  where	va	= i + size a
	vab	= va + size b
splitDigit i (Four a b c d)
  | va > 0	= Split Nothing a (Just (Three b c d))
  | vab > 0	= Split (Just (One a)) b (Just (Two c d))
  | vabc > 0	= Split (Just (Two a b)) c (Just (One d))
  | otherwise	= Split (Just (Three a b c)) d Nothing
  where	va	= i + size a
	vab	= va + size b
	vabc	= vab + size c

------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------

-- | /O(n)/. Create a sequence from a finite list of elements.
fromList  	:: [a] -> Seq a
fromList  	=  Data.List.foldl' (|>) empty

-- | /O(n)/. List of elements of the sequence.
toList		:: Seq a -> [a]
toList		=  foldr (:) []

------------------------------------------------------------------------
-- Folds
------------------------------------------------------------------------

-- | /O(n*t)/. Fold over the elements of a sequence,
-- associating to the right.
foldr :: (a -> b -> b) -> b -> Seq a -> b
foldr f z (Seq xs) = foldrTree f' z xs
  where f' (Elem x) y = f x y

foldrTree :: (a -> b -> b) -> b -> FingerTree a -> b
foldrTree _ z Empty = z
foldrTree f z (Single x) = x `f` z
foldrTree f z (Deep _ pr m sf) =
	foldrDigit f (foldrTree (flip (foldrNode f)) (foldrDigit f z sf) m) pr

foldrDigit :: (a -> b -> b) -> b -> Digit a -> b
foldrDigit f z (One a) = a `f` z
foldrDigit f z (Two a b) = a `f` (b `f` z)
foldrDigit f z (Three a b c) = a `f` (b `f` (c `f` z))
foldrDigit f z (Four a b c d) = a `f` (b `f` (c `f` (d `f` z)))

foldrNode :: (a -> b -> b) -> b -> Node a -> b
foldrNode f z (Node2 _ a b) = a `f` (b `f` z)
foldrNode f z (Node3 _ a b c) = a `f` (b `f` (c `f` z))

-- | /O(n*t)/. A variant of 'foldr' that has no base case,
-- and thus may only be applied to non-empty sequences.
foldr1 :: (a -> a -> a) -> Seq a -> a
foldr1 f (Seq xs) = getElem (foldr1Tree f' xs)
  where f' (Elem x) (Elem y) = Elem (f x y)

foldr1Tree :: (a -> a -> a) -> FingerTree a -> a
foldr1Tree _ Empty = error "foldr1: empty sequence"
foldr1Tree _ (Single x) = x
foldr1Tree f (Deep _ pr m sf) =
	foldrDigit f (foldrTree (flip (foldrNode f)) (foldr1Digit f sf) m) pr

foldr1Digit :: (a -> a -> a) -> Digit a -> a
foldr1Digit f (One a) = a
foldr1Digit f (Two a b) = a `f` b
foldr1Digit f (Three a b c) = a `f` (b `f` c)
foldr1Digit f (Four a b c d) = a `f` (b `f` (c `f` d))

-- | /O(n*t)/. Fold over the elements of a sequence,
-- associating to the left.
foldl :: (a -> b -> a) -> a -> Seq b -> a
foldl f z (Seq xs) = foldlTree f' z xs
  where f' x (Elem y) = f x y

foldlTree :: (a -> b -> a) -> a -> FingerTree b -> a
foldlTree _ z Empty = z
foldlTree f z (Single x) = z `f` x
foldlTree f z (Deep _ pr m sf) =
	foldlDigit f (foldlTree (foldlNode f) (foldlDigit f z pr) m) sf

foldlDigit :: (a -> b -> a) -> a -> Digit b -> a
foldlDigit f z (One a) = z `f` a
foldlDigit f z (Two a b) = (z `f` a) `f` b
foldlDigit f z (Three a b c) = ((z `f` a) `f` b) `f` c
foldlDigit f z (Four a b c d) = (((z `f` a) `f` b) `f` c) `f` d

foldlNode :: (a -> b -> a) -> a -> Node b -> a
foldlNode f z (Node2 _ a b) = (z `f` a) `f` b
foldlNode f z (Node3 _ a b c) = ((z `f` a) `f` b) `f` c

-- | /O(n*t)/. A variant of 'foldl' that has no base case,
-- and thus may only be applied to non-empty sequences.
foldl1 :: (a -> a -> a) -> Seq a -> a
foldl1 f (Seq xs) = getElem (foldl1Tree f' xs)
  where f' (Elem x) (Elem y) = Elem (f x y)

foldl1Tree :: (a -> a -> a) -> FingerTree a -> a
foldl1Tree _ Empty = error "foldl1: empty sequence"
foldl1Tree _ (Single x) = x
foldl1Tree f (Deep _ pr m sf) =
	foldlDigit f (foldlTree (foldlNode f) (foldl1Digit f pr) m) sf

foldl1Digit :: (a -> a -> a) -> Digit a -> a
foldl1Digit f (One a) = a
foldl1Digit f (Two a b) = a `f` b
foldl1Digit f (Three a b c) = (a `f` b) `f` c
foldl1Digit f (Four a b c d) = ((a `f` b) `f` c) `f` d

------------------------------------------------------------------------
-- Derived folds
------------------------------------------------------------------------

-- | /O(n*t)/. Fold over the elements of a sequence,
-- associating to the right, but strictly.
foldr' :: (a -> b -> b) -> b -> Seq a -> b
foldr' f z xs = foldl f' id xs z
  where f' k x z = k $! f x z

-- | /O(n*t)/. Monadic fold over the elements of a sequence,
-- associating to the right, i.e. from right to left.
foldrM :: Monad m => (a -> b -> m b) -> b -> Seq a -> m b
foldrM f z xs = foldl f' return xs z
  where f' k x z = f x z >>= k

-- | /O(n*t)/. Fold over the elements of a sequence,
-- associating to the left, but strictly.
foldl' :: (a -> b -> a) -> a -> Seq b -> a
foldl' f z xs = foldr f' id xs z
  where f' x k z = k $! f z x

-- | /O(n*t)/. Monadic fold over the elements of a sequence,
-- associating to the left, i.e. from left to right.
foldlM :: Monad m => (a -> b -> m a) -> a -> Seq b -> m a
foldlM f z xs = foldr f' return xs z
  where f' x k z = f z x >>= k

------------------------------------------------------------------------
-- Reverse
------------------------------------------------------------------------

-- | /O(n)/. The reverse of a sequence.
reverse :: Seq a -> Seq a
reverse (Seq xs) = Seq (reverseTree id xs)

reverseTree :: (a -> a) -> FingerTree a -> FingerTree a
reverseTree _ Empty = Empty
reverseTree f (Single x) = Single (f x)
reverseTree f (Deep s pr m sf) =
	Deep s (reverseDigit f sf)
		(reverseTree (reverseNode f) m)
		(reverseDigit f pr)

reverseDigit :: (a -> a) -> Digit a -> Digit a
reverseDigit f (One a) = One (f a)
reverseDigit f (Two a b) = Two (f b) (f a)
reverseDigit f (Three a b c) = Three (f c) (f b) (f a)
reverseDigit f (Four a b c d) = Four (f d) (f c) (f b) (f a)

reverseNode :: (a -> a) -> Node a -> Node a
reverseNode f (Node2 s a b) = Node2 s (f b) (f a)
reverseNode f (Node3 s a b c) = Node3 s (f c) (f b) (f a)

#if TESTING

------------------------------------------------------------------------
-- QuickCheck
------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Seq a) where
	arbitrary = liftM Seq arbitrary
	coarbitrary (Seq x) = coarbitrary x

instance Arbitrary a => Arbitrary (Elem a) where
	arbitrary = liftM Elem arbitrary
	coarbitrary (Elem x) = coarbitrary x

instance (Arbitrary a, Sized a) => Arbitrary (FingerTree a) where
	arbitrary = sized arb
	  where arb :: (Arbitrary a, Sized a) => Int -> Gen (FingerTree a)
		arb 0 = return Empty
		arb 1 = liftM Single arbitrary
		arb n = liftM3 deep arbitrary (arb (n `div` 2)) arbitrary

	coarbitrary Empty = variant 0
	coarbitrary (Single x) = variant 1 . coarbitrary x
	coarbitrary (Deep _ pr m sf) =
		variant 2 . coarbitrary pr . coarbitrary m . coarbitrary sf

instance (Arbitrary a, Sized a) => Arbitrary (Node a) where
	arbitrary = oneof [
			liftM2 node2 arbitrary arbitrary,
			liftM3 node3 arbitrary arbitrary arbitrary]

	coarbitrary (Node2 _ a b) = variant 0 . coarbitrary a . coarbitrary b
	coarbitrary (Node3 _ a b c) =
		variant 1 . coarbitrary a . coarbitrary b . coarbitrary c

instance Arbitrary a => Arbitrary (Digit a) where
	arbitrary = oneof [
			liftM One arbitrary,
			liftM2 Two arbitrary arbitrary,
			liftM3 Three arbitrary arbitrary arbitrary,
			liftM4 Four arbitrary arbitrary arbitrary arbitrary]

	coarbitrary (One a) = variant 0 . coarbitrary a
	coarbitrary (Two a b) = variant 1 . coarbitrary a . coarbitrary b
	coarbitrary (Three a b c) =
		variant 2 . coarbitrary a . coarbitrary b . coarbitrary c
	coarbitrary (Four a b c d) =
		variant 3 . coarbitrary a . coarbitrary b . coarbitrary c . coarbitrary d

------------------------------------------------------------------------
-- Valid trees
------------------------------------------------------------------------

class Valid a where
	valid :: a -> Bool

instance Valid (Elem a) where
	valid _ = True

instance Valid (Seq a) where
	valid (Seq xs) = valid xs

instance (Sized a, Valid a) => Valid (FingerTree a) where
	valid Empty = True
	valid (Single x) = valid x
	valid (Deep s pr m sf) =
		s == size pr + size m + size sf && valid pr && valid m && valid sf

instance (Sized a, Valid a) => Valid (Node a) where
	valid (Node2 s a b) = s == size a + size b && valid a && valid b
	valid (Node3 s a b c) =
		s == size a + size b + size c && valid a && valid b && valid c

instance Valid a => Valid (Digit a) where
	valid (One a) = valid a
	valid (Two a b) = valid a && valid b
	valid (Three a b c) = valid a && valid b && valid c
	valid (Four a b c d) = valid a && valid b && valid c && valid d

#endif
