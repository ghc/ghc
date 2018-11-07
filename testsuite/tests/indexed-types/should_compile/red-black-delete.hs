{-# LANGUAGE InstanceSigs, GADTs, StandaloneDeriving, DataKinds, KindSignatures, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

-- Implementation of deletion for red black trees by Matt Might
-- Editing to preserve the red/black tree invariants by Stephanie Weirich,
-- Dan Licata and John Hughes, at WG2.8 August 2014

-- Original available from:
--   http://matt.might.net/articles/red-black-delete/code/RedBlackSet.hs
-- Slides:
--   http://matt.might.net/papers/might2014redblack-talk.pdf

module MightRedBlackGADT where

import Prelude hiding (max)
import Data.List(nub,sort)
import Control.Monad(liftM)
import Data.Type.Equality
import Data.Kind (Type)
import Data.Maybe(isJust)

--
data Color = Red | Black | DoubleBlack | NegativeBlack

-- Singleton type, connects the type level color to data
-- not necessary in a full-spectrum dependently-typed language
data SColor (c :: Color) where
   R  :: SColor Red -- red
   B  :: SColor Black -- black
   BB :: SColor DoubleBlack -- double black
   NB :: SColor NegativeBlack -- negative black


-- natural numbers for tracking the black height
data Nat = Z | S Nat

-- Well-formed Red/Black trees
-- n statically tracks the black height of the tree
-- c statically tracks the color of the root node
data CT (n :: Nat) (c :: Color) (a :: Type) where
   E  :: CT Z Black a
   T  :: Valid c c1 c2 =>
         SColor c -> (CT n c1 a) -> a -> (CT n c2 a) -> CT (Incr c n) c a

-- this type class forces the children of red nodes to be black
-- and also excludes double-black and negative-black nodes from
-- a valid red-black tree.
class Valid (c :: Color) (c1 :: Color) (c2 :: Color) where
instance Valid Red Black Black
instance Valid Black c1 c2

-- incrementing the black height, based on the color
type family Incr (c :: Color) (n :: Nat) :: Nat
type instance Incr Black n             = S n
type instance Incr DoubleBlack n       = S (S n)
type instance Incr Red   n             = n
type instance Incr NegativeBlack (S n) = n

-- the top level type of red-black trees
-- the data constructor forces the root to be black and
-- hides the black height.
data RBSet a where
  Root :: (CT n Black a) -> RBSet a

deriving instance Show (SColor c)
deriving instance Show a => Show (CT n c a)
instance Show a => Show (RBSet a) where
  show (Root x) = show x


showT :: CT n c a -> String
showT E = "E"
showT (T c l x r) =
    "(T " ++ show c ++ " " ++ showT l ++ " "
          ++ "..." ++ " " ++ showT r ++ ")"


-- the test equality class gives us a proof that type level
-- colors are equal.
-- testEquality :: SColor c1 -> SColor c2 -> Maybe (c1 ~ c2)
instance TestEquality SColor where
  testEquality R R   = Just Refl
  testEquality B B   = Just Refl
  testEquality BB BB = Just Refl
  testEquality NB NB = Just Refl
  testEquality _ _   = Nothing

-- comparing two Red/Black trees for equality.
-- unfortunately, because we have two instances, we can't
-- use the testEquality class.
(%==%) :: Eq a => CT n1 c1 a -> CT n2 c2 a -> Bool
E %==% E = True
T c1 a1 x1 b1 %==% T c2 a2 x2 b2 =
  isJust (testEquality c1 c2) && a1 %==% a2 && x1 == x2 && b1 %==% b2

instance Eq a => Eq (RBSet a) where
  (Root t1) == (Root t2) = t1 %==% t2


 -- Public operations --

empty :: RBSet a
empty = Root E

member :: (Ord a) => a -> RBSet a -> Bool
member x (Root t) = aux x t where
  aux :: Ord a => a -> CT n c a -> Bool
  aux x E = False
  aux x (T _ l y r) | x < y     = aux x l
                    | x > y     = aux x r
                    | otherwise = True

elements :: Ord a => RBSet a -> [a]
elements (Root t) = aux t [] where
      aux :: Ord a => CT n c a -> [a] -> [a]
      aux E acc = acc
      aux (T _ a x b) acc = aux a (x : aux b acc)

 -- INSERTION --

insert :: (Ord a) => a -> RBSet a -> RBSet a
insert x (Root s) = blacken (ins x s)
 where ins :: Ord a => a -> CT n c a -> IR n a
       ins x E = IN R E x E
       ins x s@(T color a y b) | x < y     = balanceL color (ins x a) y b
                               | x > y     = balanceR color a y (ins x b)
                               | otherwise = (IN color a y b)


       blacken :: IR n a -> RBSet a
       blacken (IN _ a x b) = Root (T B a x b)

-- an intermediate data structure that temporarily violates the
-- red-black tree invariants during insertion. (IR stands for infra-red).
-- This tree must be non-empty, but is allowed to have two red nodes in
-- a row.
data IR n a where
   IN :: SColor c -> CT n c1 a -> a -> CT n c2 a -> IR (Incr c n) a

-- `balance` rotates away coloring conflicts
-- we separate it into two cases based on whether the infraction could
-- be on the left or the right

balanceL :: SColor c -> IR n a -> a -> CT n c1 a -> IR (Incr c n) a
balanceL B (IN R (T R a x b) y c) z d = IN R (T B a x b) y (T B c z d)
balanceL B (IN R a x (T R b y c)) z d = IN R (T B a x b) y (T B c z d)
balanceL c (IN B a x b) z d           = IN c (T B a x b) z d
balanceL c (IN R a@(T B _ _ _) x b@(T B _ _ _)) z d = IN c (T R a x b) z d
balanceL c (IN R a@E x b@E) z d       = IN c (T R a x b) z d


balanceR :: SColor c -> CT n c1 a -> a -> IR n a -> IR (Incr c n) a
balanceR B a x (IN R (T R b y c) z d) = IN R (T B a x b) y (T B c z d)
balanceR B a x (IN R b y (T R c z d)) = IN R (T B a x b) y (T B c z d)
balanceR c a x (IN B b z d)           = IN c a x (T B b z d)
balanceR c a x (IN R b@(T B _ _ _) z d@(T B _ _ _)) = IN c a x (T R b z d)
balanceR c a x (IN R b@E z d@E)       = IN c a x (T R b z d)


 -- DELETION --

-- like insert, delete relies on a helper function that may (slightly)
-- violate the red-black tree invariant

delete :: (Ord a) => a -> RBSet a -> RBSet a
delete x (Root s) = blacken (del x s)
 where blacken :: DT n a -> RBSet a
       blacken (DT B a x b)  = Root (T B a x b)
       blacken (DT BB a x b) = Root (T B a x b)
       blacken DE  = Root E
       blacken DEE = Root E
       -- note: del always produces a black or
       -- double black tree. These last two cases are unreachable
       -- but it may be difficult to prove it.
       blacken (DT R a x b) = Root (T B a x b)
       blacken (DT NB a x b) = Root (T B a x b)

       del :: Ord a => a -> CT n c a -> DT n a
       del x E = DE
       del x s@(T color a y b) | x < y     = bubbleL color (del x a) y b
                               | x > y     = bubbleR color a y (del x b)
                               | otherwise = remove s


-- deletion tree, the result of del
-- could have a double black node
-- (but only at the top or as a single leaf)
data DT n a where
  DT  :: SColor c -> CT n c1 a -> a -> CT n c2 a -> DT (Incr c n) a
  DE  :: DT Z a
  DEE :: DT (S Z) a

-- Note: we could unify this with the IR data structure by
-- adding the DE and DEE data constructors. That would allow us to
-- reuse the same balancing function (and blacken function)
-- for both insertion and deletion.
-- However, if an Agda version did that it might get into trouble,
-- trying to show that insertion never produces a leaf.


instance Show a => Show (DT n a) where
  show DE = "DE"
  show DEE = "DEE"
  show (DT c l x r) =
    "(DT " ++ show c ++ " " ++ show l ++ " "
           ++ "..." ++ " " ++ show r ++ ")"

-- Note: eliding a to make error easier below.
showDT :: DT n a -> String
showDT DE = "DE"
showDT DEE = "DEE"
showDT (DT c l x r) =
    "(DT " ++ show c ++ " " ++ showT l ++ " "
           ++ "..." ++ " " ++ showT r ++ ")"


-- maximum element from a red-black tree
max :: CT n c a -> a
max E = error "no largest element"
max (T _ _ x E) = x
max (T _ _ x r) = max r

-- Remove the top node: it might leave behind a double black node
-- if the removed node is black (note that the height is preserved)
remove :: CT n c a -> DT n a
remove (T R E _ E) = DE
remove (T B E _ E) = DEE
remove (T B E _ (T R a x b)) = DT B a x b
remove (T B (T R a x b) _ E) = DT B a x b
remove (T color l y r) = bubbleL color (removeMax l) (max l) r

-- remove the maximum element of the tree
removeMax :: CT n c a -> DT n a
removeMax E                 = error "no maximum to remove"
removeMax s@(T _ _ _ E)     = remove s
removeMax s@(T color l x r) = bubbleR color l x (removeMax r)

-- make a node more red (i.e. decreasing its black height)
-- note that the color of the resulting node is not part of the
-- result height because the result of this operation doesn't
-- necessarily satisfy the RBT invariants
redden :: CT (S n) c a -> DT n a
redden (T B a x y)  = DT R a x y
redden (T BB a x y) = DT B a x y
redden (T R a x y)  = DT NB a x y

-- assert that a tree is red. Dynamically fail otherwise
-- this is a cheat that Haskell let's us get away with, but Agda
-- would not
assertRed :: DT n a -> CT n Red a
assertRed (DT R (T B aa ax ay) x (T B ac az ad)) =
  (T R (T B aa ax ay) x (T B ac az ad))
assertRed t = error ("not a red tree " ++ showDT t)

dbalanceL :: SColor c -> DT n a -> a -> CT n c1 a -> DT (Incr c n) a
-- reshuffle (same as insert)
dbalanceL B (DT R (T R a x b) y c) z d = DT R (T B a x b) y (T B c z d)
dbalanceL B (DT R a x (T R b y c)) z d = DT R (T B a x b) y (T B c z d)
-- double-black (new for delete)
dbalanceL BB (DT R (T R a x b) y c) z d = DT B (T B a x b) y (T B c z d)
dbalanceL BB (DT R a x (T R b y c)) z d = DT B (T B a x b) y (T B c z d)
dbalanceL BB (DT NB a@(T B _ _ _) x (T B b y c)) z d =
  case (dbalanceL B (redden a) x b) of
    r@(DT R _ _ _)  -> DT B (assertRed r)  y (T B c z d)
    (DT B a1 x1 y1) -> DT B (T B a1 x1 y1) y (T B c z d)
-- fall through cases (same as insert)
dbalanceL c (DT B a x b) z d                         = DT c (T B a x b) z d
dbalanceL c (DT R a@(T B _ _ _) x b@(T B _ _ _)) z d = DT c (T R a x b) z d
dbalanceL c (DT R a@E x b@E) z d                     = DT c (T R a x b) z d
-- more fall through cases (new for delete)
dbalanceL c DE x b = (DT c E x b)
dbalanceL c a x b = error ("no case for " ++ showDT a)


dbalanceR :: SColor c -> CT n c1 a -> a -> DT n a -> DT (Incr c n) a
dbalanceR B a x (DT R (T R b y c) z d) = DT R (T B a x b) y (T B c z d)
dbalanceR B a x (DT R b y (T R c z d)) = DT R (T B a x b) y (T B c z d)
dbalanceR BB a x (DT R (T R b y c) z d) = DT B (T B a x b) y (T B c z d)
dbalanceR BB a x (DT R b y (T R c z d)) = DT B (T B a x b) y (T B c z d)
dbalanceR BB a x (DT NB (T B b y c) z d@(T B _ _ _)) =
  case (dbalanceR B c z (redden d)) of
        r@(DT R _ _ _)  -> DT B (T B a x b) y (assertRed r)
        (DT B a1 x1 y1) -> DT B (T B a x b) y (T B a1 x1 y1)

dbalanceR c a x (DT B b z d)           = DT c a x (T B b z d)
dbalanceR c a x (DT R b@(T B _ _ _) z d@(T B _ _ _)) = DT c a x (T R b z d)
dbalanceR c a x (DT R b@E z d@E) = DT c a x (T R b z d)
dbalanceR c a x DE = DT c a x E
dbalanceR c a x b = error ("no case for " ++ showDT b)


bubbleL :: SColor c -> DT n a -> a -> CT n c1 a -> DT (Incr c n) a
-- don't want to prove generally that
-- (Incr (Blacker c) 'Z ~ Incr c ('S 'Z)) so expand by cases.
-- (Note: Do we need all three of these cases?)
bubbleL B DEE x r   = dbalanceR BB E x (redden r)
bubbleL R DEE x r   = dbalanceR B E x (redden r)
bubbleL NB DEE x r  = dbalanceR R E x (redden r)
-- same thing here
bubbleL B l@(DT BB a y b) x r  = dbalanceR BB (T B a y b) x (redden r)
bubbleL R l@(DT BB a y b) x r  = dbalanceR B (T B a y b) x (redden r)
bubbleL NB l@(DT BB a y b) x r = dbalanceR R (T B a y b) x (redden r)
-- fall through case
bubbleL color l x r = dbalanceL color l x r


bubbleR :: SColor c -> CT n c1 a -> a -> DT n a -> DT (Incr c n) a
bubbleR B r x DEE    = dbalanceL BB (redden r) x E
bubbleR R r x DEE    = dbalanceL B  (redden r) x E
bubbleR NB r x DEE   = dbalanceL R  (redden r) x E
-- same thing here
bubbleR B r x l@(DT BB a y b) = dbalanceL BB (redden r) x (T B a y b)
bubbleR R r x l@(DT BB a y b)  = dbalanceL B (redden r) x (T B a y b)
bubbleR NB r x l@(DT BB a y b) = dbalanceL R (redden r) x (T B a y b)
-- fall through case
bubbleR color l x r = dbalanceR color l x r

