-- From  http://www.reddit.com/r/haskell/comments/ti5il/redblack_trees_in_haskell_using_gadts_existential/

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds#-}
{-# LANGUAGE KindSignatures#-}
module RedBlackTree where

data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)
type One = Succ Zero

data RedBlack = Black | Red deriving (Eq, Ord, Show)

-- red-black trees are rooted at a black node
data RedBlackTree a = forall n. T ( Node Black n a )
deriving instance Show a => Show (RedBlackTree a)

-- all paths from a node to a leaf have exactly n black nodes
data Node :: RedBlack -> Nat -> * -> * where
  -- all leafs are black
  Leaf  :: Node Black One a 
  -- internal black nodes can have children of either color
  B     :: Node cL n a    -> a -> Node cR n a    -> Node Black (Succ n) a 
  -- internal red nodes can only have black children
  R     :: Node Black n a -> a -> Node Black n a -> Node Red n a
deriving instance Show a => Show (Node c n a)

-- one-hole context for red-black trees
data Context :: Nat -> RedBlack -> Nat -> * -> *  where 
  -- if we're at the root, the hole is a black node
  Root :: Context n Black n a
  -- we can go left or right from a red node hole, creating a hole for a black node
  BC   :: Bool -> a -> Node Black n a -> Context m Red n a          -> Context m Black n a
  -- we can go left or right from a black node hole, creating a hole for either
  EC   :: Bool -> a -> Node cY n a    -> Context m Black (Succ n) a -> Context m cX n a
deriving instance Show a => Show (Context m c n a)

data Zipper m a = forall c n. Zipper (Node c n a) (Context m c n a)
deriving instance Show a => Show (Zipper m a)

-- create a zipper
unZip :: Node Black n a -> Zipper n a
unZip = flip Zipper Root

-- destroy a zipper
zipUp :: Zipper m a -> Node Black m a
zipUp (Zipper x Root) = x
zipUp (Zipper x (BC goLeft a y c)) = zipUp $ Zipper (if goLeft then R x a y else R y a x) c
zipUp (Zipper x (EC goLeft a y c)) = zipUp $ Zipper (if goLeft then B x a y else B y a x) c

-- locate the node that should contain a in the red-black tree
zipTo :: Ord a => a -> Zipper n a -> Zipper n a
zipTo _ z@(Zipper Leaf _) = z
zipTo a z@(Zipper (R l a' r) c) = case compare a a' of
  EQ -> z
  LT -> zipTo a $ Zipper l (BC True a' r c)
  GT -> zipTo a $ Zipper r (BC False a' l c)
zipTo a z@(Zipper (B l a' r) c) = case compare a a' of
  EQ -> z
  LT -> zipTo a $ Zipper l (EC True a' r c)
  GT -> zipTo a $ Zipper r (EC False a' l c)

-- create a red-black tree
empty :: RedBlackTree a
empty =  T Leaf

-- insert a node into a red-black tree 
-- (see http://en.wikipedia.org/wiki/Red%E2%80%93black_tree#Insertion)
insert :: Ord a => a -> RedBlackTree a -> RedBlackTree a
insert a t@(T root) = case zipTo a (unZip root) of
    -- find matching leaf and replace with red node (pointing to two leaves)
    Zipper Leaf c -> insertAt (R Leaf a Leaf) c
    -- if it's already in the tree, there's no need to modify it
    _ -> t

insertAt :: Node Red n a -> Context m c n a -> RedBlackTree a
-- 1) new node is root => paint it black and done
insertAt (R l a r) Root = T $ B l a r
-- 2) new node's parent is black => done
insertAt x (EC b a y c) = T . zipUp $ Zipper x (EC b a y c)
-- 3) uncle is red => paint parent/uncle black, g'parent red.  recurse on g'parent
insertAt x (BC pb pa py (EC gb ga (R ul ua ur) gc)) = insertAt g gc
  where p = if pb then B x pa py else B py pa x
        u = B ul ua ur
        g = if gb then R p ga u else R u ga p
-- 4) node is between parent and g'parent => inner rotation
insertAt (R l a r) (BC False pa py pc@(EC True _ _ _)) = insertAt (R py pa l) (BC True a r pc)
insertAt (R l a r) (BC True pa py pc@(EC False _ _ _)) = insertAt (R r pa py) (BC False a l pc)

-- 5) otherwise => outer rotation
insertAt x (BC True pa py (EC True ga gy@Leaf gc)) = 
 T . zipUp $ Zipper (B x pa $ R py ga gy) gc
insertAt x (BC True pa py (EC True ga gy@(B _ _ _) gc)) = 
  T . zipUp $ Zipper (B x pa $ R py ga gy) gc
-- XXX: GHC seems unable to infer that gy is Black so I have to do both cases
--      explicitly, rather than
-- insertAt x (BC False pa py (EC False ga gy gc)) = 
--   T . zipUp $ Zipper (B (R gy ga py) pa x) gc
--
-- insertAt x (BC True pa py (EC True ga gy gc)) = 
--    T . zipUp $ Zipper (B x pa $ R py ga gy) gc
{-
  BC   :: Bool -> a -> Node Black n a -> Context m Red n a          -> Context m Black n a
  EC   :: Bool -> a -> Node cY n a    -> Context m Black (Succ n) a -> Context m cX n a


  BC True pa py (EC True ga gy gc) :: Context m c n a
  Hence c~Black
        EC True ga gy gc :: Context m Red n a
        gy :: Node cY n a
-} 

insertAt x (BC False pa py (EC False ga gy@Leaf gc)) = 
  T . zipUp $ Zipper (B (R gy ga py) pa x) gc
insertAt x (BC False pa py (EC False ga gy@(B _ _ _) gc)) = 
  T . zipUp $ Zipper (B (R gy ga py) pa x) gc

-- can't derive, since we abstract over n, so we have to manually
-- check for identical structure
instance Eq a => Eq (RedBlackTree a) where
  T Leaf == T Leaf = True
  T (B l@(B _ _ _) a r@(B _ _ _)) == T (B l'@(B _ _ _) a' r'@(B _ _ _)) = 
      a == a' && T l == T l' && T r == T r'
  T (B (R ll la lr) a r@(B _ _ _)) == T (B (R ll' la' lr') a' r'@(B _ _ _)) = 
      a == a' && la == la' && 
      T ll == T ll' && T lr == T lr' && T r == T r'
  T (B l@(B _ _ _) a r@(R rl ra rr)) == T (B l'@(B _ _ _) a' r'@(R rl' ra' rr')) = 
      a == a' && ra == ra' && 
      T l == T l' && T rl == T rl' && T rr == T rr'
  T (B (R ll la lr) a r@(R rl ra rr)) == T (B (R ll' la' lr') a' r'@(R rl' ra' rr')) = 
      a == a' && la == la' && ra == ra' &&
      T ll == T ll' && T lr == T lr' && T rl == T rl' && T rr == T rr'
  _ == _ = False

-- can't derive, since B abstracts over child node colors, so
-- manually check for identical structure
instance (Eq a) => Eq (Node c n a)  where
  Leaf == Leaf                = True
  R l a r == R l' a' r'       = a == a' && l == l' && r == r'
  b@(B _ _ _) == b'@(B _ _ _) = T b == T b'
  _ == _                      = False 
