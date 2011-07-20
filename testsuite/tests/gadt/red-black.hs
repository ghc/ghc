{-# LANGUAGE GADTs #-}

module ShouldCompile where

-- data RBTree = forall n. Root (SubTree Black n)

-- Kind Colour
data Red 
data Black

-- Kind Nat
data Z
data S a

data SubTree c n where
  Leaf :: SubTree Black Z
  RNode :: SubTree Black n -> Int -> SubTree Black n -> SubTree Red n
  BNode :: SubTree cL m   ->  Int -> SubTree cR m    -> SubTree Black (S m)
  Fix   :: SubTree Red n -> SubTree Black n


ins :: Int -> SubTree c n -> SubTree c n
ins n Leaf 		     = (Fix (RNode Leaf n Leaf))
ins n (BNode x m y) | n <= m = black (ins n x) m y
ins n (BNode x m y) | n > m  = black x m (ins n y)
ins n (RNode x m y) | n <= m = RNode (ins n x) m y
ins n (RNode x m y) | n > m  = RNode x m (ins n y)
 
black :: SubTree c n -> Int -> SubTree d n -> SubTree Black (S n)
black (RNode (Fix u) v c) w (x@(RNode _ _ _)) = Fix(RNode (blacken u) v (BNode c w x))

black (RNode (Fix u) v c) w (x@(BNode _ _ _)) = BNode u v (RNode c w x)
black (RNode a v (Fix (RNode b u c))) w (x@(BNode _ _ _)) = BNode (RNode a v b) u (RNode c w x)
black (Fix x) n (Fix y) = BNode x n y
black x       n (Fix y) = BNode x n y
black (Fix x) n y       = BNode x n y
black x       n y       = BNode x n y

blacken :: SubTree Red n -> SubTree Black (S n)
blacken (RNode l e r) = (BNode l e r)

