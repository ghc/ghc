module T24229a where

newtype N a = MkN a

foo :: Int -> N (a,a) -> Maybe (a,a)
foo 0 (MkN p)     = Just p
foo n (MkN (x,y)) = foo (n-1) (MkN (y,x))

-- We should generate ONE specialisation for $wfoo,
-- and it should fire TWICE, regardless of the order
-- of the following two definitions.

wombat1 = foo 20 (MkN ("yes", "no"))
wombat2 xs ys = foo 3 (MkN (xs, ys))
