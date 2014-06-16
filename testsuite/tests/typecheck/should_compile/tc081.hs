-- !!! an example Simon made up
--
module ShouldSucceed where

f x = (x+1, x<3, g True, g 'c')
	where
	g y = if x>2 then [] else [y]
{-
Here the type-check of g will yield an LIE with an Ord dict
for x.  g still has type forall a. a -> [a].  The dictionary is
free, bound by the x.

It should be ok to add the signature:
-}

f2 x = (x+1, x<3, g2 True, g2 'c')
	where
	-- NB: this sig:
	g2 :: a -> [a]
	g2 y = if x>2 then [] else [y]
{-
or to write:
-}

f3 x = (x+1, x<3, g3 True, g3 'c')
	where
	-- NB: this line:
        g3 :: a -> [a]
	g3 = (\ y -> if x>2 then [] else [y])::(a -> [a])
