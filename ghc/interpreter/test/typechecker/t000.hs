--!!! Testing typechecker (fixed in Hugs 1.01)

{-
Hi again,

While I am at bug reporting I should as well inform you of another
problem that I encountered.

While testing different variations of the gc-bug test program I
found a difference between what would compile in the original hugs.1.01
and the hacked.hugs that I downloaded from the ftp directory.

In the hacked.hugs I have only changed: SUNOS 0, LINUX 1, and finally
I had to remove the external definition of strchr because it conflicted
with some include file definition. (Of course this will turn out
to be the reason, right?)

I also had to add the Ordering type in hugs.prelude that came with
hacked.hugs.tar.gz, because it was required to be loaded.

Have fun,

Sverker

PS:

The error message was:

ERROR "/home/nilsson/ngof/simpleprims/src/tbugx.gs" (line 15): Insufficient class constraints in instance member binding
*** Context  : (T a, T b, T c)
*** Required : T d

The test program, tbugx.gs, is:

-}
module TestTypes where

class T a where
	t :: Int ->  a

instance T Int where
	t = id

instance (T a, T b) => T (a, b) where
	t p = 
	    (t p, t p)


instance (T a, T b, T c) => T (a, b, c) where
-- The following compiles in hugs1.01, but not in hacked.hugs!
-- It induces the GC bug as well.
	t p =  (a, b, c) where
			tp = t p
			a = fst tp
			bc = snd tp
			b = fst bc
			c = snd bc
-- The following does not induce the GC bug.
-- But as the previous one, it compiles only in hugs1.01, not in hacked.hugs.
--	t p =  (a, b, c) where
--			a = t p
--			bc = t p
--			b = fst bc
--			c = snd bc

t2:: Int -> (Int,Int)
t2 = t			-- t2 has no problems

t3:: Int -> (Int,Int,Int)
t3 = t			-- t3 has problems
