{-
From augustss@cs.chalmers.se Sat Jan 11 11:56:04 1992
From: augustss@cs.chalmers.se (Lennart Augustsson)
Newsgroups: comp.lang.functional
Subject: Re: some kindof benchmark
Keywords: n
Date: 10 Jan 92 21:59:05 GMT
Organization: Chalmers University of Technology

>  My system (running on a Sun-SPARC SLC)
>  does it in 93 seconds and uses about
>  412k memory to give a motivation.

I can't resist benchmarks!  I did a quick translation to
Haskell and here is the result using hbc.
-}

----------------------------------------------------------
import System.Environment

infix 8 ^^^

data Nat = Z | S Nat deriving (Eq,Ord, Show {-was:Text-})

instance Num Nat where
    Z   + y   = y
    S x + y   = S (x + y)
    x   * Z   = Z
    x   * S y = x * y + x
    fromInteger x = if x < 1 then Z else S (fromInteger (x-1))

-- partain:sig
int :: Nat -> Int

int Z     = 0
int (S x) = 1 + int x

x ^^^ Z   = S Z
x ^^^ S y = x * (x ^^^ y)

main = do
	[power] <- getArgs
	print $ int (3 ^^^ (fromInteger $ read power))

--
-- Timing for hbc version 0.997.2
-- Heap set to 1 Mbyte
--
-- SPARC-SLC		78s (13% GC)
-- DEC5500		27s (16% GC)
-- Sequent Symmetry	165s (16% GC)
-- SUN3/180		148s (15% GC)
-- 
-- Sorry, but I havn't recompiled the compiler for any other
-- platforms yet.
--
-- 
{-

	-- Lennart Augustsson
[This signature is intentionally left blank.]

From aspect@sun1d.informatik.Uni-Bremen.DE Sat Jan 18 13:25:48 1992
From: aspect@sun1d.informatik.Uni-Bremen.DE (Joern von Holten)
Newsgroups: comp.lang.functional
Subject: Re: some kindof benchmark
Date: 17 Jan 92 10:06:57 GMT
Organization: Universitaet Bremen
Nntp-Posting-Host: sun1d


ok guys,
 
  we are responsible for the '3^8 benchmark' ... and we gave a
first approximative result of 93 sec and 412 K (old compiler version).

Here's the final result for our ASpecT compiler ... it's a strict functional
language based on algebraic specifications.

---- Sun 4/20(SLC):   9.8s   (412k) ----

and comparable results for other platforms (we are generating C as target language).

we hoped that our benchmark would initiate a collection of various outcoming
benchmarks for functional language compilers.
Where are all these compiler-freaks?

:-)

-- Joern von Holten
 

-}
