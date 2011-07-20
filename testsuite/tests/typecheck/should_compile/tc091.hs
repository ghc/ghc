-- !!! Test polymorphic recursion


-- With polymorphic recursion this one becomes legal
--	SLPJ June 97.

{-
To: Lennart Augustsson <augustss@cs.chalmers.se>
Cc: partain@dcs.gla.ac.uk, John Peterson (Yale) <peterson-john@cs.yale.edu>, 
    simonpj@dcs.gla.ac.uk
Subject: Type checking matter
Date: Fri, 23 Oct 92 15:28:38 +0100
From: Simon L Peyton Jones <simonpj@dcs.gla.ac.uk>


I've looked at the enclosed again.  It seems to me that
since "s" includes a recursive call to "sort", inside the body
of "sort", then "sort" is monomorphic, and hence so is "s";
hence the type signature (which claims full polymorphism) is 
wrong.

[Lennart says he can't see any free variables inside "s", but there
is one, namely "sort"!]

Will: one for the should-fail suite?

Simon


------- Forwarded Message


From: Lennart Augustsson <augustss@cs.chalmers.se>
To: partain
Subject: Re: just to show you I'm a nice guy...
Date: Tue, 26 May 92 17:30:12 +0200

> Here's a fairly simple module from our compiler, which includes what
> we claim is an illegal type signature (grep ILLEGAL ...).
> Last time I checked, hbc accepted this module.

Not that I don't believe you, but why is this illegal?
As far as I can see there are no free variables in the function s,
which makes me believe that it can typechecked like a top level
definition.  And for a top level defn the signature should be
all right.

	-- Lennart
- ------- End of forwarded message -------
-}
module ShouldSucceed where

sort :: Ord a => [a] -> [a]
sort xs = s xs (length xs)
   where
      s :: Ord b => [b] -> Int -> [b]	-- This signature is WRONG
      s xs k = if k <= 1 then xs
               else merge (sort ys) (sort zs)
	       where (ys,zs) = init_last xs (k `div` (2::Int))

-- Defns of merge and init_last are just dummies with the correct types
merge :: Ord a => [a] -> [a] -> [a]
merge xs ys = xs

init_last :: [a] -> Int -> ([a],[a])
init_last a b = (a,a)

