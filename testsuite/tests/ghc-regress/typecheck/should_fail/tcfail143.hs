{-# LANGUAGE UndecidableInstances, FlexibleInstances,
             MultiParamTypeClasses, FunctionalDependencies #-}

module Foo  where 

data Z		= Z
data S a	= S a

class MinMax a b c d | a b -> c d, a c d -> b, b c d -> a
instance MinMax Z Z Z Z -- (a)
instance MinMax a Z Z a -- (b)   -- L1: wrongly flagged as error src.
instance MinMax Z b Z b -- (c)
instance MinMax a b c d => MinMax (S a) (S b) (S c) (S d)
                        -- (d)

class Extend a b  where extend :: a -> b ->  b
instance Extend Z b where  Z `extend` b = b
instance MinMax a b _c b  => Extend a b where 
	_a `extend` b = b

t	:: MinMax a b _c d => a -> b -> d
t _ _	= (undefined :: d)

n0	= Z
n1	= S n0

t1 = n1 `t` n0	    -- L2

t2 = n1 `extend` n0 -- L3: uncommenting just this line produces
		    --	   an error message pointing at L1 and L2
		    --	   with no mention of the real culprit, L3.

-- t1	:: S Z	    -- L4: uncommenting this and L3 produces an
		    --	error message rightly pointing at L2 and L3.


{-  n0 :: Z;  n1 :: S Z

Call of extend gives wanted: Extend (S Z) Z
Use instance =>  MinMax (S Z) Z gamma Z
FD on (b) => gamma ~ Z, Z ~ S Z
             =>  MinMax (S Z) Z Z Z
FD on (a), 3rd fundep => Z ~ S Z
      (b) again (sadly)   Z ~ S Z

-}
{-

Here's what is happening.

Lacking the type signature t1 :: S Z, we get

  n0 :: Z
  n1 :: S v1
  t1 :: d1 	with constraint	([L2] MinMax (S v1) Z c1 d1)
  t2 :: Z	with constraint ([L3] Extend (S v1) Z)

     [L2] MinMax (S v1) Z c1 d1, [L3] Extend (S v1) Z
---> 	<by instance for Extend a b>
     [L2] MinMax (S v1) Z c1 d1, [L3] MinMax (S v1) Z c2 Z}
--->	<combining these two constraints using (a b -> c d)
     [L2] MinMax (S v1) Z c1 Z, [L3] MinMax (S v1) Z c1 Z}

Now there are the two constraints are indistinguishable,
and both give rise to the same error:

--->	<combining first with [L1] instance MinMax a Z Z a>
     c1=Z, Z=S v1	ERROR

In either case, the error points to L1.


A different sequence leads to a different error:

     [L2] MinMax (S v1) Z c1 d1, [L3] Extend (S v1) Z
---> 	<by instance for Extend a b>
     [L2] MinMax (S v1) Z c1 d1, [L3] MinMax (S v1) Z c2 Z}
--->	<combining first with [L1] instance MinMax a Z Z a>
     [L2] MinMax (S v1) Z Z (S2 v1), [L3] MinMax (S v1) Z c2 Z}

Now combining the two constraints gives rise to the error, but
this time pointing to L2,L3.

I can't explain exactly why adding the type signature for t1
changes the order.


Hmm.  Perhaps a good improvement strategy would be: 
  - first do improvement against the instance declartions
  - and only then do pairwise improvement between constraints

I've implemented that, and indeed it improves the result. 
Instead of:

  Foo.hs:1:0:
    Couldn't match `S Z' against `Z'
      Expected type: S Z
      Inferred type: Z
    When using functional dependencies to combine
      MinMax a Z Z a, arising from the instance declaration at Foo.hs:10:0
      MinMax (S Z) Z _c d, arising from use of `t' at Foo.hs:25:8-10

we get

  Foo.hs:1:0:
    Couldn't match `S Z' against `Z'
      Expected type: S Z
      Inferred type: Z
    When using functional dependencies to combine
      MinMax a Z Z a, arising from the instance declaration at Foo.hs:10:0
      MinMax (S Z) Z _c Z, arising from use of `extend' at Foo.hs:27:8-15


And this error in t2 is perfectly correct.  You get it even if you comment
out the entire definition of t1.
-}