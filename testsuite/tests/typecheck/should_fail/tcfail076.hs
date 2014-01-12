{-# LANGUAGE RankNTypes #-}

{- 
	From: Ralf Hinze <ralf@uran.informatik.uni-bonn.de>
	Date: Fri, 15 Aug 1997 15:20:51 +0200 (MET DST)

I *suppose* that there is a bug in GHC's type checker. The following
program, which I think is ill-typed, passes silently the type checker.
Needless to say that it uses some of GHC's arcane type extensions.
-}

module ShouldFail where

data ContT m a		=  KContT (forall res. (a -> m res) -> m res)
unKContT (KContT x)	=  x

callcc			:: ((a -> ContT m b) -> ContT m a) -> ContT m a
callcc f		=  KContT (\cont -> unKContT (f (\a -> KContT (\cont' -> cont a))) cont)

{-
`ContT' is a continuation monad transformer. Note that we locally
qualify over the result type `res' (sometimes called answer or
output).  IMHO this make it impossible to define control constructs
like `callcc'. Let's have a closer look: the code of `callcc' contains
the subexpression `KContT (\cont' -> cont a)'. To be well-typed the
argument of `KContT' must have the type `(All res) => (a -> m res) -> m
res'. Quantification is not possible, however, since the type variable
in `cont's type cannot be forall'd, since it also appears at an outer
level.  Right? Or wrong?
-}
