{-# LANGUAGE OverlappingInstances , UndecidableInstances, EmptyDataDecls #-}
{-# LANGUAGE RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances #-}

-- Works with new constraint solver

module T3018 where

import Control.Monad

-- minimal Data/Rep classes
data Rep (ctx :: * -> *) a

class Data (ctx :: * -> *) a where rep :: Rep ctx a 

class Sat a where dict :: a

---------  Version A: failed in 6.12.3 -----------
-- Substitution class
-- substitute [a -> t] t'.
class Subst_A a t t' where
    subst_A :: (Monad m) => a -> t -> t' -> m t'

data SubstD_A a t t' = SubstD_A {substD_A:: (Monad m) => a -> t -> t' -> m t'}

-- Allow override dictionary verion with implementation of type class Subst
instance Subst_A a t t' => Sat (SubstD_A a t t') where
    dict = SubstD_A {substD_A = subst_A}

-- Generic instance
instance Data (SubstD_A a t) t' => Subst_A a t t' where
    subst_A = undefined

---------  Version B: passed in 6.12.3 -----------
-- Substitution class
-- substitute [a -> t] t'.
class Subst_B a t t' where
    subst_B :: a -> t -> t' -> t'

data SubstD_B a t t' = SubstD_B {substD_B :: a -> t -> t' -> t'}

-- allow override dictionary verion with implementation of type class Subst
instance Subst_B a t t' => Sat (SubstD_B a t t') where
    dict = SubstD_B {substD_B = subst_B}

-- generic instance
instance Data (SubstD_B a t) t' => Subst_B a t t' where
    subst_B = undefined


{- Commentary from Trac #3018

Here are the key lines of code:

    class Subst a t t' where
        subst :: (Monad m) => a -> t -> t' -> m t'

    data SubstD a t t' 
      = SubstD (forall m. Monad m => a -> t -> t' -> m t')

    instance Data (SubstD a t) t' => Subst a t t'          -- (1)

    instance Subst a t t' => Sat (SubstD a t t') where     -- (2)
        dict = SubstD subst

The call to 'subst' on the last line gives rise to a constraint (Subst
a t t'). But that constraint can be satisfied in two different ways:

    Using the instance declaration for Subst (which matches anything!)
    Using the context of the Sat (SubstD ..) instance declaration itself 

If GHC uses (1) it gets into a corner it can't get out of, because now
it needs (Data (SubstD a t) t'), and that it can't get. The error
message is a bit misleading:

T3018.hs:29:28:
    Could not deduce (Data (SubstD a t) t') from the context (Monad m)
      arising from a use of `subst' at T3018.hs:29:28-32

it should really say

 ...from the context (Subst a t t', Monad m)

but that's a bit of a separate matter.

Now, you are hoping that (2) will happen, but I hope you can see that
it's delicate. Adding the (Monad m) context just tips things over the
edge so that GHC doesn't "see" the (Subst a t t') in the context until
too late. But the real problem is that you are asking too much. Here
is a simpler example:

    f :: Eq [a] => a -> blah
    f x = let g :: Int -> Int
              g = ....([x]==[x])...
          in ...

The use of == requires Eq [a], but GHC will probably use the list
equality instance to simplify this to Eq a; and then it can't deduce
Eq a from Eq [a]. Local constraints that shadow or override global
instance declarations are extremely delicate.

All this is perhaps soluble if GHC were to be lazier about solving
constraints, and only makes the attempt when it has all the evidence
in hand. I'm thinking quite a bit about constraint solving at the
moment and will bear that in mind. But I can't offer you an immediate
solution. At least I hope I've explained the problem.
-}