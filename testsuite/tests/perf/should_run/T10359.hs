{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}

module Main( main, boo ) where

import Prelude hiding (repeat)

boo xs f = (\x -> f x, xs)

repeat :: Int -> (a -> a) -> a -> a
repeat 1 f x = f x
repeat n f x = n `seq` x `seq` repeat (n-1) f $ f x

---- Buggy version
------------------

type Numerical a = (Fractional a, Real a)

data Box a = Box
    { func :: forall dum. (Numerical dum) => dum -> a -> a
    , obj :: !a }

do_step :: (Numerical num) => num -> Box a -> Box a
do_step number Box{..} = Box{ obj = func number obj, .. }

start :: Box Double
start = Box { func = \x y -> realToFrac x + y
            , obj = 0 }

test :: Int -> IO ()
test steps = putStrLn $ show $ obj $ repeat steps (do_step 1) start

---- Driver
-----------

main :: IO ()
main = test 2000 -- compare test2 10000000 or test3 10000000, but test4 20000


{-
---- No tuple constraint synonym is better
------------------------------------------

data Box2 a = Box2
    { func2 :: forall num. (Fractional num, Real num) => num -> a -> a
    , obj2 :: !a }

do_step2 :: (Fractional num, Real num) => num -> Box2 a -> Box2 a
do_step2 number Box2{..} = Box2{ obj2 = func2 number obj2, ..}

start2 :: Box2 Double
start2 = Box2 { func2 = \x y -> realToFrac x + y
              , obj2 = 0 }

test2 :: Int -> IO ()
test2 steps = putStrLn $ show $ obj2 $ repeat steps (do_step2 1) start2

---- Not copying the function field works too
---------------------------------------------

do_step3 :: (Numerical num) => num -> Box a -> Box a
do_step3 number b@Box{..} = b{ obj = func number obj }

test3 :: Int -> IO ()
test3 steps = putStrLn $ show $ obj $ repeat steps (do_step3 1) start

---- But record wildcards are not at fault
------------------------------------------

do_step4 :: (Numerical num) => num -> Box a -> Box a
do_step4 number Box{func = f, obj = x} = Box{ obj = f number x, func = f }

test4 :: Int -> IO ()
test4 steps = putStrLn $ show $ obj $ repeat steps (do_step4 1) start
-}


{-
First of all, very nice example. Thank you for making it so small and easy to work with.

I can see what's happening. The key part is what happens here:
{{{
do_step4 :: (Numerical num) => num -> Box a -> Box a
do_step4 number Box{ func = f, obj = x}
              = Box{ func = f, obj = f number x }
}}}
After elaboration (ie making dictionaries explicit) we get this:
{{{
do_step4 dn1 number (Box {func = f, obj = x })
  = Box { func = \dn2 -> f ( case dn2 of (f,r) -> f
                           , case dn2 of (f,r) -> r)
        , obj = f dn1 number x }
}}}
That's odd!  We expected this:
{{{
do_step4 dn1 number (Box {func = f, obj = x })
  = Box { func = f
        , obj = f dn1 number x }
}}}
And indeed, the allocation of all those `\dn2` closures is what is causing the problem.
So we are missing this optimisation:
{{{
   (case dn2 of (f,r) -> f, case dn2 of (f,r) -> r)
===>
   dn2
}}}
If we did this, then the lambda would look like `\dn2 -> f dn2` which could eta-reduce to `f`.
But there are at least three problems:
 * The tuple transformation above is hard to spot
 * The tuple transformation is not quite semantically right; if `dn2` was bottom, the LHS and RHS are different
 * The eta-reduction isn't quite semantically right: if `f` ws bottom, the LHS and RHS are different.

You might argue that the latter two can be ignored because dictionary arguments are special;
indeed we often toy with making them strict.

But perhaps a better way to avoid the tuple-transformation issue would be not to construct that strange expression in the first place. Where is it coming from?  It comes from the call to `f` (admittedly applied to no arguments) in `Box { ..., func = f }`.  GHC needs a dictionary for `(Numerical dum)` (I changed the name of the type variable in `func`'s type in the definition of `Box`).  Since it's just a pair GHC says "fine, I'll build a pair, out of `Fractional dum` and `Real dum`.  How does it get those dictionaries?  By selecting the components of the `Franctional dum` passed to `f`.

If GHC said instead "I need `Numerical dum` and behold I have one in hand, it'd be much better. It doesn't because tuple constraints are treated specially.  But if we adopted the idea in #10362, we would (automatically) get to re-use the `Numerical dum` constraint.  That would leave us with eta reduction, which is easier.

As to what will get you rolling, a good solution is `test3`, which saves instantiating and re-generalising `f`. The key thing is to update all the fields ''except'' the polymorphic `func` field. I'm surprised you say that it doesn't work.  Can you give a (presumably more complicated) example to demonstrate? Maybe there's a separate bug!

-}


