{-# LANGUAGE TypeAbstractions,
             ExplicitNamespaces,
             RequiredTypeArguments,
             DataKinds,
             NoListTuplePuns,
             OverloadedStrings #-}

module T23739a where

import Data.Tuple.Experimental
import GHC.TypeLits

{-
This code aims to test pattern-to-type transformation
(See Note [Pattern to type (P2T) conversion] in GHC.Tc.Gen.Pat)

However it relies on a questionable feature, that allows us to have
equality constraint in scope of type pattern checking. The test
doesn't establish such behavior, it just abuses it to examine P2T
transformation.

In the happy future with `forall->` in GADTs we should
rewrite this test using it.
-}

f1 :: forall a -> a ~ (Int, Bool) => Unit
f1 (b,c) = ()

f2 :: forall a -> a ~ (Int : Bool : Double : []) => Unit
f2 [a,b,c] = ()

f3 :: forall a -> a ~ [Int, Bool, Double] => Unit
f3 [a,b,c] = ()

f4 :: forall a -> a ~ [Int, Bool, Double] => Unit
f4 (a : b : c : []) = ()

f5 :: forall a -> a ~ "blah" => Unit
f5 "blah" = ()

f6 :: forall a -> a ~ 'c' => Unit
f6 'c' = ()

f7 :: forall a -> a ~ UnconsSymbol "blah" => Unit
f7 (Just ('b', "lah")) = ()

f8 :: forall a -> Unit
f8 _ = ()

f9 :: forall a -> a ~ 42 => Unit
f9 42 = ()

f10 :: forall a -> a ~ () => Unit
f10 () = ()

f11 :: forall a -> a ~ Int => Unit
f11 Int = ()

f12 :: forall a -> a ~ (Left @Bool @(Maybe b) True) => Unit
f12 (Left @Bool @(Maybe a) True) = ()

data Tup a = MkTup a a

f13 :: forall a -> a ~ (Int, MkTup 'f' 'g', 42, True, [1,2,3,4,5], (), "blah", "wombat", 'd', UnconsSymbol "corner") => Unit
f13 (Int, 'f' `MkTup` 'g', 42, True, 1 : 2 : 3 : [4,5], () ,"blah", x, 'd', Just ('c', "orner")) = ()
