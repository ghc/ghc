{-# LANGUAGE TypeAbstractions,
             ExplicitNamespaces,
             RequiredTypeArguments,
             DataKinds,
             NoListTuplePuns,
             OverloadedStrings #-}

module T23739a where

import Data.Tuple.Experimental
import GHC.TypeLits

data VisProxy a where
  VP :: forall a -> VisProxy a

f1 :: VisProxy (Int, Bool) -> Unit
f1 (VP (b,c)) = ()

f2 :: VisProxy (Int : Bool : Double : []) -> Unit
f2 (VP [a,b,c]) = ()

f3 :: VisProxy [Int, Bool, Double] -> Unit
f3 (VP [a,b,c]) = ()

f4 :: VisProxy [Int, Bool, Double] -> Unit
f4 (VP (a : b : c : [])) = ()

f5 :: VisProxy "blah" -> Unit
f5 (VP "blah") = ()

f6 :: VisProxy 'c' -> Unit
f6 (VP 'c') = ()

f7 :: VisProxy (UnconsSymbol "blah") -> Unit
f7 (VP (Just ('b', "lah"))) = ()

f8 :: VisProxy a -> Unit
f8 (VP _) = ()

f9 :: VisProxy 42 -> Unit
f9 (VP 42) = ()

f10 :: VisProxy () -> Unit
f10 (VP ()) = ()

f11 :: VisProxy Int -> Unit
f11 (VP Int) = ()

f12 :: VisProxy (Left @Bool @(Maybe b) True) -> Unit
f12 (VP (Left @Bool @(Maybe a) True)) = ()

data Tup a = MkTup a a

f13 :: VisProxy (Int, MkTup 'f' 'g', 42, True, [1,2,3,4,5], (), "blah", "wombat", 'd', UnconsSymbol "corner") -> Unit
f13 (VP (Int, 'f' `MkTup` 'g', 42, True, 1 : 2 : 3 : [4,5], () ,"blah", x, 'd', Just ('c', "orner"))) = ()
