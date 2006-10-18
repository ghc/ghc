{-# OPTIONS_GHC -fglasgow-exts #-}

-- This program sent GHC 6.6 into a loop, because the fixpointing
-- of the substitution in type refinement got its in-scope-set
-- from the answer!

module ShouldCompile where

------------------
data Foo a b where F :: a -> Foo () a

bar :: Foo () (forall a.a) -> ()
bar (F _) = ()

------------------
data Foo2 a where F2 :: a -> Foo2 [a]

bar2 :: Foo2 [forall a.a] -> ()
bar2 (F2 _) = ()
