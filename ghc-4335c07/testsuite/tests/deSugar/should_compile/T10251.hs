{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O #-}
module T10251 where

data D = D
data E = E

class Storable a where
    poke2 :: a -> E
instance Storable D where
    poke2 = poke2 -- undefined

class Foo a where
instance Foo D where

class (Foo t, Storable t) => FooStorable t where

instance FooStorable D where
    {-# SPECIALIZE instance FooStorable D #-}

{-# SPECIALIZE bug :: D -> E #-}

bug
  :: FooStorable t
  => t
  -> E
bug = poke2
{-
sf 9160 # ghc -c -fforce-recomp -Wall B.hs

ghc: panic! (the 'impossible' happened)
  (GHC version 7.10.1 for x86_64-unknown-linux):
        Template variable unbound in rewrite rule
  $fFooStorableD_XU
  [$fFooStorableD_XU]
  [$fFooStorableD_XU]
  []
  []

Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
-}
