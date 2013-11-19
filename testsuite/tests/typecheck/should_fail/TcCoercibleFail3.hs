{-# LANGUAGE RoleAnnotations, RankNTypes, ScopedTypeVariables #-}

import GHC.Prim (coerce, Coercible)

newtype List a = List [a]
data T f = T (f Int)

newtype NT1 a = NT1 (a -> Int)
newtype NT2 a = NT2 (a -> Int)

foo :: T NT1 -> T NT2
foo = coerce

main = return ()
