{-# LANGUAGE TypeFamilies #-}
-- Important: no AllowAmbiguousTypes

module CEqCanOccursCheck where

type family F a where
  F Bool = Bool
type family G a b where
  G a a = a

{-
[W] F alpha ~ alpha
[W] F alpha ~ beta
[W] G alpha beta ~ Int
-}

foo :: (F a ~ a, F a ~ b) => G a b -> ()
foo _ = ()

bar :: ()
bar = foo True

{-
[G] F a ~ a
[W] F alpha ~ alpha
[W] F alpha ~ F a
-}

notAmbig :: F a ~ a => F a
notAmbig = undefined
