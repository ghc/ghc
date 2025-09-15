{-# OPTIONS_GHC -Wname-shadowing #-}
{-# LANGUAGE Haskell2010 #-} -- Necessary to avoid `NamedFieldPuns` from `GHC2021`.
{-# LANGUAGE NoFieldSelectors #-}
module M where
data T = C { x :: () }
f x = x
