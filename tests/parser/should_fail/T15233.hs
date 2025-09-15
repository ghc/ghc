module T15233 where

-- ghc-8.6 would accept (but silently ignore) both of the following:
infixl 7 :
{-# DEPRECATED (:) "Deprecting cons" #-}

-- this was never accepted by ghc-8.6, but now that GHC.Prim emits a fixity
-- declaration for `(->)`, we need to make sure it is disallowed elsewhere.
infixr 4 ->
