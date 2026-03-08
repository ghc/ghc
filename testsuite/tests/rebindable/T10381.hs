{-# LANGUAGE RebindableSyntax, RankNTypes #-}

module T10381 where

import Prelude ( String, undefined )

newtype Cont r a = Cont { runCont :: (forall i. a i -> r) -> r }

(>>=) :: Cont r a -> (forall i. a i -> Cont r b) -> Cont r b
ma >>= fmb
  = Cont (\k -> runCont ma (\a -> runCont (fmb a) k))

fail :: String -> Cont r a
fail = undefined

return :: a i -> Cont r a
return x = Cont (\k -> k x)

foo :: Cont r []
foo = do
  bar <- foo
  return bar

{- Previously, GHC used to reject this program with:

    Couldn't match type ‘i0’ with ‘i’
      because type variable ‘i’ would escape its scope
    This (rigid, skolem) type variable is bound by
      a type expected by the context: [i] -> Cont r []
      at Bug.hs:21:3-12
    Expected type: Cont r [] -> ([i0] -> Cont r []) -> Cont r []
      Actual type: Cont r []
                   -> (forall i. [i] -> Cont r []) -> Cont r []
    In a stmt of a 'do' block: bar <- foo
    In the expression:
      do { bar <- foo;
           return bar }
    In an equation for ‘foo’:
        foo
          = do { bar <- foo;
                 return bar }
-}

