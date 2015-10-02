{-# LANGUAGE DataKinds, TypeOperators, GADTs #-}

module T9260 where

import GHC.TypeLits

data Fin n where
  Fzero :: Fin (n + 1)
  Fsucc :: Fin n -> Fin (n + 1)

test :: Fin 1
test = Fsucc Fzero

{- Only the second error is legitimate.

% ghc --version
The Glorious Glasgow Haskell Compilation System, version 7.8.2
% ghc -ignore-dot-ghci /tmp/Error.hs
[1 of 1] Compiling Error            ( /tmp/Error.hs, /tmp/Error.o )

/tmp/Error.hs:12:8:
    Couldn't match type ‘0’ with ‘1’
    Expected type: Fin 1
      Actual type: Fin (0 + 1)
    In the expression: Fsucc Fzero
    In an equation for ‘test’: test = Fsucc Fzero

/tmp/Error.hs:12:14:
    Couldn't match type ‘1’ with ‘0’
    Expected type: Fin 0
      Actual type: Fin (0 + 1)
    In the first argument of ‘Fsucc’, namely ‘Fzero’
    In the expression: Fsucc Fzero
-}
