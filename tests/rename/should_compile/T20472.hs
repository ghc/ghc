{-# OPTIONS_GHC -fdefer-out-of-scope-variables #-}

module T20472 where

a = nonexistent
b = Prelude.nonexistent
c = True
d = Nonexistent.x
