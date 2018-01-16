{-# LANGUAGE RankNTypes #-}
module Test where

foo :: () -> forall b. b
foo = undefined

works = id foo

fails = (id) foo

-- works type checks, but fails fails with the following error
-- message:
--
--   Cannot match a monotype with `() -> forall b. b'
--   Probable cause: `foo' is applied to too few arguments
--   In the first argument of `(id)', namely `foo'
--   In the expression: (id) foo
