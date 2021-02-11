module T19346 where

data T = MkT Int

f :: Bool -> T
f x = MkT x

-- Produced a bad error message when compiled with
--   -fprint-typechecker-elaboration
