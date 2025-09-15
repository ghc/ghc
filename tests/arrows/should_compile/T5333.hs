{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module T5333 where

import Prelude hiding ( id, (.) )
import Control.Arrow

cc1 :: a e b -> a e b -> a e b
cc1 = undefined

-- With GHC < 7.10.1, the following compile failures occurred:
--
-- ghc: panic! (the 'impossible' happened)
--  (GHC version 7.8.4 for x86_64-unknown-linux):
--  mkCmdEnv Not found: base:GHC.Desugar.>>>{v 02V}

-- 'g' fails to compile.
g = proc (x, y, z) ->
   ((returnA -< x) &&& (returnA -< y) &&& (returnA -< z))

-- 'f' compiles:
--   - without an infix declaration
--   - with the infixl declaration
-- and fails with the infixr declaration
infixr 6 `cc1`
-- infixl 6 `cc1`

f = proc (x, y, z) ->
  ((returnA -< x) `cc1` (returnA -< y) `cc1` (returnA -< z))
