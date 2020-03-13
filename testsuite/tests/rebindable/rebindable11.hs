{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE StaticPointers #-}
module Rebindable11 where

import Prelude

ifThenElse :: Bool -> () -> () -> Int
ifThenElse cond b1 b2 = 0

a1 = let foo = if 'a' then () else () in foo*foo
a2 = (if 'a' then () else ())*2 + 1
a3 = if 'a' then () else ()
a4 = if (if 'a' then () else ()) == 10 then () else ()
a5 = static (if 'a' then () else ())
a6 = (if 'a' then () else ()) :: Int

data A = A { field :: Int }
a7 = A { field = if 'a' then () else () }
a8 = let someA = A 10 in someA { field = if True == 'a' then () else () }
