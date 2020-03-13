{-# LANGUAGE RebindableSyntax #-}
module If where

import Prelude (length, Bool(..), otherwise, Int, String, Num(..), Eq(..))

ifThenElse :: Bool -> () -> () -> Int
ifThenElse cond b1 b2 = 0

a1 = let foo = if 'a' then () else () in foo*foo
a2 = (if 'a' then () else ())*2 + 1
a3 = if 'a' then () else ()
a4 = if (if 'a' then () else ()) == 10 then () else ()
