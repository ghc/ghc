{-# LANGUAGE MagicHash #-}

module Foo where

import GHC.Exts

g :: Int# -> Int#
{-# NOINLINE g #-}
g x = x +# 1#

-- Setting up this test case is quite delicate.
--
-- With the code below, simplification terminates too early.
--
-- Removing either of the (g (x +# 1#)) cases makes successively
-- merge 2 layers at a time, so it takes multiple iterations to
-- get a fixpoint.

f :: Int# -> Int#
f x = case g x of {
        1# -> 2# ; _ ->

      case g (x +# 1#) of { z ->

      case g x of {
        2# -> z ; _ ->

      case g (x +# 2#) of { z1 ->

      case g x of {
        3# -> 4#;  _ -> case g x of {
        4# -> z;   _ -> case g x of {
        5# -> 6#;  _ -> case g x of {
        6# -> z;   _ -> case g x of {
        7# -> 8#;  _ -> case g x of {
        8# -> 9#;  _ -> case g x of {
        9# -> 10#; _ -> 6#
        }}}}}}}}}}}
