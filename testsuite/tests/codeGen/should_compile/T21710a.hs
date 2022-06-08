{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -O #-}

module M where

import GHC.Exts

data E = A | B | C | D | E

foo x =
    case x of
        A -> 2#
        B -> 42#
        -- In this branch we already now `x` is evaluated, so we shouldn't generate an extra `call` for it.
        _ -> dataToTag# x
