{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -O #-}

module M where

import GHC.Exts

data E = A | B | C | D | E

foo x =
    case x of
        A -> 2#
        B -> 42#
        -- In this branch we already know `x` is evaluated, so dataToTag#
        -- must read the tag bits directly rather than enter `x` again:
        -- CHECK-LABEL: M.foo_entry()
        -- CHECK:       // dataToTagSmall#
        -- CHECK-NEXT:  R1 = R1 & 7 - 1;
        -- CHECK-NEXT:  Sp = Sp + 8;
        -- CHECK-NEXT:  call (P64[Sp])(R1) args:
        _ -> dataToTag# x
