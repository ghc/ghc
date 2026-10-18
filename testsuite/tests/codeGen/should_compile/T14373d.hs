module T14373d where

data BigFam = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P

-- In all cases the default block must be jumped to, not duplicated: each
-- function returns its default closure from exactly one place.  On 64-bit
-- targets the pointer tag distinguishes the first seven constructors, on
-- 32-bit targets only the first three.

-- CHECK-LABEL: T14373d.lateDefault_entry()
-- CHECK:       switch [0 .. 15]
-- CHECK-NEXT:  case 15 : goto
-- CHECK-NEXT:  default: {goto
-- CHECK:       R1 = {{.*}}_closure+2;
-- CHECK-NOT:   _closure+2;
{-# NOINLINE lateDefault #-}
lateDefault P = "Cool"
lateDefault _ = 'L' : "ate"

-- CHECK-LABEL: T14373d.earlyDefault_entry()
-- WS64:        switch [1 .. 7]
-- WS32:        switch [1 .. 3]
-- CHECK-NEXT:  case 2 : goto
-- CHECK-NEXT:  default: {goto
-- CHECK:       R1 = {{.*}}_closure+2;
-- CHECK-NOT:   _closure+2;
{-# NOINLINE earlyDefault #-}
earlyDefault B = "Cool"
earlyDefault _ = 'E' : "arly"

-- CHECK-LABEL: T14373d.mixedDefault_entry()
-- WS64:        switch [1 .. 7]
-- WS32:        switch [1 .. 3]
-- CHECK-NEXT:  case 2 : goto
-- WS64-NEXT:   case 7 : goto
-- WS32-NEXT:   case 3 : goto
-- CHECK-NEXT:  default: {goto
-- WS64:        switch [6 .. 15]
-- WS32:        switch [2 .. 15]
-- CHECK-NEXT:  case 15 : goto
-- CHECK-NEXT:  default: {goto
-- CHECK:       R1 = {{.*}}_closure+2;
-- CHECK-NOT:   _closure+2;
{-# NOINLINE mixedDefault #-}
mixedDefault B = "Cool"
mixedDefault P = "Cool"
mixedDefault _ = 'M' : "ixed"
