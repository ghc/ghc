module T14373c where

data BigFam = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P

{-# NOINLINE mixedSwitch #-}
mixedSwitch A = True
mixedSwitch B = False
mixedSwitch C = False
mixedSwitch P = True
