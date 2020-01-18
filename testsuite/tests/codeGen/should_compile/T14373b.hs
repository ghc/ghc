module T14373b where

data BigFam = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P

{-# NOINLINE earlySwitch #-}
earlySwitch A = True
earlySwitch B = False
earlySwitch C = False
