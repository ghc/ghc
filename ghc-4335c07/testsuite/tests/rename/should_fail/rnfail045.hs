-- These crashed GHC 6.4.2

module ShouldFail where

x `op1` y = True
op1 x = False

op2 x = False
x `op2` y = True
