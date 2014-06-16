module T7937 where

-- Without this operator definition, a precedence parsing error is reported.
-- Perhaps the default precedence is being assumed for the unknown operator?
-- That seems wrong, since there's no way to know what the precedence will be when
-- the operator is defined as the programmer intended.

foo = 3 > 4 *** 5 == 6


