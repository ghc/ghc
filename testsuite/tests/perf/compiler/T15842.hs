{-# OPTIONS_GHC -O1 #-}
-- | Test rules for '^', '^^' and 'stimes'
--
-- These rules are located in 'PrelRules' and should rewrite parameters of type
-- 'Integer' to 'Word'.
--
-- Unfortunately this test depends on __all__ rule firings for this little
-- program. So, adding, removing or changing any of the used rules can easily
-- break this test.
--
-- Firings this test should really assert:
--   * @Rule fired: stimes_IntegerToWord (BUILTIN)@
--   * @Rule fired: Class op stimes (BUILTIN)@
--   * @Rule fired: numExponentiation_IntegerToWord (BUILTIN)@
--   * @Rule fired: fractionalExponentiation_IntegerToWord (BUILTIN)@

module T15842 where

import Data.Semigroup

callStimes :: String
callStimes = stimes 42 "foo"

callIntegralExponential :: Integer
callIntegralExponential = 10 ^ 42

callFractionalExponential :: Float
callFractionalExponential = 10 ^^ 42
