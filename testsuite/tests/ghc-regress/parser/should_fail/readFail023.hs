module ShouldFail where

-- !!! Section precedences

-- check that we're not translating out negative literals too early:
-- the following should be an illegal section because prefix '-' has
-- precedence 6:

k = (-3 **)
  where
	(**) = const
	infixl 7 **
