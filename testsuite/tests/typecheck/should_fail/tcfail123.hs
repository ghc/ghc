{-# LANGUAGE MagicHash #-}

module ShouldFail where

-- The danger here is getting a type like
--	f :: forall (a::??). a -> Bool
-- and hence allowing the bogus calls that follow

f x = True

h v = f 3#
-- h v = (f 3#, f 4.3#, f True)
