{-# LANGUAGE GADTs, LinearTypes, ScopedTypeVariables, EmptyCase #-}
module LinearBottomMult where

-- Check that _|_ * Many is not a subusage of One
--
data Void
data U a where U :: a -> U a

elim :: U a #-> ()
elim (U _) = ()

f :: a #-> ()
f x = elim (U (\(a :: Void) -> case a of {}))
