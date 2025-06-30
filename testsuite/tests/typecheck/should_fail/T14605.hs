{-# Language TypeApplications #-}
{-# Language ImpredicativeTypes #-}
-- This isn't a test for impredicative types; it's
-- just that visible type application on a for-all type
-- is an easy way to provoke the error.
--
-- The ticket #14605 has a much longer example that
-- also fails; it does not use ImpredicativeTypes
--
-- The error message is not great; but it's an obscure program

module T14605 where

import GHC.Exts (coerce)

duplicate = coerce @(forall x. ()) @(forall x. x)
