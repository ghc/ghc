-- This program is an example of a use of UndecidableInstances and
-- QuantifiedConstraints which circumvents the usual compile-time solver
-- termination check and instead produces a program which loops at runtime.

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GADTs #-}
--
--
--
--
--
--
--

import System.IO
import Data.Void
import Data.Typeable
import Data.Type.Equality

class (forall k. k a => k b) => Equ a b

instance Equ a a

data Z' a where
  Z' :: Z' Void

data Z where
  Z :: forall a. Equ Void a => Z' a -> Z

checkZ :: Z -> Bool
checkZ (Z (Z' :: Z' a)) = case eqT of
  Nothing -> False
  Just (Refl :: a :~: Void) -> True

main :: IO ()
main = do
  hPutStrLn stderr "before..."
  hPutStrLn stderr $ show $ checkZ $ Z Z'
  hPutStrLn stderr "after!"
