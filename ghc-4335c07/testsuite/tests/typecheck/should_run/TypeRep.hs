{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

import Data.Typeable
import Data.Kind
import GHC.Exts

-- Test that Typeable works for various wired-in types.
-- See, for instance, #11120.

rep :: forall a. Typeable a => TypeRep
rep = typeRep (Proxy :: Proxy a)

main :: IO ()
main = do
  -- the basics
  print $ rep @String
  print $ rep @Char
  print $ rep @Int
  print $ rep @Word
  print $ rep @Double
  print $ rep @(IO ())
  print $ rep @IO
  print $ rep @"hi"
  print $ rep @(Char, Int, String)
  print $ rep @Bool
  print $ rep @Ordering
  print $ rep @(Int -> Int)
  print $ rep @((Eq Int, Eq String) :: Constraint)

  -- Unboxed things (#12049)
  print $ rep @Int#
  print $ rep @(##)
  print $ rep @(# Int#, Int #)

  -- Various instantiations of a kind-polymorphic type
  print $ rep @(Proxy (Eq Int))
  print $ rep @(Proxy (Int, Int))
  print $ rep @(Proxy "hello world")
  print $ rep @(Proxy 1)
  print $ rep @(Proxy [1,2,3])
  print $ rep @(Proxy 'EQ)
  print $ rep @(Proxy TYPE)
  print $ rep @(Proxy (TYPE 'LiftedRep))
  print $ rep @(Proxy *)
  print $ rep @(Proxy ★)
  print $ rep @(Proxy 'LiftedRep)

  -- Something lifted and primitive
  print $ rep @RealWorld  -- #12132
