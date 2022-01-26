{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Haskell2010 #-}

module T20989 where

import Data.Proxy
  ( Proxy )
import GHC.Records
  ( HasField )

data Bar0 where
  Bar0 :: HasField s r a => Proxy s -> Proxy r -> Proxy a -> Bar0

-- See Note [Validity checking of HasField instances] in GHC.Tc.Validity

-- 1. `HasField _ r _` where r is a variable
data Bar1 where
  Bar1 :: (forall r. HasField s r Int) => Proxy s -> Bar1

-- 2. `HasField _ (T ...) _` if T is a data family
data family Foo2 a
data Bar2 where
  Bar2 :: (forall a. HasField s (Foo2 a) Int) => Proxy s -> Bar2

-- 3. `HasField x (T ...) _` where x is a variable,
--    if T has any fields at all
data Foo3 a = Foo3 { fld1 :: Int, fld2 :: Bool }
data Bar3 where
  Bar3 :: (forall a. HasField s (Foo3 a) Int) => Proxy s -> Bar3

-- 4. `HasField "foo" (T ...) _` if T has a "foo" field.
data Foo4 a = Foo4 { foo4 :: Int }
data Bar4 where
  Bar4 :: (forall a. HasField "foo4" (Foo4 a) Int) => Bar4
