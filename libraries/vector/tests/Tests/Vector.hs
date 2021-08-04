{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector (tests) where

import Test.Tasty (testGroup)
import qualified Tests.Vector.Boxed
import qualified Tests.Vector.Primitive
import qualified Tests.Vector.Storable
import qualified Tests.Vector.Unboxed

tests =
  [ testGroup "Tests.Vector.Boxed" Tests.Vector.Boxed.tests
  , testGroup "Tests.Vector.Primitive" Tests.Vector.Primitive.tests
  , testGroup "Tests.Vector.Storable" Tests.Vector.Storable.tests
  , testGroup "Tests.Vector.Unboxed" Tests.Vector.Unboxed.tests
  ]
