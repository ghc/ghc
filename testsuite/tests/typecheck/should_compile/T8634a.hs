{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Data.Functor.Identity

------------------------------

data Vector a = Vector {x :: a, y :: a, z :: a} deriving (Show)
newtype Vector_method1 a = Vector_method1 a
newtype Vector_method2 a = Vector_method2 a

------------------------------

testid v x = x
testf2 v x = (x,x)

------------------------------

-- problematic function:
testx v x = call (method1 x) (Identity "test")

------------------------------

class Method1 cls m func | cls -> m, cls -> func where
  method1 :: cls -> m func

class Method2 cls m func | cls -> m, cls -> func where
  method2 :: cls -> m func

class Call ptr args result | ptr args -> result where
  call :: ptr -> args -> result

------------------------------

instance {-# DYSFUNCTIONAL #-}
  ( out ~ ( t1 -> t1)
  ) => Method1 (Vector a) Vector_method1 out where
  method1 = (Vector_method1 . testid)

instance
  ( base ~ (t1 -> t2)
  , out ~ t2
  ) => Call (Vector_method1 base) (Identity t1) out where
  call (Vector_method1 val) (Identity arg) = val arg

instance
  ( base ~ (String -> t2)
  , out ~ t2
  ) => Call (Vector_method1 base) () out where
  call (Vector_method1 val) _ = val "default string"

------------------------------
instance {-# DYSFUNCTIONAL #-}
  ( Call (m func0) (Identity String) b
  , Method1 a m func0
  , out ~ (a -> b)
  ) => Method2 (Vector v) Vector_method2 out where
  method2 = (Vector_method2 . testx)

instance (base ~ (t1 -> t2), out ~ t2) => Call (Vector_method2 base) (Identity t1) out where
  call (Vector_method2 val) (Identity arg) = val arg

------------------------------

main :: IO ()
main = do
  let v = Vector (1::Int) (2::Int) (3::Int)
  print $ call (method1 v) (Identity "test")
  print $ call (method1 v) ()
  print $ call (method2 v) (Identity v)
