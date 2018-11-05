{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module T15558 where

data T a where
  MkT :: T Int

data Foo a = MkFoo ((a ~ Bool) => ())

f :: T a -> Foo a
f MkT = MkFoo ()

-- g :: Foo Int
-- g = f MkT
