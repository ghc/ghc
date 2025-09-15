{-# LANGUAGE TypeFamilies, RecordWildCards #-}

module T18311 where

type family F a

type instance F Int = Int
type instance F Bool = Int

data T a = MkT {x :: F a, y :: a}


foo1 :: T Int -> T Bool
foo1 (MkT { x = x }) = MkT { x = x , y = True }

foo2 :: T Int -> T Bool
foo2 t = t {y = False }

