{-# OPTIONS -fglasgow-exts #-}

-- From Yann Regis-Gianas at INRIA

module ShouldCompile where

data T a where
  K :: T Int

-- Should fail
i1 :: T  a -> a -> Int
i1 t y = (\t1 y1 -> case t1 of K -> y1) t y

-- No type signature; should type-check
i1b t y = (\t1 y1 -> case t1 of K -> y1) t y

i2 :: T a -> a -> Int
i2 t (y::b)  = case t of { K -> y+(1::Int) }

i3 :: forall a. T a -> a -> Int
i3 t y = let (t :: T b) = t in
  let (y :: b) = y in
   case t of K -> y

i4 :: forall a. T a -> a -> Int
i4 (t :: T c) (y :: c) = 
    let (t :: T b) = t in
    let (y :: b) = y in
  case t of K -> y




