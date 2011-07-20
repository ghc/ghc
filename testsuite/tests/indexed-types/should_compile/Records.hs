{-# LANGUAGE TypeFamilies #-}

-- See Trac #1204

module ShouldCompile where

data FooC = FooC

data family T c
data instance T FooC = MkT { moo :: Int }

t1 :: Int -> T FooC
t1 i = MkT { moo = i }

t2 :: T FooC -> Int
t2 (MkT { moo = i }) = i

t3 :: T FooC -> Int
t3 m = moo m

f :: T FooC -> T FooC
f r = r { moo = 3 }


------------------------------------------------------------------------------
class D c where
  data D1 c
  works :: Int -> D1 c -> D1 c
  buggy :: Int -> D1 c -> D1 c 
  buggy2 :: Int -> D1 c -> D1 c

instance D FooC where
  data D1 FooC = D1F { noo :: Int }

  works x d = d  -- d unchanged, so OK

  buggy x d@(D1F { noo = k }) = 
    d { noo = k + x }

  buggy2 x d@(D1F { noo = k }) =
    (d :: D1 FooC) { noo = k + x }
