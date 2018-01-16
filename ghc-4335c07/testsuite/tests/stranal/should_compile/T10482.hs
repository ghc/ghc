{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module T10482 where

data family Foo a
data instance Foo (a, b) = FooPair !(Foo a) !(Foo b)
newtype instance Foo Int = Foo Int

foo :: Foo ((Int, Int), Int) -> Int -> Int
foo !f k =
  if k == 0 then 0
  else if even k then foo f (k-1)
  else case f of
    FooPair (FooPair (Foo n) _) _ -> n
