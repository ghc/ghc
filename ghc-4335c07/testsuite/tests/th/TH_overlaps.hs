{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module TH_overlaps where

import Language.Haskell.TH

class C1 a where c1 :: a
class C2 a where c2 :: a
class C3 a where c3 :: a

[d|
  instance {-# OVERLAPPABLE #-} C1 [a]      where c1 = []
  instance                      C1 [Int]    where c1 = [1]

  instance                      C2 [a]      where c2 = []
  instance {-# OVERLAPPING #-}  C2 [Int]    where c2 = [1]

  instance                      C3 [a]      where c3 = []
  instance {-# OVERLAPS #-}     C3 [[a]]    where c3 = [[]]
  instance                      C3 [[Int]]  where c3 = [[1]]
  |]

test1 :: ([Char],[Int])
test1 = (c1,c1)

test2 :: ([Char],[Int])
test2 = (c2,c2)

test3 :: ([Char],[[Char]],[[Int]])
test3 = (c3,c3,c3)
