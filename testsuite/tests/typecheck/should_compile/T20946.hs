{-# LANGUAGE FlexibleInstances #-}

module T20946 where


class C123 a where c123 :: a

instance                  C123 [[Int]]  where c123 = [[1]]
instance {-# OVERLAPS #-} C123 [[a]]    where c123 = [[]]
instance                  C123 [a]      where c123 = []

test123 :: [[Int]]
test123 = c123


class C132 a where c132 :: a

instance                  C132 [[Int]]  where c132 = [[1]]
instance                  C132 [a]      where c132 = []
instance {-# OVERLAPS #-} C132 [[a]]    where c132 = [[]]

test132 :: [[Int]]
test132 = c132


class C213 a where c213 :: a

instance {-# OVERLAPS #-} C213 [[a]]    where c213 = [[]]
instance                  C213 [[Int]]  where c213 = [[1]]
instance                  C213 [a]      where c213 = []

test213 :: [[Int]]
test213 = c213


class C231 a where c231 :: a

instance {-# OVERLAPS #-} C231 [[a]]    where c231 = [[]]
instance                  C231 [a]      where c231 = []
instance                  C231 [[Int]]  where c231 = [[1]]

test231 :: [[Int]]
test231 = c231


class C312 a where c312 :: a

instance                  C312 [a]      where c312 = []
instance                  C312 [[Int]]  where c312 = [[1]]
instance {-# OVERLAPS #-} C312 [[a]]    where c312 = [[]]

test312 :: [[Int]]
test312 = c312


class C321 a where c321 :: a

instance                  C321 [a]      where c321 = []
instance {-# OVERLAPS #-} C321 [[a]]    where c321 = [[]]
instance                  C321 [[Int]]  where c321 = [[1]]

test321 :: [[Int]]
test321 = c321
