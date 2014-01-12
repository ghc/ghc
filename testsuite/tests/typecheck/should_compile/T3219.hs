-- Trac #3219.  Lint error in GHC 6.10

module T3219 where

data T a = A{ m1 :: a } | B{ m1, m2 :: a } | C{ m2 :: a }

-- bar :: (a -> a) -> T a -> T a
bar f x@(A m) = x{m1 = f m}

-- foo :: (a -> a) -> T a -> T a
foo f x@(C m) = x{m2 = f m}
