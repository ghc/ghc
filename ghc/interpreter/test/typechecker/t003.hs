--!!! Testing monad comprehensions
module MonadTest where

-- Old uses of list comprehensions
as :: [Bool]
as = [ odd x | x <- [1..10] ]

-- The next 4 tests used to check that list comprehension syntax
-- could be used for monad comprehensions.
-- Anticipating Standard Haskell's removal of this feature, we don't
-- test (or implement!) that anymore.

-- Use in monad comprehensions
mmap :: (a -> b) -> ([] a -> [] b)
mmap f xs = [ f x | x <- xs ]

-- use ","
bind1 :: [] a -> (a -> [] b) -> [] b
bind1 m k = [ b | a <- m, b <- k a ]

bind2 :: [] Int -> (Int -> [] b) -> [] b
bind2 m k = [ b | a <- m, odd a, b <- k a ]

-- use local binding
bind3 :: [] a -> (a -> b) -> (b -> [] c) -> [] c
bind3 m f k = [ c | a <- m, let b = f a, c <- k b ]


-- The next 4 tests check the use of "do-syntax" for monad comprehensions

-- Use in monad comprehensions
mmap2 :: Monad m => (a -> b) -> (m a -> m b)
mmap2 f xs = do { x <- xs; return (f x) }

-- use ","
bind12 :: Monad m => m a -> (a -> m b) -> m b
bind12 m k = do { a <- m; b <- k a; return b }

bind22 :: MonadZero m => m Int -> (Int -> m b) -> m b
bind22 m k = do { a <- m; guard (odd a); b <- k a; return b }

-- use local binding
bind32 :: Monad m => m a -> (a -> b) -> (b -> m c) -> m c
bind32 m f k = do { a <- m; let { b = f a }; c <- k b; return c }


