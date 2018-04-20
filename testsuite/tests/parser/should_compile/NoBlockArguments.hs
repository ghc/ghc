module NoBlockArguments where

-- Make sure things parse normally
f  :: a -> a
f = id

foo :: [Int]
foo = f [x | x <- [1 .. 10]]
