whnf :: a -> IO ()
whnf a = a `seq` (return ())

foo :: [Int]
foo = [1..]
