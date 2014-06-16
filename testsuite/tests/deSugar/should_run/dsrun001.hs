{-	Check that list comprehensions can be written
	in do-notation. This actually broke 2.02, with
	a pattern match failure in dsListComp!
-}

module Main where

main = putStrLn (show theList)
theList = do x <- [1..3]
             y <- [1..3]
             return (x,y)

