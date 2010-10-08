{-# LANGUAGE ImplicitParams #-}

-- Implicit parameters should not give rise to ambiguity.

module Main (main) where

foo :: (?x :: [a]) => Int -> String
foo n = show (n + length ?x)


main = do { putStrLn (let ?x = [True,False] in foo 3) ;
	    putStrLn (let ?x = "fred"       in foo 4) }

