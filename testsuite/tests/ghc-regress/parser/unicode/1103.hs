{-# OPTIONS -fglasgow-exts #-}

{-
	Three kinds of Unicode tests for our purposes.

	Note that GHC_OPTIONS instead of OPTIONS above does not work.
-}

module UniTest where

-- Non working Japanese Unicode test.

てすと3 ∷ IO ()
てすと3 = do
	putStrLn $ show 人間虫 where
	人間虫 = "humasect"
