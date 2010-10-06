{-# LANGUAGE UnicodeSyntax #-}
{-
	Three kinds of Unicode tests for our purposes.
-}

module UniTest where

-- Non working Japanese Unicode test.

てすと3 ∷ IO ()
てすと3 = do
	putStrLn $ show 人間虫 where
	人間虫 = "humasect"
