{-# LANGUAGE ScopedTypeVariables #-}

module ShouldFail where

f3 :: forall a. [a] -> [a] 
Just f3 = Just (\(x:xs) -> xs ++ [ x :: a ])   -- Not OK!
	-- The type variable does not scope in a pattern binding
