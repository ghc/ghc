{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}
{-# LANGUAGE EmptyCase, LambdaCase          #-}

-- Check some predefined types
module EmptyCase001 where

-- Non-exhaustive with *infinite* inhabitants
f1 :: Int -> a
f1 = \case

-- Non-exhaustive. Since a string is just a list of characters
-- (that is, an algebraic type), we have [] and (_:_) as missing.
f2 :: String -> a
f2 x = case x of {}

-- Non-exhaustive (do not unfold the alternatives)
f3 :: Char -> a
f3 x = case x of {}
