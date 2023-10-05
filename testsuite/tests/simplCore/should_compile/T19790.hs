module T19790 where

newtype Stream = MkStream { unStream :: Int -> Int }

fromStream :: Stream -> [Int]
{-# NOINLINE fromStream #-}
fromStream (MkStream f) = map f [1,2]

toStream :: [Int] -> Stream
{-# NOINLINE toStream #-}
toStream xs = MkStream (\x -> x + length xs)


foo :: [Int] -> [Int]
foo xs = fromStream (MkStream (\p -> unStream (toStream xs) p))

-- The question is: does this rule fire?  It should!
{-# RULES "This rule should fire!" forall xs. fromStream (toStream xs) = xs #-}

