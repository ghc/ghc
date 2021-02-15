module T19347b where

(%%) :: [a] -> [a] -> [a]
(%%) = (++)
{-# ANN (T19347b.%%) "This is an annotation" #-}
