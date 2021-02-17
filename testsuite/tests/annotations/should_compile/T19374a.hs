{-# LANGUAGE TypeOperators #-}
module T19374 where

(%%) :: [a] -> [a] -> [a]
(%%) = (++)
{-# ANN (%%) "This is an annotation" #-}

data (%%%)
{-# ANN type (%%%) "This is also an annotation" #-}
