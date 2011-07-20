module Bug where

compose :: [a -> a] -> a -> a
compose =  foldr (.) id

class Compose a where
    compose1 :: a -> a -> a
