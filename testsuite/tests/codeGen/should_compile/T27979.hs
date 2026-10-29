module T27979 (poly, elemAge) where

data Box a = Box Int [a]

newtype Age = Age Int

mkBox :: Int -> Box a
mkBox n = Box n []
{-# NOINLINE mkBox #-}

cafP :: Box a
cafP = mkBox 1

poly :: Int -> Int
poly i = case (cafP :: Box ()) of Box n _ -> n + i

elemAge :: [Age] -> Int
elemAge (Age n : _) = n
elemAge [] = 0
