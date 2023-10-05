{-# LANGUAGE RankNTypes, ViewPatterns #-}

module ViewPats where

ex1 :: forall a. a -> a -> Int -> Eq a => Bool
-- Accept; we skolemise over three args
ex1 x ((== x) -> result) _ = result

{-
ex2 :: forall a. a -> a -> Int -> Eq a => Bool
-- Reject: only skolemise over two args
ex2 x ((== x) -> result) = \ _ -> result

ex3 :: forall a. a -> a -> Int -> Eq a => Bool
-- Reject: only skolemise over two args
--    const (result :: Bool) :: b -> Eq a => Bool
ex3 x ((== x) -> result) = const result
-}

ex4 :: forall a. a -> a -> Int -> Eq a => Bool
-- Accept
ex4 x y _ = x == y

ex5 :: forall a. a -> a -> Int -> Eq a => Bool
-- Accept
ex5 x y = \ _ -> x == y

{-
ex6 :: forall a. a -> a -> Int -> Eq a => Bool
-- Reject. Needs (const (bla :: Bool)) :: Int -> Eq a => Bool
ex6 x y = const (x == y)
-}

ex7 :: forall a. a -> a -> Eq a => Bool
-- Accept
ex7 x ((== x) -> result) = result

ex8 :: forall a. a -> a -> Eq a => Bool
-- Accept
ex8 x y = x == y

ex9 :: forall a. a -> Eq a => a -> Bool
-- Accept
ex9 x ((== x) -> result) = result

ex10 :: forall a. a -> Eq a => a -> Bool
-- Accept
ex10 x y = x == y

ex11 :: forall a. a -> Eq a => a -> Bool
-- Accept
ex11 x = (== x)
