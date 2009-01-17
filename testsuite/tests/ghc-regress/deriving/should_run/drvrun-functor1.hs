{-# LANGUAGE DeriveFunctor #-}

module Main where

-- Derive functor for a simple data type

data List a = Nil | Cons a (List a)
    deriving (Functor,Show)

someList   = Cons 1 (Cons 1 (Cons 2 (Cons 3 Nil)))
doubleList = fmap (*2) someList

test1 = do
    putStr "normal:  " >> print someList
    putStr "double:  " >> print doubleList

-- Derive functor for a data type with functions and tuples

data ReaderWriter r w a = RW { runRW :: r -> (a,w) }
    deriving (Functor)

data Cont r a = Cont { runCont :: (a -> r) -> r }
    deriving (Functor)

test2 = do
    let rw = RW (\r -> ("something",r*3))
    putStr "normal:  " >> print (runRW rw 123)
    putStr "reverse: " >> print (runRW (fmap reverse rw) 456)
    let five = Cont ($ 5)
    putStr "normal:  " >> runCont five print
    putStr "double:  " >> runCont (fmap (*2) five) print

-- Derive functor in such a way that we need a constraint

newtype Compose f g a = Compose (f (g a))
    deriving (Functor,Show)

listOfLists = Compose [[1,2,3],[7,8,9]]

test3 = do
    putStr "normal:  " >> print listOfLists
    putStr "double:  " >> print (fmap (*2) listOfLists)

-- All tests

main = do
    test1
    test2
    test3
