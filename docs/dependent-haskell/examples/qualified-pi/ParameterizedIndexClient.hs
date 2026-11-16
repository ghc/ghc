{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Observe imported parameterized indices from mutable inputs and lazy fields.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.IORef
import PackedVecLibrary
import ParameterizedIndexLibrary

-- | Check and print one finite observation.
observe :: (Eq a, Show a) => String -> a -> a -> IO ()
observe label expected actual = do
    unless (actual == expected) (error (label ++ ": unexpected result " ++ show actual))
    putStrLn (label ++ ": " ++ show actual)

-- | Compute a runtime length without a static index equation.
countList :: [Bool] -> Nat
countList [] = Z
countList (_ : rest) = S (countList rest)

-- | Return a constructor without evaluating the list argument.
constantList :: [Bool] -> Nat
constantList ignored = S (S Z)

-- | Exercise retained values obtained from IO, constructor operands and packages.
main :: IO ()
main = do
    ref <- newIORef [True, False, True]
    bs <- readIORef ref
    observe "runtime list vector" [1, 0, 1] (toList (listVector bs))
    writeIORef ref [False]
    changed <- readIORef ref
    observe "mutated list vector" [0] (toList (listVector changed))
    observe "empty list vector" [] (toList (listVector []))
    observe "literal list vector" [1, 0] (toList (listVector [True, False]))
    observe "packaged list" (3, [1, 0, 1]) (openListVector (packListVector bs))
    _ <- evaluate (packListVector (error "ignored package list"))
    observe "package ignores bottom list" () ()
    let lazyHead = [error "ignored Boolean head"] :: [Bool]
    observe "list head remains lazy" 1 (length (toList (listVector lazyHead)))
    let lazyTail = True : error "ignored list tail"
    observe "list tail remains lazy" 1 (head (toList (listVector lazyTail)))
    observe "optional Nothing ignores payload" 0 (optionalSize Nothing (error "ignored Nothing payload"))
    natRef <- newIORef (S (S Z))
    n <- readIORef natRef
    observe "optional runtime Just" 2 (optionalRoundtrip n)
    treeRef <- newIORef (Node (Leaf True) (Leaf False))
    tree <- readIORef treeRef
    observe "tree payload" "(11,False)" (renderTree tree (treePayload tree))
    let lazyTree = Node (Leaf True) (error "ignored right subtree")
    _ <- evaluate (treePayload lazyTree)
    observe "tree tuple keeps subtrees lazy" () ()
    observe "constant call ignores bottom list" [7, 7] (observeListCall constantList (error "ignored call list"))
    observe "call uses runtime list" [7, 7, 7] (observeListCall countList bs)
