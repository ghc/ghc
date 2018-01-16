-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.State.Strict
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Strict state monads.
--
--      This module is inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.

-----------------------------------------------------------------------------

module Control.Monad.State.Strict (
    -- * MonadState class
    MonadState(..),
    modify,
    modify',
    gets,
    -- * The State monad
    State,
    runState,
    evalState,
    execState,
    mapState,
    withState,
    -- * The StateT monad transformer
    StateT(StateT),
    runStateT,
    evalStateT,
    execStateT,
    mapStateT,
    withStateT,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    -- * Examples
    -- $examples
  ) where

import Control.Monad.State.Class

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
        (State, runState, evalState, execState, mapState, withState,
         StateT(StateT), runStateT, evalStateT, execStateT, mapStateT, withStateT)

import Control.Monad
import Control.Monad.Fix

-- ---------------------------------------------------------------------------
-- $examples
-- A function to increment a counter.  Taken from the paper
-- /Generalising Monads to Arrows/, John
-- Hughes (<http://www.math.chalmers.se/~rjmh/>), November 1998:
--
-- > tick :: State Int Int
-- > tick = do n <- get
-- >           put (n+1)
-- >           return n
--
-- Add one to the given number using the state monad:
--
-- > plusOne :: Int -> Int
-- > plusOne n = execState tick n
--
-- A contrived addition example. Works only with positive numbers:
--
-- > plus :: Int -> Int -> Int
-- > plus n x = execState (sequence $ replicate n tick) x
--
-- An example from /The Craft of Functional Programming/, Simon
-- Thompson (<http://www.cs.kent.ac.uk/people/staff/sjt/>),
-- Addison-Wesley 1999: \"Given an arbitrary tree, transform it to a
-- tree of integers in which the original elements are replaced by
-- natural numbers, starting from 0.  The same element has to be
-- replaced by the same number at every occurrence, and when we meet
-- an as-yet-unvisited element we have to find a \'new\' number to match
-- it with:\"
--
-- > data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq)
-- > type Table a = [a]
--
-- > numberTree :: Eq a => Tree a -> State (Table a) (Tree Int)
-- > numberTree Nil = return Nil
-- > numberTree (Node x t1 t2)
-- >        =  do num <- numberNode x
-- >              nt1 <- numberTree t1
-- >              nt2 <- numberTree t2
-- >              return (Node num nt1 nt2)
-- >     where
-- >     numberNode :: Eq a => a -> State (Table a) Int
-- >     numberNode x
-- >        = do table <- get
-- >             (newTable, newPos) <- return (nNode x table)
-- >             put newTable
-- >             return newPos
-- >     nNode::  (Eq a) => a -> Table a -> (Table a, Int)
-- >     nNode x table
-- >        = case (findIndexInList (== x) table) of
-- >          Nothing -> (table ++ [x], length table)
-- >          Just i  -> (table, i)
-- >     findIndexInList :: (a -> Bool) -> [a] -> Maybe Int
-- >     findIndexInList = findIndexInListHelp 0
-- >     findIndexInListHelp _ _ [] = Nothing
-- >     findIndexInListHelp count f (h:t)
-- >        = if (f h)
-- >          then Just count
-- >          else findIndexInListHelp (count+1) f t
--
-- numTree applies numberTree with an initial state:
--
-- > numTree :: (Eq a) => Tree a -> Tree Int
-- > numTree t = evalState (numberTree t) []
--
-- > testTree = Node "Zero" (Node "One" (Node "Two" Nil Nil) (Node "One" (Node "Zero" Nil Nil) Nil)) Nil
-- > numTree testTree => Node 0 (Node 1 (Node 2 Nil Nil) (Node 1 (Node 0 Nil Nil) Nil)) Nil
--
-- sumTree is a little helper function that does not use the State monad:
--
-- > sumTree :: (Num a) => Tree a -> a
-- > sumTree Nil = 0
-- > sumTree (Node e t1 t2) = e + (sumTree t1) + (sumTree t2)
