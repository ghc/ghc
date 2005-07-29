-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Queue
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- NOTE: This module is DEPRECATED.
-- The data structure in "Data.Sequence" is a faster queue and also
-- supports a wider variety of operations.
--
-- Queues with constant time operations, from
-- /Simple and efficient purely functional queues and deques/,
-- by Chris Okasaki, /JFP/ 5(4):583-592, October 1995.
--
-----------------------------------------------------------------------------

module Data.Queue
{-# DEPRECATED "Use Data.Sequence instead: it's faster and has more operations" #-}
	(Queue,
	-- * Primitive operations
	-- | Each of these requires /O(1)/ time in the worst case.
	emptyQueue, addToQueue, deQueue,
	-- * Queues and lists
	listToQueue, queueToList
    ) where

import Prelude -- necessary to get dependencies right
import Data.Typeable

-- | The type of FIFO queues.
data Queue a = Q [a] [a] [a]

#include "Typeable.h"
INSTANCE_TYPEABLE1(Queue,queueTc,"Queue")

-- Invariants for Q xs ys xs':
--	length xs = length ys + length xs'
--	xs' = drop (length ys) xs	-- in fact, shared (except after fmap)
-- The queue then represents the list xs ++ reverse ys

instance Functor Queue where
	fmap f (Q xs ys xs') = Q (map f xs) (map f ys) (map f xs')
	-- The new xs' does not share the tail of the new xs, but it does
	-- share the tail of the old xs, so it still forces the rotations.
	-- Note that elements of xs' are ignored.

-- | The empty queue.
emptyQueue :: Queue a
emptyQueue = Q [] [] []

-- | Add an element to the back of a queue.
addToQueue :: Queue a -> a -> Queue a
addToQueue (Q xs ys xs') y = makeQ xs (y:ys) xs'

-- | Attempt to extract the front element from a queue.
-- If the queue is empty, 'Nothing',
-- otherwise the first element paired with the remainder of the queue.
deQueue :: Queue a -> Maybe (a, Queue a)
deQueue (Q [] _ _) = Nothing
deQueue (Q (x:xs) ys xs') = Just (x, makeQ xs ys xs')

-- Assuming
--	length ys <= length xs + 1
--	xs' = drop (length ys - 1) xs
-- construct a queue respecting the invariant.
makeQ :: [a] -> [a] -> [a] -> Queue a
makeQ xs ys [] = listToQueue (rotate xs ys [])
makeQ xs ys (_:xs') = Q xs ys xs'

-- Assuming length ys = length xs + 1,
--	rotate xs ys zs = xs ++ reverse ys ++ zs
rotate :: [a] -> [a] -> [a] -> [a]
rotate [] (y:_) zs = y : zs		-- the _ here must be []
rotate (x:xs) (y:ys) zs = x : rotate xs ys (y:zs)

-- | A queue with the same elements as the list.
listToQueue :: [a] -> Queue a
listToQueue xs = Q xs [] xs

-- | The elements of a queue, front first.
queueToList :: Queue a -> [a]
queueToList (Q xs ys _) = xs ++ reverse ys
