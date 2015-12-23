{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, NoImplicitPrelude #-}

-- Copyright (c) 2008, Ralf Hinze
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
--     * Redistributions of source code must retain the above
--       copyright notice, this list of conditions and the following
--       disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials
--       provided with the distribution.
--
--     * The names of the contributors may not be used to endorse or
--       promote products derived from this software without specific
--       prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
-- COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
-- OF THE POSSIBILITY OF SUCH DAMAGE.

-- | A /priority search queue/ (henceforth /queue/) efficiently
-- supports the operations of both a search tree and a priority queue.
-- An 'Elem'ent is a product of a key, a priority, and a
-- value. Elements can be inserted, deleted, modified and queried in
-- logarithmic time, and the element with the least priority can be
-- retrieved in constant time.  A queue can be built from a list of
-- elements, sorted by keys, in linear time.
--
-- This implementation is due to Ralf Hinze with some modifications by
-- Scott Dillard and Johan Tibell.
--
-- * Hinze, R., /A Simple Implementation Technique for Priority Search
-- Queues/, ICFP 2001, pp. 110-121
--
-- <http://citeseer.ist.psu.edu/hinze01simple.html>
module GHC.Event.PSQ
    (
    -- * Binding Type
    Elem(..)
    , Key
    , Prio

    -- * Priority Search Queue Type
    , PSQ

    -- * Query
    , size
    , null
    , lookup

    -- * Construction
    , empty
    , singleton

    -- * Insertion
    , insert

    -- * Delete/Update
    , delete
    , adjust

    -- * Conversion
    , toList
    , toAscList
    , toDescList
    , fromList

    -- * Min
    , findMin
    , deleteMin
    , minView
    , atMost
    ) where

import GHC.Base hiding (empty)
import GHC.Num (Num(..))
import GHC.Show (Show(showsPrec))
import GHC.Event.Unique (Unique)

-- | @E k p@ binds the key @k@ with the priority @p@.
data Elem a = E
    { key   :: {-# UNPACK #-} !Key
    , prio  :: {-# UNPACK #-} !Prio
    , value :: a
    } deriving (Eq, Show)

------------------------------------------------------------------------
-- | A mapping from keys @k@ to priorites @p@.

type Prio = Double
type Key = Unique

data PSQ a = Void
           | Winner {-# UNPACK #-} !(Elem a)
                    !(LTree a)
                    {-# UNPACK #-} !Key  -- max key
           deriving (Eq, Show)

-- | /O(1)/ The number of elements in a queue.
size :: PSQ a -> Int
size Void            = 0
size (Winner _ lt _) = 1 + size' lt

-- | /O(1)/ True if the queue is empty.
null :: PSQ a -> Bool
null Void           = True
null (Winner _ _ _) = False

-- | /O(log n)/ The priority and value of a given key, or Nothing if
-- the key is not bound.
lookup :: Key -> PSQ a -> Maybe (Prio, a)
lookup k q = case tourView q of
    Null -> Nothing
    Single (E k' p v)
        | k == k'   -> Just (p, v)
        | otherwise -> Nothing
    tl `Play` tr
        | k <= maxKey tl -> lookup k tl
        | otherwise      -> lookup k tr

------------------------------------------------------------------------
-- Construction

empty :: PSQ a
empty = Void

-- | /O(1)/ Build a queue with one element.
singleton :: Key -> Prio -> a -> PSQ a
singleton k p v = Winner (E k p v) Start k

------------------------------------------------------------------------
-- Insertion

-- | /O(log n)/ Insert a new key, priority and value in the queue.  If
-- the key is already present in the queue, the associated priority
-- and value are replaced with the supplied priority and value.
insert :: Key -> Prio -> a -> PSQ a -> PSQ a
insert k p v q = case q of
    Void -> singleton k p v
    Winner (E k' p' v') Start _ -> case compare k k' of
        LT -> singleton k  p  v  `play` singleton k' p' v'
        EQ -> singleton k  p  v
        GT -> singleton k' p' v' `play` singleton k  p  v
    Winner e (RLoser _ e' tl m tr) m'
        | k <= m    -> insert k p v (Winner e tl m) `play` (Winner e' tr m')
        | otherwise -> (Winner e tl m) `play` insert k p v (Winner e' tr m')
    Winner e (LLoser _ e' tl m tr) m'
        | k <= m    -> insert k p v (Winner e' tl m) `play` (Winner e tr m')
        | otherwise -> (Winner e' tl m) `play` insert k p v (Winner e tr m')

------------------------------------------------------------------------
-- Delete/Update

-- | /O(log n)/ Delete a key and its priority and value from the
-- queue.  When the key is not a member of the queue, the original
-- queue is returned.
delete :: Key -> PSQ a -> PSQ a
delete k q = case q of
    Void -> empty
    Winner (E k' p v) Start _
        | k == k'   -> empty
        | otherwise -> singleton k' p v
    Winner e (RLoser _ e' tl m tr) m'
        | k <= m    -> delete k (Winner e tl m) `play` (Winner e' tr m')
        | otherwise -> (Winner e tl m) `play` delete k (Winner e' tr m')
    Winner e (LLoser _ e' tl m tr) m'
        | k <= m    -> delete k (Winner e' tl m) `play` (Winner e tr m')
        | otherwise -> (Winner e' tl m) `play` delete k (Winner e tr m')

-- | /O(log n)/ Update a priority at a specific key with the result
-- of the provided function.  When the key is not a member of the
-- queue, the original queue is returned.
adjust :: (Prio -> Prio) -> Key -> PSQ a -> PSQ a
adjust f k q0 =  go q0
  where
    go q = case q of
        Void -> empty
        Winner (E k' p v) Start _
            | k == k'   -> singleton k' (f p) v
            | otherwise -> singleton k' p v
        Winner e (RLoser _ e' tl m tr) m'
            | k <= m    -> go (Winner e tl m) `unsafePlay` (Winner e' tr m')
            | otherwise -> (Winner e tl m) `unsafePlay` go (Winner e' tr m')
        Winner e (LLoser _ e' tl m tr) m'
            | k <= m    -> go (Winner e' tl m) `unsafePlay` (Winner e tr m')
            | otherwise -> (Winner e' tl m) `unsafePlay` go (Winner e tr m')
{-# INLINE adjust #-}

------------------------------------------------------------------------
-- Conversion

-- | /O(n*log n)/ Build a queue from a list of key/priority/value
-- tuples.  If the list contains more than one priority and value for
-- the same key, the last priority and value for the key is retained.
fromList :: [Elem a] -> PSQ a
fromList = foldr (\(E k p v) q -> insert k p v q) empty

-- | /O(n)/ Convert to a list of key/priority/value tuples.
toList :: PSQ a -> [Elem a]
toList = toAscList

-- | /O(n)/ Convert to an ascending list.
toAscList :: PSQ a -> [Elem a]
toAscList q  = seqToList (toAscLists q)

toAscLists :: PSQ a -> Sequ (Elem a)
toAscLists q = case tourView q of
    Null         -> emptySequ
    Single e     -> singleSequ e
    tl `Play` tr -> toAscLists tl <> toAscLists tr

-- | /O(n)/ Convert to a descending list.
toDescList :: PSQ a -> [ Elem a ]
toDescList q = seqToList (toDescLists q)

toDescLists :: PSQ a -> Sequ (Elem a)
toDescLists q = case tourView q of
    Null         -> emptySequ
    Single e     -> singleSequ e
    tl `Play` tr -> toDescLists tr <> toDescLists tl

------------------------------------------------------------------------
-- Min

-- | /O(1)/ The element with the lowest priority.
findMin :: PSQ a -> Maybe (Elem a)
findMin Void           = Nothing
findMin (Winner e _ _) = Just e

-- | /O(log n)/ Delete the element with the lowest priority.  Returns
-- an empty queue if the queue is empty.
deleteMin :: PSQ a -> PSQ a
deleteMin Void           = Void
deleteMin (Winner _ t m) = secondBest t m

-- | /O(log n)/ Retrieve the binding with the least priority, and the
-- rest of the queue stripped of that binding.
minView :: PSQ a -> Maybe (Elem a, PSQ a)
minView Void           = Nothing
minView (Winner e t m) = Just (e, secondBest t m)

secondBest :: LTree a -> Key -> PSQ a
secondBest Start _                 = Void
secondBest (LLoser _ e tl m tr) m' = Winner e tl m `play` secondBest tr m'
secondBest (RLoser _ e tl m tr) m' = secondBest tl m `play` Winner e tr m'

-- | /O(r*(log n - log r))/ Return a list of elements ordered by
-- key whose priorities are at most @pt@.
atMost :: Prio -> PSQ a -> ([Elem a], PSQ a)
atMost pt q = let (sequ, q') = atMosts pt q
              in (seqToList sequ, q')

atMosts :: Prio -> PSQ a -> (Sequ (Elem a), PSQ a)
atMosts !pt q = case q of
    (Winner e _ _)
        | prio e > pt -> (emptySequ, q)
    Void              -> (emptySequ, Void)
    Winner e Start _  -> (singleSequ e, Void)
    Winner e (RLoser _ e' tl m tr) m' ->
        let (sequ, q')   = atMosts pt (Winner e tl m)
            (sequ', q'') = atMosts pt (Winner e' tr m')
        in (sequ <> sequ', q' `play` q'')
    Winner e (LLoser _ e' tl m tr) m' ->
        let (sequ, q')   = atMosts pt (Winner e' tl m)
            (sequ', q'') = atMosts pt (Winner e tr m')
        in (sequ <> sequ', q' `play` q'')

------------------------------------------------------------------------
-- Loser tree

type Size = Int

data LTree a = Start
             | LLoser {-# UNPACK #-} !Size
                      {-# UNPACK #-} !(Elem a)
                      !(LTree a)
                      {-# UNPACK #-} !Key  -- split key
                      !(LTree a)
             | RLoser {-# UNPACK #-} !Size
                      {-# UNPACK #-} !(Elem a)
                      !(LTree a)
                      {-# UNPACK #-} !Key  -- split key
                      !(LTree a)
             deriving (Eq, Show)

size' :: LTree a -> Size
size' Start              = 0
size' (LLoser s _ _ _ _) = s
size' (RLoser s _ _ _ _) = s

left, right :: LTree a -> LTree a

left Start                = moduleError "left" "empty loser tree"
left (LLoser _ _ tl _ _ ) = tl
left (RLoser _ _ tl _ _ ) = tl

right Start                = moduleError "right" "empty loser tree"
right (LLoser _ _ _  _ tr) = tr
right (RLoser _ _ _  _ tr) = tr

maxKey :: PSQ a -> Key
maxKey Void           = moduleError "maxKey" "empty queue"
maxKey (Winner _ _ m) = m

lloser, rloser :: Key -> Prio -> a -> LTree a -> Key -> LTree a -> LTree a
lloser k p v tl m tr = LLoser (1 + size' tl + size' tr) (E k p v) tl m tr
rloser k p v tl m tr = RLoser (1 + size' tl + size' tr) (E k p v) tl m tr

------------------------------------------------------------------------
-- Balancing

-- | Balance factor
omega :: Int
omega = 4

lbalance, rbalance :: Key -> Prio -> a -> LTree a -> Key -> LTree a -> LTree a

lbalance k p v l m r
    | size' l + size' r < 2     = lloser        k p v l m r
    | size' r > omega * size' l = lbalanceLeft  k p v l m r
    | size' l > omega * size' r = lbalanceRight k p v l m r
    | otherwise                 = lloser        k p v l m r

rbalance k p v l m r
    | size' l + size' r < 2     = rloser        k p v l m r
    | size' r > omega * size' l = rbalanceLeft  k p v l m r
    | size' l > omega * size' r = rbalanceRight k p v l m r
    | otherwise                 = rloser        k p v l m r

lbalanceLeft :: Key -> Prio -> a -> LTree a -> Key -> LTree a -> LTree a
lbalanceLeft  k p v l m r
    | size' (left r) < size' (right r) = lsingleLeft  k p v l m r
    | otherwise                        = ldoubleLeft  k p v l m r

lbalanceRight :: Key -> Prio -> a -> LTree a -> Key -> LTree a -> LTree a
lbalanceRight k p v l m r
    | size' (left l) > size' (right l) = lsingleRight k p v l m r
    | otherwise                        = ldoubleRight k p v l m r

rbalanceLeft :: Key -> Prio -> a -> LTree a -> Key -> LTree a -> LTree a
rbalanceLeft  k p v l m r
    | size' (left r) < size' (right r) = rsingleLeft  k p v l m r
    | otherwise                        = rdoubleLeft  k p v l m r

rbalanceRight :: Key -> Prio -> a -> LTree a -> Key -> LTree a -> LTree a
rbalanceRight k p v l m r
    | size' (left l) > size' (right l) = rsingleRight k p v l m r
    | otherwise                        = rdoubleRight k p v l m r

lsingleLeft :: Key -> Prio -> a -> LTree a -> Key -> LTree a -> LTree a
lsingleLeft k1 p1 v1 t1 m1 (LLoser _ (E k2 p2 v2) t2 m2 t3)
    | p1 <= p2  = lloser k1 p1 v1 (rloser k2 p2 v2 t1 m1 t2) m2 t3
    | otherwise = lloser k2 p2 v2 (lloser k1 p1 v1 t1 m1 t2) m2 t3
lsingleLeft k1 p1 v1 t1 m1 (RLoser _ (E k2 p2 v2) t2 m2 t3) =
    rloser k2 p2 v2 (lloser k1 p1 v1 t1 m1 t2) m2 t3
lsingleLeft _ _ _ _ _ _ = moduleError "lsingleLeft" "malformed tree"

rsingleLeft :: Key -> Prio -> a -> LTree a -> Key -> LTree a -> LTree a
rsingleLeft k1 p1 v1 t1 m1 (LLoser _ (E k2 p2 v2) t2 m2 t3) =
    rloser k1 p1 v1 (rloser k2 p2 v2 t1 m1 t2) m2 t3
rsingleLeft k1 p1 v1 t1 m1 (RLoser _ (E k2 p2 v2) t2 m2 t3) =
    rloser k2 p2 v2 (rloser k1 p1 v1 t1 m1 t2) m2 t3
rsingleLeft _ _ _ _ _ _ = moduleError "rsingleLeft" "malformed tree"

lsingleRight :: Key -> Prio -> a -> LTree a -> Key -> LTree a -> LTree a
lsingleRight k1 p1 v1 (LLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    lloser k2 p2 v2 t1 m1 (lloser k1 p1 v1 t2 m2 t3)
lsingleRight k1 p1 v1 (RLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    lloser k1 p1 v1 t1 m1 (lloser k2 p2 v2 t2 m2 t3)
lsingleRight _ _ _ _ _ _ = moduleError "lsingleRight" "malformed tree"

rsingleRight :: Key -> Prio -> a -> LTree a -> Key -> LTree a -> LTree a
rsingleRight k1 p1 v1 (LLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    lloser k2 p2 v2 t1 m1 (rloser k1 p1 v1 t2 m2 t3)
rsingleRight k1 p1 v1 (RLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3
    | p1 <= p2  = rloser k1 p1 v1 t1 m1 (lloser k2 p2 v2 t2 m2 t3)
    | otherwise = rloser k2 p2 v2 t1 m1 (rloser k1 p1 v1 t2 m2 t3)
rsingleRight _ _ _ _ _ _ = moduleError "rsingleRight" "malformed tree"

ldoubleLeft :: Key -> Prio -> a -> LTree a -> Key -> LTree a -> LTree a
ldoubleLeft k1 p1 v1 t1 m1 (LLoser _ (E k2 p2 v2) t2 m2 t3) =
    lsingleLeft k1 p1 v1 t1 m1 (lsingleRight k2 p2 v2 t2 m2 t3)
ldoubleLeft k1 p1 v1 t1 m1 (RLoser _ (E k2 p2 v2) t2 m2 t3) =
    lsingleLeft k1 p1 v1 t1 m1 (rsingleRight k2 p2 v2 t2 m2 t3)
ldoubleLeft _ _ _ _ _ _ = moduleError "ldoubleLeft" "malformed tree"

ldoubleRight :: Key -> Prio -> a -> LTree a -> Key -> LTree a -> LTree a
ldoubleRight k1 p1 v1 (LLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    lsingleRight k1 p1 v1 (lsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
ldoubleRight k1 p1 v1 (RLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    lsingleRight k1 p1 v1 (rsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
ldoubleRight _ _ _ _ _ _ = moduleError "ldoubleRight" "malformed tree"

rdoubleLeft :: Key -> Prio -> a -> LTree a -> Key -> LTree a -> LTree a
rdoubleLeft k1 p1 v1 t1 m1 (LLoser _ (E k2 p2 v2) t2 m2 t3) =
    rsingleLeft k1 p1 v1 t1 m1 (lsingleRight k2 p2 v2 t2 m2 t3)
rdoubleLeft k1 p1 v1 t1 m1 (RLoser _ (E k2 p2 v2) t2 m2 t3) =
    rsingleLeft k1 p1 v1 t1 m1 (rsingleRight k2 p2 v2 t2 m2 t3)
rdoubleLeft _ _ _ _ _ _ = moduleError "rdoubleLeft" "malformed tree"

rdoubleRight :: Key -> Prio -> a -> LTree a -> Key -> LTree a -> LTree a
rdoubleRight k1 p1 v1 (LLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    rsingleRight k1 p1 v1 (lsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
rdoubleRight k1 p1 v1 (RLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    rsingleRight k1 p1 v1 (rsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
rdoubleRight _ _ _ _ _ _ = moduleError "rdoubleRight" "malformed tree"

-- | Take two pennants and returns a new pennant that is the union of
-- the two with the precondition that the keys in the ﬁrst tree are
-- strictly smaller than the keys in the second tree.
play :: PSQ a -> PSQ a -> PSQ a
Void `play` t' = t'
t `play` Void  = t
Winner e@(E k p v) t m `play` Winner e'@(E k' p' v') t' m'
    | p <= p'   = Winner e (rbalance k' p' v' t m t') m'
    | otherwise = Winner e' (lbalance k p v t m t') m'
{-# INLINE play #-}

-- | A version of 'play' that can be used if the shape of the tree has
-- not changed or if the tree is known to be balanced.
unsafePlay :: PSQ a -> PSQ a -> PSQ a
Void `unsafePlay` t' =  t'
t `unsafePlay` Void  =  t
Winner e@(E k p v) t m `unsafePlay` Winner e'@(E k' p' v') t' m'
    | p <= p'   = Winner e (rloser k' p' v' t m t') m'
    | otherwise = Winner e' (lloser k p v t m t') m'
{-# INLINE unsafePlay #-}

data TourView a = Null
                | Single {-# UNPACK #-} !(Elem a)
                | (PSQ a) `Play` (PSQ a)

tourView :: PSQ a -> TourView a
tourView Void               = Null
tourView (Winner e Start _) = Single e
tourView (Winner e (RLoser _ e' tl m tr) m') =
    Winner e tl m `Play` Winner e' tr m'
tourView (Winner e (LLoser _ e' tl m tr) m') =
    Winner e' tl m `Play` Winner e tr m'

------------------------------------------------------------------------
-- Utility functions

moduleError :: String -> String -> a
moduleError fun msg = errorWithoutStackTrace ("GHC.Event.PSQ." ++ fun ++ ':' : ' ' : msg)
{-# NOINLINE moduleError #-}

------------------------------------------------------------------------
-- Hughes's efficient sequence type

newtype Sequ a = Sequ ([a] -> [a])

emptySequ :: Sequ a
emptySequ = Sequ (\as -> as)

singleSequ :: a -> Sequ a
singleSequ a = Sequ (\as -> a : as)

(<>) :: Sequ a -> Sequ a -> Sequ a
Sequ x1 <> Sequ x2 = Sequ (\as -> x1 (x2 as))
infixr 5 <>

seqToList :: Sequ a -> [a]
seqToList (Sequ x) = x []

instance Show a => Show (Sequ a) where
    showsPrec d a = showsPrec d (seqToList a)

