
-- |An efficient implementation of 'Data.Graph.Inductive.Graph.Graph'
-- using big-endian patricia tree (i.e. "Data.IntMap").
--
-- This module provides the following specialised functions to gain
-- more performance, using GHC's RULES pragma:
--
-- * 'Data.Graph.Inductive.Graph.insNode'
--
-- * 'Data.Graph.Inductive.Graph.insEdge'
--
-- * 'Data.Graph.Inductive.Graph.gmap'
--
-- * 'Data.Graph.Inductive.Graph.nmap'
--
-- * 'Data.Graph.Inductive.Graph.emap'
--
-- Code is from Hackage `fgl` package version 5.7.0.3


module GHC.Data.Graph.Inductive.PatriciaTree
    ( Gr
    , UGr
    )
    where

import GHC.Prelude

import GHC.Data.Graph.Inductive.Graph

import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IM
import           Data.List           (sort)
import           Data.Maybe          (fromMaybe)
import           Data.Tuple          (swap)

import qualified Data.IntMap.Strict as IMS

import GHC.Generics (Generic)

import Data.Bifunctor

----------------------------------------------------------------------
-- GRAPH REPRESENTATION
----------------------------------------------------------------------

newtype Gr a b = Gr (GraphRep a b)
  deriving (Generic)

type GraphRep a b = IntMap (Context' a b)
type Context' a b = (IntMap [b], a, IntMap [b])

type UGr = Gr () ()

----------------------------------------------------------------------
-- CLASS INSTANCES
----------------------------------------------------------------------

instance (Eq a, Ord b) => Eq (Gr a b) where
  (Gr g1) == (Gr g2) = fmap sortAdj g1 == fmap sortAdj g2
    where
      sortAdj (p,n,s) = (fmap sort p,n,fmap sort s)

instance (Show a, Show b) => Show (Gr a b) where
  showsPrec d g = showParen (d > 10) $
                    showString "mkGraph "
                    . shows (labNodes g)
                    . showString " "
                    . shows (labEdges g)

instance (Read a, Read b) => Read (Gr a b) where
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("mkGraph", s) <- lex r
    (ns,t) <- reads s
    (es,u) <- reads t
    return (mkGraph ns es, u)

instance Graph Gr where
    empty           = Gr IM.empty

    isEmpty (Gr g)  = IM.null g

    match           = matchGr

    mkGraph vs es   = insEdges es
                      . Gr
                      . IM.fromList
                      . map (second (\l -> (IM.empty,l,IM.empty)))
                      $ vs

    labNodes (Gr g) = [ (node, label)
                            | (node, (_, label, _)) <- IM.toList g ]

    noNodes   (Gr g) = IM.size g

    nodeRange (Gr g) = fromMaybe (error "nodeRange of empty graph")
                       $ liftA2 (,) (ix (IM.minViewWithKey g))
                                    (ix (IM.maxViewWithKey g))
      where
        ix = fmap (fst . fst)

    labEdges (Gr g) = do (node, (_, _, s)) <- IM.toList g
                         (next, labels)    <- IM.toList s
                         label             <- labels
                         return (node, next, label)

instance DynGraph Gr where
    (p, v, l, s) & (Gr g)
        = let !g1 = IM.insert v (preds, l, succs) g
              !(np, preds) = fromAdjCounting p
              !(ns, succs) = fromAdjCounting s
              !g2 = addSucc g1 v np preds
              !g3 = addPred g2 v ns succs
          in Gr g3


instance Functor (Gr a) where
  fmap = fastEMap

instance Bifunctor Gr where
  bimap = fastNEMap

  first = fastNMap

  second = fastEMap


matchGr :: Node -> Gr a b -> Decomp Gr a b
matchGr node (Gr g)
    = case IM.lookup node g of
        Nothing
            -> (Nothing, Gr g)

        Just (p, label, s)
            -> let !g1 = IM.delete node g
                   !p' = IM.delete node p
                   !s' = IM.delete node s
                   !g2 = clearPred g1 node s'
                   !g3 = clearSucc g2 node p'
               in (Just (toAdj p', node, label, toAdj s), Gr g3)

----------------------------------------------------------------------
-- OVERRIDING FUNCTIONS
----------------------------------------------------------------------

{-

{- RULES
      "insNode/Data.Graph.Inductive.PatriciaTree"  insNode = fastInsNode
  -}
fastInsNode :: LNode a -> Gr a b -> Gr a b
fastInsNode (v, l) (Gr g) = g' `seq` Gr g'
  where
    g' = IM.insert v (IM.empty, l, IM.empty) g

-}
{-# RULES
      "insEdge/GHC.Data.Graph.Inductive.PatriciaTree"  insEdge = fastInsEdge
  #-}
fastInsEdge :: LEdge b -> Gr a b -> Gr a b
fastInsEdge (v, w, l) (Gr g) = g2 `seq` Gr g2
  where
    g1 = IM.adjust addS' v g
    g2 = IM.adjust addP' w g1

    addS' (ps, l', ss) = (ps, l', IM.insertWith addLists w [l] ss)
    addP' (ps, l', ss) = (IM.insertWith addLists v [l] ps, l', ss)

{-

{- RULES
      "gmap/Data.Graph.Inductive.PatriciaTree"  gmap = fastGMap
  -}
fastGMap :: forall a b c d. (Context a b -> Context c d) -> Gr a b -> Gr c d
fastGMap f (Gr g) = Gr (IM.mapWithKey f' g)
  where
    f' :: Node -> Context' a b -> Context' c d
    f' = ((fromContext . f) .) . toContext

{- RULES
      "nmap/Data.Graph.Inductive.PatriciaTree"  nmap = fastNMap
  -}
-}
fastNMap :: forall a b c. (a -> c) -> Gr a b -> Gr c b
fastNMap f (Gr g) = Gr (IM.map f' g)
  where
    f' :: Context' a b -> Context' c b
    f' (ps, a, ss) = (ps, f a, ss)
{-

{- RULES
      "emap/GHC.Data.Graph.Inductive.PatriciaTree"  emap = fastEMap
   -}
-}
fastEMap :: forall a b c. (b -> c) -> Gr a b -> Gr a c
fastEMap f (Gr g) = Gr (IM.map f' g)
  where
    f' :: Context' a b -> Context' a c
    f' (ps, a, ss) = (IM.map (map f) ps, a, IM.map (map f) ss)

{-  RULES
      "nemap/GHC.Data.Graph.Inductive.PatriciaTree"  nemap = fastNEMap
   -}

fastNEMap :: forall a b c d. (a -> c) -> (b -> d) -> Gr a b -> Gr c d
fastNEMap fn fe (Gr g) = Gr (IM.map f g)
  where
    f :: Context' a b -> Context' c d
    f (ps, a, ss) = (IM.map (map fe) ps, fn a, IM.map (map fe) ss)


----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------

toAdj :: IntMap [b] -> Adj b
toAdj = concatMap expand . IM.toList
  where
    expand (n,ls) = map (flip (,) n) ls

--fromAdj :: Adj b -> IntMap [b]
--fromAdj = IM.fromListWith addLists . map (second (:[]) . swap)

data FromListCounting a = FromListCounting !Int !(IntMap a)
  deriving (Eq, Show, Read)

getFromListCounting :: FromListCounting a -> (Int, IntMap a)
getFromListCounting (FromListCounting i m) = (i, m)
{-# INLINE getFromListCounting #-}

fromListWithKeyCounting :: (Int -> a -> a -> a) -> [(Int, a)] -> (Int, IntMap a)
fromListWithKeyCounting f = getFromListCounting . foldl' ins (FromListCounting 0 IM.empty)
  where
    ins (FromListCounting i t) (k,x) = FromListCounting (i + 1) (IM.insertWithKey f k x t)
{-# INLINE fromListWithKeyCounting #-}

fromListWithCounting :: (a -> a -> a) -> [(Int, a)] -> (Int, IntMap a)
fromListWithCounting f = fromListWithKeyCounting (\_ x y -> f x y)
{-# INLINE fromListWithCounting #-}

fromAdjCounting :: Adj b -> (Int, IntMap [b])
fromAdjCounting = fromListWithCounting addLists . map (second (:[]) . swap)

-- We use differenceWith to modify a graph more than bulkThreshold times,
-- and repeated insertWith otherwise.
bulkThreshold :: Int
bulkThreshold = 5

--toContext :: Node -> Context' a b -> Context a b
--toContext v (ps, a, ss) = (toAdj ps, v, a, toAdj ss)

--fromContext :: Context a b -> Context' a b
--fromContext (ps, _, a, ss) = (fromAdj ps, a, fromAdj ss)

-- A version of @++@ where order isn't important, so @xs ++ [x]@
-- becomes @x:xs@.  Used when we have to have a function of type @[a]
-- -> [a] -> [a]@ but one of the lists is just going to be a single
-- element (and it isn't possible to tell which).
addLists :: [a] -> [a] -> [a]
addLists [a] as  = a : as
addLists as  [a] = a : as
addLists xs  ys  = xs ++ ys

addSucc :: forall a b . GraphRep a b -> Node -> Int -> IM.IntMap [b] -> GraphRep a b
addSucc g0 v numAdd xs
  | numAdd < bulkThreshold = foldlWithKey' go g0 xs
  where
    go :: GraphRep a b -> Node -> [b] -> GraphRep a b
    go g p l = IMS.adjust f p g
      where f (ps, l', ss) = let !ss' = IM.insertWith addLists v l ss
                             in (ps, l', ss')
addSucc g v _ xs = IMS.differenceWith go g xs
  where
    go :: Context' a b -> [b] -> Maybe (Context' a b)
    go (ps, l', ss) l = let !ss' = IM.insertWith addLists v l ss
                        in Just (ps, l', ss')

foldlWithKey' :: (a -> IM.Key -> b -> a) -> a -> IntMap b -> a
foldlWithKey' =
  IM.foldlWithKey'

addPred :: forall a b . GraphRep a b -> Node -> Int -> IM.IntMap [b] -> GraphRep a b
addPred g0 v numAdd xs
  | numAdd < bulkThreshold = foldlWithKey' go g0 xs
  where
    go :: GraphRep a b -> Node -> [b] -> GraphRep a b
    go g p l = IMS.adjust f p g
      where f (ps, l', ss) = let !ps' = IM.insertWith addLists v l ps
                             in (ps', l', ss)
addPred g v _ xs = IMS.differenceWith go g xs
  where
    go :: Context' a b -> [b] -> Maybe (Context' a b)
    go (ps, l', ss) l = let !ps' = IM.insertWith addLists v l ps
                        in Just (ps', l', ss)

clearSucc :: forall a b x . GraphRep a b -> Node -> IM.IntMap x -> GraphRep a b
clearSucc g v = IMS.differenceWith go g
  where
    go :: Context' a b -> x -> Maybe (Context' a b)
    go (ps, l, ss) _ = let !ss' = IM.delete v ss
                       in Just (ps, l, ss')

clearPred :: forall a b x . GraphRep a b -> Node -> IM.IntMap x -> GraphRep a b
clearPred g v = IMS.differenceWith go g
  where
    go :: Context' a b -> x -> Maybe (Context' a b)
    go (ps, l, ss) _ = let !ps' = IM.delete v ps
                       in Just (ps', l, ss)

{-----------------------------------------------------------------

Copyright (c) 1999-2008, Martin Erwig
              2010, Ivan Lazar Miljenovic
              2022, Norman Ramsey
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the author nor the names of its contributors may be
   used to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

----------------------------------------------------------------}
