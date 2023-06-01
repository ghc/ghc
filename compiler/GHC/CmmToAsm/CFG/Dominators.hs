{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}

{- |
  Module      :  GHC.CmmToAsm.CFG.Dominators
  Copyright   :  (c) Matt Morrow 2009
  License     :  BSD3
  Maintainer  :  <klebinger.andreas@gmx.at>
  Stability   :  stable
  Portability :  portable

  The Lengauer-Tarjan graph dominators algorithm.

    \[1\] Lengauer, Tarjan,
      /A Fast Algorithm for Finding Dominators in a Flowgraph/, 1979.

    \[2\] Muchnick,
      /Advanced Compiler Design and Implementation/, 1997.

    \[3\] Brisk, Sarrafzadeh,
      /Interference CGraphs for Procedures in Static Single/
      /Information Form are Interval CGraphs/, 2007.

 * Strictness

 Unless stated otherwise all exposed functions might fully evaluate their input
 but are not guaranteed to do so.

-}

module GHC.CmmToAsm.CFG.Dominators (
   Node,Path,Edge
  ,Graph,Rooted
  ,idom,ipdom
  ,domTree,pdomTree
  ,dom,pdom
  ,pddfs,rpddfs
  ,fromAdj,fromEdges
  ,toAdj,toEdges
  ,asTree,asCGraph
  ,parents,ancestors
) where

import GHC.Prelude
import Data.Bifunctor
import Data.Tuple (swap)

import Data.Tree
import Data.IntMap(IntMap)
import Data.IntSet(IntSet)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import Control.Monad
import Control.Monad.ST.Strict

import Data.Array.ST
import Data.Array.Base
  (unsafeNewArray_
  ,unsafeWrite,unsafeRead)
import GHC.Data.Word64Set (Word64Set)
import qualified GHC.Data.Word64Set as WS
import GHC.Data.Word64Map (Word64Map)
import qualified GHC.Data.Word64Map as WM
import Data.Word

-----------------------------------------------------------------------------

-- Compacted nodes; these can be stored in contiguous arrays
type CNode       = Int
type CGraph      = IntMap IntSet

type Node     = Word64
type Path     = [Node]
type Edge     = (Node, Node)
type Graph    = Word64Map Word64Set
type Rooted   = (Node, Graph)

-----------------------------------------------------------------------------

-- | /Dominators/.
-- Complexity as for @idom@
dom :: Rooted -> [(Node, Path)]
dom = ancestors . domTree

-- | /Post-dominators/.
-- Complexity as for @idom@.
pdom :: Rooted -> [(Node, Path)]
pdom = ancestors . pdomTree

-- | /Dominator tree/.
-- Complexity as for @idom@.
domTree :: Rooted -> Tree Node
domTree a@(r,_) =
  let is = filter ((/=r).fst) (idom a)
      tg = fromEdges (fmap swap is)
  in asTree (r,tg)

-- | /Post-dominator tree/.
-- Complexity as for @idom@.
pdomTree :: Rooted -> Tree Node
pdomTree a@(r,_) =
  let is = filter ((/=r).fst) (ipdom a)
      tg = fromEdges (fmap swap is)
  in asTree (r,tg)

-- | /Immediate dominators/.
-- /O(|E|*alpha(|E|,|V|))/, where /alpha(m,n)/ is
-- \"a functional inverse of Ackermann's function\".
--
-- This Complexity bound assumes /O(1)/ indexing. Since we're
-- using @IntMap@, it has an additional /lg |V|/ factor
-- somewhere in there. I'm not sure where.
idom :: Rooted -> [(Node,Node)]
idom rg = runST (evalS idomM =<< initEnv (pruneReach rg))

-- | /Immediate post-dominators/.
-- Complexity as for @idom@.
ipdom :: Rooted -> [(Node,Node)]
ipdom rg = runST (evalS idomM =<< initEnv (pruneReach (second predGW rg)))

-----------------------------------------------------------------------------

-- | /Post-dominated depth-first search/.
pddfs :: Rooted -> [Node]
pddfs = reverse . rpddfs

-- | /Reverse post-dominated depth-first search/.
rpddfs :: Rooted -> [Node]
rpddfs = concat . levels . pdomTree

-----------------------------------------------------------------------------

type Dom s a = S s (Env s) a
type NodeSet    = Word64Set
type NodeMap a  = Word64Map a
data Env s = Env
  {succE      :: !CGraph
  ,predE      :: !CGraph
  ,bucketE    :: !CGraph
  ,dfsE       :: {-# UNPACK #-}!Int
  ,zeroE      :: {-# UNPACK #-}!CNode
  ,rootE      :: {-# UNPACK #-}!CNode
  ,labelE     :: {-# UNPACK #-}!(Arr s CNode)
  ,parentE    :: {-# UNPACK #-}!(Arr s CNode)
  ,ancestorE  :: {-# UNPACK #-}!(Arr s CNode)
  ,childE     :: {-# UNPACK #-}!(Arr s CNode)
  ,ndfsE      :: {-# UNPACK #-}!(Arr s CNode)
  ,dfnE       :: {-# UNPACK #-}!(Arr s Int)
  ,sdnoE      :: {-# UNPACK #-}!(Arr s Int)
  ,sizeE      :: {-# UNPACK #-}!(Arr s Int)
  ,domE       :: {-# UNPACK #-}!(Arr s CNode)
  ,rnE        :: {-# UNPACK #-}!(Arr s Node)}

-----------------------------------------------------------------------------

idomM :: Dom s [(Node,Node)]
idomM = do
  dfsDom =<< rootM
  n <- gets dfsE
  forM_ [n,n-1..1] (\i-> do
    w <- ndfsM i
    ps <- predsM w
    forM_ ps (\v-> do
      sw <- sdnoM w
      u <- eval v
      su <- sdnoM u
      when (su < sw)
        (store sdnoE w su))
    z <- ndfsM =<< sdnoM w
    modify(\e->e{bucketE=IM.adjust
                      (w`IS.insert`)
                      z (bucketE e)})
    pw <- parentM w
    link pw w
    bps <- bucketM pw
    forM_ bps (\v-> do
      u <- eval v
      su <- sdnoM u
      sv <- sdnoM v
      let dv = case su < sv of
                True-> u
                False-> pw
      store domE v dv))
  forM_ [1..n] (\i-> do
    w <- ndfsM i
    j <- sdnoM w
    z <- ndfsM j
    dw <- domM w
    when (dw /= z)
      (do ddw <- domM dw
          store domE w ddw))
  fromEnv

-----------------------------------------------------------------------------

eval :: CNode -> Dom s CNode
eval v = do
  n0 <- zeroM
  a  <- ancestorM v
  case a==n0 of
    True-> labelM v
    False-> do
      compress v
      a   <- ancestorM v
      l   <- labelM v
      la  <- labelM a
      sl  <- sdnoM l
      sla <- sdnoM la
      case sl <= sla of
        True-> return l
        False-> return la

compress :: CNode -> Dom s ()
compress v = do
  n0  <- zeroM
  a   <- ancestorM v
  aa  <- ancestorM a
  when (aa /= n0) (do
    compress a
    a   <- ancestorM v
    aa  <- ancestorM a
    l   <- labelM v
    la  <- labelM a
    sl  <- sdnoM l
    sla <- sdnoM la
    when (sla < sl)
      (store labelE v la)
    store ancestorE v aa)

-----------------------------------------------------------------------------

link :: CNode -> CNode -> Dom s ()
link v w = do
  n0  <- zeroM
  lw  <- labelM w
  slw <- sdnoM lw
  let balance s = do
        c   <- childM s
        lc  <- labelM c
        slc <- sdnoM lc
        case slw < slc of
          False-> return s
          True-> do
            zs  <- sizeM s
            zc  <- sizeM c
            cc  <- childM c
            zcc <- sizeM cc
            case 2*zc <= zs+zcc of
              True-> do
                store ancestorE c s
                store childE s cc
                balance s
              False-> do
                store sizeE c zs
                store ancestorE s c
                balance c
  s   <- balance w
  lw  <- labelM w
  zw  <- sizeM w
  store labelE s lw
  store sizeE v . (+zw) =<< sizeM v
  let follow s =
        when (s /= n0) (do
          store ancestorE s v
          follow =<< childM s)
  zv  <- sizeM v
  follow =<< case zv < 2*zw of
              False-> return s
              True-> do
                cv <- childM v
                store childE v s
                return cv

-----------------------------------------------------------------------------

dfsDom :: CNode -> Dom s ()
dfsDom i = do
  _   <- go i
  n0  <- zeroM
  r   <- rootM
  store parentE r n0
  where go i = do
          n <- nextM
          store dfnE   i n
          store sdnoE  i n
          store ndfsE  n i
          store labelE i i
          ss <- succsM i
          forM_ ss (\j-> do
            s <- sdnoM j
            case s==0 of
              False-> return()
              True-> do
                store parentE j i
                go j)

-----------------------------------------------------------------------------

initEnv :: Rooted -> ST s (Env s)
initEnv (r0,g0) = do
  -- CGraph renumbered to indices from 1 to |V|
  let (g,rnmap) = renum 1 g0
      pred      = predG g -- reverse graph
      root      = rnmap WM.! r0 -- renamed root
      n         = IM.size g
      ns        = [0..n]
      m         = n+1

  let bucket = IM.fromList
        (zip ns (repeat mempty))

  rna <- newW m
  writes rna (fmap swap
        (WM.toList rnmap))

  doms      <- newI m
  sdno      <- newI m
  size      <- newI m
  parent    <- newI m
  ancestor  <- newI m
  child     <- newI m
  label     <- newI m
  ndfs      <- newI m
  dfn       <- newI m

  -- Initialize all arrays
  forM_ [0..n] (doms.=0)
  forM_ [0..n] (sdno.=0)
  forM_ [1..n] (size.=1)
  forM_ [0..n] (ancestor.=0)
  forM_ [0..n] (child.=0)

  (doms.=root) root
  (size.=0) 0
  (label.=0) 0

  return (Env
    {rnE        = rna
    ,dfsE       = 0
    ,zeroE      = 0
    ,rootE      = root
    ,labelE     = label
    ,parentE    = parent
    ,ancestorE  = ancestor
    ,childE     = child
    ,ndfsE      = ndfs
    ,dfnE       = dfn
    ,sdnoE      = sdno
    ,sizeE      = size
    ,succE      = g
    ,predE      = pred
    ,bucketE    = bucket
    ,domE       = doms})

fromEnv :: Dom s [(Node,Node)]
fromEnv = do
  dom   <- gets domE
  rn    <- gets rnE
  -- r     <- gets rootE
  (_,n) <- st (getBounds dom)
  forM [1..n] (\i-> do
    j <- st(rn!:i)
    d <- st(dom!:i)
    k <- st(rn!:d)
    return (j,k))

-----------------------------------------------------------------------------

zeroM :: Dom s CNode
zeroM = gets zeroE
domM :: CNode -> Dom s CNode
domM = fetch domE
rootM :: Dom s CNode
rootM = gets rootE
succsM :: CNode -> Dom s [CNode]
succsM i = gets (IS.toList . (! i) . succE)
predsM :: CNode -> Dom s [CNode]
predsM i = gets (IS.toList . (! i) . predE)
bucketM :: CNode -> Dom s [CNode]
bucketM i = gets (IS.toList . (! i) . bucketE)
sizeM :: CNode -> Dom s Int
sizeM = fetch sizeE
sdnoM :: CNode -> Dom s Int
sdnoM = fetch sdnoE
-- dfnM :: CNode -> Dom s Int
-- dfnM = fetch dfnE
ndfsM :: Int -> Dom s CNode
ndfsM = fetch ndfsE
childM :: CNode -> Dom s CNode
childM = fetch childE
ancestorM :: CNode -> Dom s CNode
ancestorM = fetch ancestorE
parentM :: CNode -> Dom s CNode
parentM = fetch parentE
labelM :: CNode -> Dom s CNode
labelM = fetch labelE
nextM :: Dom s Int
nextM = do
  n <- gets dfsE
  let n' = n+1
  modify(\e->e{dfsE=n'})
  return n'

-----------------------------------------------------------------------------

type A = STUArray
type Arr s a = A s Int a

infixl 9 !:
infixr 2 .=

-- | arr .= x idx => write x to index
(.=) :: (MArray (A s) a (ST s))
     => Arr s a -> a -> Int -> ST s ()
(v .= x) i = unsafeWrite v i x

(!:) :: (MArray (A s) a (ST s))
     => A s Int a -> Int -> ST s a
a !: i = do
  o <- unsafeRead a i
  return $! o

new :: (MArray (A s) a (ST s))
    => Int -> ST s (Arr s a)
new n = unsafeNewArray_ (0,n-1)

newI :: Int -> ST s (Arr s Int)
newI = new

newW :: Int -> ST s (Arr s Node)
newW = new

writes :: (MArray (A s) a (ST s))
     => Arr s a -> [(Int,a)] -> ST s ()
writes a xs = forM_ xs (\(i,x) -> (a.=x) i)


(!) :: Monoid a => IntMap a -> Int -> a
(!) g n = maybe mempty id (IM.lookup n g)

fromAdj :: [(Node, [Node])] -> Graph
fromAdj = WM.fromList . fmap (second WS.fromList)

fromEdges :: [Edge] -> Graph
fromEdges = collectW WS.union fst (WS.singleton . snd)

toAdj :: Graph -> [(Node, [Node])]
toAdj = fmap (second WS.toList) . WM.toList

toEdges :: Graph -> [Edge]
toEdges = concatMap (uncurry (fmap . (,))) . toAdj

predG :: CGraph -> CGraph
predG g = IM.unionWith IS.union (go g) g0
  where g0 = fmap (const mempty) g
        go = flip IM.foldrWithKey mempty (\i a m ->
                foldl' (\m p -> IM.insertWith mappend p
                                      (IS.singleton i) m)
                        m
                       (IS.toList a))

predGW :: Graph -> Graph
predGW g = WM.unionWith WS.union (go g) g0
  where g0 = fmap (const mempty) g
        go = flip WM.foldrWithKey mempty (\i a m ->
                foldl' (\m p -> WM.insertWith mappend p
                                      (WS.singleton i) m)
                        m
                       (WS.toList a))

pruneReach :: Rooted -> Rooted
pruneReach (r,g) = (r,g2)
  where is = reachable
              (maybe mempty id
                . flip WM.lookup g) $ r
        g2 = WM.fromList
            . fmap (second (WS.filter (`WS.member`is)))
            . filter ((`WS.member`is) . fst)
            . WM.toList $ g

tip :: Tree a -> (a, [Tree a])
tip (Node a ts) = (a, ts)

parents :: Tree a -> [(a, a)]
parents (Node i xs) = p i xs
        ++ concatMap parents xs
  where p i = fmap (flip (,) i . rootLabel)

ancestors :: Tree a -> [(a, [a])]
ancestors = go []
  where go acc (Node i xs)
          = let acc' = i:acc
            in p acc' xs ++ concatMap (go acc') xs
        p is = fmap (flip (,) is . rootLabel)

asCGraph :: Tree Node -> Rooted
asCGraph t@(Node a _) = let g = go t in (a, fromAdj g)
  where go (Node a ts) = let as = (fst . unzip . fmap tip) ts
                          in (a, as) : concatMap go ts

asTree :: Rooted -> Tree Node
asTree (r,g) = let go a = Node a (fmap go ((WS.toList . f) a))
                   f = (g !)
            in go r
  where (!) g n = maybe mempty id (WM.lookup n g)


reachable :: (Node -> NodeSet) -> (Node -> NodeSet)
reachable f a = go (WS.singleton a) a
  where go seen a = let s = f a
                        as = WS.toList (s `WS.difference` seen)
                    in foldl' go (s `WS.union` seen) as

collectW :: (c -> c -> c)
        -> (a -> Node) -> (a -> c) -> [a] -> Word64Map c
collectW (<>) f g
  = foldl' (\m a -> WM.insertWith (<>)
                                  (f a)
                                  (g a) m) mempty

-- | renum n g: Rename all nodes
--
-- Gives nodes sequential names starting at n.
-- Returns the new graph and a mapping.
-- (renamed, old -> new)
renum :: Int -> Graph -> (CGraph, NodeMap CNode)
renum from = (\(_,m,g)->(g,m))
  . WM.foldrWithKey
      (\i ss (!n,!env,!new)->
          let (j,n2,env2) = go n env i
              (n3,env3,ss2) = WS.fold
                (\k (!n,!env,!new)->
                    case go n env k of
                      (l,n2,env2)-> (n2,env2,l `IS.insert` new))
                (n2,env2,mempty) ss
              new2 = IM.insertWith IS.union j ss2 new
          in (n3,env3,new2)) (from,mempty,mempty)
  where go :: Int
           -> NodeMap CNode
           -> Node
           -> (CNode,Int,NodeMap CNode)
        go !n !env i =
          case WM.lookup i env of
            Just j -> (j,n,env)
            Nothing -> (n,n+1,WM.insert i n env)

-----------------------------------------------------------------------------

-- Nothing better than reinventing the state monad.
newtype S z s a = S {unS :: forall o. (a -> s -> ST z o) -> s -> ST z o}
  deriving (Functor)
instance Monad (S z s) where
  return = pure
  S g >>= f = S (\k -> g (\a -> unS (f a) k))
instance Applicative (S z s) where
  pure a = S (\k -> k a)
  (<*>) = ap
-- get :: S z s s
-- get = S (\k s -> k s s)
gets :: (s -> a) -> S z s a
gets f = S (\k s -> k (f s) s)
-- set :: s -> S z s ()
-- set s = S (\k _ -> k () s)
modify :: (s -> s) -> S z s ()
modify f = S (\k -> k () . f)
-- runS :: S z s a -> s -> ST z (a, s)
-- runS (S g) = g (\a s -> return (a,s))
evalS :: S z s a -> s -> ST z a
evalS (S g) = g ((return .) . const)
-- execS :: S z s a -> s -> ST z s
-- execS (S g) = g ((return .) . flip const)
st :: ST z a -> S z s a
st m = S (\k s-> do
  a <- m
  k a s)
store :: (MArray (A z) a (ST z))
      => (s -> Arr z a) -> Int -> a -> S z s ()
store f i x = do
  a <- gets f
  st ((a.=x) i)
fetch :: (MArray (A z) a (ST z))
      => (s -> Arr z a) -> Int -> S z s a
fetch f i = do
  a <- gets f
  st (a!:i)
