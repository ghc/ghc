module ZipCfg
    ( 	-- These data types and names are carefully thought out
      Graph(..), LGraph(..), FGraph(..)
    , Block(..), ZBlock(..), ZHead(..), ZTail(..), ZLast(..)
    , insertBlock
    , HavingSuccessors, succs, fold_succs
    , LastNode, mkBranchNode, isBranchNode, branchNodeTarget

        -- Observers and transformers
	-- (open to renaming suggestions here)
    , blockId, zip, unzip, last, goto_end, zipht, tailOfLast
    , splice_tail, splice_head, splice_head_only', splice_head'
    , of_block_list, to_block_list
    , graphOfLGraph
    , map_blocks, map_one_block, map_nodes, mapM_blocks
    , postorder_dfs, postorder_dfs_from, postorder_dfs_from_except
    , fold_layout
    , fold_blocks, fold_fwd_block
    , translate

    , pprLgraph, pprGraph

    , entry -- exported for the convenience of ZipDataflow0, at least for now

    {-
    -- the following functions might one day be useful and can be found
    -- either below or in ZipCfgExtras:
    , entry, exit, focus, focusp, unfocus
    , ht_to_block, ht_to_last, 
    , splice_focus_entry, splice_focus_exit
    , foldM_fwd_block
    -}

    )
where

#include "HsVersions.h"

import BlockId ( BlockId, BlockEnv, emptyBlockEnv, lookupBlockEnv, extendBlockEnv
               , BlockSet, emptyBlockSet, unitBlockSet, elemBlockSet, extendBlockSet
               , delFromBlockEnv, foldBlockEnv', mapBlockEnv
               , eltsBlockEnv, isNullBEnv, plusBlockEnv)
import CmmExpr ( UserOfLocalRegs(..) )
import PprCmm()

import Outputable hiding (empty)

import Data.Maybe
import Prelude hiding (zip, unzip, last)

-------------------------------------------------------------------------
--               GENERIC ZIPPER-BASED CONTROL-FLOW GRAPH               --
-------------------------------------------------------------------------
{-

This module defines datatypes used to represent control-flow graphs,
along with some functions for analyzing and splicing graphs.
Functions for building graphs are found in a separate module 'MkZipCfg'.

Every graph has a distinguished entry point.  A graph has at least one
exit; most exits are instructions (or statements) like 'jump' or
'return', which transfer control to other procedures, but a graph may
have up to one 'fall through' exit.  (A graph that represents an
entire Haskell or C-- procedure does not have a 'fall through' exit.)

A graph is a collection of basic blocks.  A basic block begins with a
label (unique id; see Note [Unique BlockId]) which is followed by a
sequence of zero or more 'middle' nodes; the basic block ends with a
'last' node.  Each 'middle' node is a single-entry, single-exit,
uninterruptible computation.  A 'last' node is a single-entry,
multiple-exit computation.  A last node may have zero or more successors,
which are identified by their unique ids.

A special case of last node is the ``default exit,'' which represents
'falling off the end' of the graph.  Such a node is always represented by
the data constructor 'LastExit'.  A graph may contain at most one
'LastExit' node, and a graph representing a full procedure should not
contain any 'LastExit' nodes.  'LastExit' nodes are used only to splice
graphs together, either during graph construction (see module 'MkZipCfg')
or during optimization (see module 'ZipDataflow').

A graph is parameterized over the types of middle and last nodes.  Each of
these types will typically be instantiated with a subset of C-- statements
(see module 'ZipCfgCmmRep') or a subset of machine instructions (yet to be
implemented as of August 2007).


Note [Kinds of Graphs]
~~~~~~~~~~~~~~~~~~~~~~
This module exposes three representations of graphs.  In order of
increasing complexity, they are:

  Graph  m l      The basic graph with its distinguished entry point

  LGraph m l      A graph with a *labelled* entry point

  FGraph m l      A labelled graph with the *focus* on a particular edge

There are three types because each type offers a slightly different
invariant or cost model.  

  * The distinguished entry of a Graph has no label.  Because labels must be
    unique, acquiring one requires a supply of Unique labels (BlockId's).
    The primary advantage of the Graph representation is that we can build a
    small Graph purely functionally, without needing a fresh BlockId or
    Unique.  For example, during optimization we can easily rewrite a single
    middle node into a Graph containing a sequence of two middle nodes
    followed by LastExit.

  * In an LGraph, every basic block is labelled.  The primary advantage of
    this representation is its simplicity: each basic block can be treated
    like any other.  This representation is used for mapping, folding, and
    translation, as well as layout.

    Like any graph, an LGraph still has a distinguished entry point, 
    which you can discover using 'lg_entry'.

  * An FGraph is an LGraph with the *focus* on one particular edge.  The
    primary advantage of this representation is that it provides
    constant-time access to the nodes connected by that edge, and it also
    allows constant-time, functional *replacement* of those nodes---in the
    style of Huet's 'zipper'.

None of these representations is ideally suited to the incremental
construction of large graphs.  A separate module, 'MkZipCfg', provides a
fourth representation that is asymptotically optimal for such construction.
    
-}

--------------- Representation --------------------

-- | A basic block is a 'first' node, followed by zero or more 'middle'
-- nodes, followed by a 'last' node.

-- eventually this module should probably replace the original Cmm, but for
-- now we leave it to dynamic invariants what can be found where

data ZLast l
  = LastExit     -- fall through; used for the block that has no last node
                 -- LastExit is a device used only for graphs under 
                 -- construction, or framgments of graph under optimisation,
                 -- so we don't want to pollute the 'l' type parameter with it
  | LastOther l

--So that we don't have orphan instances, this goes here or in CmmExpr.
--At least UserOfLocalRegs (ZLast Last) is needed (Last defined elsewhere),
--but there's no need for non-Haskell98 instances for that.
instance UserOfLocalRegs a => UserOfLocalRegs (ZLast a) where
    foldRegsUsed  f z (LastOther l) = foldRegsUsed f z l
    foldRegsUsed _f z LastExit      = z


data ZHead m   = ZFirst BlockId
               | ZHead (ZHead m) m
    -- ZHead is a (reversed) sequence of middle nodes labeled by a BlockId
data ZTail m l = ZLast (ZLast l) | ZTail m (ZTail m l)
    -- ZTail is a sequence of middle nodes followed by a last node

-- | Blocks and flow graphs; see Note [Kinds of graphs]

data Block m l = Block { bid       :: BlockId
                       , tail      :: ZTail m l }

data Graph m l = Graph { g_entry :: (ZTail m l), g_blocks :: (BlockEnv (Block m l)) }

data LGraph m l = LGraph  { lg_entry     :: BlockId
                          , lg_blocks    :: BlockEnv (Block m l)}
	-- Invariant: lg_entry is in domain( lg_blocks )

-- | And now the zipper.  The focus is between the head and tail.
-- We cannot ever focus on an inter-block edge.
data ZBlock m l = ZBlock (ZHead m) (ZTail m l)
data FGraph m l = FGraph { fg_entry  :: BlockId
                         , fg_focus  :: ZBlock m l
                         , fg_others :: BlockEnv (Block m l) }
                    -- Invariant: the block represented by 'fg_focus' is *not*
                    -- in the map 'fg_others'

----  Utility functions ---

blockId   :: Block  m l -> BlockId
zip       :: ZBlock m l -> Block  m l
unzip     :: Block  m l -> ZBlock m l

last      :: ZBlock m l -> ZLast l
goto_end  :: ZBlock m l -> (ZHead m, ZLast l)

tailOfLast :: l -> ZTail m l

-- | Take a head and tail and go to beginning or end.  The asymmetry
-- in the types and names is a bit unfortunate, but 'Block m l' is
-- effectively '(BlockId, ZTail m l)' and is accepted in many more places.

ht_to_block, zipht :: ZHead m -> ZTail m l -> Block m l
ht_to_last         :: ZHead m -> ZTail m l -> (ZHead m, ZLast l)

-- | We can splice a single-entry, single-exit LGraph onto a head or a tail.
-- For a head, we have a head 'h' followed by a LGraph 'g'.
-- The entry node of 'g' gets joined to 'h', forming the entry into
-- the new LGraph.  The exit of 'g' becomes the new head.
-- For both arguments and results, the order of values is the order of
-- control flow: before splicing, the head flows into the LGraph; after
-- splicing, the LGraph flows into the head.
-- Splicing a tail is the dual operation.
-- (In order to maintain the order-means-control-flow convention, the
-- orders are reversed.)
--
-- For example, assume
--	head = [L: x:=0]
--	grph = (M, [M: <stuff>,
--		    <blocks>,
--                  N: y:=x; LastExit])
--	tail = [return (y,x)]
--
-- Then 	splice_head head grph
--		= ((L, [L: x:=0; goto M,
--			M: <stuff>,
--			<blocks>])
--		   , N: y:=x)
--
-- Then 	splice_tail grph tail
--		= ( <stuff>
--		  , (???, [<blocks>,
--			   N: y:=x; return (y,x)])

splice_head  :: ZHead m   -> LGraph m l -> (LGraph m l, ZHead  m)
splice_head' :: ZHead m   -> Graph m l  -> (BlockEnv (Block m l), ZHead m)
splice_tail  :: Graph m l -> ZTail  m l -> Graph m l

-- | We can also splice a single-entry, no-exit Graph into a head.
splice_head_only  :: ZHead m -> LGraph m l -> LGraph m l
splice_head_only' :: ZHead m -> Graph m l  -> LGraph m l


-- | A safe operation 

-- | Conversion to and from the environment form is convenient.  For
-- layout or dataflow, however, one will want to use 'postorder_dfs'
-- in order to get the blocks in an order that relates to the control
-- flow in the procedure.
of_block_list :: BlockId -> [Block m l] -> LGraph m l  -- N log N
to_block_list :: LGraph m l -> [Block m l]  -- N log N

-- | Conversion from LGraph to Graph
graphOfLGraph :: LastNode l => LGraph m l -> Graph m l
graphOfLGraph (LGraph eid blocks) = Graph (ZLast $ mkBranchNode eid) blocks


-- | Traversal: 'postorder_dfs' returns a list of blocks reachable
-- from the entry node.  This list has the following property:
--
--	Say a "back reference" exists if one of a block's
--	control-flow successors precedes it in the output list
--
--	Then there are as few back references as possible
--
-- The output is suitable for use in
-- a forward dataflow problem.  For a backward problem, simply reverse
-- the list.  ('postorder_dfs' is sufficiently tricky to implement that
-- one doesn't want to try and maintain both forward and backward
-- versions.)

postorder_dfs :: LastNode l => LGraph m l -> [Block m l]

-- | For layout, we fold over pairs of 'Block m l' and 'Maybe BlockId'
-- in layout order.  The 'Maybe BlockId', if present, identifies the
-- block that will be the layout successor of the current block.  This
-- may be useful to help an emitter omit the final 'goto' of a block
-- that flows directly to its layout successor.
--
-- For example: fold_layout f z [ L1:B1, L2:B2, L3:B3 ]
--		= z <$> f (L1:B1) (Just L2)
--		    <$> f (L2:B2) (Just L3)
--		    <$> f (L3:B3) Nothing
-- where a <$> f = f a
fold_layout ::
    LastNode l => (Block m l -> Maybe BlockId -> a -> a) -> a -> LGraph m l-> a

-- | We can also fold over blocks in an unspecified order.  The
-- 'ZipCfgExtras' module provides a monadic version, which we
-- haven't needed (else it would be here).
fold_blocks :: (Block m l -> a -> a) -> a -> LGraph m l -> a

-- | Fold from first to last
fold_fwd_block :: (BlockId -> a -> a) -> (m -> a -> a) ->
                  (ZLast l -> a -> a) -> Block m l -> a -> a

map_one_block :: (BlockId -> BlockId) -> (m -> m') -> (l -> l') -> Block m l -> Block m' l'

map_nodes :: (BlockId -> BlockId) -> (m -> m') -> (l -> l') -> LGraph m l -> LGraph m' l'
   -- mapping includes the entry id!

map_blocks  :: (Block m l -> Block m' l') -> LGraph m l -> LGraph m' l'
mapM_blocks :: Monad mm
            => (Block m l -> mm (Block m' l')) -> LGraph m l -> mm (LGraph m' l')

-- | These translation functions are speculative.  I hope eventually
-- they will be used in the native-code back ends ---NR
translate :: Monad tm =>
             (m          -> tm (LGraph m' l')) ->
             (l          -> tm (LGraph m' l')) ->
             (LGraph m l -> tm (LGraph m' l'))

{-
-- | It's possible that another form of translation would be more suitable:
translateA :: (m -> Agraph m' l') -> (l -> AGraph m' l') -> LGraph m l -> LGraph m' l'
-}

------------------- Last nodes

-- | We can't make a graph out of just any old 'last node' type.  A last node
-- has to be able to find its successors, and we need to be able to create and
-- identify unconditional branches.  We put these capabilities in a type class.
-- Moreover, the property of having successors is also shared by 'Block's and
-- 'ZTails', so it is useful to have that property in a type class of its own.

class HavingSuccessors b where
    succs :: b -> [BlockId]
    fold_succs :: (BlockId -> a -> a) -> b -> a -> a

    fold_succs add l z = foldr add z $ succs l

class HavingSuccessors l => LastNode l where
    mkBranchNode     :: BlockId -> l
    isBranchNode     :: l -> Bool
    branchNodeTarget :: l -> BlockId  -- panics if not branch node
      -- ^ N.B. This interface seems to make for more congenial clients than a
      -- single function of type 'l -> Maybe BlockId'

instance HavingSuccessors l => HavingSuccessors (ZLast l) where
    succs LastExit = []
    succs (LastOther l) = succs l
    fold_succs _ LastExit z = z
    fold_succs f (LastOther l) z = fold_succs f l z

instance LastNode l => LastNode (ZLast l) where
    mkBranchNode id = LastOther $ mkBranchNode id
    isBranchNode LastExit = False
    isBranchNode (LastOther l) = isBranchNode l
    branchNodeTarget LastExit = panic "branchNodeTarget LastExit"
    branchNodeTarget (LastOther l) = branchNodeTarget l

instance LastNode l => HavingSuccessors (ZBlock m l) where
    succs b = succs (last b)

instance LastNode l => HavingSuccessors (Block m l) where
    succs b = succs (unzip b)

instance LastNode l => HavingSuccessors (ZTail m l) where
    succs b = succs (lastTail b)



-- ================ IMPLEMENTATION ================--

----- block manipulations

blockId (Block id _) = id

-- | Convert block between forms.
-- These functions are tail-recursive, so we can go as deep as we like
-- without fear of stack overflow.  

ht_to_block head tail = case head of
  ZFirst id -> Block id tail
  ZHead h m -> ht_to_block h (ZTail m tail) 

ht_to_last head (ZLast l)   = (head, l)
ht_to_last head (ZTail m t) = ht_to_last (ZHead head m) t 

zipht            h t  = ht_to_block h t
zip      (ZBlock h t) = ht_to_block h t
goto_end (ZBlock h t) = ht_to_last  h t

unzip (Block id t) = ZBlock (ZFirst id) t

head_id :: ZHead m -> BlockId
head_id (ZFirst id) = id
head_id (ZHead  h  _)   = head_id h

last (ZBlock _ t) = lastTail t

lastTail :: ZTail m l -> ZLast l
lastTail (ZLast l) = l
lastTail (ZTail _ t) = lastTail t

tailOfLast l = ZLast (LastOther l) -- tedious to write in every client


------------------ simple graph manipulations

focus :: BlockId -> LGraph m l -> FGraph m l -- focus on edge out of node with id 
focus id (LGraph entry blocks) =
    case lookupBlockEnv blocks id of
      Just b -> FGraph entry (unzip b) (delFromBlockEnv blocks id)
      Nothing -> panic "asked for nonexistent block in flow graph"

entry   :: LGraph m l -> FGraph m l         -- focus on edge out of entry node 
entry g@(LGraph eid _) = focus eid g

-- | pull out a block satisfying the predicate, if any
splitp_blocks :: (Block m l -> Bool) -> BlockEnv (Block m l) ->
                 Maybe (Block m l, BlockEnv (Block m l))
splitp_blocks p blocks = lift $ foldBlockEnv' scan (Nothing, emptyBlockEnv) blocks 
    where scan b (yes, no) =
              case yes of
                Nothing | p b -> (Just b, no)
                        | otherwise -> (yes, insertBlock b no)
                Just _ -> (yes, insertBlock b no)
          lift (Nothing, _) = Nothing
          lift (Just b, bs) = Just (b, bs)

-- | 'insertBlock' should not be used to /replace/ an existing block
-- but only to insert a new one
insertBlock :: Block m l -> BlockEnv (Block m l) -> BlockEnv (Block m l)
insertBlock b bs =
      ASSERT (isNothing $ lookupBlockEnv bs id)
      extendBlockEnv bs id b
    where id = blockId b

-- | Used in assertions; tells if a graph has exactly one exit
single_exit :: LGraph l m -> Bool
single_exit g = foldBlockEnv' check 0 (lg_blocks g) == 1
    where check block count = case last (unzip block) of
                                LastExit -> count + (1 :: Int)
                                _ -> count

-- | Used in assertions; tells if a graph has exactly one exit
single_exitg :: Graph l m -> Bool
single_exitg (Graph tail blocks) = foldBlockEnv' add (exit_count (lastTail tail)) blocks == 1
    where add block count = count + exit_count (last (unzip block))
          exit_count LastExit = 1 :: Int
          exit_count _        = 0

------------------ graph traversals

-- | This is the most important traversal over this data structure.  It drops
-- unreachable code and puts blocks in an order that is good for solving forward
-- dataflow problems quickly.  The reverse order is good for solving backward
-- dataflow problems quickly.  The forward order is also reasonably good for
-- emitting instructions, except that it will not usually exploit Forrest
-- Baskett's trick of eliminating the unconditional branch from a loop.  For
-- that you would need a more serious analysis, probably based on dominators, to
-- identify loop headers.
--
-- The ubiquity of 'postorder_dfs' is one reason for the ubiquity of the 'LGraph'
-- representation, when for most purposes the plain 'Graph' representation is
-- more mathematically elegant (but results in more complicated code).
--
-- Here's an easy way to go wrong!  Consider
-- @
--	A -> [B,C]
--	B -> D
--	C -> D
-- @
-- Then ordinary dfs would give [A,B,D,C] which has a back ref from C to D.
-- Better to get [A,B,C,D]


postorder_dfs g@(LGraph _ blockenv) =
    let FGraph id eblock _ = entry g in
     zip eblock : postorder_dfs_from_except blockenv eblock (unitBlockSet id)

postorder_dfs_from_except :: forall m b l. (HavingSuccessors b, LastNode l)
                          => BlockEnv (Block m l) -> b -> BlockSet -> [Block m l]
postorder_dfs_from_except blocks b visited 
  = vchildren (get_children b) (\acc _visited -> acc) [] visited
  where
    vnode :: Block m l -> ([Block m l] -> BlockSet -> a)
          -> [Block m l] -> BlockSet -> a
    vnode block@(Block id _) cont acc visited =
        if elemBlockSet id visited then
            cont acc visited
        else
            let cont' acc visited = cont (block:acc) visited in
            vchildren (get_children block) cont' acc (extendBlockSet visited id)

    vchildren :: [Block m l] -> ([Block m l] -> BlockSet -> a)
              -> [Block m l] -> BlockSet -> a
    vchildren bs cont acc visited =
        let next children acc visited =
                case children of []     -> cont acc visited
                                 (b:bs) -> vnode b (next bs) acc visited
        in next bs acc visited

    get_children :: HavingSuccessors c => c -> [Block m l]
    get_children block = foldl add_id [] (succs block)

    add_id :: [Block m l] -> BlockId -> [Block m l]
    add_id rst id = case lookupBlockEnv blocks id of
                      Just b -> b : rst
                      Nothing -> rst

postorder_dfs_from
    :: (HavingSuccessors b, LastNode l) => BlockEnv (Block m l) -> b -> [Block m l]
postorder_dfs_from blocks b = postorder_dfs_from_except blocks b emptyBlockSet



-- | Slightly more complicated than the usual fold because we want to tell block
-- 'b1' what its inline successor is going to be, so that if 'b1' ends with
-- 'goto b2', the goto can be omitted.

fold_layout f z g@(LGraph eid _) = fold (postorder_dfs g) z
  where fold blocks z =
            case blocks of [] -> z
                           [b] -> f b Nothing z
                           b1 : b2 : bs -> fold (b2 : bs) (f b1 (nextlabel b2) z)
        nextlabel (Block id _) =
            if id == eid then panic "entry as successor"
            else Just id

-- | The rest of the traversals are straightforward

map_blocks f (LGraph eid blocks) = LGraph eid (mapBlockEnv f blocks)

map_nodes idm middle last (LGraph eid blocks) =
  LGraph (idm eid) (mapBlockEnv (map_one_block idm middle last) blocks)

map_one_block idm middle last (Block id t) = Block (idm id) (tail t)
    where tail (ZTail m t) = ZTail (middle m) (tail t)
          tail (ZLast LastExit) = ZLast LastExit
          tail (ZLast (LastOther l)) = ZLast (LastOther (last l))


mapM_blocks f (LGraph eid blocks) = blocks' >>= return . LGraph eid
    where blocks' =
            foldBlockEnv' (\b mblocks -> do { blocks <- mblocks
                                      ; b <- f b
                                      ; return $ insertBlock b blocks })
                    (return emptyBlockEnv) blocks

fold_blocks f z (LGraph _ blocks) = foldBlockEnv' f z blocks
fold_fwd_block first middle last (Block id t) z = tail t (first id z)
    where tail (ZTail m t) z = tail t (middle m z)
          tail (ZLast l)   z = last l z

of_block_list e blocks = LGraph e $ foldr insertBlock emptyBlockEnv blocks 
to_block_list (LGraph _ blocks) = eltsBlockEnv blocks


-- We want to be able to scrutinize a single-entry, single-exit 'LGraph' for
-- splicing purposes.  There are two useful cases: the 'LGraph' is a single block
-- or it isn't.  We use continuation-passing style.

prepare_for_splicing ::
  LGraph m l -> (ZTail m l -> a) -> (ZTail m l -> ZHead m -> BlockEnv (Block m l) -> a)
  -> a
prepare_for_splicing g single multi =
  let FGraph _ gentry gblocks = entry g 
      ZBlock _ etail = gentry
  in if isNullBEnv gblocks then
         case last gentry of
           LastExit -> single etail
           _ -> panic "bad single block"
     else
       case splitp_blocks is_exit gblocks of
         Nothing -> panic "Can't find an exit block"
         Just (gexit, gblocks) ->
              let (gh, gl) = goto_end $ unzip gexit in
              case gl of LastExit -> multi etail gh gblocks
                         _ -> panic "exit is not exit?!"

prepare_for_splicing' ::
  Graph m l -> (ZTail m l -> a) -> (ZTail m l -> ZHead m -> BlockEnv (Block m l) -> a)
  -> a
prepare_for_splicing' (Graph etail gblocks) single multi =
   if isNullBEnv gblocks then
       case lastTail etail of
         LastExit -> single etail
         _ -> panic "bad single block"
   else
     case splitp_blocks is_exit gblocks of
       Nothing -> panic "Can't find an exit block"
       Just (gexit, gblocks) ->
            let (gh, gl) = goto_end $ unzip gexit in
            case gl of LastExit -> multi etail gh gblocks
                       _ -> panic "exit is not exit?!"

is_exit :: Block m l -> Bool
is_exit b = case last (unzip b) of { LastExit -> True; _ -> False }

splice_head head g@(LGraph _ _) = 
  ASSERT (single_exit g) prepare_for_splicing g splice_one_block splice_many_blocks
   where eid = head_id head
         splice_one_block tail' =
             case ht_to_last head tail' of
               (head, LastExit) -> (LGraph eid emptyBlockEnv, head)
               _ -> panic "spliced LGraph without exit" 
         splice_many_blocks entry exit others =
             (LGraph eid (insertBlock (zipht head entry) others), exit)

splice_head' head g = 
  ASSERT (single_exitg g) prepare_for_splicing' g splice_one_block splice_many_blocks
   where splice_one_block tail' = 
             case ht_to_last head tail' of
               (head, LastExit) -> (emptyBlockEnv, head)
               _ -> panic "spliced LGraph without exit" 
         splice_many_blocks entry exit others =
             (insertBlock (zipht head entry) others, exit)

-- splice_tail :: Graph m l -> ZTail m l -> Graph m l
splice_tail g tail =
  ASSERT (single_exitg g) prepare_for_splicing' g splice_one_block splice_many_blocks
    where splice_one_block tail' = Graph (tail' `append_tails` tail) emptyBlockEnv
          append_tails (ZLast LastExit) tail = tail
          append_tails (ZLast _) _ = panic "spliced single block without LastExit"
          append_tails (ZTail m t) tail = ZTail m (append_tails t tail)
          splice_many_blocks entry exit others =
              Graph entry (insertBlock (zipht exit tail) others)

{-
splice_tail g tail =
  AS SERT (single_exit g) prepare_for_splicing g splice_one_block splice_many_blocks
    where splice_one_block tail' =  -- return tail' .. tail 
            case ht_to_last (ZFirst (lg_entry g)) tail' of
              (head', LastExit) ->
                  case ht_to_block head' tail of
                     Block id t | id == lg_entry g -> (t, LGraph id emptyBlockEnv)
                     _ -> panic "entry in; garbage out"
              _ -> panic "spliced single block without Exit" 
          splice_many_blocks entry exit others =
              (entry, LGraph (lg_entry g) (insertBlock (zipht exit tail) others))
-}

splice_head_only head g =
  let FGraph eid gentry gblocks = entry g
  in case gentry of
       ZBlock (ZFirst _) tail ->
         LGraph eid (insertBlock (zipht head tail) gblocks)
       _ -> panic "entry not at start of block?!"

splice_head_only' head (Graph tail gblocks) =
  let eblock = zipht head tail in
  LGraph (blockId eblock) (insertBlock eblock gblocks)
  -- the offset probably should never be used, but well, it's correct for this LGraph


--- Translation

translate txm txl (LGraph eid blocks) =
    do blocks' <- foldBlockEnv' txblock (return emptyBlockEnv) blocks
       return $ LGraph eid blocks'
    where
      -- txblock ::
      -- Block m l -> tm (BlockEnv (Block m' l')) -> tm (BlockEnv (Block m' l'))
      txblock (Block id t) expanded =
        do blocks' <- expanded
           txtail (ZFirst id) t blocks'
      -- txtail :: ZHead m' -> ZTail m l -> BlockEnv (Block m' l') ->
      --           tm (BlockEnv (Block m' l'))
      txtail h (ZTail m t) blocks' =
        do m' <- txm m 
           let (g, h') = splice_head h m' 
           txtail h' t (plusBlockEnv (lg_blocks g) blocks')
      txtail h (ZLast (LastOther l)) blocks' =
        do l' <- txl l
           return $ plusBlockEnv (lg_blocks (splice_head_only h l')) blocks'
      txtail h (ZLast LastExit) blocks' =
        return $ insertBlock (zipht h (ZLast LastExit)) blocks'

----------------------------------------------------------------
---- Prettyprinting
----------------------------------------------------------------

-- putting this code in PprCmmZ leads to circular imports :-(

instance (Outputable m, Outputable l) => Outputable (ZTail m l) where
    ppr = pprTail

instance (Outputable m, Outputable l, LastNode l) => Outputable (Graph m l) where
    ppr = pprGraph

instance (Outputable m, Outputable l, LastNode l) => Outputable (LGraph m l) where
    ppr = pprLgraph

instance (Outputable m, Outputable l, LastNode l) => Outputable (Block m l) where
    ppr = pprBlock

instance (Outputable l) => Outputable (ZLast l) where
    ppr = pprLast

pprTail :: (Outputable m, Outputable l) => ZTail m l -> SDoc 
pprTail (ZTail m t) = ppr m $$ ppr t
pprTail (ZLast l) = ppr l

pprLast :: (Outputable l) => ZLast l -> SDoc
pprLast LastExit = text "<exit>"
pprLast (LastOther l) = ppr l

pprBlock :: (Outputable m, Outputable l, LastNode l) => Block m l -> SDoc
pprBlock (Block id tail) =
  ppr id <>  colon
         $$  (nest 3 (ppr tail))

pprLgraph :: (Outputable m, Outputable l, LastNode l) => LGraph m l -> SDoc
pprLgraph g = text "{" <> text "offset" $$
              nest 2 (vcat $ map ppr blocks) $$ text "}"
    where blocks = postorder_dfs g

pprGraph :: (Outputable m, Outputable l, LastNode l) => Graph m l -> SDoc
pprGraph (Graph tail blockenv) =
        text "{" $$ nest 2 (ppr tail $$ (vcat $ map ppr blocks)) $$ text "}"
    where blocks = postorder_dfs_from blockenv tail

