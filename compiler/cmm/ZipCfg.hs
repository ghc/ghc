{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module ZipCfg
    ( BlockId(..), freshBlockId
    , BlockEnv, emptyBlockEnv, lookupBlockEnv, extendBlockEnv, insertBlock, mkBlockEnv
    , BlockSet, emptyBlockSet, elemBlockSet, extendBlockSet, mkBlockSet
    , Graph(..), LGraph(..), FGraph(..)
    , Block(..), ZBlock(..), ZHead(..), ZTail(..), ZLast(..)
    , HavingSuccessors, succs, fold_succs
    , LastNode, mkBranchNode, isBranchNode, branchNodeTarget

        -- Observers and transformers
    , entry, exit, focus, focusp, unfocus
    , blockId, zip, unzip, last, goto_end, ht_to_first, ht_to_last, zipht
    , tailOfLast
    , splice_head, splice_tail, splice_head_only, splice_focus_entry
                 , splice_focus_exit, remove_entry_label
    , of_block_list, to_block_list
    , postorder_dfs
    , fold_layout, fold_blocks
    , fold_fwd_block, foldM_fwd_block
    , map_nodes, translate

    , pprLgraph
    )
where

import Maybes
import Outputable hiding (empty)
import Panic
import Prelude hiding (zip, unzip, last)
import Unique
import UniqFM
import UniqSet
import UniqSupply

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



This module exposes three representations of graphs.  In order of
increasing complexity, they are:

  Graph  m l      The basic graph with its distinguished entry point

  LGraph m l      A graph with a *labelled* entry point

  FGraph m l      A labelled graph with the *focus* on a particular edge

There are three types because each type offers a slightly different
invariant or cost model.  

  * The distinguished entry of a Graph has no label.  Because labels must
    be unique, acquiring one requires a monadic operation ('freshBlockId').
    The primary advantage of the Graph representation is that we can build
    a small Graph purely functionally, without entering a monad.  For
    example, during optimization we can easily rewrite a single middle
    node into a Graph containing a sequence of two middle nodes followed by
    LastExit.

  * In an LGraph, every basic block is labelled.  The primary advantage of
    this representation is its simplicity: each basic block can be treated
    like any other.  This representation is used for mapping, folding, and
    translation, as well as layout.

    Like any graph, an LGraph still has a distinguished entry point, 
    which you can discover using 'gr_entry'.

  * An FGraph is an LGraph with the *focus* on one particular edge.  The
    primary advantage of this representation is that it provides
    constant-time access to the nodes connected by that edge, and it also
    allows constant-time, functional *replacement* of those nodes---in the
    style of Huet's 'zipper'.

None of these representations is ideally suited to the incremental
construction of large graphs.  A separate module, 'MkZipCfg', provides a
fourth representation that is asymptotically optimal for such construction.
    
-}

entry   :: LGraph m l -> FGraph m l         -- focus on edge out of entry node 
exit    :: LGraph m l -> FGraph m l         -- focus on edge into default exit node 
                                            -- (fails if there isn't one)
focus   :: BlockId -> LGraph m l -> FGraph m l -- focus on edge out of node with id 
focusp  :: (Block m l -> Bool) -> LGraph m l -> Maybe (FGraph m l)
                                      -- focus on start of block satisfying predicate
unfocus :: FGraph m l -> LGraph m l            -- lose focus 

-- | We can insert a single-entry, single-exit subgraph at
-- the current focus.
-- The new focus can be at either the entry edge or the exit edge.

splice_focus_entry :: FGraph m l -> LGraph m l -> FGraph m l
splice_focus_exit  :: FGraph m l -> LGraph m l -> FGraph m l

--------------- Representation --------------------

-- | A basic block is a [[first]] node, followed by zero or more [[middle]]
-- nodes, followed by a [[last]] node.

-- eventually this module should probably replace the original Cmm, but for
-- now we leave it to dynamic invariants what can be found where

data ZLast l
  = LastExit     -- fall through; used for the block that has no last node
                 -- LastExit is a device used only for graphs under 
                 -- construction, or framgments of graph under optimisation,
                 -- so we don't want to pollute the 'l' type parameter with it
  | LastOther l

data ZHead m   = ZFirst BlockId  | ZHead (ZHead m) m
    -- ZHead is a (reversed) sequence of middle nodes labeled by a BlockId
data ZTail m l = ZLast (ZLast l) | ZTail m (ZTail m l)
    -- ZTail is a sequence of middle nodes followed by a last node

-- | Blocks and flow graphs
data Block m l = Block BlockId (ZTail m l)

data Graph m l = Graph (ZTail m l) (BlockEnv (Block m l))

data LGraph m l = LGraph  { gr_entry  :: BlockId
                          , gr_blocks :: BlockEnv (Block m l) }

-- | And now the zipper.  The focus is between the head and tail.
-- Notice we cannot ever focus on an inter-block edge.
data ZBlock m l = ZBlock (ZHead m) (ZTail m l)
data FGraph m l = FGraph { zg_entry  :: BlockId
                         , zg_focus  :: ZBlock m l
                         , zg_others :: BlockEnv (Block m l) }
                    -- Invariant: the block represented by 'zg_focus' is *not*
                    -- in the map 'zg_others'

----  Utility functions ---

blockId   :: Block  m l -> BlockId
zip       :: ZBlock m l -> Block m l
unzip     :: Block m l  -> ZBlock m l

last     :: ZBlock m l -> ZLast l
goto_end :: ZBlock m l -> (ZHead m, ZLast l)

tailOfLast :: l -> ZTail m l

-- | Some ways to combine parts:
ht_to_first :: ZHead m -> ZTail m l -> Block m l -- was (ZFirst, ZTail)
ht_to_last  :: ZHead m -> ZTail m l -> (ZHead m, ZLast l)

zipht       :: ZHead m -> ZTail m l -> Block m l

-- | We can splice a single-entry, single-exit LGraph onto a head or a tail.
-- For a head, we have a head~[[h]] followed by a LGraph~[[g]].
-- The entry node of~[[g]] gets joined to~[[h]], forming the entry into
-- the new LGraph.  The exit of~[[g]] becomes the new head.
-- For both arguments and results, the order of values is the order of
-- control flow: before splicing, the head flows into the LGraph; after
-- splicing, the LGraph flows into the head.
-- Splicing a tail is the dual operation.
-- (In order to maintain the order-means-control-flow convention, the
-- orders are reversed.)

splice_head :: ZHead m   -> LGraph m l -> (LGraph m l, ZHead m)
splice_tail :: LGraph m l -> ZTail m l -> (ZTail m l, LGraph m l)

-- | We can also splice a single-entry, no-exit LGraph into a head.
splice_head_only :: ZHead m -> LGraph m l -> LGraph m l

-- | Finally, we can remove the entry label of an LGraph and remove
-- it, leaving a Graph:
remove_entry_label :: LGraph m l -> Graph m l

of_block_list :: BlockId -> [Block m l] -> LGraph m l  -- N log N
to_block_list :: LGraph m l -> [Block m l]  -- N log N

-- | Traversal: [[postorder_dfs]] returns a list of blocks reachable from
-- the entry node.
-- The postorder depth-first-search order means the list is in roughly
-- first-to-last order, as suitable for use in a forward dataflow problem.

postorder_dfs :: LastNode l => LGraph m l -> [Block m l]

-- | For layout, we fold over pairs of [[Block m l]] and [[Maybe BlockId]] 
-- in layout order.  The [[BlockId]], if any, identifies the block that
-- will be the layout successor of the current block.  This may be
-- useful to help an emitter omit the final [[goto]] of a block that
-- flows directly to its layout successor.
fold_layout ::
    LastNode l => (Block m l -> Maybe BlockId -> a -> a) -> a -> LGraph m l-> a

-- | We can also fold and iterate over blocks.
fold_blocks :: (Block m l -> a -> a) -> a -> LGraph m l -> a

map_nodes :: (BlockId -> BlockId) -> (m -> m') -> (l -> l') -> LGraph m l -> LGraph m' l'
   -- mapping includes the entry id!
translate :: (m -> UniqSM (LGraph m' l')) -> (l -> UniqSM (LGraph m' l')) ->
             LGraph m l -> UniqSM (LGraph m' l')

{-
translateA :: (m -> Agraph m' l') -> (l -> AGraph m' l') -> LGraph m l -> LGraph m' l'
-}

------------------- Last nodes

-- | We can't make a graph out of just any old 'last node' type.  A
-- last node has to be able to find its successors, and we need to
-- be able to create and identify unconditional branches.  We put
-- these capabilities in a type class.

class HavingSuccessors b where
  succs :: b -> [BlockId]
  fold_succs :: (BlockId -> a -> a) -> b -> a -> a

  fold_succs add l z = foldr add z $ succs l

class HavingSuccessors l => LastNode l where
  mkBranchNode :: BlockId -> l
  isBranchNode :: l -> Bool
  branchNodeTarget :: l -> BlockId  -- panics if not branch node

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


------------------- Observing nodes

-- | Fold from first to last
fold_fwd_block ::
  (BlockId -> a -> a) -> (m -> a -> a) -> (ZLast l -> a -> a) ->
  Block m l -> a -> a

-- | iterate from first to last
foldM_fwd_block ::
  Monad m => (BlockId -> a -> m a) -> (mid -> a -> m a) -> (ZLast l -> a -> m a) ->
             Block mid l -> a -> m a

-- ================ IMPLEMENTATION ================--

blockId (Block id _) = id

-- | Convert block between forms.
-- These functions are tail-recursive, so we can go as deep as we like
-- without fear of stack overflow.  

ht_to_first head tail = case head of
  ZFirst id -> Block id tail
  ZHead h m -> ht_to_first h (ZTail m tail) 

head_id :: ZHead m -> BlockId
head_id (ZFirst id) = id
head_id (ZHead h _) = head_id h

zip (ZBlock h t) = ht_to_first h t

ht_to_last head (ZLast l)   = (head, l)
ht_to_last head (ZTail m t) = ht_to_last (ZHead head m) t 

goto_end (ZBlock h t) = ht_to_last h t

tailOfLast l = ZLast (LastOther l)

zipht = ht_to_first
unzip (Block id t) = ZBlock (ZFirst id) t

last (ZBlock _ t) = lastt t
  where lastt (ZLast l) = l
        lastt (ZTail _ t) = lastt t

focus id (LGraph entry blocks) =
    case lookupBlockEnv blocks id of
      Just b -> FGraph entry (unzip b) (delFromUFM blocks id)
      Nothing -> panic "asked for nonexistent block in flow graph"

focusp p (LGraph entry blocks) =
    fmap (\(b, bs) -> FGraph entry (unzip b) bs) (splitp_blocks p blocks)

splitp_blocks :: (Block m l -> Bool) -> BlockEnv (Block m l) ->
                 Maybe (Block m l, BlockEnv (Block m l))
splitp_blocks p blocks = lift $ foldUFM scan (Nothing, emptyBlockEnv) blocks 
    where scan b (yes, no) =
              case yes of
                Nothing | p b -> (Just b, no)
                        | otherwise -> (yes, insertBlock b no)
                Just _ -> (yes, insertBlock b no)
          lift (Nothing, _) = Nothing
          lift (Just b, bs) = Just (b, bs)

entry g@(LGraph eid _) = focus eid g

exit g@(LGraph eid _) = FGraph eid (ZBlock h (ZLast l)) others
    where FGraph _ b others = focusp is_exit g `orElse` panic "no exit in flow graph"
          (h, l) = goto_end b

is_exit :: Block m l -> Bool
is_exit b = case last (unzip b) of { LastExit -> True; _ -> False }

-- | 'insertBlock' should not be used to *replace* an existing block
-- but only to insert a new one
insertBlock :: Block m l -> BlockEnv (Block m l) -> BlockEnv (Block m l)
insertBlock b bs =
    case lookupBlockEnv bs id of
      Nothing -> extendBlockEnv bs id b
      Just _ -> panic ("duplicate labels " ++ show id ++ " in ZipCfg graph")
    where id = blockId b

unfocus (FGraph e bz bs) = LGraph e (insertBlock (zip bz) bs)

check_single_exit :: LGraph l m -> a -> a
check_single_exit g =
  let check block found = case last (unzip block) of
                            LastExit -> if found then panic "graph has multiple exits"
                                        else True
                            _ -> found
  in if not (foldUFM check False (gr_blocks g)) then
         panic "graph does not have an exit"
     else
         \a -> a

freshBlockId :: String -> UniqSM BlockId
freshBlockId _ = do { u <- getUniqueUs; return $ BlockId u }

postorder_dfs g@(LGraph _ blocks) =
  let FGraph _ eblock _ = entry g
  in  vnode (zip eblock) (\acc _visited -> acc) [] emptyBlockSet
  where
    -- vnode :: Block m l -> ([Block m l] -> BlockSet -> a) -> [Block m l] -> BlockSet ->a
    vnode block@(Block id _) cont acc visited =
        if elemBlockSet id visited then
            cont acc visited
        else
            vchildren block (get_children block) cont acc (extendBlockSet visited id)
    vchildren block bs cont acc visited =
        let next children acc visited =
                case children of []     -> cont (block : acc) visited
                                 (b:bs) -> vnode b (next bs) acc visited
        in next bs acc visited
    get_children block = foldl add_id [] (succs block)
    add_id rst id = case lookupBlockEnv blocks id of
                      Just b -> b : rst
                      Nothing -> rst

fold_layout f z g@(LGraph eid _) = fold (postorder_dfs g) z
  where fold blocks z =
            case blocks of [] -> z
                           [b] -> f b Nothing z
                           b1 : b2 : bs -> fold (b2 : bs) (f b1 (nextlabel b2) z)
        nextlabel (Block id _) =
            if id == eid then panic "entry as successor"
            else Just id

fold_fwd_block first middle last (Block id t) z = tail t (first id z)
    where tail (ZTail m t) z = tail t (middle m z)
          tail (ZLast l)   z = last l z

foldM_fwd_block first middle last (Block id t) z = do { z <- first id z; tail t z }
    where tail (ZTail m t) z = do { z <- middle m z; tail t z }
          tail (ZLast l)   z = last l z

fold_blocks f z (LGraph _ blocks) = foldUFM f z blocks

map_nodes idm middle last (LGraph eid blocks) = LGraph (idm eid) (mapUFM block blocks)
    where block (Block id t) = Block (idm id) (tail t)
          tail (ZTail m t) = ZTail (middle m) (tail t)
          tail (ZLast LastExit) = ZLast LastExit
          tail (ZLast (LastOther l)) = ZLast (LastOther (last l))

of_block_list e blocks = LGraph e $ foldr insertBlock emptyBlockEnv blocks 
to_block_list (LGraph _ blocks) = eltsUFM blocks

{-
\paragraph{Splicing support}

We want to be able to scrutinize a single-entry, single-exit LGraph for
splicing purposes. 
There are two useful cases: the LGraph is a single block or it isn't.
We use continuation-passing style.
-}

prepare_for_splicing ::
  LGraph m l -> (ZTail m l -> a) -> (ZTail m l -> ZHead m -> BlockEnv (Block m l) -> a)
  -> a
prepare_for_splicing g single multi =
  let FGraph _ gentry gblocks = entry g 
      ZBlock _ etail = gentry
  in if isNullUFM gblocks then
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

splice_head head g =
  check_single_exit g $
  let eid = head_id head
      splice_one_block tail' =
          case ht_to_last head tail' of
            (head, LastExit) -> (LGraph eid emptyBlockEnv, head)
            _ -> panic "spliced LGraph without exit" 
      splice_many_blocks entry exit others =
          (LGraph eid (insertBlock (zipht head entry) others), exit)
  in  prepare_for_splicing g splice_one_block splice_many_blocks

splice_tail g tail =
  check_single_exit g $
  let splice_one_block tail' =  -- return tail' .. tail 
        case ht_to_last (ZFirst (gr_entry g)) tail' of
          (head', LastExit) ->
              case ht_to_first head' tail of
                 Block id t | id == gr_entry g -> (t, LGraph id emptyBlockEnv)
                 _ -> panic "entry in; garbage out"
          _ -> panic "spliced single block without Exit" 
      splice_many_blocks entry exit others =
         (entry, LGraph (gr_entry g) (insertBlock (zipht exit tail) others))
  in  prepare_for_splicing g splice_one_block splice_many_blocks

splice_focus_entry (FGraph eid (ZBlock head tail) blocks) g =
  let (tail', g') = splice_tail g tail in
  FGraph eid (ZBlock head tail') (plusUFM (gr_blocks g') blocks)

splice_focus_exit (FGraph eid (ZBlock head tail) blocks) g =
  let (g', head') = splice_head head g in
  FGraph eid (ZBlock head' tail) (plusUFM (gr_blocks g') blocks)

splice_head_only head g =
  let FGraph eid gentry gblocks = entry g
  in case gentry of
       ZBlock (ZFirst _) tail -> LGraph eid (insertBlock (zipht head tail) gblocks)
       _ -> panic "entry not at start of block?!"

remove_entry_label g =
    let FGraph e eblock others = entry g
    in case eblock of
         ZBlock (ZFirst id) tail
             | id == e -> Graph tail others
         _ -> panic "id doesn't match on entry block"

--- Translation

translate txm txl (LGraph eid blocks) =
    do blocks' <- foldUFM txblock (return emptyBlockEnv) blocks
       return $ LGraph eid blocks'
    where
      -- txblock ::
      -- Block m l -> UniqSM (BlockEnv (Block m' l')) -> UniqSM (BlockEnv (Block m' l'))
      txblock (Block id t) expanded =
        do blocks' <- expanded
           txtail (ZFirst id) t blocks'
      -- txtail :: ZHead m' -> ZTail m l -> BlockEnv (Block m' l') ->
      --           UniqSM (BlockEnv (Block m' l'))
      txtail h (ZTail m t) blocks' =
        do m' <- txm m 
           let (g, h') = splice_head h m' 
           txtail h' t (plusUFM (gr_blocks g) blocks')
      txtail h (ZLast (LastOther l)) blocks' =
        do l' <- txl l
           return $ plusUFM (gr_blocks (splice_head_only h l')) blocks'
      txtail h (ZLast LastExit) blocks' =
        return $ insertBlock (zipht h (ZLast LastExit)) blocks'

----------------------------------------------------------------
--- Block Ids, their environments, and their sets

{- Note [Unique BlockId]
~~~~~~~~~~~~~~~~~~~~~~~~
Although a 'BlockId' is a local label, for reasons of implementation,
'BlockId's must be unique within an entire compilation unit.  The reason
is that each local label is mapped to an assembly-language label, and in
most assembly languages allow, a label is visible throughout the enitre
compilation unit in which it appears.
-}

newtype BlockId = BlockId Unique
  deriving (Eq,Ord)

instance Uniquable BlockId where
  getUnique (BlockId u) = u

instance Show BlockId where
  show (BlockId u) = show u

instance Outputable BlockId where
  ppr = ppr . getUnique


type BlockEnv a = UniqFM {- BlockId -} a
emptyBlockEnv :: BlockEnv a
emptyBlockEnv = emptyUFM
lookupBlockEnv :: BlockEnv a -> BlockId -> Maybe a
lookupBlockEnv = lookupUFM
extendBlockEnv :: BlockEnv a -> BlockId -> a -> BlockEnv a
extendBlockEnv = addToUFM
mkBlockEnv :: [(BlockId,a)] -> BlockEnv a
mkBlockEnv = listToUFM

type BlockSet = UniqSet BlockId
emptyBlockSet :: BlockSet
emptyBlockSet = emptyUniqSet
elemBlockSet :: BlockId -> BlockSet -> Bool
elemBlockSet = elementOfUniqSet
extendBlockSet :: BlockSet -> BlockId -> BlockSet
extendBlockSet = addOneToUniqSet
mkBlockSet :: [BlockId] -> BlockSet
mkBlockSet = mkUniqSet

----------------------------------------------------------------
-- putting this code in PprCmmZ leads to circular imports :-(

instance (Outputable m, Outputable l) => Outputable (ZTail m l) where
    ppr = pprTail

-- | 'pprTail' is used for debugging only
pprTail :: (Outputable m, Outputable l) => ZTail m l -> SDoc 
pprTail (ZTail m t) = ppr m $$ ppr t
pprTail (ZLast LastExit) = text "<exit>"
pprTail (ZLast (LastOther l)) = ppr l

pprLgraph :: (Outputable m, Outputable l, LastNode l) => LGraph m l -> SDoc
pprLgraph g = text "{" $$ nest 2 (vcat $ map pprBlock blocks) $$ text "}"
    where pprBlock (Block id tail) = ppr id <> colon $$ ppr tail
          blocks = postorder_dfs g
