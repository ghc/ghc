{-# LANGUAGE GADTs, BangPatterns, RecordWildCards,
    GeneralizedNewtypeDeriving, NondecreasingIndentation #-}

module CmmBuildInfoTables
  ( CAFSet, CAFEnv, cafAnal
  , doSRTs, ModuleSRTInfo, emptySRT
  ) where

import GhcPrelude hiding (succ)

import BlockId
import Hoopl.Block
import Hoopl.Graph
import Hoopl.Label
import Hoopl.Collections
import Hoopl.Dataflow
import Module
import Digraph
import CLabel
import PprCmmDecl ()
import Cmm
import CmmUtils
import DynFlags
import Maybes
import Outputable
import SMRep
import UniqSupply
import CostCentre
import StgCmmHeap

import PprCmm()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class


{- Note [SRTs]

SRTs are the mechanism by which the garbage collector can determine
the live CAFs in the program.

Representation
^^^^^^^^^^^^^^

+------+
| info |
|      |     +-----+---+---+---+
|   -------->|SRT_2| | | | | 0 |
|------|     +-----+-|-+-|-+---+
|      |             |   |
| code |             |   |
|      |             v   v

An SRT is simply an object in the program's data segment. It has the
same representation as a static constructor.  There are 16
pre-compiled SRT info tables: stg_SRT_1_info, .. stg_SRT_16_info,
representing SRT objects with 1-16 pointers, respectively.

The entries of an SRT object point to static closures, which are either
- FUN_STATIC, THUNK_STATIC or CONSTR
- Another SRT (actually just a CONSTR)

The final field of the SRT is the static link field, used by the
garbage collector to chain together static closures that it visits and
to determine whether a static closure has been visited or not. (see
Note [STATIC_LINK fields])

By traversing the transitive closure of an SRT, the GC will reach all
of the CAFs that are reachable from the code associated with this SRT.

If we need to create an SRT with more than 16 entries, we build a
chain of SRT objects with all but the last having 16 entries.

+-----+---+- -+---+---+
|SRT16| | |   | | | 0 |
+-----+-|-+- -+-|-+---+
        |       |
        v       v
              +----+---+---+---+
              |SRT2| | | | | 0 |
              +----+-|-+-|-+---+
                     |   |
                     |   |
                     v   v

Referring to an SRT from the info table
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following things have SRTs:

- Static functions (FUN)
- Static thunks (THUNK), ie. CAFs
- Continuations (RET_SMALL, etc.)

In each case, the info table points to the SRT.

- info->srt is zero if there's no SRT, otherwise:
- info->srt == 1 and info->f.srt_offset points to the SRT

(but see TODO below, we can improve this)

e.g. for a FUN with an SRT:

StgFunInfoTable       +------+
  info->f.srt_offset  |  ------------> offset to SRT object
StgStdInfoTable       +------+
  info->layout.ptrs   | ...  |
  info->layout.nptrs  | ...  |
  info->srt           |  1   |
  info->type          | ...  |
                      |------|


EXAMPLE
^^^^^^^

f = \x. ... g ...
  where
    g = \y. ... h ... c1 ...
    h = \z. ... c2 ...

c1 & c2 are CAFs

g and h are local functions, but they have no static closures.  When
we generate code for f, we start with a CmmGroup of four CmmDecls:

   [ f_closure, f_entry, g_entry, h_entry ]

we process each CmmDecl separately in cpsTop, giving us a list of
CmmDecls. e.g. for f_entry, we might end up with

   [ f_entry, f1_ret, f2_proc ]

where f1_ret is a return point, and f2_proc is a proc-point.  We have
a CAFSet for each of these CmmDecls, let's suppose they are

   [ f_entry{g_info}, f1_ret{g_info}, f2_proc{} ]
   [ g_entry{h_info, c1_closure} ]
   [ h_entry{c2_closure} ]

Next, we make an SRT for each of these functions:

  f_srt : [g_info]
  g_srt : [h_info, c1_closure]
  h_srt : [c2_closure]

Now, for g_info and h_info, we want to refer to the SRTs for g and h
respectively, which we'll label g_srt and h_srt:

  f_srt : [g_srt]
  g_srt : [h_srt, c1_closure]
  h_srt : [c2_closure]

Now, when an SRT has a single entry, we don't actually generate an SRT
closure for it, instead we just replace references to it with its
single element.  So, since h_srt == c2_closure, we have

  f_srt : [g_srt]
  g_srt : [c2_closure, c1_closure]
  h_srt : [c2_closure]

and the only SRT closure we generate is

  g_srt = SRT_2 [c2_closure, c1_closure]


Optimisations
^^^^^^^^^^^^^

To reduce the code size overhead and the cost of traversing SRTs in
the GC, we want to simplify SRTs where possible. We therefore apply
the following optimisations.  Each has a [keyword]; search for the
keyword in the code below to see where the optimisation is
implemented.

1. [Shortcut] we never create an SRT with a single entry, instead
   we replace all references to the singleton SRT with a reference
   to its element.  This includes references from info tables.

   i.e. instead of

    +------+
    | info |
    |      |     +-----+---+---+
    |   -------->|SRT_1| | | 0 |
    |------|     +-----+-|-+---+
    |      |             |
    | code |             |
    |      |             v
                      closure

   we can point directly to the closure:

    +------+
    | info |
    |      |
    |   -------->closure
    |------|
    |      |
    | code |
    |      |


   The exception to this is when we're doing dynamic linking. In that
   case, if the closure is not locally defined then we can't point to
   it directly from the info table, because this is the text section
   which cannot contain runtime relocations. In this case we skip this
   optimisation and generate the singleton SRT, becase SRTs are in the
   data section and *can* have relocatable references.

2. [FUN] If an SRT refers to a top-level function (a FUN_STATIC), then
   we can shortcut the reference to point directly to the function's
   SRT instead.

   i.e. instead of

   +---+---+---
   |SRT| | |
   +---+-|-+---
         |
         v
       +---+---+
       | | | 0 |
       +-|-+---+
         |
         |      +------+
         |      | info |
         |      |      |     +-----+---+---+
         |      |   -------->|SRT_1| | | 0 |
         `----->|------|     +-----+-|-+---+
                |      |             |
                | code |             |
                |      |             v
                                  closure

   we can generate

   +---+---+---
   |SRT| | |
   +---+-|-+---
         `----------------------,
                                |
       +---+---+                |
       | | | 0 |                |
       +-|-+---+                |
         |                      |
         |      +------+        |
         |      | info |        v
         |      |      |     +-----+---+---+
         |      |   -------->|SRT_1| | | 0 |
         `----->|------|     +-----+-|-+---+
                |      |             |
                | code |             |
                |      |             v
                                  closure

   This is quicker for the garbage collector to traverse, and avoids
   setting the static link field on the function's closure.

   Of course we can only do this if we know what the function's SRT
   is. Due to [Shortcut] the function's SRT can be an arbitrary
   closure, so this optimisation only applies within a module.

   Note: we can *not* do this optimisation for top-level thunks
   (CAFs), because we want the SRT to point directly to the
   CAF. Otherwise the SRT would keep the CAF's static references alive
   even after the CAF had been evaluated!

3. [Common] Identical SRTs can be commoned up.

4. [Filter] If an SRT A refers to an SRT B and a closure C, and B also
   refers to C (perhaps transitively), then we can omit the reference
   to C from A.


As an alternative to [FUN]: we could merge the FUN's SRT with the FUN
object itself.

TODO: make info->srt be an offset to the SRT, or zero if none (save
one word per info table that has an SRT)

Note that there are many other optimisations that we could do, but
aren't implemented. In general, we could omit any reference from an
SRT if everything reachable from it is also reachable from the other
fields in the SRT. Our [Filter] optimisation is a special case of
this.

Another opportunity we don't exploit is this:

A = {X,Y,Z}
B = {Y,Z}
C = {X,B}

Here we could use C = {A} and therefore [Shortcut] C = A.

-}

-- ---------------------------------------------------------------------
-- Label types

-- Labels that come from cafAnal can be:
--   - _closure labels for static functions or CAFs
--   - _info labels for dynamic functions, thunks, or continuations
--   - _entry labels for functions or thunks
--
-- Meanwhile the labels on top-level blocks are _entry labels.
--
-- To put everything in the same namespace we convert all labels to
-- closure labels using toClosureLbl.  Note that some of these
-- labels will not actually exist; that's ok because we're going to
-- map them to SRTEntry later, which ranges over labels that do exist.
--
newtype CAFLabel = CAFLabel CLabel
  deriving (Eq,Ord,Outputable)

type CAFSet = Set CAFLabel
type CAFEnv = LabelMap CAFSet

mkCAFLabel :: CLabel -> CAFLabel
mkCAFLabel lbl = CAFLabel (toClosureLbl lbl)

-- This is a label that we can put in an SRT.  It *must* be a closure label,
-- pointing to either a FUN_STATIC, THUNK_STATIC, or CONSTR.
newtype SRTEntry = SRTEntry CLabel
  deriving (Eq, Ord, Outputable)

-- ---------------------------------------------------------------------
-- CAF analysis

-- |
-- For each code block:
--   - collect the references reachable from this code block to FUN,
--     THUNK or RET labels for which hasCAF == True
--
-- This gives us a `CAFEnv`: a mapping from code block to sets of labels
--
cafAnal
  :: LabelSet   -- The blocks representing continuations, ie. those
                -- that will get RET info tables.  These labels will
                -- get their own SRTs, so we don't aggregate CAFs from
                -- references to these labels, we just use the label.
  -> CLabel     -- The top label of the proc
  -> CmmGraph
  -> CAFEnv
cafAnal contLbls topLbl cmmGraph =
  analyzeCmmBwd cafLattice
    (cafTransfers contLbls (g_entry cmmGraph) topLbl) cmmGraph mapEmpty


cafLattice :: DataflowLattice CAFSet
cafLattice = DataflowLattice Set.empty add
  where
    add (OldFact old) (NewFact new) =
        let !new' = old `Set.union` new
        in changedIf (Set.size new' > Set.size old) new'


cafTransfers :: LabelSet -> Label -> CLabel -> TransferFun CAFSet
cafTransfers contLbls entry topLbl
  (BlockCC eNode middle xNode) fBase =
    let joined = cafsInNode xNode $! live'
        !result = foldNodesBwdOO cafsInNode middle joined

        facts = mapMaybe successorFact (successors xNode)
        live' = joinFacts cafLattice facts

        successorFact s
          -- If this is a loop back to the entry, we can refer to the
          -- entry label.
          | s == entry = Just (add topLbl Set.empty)
          -- If this is a continuation, we want to refer to the
          -- SRT for the continuation's info table
          | s `setMember` contLbls
          = Just (Set.singleton (mkCAFLabel (infoTblLbl s)))
          -- Otherwise, takes the CAF references from the destination
          | otherwise
          = lookupFact s fBase

        cafsInNode :: CmmNode e x -> CAFSet -> CAFSet
        cafsInNode node set = foldExpDeep addCaf node set

        addCaf expr !set =
          case expr of
              CmmLit (CmmLabel c) -> add c set
              CmmLit (CmmLabelOff c _) -> add c set
              CmmLit (CmmLabelDiffOff c1 c2 _ _) -> add c1 $! add c2 set
              _ -> set
        add l s | hasCAF l  = Set.insert (mkCAFLabel l) s
                | otherwise = s

    in mapSingleton (entryLabel eNode) result


-- -----------------------------------------------------------------------------
-- ModuleSRTInfo

data ModuleSRTInfo = ModuleSRTInfo
  { thisModule :: Module
    -- ^ Current module being compiled. Required for calling labelDynamic.
  , dedupSRTs :: Map (Set SRTEntry) SRTEntry
    -- ^ previous SRTs we've emitted, so we can de-duplicate.
    -- Used to implement the [Common] optimisation.
  , flatSRTs :: Map SRTEntry (Set SRTEntry)
    -- ^ The reverse mapping, so that we can remove redundant
    -- entries. e.g.  if we have an SRT [a,b,c], and we know that b
    -- points to [c,d], we can omit c and emit [a,b].
    -- Used to implement the [Filter] optimisation.
  }
instance Outputable ModuleSRTInfo where
  ppr ModuleSRTInfo{..} =
    text "ModuleSRTInfo:" <+> ppr dedupSRTs <+> ppr flatSRTs

emptySRT :: Module -> ModuleSRTInfo
emptySRT mod =
  ModuleSRTInfo
    { thisModule = mod
    , dedupSRTs = Map.empty
    , flatSRTs = Map.empty }

-- -----------------------------------------------------------------------------
-- Constructing SRTs

{- Implementation notes

- In each CmmDecl there is a mapping info_tbls from Label -> CmmInfoTable

- The entry in info_tbls corresponding to g_entry is the closure info
  table, the rest are continuations.

- Each entry in info_tbls possibly needs an SRT.  We need to make a
  label for each of these.

- We get the CAFSet for each entry from the CAFEnv

-}

-- | Return a (Label,CLabel) pair for each labelled block of a CmmDecl,
--   where the label is
--   - the info label for a continuation or dynamic closure
--   - the closure label for a top-level function (not a CAF)
getLabelledBlocks :: CmmDecl -> [(Label, CAFLabel)]
getLabelledBlocks (CmmData _ _) = []
getLabelledBlocks (CmmProc top_info _ _ _) =
  [ (blockId, mkCAFLabel (cit_lbl info))
  | (blockId, info) <- mapToList (info_tbls top_info)
  , let rep = cit_rep info
  , not (isStaticRep rep) || not (isThunkRep rep)
  ]


-- | Get (Label,CLabel) pairs for each block that represents a CAF.
-- These are treated differently from other labelled blocks:
--  - we never resolve a reference to a CAF to the contents of its SRT, since
--    the point of SRTs is to keep CAFs alive.
--  - CAFs therefore don't take part in the dependency analysis in depAnalSRTs.
--    instead we generate their SRTs after everything else, so that we can
--    resolve references in the CAF's SRT.
getCAFs :: CmmDecl -> [(Label, CAFLabel)]
getCAFs (CmmData _ _) = []
getCAFs (CmmProc top_info topLbl _ g)
  | Just info <- mapLookup (g_entry g) (info_tbls top_info)
  , let rep = cit_rep info
  , isStaticRep rep && isThunkRep rep = [(g_entry g, mkCAFLabel topLbl)]
  | otherwise = []


-- | Put the labelled blocks that we will be annotating with SRTs into
-- dependency order.  This is so that we can process them one at a
-- time, resolving references to earlier blocks to point to their
-- SRTs.
depAnalSRTs
  :: CAFEnv
  -> [CmmDecl]
  -> [SCC (Label, CAFLabel, Set CAFLabel)]

depAnalSRTs cafEnv decls =
  srtTrace "depAnalSRTs" (ppr blockToLabel $$ ppr (graph ++ cafSCCs)) $
  (graph ++ cafSCCs)
 where
  cafs = concatMap getCAFs decls
  cafSCCs = [ AcyclicSCC (blockid, lbl, cafs)
            | (blockid, lbl) <- cafs
            , Just cafs <- [mapLookup blockid cafEnv] ]
  labelledBlocks = concatMap getLabelledBlocks decls
  blockToLabel :: LabelMap CAFLabel
  blockToLabel = mapFromList (cafs ++ labelledBlocks)
  labelToBlock = Map.fromList (map swap labelledBlocks)
  graph = stronglyConnCompFromEdgedVerticesOrd
             [ let cafs' = Set.delete lbl cafs in
               DigraphNode (l,lbl,cafs') l
                 (mapMaybe (flip Map.lookup labelToBlock) (Set.toList cafs'))
             | (l, lbl) <- labelledBlocks
             , Just cafs <- [mapLookup l cafEnv] ]


-- | Maps labels from 'cafAnal' to the final CLabel that will appear
-- in the SRT.
--   - closures with singleton SRTs resolve to their single entry
--   - closures with larger SRTs map to the label for that SRT
--   - CAFs must not map to anything!
--   - if a labels maps to Nothing, we found that this label's SRT
--     is empty, so we don't need to refer to it from other SRTs.
type SRTMap = Map CAFLabel (Maybe SRTEntry)

-- | resolve a CAFLabel to its SRTEntry using the SRTMap
resolveCAF :: SRTMap -> CAFLabel -> Maybe SRTEntry
resolveCAF srtMap lbl@(CAFLabel l) =
  Map.findWithDefault (Just (SRTEntry (toClosureLbl l))) lbl srtMap


-- | Attach SRTs to all info tables in the CmmDecls, and add SRT
-- declarations to the ModuleSRTInfo.
--
doSRTs
  :: DynFlags
  -> ModuleSRTInfo
  -> [(CAFEnv, [CmmDecl])]
  -> IO (ModuleSRTInfo, [CmmDecl])

doSRTs dflags topSRT tops = do
  us <- mkSplitUniqSupply 'u'

  -- Ignore the original grouping of decls, and combine all the
  -- CAFEnvs into a single CAFEnv.
  let (cafEnvs, declss) = unzip tops
      cafEnv = mapUnions cafEnvs
      decls = concat declss

  -- Put the decls in dependency order. Why? So that we can implement
  -- [Shortcut] and [Filter].  If we need to refer to an SRT that has
  -- a single entry, we use the entry itself, which means that we
  -- don't need to generate the singleton SRT in the first place.  But
  -- to do this we need to process blocks before things that depend on
  -- them.
  let sccs = depAnalSRTs cafEnv decls

  -- On each strongly-connected group of decls, construct the SRT
  -- closures and the SRT fields for info tables.
  let (((declss, pairs), _srtMap), topSRT') =
        initUs_ us $
        flip runStateT topSRT $
        flip runStateT Map.empty $
        mapAndUnzipM (doSCC dflags) sccs

  -- Next, update the info tables with the SRTs
  let decls' = map (updInfoSRTs (mapFromList (concat pairs))) decls

  return (topSRT', concat declss ++ decls')


-- | Build the SRT for a strongly-connected component of blocks
doSCC
  :: DynFlags
  -> SCC (Label, CAFLabel, Set CAFLabel)
  -> StateT SRTMap
        (StateT ModuleSRTInfo UniqSM)
        ( [CmmDecl]           -- generated SRTs
        , [(Label, CLabel)] -- SRT fields for info tables
        )

doSCC dflags  (AcyclicSCC (l, cafLbl, cafs)) =
  oneSRT dflags [l] [cafLbl] cafs

doSCC dflags (CyclicSCC nodes) = do
  -- build a single SRT for the whole cycle
  let (blockids, lbls, cafsets) = unzip3 nodes
      cafs = Set.unions cafsets `Set.difference` Set.fromList lbls
  oneSRT dflags blockids lbls cafs


-- | Build an SRT for a set of blocks
oneSRT
  :: DynFlags
  -> [Label]                    -- blocks in this set
  -> [CAFLabel]                 -- labels for those blocks
  -> Set CAFLabel               -- SRT for this set
  -> StateT SRTMap
       (StateT ModuleSRTInfo UniqSM)
       ( [CmmDecl]                    -- SRT objects we built
       , [(Label, CLabel)]            -- SRT fields for these blocks' itbls
       )

oneSRT dflags blockids lbls cafs = do
  srtMap <- get
  topSRT <- lift get
  let
    -- First resolve all the CAFLabels to SRTEntries
    -- implements the [Shortcut] optimisation.
    resolved =
       Set.fromList $
       catMaybes (map (resolveCAF srtMap) (Set.toList cafs))

    -- The set of all SRTEntries in SRTs that we refer to from here.
    allBelow =
      Set.unions [ lbls | caf <- Set.toList resolved
                        , Just lbls <- [Map.lookup caf (flatSRTs topSRT)] ]

    -- Remove SRTEntries that are also in an SRT that we refer to.
    -- Implements the [Filter] optimisation.
    filtered = Set.difference resolved allBelow

  srtTrace "oneSRT:"
     (ppr cafs <+> ppr resolved <+> ppr allBelow <+> ppr filtered) $ return ()

  let
    updateSRTMap srtEntry = do
      let newSRTMap = Map.fromList [(cafLbl, srtEntry) | cafLbl <- lbls]
      put (Map.union newSRTMap srtMap)

  case Set.toList filtered of
    [] -> do
      srtTrace "oneSRT: empty" (ppr lbls) $ return ()
      updateSRTMap Nothing
      return ([], [])

    [one@(SRTEntry lbl)]
      | not (labelDynamic dflags (thisModule topSRT) lbl) -> do
        updateSRTMap (Just one)
        return ([], [(l, lbl) | l <- blockids])

    cafList ->
      -- Check whether an SRT with the same entries has been emitted already.
      -- Implements the [Common] optimisation.
      case Map.lookup filtered (dedupSRTs topSRT) of
        Just srtEntry@(SRTEntry srtLbl)  -> do
          srtTrace "oneSRT [Common]" (ppr lbls <+> ppr srtLbl) $ return ()
          updateSRTMap (Just srtEntry)
          return ([], [(l, srtLbl) | l <- blockids])
        Nothing -> do
          -- No duplicates: we have to build a new SRT object
          srtTrace "oneSRT: new" (ppr lbls <+> ppr filtered) $ return ()
          (decls, srtEntry) <- lift . lift $ buildSRTChain dflags cafList
          updateSRTMap (Just srtEntry)
          let allBelowThis = Set.union allBelow filtered
              oldFlatSRTs = flatSRTs topSRT
              newFlatSRTs = Map.insert srtEntry allBelowThis oldFlatSRTs
              newDedupSRTs = Map.insert filtered srtEntry (dedupSRTs topSRT)
          lift (put (topSRT { dedupSRTs = newDedupSRTs
                            , flatSRTs = newFlatSRTs }))
          let SRTEntry lbl = srtEntry
          return (decls, [(l, lbl) | l <- blockids])


-- | build a static SRT object (or a chain of objects) from a list of
-- SRTEntries.
buildSRTChain
   :: DynFlags
   -> [SRTEntry]
   -> UniqSM
        ( [CmmDecl]    -- The SRT object(s)
        , SRTEntry     -- label to use in the info table
        )
buildSRTChain _ [] = panic "buildSRT: empty"
buildSRTChain dflags cafSet =
  case splitAt mAX_SRT_SIZE cafSet of
    (these, []) -> do
      (decl,lbl) <- buildSRT dflags these
      return ([decl], lbl)
    (these,those) -> do
      (rest, rest_lbl) <- buildSRTChain dflags (head these : those)
      (decl,lbl) <- buildSRT dflags (rest_lbl : tail these)
      return (decl:rest, lbl)
  where
    mAX_SRT_SIZE = 16


buildSRT :: DynFlags -> [SRTEntry] -> UniqSM (CmmDecl, SRTEntry)
buildSRT dflags refs = do
  id <- getUniqueM
  let
    lbl = mkSRTLabel id
    srt_n_info = mkSRTInfoLabel (length refs)
    fields =
      mkStaticClosure dflags srt_n_info dontCareCCS
        [ CmmLabel lbl | SRTEntry lbl <- refs ]
        [] -- no padding
        [mkIntCLit dflags 0] -- link field
        [] -- no saved info
  return (mkDataLits (Section Data lbl) lbl fields, SRTEntry lbl)


{- Note [reverse gs]

   It is important to keep the code blocks in the same order,
   otherwise binary sizes get slightly bigger.  I'm not completely
   sure why this is, perhaps the assembler generates bigger jump
   instructions for forward refs.  --SDM
-}

updInfoSRTs :: LabelMap CLabel -> CmmDecl -> CmmDecl
updInfoSRTs srt_env (CmmProc top_info top_l live g) =
  CmmProc (top_info {info_tbls = mapMapWithKey updInfoTbl (info_tbls top_info)}) top_l live g
  where updInfoTbl l info_tbl
             = info_tbl { cit_srt = mapLookup l srt_env }
updInfoSRTs _ t = t


srtTrace :: String -> SDoc -> b -> b
srtTrace _ _ b = b
