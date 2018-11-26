{-# LANGUAGE GADTs, BangPatterns, RecordWildCards,
    GeneralizedNewtypeDeriving, NondecreasingIndentation, TupleSections #-}

module CmmBuildInfoTables
  ( CAFSet, CAFEnv, cafAnal
  , doSRTs, ModuleSRTInfo, emptySRT
  ) where

import GhcPrelude hiding (succ)

import Id
import BlockId
import Hoopl.Block
import Hoopl.Graph
import Hoopl.Label
import Hoopl.Collections
import Hoopl.Dataflow
import Module
import Platform
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
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple
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

e.g. for a FUN with an SRT:

StgFunInfoTable       +------+
  info->f.srt_offset  |  ------------> offset to SRT object
StgStdInfoTable       +------+
  info->layout.ptrs   | ...  |
  info->layout.nptrs  | ...  |
  info->srt           |  1   |
  info->type          | ...  |
                      |------|

On x86_64, we optimise the info table representation further.  The
offset to the SRT can be stored in 32 bits (all code lives within a
2GB region in x86_64's small memory model), so we can save a word in
the info table by storing the srt_offset in the srt field, which is
half a word.

On x86_64 with TABLES_NEXT_TO_CODE (except on MachO, due to #15169):

- info->srt is zero if there's no SRT, otherwise:
- info->srt is an offset from the info pointer to the SRT object

StgStdInfoTable       +------+
  info->layout.ptrs   |      |
  info->layout.nptrs  |      |
  info->srt           |  ------------> offset to SRT object
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

1. [Inline] we never create an SRT with a single entry, instead we
   point to the single entry directly from the info table.

   i.e. instead of

    +------+
    | info |
    |      |     +-----+---+---+
    |   -------->|SRT_1| | | 0 |
    |------|     +-----+-|-+---+
    |      |             |
    | code |             |
    |      |             v
                         C

   we can point directly to the closure:

    +------+
    | info |
    |      |
    |   -------->C
    |------|
    |      |
    | code |
    |      |


   Furthermore, the SRT for any code that refers to this info table
   can point directly to C.

   The exception to this is when we're doing dynamic linking. In that
   case, if the closure is not locally defined then we can't point to
   it directly from the info table, because this is the text section
   which cannot contain runtime relocations. In this case we skip this
   optimisation and generate the singleton SRT, becase SRTs are in the
   data section and *can* have relocatable references.

2. [FUN] A static function closure can also be an SRT, we simply put
   the SRT entries as fields in the static closure.  This makes a lot
   of sense: the static references are just like the free variables of
   the FUN closure.

   i.e. instead of

   f_closure:
   +-----+---+
   |  |  | 0 |
   +- |--+---+
      |            +------+
      |            | info |     f_srt:
      |            |      |     +-----+---+---+---+
      |            |   -------->|SRT_2| | | | + 0 |
      `----------->|------|     +-----+-|-+-|-+---+
                   |      |             |   |
                   | code |             |   |
                   |      |             v   v


   We can generate:

   f_closure:
   +-----+---+---+---+
   |  |  | | | | | 0 |
   +- |--+-|-+-|-+---+
      |    |   |   +------+
      |    v   v   | info |
      |            |      |
      |            |   0  |
      `----------->|------|
                   |      |
                   | code |
                   |      |


   (note: we can't do this for THUNKs, because the thunk gets
   overwritten when it is entered, so we wouldn't be able to share
   this SRT with other info tables that want to refer to it (see
   [Common] below). FUNs are immutable so don't have this problem.)

3. [Common] Identical SRTs can be commoned up.

4. [Filter] If an SRT A refers to an SRT B and a closure C, and B also
   refers to C (perhaps transitively), then we can omit the reference
   to C from A.


Note that there are many other optimisations that we could do, but
aren't implemented. In general, we could omit any reference from an
SRT if everything reachable from it is also reachable from the other
fields in the SRT. Our [Filter] optimisation is a special case of
this.

Another opportunity we don't exploit is this:

A = {X,Y,Z}
B = {Y,Z}
C = {X,B}

Here we could use C = {A} and therefore [Inline] C = A.
-}

-- ---------------------------------------------------------------------
{- Note [Invalid optimisation: shortcutting]

You might think that if we have something like

A's SRT = {B}
B's SRT = {X}

that we could replace the reference to B in A's SRT with X.

A's SRT = {X}
B's SRT = {X}

and thereby perhaps save a little work at runtime, because we don't
have to visit B.

But this is NOT valid.

Consider these cases:

0. B can't be a constructor, because constructors don't have SRTs

1. B is a CAF. This is the easy one. Obviously we want A's SRT to
   point to B, so that it keeps B alive.

2. B is a function.  This is the tricky one. The reason we can't
shortcut in this case is that we aren't allowed to resurrect static
objects.

== How does this cause a problem? ==

The particular case that cropped up when we tried this was #15544.
- A is a thunk
- B is a static function
- X is a CAF
- suppose we GC when A is alive, and B is not otherwise reachable.
- B is "collected", meaning that it doesn't make it onto the static
  objects list during this GC, but nothing bad happens yet.
- Next, suppose we enter A, and then call B. (remember that A refers to B)
  At the entry point to B, we GC. This puts B on the stack, as part of the
  RET_FUN stack frame that gets pushed when we GC at a function entry point.
- This GC will now reach B
- But because B was previous "collected", it breaks the assumption
  that static objects are never resurrected. See Note [STATIC_LINK
  fields] in rts/sm/Storage.h for why this is bad.
- In practice, the GC thinks that B has already been visited, and so
  doesn't visit X, and catastrophe ensues.

== Isn't this caused by the RET_FUN business? ==

Maybe, but could you prove that RET_FUN is the only way that
resurrection can occur?

So, no shortcutting.
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


-- | Put the labelled blocks that we will be annotating with SRTs into
-- dependency order.  This is so that we can process them one at a
-- time, resolving references to earlier blocks to point to their
-- SRTs. CAFs themselves are not included here; see getCAFs below.
depAnalSRTs
  :: CAFEnv
  -> [CmmDecl]
  -> [SCC (Label, CAFLabel, Set CAFLabel)]
depAnalSRTs cafEnv decls =
  srtTrace "depAnalSRTs" (ppr graph) graph
 where
  labelledBlocks = concatMap getLabelledBlocks decls
  labelToBlock = Map.fromList (map swap labelledBlocks)
  graph = stronglyConnCompFromEdgedVerticesOrd
             [ let cafs' = Set.delete lbl cafs in
               DigraphNode (l,lbl,cafs') l
                 (mapMaybe (flip Map.lookup labelToBlock) (Set.toList cafs'))
             | (l, lbl) <- labelledBlocks
             , Just cafs <- [mapLookup l cafEnv] ]


-- | Get (Label, CAFLabel, Set CAFLabel) for each block that represents a CAF.
-- These are treated differently from other labelled blocks:
--  - we never shortcut a reference to a CAF to the contents of its
--    SRT, since the point of SRTs is to keep CAFs alive.
--  - CAFs therefore don't take part in the dependency analysis in depAnalSRTs.
--    instead we generate their SRTs after everything else.
getCAFs :: CAFEnv -> [CmmDecl] -> [(Label, CAFLabel, Set CAFLabel)]
getCAFs cafEnv decls =
  [ (g_entry g, mkCAFLabel topLbl, cafs)
  | CmmProc top_info topLbl _ g <- decls
  , Just info <- [mapLookup (g_entry g) (info_tbls top_info)]
  , let rep = cit_rep info
  , isStaticRep rep && isThunkRep rep
  , Just cafs <- [mapLookup (g_entry g) cafEnv]
  ]


-- | Get the list of blocks that correspond to the entry points for
-- FUN_STATIC closures.  These are the blocks for which if we have an
-- SRT we can merge it with the static closure. [FUN]
getStaticFuns :: [CmmDecl] -> [(BlockId, CLabel)]
getStaticFuns decls =
  [ (g_entry g, lbl)
  | CmmProc top_info _ _ g <- decls
  , Just info <- [mapLookup (g_entry g) (info_tbls top_info)]
  , Just (id, _) <- [cit_clo info]
  , let rep = cit_rep info
  , isStaticRep rep && isFunRep rep
  , let lbl = mkLocalClosureLabel (idName id) (idCafInfo id)
  ]


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

doSRTs dflags moduleSRTInfo tops = do
  us <- mkSplitUniqSupply 'u'

  -- Ignore the original grouping of decls, and combine all the
  -- CAFEnvs into a single CAFEnv.
  let (cafEnvs, declss) = unzip tops
      cafEnv = mapUnions cafEnvs
      decls = concat declss
      staticFuns = mapFromList (getStaticFuns decls)

  -- Put the decls in dependency order. Why? So that we can implement
  -- [Inline] and [Filter].  If we need to refer to an SRT that has
  -- a single entry, we use the entry itself, which means that we
  -- don't need to generate the singleton SRT in the first place.  But
  -- to do this we need to process blocks before things that depend on
  -- them.
  let
    sccs = depAnalSRTs cafEnv decls
    cafsWithSRTs = getCAFs cafEnv decls

  -- On each strongly-connected group of decls, construct the SRT
  -- closures and the SRT fields for info tables.
  let result ::
        [ ( [CmmDecl]              -- generated SRTs
          , [(Label, CLabel)]      -- SRT fields for info tables
          , [(Label, [SRTEntry])]  -- SRTs to attach to static functions
          ) ]
      ((result, _srtMap), moduleSRTInfo') =
        initUs_ us $
        flip runStateT moduleSRTInfo $
        flip runStateT Map.empty $ do
          nonCAFs <- mapM (doSCC dflags staticFuns) sccs
          cAFs <- forM cafsWithSRTs $ \(l, cafLbl, cafs) ->
            oneSRT dflags staticFuns [l] [cafLbl] True{-is a CAF-} cafs
          return (nonCAFs ++ cAFs)

      (declss, pairs, funSRTs) = unzip3 result

  -- Next, update the info tables with the SRTs
  let
    srtFieldMap = mapFromList (concat pairs)
    funSRTMap = mapFromList (concat funSRTs)
    decls' = concatMap (updInfoSRTs dflags srtFieldMap funSRTMap) decls

  return (moduleSRTInfo', concat declss ++ decls')


-- | Build the SRT for a strongly-connected component of blocks
doSCC
  :: DynFlags
  -> LabelMap CLabel           -- which blocks are static function entry points
  -> SCC (Label, CAFLabel, Set CAFLabel)
  -> StateT SRTMap
        (StateT ModuleSRTInfo UniqSM)
        ( [CmmDecl]              -- generated SRTs
        , [(Label, CLabel)]      -- SRT fields for info tables
        , [(Label, [SRTEntry])]  -- SRTs to attach to static functions
        )

doSCC dflags staticFuns  (AcyclicSCC (l, cafLbl, cafs)) =
  oneSRT dflags staticFuns [l] [cafLbl] False cafs

doSCC dflags staticFuns (CyclicSCC nodes) = do
  -- build a single SRT for the whole cycle, see Note [recursive SRTs]
  let (blockids, lbls, cafsets) = unzip3 nodes
      cafs = Set.unions cafsets
  oneSRT dflags staticFuns blockids lbls False cafs


{- Note [recursive SRTs]

If the dependency analyser has found us a recursive group of
declarations, then we build a single SRT for the whole group, on the
grounds that everything in the group is reachable from everything
else, so we lose nothing by having a single SRT.

However, there are a couple of wrinkles to be aware of.

* The Set CAFLabel for this SRT will contain labels in the group
itself. The SRTMap will therefore not contain entries for these labels
yet, so we can't turn them into SRTEntries using resolveCAF. BUT we
can just remove recursive references from the Set CAFLabel before
generating the SRT - the SRT will still contain all the CAFLabels that
we need to refer to from this group's SRT.

* That is, EXCEPT for static function closures. For the same reason
described in Note [Invalid optimisation: shortcutting], we cannot omit
references to static function closures.
  - But, since we will merge the SRT with one of the static function
    closures (see [FUN]), we can omit references to *that* static
    function closure from the SRT.
-}

-- | Build an SRT for a set of blocks
oneSRT
  :: DynFlags
  -> LabelMap CLabel            -- which blocks are static function entry points
  -> [Label]                    -- blocks in this set
  -> [CAFLabel]                 -- labels for those blocks
  -> Bool                       -- True <=> this SRT is for a CAF
  -> Set CAFLabel               -- SRT for this set
  -> StateT SRTMap
       (StateT ModuleSRTInfo UniqSM)
       ( [CmmDecl]                    -- SRT objects we built
       , [(Label, CLabel)]            -- SRT fields for these blocks' itbls
       , [(Label, [SRTEntry])]        -- SRTs to attach to static functions
       )

oneSRT dflags staticFuns blockids lbls isCAF cafs = do
  srtMap <- get
  topSRT <- lift get
  let
    -- Can we merge this SRT with a FUN_STATIC closure?
    (maybeFunClosure, otherFunLabels) =
      case [ (l,b) | b <- blockids, Just l <- [mapLookup b staticFuns] ] of
        [] -> (Nothing, [])
        ((l,b):xs) -> (Just (l,b), map (mkCAFLabel . fst) xs)

    -- Remove recursive references from the SRT, except for (all but
    -- one of the) static functions. See Note [recursive SRTs].
    nonRec = cafs `Set.difference`
      (Set.fromList lbls `Set.difference` Set.fromList otherFunLabels)

    -- First resolve all the CAFLabels to SRTEntries
    -- Implements the [Inline] optimisation.
    resolved =
       Set.fromList $
       catMaybes (map (resolveCAF srtMap) (Set.toList nonRec))

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
    isStaticFun = isJust maybeFunClosure

    -- For a label without a closure (e.g. a continuation), we must
    -- update the SRTMap for the label to point to a closure. It's
    -- important that we don't do this for static functions or CAFs,
    -- see Note [Invalid optimisation: shortcutting].
    updateSRTMap srtEntry =
      when (not isCAF && not isStaticFun) $ do
        let newSRTMap = Map.fromList [(cafLbl, srtEntry) | cafLbl <- lbls]
        put (Map.union newSRTMap srtMap)

    this_mod = thisModule topSRT

  case Set.toList filtered of
    [] -> do
      srtTrace "oneSRT: empty" (ppr lbls) $ return ()
      updateSRTMap Nothing
      return ([], [], [])

    -- [Inline] - when we have only one entry there is no need to
    -- build an SRT object at all, instead we put the singleton SRT
    -- entry in the info table.
    [one@(SRTEntry lbl)]
      | -- Info tables refer to SRTs by offset (as noted in the section
        -- "Referring to an SRT from the info table" of Note [SRTs]). However,
        -- when dynamic linking is used we cannot guarantee that the offset
        -- between the SRT and the info table will fit in the offset field.
        -- Consequently we build a singleton SRT in in this case.
        not (labelDynamic dflags this_mod lbl)

        -- MachO relocations can't express offsets between compilation units at
        -- all, so we are always forced to build a singleton SRT in this case.
          && (not (osMachOTarget $ platformOS $ targetPlatform dflags)
             || isLocalCLabel this_mod lbl) -> do

        -- If we have a static function closure, then it becomes the
        -- SRT object, and everything else points to it. (the only way
        -- we could have multiple labels here is if this is a
        -- recursive group, see Note [recursive SRTs])
        case maybeFunClosure of
          Just (staticFunLbl,staticFunBlock) -> return ([], withLabels, [])
            where
              withLabels =
                [ (b, if b == staticFunBlock then lbl else staticFunLbl)
                | b <- blockids ]
          Nothing -> do
            updateSRTMap (Just one)
            return ([], map (,lbl) blockids, [])

    cafList ->
      -- Check whether an SRT with the same entries has been emitted already.
      -- Implements the [Common] optimisation.
      case Map.lookup filtered (dedupSRTs topSRT) of
        Just srtEntry@(SRTEntry srtLbl)  -> do
          srtTrace "oneSRT [Common]" (ppr lbls <+> ppr srtLbl) $ return ()
          updateSRTMap (Just srtEntry)
          return ([], map (,srtLbl) blockids, [])
        Nothing -> do
          -- No duplicates: we have to build a new SRT object
          srtTrace "oneSRT: new" (ppr lbls <+> ppr filtered) $ return ()
          (decls, funSRTs, srtEntry) <-
            case maybeFunClosure of
              Just (fun,block) ->
                return ( [], [(block, cafList)], SRTEntry fun )
              Nothing -> do
                (decls, entry) <- lift . lift $ buildSRTChain dflags cafList
                return (decls, [], entry)
          updateSRTMap (Just srtEntry)
          let allBelowThis = Set.union allBelow filtered
              oldFlatSRTs = flatSRTs topSRT
              newFlatSRTs = Map.insert srtEntry allBelowThis oldFlatSRTs
              newDedupSRTs = Map.insert filtered srtEntry (dedupSRTs topSRT)
          lift (put (topSRT { dedupSRTs = newDedupSRTs
                            , flatSRTs = newFlatSRTs }))
          let SRTEntry lbl = srtEntry
          return (decls, map (,lbl) blockids, funSRTs)


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


-- | Update info tables with references to their SRTs. Also generate
-- static closures, splicing in SRT fields as necessary.
updInfoSRTs
  :: DynFlags
  -> LabelMap CLabel               -- SRT labels for each block
  -> LabelMap [SRTEntry]           -- SRTs to merge into FUN_STATIC closures
  -> CmmDecl
  -> [CmmDecl]

updInfoSRTs dflags srt_env funSRTEnv (CmmProc top_info top_l live g)
  | Just (_,closure) <- maybeStaticClosure = [ proc, closure ]
  | otherwise = [ proc ]
  where
    proc = CmmProc top_info { info_tbls = newTopInfo } top_l live g
    newTopInfo = mapMapWithKey updInfoTbl (info_tbls top_info)
    updInfoTbl l info_tbl
      | l == g_entry g, Just (inf, _) <- maybeStaticClosure = inf
      | otherwise  = info_tbl { cit_srt = mapLookup l srt_env }

    -- Generate static closures [FUN].  Note that this also generates
    -- static closures for thunks (CAFs), because it's easier to treat
    -- them uniformly in the code generator.
    maybeStaticClosure :: Maybe (CmmInfoTable, CmmDecl)
    maybeStaticClosure
      | Just info_tbl@CmmInfoTable{..} <-
           mapLookup (g_entry g) (info_tbls top_info)
      , Just (id, ccs) <- cit_clo
      , isStaticRep cit_rep =
        let
          (newInfo, srtEntries) = case mapLookup (g_entry g) funSRTEnv of
            Nothing ->
              -- if we don't add SRT entries to this closure, then we
              -- want to set the srt field in its info table as usual
              (info_tbl { cit_srt = mapLookup (g_entry g) srt_env }, [])
            Just srtEntries -> srtTrace "maybeStaticFun" (ppr res)
              (info_tbl { cit_rep = new_rep }, res)
              where res = [ CmmLabel lbl | SRTEntry lbl <- srtEntries ]
          fields = mkStaticClosureFields dflags info_tbl ccs (idCafInfo id)
            srtEntries
          new_rep = case cit_rep of
             HeapRep sta ptrs nptrs ty ->
               HeapRep sta (ptrs + length srtEntries) nptrs ty
             _other -> panic "maybeStaticFun"
          lbl = mkLocalClosureLabel (idName id) (idCafInfo id)
        in
          Just (newInfo, mkDataLits (Section Data lbl) lbl fields)
      | otherwise = Nothing

updInfoSRTs _ _ _ t = [t]


srtTrace :: String -> SDoc -> b -> b
-- srtTrace = pprTrace
srtTrace _ _ b = b
