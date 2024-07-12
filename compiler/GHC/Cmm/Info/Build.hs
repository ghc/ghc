{-# LANGUAGE GADTs, RecordWildCards,
    NondecreasingIndentation,
    OverloadedStrings, LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}


module GHC.Cmm.Info.Build
  ( CAFSet, CAFEnv, cafAnal, cafAnalData
  , doSRTs, ModuleSRTInfo (..), emptySRT
  , SRTMap, srtMapNonCAFs

  -- * Some internal bits
  , SRTEntry(..)
  , CAFfyLabel(..)
  ) where

import GHC.Prelude hiding (succ)

import GHC.Platform
import GHC.Platform.Profile

import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Cmm.BlockId
import GHC.Cmm.Config
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import qualified GHC.Cmm.Dataflow.Label as Det
import GHC.Cmm.Dataflow.Label.NonDet (lookupFact, Label)
import qualified GHC.Cmm.Dataflow.Label.NonDet as NonDet
import GHC.Cmm.Dataflow
import GHC.Unit.Module
import GHC.Data.Graph.Directed
import GHC.Cmm.CLabel
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Data.Maybe
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Runtime.Heap.Layout
import GHC.Types.CostCentre
import GHC.StgToCmm.Heap

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.List (unzip4)

import GHC.Types.Name.Set
import GHC.Types.Unique.DSM

{- Note [SRTs]
   ~~~~~~~~~~~
Static Reference Tables (SRTs) are the mechanism by which the garbage collector
can determine the live CAFs in the program. An SRT is a static table associated
with a CAFfy closure which record which CAFfy objects are reachable from
the closure's code.

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

In each case, the info table points to the SRT, if there is one.

- info->srt is 0 if there's no SRT
- otherwise, there are three ways which we may encode the location of the SRT in
  the info table, described below.

USE_SRT_POINTER
---------------
Most general implementation. Can always be used, but other ways are more efficient.

- info->srt is a pointer

We encode an **absolute pointer** to the SRT in info->srt. e.g. for a FUN
with an SRT:

StgInfoTable          +------+
  info->layout.ptrs   | ...  |
  info->layout.nptrs  | ...  |
  info->srt           |  ------------> pointer to SRT object
  info->type          | ...  |
                      |------|

USE_SRT_OFFSET
--------------
Requires:
  - tables-next-to-code enabled

In this case we use the info->srt to encode whether or not there is an SRT and
if so encode the offset to its location in info->f.srt_offset:

- info->srt is a half-word
- info->f.srt_offset is a 32-bit int
- info->srt is 0 if there's no SRT, otherwise,
- info->srt == 1 and info->f.srt_offset is a offset to the SRT, relative to the
field address itself

e.g. for a FUN with an SRT:

StgFunInfoTable       +------+
  info->f.srt_offset  |  ------------> offset to SRT object
StgInfoTable          +------+
  info->layout.ptrs   | ...  |
  info->layout.nptrs  | ...  |
  info->srt           |  1   |
  info->type          | ...  |
                      |------|

USE_INLINE_SRT_FIELD
--------------------
Requires:
  - tables-next-to-code enabled
  - 64-bit architecture
  - small memory model

We optimise the info table representation further.  The offset to the SRT can
be stored in 32 bits (all code lives within a 2GB region in x86_64's small
memory model), so we can save a word in the info table by storing the
srt_offset in the srt field, which is half a word.

- info->srt is a half-word
- info->srt is 0 if there's no SRT, otherwise:
- info->srt is an offset from the info pointer to the SRT object

StgInfoTable          +------+
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

Algorithm
^^^^^^^^^

0. let srtMap :: Map CAFfyLabel (Maybe SRTEntry) = {}
   Maps closures to their SRT entries (i.e. how they appear in a SRT payload)

1. Start with decls :: [CmmDecl]. This corresponds to an SCC of bindings in STG
   after code-generation.

2. CPS-convert each CmmDecl (GHC.Cmm.Pipeline.cpsTop), resulting in a list
   [CmmDecl]. There might be multiple CmmDecls in the result, due to proc-point
   splitting.

3. In cpsTop, *before* proc-point splitting, when we still have a single
   CmmDecl, we do cafAnal for procs:

   * cafAnal performs a backwards analysis on the code blocks

   * For each labelled block, the analysis produces a CAFSet (= Set CAFfyLabel),
     representing all the CAFfyLabels reachable from this label.

   * A label is added to the set if it refers to a FUN, THUNK, or RET,
     and its CafInfo /= NoCafRefs.
     (NB. all CafInfo for Ids in the current module should be initialised to
     MayHaveCafRefs)

   * The result is CAFEnv = LabelMap CAFSet

   (Why *before* proc-point splitting? Because the analysis needs to propagate
   information across branches, and proc-point splitting turns branches into
   CmmCalls to top-level CmmDecls.  The analysis would fail to find all the
   references to CAFFY labels if we did it after proc-point splitting.)

   For static data, cafAnalData simply returns set of all labels that refer to a
   FUN, THUNK, and RET whose CafInfos /= NoCafRefs.

4. The result of cpsTop is (CAFEnv, [CmmDecl]) for procs and (CAFSet, CmmDecl)
   for static data. So after `mapM cpsTop decls` we have
   [Either (CAFEnv, [CmmDecl]) (CAFSet, CmmDecl)]

5. For procs concat the decls and union the CAFEnvs to get (CAFEnv, [CmmDecl])

6. For static data generate a Map CLabel CAFSet (maps static data to their CAFSets)

7. Dependency-analyse the decls using CAFEnv and CAFSets, giving us SCC CAFfyLabel

8. For each SCC in dependency order
   - Let lbls :: [CAFfyLabel] be the non-recursive labels in this SCC
   - Apply CAFEnv to each label and concat the result :: [CAFfyLabel]
   - For each CAFfyLabel in the set apply srtMap (and ignore Nothing) to get
     srt :: [SRTEntry]
   - Make a label for this SRT, call it l
   - If the SRT is not empty (i.e. the group is CAFFY) add FUN_STATICs in the
     group to the SRT (see Note [Invalid optimisation: shortcutting])
   - Add to srtMap: lbls -> if null srt then Nothing else Just l

9. At the end, update the IdInfo for every top-level binding x:
   if srtMap x == Nothing, then the binding is non-CAFFY, otherwise it is
   CAFFY.

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
   optimisation and generate the singleton SRT, because SRTs are in the
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
{-
Note [No static object resurrection]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The "static flag" mechanism (see Note [STATIC_LINK fields] in smStorage.h) that
the GC uses to track liveness of static objects assumes that unreachable
objects will never become reachable again (i.e. are never "resurrected").
Breaking this assumption can result in extremely subtle GC soundness issues
(e.g. #15544, #20959).

Guaranteeing that this assumption is not violated requires that all CAFfy
static objects reachable from the object's code are reachable from its SRT.  In
the past we have gotten this wrong in a few ways:

 * shortcutting references to FUN_STATICs to instead point to the FUN_STATIC's
   SRT. This lead to #15544 and is described in more detail in Note [Invalid
   optimisation: shortcutting].

 * omitting references to static data constructor applications. This previously
   happened due to an oversight (#20959): when generating an SRT for a
   recursive group we would drop references to the CAFfy static data
   constructors.

To see why we cannot allow object resurrection, see the examples in the
above-mentioned Notes.

If a static closure definitely does not transitively refer to any CAFs, then it
*may* be advertised as not-CAFfy in the interface file and consequently *may*
be omitted from SRTs. Regardless of whether the closure is advertised as CAFfy
or non-CAFfy, its STATIC_LINK field *must* be set to 3, so that it never
appears on the static closure list.
-}

{-
Note [Invalid optimisation: shortcutting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
   objects for the reason described in Note [No static object resurrection].
   We noticed this in #15544.

The particular case that cropped up when we tried this in #15544 was:

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



Note [Ticky labels in SRT analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Raw Cmm data (CmmStaticsRaw) can't contain pointers so they're considered
non-CAFFY in SRT analysis and we update the SRTMap mapping them to `Nothing`
(meaning they're not CAFFY).

However when building with -ticky we generate ticky CLabels using the function's
`Name`. For example, if we have a top-level function `sat_s1rQ`, in a ticky
build we get two IdLabels using the name `sat_s1rQ`:

- For the function itself: IdLabel sat_s1rQ ... Entry
- For the ticky counter: IdLabel sat_s1rQ ... RednCounts

In these cases we really want to use the function definition for the SRT
analysis of this Name, because that's what we export for this Name -- ticky
counters are not exported. So we ignore ticky counters in SRT analysis (which
are never CAFFY and never exported).

Not doing this caused #17947 where we analysed the function first mapped the
name to CAFFY. We then saw the ticky constructor, and because it has the same
Name as the function and is not CAFFY we overrode the CafInfo of the name as
non-CAFFY.
-}

-- ---------------------------------------------------------------------
-- Label types

-- |
-- The label of a CAFfy thing.
--
-- Labels that come from 'cafAnal' can be:
--   - @_closure@ labels for static functions, static data constructor
--     applications, or static thunks
--   - @_info@ labels for dynamic functions, thunks, or continuations
--   - @_entry@ labels for functions or thunks
--
-- Meanwhile the labels on top-level blocks are @_entry@ labels.
--
-- To put everything in the same namespace we convert all labels to
-- closure labels using 'toClosureLbl'.  Note that some of these
-- labels will not actually exist; that's ok because we're going to
-- map them to SRTEntry later, which ranges over labels that do exist.
--
newtype CAFfyLabel = CAFfyLabel CLabel
  deriving (Eq,Ord)

deriving newtype instance OutputableP env CLabel => OutputableP env CAFfyLabel

type CAFSet = Set CAFfyLabel
type CAFEnv = NonDet.LabelMap CAFSet

-- | Records the CAFfy references of a set of static data decls.
type DataCAFEnv = Map CLabel CAFSet


mkCAFfyLabel :: Platform -> CLabel -> CAFfyLabel
mkCAFfyLabel platform lbl = CAFfyLabel (toClosureLbl platform lbl)

-- This is a label that we can put in an SRT.  It *must* be a closure label,
-- pointing to either a @FUN_STATIC@, @THUNK_STATIC@, or @CONSTR@.
newtype SRTEntry = SRTEntry CLabel
  deriving (Eq, Ord)

deriving newtype instance OutputableP env CLabel => OutputableP env SRTEntry


-- ---------------------------------------------------------------------
-- CAF analysis

addCafLabel :: Platform -> CLabel -> CAFSet -> CAFSet
addCafLabel platform l s
  | Just _ <- hasHaskellName l
  , let caf_label = mkCAFfyLabel platform l
    -- For imported Ids hasCAF will have accurate CafInfo
    -- Locals are initialized as CAFFY. We turn labels with empty SRTs into
    -- non-CAFFYs in doSRTs
  , hasCAF l
  = Set.insert caf_label s
  | otherwise
  = s

-- | Collect possible CAFfy references from a 'CmmData' decl.
cafAnalData
  :: Platform
  -> CmmStatics
  -> CAFSet
cafAnalData platform st = case st of
   CmmStaticsRaw _lbl _data           -> Set.empty
   CmmStatics _lbl _itbl _ccs payload _extras ->
       foldl' analyzeStatic Set.empty payload
     where
       analyzeStatic s lit =
         case lit of
           CmmLabel c -> addCafLabel platform c s
           CmmLabelOff c _ -> addCafLabel platform c s
           CmmLabelDiffOff c1 c2 _ _ -> addCafLabel platform c1 $! addCafLabel platform c2 s
           _ -> s

-- |
-- For each code block:
--   - collect the references reachable from this code block to FUN,
--     THUNK or RET labels for which @hasCAF == True@
--
-- This gives us a 'CAFEnv': a mapping from code block to sets of labels
--
cafAnal
  :: Platform
  -> NonDet.LabelSet   -- ^ The blocks representing continuations, ie. those
                -- that will get RET info tables.  These labels will
                -- get their own SRTs, so we don't aggregate CAFs from
                -- references to these labels, we just use the label.
  -> CLabel     -- ^ The top label of the proc
  -> CmmGraph
  -> CAFEnv
cafAnal platform contLbls topLbl cmmGraph =
  analyzeCmmBwd cafLattice
    (cafTransfers platform contLbls (g_entry cmmGraph) topLbl) cmmGraph NonDet.mapEmpty


cafLattice :: DataflowLattice CAFSet
cafLattice = DataflowLattice Set.empty add
  where
    add (OldFact old) (NewFact new) =
        let !new' = old `Set.union` new
        in changedIf (Set.size new' > Set.size old) new'


cafTransfers :: Platform -> NonDet.LabelSet -> Label -> CLabel -> TransferFun CAFSet
cafTransfers platform contLbls entry topLbl
  block@(BlockCC eNode middle xNode) fBase =
    let joined :: CAFSet
        joined = cafsInNode xNode $! live'

        result :: CAFSet
        !result = foldNodesBwdOO cafsInNode middle joined

        facts :: [Set CAFfyLabel]
        facts = mapMaybe successorFact (successors xNode)

        live' :: CAFSet
        live' = joinFacts cafLattice facts

        successorFact :: Label -> Maybe (Set CAFfyLabel)
        successorFact s
          -- If this is a loop back to the entry, we can refer to the
          -- entry label.
          | s == entry = Just (addCafLabel platform topLbl Set.empty)
          -- If this is a continuation, we want to refer to the
          -- SRT for the continuation's info table
          | s `NonDet.setMember` contLbls
          = Just (Set.singleton (mkCAFfyLabel platform (infoTblLbl s)))
          -- Otherwise, takes the CAF references from the destination
          | otherwise
          = lookupFact s fBase

        cafsInNode :: CmmNode e x -> CAFSet -> CAFSet
        cafsInNode node set = foldExpDeep addCafExpr node set

        addCafExpr :: CmmExpr -> Set CAFfyLabel -> Set CAFfyLabel
        addCafExpr expr !set =
          case expr of
            CmmLit (CmmLabel c) ->
              addCafLabel platform c set
            CmmLit (CmmLabelOff c _) ->
              addCafLabel platform c set
            CmmLit (CmmLabelDiffOff c1 c2 _ _) ->
              addCafLabel platform c1 $! addCafLabel platform c2 set
            _ ->
              set
    in
      srtTrace "cafTransfers" (text "block:"         <+> pdoc platform block $$
                                text "contLbls:"     <+> ppr contLbls $$
                                text "entry:"        <+> ppr entry $$
                                text "topLbl:"       <+> pdoc platform topLbl $$
                                text "cafs in exit:" <+> pdoc platform joined $$
                                text "result:"       <+> pdoc platform result) $
        NonDet.mapSingleton (entryLabel eNode) result


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
  , moduleSRTMap :: SRTMap
  }

instance OutputableP env CLabel => OutputableP env ModuleSRTInfo where
  pdoc env ModuleSRTInfo{..} =
    text "ModuleSRTInfo {" $$
      (nest 4 $ text "dedupSRTs ="    <+> pdoc env dedupSRTs $$
                text "flatSRTs ="     <+> pdoc env flatSRTs $$
                text "moduleSRTMap =" <+> pdoc env moduleSRTMap) $$ char '}'

emptySRT :: Module -> ModuleSRTInfo
emptySRT mod =
  ModuleSRTInfo
    { thisModule = mod
    , dedupSRTs = Map.empty
    , flatSRTs = Map.empty
    , moduleSRTMap = Map.empty
    }

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

data SomeLabel
  = BlockLabel !Label
  | DeclLabel CLabel
  deriving (Eq, Ord)

instance OutputableP env CLabel => OutputableP env SomeLabel where
   pdoc env = \case
      BlockLabel l -> text "b:" <+> pdoc env l
      DeclLabel l  -> text "s:" <+> pdoc env l

getBlockLabel :: SomeLabel -> Maybe Label
getBlockLabel (BlockLabel l) = Just l
getBlockLabel (DeclLabel _) = Nothing

getBlockLabels :: [SomeLabel] -> [Label]
getBlockLabels = mapMaybe getBlockLabel

-- | Return a @(Label,CLabel)@ pair for each labelled block of a 'CmmDecl',
--   where the label is
--   - the info label for a continuation or dynamic closure
--   - the closure label for a top-level function (not a CAF)
getLabelledBlocks :: Platform -> CmmDecl -> [(SomeLabel, CAFfyLabel)]
getLabelledBlocks platform decl = case decl of
   CmmData _ (CmmStaticsRaw _ _)    -> []
   CmmData _ (CmmStatics lbl info _ _ _) -> [ (DeclLabel lbl, mkCAFfyLabel platform lbl)
                                            | not (isThunkRep (cit_rep info))
                                            ]
   CmmProc top_info _ _ _           -> [ (BlockLabel blockId, caf_lbl)
                                       | (blockId, info) <- Det.mapToList (info_tbls top_info)
                                       , let rep = cit_rep info
                                       , not (isStaticRep rep) || not (isThunkRep rep)
                                       , let !caf_lbl = mkCAFfyLabel platform (cit_lbl info)
                                       ]

-- | Put the labelled blocks that we will be annotating with SRTs into
-- dependency order.  This is so that we can process them one at a
-- time, resolving references to earlier blocks to point to their
-- SRTs. CAFs themselves are not included here; see 'getCAFs' below.
depAnalSRTs
  :: Platform
  -> CAFEnv            -- ^ 'CAFEnv' for procedures. From 'cafAnal'.
  -> Map CLabel CAFSet -- ^ CAFEnv for statics. Maps statics to the set of the
                       -- CAFfy things which they refer to. From 'cafAnalData'.
  -> [CmmDecl]         -- ^ the decls to analyse.
  -> [SCC (SomeLabel, CAFfyLabel, Set CAFfyLabel)]
depAnalSRTs platform cafEnv cafEnv_static decls =
  srtTrace "depAnalSRTs" (text "decls:"  <+> pdoc platform decls $$
                           text "nodes:" <+> pdoc platform (map node_payload nodes) $$
                           text "graph:" <+> pdoc platform graph) graph
 where
  labelledBlocks :: [(SomeLabel, CAFfyLabel)]
  labelledBlocks = concatMap (getLabelledBlocks platform) decls
  labelToBlock :: Map CAFfyLabel SomeLabel
  labelToBlock = foldl' (\m (v,k) -> Map.insert k v m) Map.empty labelledBlocks

  -- the set of graph nodes. A node is identified by either a BlockLabel (in
  -- the case of code) or a DeclLabel (in the case of static data).
  nodes :: [Node SomeLabel (SomeLabel, CAFfyLabel, Set CAFfyLabel)]
  nodes = [ DigraphNode (l,lbl,cafs') l
              (mapMaybe (flip Map.lookup labelToBlock) (Set.toList cafs'))
          | (l, lbl) <- labelledBlocks
          , Just (cafs :: Set CAFfyLabel) <-
              [case l of
                 BlockLabel l -> NonDet.mapLookup l cafEnv
                 DeclLabel cl -> Map.lookup cl cafEnv_static]
          , let cafs' = Set.delete lbl cafs
          ]

  graph :: [SCC (SomeLabel, CAFfyLabel, Set CAFfyLabel)]
  graph = stronglyConnCompFromEdgedVerticesOrd nodes

-- | Get @(Maybe Label, CAFfyLabel, Set CAFfyLabel)@ for each CAF block.
-- The @Set CAFfyLabel@ represents the set of CAFfy things which this CAF's code
-- depends upon.
--
--  - The 'Label' represents the entry code of the closure. This may be
--    'Nothing' if it is a standard closure type (e.g. @stg_unpack_cstring@; see
--    Note [unpack_cstring closures] in StgStdThunks.cmm).
--  - The 'CAFLabel' is the label of the CAF closure.
--  - The @Set CAFLabel@ is the set of CAFfy closures which should be included
--    in the closure's SRT.
--
-- Note that CAFs are treated differently from other labelled blocks:
--
--  - we never shortcut a reference to a CAF to the contents of its
--    SRT, since the point of SRTs is to keep CAFs alive.
--
--  - CAFs therefore don't take part in the dependency analysis in depAnalSRTs.
--    instead we generate their SRTs after everything else.
getCAFs :: Platform -> CAFEnv -> [CmmDecl] -> [(Maybe Label, CAFfyLabel, Set CAFfyLabel)]
getCAFs platform cafEnv = mapMaybe getCAFLabel
  where
    getCAFLabel :: CmmDecl -> Maybe (Maybe Label, CAFfyLabel, Set CAFfyLabel)

    getCAFLabel (CmmProc top_info top_lbl _ g)
      | Just info <- Det.mapLookup (g_entry g) (info_tbls top_info)
      , let rep = cit_rep info
      , isStaticRep rep && isThunkRep rep
      , Just cafs <- NonDet.mapLookup (g_entry g) cafEnv
      = Just (Just (g_entry g), mkCAFfyLabel platform top_lbl, cafs)

      | otherwise
      = Nothing

    getCAFLabel (CmmData _ (CmmStatics top_lbl info _ccs _payload _extras))
      | isThunkRep (cit_rep info)
      = Just (Nothing, mkCAFfyLabel platform top_lbl, Set.empty)

      | otherwise
      = Nothing

    getCAFLabel (CmmData _ (CmmStaticsRaw _lbl _payload))
      = Nothing

-- | Get the list of blocks that correspond to the entry points for
-- @FUN_STATIC@ closures.  These are the blocks for which if we have an
-- SRT we can merge it with the static closure. [FUN]
getStaticFuns :: [CmmDecl] -> [(BlockId, CLabel)]
getStaticFuns decls =
  [ (g_entry g, lbl)
  | CmmProc top_info _ _ g <- decls
  , Just info <- [Det.mapLookup (g_entry g) (info_tbls top_info)]
  , Just (id, _) <- [cit_clo info]
  , let rep = cit_rep info
  , isStaticRep rep && isFunRep rep
  , let !lbl = mkClosureLabel (idName id) (idCafInfo id)
  ]


-- | Maps labels from 'cafAnal' to the final CLabel that will appear
-- in the SRT.
--   - closures with singleton SRTs resolve to their single entry
--   - closures with larger SRTs map to the label for that SRT
--   - CAFs must not map to anything!
--   - if a labels maps to Nothing, we found that this label's SRT
--     is empty, so we don't need to refer to it from other SRTs.
type SRTMap = Map CAFfyLabel (Maybe SRTEntry)


-- | Given 'SRTMap' of a module, returns the set of non-CAFFY names in the
-- module.  Any 'Name's not in the set are CAFFY.
srtMapNonCAFs :: SRTMap -> NonCaffySet
srtMapNonCAFs srtMap =
    NonCaffySet $ mkNameSet (mapMaybe get_name (Map.toList srtMap))
  where
    get_name (CAFfyLabel l, Nothing) = hasHaskellName l
    get_name (_l, Just _srt_entry) = Nothing

-- | Resolve a CAFfyLabel to its 'SRTEntry' using the 'SRTMap'.
resolveCAF :: Platform -> SRTMap -> CAFfyLabel -> Maybe SRTEntry
resolveCAF platform srtMap lbl@(CAFfyLabel l) =
    srtTrace "resolveCAF" ("l:" <+> pdoc platform l <+> "resolved:" <+> pdoc platform ret) ret
  where
    ret = Map.findWithDefault (Just (SRTEntry (toClosureLbl platform l))) lbl srtMap

anyCafRefs :: [CafInfo] -> CafInfo
anyCafRefs caf_infos = case any mayHaveCafRefs caf_infos of
                         True -> MayHaveCafRefs
                         False -> NoCafRefs

-- | Attach SRTs to all info tables in the 'CmmDecl's, and add SRT
-- declarations to the 'ModuleSRTInfo'.
--
doSRTs
  :: CmmConfig
  -> ModuleSRTInfo
  -> DUniqSupply
  -> [(CAFEnv, [CmmDecl])]   -- ^ 'CAFEnv's and 'CmmDecl's for code blocks
  -> [(CAFSet, CmmDataDecl)]     -- ^ static data decls and their 'CAFSet's
  -> IO (ModuleSRTInfo, DUniqSupply, [CmmDeclSRTs])

doSRTs cfg moduleSRTInfo dus procs data_ = do

  let runUDSM = runUniqueDSM dus
  let profile = cmmProfile cfg

  -- Ignore the original grouping of decls, and combine all the
  -- CAFEnvs into a single CAFEnv.
  let static_data_env :: DataCAFEnv
      static_data_env =
        Map.fromList $
        flip map data_ $
        \(set, decl) ->
          case decl of
            CmmProc void _ _ _ -> case void of
            CmmData _ static ->
              case static of
                CmmStatics lbl _ _ _ _ -> (lbl, set)
                CmmStaticsRaw lbl _ -> (lbl, set)

      (proc_envs, procss) = unzip procs
      cafEnv = NonDet.mapUnions proc_envs
      decls = map (cmmDataDeclCmmDecl . snd) data_ ++ concat procss
      staticFuns = NonDet.mapFromList (getStaticFuns decls)

      platform = cmmPlatform cfg

  -- Put the decls in dependency order. Why? So that we can implement
  -- [Inline] and [Filter].  If we need to refer to an SRT that has
  -- a single entry, we use the entry itself, which means that we
  -- don't need to generate the singleton SRT in the first place.  But
  -- to do this we need to process blocks before things that depend on
  -- them.
  let
    sccs :: [SCC (SomeLabel, CAFfyLabel, Set CAFfyLabel)]
    sccs = {-# SCC depAnalSRTs #-} depAnalSRTs platform cafEnv static_data_env decls

    cafsWithSRTs :: [(Maybe Label, CAFfyLabel, Set CAFfyLabel)]
    cafsWithSRTs = getCAFs platform cafEnv decls

  srtTraceM "doSRTs" (text "data:"            <+> pdoc platform data_ $$
                      text "procs:"           <+> pdoc platform procs $$
                      text "static_data_env:" <+> pdoc platform static_data_env $$
                      text "sccs:"            <+> pdoc platform sccs $$
                      text "cafsWithSRTs:"    <+> pdoc platform cafsWithSRTs)

  -- On each strongly-connected group of decls, construct the SRT
  -- closures and the SRT fields for info tables.
  let result ::
        [ ( [CmmDeclSRTs]          -- generated SRTs
          , [(Label, CLabel)]      -- SRT fields for info tables
          , [(Label, [SRTEntry])]  -- SRTs to attach to static functions
          , CafInfo                -- Whether the group has CAF references
          ) ]

      ((result, moduleSRTInfo'), dus') =
        runUDSM $
        flip runStateT moduleSRTInfo $ do
          nonCAFs <- mapM (doSCC cfg staticFuns static_data_env) sccs
          cAFs <- forM cafsWithSRTs $ \(l, cafLbl, cafs) ->
            oneSRT cfg staticFuns (map BlockLabel (maybeToList l)) [cafLbl]
                   True{-is a CAF-} cafs static_data_env
          return (nonCAFs ++ cAFs)

      (srt_declss, pairs, funSRTs, has_caf_refs) = unzip4 result
      srt_decls = concat srt_declss

  -- Next, update the info tables with the SRTs
  let
    srtFieldMap = NonDet.mapFromList (concat pairs)
    funSRTMap = NonDet.mapFromList (concat funSRTs)
    has_caf_refs' = anyCafRefs has_caf_refs
    decls' =
      concatMap (updInfoSRTs profile srtFieldMap funSRTMap has_caf_refs') decls

  -- Finally update CafInfos for raw static literals (CmmStaticsRaw). Those are
  -- not analysed in oneSRT so we never add entries for them to the SRTMap.
  let srtMap_w_raws =
        foldl' (\(srtMap :: SRTMap) (_, decl) ->
                  case decl of
                    CmmData _ CmmStatics{} ->
                      -- already updated by oneSRT
                      srtMap
                    CmmData _ (CmmStaticsRaw lbl _)
                      | isIdLabel lbl && not (isTickyLabel lbl) ->
                          -- Raw data are not analysed by oneSRT and they can't
                          -- be CAFFY.
                          -- See Note [Ticky labels in SRT analysis] above for
                          -- why we exclude ticky labels here.
                          Map.insert (mkCAFfyLabel platform lbl) Nothing srtMap
                      | otherwise ->
                          -- Not an IdLabel, ignore
                          srtMap
                    CmmProc void _ _ _ -> case void of)
               (moduleSRTMap moduleSRTInfo') data_

  return (moduleSRTInfo'{ moduleSRTMap = srtMap_w_raws }, dus', srt_decls ++ decls')


-- | Build the SRT for a strongly-connected component of blocks.
doSCC
  :: CmmConfig
  -> NonDet.LabelMap CLabel -- ^ which blocks are static function entry points
  -> DataCAFEnv      -- ^ static data
  -> SCC (SomeLabel, CAFfyLabel, Set CAFfyLabel)
  -> StateT ModuleSRTInfo UniqDSM
        ( [CmmDeclSRTs]          -- generated SRTs
        , [(Label, CLabel)]      -- SRT fields for info tables
        , [(Label, [SRTEntry])]  -- SRTs to attach to static functions
        , CafInfo                -- Whether the group has CAF references
        )

doSCC cfg staticFuns static_data_env (AcyclicSCC (l, cafLbl, cafs)) =
  oneSRT cfg staticFuns [l] [cafLbl] False cafs static_data_env

doSCC cfg staticFuns static_data_env (CyclicSCC nodes) = do
  -- build a single SRT for the whole cycle, see Note [recursive SRTs]
  let (lbls, caf_lbls, cafsets) = unzip3 nodes
      cafs = Set.unions cafsets
  oneSRT cfg staticFuns lbls caf_lbls False cafs static_data_env


{- Note [recursive SRTs]
   ~~~~~~~~~~~~~~~~~~~~~
If the dependency analyser has found us a recursive group of
declarations, then we build a single SRT for the whole group, on the
grounds that everything in the group is reachable from everything
else, so we lose nothing by having a single SRT.

However, there are a couple of wrinkles to be aware of.

* The Set CAFfyLabel for this SRT will contain labels in the group
  itself. The SRTMap will therefore not contain entries for these labels
  yet, so we can't turn them into SRTEntries using resolveCAF. BUT we
  can just remove recursive references from the Set CAFLabel before
  generating the SRT - the group SRT will consist of the union of the SRTs of
  each of group's constituents minus recursive references.

* That is, EXCEPT for static function closures and static data constructor
  applications. For the same reason described in Note [No static object
  resurrection], we cannot omit references to static function closures and
  constructor applications.

  But, since we will merge the SRT with one of the static function
  closures (see [FUN]), we can omit references to *that* static
  function closure from the SRT.

* Similarly, we must reintroduce recursive references to static data
  constructor applications into the group's SRT.
-}

-- | Build an SRT for a set of blocks
oneSRT
  :: CmmConfig
  -> NonDet.LabelMap CLabel            -- ^ which blocks are static function entry points
  -> [SomeLabel]                -- ^ blocks in this set
  -> [CAFfyLabel]               -- ^ labels for those blocks
  -> Bool                       -- ^ True <=> this SRT is for a CAF
  -> Set CAFfyLabel             -- ^ SRT for this set
  -> DataCAFEnv                 -- Static data labels in this group
  -> StateT ModuleSRTInfo UniqDSM
       ( [CmmDeclSRTs]                -- SRT objects we built
       , [(Label, CLabel)]            -- SRT fields for these blocks' itbls
       , [(Label, [SRTEntry])]        -- SRTs to attach to static functions
       , CafInfo                      -- Whether the group has CAF references
       )

oneSRT cfg staticFuns lbls caf_lbls isCAF cafs static_data_env = do
  topSRT <- get

  let
    this_mod = thisModule topSRT
    profile  = cmmProfile cfg
    platform = profilePlatform profile
    srtMap   = moduleSRTMap topSRT

    blockids = getBlockLabels lbls

    -- Can we merge this SRT with a FUN_STATIC closure?
    maybeFunClosure :: Maybe (CLabel, Label)
    otherFunLabels :: [CLabel]
    (maybeFunClosure, otherFunLabels) =
      case [ (l,b) | b <- blockids, Just l <- [NonDet.mapLookup b staticFuns] ] of
        [] -> (Nothing, [])
        ((l,b):xs) -> (Just (l,b), map fst xs)

    -- Remove recursive references from the SRT as described in
    -- Note [recursive SRTs]. We carefully reintroduce references to static
    -- functions and data constructor applications below, as is necessary due
    -- to Note [No static object resurrection].
    nonRec :: Set CAFfyLabel
    nonRec = cafs `Set.difference` Set.fromList caf_lbls

    -- Resolve references to their SRT entries
    resolved :: [SRTEntry]
    resolved = mapMaybe (resolveCAF platform srtMap) (Set.toList nonRec)

    -- The set of all SRTEntries in SRTs that we refer to from here.
    allBelow =
      Set.unions [ lbls | caf <- resolved
                        , Just lbls <- [Map.lookup caf (flatSRTs topSRT)] ]

    -- Remove SRTEntries that are also in an SRT that we refer to.
    -- Implements the [Filter] optimisation.
    filtered0 = Set.fromList resolved `Set.difference` allBelow

  srtTraceM "oneSRT:"
     (text "srtMap:"          <+> pdoc platform srtMap $$
      text "nonRec:"          <+> pdoc platform nonRec $$
      text "lbls:"            <+> pdoc platform lbls $$
      text "caf_lbls:"        <+> pdoc platform caf_lbls $$
      text "static_data_env:" <+> pdoc platform static_data_env $$
      text "cafs:"            <+> pdoc platform cafs $$
      text "blockids:"        <+> ppr blockids $$
      text "maybeFunClosure:" <+> pdoc platform maybeFunClosure $$
      text "otherFunLabels:"  <+> pdoc platform otherFunLabels $$
      text "resolved:"        <+> pdoc platform resolved $$
      text "allBelow:"        <+> pdoc platform allBelow $$
      text "filtered0:"       <+> pdoc platform filtered0)

  let
    isStaticFun = isJust maybeFunClosure

    -- For a label without a closure (e.g. a continuation), we must
    -- update the SRTMap for the label to point to a closure. It's
    -- important that we don't do this for static functions or CAFs,
    -- see Note [Invalid optimisation: shortcutting].
    updateSRTMap :: Maybe SRTEntry -> StateT ModuleSRTInfo UniqDSM ()
    updateSRTMap srtEntry =
      srtTrace "updateSRTMap"
        (pdoc platform srtEntry <+> "isCAF:" <+> ppr isCAF <+>
         "isStaticFun:" <+> ppr isStaticFun) $
      when (not isCAF && (not isStaticFun || isNothing srtEntry)) $
        modify' $ \state ->
           let !srt_map =
                 foldl' (\srt_map cafLbl@(CAFfyLabel clbl) ->
                          -- Only map static data to Nothing (== not CAFFY). For CAFFY
                          -- statics we refer to the static itself instead of a SRT.
                          if not (Map.member clbl static_data_env) || isNothing srtEntry then
                            Map.insert cafLbl srtEntry srt_map
                          else
                            srt_map)
                        (moduleSRTMap state)
                        caf_lbls
           in
               state{ moduleSRTMap = srt_map }

    allStaticData =
      all (\(CAFfyLabel clbl) -> Map.member clbl static_data_env) caf_lbls

  if Set.null filtered0 then do
    srtTraceM "oneSRT: empty" (pdoc platform caf_lbls)
    updateSRTMap Nothing
    return ([], [], [], NoCafRefs)
  else do
    -- We're going to build an SRT for this group, which should include function
    -- references in the group. See Note [recursive SRTs].
    let allBelow_funs =
          Set.fromList (map (SRTEntry . toClosureLbl platform) otherFunLabels)
    -- We must also ensure that all CAFfy static data constructor applications
    -- are included. See Note [recursive SRTs] and #20959.
    let allBelow_data =
          Set.fromList
          [ SRTEntry $ toClosureLbl platform lbl
          | DeclLabel lbl <- lbls
          , Just refs <- pure $ Map.lookup lbl static_data_env
          , not $ Set.null refs
          ]
    let filtered = filtered0 `Set.union` allBelow_funs `Set.union` allBelow_data
    srtTraceM "oneSRT" (text "filtered:"      <+> pdoc platform filtered $$
                        text "allBelow_funs:" <+> pdoc platform allBelow_funs)
    case Set.toList filtered of
      [] -> pprPanic "oneSRT" empty -- unreachable

      -- [Inline] - when we have only one entry there is no need to
      -- build an SRT object at all, instead we put the singleton SRT
      -- entry in the info table.
      [one@(SRTEntry lbl)]
        | -- Info tables refer to SRTs by offset (as noted in the section
          -- "Referring to an SRT from the info table" of Note [SRTs]). However,
          -- when dynamic linking is used we cannot guarantee that the offset
          -- between the SRT and the info table will fit in the offset field.
          -- Consequently we build a singleton SRT in this case.
          not (labelDynamic this_mod platform (cmmExternalDynamicRefs cfg) lbl)

          -- MachO relocations can't express offsets between compilation units at
          -- all, so we are always forced to build a singleton SRT in this case
          -- (cf #15169)
            && (not (osMachOTarget $ platformOS $ profilePlatform profile)
               || isLocalCLabel this_mod lbl) -> do

          -- If we have a static function closure, then it becomes the
          -- SRT object, and everything else points to it. (the only way
          -- we could have multiple labels here is if this is a
          -- recursive group, see Note [recursive SRTs])
          case maybeFunClosure of
            Just (staticFunLbl,staticFunBlock) ->
                return ([], withLabels, [], MayHaveCafRefs)
              where
                withLabels =
                  [ (b, if b == staticFunBlock then lbl else staticFunLbl)
                  | b <- blockids ]
            Nothing -> do
              srtTraceM "oneSRT: one" (text "caf_lbls:" <+> pdoc platform caf_lbls $$
                                       text "one:"      <+> pdoc platform one)
              updateSRTMap (Just one)
              return ([], map (,lbl) blockids, [], MayHaveCafRefs)

      cafList | allStaticData ->
        let caffiness = if null cafList then NoCafRefs else MayHaveCafRefs
        in return ([], [], [], caffiness)

      cafList ->
        -- Check whether an SRT with the same entries has been emitted already.
        -- Implements the [Common] optimisation.
        case Map.lookup filtered (dedupSRTs topSRT) of
          Just srtEntry@(SRTEntry srtLbl)  -> do
            srtTraceM "oneSRT [Common]" (pdoc platform caf_lbls <+> pdoc platform srtLbl)
            updateSRTMap (Just srtEntry)
            return ([], map (,srtLbl) blockids, [], MayHaveCafRefs)
          Nothing -> do
            -- No duplicates: we have to build a new SRT object
            (decls, funSRTs, srtEntry) <-
              case maybeFunClosure of
                Just (fun,block) ->
                  return ( [], [(block, cafList)], SRTEntry fun )
                Nothing -> do
                  (decls, entry) <- lift $ buildSRTChain profile cafList
                  return (decls, [], entry)
            updateSRTMap (Just srtEntry)
            let allBelowThis = Set.union allBelow filtered
                newFlatSRTs = Map.insert srtEntry allBelowThis (flatSRTs topSRT)
                -- When all definition in this group are static data we don't
                -- generate any SRTs.
                newDedupSRTs = Map.insert filtered srtEntry (dedupSRTs topSRT)
            modify' (\state -> state{ dedupSRTs = newDedupSRTs,
                                      flatSRTs = newFlatSRTs })
            srtTraceM "oneSRT: new" (text "caf_lbls:"      <+> pdoc platform caf_lbls $$
                                      text "filtered:"     <+> pdoc platform filtered $$
                                      text "srtEntry:"     <+> pdoc platform srtEntry $$
                                      text "newDedupSRTs:" <+> pdoc platform newDedupSRTs $$
                                      text "newFlatSRTs:"  <+> pdoc platform newFlatSRTs)
            let SRTEntry lbl = srtEntry
            return (decls, map (,lbl) blockids, funSRTs, MayHaveCafRefs)


-- | Build a static SRT object (or a chain of objects) from a list of
-- 'SRTEntry's.
buildSRTChain
   :: Profile
   -> [SRTEntry]
   -> UniqDSM
        ( [CmmDeclSRTs] -- The SRT object(s)
        , SRTEntry      -- label to use in the info table
        )
buildSRTChain profile cafSet =
  case splitAt mAX_SRT_SIZE cafSet of
    ([], _) -> panic "buildSRT: empty"
    (these, []) -> do
      (decl,lbl) <- buildSRT profile these
      return ([decl], lbl)
    (this:these,those) -> do
      (rest, rest_lbl) <- buildSRTChain profile (this : those)
      (decl,lbl) <- buildSRT profile (rest_lbl : these)
      return (decl:rest, lbl)
  where
    mAX_SRT_SIZE = 16


buildSRT :: Profile -> [SRTEntry] -> UniqDSM (CmmDeclSRTs, SRTEntry)
buildSRT profile refs = do
  id <- getUniqueDSM
  let
    lbl = mkSRTLabel id
    platform = profilePlatform profile
    srt_n_info = mkSRTInfoLabel (length refs)
    fields =
      mkStaticClosure profile srt_n_info dontCareCCS
        [ CmmLabel lbl | SRTEntry lbl <- refs ]
        [] -- no padding
        [mkIntCLit platform 0] -- link field
        [] -- no saved info
        [] -- no extras
  return (mkDataLits (Section Data lbl) lbl fields, SRTEntry lbl)

-- | Update info tables with references to their SRTs. Also generate
-- static closures, splicing in SRT fields as necessary.
updInfoSRTs
  :: Profile
  -> NonDet.LabelMap CLabel               -- ^ SRT labels for each block
  -> NonDet.LabelMap [SRTEntry]           -- ^ SRTs to merge into FUN_STATIC closures
  -> CafInfo                       -- ^ Whether the CmmDecl's group has CAF references
  -> CmmDecl
  -> [CmmDeclSRTs]

updInfoSRTs _ _ _ _ (CmmData s (CmmStaticsRaw lbl statics))
  = [CmmData s (CmmStaticsRaw lbl statics)]

updInfoSRTs profile _ _ caffy (CmmData s (CmmStatics lbl itbl ccs payload extras))
  = [CmmData s (CmmStaticsRaw lbl (map CmmStaticLit field_lits))]
  where
    field_lits = mkStaticClosureFields profile itbl ccs caffy payload extras

updInfoSRTs profile srt_env funSRTEnv caffy (CmmProc top_info top_l live g)
  | Just (_,closure) <- maybeStaticClosure = [ proc, closure ]
  | otherwise = [ proc ]
  where
    proc = CmmProc top_info { info_tbls = newTopInfo } top_l live g
    newTopInfo = Det.mapMapWithKey updInfoTbl (info_tbls top_info)
    updInfoTbl l info_tbl
      | l == g_entry g, Just (inf, _) <- maybeStaticClosure = inf
      | otherwise  = info_tbl { cit_srt = NonDet.mapLookup l srt_env }

    -- Generate static closures [FUN].  Note that this also generates
    -- static closures for thunks (CAFs), because it's easier to treat
    -- them uniformly in the code generator.
    maybeStaticClosure :: Maybe (CmmInfoTable, CmmDeclSRTs)
    maybeStaticClosure
      | Just info_tbl@CmmInfoTable{..} <-
           Det.mapLookup (g_entry g) (info_tbls top_info)
      , Just (id, ccs) <- cit_clo
      , isStaticRep cit_rep =
        let
          (newInfo, srtEntries) = case NonDet.mapLookup (g_entry g) funSRTEnv of
            Nothing ->
              -- if we don't add SRT entries to this closure, then we
              -- want to set the srt field in its info table as usual
              (info_tbl { cit_srt = NonDet.mapLookup (g_entry g) srt_env }, [])
            Just srtEntries -> srtTrace "maybeStaticFun" (pdoc (profilePlatform profile) res)
              (info_tbl { cit_rep = new_rep }, res)
              where res = [ CmmLabel lbl | SRTEntry lbl <- srtEntries ]
          fields = mkStaticClosureFields profile info_tbl ccs caffy srtEntries []
          new_rep = case cit_rep of
             HeapRep sta ptrs nptrs ty ->
               HeapRep sta (ptrs + length srtEntries) nptrs ty
             _other -> panic "maybeStaticFun"
          lbl = mkClosureLabel (idName id) caffy
        in
          Just (newInfo, mkDataLits (Section Data lbl) lbl fields)
      | otherwise = Nothing


srtTrace :: String -> SDoc -> b -> b
-- srtTrace = pprTrace
srtTrace _ _ b = b

srtTraceM :: Applicative f => String -> SDoc -> f ()
srtTraceM str doc = srtTrace str doc (pure ())
