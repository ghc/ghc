{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

--
-- (c) 2021 Benjamin Maurer
--

module GHC.CmmToAsm.SSA (
        SsaLiveCmmDecl,
        prunedSSAFromLiveCmmDecl,
        cssaToLiveCmmDecl,

        fromBlockLookupTable,
        SSABasicBlock(..), PhiFun(..),

        pprSsaLiveCmmDecl
) where

import GHC.Prelude

import Control.Monad (foldM)

import Data.Bifunctor (Bifunctor (bimap))
import Data.Containers.ListUtils (nubOrd)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (intersect)
import Data.Maybe
import Data.Tree (Tree)
import qualified Data.Tree as T
import Data.Tuple (swap)

import GHC.Cmm (GenCmmDecl(..), GenBasicBlock(..))
import GHC.Cmm.BlockId (BlockId, mkBlockId)
import GHC.Cmm.Dataflow.Collections
    ( IsMap(..), IsSet(..) )
import GHC.Cmm.Dataflow.Label (LabelMap, LabelSet)

import GHC.CmmToAsm.CFG
import GHC.CmmToAsm.CFG.Dominators
    ( Node, idom, fromEdges, asTree )
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Reg.Liveness

import GHC.Data.Graph.Directed (SCC(..))

import GHC.Platform (Platform)
import GHC.Platform.Reg (isVirtualReg, Reg (RegVirtual, RegReal), renameVirtualReg)

import GHC.Types.Unique
import GHC.Types.Unique.DFM
import GHC.Types.Unique.FM
import GHC.Types.Unique.SDFM
import GHC.Types.Unique.Set
import GHC.Types.Unique.Supply (UniqSM, UniqSupply, takeUniqFromSupply, getUniqueSupplyM)

import GHC.Utils.Misc (HasDebugCallStack)
import GHC.Utils.Monad.State
import GHC.Utils.Outputable
import GHC.Utils.Panic (panic)

-- Debugging only
-- import GHC.Driver.Ppr


-- | Wrapping SSA-Blocks in Top Level CmmDecl
type SsaLiveCmmDecl statics instr
        = GenCmmDecl
                statics
                LiveInfo
                (BlockLookupTable instr)


-- | Map of register to blocks in which it is defined.
--   This should only ever contain virtual regs.
type DefSites = RegMap (UniqSet BlockId)

-- | List of pair of node and its immediate dominators
type IDomList = [(Node, Node)]

type BlockLookupTable instr
        = UniqDFM BlockId (SccBits (SSABasicBlock instr))


-- Store blocks with additional info to be able to rebuild SCCs
-- CyclicBit carries number of CyclicSCC to distinguish consecutive
-- CyclicSCCs.
data SccBits a
        = AcyclicBit a | CyclicBit Int a
        deriving (Functor)


lookupCyclicBitNumber :: SccBits a -> Maybe Int
lookupCyclicBitNumber sccBit
        | (CyclicBit n _)
                <- sccBit       = Just n
        | otherwise             = Nothing


sccBitBlock :: SccBits a -> a
sccBitBlock (AcyclicBit b)  = b
sccBitBlock (CyclicBit _ b) = b


type RegId = Unique

-- | Representing Phi-function, i.e., parallel copies to new name.
data PhiFun = PhiF RegId RegId [RegId]
              -- ^ oldId newId args

phiDef :: PhiFun -> RegId
phiDef (PhiF _ def _) = def


-- | Wrapping a LiveBasicBlock to add Phi-Functions
data SSABasicBlock instr
        = SSABB [PhiFun] (LiveBasicBlock instr)
        deriving (Functor)


mkSSABasicBlock
        :: Instruction instr
        => BlockMap [PhiFun]
        -> LiveBasicBlock instr
        -> SSABasicBlock instr

mkSSABasicBlock phis liveBB@(BasicBlock i _)
 = let bbPhis = mapFindWithDefault [] i phis
   in  SSABB bbPhis liveBB


ssaBBlockId :: SSABasicBlock instr -> BlockId
ssaBBlockId (SSABB _ (BasicBlock bid _)) = bid


ssaBBlockPhiFuns :: SSABasicBlock instr -> [PhiFun]
ssaBBlockPhiFuns (SSABB phis _) = phis


ssaBBlockLiveBB :: SSABasicBlock instr -> LiveBasicBlock instr
ssaBBlockLiveBB (SSABB _ li) = li


{- Note [ASM-SSA construction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*) Phi-Function representation

*) 2-Address and modifying instructions

*) Liveness information

-}

prunedSSAFromLiveCmmDecl
        :: (HasDebugCallStack, Instruction instr)
        => Platform
        -> CFG
        -> LiveCmmDecl statics instr
        -> UniqSM (SsaLiveCmmDecl statics instr)

prunedSSAFromLiveCmmDecl platform cfg liveCmmDecl
 = do
        -- Grab the unique supply from the monad.
        us              <- getUniqueSupplyM
        let ssaCmmDecl  =
                evalState (constructPrunedSSA platform cfg liveCmmDecl)
                         (initRenameS us)

        return (ssaCmmDecl)


------- SSA construction ----------------------------------

constructPrunedSSA
        :: (HasDebugCallStack, Instruction instr)
        => Platform
        -> CFG
        -> LiveCmmDecl statics instr
        -> RenameM (SsaLiveCmmDecl statics instr)

constructPrunedSSA _ _ (CmmData i d)
        = return (CmmData i d)

constructPrunedSSA _ _ (CmmProc liveInfo lbl live [])
        = return (CmmProc liveInfo lbl live emptyUDFM)

constructPrunedSSA platform cfg (CmmProc liveInfo lbl live sccs)
 | LiveInfo info entries liveVRegsOnEntry liveStackSlots <- liveInfo
 = do
         let entry      = if not $ null entries
                          then head entries
                          else panic "GHC.CmmToAsm.SSA.toPrunedSSA: No proc entry points!"
         let defsites   = calcDefSites platform sccs

         -- -- DEBUG
         -- (pprTraceIt "Defsites: " defsites) `seq` return ()

         -- Bring CFG in shape for dom. Taken from GHC.CmmToAsm.CFG.loopInfo
         -- TODO: Refactor - extract to Utils or something
         let graph          = fmap (setFromList . mapKeys ) cfg :: LabelMap LabelSet
         let rooted         = ( fromBlockId entry
                              , toIntMap $ fmap toIntSet graph) :: (Node, IntMap IntSet)

         -- All immediate dominators
         let idomList   = idom rooted
         let ssaSccs    = {-# SCC "placePhis" #-}
                 placePhis cfg idomList defsites liveVRegsOnEntry sccs

         let blkTbl     = toBlockLookupTable ssaSccs
         let domTree    = domTreeFromList entry idomList

         (blkTblRenamed, liveInsRenamed)
                <- {-# SCC "renameVars" #-}
                   renameVars platform cfg blkTbl liveVRegsOnEntry domTree

         return (CmmProc
                        (LiveInfo info entries liveInsRenamed liveStackSlots)
                        lbl live blkTblRenamed)

toIntSet :: LabelSet -> IntSet
toIntSet s     = IS.fromList . map fromBlockId . setElems $ s

toIntMap :: LabelMap a -> IntMap a
toIntMap m     = IM.fromList $ map (\(x,y) -> (fromBlockId x,y)) $ mapToList m


-- Adapted from "Engineering a Compiler" by Cooper, Torczon
placePhis
        :: Instruction instr
        => CFG                  -- ^ Control Flow Graph
        -> IDomList             -- ^ List of pre-computed immediate dominator for each node
        -> DefSites             -- ^ Map of vregs to blocks where they are defined
        -> BlockMap RegSet      -- ^ Live_in sets
        -> [SCC (LiveBasicBlock instr)]
        -> [SCC (SSABasicBlock instr)]

placePhis cfg idomList defsites liveIns sccs
 = map (fmap (mkSSABasicBlock allPhis)) sccs
 where
         -- 'Globals' as in alive outside of block with definition.
         globals = filterUniqSet isVirtualReg
                 $ unionManyUniqSets
                 $ mapElems liveIns

         -- Global to its definition sites
         globalDefs = intersectUFM defsites $ getUniqSet globals

         -- Dominance Frontier of node n
         df n = mapFindWithDefault [] n
              $ domFrontiers cfg idomList

         -- Map of Blocks to Phi-Funcs to insert
         allPhis = foldl' overVar mapEmpty $ nonDetUFMToList
                 -- $ pprTraceIt "globalDefs: "
                globalDefs
         -- ^ See Note [Unique Determinism and code generation]

         -- Top level fun for mkVarPhi
         overVar !phis (u, defs) = mkVarPhi phis setEmpty u
                                 $ nonDetEltsUniqSet defs
         -- ^ See Note [Unique Determinism and code generation]

         -- Makes all Phi-Funcs for a variable
         mkVarPhi :: BlockMap [PhiFun] -> LabelSet -> RegId -> [BlockId] -> BlockMap [PhiFun]
         mkVarPhi phis _ _ []           = phis
         mkVarPhi phis visited u (b:bs)
                = let visited'    = setInsert b visited
                      (phis', wl) = overDF phis visited' u b
                  in  mkVarPhi phis' visited' u (wl ++ bs)

         -- Function over DF(n), adding Phi-Funcs to blocks and
         -- return work-list with new nodes to visit
         overDF phis visited u n =
                foldl'
                (\(ph, wl) x
                        -> (if isLive    u x       then mkBlockPhi ph u x else ph,
                            if setMember x visited then wl                else x:wl))
                (phis, [])
                $ df n

         -- Helper to create Phi stub, i.e.,
         -- list of names and new def name will be added in rename
         mkPhiFun u      = PhiF u u []

         -- Make Phi-Function for one var in one block
         mkBlockPhi (phis :: BlockMap [PhiFun]) u y
                = mapAlter
                        (\mx -> let newph = mkPhiFun u
                                in case mx of
                                        Just pfs -> Just (if present u pfs then pfs else newph:pfs)
                                        Nothing  -> Just [newph])
                        y phis

         -- Check if Phi for this unique already present
         present u = any (\(PhiF oldId _ _) -> oldId == u)

         -- Is vreg live in block b? We don't want to insert dead Phi-nodes
         isLive u b = maybe False (elemUniqSet_Directly u) (mapLookup b liveIns)


adjustUDFM_M
        :: Monad m
        => ((SccBits (SSABasicBlock instr)) -> m ((SccBits (SSABasicBlock instr))))
        -> BlockLookupTable instr
        -> BlockId
        -> m (BlockLookupTable instr)
adjustUDFM_M f tbl bid = sequence $ adjustUDFM (f =<<) (mapUDFM return tbl) bid


-- | We iterate over all blocks to propagate reaching definitions while
--   introducing new names for defs and renaming all uses, also filling in
--   the Phi-Functions.
--
--   Vars in SSA would normally be named by subscript, e.g., a_0, a_1, a_2,
--   but we have uniques, so we just generate new ones. The only downside is,
--   that it's harder to debug just looking at the code.
--
--   Adapted from "Engineering a Compiler" by Cooper, Torczon
renameVars
        :: Instruction instr
        => Platform
        -> CFG                          -- ^ Control Flow Graph
        -> BlockLookupTable instr       -- ^ Lookup table for random access to blocks
        -> BlockMap RegSet              -- ^ Live_in sets
        -> Tree Node                    -- ^ Precomputed Dominator Tree
                -- | Result: Live_in sets with new definitions, blocks with renamed vars
        -> RenameM (BlockLookupTable instr, BlockMap RegSet)

renameVars platform cfg blkTbl liveIns (T.Node n sub)
 = do
        enterScope

        let bid = toBlockId n

        -- Update Live_in set by applying renames
        rd           <- gets stateReachingDef
        let liveIns1 = mapAdjust (
                mapUniqSet (replaceReg $ (peekDef rd) . getUnique))
                bid liveIns

        blkTbl1 <- adjustUDFM_M
                        (renameVars_block platform)
                        blkTbl
                        bid

        blkTbl2 <- addSuccessorPhiArgs cfg blkTbl1 bid

        -- Update Live_in part 2: Now that defintion of Phi has been renamed.
        -- With "Phi functions are parallel copies"-semantics, given a_n = Phi(a_0,...),
        -- a_n is in Live_in of Block b_n and for k < n, a_k is in Live_out of Block b_k
        let mBlk     = lookupUDFM blkTbl bid
        let mPhis    = fmap (ssaBBlockPhiFuns . sccBitBlock) mBlk
        let liveIns2 = maybe liveIns
                        (\phis -> mapAdjust (\rs -> foldl' mergeMaybe rs phis) bid liveIns1)
                        mPhis

        -- Recursing into successors in DomTree, i.e., DFS pre-order iteration
        (blkTbl_updated, liveIns_updated)
                <- if null sub
                        then return (blkTbl2, liveIns2)
                        else foldM
                                (\(bs, ls) tr -> renameVars platform cfg bs ls tr)
                                (blkTbl2, liveIns2)
                                sub

        leaveScope

        return (blkTbl_updated, liveIns_updated)
 where
         mergeMaybe rs phi = fromMaybe rs (mergePhiToLiveSet rs phi)


renameVars_block
        :: Instruction instr
        => Platform
        -> SccBits (SSABasicBlock instr)        -- ^ Block to apply renames to
        -> RenameM (SccBits (SSABasicBlock instr))

renameVars_block platform sccBit
 | (SSABB phis (BasicBlock bid ins))    <- sccBitBlock sccBit
 = let
        patchPhi (PhiF old _ args) nu    = PhiF old nu args
   in  do
           -- Rename defs of phis and add to reaching definitions
           phis'        <- mapM (\p -> (patchPhi p) <$> (newDef $ phiDef p)) phis

           -- Rename instructions
           ins'         <- mapM (renameVars_instr platform) ins

           return $ fmap (const (SSABB phis' (BasicBlock bid ins'))) sccBit


-- | Fill in Phi arguments in successors
addSuccessorPhiArgs
        :: CFG                                  -- ^ Control Flow Graph
        -> BlockLookupTable instr               -- ^ Lookup table for random access to blocks
        -> BlockId
        -> RenameM (BlockLookupTable instr)
addSuccessorPhiArgs cfg blkTbl bid
 = do
         let succs    =  getSuccessors cfg bid
         rds          <- gets stateReachingDef
         let blkTbl'  =  foldl' (adjustUDFM $ fmap (mapOverPhis (insertPhiArg rds))) blkTbl succs
         return blkTbl'
 where
         mapOverPhis f (SSABB phis blk) = SSABB (map f phis) blk


-- | Replace old definition with new def of Phi function in Live Set.
mergePhiToLiveSet :: HasDebugCallStack => RegSet -> PhiFun -> Maybe (RegSet)
mergePhiToLiveSet rsLive (PhiF old new _)
 = do
         -- Get old entry
         oldReg         <- lookupUniqSet_Directly rsLive old
         let rsl1       = delOneFromUniqSet_Directly rsLive old
         let newReg     = renameVirtualRegOrPanic oldReg new
         let rsl2       = addOneToUniqSet rsl1 newReg

         return rsl2


-- | Helper function to rename reg when we know it has to be a virtual reg.
renameVirtualRegOrPanic :: HasDebugCallStack => Reg -> RegId -> Reg
renameVirtualRegOrPanic reg nu = case reg of
                 RegVirtual vr  -> RegVirtual (renameVirtualReg nu vr)
                 RegReal{}      -> panic $ "GHC.CmmToAsm.SSA.renameVirtualRegOrPanic: \
                                        \Expected virtual reg, got "
                                        ++ show reg ++ ", new unique: "++ show nu


-- | Insert argument def_(n-1) into phi function:
--       def_n = \Phi(def_0,..,def_(n-1))
--
--   The original name of a vreg is used to lookup the current reaching definition.
--   We rely on the fact, that each node is visited once, so we can just prepend
--   ths definition to args.
insertPhiArg :: UniqFM RegId [RegId] -> PhiFun -> PhiFun
insertPhiArg rds phi@(PhiF old def args)
 = let  rd              = peekDef rds old
   in   maybe phi (\x -> PhiF old def (x:args)) rd


-- | Renaming step over instruction. Replace uses with reaching definitions,
--   introduce new names for definitions and store them in reaching definitions.
--
--   See Note [ASM-SSA construction] for caveat about modifying/2-Addr. instructions
renameVars_instr
        :: Instruction instr
        => Platform
        -> LiveInstr instr
        -> RenameM (LiveInstr instr)

renameVars_instr platform (LiveInstr instr mLiveness)
 = do
         let RU rlRead rlWritten = regUsageOfInstr platform instr
         let rsRead              = filterUniqSet isVirtualReg $ mkUniqSet rlRead
         let rsWritten           = filterUniqSet isVirtualReg $ mkUniqSet rlWritten
         let rsModified          = rsRead `intersectUniqSets` rsWritten
         let rsUsed              = rsRead `unionUniqSets` rsWritten

         -- Excluding modified vregs from definitions, i.e., 2-Addr instructions
         -- and things like `inc x`, bc. we can't use 2 separate names for these.
         let defs                = filterUniqSet isVirtualReg
                                 $ maybe emptyUniqSet
                                        (\l -> liveBorn l `minusUniqSet` rsModified) mLiveness

         -- Rename uses
         -- No non-determinism, See Note [Unique Determinism and code generation]
         mRenames        <- mapM getRenamePair $ nonDetEltsUniqSet (rsUsed `minusUniqSet` defs)
         let renames     = catMaybes mRenames
         let instr_use   = foldl' (uncurry . patchInstr) instr renames

         -- Introduce new names for definitions and save them to state
         -- No non-determinism, See Note [Unique Determinism and code generation]
         defRenames      <- mapM (newVregDef) $ nonDetEltsUniqSet defs
         let instr_def   = foldl' (uncurry . patchInstr) instr_use defRenames

         -- Update Liveness info
         s <- get
         let mLiveness' = updateLiveness (\r -> evalState (reachingDef $ getUnique r) s) <$> mLiveness

         return (LiveInstr instr_def mLiveness')
 where
         getRenamePair r = fmap (r,) <$> (reachingDef $ getUnique r)

         newVregDef r
                = let ru = getUnique r
                  in case r of
                        RegVirtual{}   -> (r,) <$> newDef ru
                        RegReal{}      -> return (r, ru)


-- | Update instructon liveness info
--   No non-determinism, See Note [Unique Determinism and code generation]
updateLiveness :: (Reg -> Maybe RegId) -> Liveness -> Liveness
updateLiveness renames (Liveness born dieRead dieWrite)
 = let upd = mkUniqSet . map (replaceReg renames) . nonDetEltsUniqSet
   in  Liveness (upd born) (upd dieRead) (upd dieWrite)


-- | Patch reg in instruction. Adapted from Spill.hs
patchInstr
        :: Instruction instr
        => instr -> Reg -> Unique -> instr

patchInstr instr reg nu
 = case reg of
        RegVirtual vr
                -> patchReg reg (RegVirtual (renameVirtualReg nu vr)) instr

        RegReal{}
                -> instr


-- Taken from Spill.hs, should also extract
patchReg
        :: Instruction instr
        => Reg -> Reg -> instr -> instr

patchReg old new instr
 = let  patchF r
                | r == old      = new
                | otherwise     = r
   in   patchRegsOfInstr instr patchF


-- | Helper function to replace unique in reg if in given lookup table.
replaceReg :: (Reg -> Maybe RegId) -> Reg -> Reg
replaceReg _ old@(RegReal{}) = old
replaceReg renames old@(RegVirtual vr)
 = case renames old of
         Just nu        -> RegVirtual (renameVirtualReg nu vr)
         Nothing        -> old


------- SSA destruction -----------------------------------


-- | Precondition: MUST be in conventional SSA, otherwise transformation is *wrong*.
--   Since CSSA means, that vars connected by Phi-functions don't interfere,
--   they can simply be renamed to one variable.
--   This is **not correct** if transformations have been applied and SSA is
--   no longer conventional. In that case, copies may be needed.
cssaToLiveCmmDecl
        :: (HasDebugCallStack, Instruction instr)
        => Platform
        -> SsaLiveCmmDecl statics instr
        -> LiveCmmDecl statics instr

cssaToLiveCmmDecl _ (CmmData i d)
 = CmmData i d

cssaToLiveCmmDecl platform (CmmProc liveInfo lbl live blkTbl)
 | LiveInfo info entries liveVRegsOnEntry liveStackSlots <- liveInfo
 = let phiWebs  = discoverPhiWebs blkTbl
       liveIns' = mapMap (mapUniqSet $ replaceReg ((lookupUSDFM phiWebs) . getUnique)) liveVRegsOnEntry
       info'    = LiveInfo info entries liveIns' liveStackSlots
       blkTbl'  = mapUDFM (renamePhiWebs platform phiWebs) blkTbl
       sccs     = map (fmap ssaBBlockLiveBB) $ fromBlockLookupTable blkTbl'
   in  (CmmProc info' lbl live sccs)


-- | Given conventional SSA form, perform union-find on Phi-functions to
--   build webs.
discoverPhiWebs
        :: BlockLookupTable instr
        -> UniqSDFM RegId RegId         -- ^ Disjoint-set: Names in web to new name

discoverPhiWebs blkTbl
 = let bitPhis ssaBit = ssaBBlockPhiFuns $ sccBitBlock ssaBit
       unionNames dset ssaBit = foldl' unionPhi dset $ bitPhis ssaBit
       unionPhi dset (PhiF _ new args)
        = foldl' (\ds k -> snd $ equateUSDFM ds k new) (addToUSDFM dset new new) args
   in  foldl' unionNames emptyUSDFM $ eltsUDFM blkTbl


renamePhiWebs
        :: Instruction instr
        => Platform
        -> UniqSDFM RegId RegId                 -- ^ Disjoint-set of vreg uniques
        -> SccBits (SSABasicBlock instr)        -- ^ Block to apply renames to
        -> SccBits (SSABasicBlock instr)

renamePhiWebs platform dset sccBit
 | (SSABB phis (BasicBlock bid ins))    <- sccBitBlock sccBit
 = let ins' = map (renamePhiWebs_instr platform dset) ins
   in (SSABB phis (BasicBlock bid ins')) <$ sccBit


-- | Rename all occurences of vregs in the instruction with
--   the new name of their respective Phi-Web.
renamePhiWebs_instr
        :: Instruction instr
        => Platform
        -> UniqSDFM RegId RegId -- ^ Disjoint-set of vreg uniques
        -> LiveInstr instr
        -> LiveInstr instr

renamePhiWebs_instr platform dset (LiveInstr instr mLiveness)
 = let  RU rlRead rlWritten
                   = regUsageOfInstr platform instr
        rsUsed     = nubOrd $ filter isVirtualReg (rlRead ++ rlWritten)
        rsRenames  = mapMaybe (\r -> (r,) <$> (lookupUSDFM dset $ getUnique r)) rsUsed
        instr'     = foldl' (uncurry . patchInstr) instr rsRenames
        mLiveness' = updateLiveness (lookupUFM $ listToUFM rsRenames) <$> mLiveness

   in   LiveInstr instr' mLiveness'


------ Block Lookup Tabe ----------------------------------

-- | We need to iterate over blocks in dfs pre-order of dominator tree,
--   but we have a list of SCCs...
--   The SCCs are flattened and put into a map for random access,
--   but the SccBits and deterministic nature of UDFS allow us to rebuild them later.
toBlockLookupTable
        :: [SCC (SSABasicBlock instr)]
        -> BlockLookupTable instr

toBlockLookupTable sccs = addListToUDFM emptyUDFM $ concat $ go (0 :: Int) sccs
 where
         go _ []  = []
         go cnt ((AcyclicSCC v):sccs) = [(ssaBBlockId v, AcyclicBit v)] : go cnt sccs
         go cnt ((CyclicSCC vs):sccs) = map (\v -> (ssaBBlockId v, CyclicBit cnt v)) vs : go (succ cnt) sccs


-- | Turn lookup table back into list of SCCs
fromBlockLookupTable
        :: BlockLookupTable instr
        -> [SCC (SSABasicBlock instr)]

fromBlockLookupTable tbl = go $ eltsUDFM tbl
 where
         go []                        = []
         go ((AcyclicBit b):bs)       = (AcyclicSCC b) : go bs
         go all@((CyclicBit n _):_)
                = let (vs, rest) = span (\x -> lookupCyclicBitNumber x == Just n) all
                  in  (CyclicSCC (map sccBitBlock vs)) : go rest


-- | Map all vregs to blocks where they are defined.
calcDefSites
        :: Instruction instr
        => Platform
        -> [SCC (LiveBasicBlock instr)]
        -> DefSites

calcDefSites platform sccs
        | null sccs = emptyUFM
        | otherwise = go emptyUFM sccs
 where
        go defs []  = defs
        go defs (scc:sccs)
                | AcyclicSCC blk <- scc = merge (slurpDefs platform defs blk) (go defs sccs)
                | CyclicSCC blks <- scc = foldl' (slurpDefs platform) defs blks
                                                `merge` (go defs sccs)

        merge = plusUFM_C unionUniqSets


-- | Slurp out value definitions from block, i.e., which vregs are born
--   in this block. Note that modifying instructions are ignored, as we
--   can't introduce a new name for them.
slurpDefs
        :: Instruction instr
        => Platform
        -> DefSites
        -> LiveBasicBlock instr
        -> DefSites

slurpDefs platfrom defs (BasicBlock bid instrs)
 = foldl' (\ds i -> (addDefs ds) $ findDefs i) defs instrs
 where
         modified i
                | RU rsRead rsWritten <- regUsageOfInstr platfrom i
                                          = rsRead `intersect` rsWritten

        -- Order of defs irrelevant. See Note [Unique Determinism and code generation]
         findDefs (LiveInstr i mLiveness) = nonDetEltsUniqSet
                                          $ filterUniqSet isVirtualReg
                                          $ delListFromUniqSet
                                                (maybe emptyUniqSet liveBorn mLiveness)
                                                (modified i)

         addDefs oldDefs newDefs = plusUFM_C unionUniqSets oldDefs
                                 $ zipToUFM newDefs (repeat $ unitUniqSet bid)


-- | Calculate dominance frontier for each node in graph.
-- TODO: Maybe implement this internally in Dominators, s.t. we can reuse the internal state
domFrontiers
        :: HasDebugCallStack
        => CFG          -- ^ Control Flow Graph
        -> IDomList     -- ^ List of pre-computed immediate dominator for each node
        -> BlockMap [BlockId]
domFrontiers cfg idomList
 = let
         cfgNodeIds     = getCfgNodes cfg
         idoms          = IM.fromList idomList

         df_inner n df r = if Just r == (IM.lookup n idoms)
                           then df
                           else let df' = (IM.insertWith (++) r [n] df)
                                in case IM.lookup r idoms of
                                        Nothing -> df'
                                        Just r' -> df_inner n df' r'

         df_preds df bid
                = let preds = map fromBlockId $ getPredecessors cfg bid
                      n     = fromBlockId bid
                  in  if length preds <= 1
                      then df
                      else foldl' (df_inner n) df preds

         dominanceFrontiers = foldl' df_preds IM.empty cfgNodeIds

   in    toBlockMap $ IM.toList dominanceFrontiers


-- | Build the dominator tree from given list of immediate dominators
--   (Adapted from GHC.CmmToAsm.CFG.Dominators.domTree)
domTreeFromList :: BlockId -> IDomList -> Tree Node
domTreeFromList root idomList
 = let r  = fromBlockId root
       is = filter ((/=r).fst) idomList
       tg = fromEdges (fmap swap is)
   in asTree (r,tg)


getPredecessors :: CFG -> BlockId -> [BlockId]
getPredecessors cfg n = let reverseCfg = reverseEdges cfg
                        in  getSuccessors reverseCfg n

-- TODO refactor to Utils
fromBlockId :: BlockId -> Int
fromBlockId= getKey . getUnique

toBlockId :: Int -> BlockId
toBlockId = mkBlockId . mkUniqueGrimily

toBlockMap :: [(Int, [Int])] -> BlockMap [BlockId]
toBlockMap xs = mapFromList $ map (bimap toBlockId (map toBlockId)) xs

-- Rename Monad -----------------------------------------------------
-- TODO: add Stats
type RenameM a
        = State RenameS a

-- | SSA Renamer code generator state.
data RenameS
        = RenameS
        { -- | Unique supply for generating fresh vregs.
          stateUS           :: UniqSupply

          -- | Current new name for original vreg
        , stateReachingDef  :: UniqFM RegId [RegId]

          -- | New definitions in current scope. Used to pop defs in rename.
        , stateScopedDefs   :: [[RegId]] }


-- | Create a new renamer state.
initRenameS :: UniqSupply -> RenameS
initRenameS uniqueSupply
        = RenameS
        { stateUS           = uniqueSupply
        , stateReachingDef  = emptyUFM
        , stateScopedDefs   = [] }


-- | Allocate a new unique in the renamer monad.
newUnique :: RenameM Unique
newUnique
 = do   us      <- gets stateUS
        case takeUniqFromSupply us of
         (uniq, us')
          -> do modify $ \s -> s { stateUS = us' }
                return uniq


-- | Create a new name for a definition with given old RegId
--   and update reaching definition.
newDef :: RegId -> RenameM (RegId)
newDef old
 = do
         nUnique        <- newUnique
         modify $ \s -> s { stateReachingDef
                = alterUFM_Directly (pushDef nUnique) (stateReachingDef s) old
                , stateScopedDefs = addTop (stateScopedDefs s) old }
         return (nUnique)
 where
         addTop (xs:xss) y = ((y:xs):xss)
         addTop [] _       = panic "GHC.CmmToAsm.SSA.newDef: Error, no open scope!"


-- | Entering a scope means adding a new scope stack
enterScope :: RenameM ()
enterScope = modify $ \s -> s {
        stateScopedDefs = []:(stateScopedDefs s)
}

-- | Leaving a scope means popping all reaching definitions defined in this scope.
leaveScope :: RenameM ()
leaveScope = modify $ \s -> s {
        stateReachingDef = foldl' popDef (stateReachingDef s) $ head (stateScopedDefs s),
        stateScopedDefs  = tail $ stateScopedDefs s
}

-- | Return current reaching definition of 'old' or Nothing.
reachingDef :: RegId -> RenameM (Maybe RegId)
reachingDef old
 = do
         rd             <- gets stateReachingDef
         let mStack     = lookupUFM_Directly rd old
         let res        = mStack >>= listToMaybe
         return res


-- | Will return the current reaching name of 'old', or 'old'
reachingDefOrDefault :: RegId -> RenameM RegId
reachingDefOrDefault old
 = do
         rd             <- gets stateReachingDef
         let stack      = lookupWithDefaultUFM_Directly rd [] old
         let res        = if null stack then old else head stack
         return res


-- | Push new definition onto name stack if exists, or create new one.
pushDef :: RegId -> Maybe [RegId] -> Maybe [RegId]
pushDef x mXs = case mXs of
        Just xs -> Just $ x:xs
        Nothing -> Just [x]


-- | Remove top definition for given name from name stack.
popDef :: UniqFM RegId [RegId] -> RegId -> UniqFM RegId [RegId]
popDef mp x = adjustUFM_Directly (drop 1) mp x


-- | Get top definition for given name from name stack, if exists.
peekDef :: UniqFM RegId [RegId] -> RegId -> Maybe RegId
peekDef mp k = let mStack = lookupUFM_Directly mp k
               in  mStack >>= (\s -> if null s then Nothing else Just $ head s)


-- Pretty printing machinery

instance Outputable PhiFun where
        ppr (PhiF old new args)
                = hsep [(ppr new), equals, phi
                       , parens $ hcat (punctuate comma $ map ppr args)
                       , space, brackets (text "old: " <> ppr old)]
                where phi = unicodeSyntax (char 'φ') (text "Phi")


instance (Instruction instr) => OutputableP Platform (SSABasicBlock instr) where
        pdoc platform (SSABB phis liveBB)
            =  text "Phi-Functions:"
            $$ nest 8 (vcat (map ppr phis))
            $$ ppr (fmap (fmap (pprInstr platform)) liveBB)


pprSsaLiveCmmDecl
    :: (OutputableP Platform statics, Instruction instr)
    => Platform
    -> SsaLiveCmmDecl statics instr
    -> SDoc

pprSsaLiveCmmDecl platform ssaThing
    = let pprSsaBBs = fmap (eltsUDFM . mapUDFM (sccBitBlock . fmap (pdoc platform)))
      in  pdoc platform (pprSsaBBs ssaThing)
