{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

--
-- (c) 2021 Benjamin Maurer
--

module GHC.CmmToAsm.SSA (
    SsaCmmDecl, SSABasicBlock(..),
    PhiFun(..),
    cmmSsaTransformAll,
    cmmSsaTransform,
    cssaToNatCmmDecl,
    pprSsaCmmDecl
) where

import GHC.Prelude

import Control.Monad (when)

import Data.Function (on)
import Data.List (nub, sortBy, groupBy)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, fromJust)

import GHC.Cmm (GenCmmDecl(..))
import GHC.Cmm.BlockId (BlockId)
import GHC.Cmm.Dataflow.Collections (mapEmpty)
import GHC.Cmm.Dataflow.Label (LabelMap)

import GHC.CmmToAsm.CFG
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Types

import GHC.Platform (Platform)
import GHC.Platform.Reg (isVirtualReg, Reg (..), renameVirtualReg)

import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.SDFM
import GHC.Types.Unique.Set

import GHC.Utils.Misc (HasDebugCallStack, notNull)
import GHC.Utils.Monad (allM, andM, whenM)
import GHC.Utils.Monad.State.Strict
    (execState, gets, modify, State)
import GHC.Utils.Outputable
import GHC.Utils.Panic (panic, pprPanic)
import GHC.Utils.Panic.Plain (assert)

-- Debugging only
-- import GHC.Driver.Ppr (pprTrace, pprTraceIt)


{- Note [ASM-SSA construction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Transform the machine code into (pseudo) Single Static Assignment (SSA) form.
Each definition introduces a new name and Phi nodes/functions merge multiple
incoming names for the same "variable" at join points.

While SSA enables many optimizations in an efficient manner,
the main motivation to implement this was renaming disjoint webs.
When lowering Cmm, the same virtual register name will be emitted for what
ends up being disjoint def-use chains. This constrains them to be assigned
the same CPU register by the graph coloring register allocator, for no reason.
Constructing and then destructing SSA is one way to identify and rename webs,
while enabling further optimizations.

*) Phi-Function representation

   Phi functions are not represented in the instruction stream, but instead as
   fields of the SSABasicBlock. This avoids having yet another IR or adding
   pseudo-instructions and the need to scan the block for Phi functions.
   Semantically, Phi functions are thought to be executed in parallel before
   entering the block anyway.

*) 2-Address, modifying and conditional instructions

   The defining property of SSA is, that each assignment of a value creates a
   new name and that each definition of a name dominates all its uses.
   Modifying and 2-Address-Instructions present a problem here. Strictly speaking,
   we would have to split them up and consider register constraints in destruction.

   For simplicitly, modifications are not considered defintions and won't introduce
   a new name. This is fine for the time being and the purpose of identifying webs
   and will lead to correct code on destruction.
   It will, most likely, impede certain optimizations, i.e., for any code motion
   the relative location of these instructions must be pinned.

   Pre-colored instructions, i.e., hardware registers are also left as-is.

*) Algorithm

   The implementation is based on
   Braun et al., "Simple and Efficient Construction of Static Single Assignment Form", 2013

   When encountering a use, the algorithm looks at the local definitions and recurses
   into predecessor blocks, if none are found. If a node has multiple predecessors,
   Phi functions are placed.

   Some differences and implementation details:
   We don't perform _value numbering_ like in the original algorithm,
   as this would necessitate modeling instruction effects for each platform.
   So we are only looking at SSA names as "values".

   The algorithm tries to remove _trivial phi nodes_ along the way.
   This poses a problem in regards to replacing vregs in the actual instructions,
   because we do not keep explicit def-use-chains.
   To mitigate this, we keep a Phi dependency graph and a map of aliases.
   When a Phi node gets removed, an alias is recorded and Phi nodes depending on
   it will also be checked whether they are trivial.
   In the end, we have a second pass over the instructions, renaming all
   aliased names to the final name.

   The algorithm supports "incomplete CFGs", as it can be performed
   during IR construction.
   This doesn't really apply here, as we already have a complete CFG.
   Nonetheless, when traversing the CFG in reverse post-order,
   we visit the loop header before the final loop block with the back-edge.
   This means that we still need to deal with predecessors, that have not
   been visited yet and can not provide definitions.
   Therefore, the concept of "filled" and "sealed" blocks is used regardless.
   To quote the paper:

   "We call a basic block sealed if no further predecessors will be added to the block.
   As only filled blocks may have successors, predecessors are always filled. Note
   that a sealed block is not necessarily filled. Intuitively, a filled block contains
   all its instructions and can provide variable definitions for its successors. Con-
   versely, a sealed block may look up variable definitions in its predecessors as all
   predecessors are known."

   The difference is, that in this implementation, "filled" blocks don't refer
   to blocks where the instructions have been emitted (bc. that is ALL blocks),
   but blocks where instructions have been processed by the algorithm to provide
   definitions. But the intention is the same: A filled block can provide
   definitions for its successors.

   Destruction currently uses the most simple scheme, a union-find over
   Phi nodes. This won't work unless the code is in conventional SSA
   and a more complicated scheme inserting copies will be needed after
   applying certain optimizations.

-}


-- Using differen Unique tags to differentiate Phis and "normal" vregs.
{-# INLINE vAR_TAG #-}
vAR_TAG :: Char
vAR_TAG = 'V'


{-# INLINE pHI_TAG #-}
pHI_TAG :: Char
pHI_TAG = 'P'


type RegId = Unique

-- To distinguish the original CFG and reverse CFG
type ReverseCFG = CFG

-- | Representing Phi-function, i.e., parallel copies to new name.
data PhiFun = PhiF RegId RegId [RegId]
              -- ^ oldId newId args

phiDef :: PhiFun -> RegId
phiDef (PhiF _ def _) = def

instance Eq PhiFun where
    (==) (PhiF _ x _) (PhiF _ y _) = x == y


-- | Wrapping SSA-Blocks in Top Level CmmDecl
type SsaCmmDecl statics instr
        = GenCmmDecl
                statics
                (LabelMap RawCmmStatics)
                [SSABasicBlock instr]


-- | Wrapping a GenBasicBlock to add Phi-Functions
data SSABasicBlock instr
        = SSABB [PhiFun] !(GenBasicBlock instr)
        deriving (Functor)


ssaBBlockPhiFuns :: SSABasicBlock instr -> [PhiFun]
ssaBBlockPhiFuns (SSABB phis _) = phis


ssaBBMapPhiFuns :: ([PhiFun] -> [PhiFun]) -> SSABasicBlock instr -> SSABasicBlock instr
ssaBBMapPhiFuns f (SSABB phis ins) = SSABB (f phis) ins


ssaBBlockId :: SSABasicBlock instr -> BlockId
ssaBBlockId (SSABB _ blk) = blockId blk


-- We need random access of blocks, so we store them in a map.
type BlockLookupTable instr
        = UniqFM BlockId (SSABasicBlock instr)


-- SSA Construction

-- Top-level function to transform a list of procs.
-- Uniques are constructed ad-hoc for performance.
-- Since we are replacing all vregs, the names are guaranteed to be unique.
-- Uniques are threaded through all procs, so they are unique for the whole module
-- and names are not reused for different procs.
cmmSsaTransformAll
    :: (HasDebugCallStack, OutputableP Platform statics, Instruction instr)
    => Platform
    -> CFG
    -> Bool     -- ^ Validate (lint) final result
    -> [NatCmmDecl statics instr]
    -> [SsaCmmDecl statics instr]

cmmSsaTransformAll platform cfg validate natCmmDecls
 = snd $ foldr transform (startUniques, []) natCmmDecls
 where  startUniques = (mkUnique vAR_TAG 0, mkUnique pHI_TAG 0)
        transform proc (us, lst)
                = let (us', proc') = cmmSsaTransform platform cfg validate us proc
                  in  (us', proc' : lst)


cmmSsaTransform
    :: (HasDebugCallStack, OutputableP Platform statics, Instruction instr)
    => Platform
    -> CFG
    -> Bool                     -- ^ Validate (lint) final result
    -> (Unique, Unique)         -- ^ First uniques for Phis and normal vregs
    -> NatCmmDecl statics instr
    -> ((Unique, Unique), SsaCmmDecl statics instr)     -- ^ Tuple of next Phi and vreg uniques
                                                        --    and proc in SSA form.

cmmSsaTransform _ _ _ us (CmmData i d)
 = (us, CmmData i d)

cmmSsaTransform _ _ _ us (CmmProc _ lbl live (ListGraph []))
 = (us, CmmProc mapEmpty lbl live [])

cmmSsaTransform platform cfg validate uniqStart (CmmProc info lbl live (ListGraph blocks@(first : _)))
 = let
         -- We're filtering out unreachable blocks here, orphaned by previous optimizations
         reachableBlocks
                        = [(bid, SSABB [] b) | b@(BasicBlock bid _) <- blocks, hasNode cfg bid]
         reverseCfg     = reverseEdges cfg
         blkTbl         = listToUFM reachableBlocks
         renameSM       = initRenameS blkTbl reverseCfg uniqStart
         revPostOrder   = revPostorderFrom cfg (blockId first)
         -- The first pass performs most of the algorithm, the rest is just rewriting aliases
         firstPass      = -- traceSealed <$> sealedCnt >>
                          mapM_ (cmmSsaTransform_block platform cfg) revPostOrder
         s = execState (   firstPass
                        -- >> assertM (gets stateSealedBlks >>= (\x -> return $ sizeUniqSet x == sizeUFM blkTbl))
                        >> clearStateForRewrite
                        >> resolvePhiRenameAliases
                        >> rewriteAliases platform )
                        renameSM
         flattenedBlks  = mapMaybe (\(bid, _) -> lookupUFM (stateBlockLookupTable s) bid) reachableBlocks
         nextUniques    = (stateVarUnique s, statePhiUnique s)

         -- cfgNodesCnt    = length $ getCfgNodes cfg
         -- sealedCnt      = sizeUniqSet <$> gets stateSealedBlks
         -- traceSealed s  = trace $ "#CFG " ++ show cfgNodesCnt ++ " #Sealed " ++ show s

         finalProc      = if validate
                                then validateSSA cfg $ CmmProc info lbl live flattenedBlks
                                else CmmProc info lbl live flattenedBlks

   in    (nextUniques, finalProc)


cmmSsaTransform_block
        :: (HasDebugCallStack, Instruction instr)
        => Platform
        -> CFG
        -> BlockId
        -> RenameM instr ()
cmmSsaTransform_block platform cfg bid
 = do
         (SSABB _ (BasicBlock _ ins))   <- getBlockOrFail bid
         let ins_productive             = filter isProductiveInstr ins
         ins'   <- mapM (cmmSsaTransform_instr platform bid) ins_productive
         updateBlockInstructions bid ins'
         markBlockFilled bid
         mapM_ sealIfPossible $ bid : (getSuccessors cfg bid)
 where
         sealIfPossible b
          = do
                  revCfg        <- gets stateReverseCfg
                  whenM ((not <$> isSealedBlock b) `andM`
                                        (allM isFilledBlock $ getPredecessors revCfg b))
                        (sealBlock b)


cmmSsaTransform_instr
    :: (HasDebugCallStack, Instruction instr)
    => Platform
    -> BlockId
    -> instr
    -> RenameM instr instr
cmmSsaTransform_instr platform bid instr
 = do
         let RU rlRead rlWritten = regUsageOfInstr platform instr
         let rsRead              = mkUniqSet $ filter isVirtualReg rlRead
         let rsWritten           = mkUniqSet $ filter isVirtualReg rlWritten

         -- Exclude conditional instructions and modified operands
         -- (read and written), because we can't introduce new name for them.
         let defs = if isConditionalInstr instr
                        then emptyUniqSet
                        else rsWritten `minusUniqSet` rsRead

         -- All reads, including modified operands, are uses and can be renamed.
         let uses = rsRead

         -- Not introducing non-determinism, order doesn't matter:
         -- See Note [Unique Determinism and code generation]

         -- Generate and store new names for defs, make pairs of (old name, new name).
         defsRenames    <- mapM (getRenamePair $ \r -> newVarUnique >>= writeVar bid r)
                        $ nonDetEltsUniqSet defs
         let instr_defs = foldl' (uncurry . patchInstr) instr defsRenames

         -- A "use" needs name lookup, may trigger recursive search and insertion of Phis
         usesRenames    <- mapM (getRenamePair $ readVar bid) $ nonDetEltsUniqSet uses
         let instr_uses = foldl' (uncurry . patchInstr) instr_defs usesRenames

         return instr_uses
 where
         getRenamePair f r = (r,) <$> (f $ getUnique r)


-- Rewrite any vreg in instruction that has alias name.
rewriteAliases :: Instruction instr => Platform -> RenameM instr ()
rewriteAliases platform
 = do
         blocks <- nonDetUFMToList <$> gets stateBlockLookupTable
         blocks' <- mapM (\(k, blk) -> (k,) <$> rewriteAliases_block platform blk) blocks
         modify $ \s -> s {
                 stateBlockLookupTable = listToUFM_Directly blocks' }


rewriteAliases_instr :: Instruction instr => Platform -> instr -> RenameM instr instr
rewriteAliases_instr platform instr
 = do
         aliases                 <- gets statePhiRenames
         let RU rlRead rlWritten = regUsageOfInstr platform instr
         let vregs               = filterUniqSet isVirtualReg
                                   $ addListToUniqSet (mkUniqSet rlRead) rlWritten
         let renames             = mapMaybe (\r -> (r,) <$> (lookupAlias aliases $ getUnique r))
                                   $ nonDetEltsUniqSet vregs
         let instr'              = foldl' (uncurry . patchInstr) instr renames

         return instr'
 where
         lookupAlias aliases = lookupUFM_Directly aliases


rewriteAliases_block
        :: Instruction instr
        => Platform
        -> SSABasicBlock instr
        -> RenameM instr (SSABasicBlock instr)
rewriteAliases_block platform (SSABB phis (BasicBlock bid ins))
 = do
         ins' <- mapM (rewriteAliases_instr platform) ins
         return (SSABB phis (BasicBlock bid ins'))


-- SSA Destruction

-- | Precondition: MUST be in conventional SSA, otherwise transformation is *wrong*.
--   Since CSSA means, that vars connected by Phi-functions don't interfere,
--   they can simply be renamed to one variable.
--   This is **not correct** if transformations have been applied and SSA is
--   no longer conventional. In that case, copies may be needed.
cssaToNatCmmDecl
        :: Instruction instr
        => Platform
        -> SsaCmmDecl statics instr
        -> NatCmmDecl statics instr

cssaToNatCmmDecl _ (CmmData i d)
 = CmmData i d

cssaToNatCmmDecl platform (CmmProc info lbl live blks)
 = let  phiWebs            = discoverPhiWebs blks
        getBB (SSABB _ bb) = bb
        blks'  = map (getBB . renamePhiWebs platform phiWebs) blks
   in   CmmProc info lbl live (ListGraph blks')


-- | Given conventional SSA form, perform union-find on Phi-functions to
--   build webs.
discoverPhiWebs
        :: Instruction instr
        => [SSABasicBlock instr]
        -> UniqSDFM RegId RegId         -- ^ Disjoint-set: Names in web to new name

discoverPhiWebs blks
 = let unionNames dset blk = foldl' unionPhi dset $ ssaBBlockPhiFuns blk
       unionPhi dset (PhiF _ new args)
        = foldl' (\ds k -> snd $ equateUSDFM ds k new) (addToUSDFM dset new new) args
   in  foldl' unionNames emptyUSDFM blks


renamePhiWebs
        :: Instruction instr
        => Platform
        -> UniqSDFM RegId RegId         -- ^ Disjoint-set of vreg uniques
        -> SSABasicBlock instr          -- ^ Block to apply renames to
        -> SSABasicBlock instr

renamePhiWebs platform dset (SSABB phis (BasicBlock bid ins))
 = let ins' = map (renamePhiWebs_instr platform dset) ins
   in  SSABB phis (BasicBlock bid ins')


-- | Rename all occurences of vregs in the instruction with
--   the new name of their respective Phi-Web.
renamePhiWebs_instr
        :: Instruction instr
        => Platform
        -> UniqSDFM RegId RegId -- ^ Disjoint-set of vreg uniques
        -> instr
        -> instr

renamePhiWebs_instr platform dset instr
 = let  RU rlRead rlWritten
                   = regUsageOfInstr platform instr
        rsUsed     = nonDetEltsUniqSet $ filterUniqSet isVirtualReg
                        $ addListToUniqSet (mkUniqSet rlRead) rlWritten
        rsRenames  = mapMaybe (\r -> (r,) <$> (lookupUSDFM dset $ getUnique r)) rsUsed
        instr'     = foldl' (uncurry . patchInstr) instr rsRenames

   in   instr'

-- Filter unproductive move instructions
-- Sometimes optimizations leave a move with the same source and destination
-- behind, e.g., `mov r0, r0`.
-- Those are redundant and we don't want them to introduce a new SSA name,
-- so we filter them out.
-- TODO: is this safe? `mov eax,eax` does clear top 32 bits,
-- but I don't think NCG generates code like that, bc. it generally only uses
-- full 64 Bit registers.
isProductiveInstr :: Instruction instr => instr -> Bool
isProductiveInstr i = case takeRegRegMoveInstr i of
                        Just (r1, r2)   -> r1 /= r2
                        Nothing         -> True


addPhiOperands :: HasDebugCallStack => [BlockId] -> PhiFun -> RenameM i (Either RegId PhiFun)
addPhiOperands predecessors (PhiF old def args)
 = do   operands        <- mapM (\b -> readVar b old) predecessors
        let args'       = args ++ operands

        -- Add dependency 'op -> def', i.e., operand used by this phi.
        -- If 'op' turns out to be trivial, we'll have to update all phis
        -- depending on it.
        let phiOps      = filter isPhi operands
        mapM_ (addPhiDependency def) phiOps -- $ pprTrace "Add deps for " (ppr def <> colon <> ppr phiOps) phiOps

        let phi'        = -- pprTrace "addPhiOperands: "
                          --      (ppr def <> text " old args: " <> ppr args <> text " new args: " <> ppr operands)
                                (PhiF old def args')
        tryRemoveTrivialPhi phi'


-- Search for local definition
readVar :: HasDebugCallStack => BlockId -> RegId -> RenameM i RegId
readVar b r
 = do   curDefs <- gets stateCurrentDef
        let curDef = lookupUFM_Directly curDefs r >>= (\fm -> lookupUFM fm b)
        case curDef of
            Just def -> return def              -- Local definition
            Nothing  -> readVarRecursive b r    -- Global value numbering


-- Search for definition in predecessors, place Phis if necessary
readVarRecursive :: HasDebugCallStack => BlockId -> RegId -> RenameM i RegId
readVarRecursive b r
 = do   isSealed        <- isSealedBlock b
        rcfg            <- gets stateReverseCfg
        nu <- if not isSealed
                then incompletePhi b r -- Incomplete CFG
                        -- do
                        -- incompletePhi   <- incompletePhi b r -- Incomplete CFG
                        -- return $ pprTrace "readVarRecursive: not sealed"
                        --        (pprInfo <> text " incomplete Phi: " <> ppr incompletePhi) incompletePhi
                else case getPredecessors rcfg b of
                        []      -> panic $ "GHC.CmmToAsm.SSA.readVarRecursive: Potential uninitialized value for '"
                                        ++ show r ++ "' in block " ++ show b
                        [p]     -> -- pprTrace "readVarRecursive: single pred" pprInfo $
                                        readVar p r
                        preds   -> do -- Create Phi-node
                                        phi@(PhiF _ new _)      <- createPhi b r
                                        _                       <- writeVar b r new
                                        valOrPhi                <- addPhiOperands preds phi
                                        -- let valOrPhi' = pprTrace "readVarRecursive: add Phi: " (ppr valOrPhi <> char '-' <> pprInfo) valOrPhi
                                        case valOrPhi of
                                                Left v          -> return v
                                                Right phiNubbed -> updatePhiInBlock b phiNubbed

        writeVar b r nu
--  where
--          pprInfo = text "block: " <> ppr b <> text " var: " <> ppr r


-- Seal block if all its predecessors are filled. Complete all incomplete Phis of block.
sealBlock :: HasDebugCallStack => BlockId -> RenameM i ()
sealBlock b
 = do   rcfg            <- gets stateReverseCfg
        let preds       = getPredecessors rcfg b
        incompletePhis  <- gets $ \s -> fromMaybe [] $ lookupUFM (stateIncompletePhis s) b
        mapM_ (completePhi preds) incompletePhis -- $ pprTrace "Sealing " (ppr b <> text ", incomplete Phis: " <> ppr incompletePhis) incompletePhis
        modify $ \s -> s {
                stateIncompletePhis = delFromUFM (stateIncompletePhis s) b,
                stateSealedBlks     = addOneToUniqSet (stateSealedBlks s) b }
 where
        completePhi preds phi@(PhiF old new _)
                = addPhiOperands preds phi
                >>= either
                        (\v -> removePhiFromBlock b new >> writeVar b old v)
                        (updatePhiInBlock b)


-- Check if a Phi node is "trivial", i.e., does not contain at least two distinct
-- arguments, exkluding its own definition.
-- E.g., p1 = Phi (p1, v2) is trivial and can be replaced with v2
--
-- This is probably one of the most complex functions for this algorithm.
tryRemoveTrivialPhi :: HasDebugCallStack => PhiFun -> RenameM i (Either RegId PhiFun)
tryRemoveTrivialPhi phi@(PhiF old def args)
 = -- pprTraceIt "tryRemoveTrivialPhi res: " <$>
   case nonSelfArgs of
         -- Undefined! Error or incomplete Phi?
         []     -> do
                        incomplete <- isIncomplete
                        if incomplete
                                then return $ Right phi -- $ pprTraceIt "incomplete Phi: " phi
                                else do
                                        origin         <- fromJust <$> getPhiDefBlock def
                                        incompletePhis  <- gets stateIncompletePhis
                                        pprPanic "GHC.CmmToAsm.SSA.tryRemoveTrivialPhi: Phi node "
                                                (ppr def <> text " is undefined in block " <> ppr origin
                                                <> text " with args: " <> ppr args <> blankLine
                                                <> text " incomplete state: " <> ppr incompletePhis)
                                                phi

         -- Is 'trivial', i.e., at most one unique value on RHS of Phi.
         [r]    -> do
                        -- Remove trivial Phi from block.
                        mOrigin          <- getPhiDefBlock def
                        whenJust mOrigin (\blk -> removePhiFromBlock blk def >> writeVar blk old r >> return ())

                        -- Add rename from Phi to its only value. We need this to rewrite the code _afterwards_,
                        -- to replace all references to the removed Phi node.
                        putPhiRename def r

                        modify $ \s -> s {
                                stateCurrentDef = adjustUFM (mapUFM (\x -> if x == def then r else x)) (stateCurrentDef s) old
                        }

                        -- Replaces this Phi operand in users with `r`.
                        -- Also updates Phi dependency graph.
                        modified        <- removePhiUses def r

                        -- Check if any user has become trivial by replacing this Phi with `r`,
                        -- e.g., p2 = PHI (p1, r) -> p2 = PHI (r, r):
                        --      p2 has become trivial and can be replaced by `r`.
                        mapM_ tryRemoveTrivialPhi modified -- $ pprTraceIt "tryRemoveTrivialPhi modified: " modified

                        -- Trivial Phi replaced by its only argument value
                        return $ Left r

        -- Non trivial Phi, keep it
         _      -> return $ Right (PhiF old def nonSelfArgs)
 where
         uniqueArgs     = nub args      -- TODO: Use UniqSet instead? But length will usually be short. Maybe array?
         nonSelfArgs    = -- pprTrace "tryRemoveTrivialPhi input: " (ppr phi) $
                          filter (/= def) uniqueArgs
         whenJust mg f  = maybe (pure ()) f mg

         isIncomplete   = do
                            mOrigin         <- getPhiDefBlock def
                            incompletePhis  <- gets stateIncompletePhis
                            let foo = (mOrigin >>= (lookupUFM incompletePhis))
                            return $ maybe False (phi `elem`) foo


-- Replace a trivial Phi node with its replacement value in the operand list of all its
-- users (other Phi nodes).
-- This replaces the operand in the actual Phi functions in basic blocks and also
-- updates the Phi dependency graph (to track users of a Phi).
-- If the new 'value' is just a value, we can remove the dependency, but if it is another
-- Phi function, we need to add it as a dependency.
removePhiUses :: RegId -> RegId -> RenameM i [PhiFun]
removePhiUses deadArg newVal
 = do
        -- Find all users of trivial Phi to replace the operand.
        mUsers          <- gets (\s -> lookupUFM_Directly (statePhiDeps s) deadArg)
        let users       = maybe [] (nonDetEltsUniqSet) mUsers

        mDefBlks        <- mapM (\p -> (fmap (p,)) <$> getPhiDefBlock p)
                           -- $ pprTraceIt "tryRemoveTrivialPhi users: "
                           users
        -- Grouping by blocks so we have to update each block only once
        let defBlks     = groupBy ((==) `on` snd)
                        $ sortBy (compare `on` snd)
                        $ catMaybes mDefBlks
        modPhis         <- mapM (replaceArgInBlock deadArg) defBlks

        -- mapM_ adjustDependency users

        modify $ \s -> s {
                statePhiDeps = adjustDependencies (statePhiDeps s) users
        }

        return $ concat modPhis
 where
        replaceArgInBlock _ []      = return []
        replaceArgInBlock del usrs@((_, bid):_)
         = do
                let usrsPhi = map fst usrs
                modify $ \s -> s {
                        stateBlockLookupTable
                                = adjustUFM
                                        (ssaBBMapPhiFuns (map (replacePhiArg usrsPhi del newVal)))
                                        (stateBlockLookupTable s)
                                        bid
                }

                mModifiedPhis   <- gets $ \s -> ssaBBlockPhiFuns
                                        <$> lookupUFM (stateBlockLookupTable s) bid
                let modifiedPhis
                                = filter (\p -> (phiDef p) `elem` usrsPhi)
                                $ fromMaybe [] mModifiedPhis

                return modifiedPhis

        -- Delete dependency to dead arg, add a new dep. if it's a Phi not simply vreg.
        adjustDependencies deps users
                = let deps' = delFromUFM_Directly deps deadArg -- $ traceDeadArgDeps deps deadArg
                  in  if isPhi newVal
                        then adjustUFM (`addListToUniqSet` users) deps' newVal -- $ traceNewValDeps deps' newVal
                        else deps'

        -- traceDeadArgDeps deps x = pprTrace "  adjustDependencies: deadArg deps:"
        --                         (ppr deadArg <> text ": " <>
        --                          ppr (lookupWithDefaultUFM_Directly deps emptyUniqSet deadArg)) x

        -- traceNewValDeps deps x  = pprTrace "  adjustDependencies: newVal deps:"
        --                         (ppr newVal <> text ": " <>
        --                          ppr (lookupWithDefaultUFM_Directly deps emptyUniqSet newVal)) x



replacePhiArg :: [RegId] -> RegId -> RegId -> PhiFun -> PhiFun
replacePhiArg users del repl phi@(PhiF old new args) =
         if new `elem` users
                 then PhiF old new (map (\x -> if x == del then repl else x) args)
                 else phi


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


-- CFG Utilitiy functions -------------------------------------------

-- We want the predecessors in the original CFG, which maps to the
-- successors in the reverse CFG
getPredecessors :: HasDebugCallStack => ReverseCFG -> BlockId -> [BlockId]
getPredecessors = getSuccessors


-- Phi Utility functions --------------------------------------------

isPhi :: RegId -> Bool
isPhi r = tag == pHI_TAG
 where tag = fst $ unpkUnique r

-- Rename Monad -----------------------------------------------------

type RenameM instr a
        = State (RenameS instr) a

-- | SSA Renamer code generator state.
data RenameS instr
        = RenameS
        { -- | Current unique for new vreg names.
          stateVarUnique        :: Unique

          -- | Unique with different domain for Phis,
          --   so we can track Phi dependencies for tryRemoveTrivialPhi.
        , statePhiUnique        :: Unique

          -- | Current new name for original vreg.
        , stateCurrentDef       :: UniqFM RegId (UniqFM BlockId RegId)

          -- | Block is sealed if it can lookup definitions in its predecessors
          -- as they have already been processed.
        , stateSealedBlks       :: UniqSet BlockId

          -- | Block whose instructions already have been processed,
          -- so it can provide definitions for its successors.
        , stateFilledBlks       :: UniqSet BlockId

          -- | Phis that were placed as markers and have no arguments yet.
        , stateIncompletePhis   :: UniqFM BlockId [PhiFun]

          -- | Phis that were removed and uses need to be rewritten in
          -- second pass.
        , statePhiRenames       :: UniqFM RegId RegId

          -- | Phi dependency graph for recursive removal of trivial Phis.
        , statePhiDeps          :: UniqFM RegId (UniqSet RegId)

          -- | Map Phi name to block in which it is defined.
        , statePhiDefBlocks     :: UniqFM RegId BlockId

          -- | Basic blocks of proc to query and update.
        , stateBlockLookupTable :: BlockLookupTable instr

          -- | Reverse CFG of procedure.
        , stateReverseCfg       :: ReverseCFG

          -- | Stats: Count of unique names starting out.
        , stateOriginalNames    :: !Int }


-- | Create a new renamer state.
initRenameS
        :: Instruction instr
        => BlockLookupTable instr
        -> ReverseCFG
        -> (Unique, Unique)
        -> RenameS instr
initRenameS blkTbl rcfg (varStart, phiStart)
        = RenameS
        { -- Make sure Var and Phi Uniques have correct domain
          stateVarUnique        = newTagUnique varStart vAR_TAG
        , statePhiUnique        = newTagUnique phiStart pHI_TAG
        , stateCurrentDef       = emptyUFM
        , stateSealedBlks       = emptyUniqSet
        , stateFilledBlks       = emptyUniqSet
        , stateIncompletePhis   = emptyUFM
        , statePhiRenames       = emptyUFM
        , statePhiDeps          = emptyUFM
        , statePhiDefBlocks     = emptyUFM
        , stateBlockLookupTable = blkTbl
        , stateReverseCfg       = rcfg
        , stateOriginalNames    = 0 }


-- Clear state before last rewrite step, except for statePhiRenames, which we need.
clearStateForRewrite :: RenameM i ()
clearStateForRewrite = modify $ \s -> s {
        stateCurrentDef       = emptyUFM
        , stateSealedBlks       = emptyUniqSet
        , stateFilledBlks       = emptyUniqSet
        , stateIncompletePhis   = emptyUFM
        , statePhiDeps          = emptyUFM
        , statePhiDefBlocks     = emptyUFM }

-- | Allocate a new var unique in the renamer monad.
newVarUnique :: RenameM i Unique
newVarUnique
 = do   u      <- gets stateVarUnique
        modify $ \s -> s { stateVarUnique = incrUnique u }
        return u


-- | Allocate a new var unique in the renamer monad.
newPhiUnique :: RenameM i Unique
newPhiUnique
 = do   u      <- gets statePhiUnique
        modify $ \s -> s { statePhiUnique = incrUnique u }
        return u


isSealedBlock :: BlockId -> RenameM i Bool
isSealedBlock bid = elementOfUniqSet bid <$> gets stateSealedBlks


isFilledBlock :: BlockId -> RenameM i Bool
isFilledBlock bid = elementOfUniqSet bid <$> gets stateFilledBlks


markBlockFilled :: BlockId -> RenameM i ()
markBlockFilled bid = modify $ \s -> s {
        stateFilledBlks = addOneToUniqSet(stateFilledBlks s) bid }


putPhiRename :: RegId -> RegId -> RenameM i ()
putPhiRename old new = modify $ \s -> s {
        statePhiRenames = addToUFM_Directly (statePhiRenames s) old new }


-- We may have alias chains when multiple levels of Phi functions become trivial, e.g.,
-- p1 -> p2, p2 -> p3, p3 -> v1.
-- We want to resolve them to: p1 -> v1, p2 -> v1, p3 -> v1
resolvePhiRenameAliases :: RenameM i ()
resolvePhiRenameAliases
 = do
        renames         <- gets statePhiRenames
        let keys        = nonDetKeysUFM renames
        let renames'    = foldl' resolveAlias renames keys
        modify $ \s -> s { statePhiRenames = renames' }
        return ()
 where
        resolveAlias fm k       = let mCurVal   = lookupUFM_Directly fm k
                                      mLastVal  = findLastAlias fm <$> mCurVal
                                  in  maybe fm (addToUFM_Directly fm k) mLastVal

        findLastAlias fm x      = case lookupUFM_Directly fm x of
                                        Just y  -> findLastAlias fm y
                                        Nothing -> x


getPhiDefBlock :: RegId -> RenameM i (Maybe BlockId)
getPhiDefBlock p = gets (\s -> lookupUFM_Directly (statePhiDefBlocks s) p)


-- | Get a block from lookup table. Must exist or else panic!
getBlockOrFail :: BlockId -> RenameM i (SSABasicBlock i)
getBlockOrFail bid
 = do   mBlk <- (\fm -> lookupUFM fm bid) <$> gets stateBlockLookupTable
        case mBlk of
                        Nothing  -> panic $ "GHC.CmmToAsm.SSA.getBlockOrFail: Block '" ++ show bid ++ "' doesn't exist!"
                        Just blk -> return blk


updateBlockInstructions
        :: Instruction instr
        => BlockId
        -> [instr]
        -> RenameM instr ()
updateBlockInstructions bid ins
 = modify $ \s -> s { stateBlockLookupTable = adjustUFM f (stateBlockLookupTable s) bid }
   where f (SSABB phis (BasicBlock bid _)) = SSABB phis (BasicBlock bid ins)


addEmptyPhiToBlock :: BlockId -> RegId -> RegId -> RenameM instr PhiFun
addEmptyPhiToBlock bid r u
 = do   modify $ \s -> s {
         stateBlockLookupTable  = adjustUFM addPhi (stateBlockLookupTable s) bid,
         statePhiDefBlocks      = addToUFM_Directly (statePhiDefBlocks s) u bid
        }
        return newPhi
   where   newPhi = (PhiF r u [])
           addPhi (SSABB phis bb) = SSABB (newPhi:phis) bb


updatePhiInBlock :: BlockId -> PhiFun -> RenameM instr RegId
updatePhiInBlock bid phi@(PhiF _ new _)
 = do   modify $ \s -> s {
         stateBlockLookupTable  = adjustUFM replacePhi (stateBlockLookupTable s) bid
        }
        return new
 where  replacePhi (SSABB phis bb) = SSABB (map (\p@(PhiF _ n _) -> if n == new then phi else p) phis) bb


removePhiFromBlock :: BlockId -> RegId -> RenameM instr ()
removePhiFromBlock bid r
 = do   modify $ \s -> s {
         stateBlockLookupTable  = adjustUFM delPhi (stateBlockLookupTable s) bid,
         statePhiDefBlocks      = delFromUFM_Directly (statePhiDefBlocks s) r
        }
   where   delPhi (SSABB phis bb) = SSABB (filter (\p -> phiDef p /= r) phis) bb


addMapping :: Uniquable key => key -> elt -> Maybe (UniqFM key elt) -> Maybe (UniqFM key elt)
addMapping k v Nothing   = Just $ unitUFM k v
addMapping k v (Just fm) = Just $ addToUFM fm k v


writeVar :: BlockId -> RegId -> RegId -> RenameM i RegId
writeVar b old new
 = do   modify $ \s -> s { stateCurrentDef
            = alterUFM_Directly (addMapping b new) (stateCurrentDef s) old }
        return new


incompletePhi
        :: BlockId
        -> RegId
        -> RenameM i RegId
incompletePhi b r
 = do   phi     <- createPhi b r
        modify $ \s -> s { stateIncompletePhis
                = alterUFM  (addPhi phi) (stateIncompletePhis s) b}

        return $ phiDef phi
 where
        addPhi phi Nothing      = Just [phi]
        addPhi phi (Just lst)   = Just $ phi : lst


createPhi :: BlockId -> RegId -> RenameM i PhiFun
createPhi b r
 = do   u       <- newPhiUnique
        addEmptyPhiToBlock b r u


addPhiDependency :: RegId -> RegId -> RenameM i ()
addPhiDependency phi dependent
 = do
        when (dependent /= phi)
            (modify $ \s -> s {
                    statePhiDeps = alterUFM_Directly addDep (statePhiDeps s) $ assertPhis dependent
            })
 where
         addDep (Just set)  = Just $ addOneToUniqSet set phi
         addDep Nothing     = Just $ unitUniqSet phi
         -- Assert input uniques are actually Phis
         assertPhis p       = assert (isPhi phi && isPhi dependent) p
         -- traceDeps p        = pprTrace "Add Phi dependency: " (ppr phi <> text " on " <> ppr dependent) p


-- Pretty printing machinery

instance Outputable PhiFun where
        ppr (PhiF old new args)
                = hsep [(ppr new), equals, phi
                       , parens $ hcat (punctuate comma $ map ppr args)
                       , space, brackets (text "old: " <> ppr old)]
                where phi = unicodeSyntax (char 'φ') (text "Phi")


instance (Instruction instr) => OutputableP Platform (SSABasicBlock instr) where
        pdoc platform (SSABB phis bb)
            = vcat (map ppr phis)
              $$ ppr (fmap (pprInstr platform) bb)


pprSsaCmmDecl
    :: (OutputableP Platform statics, Instruction instr)
    => Platform
    -> SsaCmmDecl statics instr
    -> SDoc

pprSsaCmmDecl platform ssaThing
    = let pprSsaBBs = fmap (fmap $ pdoc platform)
      in  pdoc platform (pprSsaBBs ssaThing)


-- Validate SSA

validateSSA
    :: (OutputableP Platform statics, Instruction instr)
    => CFG
    -> SsaCmmDecl statics instr
    -> SsaCmmDecl statics instr

validateSSA _ dt@CmmData{} = dt

validateSSA _ proc@(CmmProc _ _ _ []) = proc

validateSSA cfg proc@(CmmProc _ _ _ blks@(first:_))
 = let  blkTbl         = zipToUFM (map ssaBBlockId blks) blks
        revPostOrder   = revPostorderFrom cfg (ssaBBlockId first)
        blocksInRPO    = map (fromJust . lookupUFM blkTbl) revPostOrder
        validateBlocks = foldl' validateSSA_block emptyUniqSet
   in   validateBlocks blocksInRPO `seq` proc


-- Check that no trivial Phis remain and that all Phi arguments
-- have been defined previously.
validateSSA_block
    :: Instruction instr
    => UniqSet RegId
    -> SSABasicBlock instr
    -> UniqSet RegId
validateSSA_block seen (SSABB phis (BasicBlock bid _))
 = assert
        (all (\p -> assertNonTrivial p && assertInitReads p) phis)
        $ foldl' (\set phi-> addOneToUniqSet set $ phiDef phi) seen phis
 where
        assertNonTrivial phi@(PhiF _ new args)
         = (notNull $ filter (/= new) $ nub args)
                || pprPanic "GHC.CmmToAsm.SSA.validateSSA_block: Trivial Phi function found: "
                   (text "In Block " <> ppr bid <> colon <> ppr phi)

        assertInitReads (PhiF _ new args)
         = all (assertArg new) args

        assertArg new arg
         = elemUniqSet_Directly arg seen
                || pprPanic "GHC.CmmToAsm.SSA.validateSSA_block: Uninitialized Phi argument: "
                   (text "In Block " <> ppr bid <> hsep [colon, ppr new, equals,
                    text "(..," <> ppr arg <> text ",..)"])
