{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Annotate a CmmGraph with ThreadSanitizer instrumentation calls.
module GHC.Cmm.ThreadSanitizer (annotateTSAN) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Regs (activeStgRegs, callerSaves)
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Data.FastString
import GHC.Types.Basic
import GHC.Types.ForeignCall
import GHC.Types.Unique
import GHC.Types.Unique.Supply

import Data.Maybe (fromMaybe)

data Env = Env { platform :: Platform
               , uniques :: UniqSupply
               }

annotateTSAN :: Platform -> CmmGraph -> UniqSM CmmGraph
annotateTSAN platform graph = do
    env <- Env platform <$> getUniqueSupplyM
    return $ modifyGraph (mapGraphBlocks (annotateBlock env)) graph

mapBlockList :: (forall e' x'. n e' x' -> Block n e' x')
             -> Block n e x -> Block n e x
mapBlockList f (BlockCO n rest  ) = f n `blockAppend` mapBlockList f rest
mapBlockList f (BlockCC n rest m) = f n `blockAppend` mapBlockList f rest `blockAppend` f m
mapBlockList f (BlockOC   rest m) = mapBlockList f rest `blockAppend` f m
mapBlockList _ BNil               = BNil
mapBlockList f (BMiddle blk)      = f blk
mapBlockList f (BCat a b)         = mapBlockList f a `blockAppend` mapBlockList f b
mapBlockList f (BSnoc a n)        = mapBlockList f a `blockAppend` f n
mapBlockList f (BCons n a)        = f n `blockAppend` mapBlockList f a

annotateBlock :: Env -> Block CmmNode e x -> Block CmmNode e x
annotateBlock env = mapBlockList (annotateNode env)

annotateNode :: Env -> CmmNode e x -> Block CmmNode e x
annotateNode env node =
    case node of
      CmmEntry{}              -> BlockCO node BNil
      CmmComment{}            -> BMiddle node
      CmmTick{}               -> BMiddle node
      CmmUnwind{}             -> BMiddle node
      CmmAssign{}             -> annotateNodeOO env node
      -- TODO: Track unaligned stores
      CmmStore _ _ Unaligned  -> annotateNodeOO env node
      CmmStore lhs rhs NaturallyAligned  ->
          let ty = cmmExprType (platform env) rhs
              rhs_nodes = annotateLoads env (collectExprLoads rhs)
              lhs_nodes = annotateLoads env (collectExprLoads lhs)
              st        = tsanStore env ty lhs
          in rhs_nodes `blockAppend` lhs_nodes `blockAppend` st `blockSnoc` node
      CmmUnsafeForeignCall (PrimTarget op) formals args ->
          let node' = fromMaybe (BMiddle node) (annotatePrim env op formals args)
              arg_nodes = blockConcat $ map (annotateExpr env) args
          in arg_nodes `blockAppend` node'
      CmmUnsafeForeignCall{}  -> annotateNodeOO env node
      CmmBranch{}             -> annotateNodeOC env node
      CmmCondBranch{}         -> annotateNodeOC env node
      CmmSwitch{}             -> annotateNodeOC env node
      CmmCall{}               -> annotateNodeOC env node
      CmmForeignCall{}        -> annotateNodeOC env node

annotateNodeOO :: Env -> CmmNode O O -> Block CmmNode O O
annotateNodeOO env node =
    annotateLoads env (collectLoadsNode node) `blockSnoc` node

annotateNodeOC :: Env -> CmmNode O C -> Block CmmNode O C
annotateNodeOC env node =
    annotateLoads env (collectLoadsNode node) `blockJoinTail` node

annotateExpr :: Env -> CmmExpr -> Block CmmNode O O
annotateExpr env expr =
    annotateLoads env (collectExprLoads expr)

-- | A load mentioned in a 'CmmExpr'.
data Load = Load CmmType AlignmentSpec CmmExpr

annotateLoads :: Env -> [Load] -> Block CmmNode O O
annotateLoads env loads =
    blockConcat
    [ tsanLoad env align ty addr
    | Load ty align addr <- loads
    ]

collectLoadsNode :: CmmNode e x -> [Load]
collectLoadsNode node =
    foldExp (\exp rest -> collectExprLoads exp ++ rest) node []

-- | Collect all of the memory locations loaded from by a 'CmmExpr'.
collectExprLoads :: CmmExpr -> [Load]
collectExprLoads (CmmLit _)           = []
collectExprLoads (CmmLoad e ty align) = [Load ty align e]
collectExprLoads (CmmReg _)           = []
-- N.B. we don't bother telling TSAN about MO_RelaxedReads
-- since doing so would be inconvenient and they by
-- definition can neither race nor introduce ordering.
collectExprLoads (CmmMachOp _op args) = foldMap collectExprLoads args
collectExprLoads (CmmStackSlot _ _)   = []
collectExprLoads (CmmRegOff _ _)      = []

-- | Generate TSAN instrumentation for a 'CallishMachOp' occurrence.
annotatePrim :: Env
             -> CallishMachOp   -- ^ the applied operation
             -> [CmmFormal]     -- ^ results
             -> [CmmActual]     -- ^ arguments
             -> Maybe (Block CmmNode O O)
                                -- ^ 'Just' a block of instrumentation, if applicable
annotatePrim env (MO_AtomicRMW w aop)    [dest]   [addr, val]  = Just $ tsanAtomicRMW env MemOrderSeqCst aop w addr val dest
annotatePrim env (MO_AtomicRead w mord)  [dest]   [addr]       = Just $ tsanAtomicLoad env mord w addr dest
annotatePrim env (MO_AtomicWrite w mord) []       [addr, val]  = Just $ tsanAtomicStore env mord w val addr
annotatePrim env (MO_Xchg w)             [dest]   [addr, val]  = Just $ tsanAtomicExchange env MemOrderSeqCst w val addr dest
annotatePrim env (MO_Cmpxchg w)          [dest]   [addr, expected, new]
                                                               = Just $ tsanAtomicCas env MemOrderSeqCst MemOrderSeqCst w addr expected new dest
annotatePrim _    _                       _        _           = Nothing

mkUnsafeCall :: Env
             -> ForeignTarget  -- ^ function
             -> [CmmFormal]    -- ^ results
             -> [CmmActual]    -- ^ arguments
             -> Block CmmNode O O
mkUnsafeCall env ftgt formals args =
    save `blockAppend`     -- save global registers
    bind_args `blockSnoc`  -- bind arguments to local registers
    call `blockAppend`     -- perform call
    restore                -- restore global registers
  where
    (save, restore) = saveRestoreCallerRegs gregs_us (platform env)

    (arg_us, gregs_us) = splitUniqSupply (uniques env)

    -- We also must be careful not to mention caller-saved registers in
    -- arguments as Cmm-Lint checks this. To accomplish this we instead bind
    -- the arguments to local registers.
    arg_regs :: [CmmReg]
    arg_regs = zipWith arg_reg (uniqsFromSupply arg_us) args
      where
        arg_reg :: Unique -> CmmExpr -> CmmReg
        arg_reg u expr = CmmLocal $ LocalReg u (cmmExprType (platform env) expr)

    bind_args :: Block CmmNode O O
    bind_args = blockConcat $ zipWith (\r e -> BMiddle $ CmmAssign r e) arg_regs args

    call = CmmUnsafeForeignCall ftgt formals (map CmmReg arg_regs)

-- | We save the contents of global registers in locals and allow the
-- register allocator to spill them to the stack around the call.
-- We cannot use the register table for this since we would interface
-- with {SAVE,RESTORE}_THREAD_STATE.
saveRestoreCallerRegs :: UniqSupply -> Platform
                      -> (Block CmmNode O O, Block CmmNode O O)
saveRestoreCallerRegs us platform =
    (save, restore)
  where
    regs_to_save :: [GlobalReg]
    regs_to_save = filter (callerSaves platform) (activeStgRegs platform)

    nodes :: [(CmmNode O O, CmmNode O O)]
    nodes =
        zipWith mk_reg regs_to_save (uniqsFromSupply us)
      where
        mk_reg :: GlobalReg -> Unique -> (CmmNode O O, CmmNode O O)
        mk_reg reg u =
            let ty = globalRegSpillType platform reg
                greg = CmmGlobal (GlobalRegUse reg ty)
                lreg = CmmLocal (LocalReg u ty)
                save = CmmAssign lreg (CmmReg greg)
                restore = CmmAssign greg (CmmReg lreg)
            in (save, restore)

    (save_nodes, restore_nodes) = unzip nodes
    save = blockFromList save_nodes
    restore = blockFromList restore_nodes

-- | Mirrors __tsan_memory_order
-- <https://github.com/llvm/llvm-project/blob/main/compiler-rt/include/sanitizer/tsan_interface_atomic.h#L34>
memoryOrderToTsanMemoryOrder :: Env -> MemoryOrdering -> CmmExpr
memoryOrderToTsanMemoryOrder env mord =
    mkIntExpr (platform env) n
  where
    n = case mord of
      MemOrderRelaxed -> 0
      MemOrderAcquire -> 2
      MemOrderRelease -> 3
      MemOrderSeqCst  -> 5

tsanTarget :: FastString     -- ^ function name
           -> [ForeignHint]  -- ^ formals
           -> [ForeignHint]  -- ^ arguments
           -> ForeignTarget
tsanTarget fn formals args =
    ForeignTarget (CmmLit (CmmLabel lbl)) conv
  where
    conv = ForeignConvention CCallConv args formals CmmMayReturn
    lbl = mkForeignLabel fn Nothing ForeignLabelInExternalPackage IsFunction

tsanStore :: Env
          -> CmmType -> CmmExpr
          -> Block CmmNode O O
tsanStore env ty addr =
    mkUnsafeCall env ftarget [] [addr]
  where
    ftarget = tsanTarget fn [] [AddrHint]
    w = widthInBytes (typeWidth ty)
    fn = fsLit $ "__tsan_write" ++ show w

tsanLoad :: Env
         -> AlignmentSpec -> CmmType -> CmmExpr
         -> Block CmmNode O O
tsanLoad env align ty addr =
    mkUnsafeCall env ftarget [] [addr]
  where
    ftarget = tsanTarget fn [] [AddrHint]
    w = widthInBytes (typeWidth ty)
    fn = case align of
           Unaligned
             | w > 1    -> fsLit $ "__tsan_unaligned_read" ++ show w
           _            -> fsLit $ "__tsan_read" ++ show w

tsanAtomicStore :: Env
                -> MemoryOrdering -> Width -> CmmExpr -> CmmExpr
                -> Block CmmNode O O
tsanAtomicStore env mord w val addr =
    mkUnsafeCall env ftarget [] [addr, val, mord']
  where
    mord' = memoryOrderToTsanMemoryOrder env mord
    ftarget = tsanTarget fn [] [AddrHint, NoHint, NoHint]
    fn = fsLit $ "__tsan_atomic" ++ show (widthInBits w) ++ "_store"

tsanAtomicLoad :: Env
               -> MemoryOrdering -> Width -> CmmExpr -> LocalReg
               -> Block CmmNode O O
tsanAtomicLoad env mord w addr dest =
    mkUnsafeCall env ftarget [dest] [addr, mord']
  where
    mord' = memoryOrderToTsanMemoryOrder env mord
    ftarget = tsanTarget fn [NoHint] [AddrHint, NoHint]
    fn = fsLit $ "__tsan_atomic" ++ show (widthInBits w) ++ "_load"

tsanAtomicExchange :: Env
                   -> MemoryOrdering -> Width -> CmmExpr -> CmmExpr -> LocalReg
                   -> Block CmmNode O O
tsanAtomicExchange env mord w val addr dest =
    mkUnsafeCall env ftarget [dest] [addr, val, mord']
  where
    mord' = memoryOrderToTsanMemoryOrder env mord
    ftarget = tsanTarget fn [NoHint] [AddrHint, NoHint, NoHint]
    fn = fsLit $ "__tsan_atomic" ++ show (widthInBits w) ++ "_exchange"

-- N.B. C11 CAS returns a boolean (to avoid the ABA problem) whereas Cmm's CAS
-- returns the expected value. We use define a shim in the RTS to provide
-- Cmm's semantics using the TSAN C11 primitive.
tsanAtomicCas :: Env
              -> MemoryOrdering  -- ^ success ordering
              -> MemoryOrdering  -- ^ failure ordering
              -> Width
              -> CmmExpr         -- ^ address
              -> CmmExpr         -- ^ expected value
              -> CmmExpr         -- ^ new value
              -> LocalReg        -- ^ result destination
              -> Block CmmNode O O
tsanAtomicCas env mord_success mord_failure w addr expected new dest =
    mkUnsafeCall env ftarget [dest] [addr, expected, new, mord_success', mord_failure']
  where
    mord_success' = memoryOrderToTsanMemoryOrder env mord_success
    mord_failure' = memoryOrderToTsanMemoryOrder env mord_failure
    ftarget = tsanTarget fn [NoHint] [AddrHint, NoHint, NoHint, NoHint, NoHint]
    fn = fsLit $ "ghc_tsan_atomic" ++ show (widthInBits w) ++ "_compare_exchange"

tsanAtomicRMW :: Env
              -> MemoryOrdering -> AtomicMachOp -> Width -> CmmExpr -> CmmExpr -> LocalReg
              -> Block CmmNode O O
tsanAtomicRMW env mord op w addr val dest =
    mkUnsafeCall env ftarget [dest] [addr, val, mord']
  where
    mord' = memoryOrderToTsanMemoryOrder env mord
    ftarget = tsanTarget fn [NoHint] [AddrHint, NoHint, NoHint]
    op' = case op of
           AMO_Add  -> "fetch_add"
           AMO_Sub  -> "fetch_sub"
           AMO_And  -> "fetch_and"
           AMO_Nand -> "fetch_nand"
           AMO_Or   -> "fetch_or"
           AMO_Xor  -> "fetch_xor"
    fn = fsLit $ "__tsan_atomic" ++ show (widthInBits w) ++ "_" ++ op'
