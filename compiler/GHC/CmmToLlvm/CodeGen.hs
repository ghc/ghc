{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs, MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Handle conversion of CmmProc to LLVM code.
module GHC.CmmToLlvm.CodeGen ( genLlvmProc ) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Regs ( activeStgRegs )

import GHC.Llvm
import GHC.Llvm.Types
import GHC.CmmToLlvm.Base
import GHC.CmmToLlvm.Config
import GHC.CmmToLlvm.Regs

import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.Switch
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label

import GHC.Data.FastString
import GHC.Data.Maybe (expectJust)
import GHC.Data.OrdList

import GHC.Types.ForeignCall
import GHC.Types.Unique.DSM
import GHC.Types.Unique

import GHC.Utils.Outputable
import qualified GHC.Utils.Panic as Panic
import GHC.Utils.Misc

import Control.Applicative (Alternative((<|>)))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad

import qualified Data.Semigroup as Semigroup
import Data.Foldable ( toList )
import Data.List ( nub )
import qualified Data.List as List
import Data.List.NonEmpty ( NonEmpty (..), nonEmpty )
import Data.Maybe ( catMaybes )

type Atomic = Maybe MemoryOrdering
type LlvmStatements = OrdList LlvmStatement

data Signage = Signed | Unsigned deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- | Top-level of the LLVM proc Code generator
--
genLlvmProc :: RawCmmDecl -> LlvmM [LlvmCmmDecl]
genLlvmProc (CmmProc infos lbl live graph)
  | Just blocks <- nonEmpty $ toBlockListEntryFirstFalseFallthrough graph = do
    (lmblocks, lmdata) <- basicBlocksCodeGen live blocks
    let info = mapLookup (g_entry graph) infos
        proc = CmmProc info lbl live (ListGraph lmblocks)
    return (proc:lmdata)

genLlvmProc _ = panic "genLlvmProc: case that shouldn't reach here!"

-- -----------------------------------------------------------------------------
-- * Block code generation
--

-- | Unreachable basic block
--
-- See Note [Unreachable block as default destination in Switch]
newtype UnreachableBlockId = UnreachableBlockId BlockId

-- | Generate code for a list of blocks that make up a complete
-- procedure. The first block in the list is expected to be the entry
-- point.
basicBlocksCodeGen :: LiveGlobalRegUses -> NonEmpty CmmBlock
                      -> LlvmM ([LlvmBasicBlock], [LlvmCmmDecl])
basicBlocksCodeGen live cmmBlocks
  = do -- Emit the prologue
       -- N.B. this must be its own block to ensure that the entry block of the
       -- procedure has no predecessors, as required by the LLVM IR. See #17589
       -- and #11649.
       bid <- newBlockId
       (prologue, prologueTops) <- funPrologue live cmmBlocks
       let entryBlock = BasicBlock bid (fromOL prologue)

       -- allocate one unreachable basic block that can be used as a default
       -- destination in exhaustive switches.
       --
       -- See Note [Unreachable block as default destination in Switch]
       ubid@(UnreachableBlockId ubid') <- UnreachableBlockId <$> newBlockId
       let ubblock = BasicBlock ubid' [Unreachable]

       -- Generate code
       (blocks, topss) <- fmap unzip $ mapM (basicBlockCodeGen ubid) $ toList cmmBlocks

       -- Compose
       return (entryBlock : ubblock : blocks, prologueTops ++ concat topss)


-- | Generate code for one block
basicBlockCodeGen :: UnreachableBlockId -> CmmBlock -> LlvmM ( LlvmBasicBlock, [LlvmCmmDecl] )
basicBlockCodeGen ubid block
  = do let (_, nodes, tail)  = blockSplit block
           id = entryLabel block
       (mid_instrs, top) <- stmtsToInstrs ubid $ blockToList nodes
       (tail_instrs, top')  <- stmtToInstrs ubid tail
       let instrs = fromOL (mid_instrs `appOL` tail_instrs)
       return (BasicBlock id instrs, top' ++ top)

-- -----------------------------------------------------------------------------
-- * CmmNode code generation
--

-- A statement conversion return data.
--   * LlvmStatements: The compiled LLVM statements.
--   * LlvmCmmDecl: Any global data needed.
type StmtData = (LlvmStatements, [LlvmCmmDecl])


-- | Convert a list of CmmNode's to LlvmStatement's
stmtsToInstrs :: UnreachableBlockId -> [CmmNode e x] -> LlvmM StmtData
stmtsToInstrs ubid stmts
   = do (instrss, topss) <- fmap unzip $ mapM (stmtToInstrs ubid) stmts
        return (concatOL instrss, concat topss)


-- | Convert a CmmStmt to a list of LlvmStatement's
stmtToInstrs :: UnreachableBlockId -> CmmNode e x -> LlvmM StmtData
stmtToInstrs ubid stmt = case stmt of

    CmmComment _         -> return (nilOL, []) -- nuke comments
    CmmTick    _         -> return (nilOL, [])
    CmmUnwind  {}        -> return (nilOL, [])

    CmmAssign reg src    -> genAssign reg src
    CmmStore addr src align
                         -> genStore addr src align

    CmmBranch id         -> genBranch id
    CmmCondBranch arg true false likely
                         -> genCondBranch arg true false likely
    CmmSwitch arg ids    -> genSwitch ubid arg ids

    -- Foreign Call
    CmmUnsafeForeignCall target res args
        -> genCall target res args

    -- Tail call
    CmmCall { cml_target = arg,
              cml_args_regs = live } -> genJump arg live

    _ -> panic "Llvm.CodeGen.stmtToInstrs"

-- | Wrapper function to declare an instrinct function by function type
getInstrinct2 :: LMString -> LlvmType -> LlvmM ExprData
getInstrinct2 fname fty@(LMFunction funSig) = do

    let fv   = LMGlobalVar fname fty (funcLinkage funSig) Nothing Nothing Constant

    fn <- funLookup fname
    tops <- case fn of
      Just _  ->
        return []
      Nothing -> do
        funInsert fname fty
        un <- getUniqueM
        let lbl = mkAsmTempLabel un
        return [CmmData (Section Data lbl) [([],[fty])]]

    return (fv, nilOL, tops)

getInstrinct2 _ _ = error "getInstrinct2: Non-function type!"

-- | Declares an instrinct function by return and parameter types
getInstrinct :: LMString -> LlvmType -> [LlvmType] -> LlvmM ExprData
getInstrinct fname retTy parTys =
    let funSig = LlvmFunctionDecl fname ExternallyVisible CC_Ccc retTy
                    FixedArgs (tysToParams parTys) Nothing
        fty = LMFunction funSig
    in getInstrinct2 fname fty

-- | Foreign Calls
genCall :: ForeignTarget -> [CmmFormal] -> [CmmActual] -> LlvmM StmtData

-- Barriers need to be handled specially as they are implemented as LLVM
-- intrinsic functions.
genCall (PrimTarget MO_AcquireFence) _ _ = runStmtsDecls $
    statement $ Fence False SyncAcquire
genCall (PrimTarget MO_ReleaseFence) _ _ = runStmtsDecls $
    statement $ Fence False SyncRelease
genCall (PrimTarget MO_SeqCstFence) _ _ = runStmtsDecls $
    statement $ Fence False SyncSeqCst

genCall (PrimTarget MO_Touch) _ _ =
    return (nilOL, [])

genCall (PrimTarget (MO_UF_Conv w)) [dst] [e] = runStmtsDecls $ do
    (dstV, ty) <- getCmmRegW (CmmLocal dst)
    let width = widthToLlvmFloat w
    castV <- lift $ mkLocalVar ty
    ve <- exprToVarW e
    statement $ Assignment castV $ Cast LM_Uitofp ve width
    statement $ Store castV dstV Nothing []

genCall (PrimTarget (MO_UF_Conv _)) [_] args =
    panic $ "genCall: Too many arguments to MO_UF_Conv. " ++
    "Can only handle 1, given" ++ show (length args) ++ "."

-- Handle prefetching data
genCall t@(PrimTarget (MO_Prefetch_Data localityInt)) [] args
  | 0 <= localityInt && localityInt <= 3 = runStmtsDecls $ do
    let argTy = [i8Ptr, i32, i32, i32]
        funTy = \name -> LMFunction $ LlvmFunctionDecl name ExternallyVisible
                             CC_Ccc LMVoid FixedArgs (tysToParams argTy) Nothing

    let (_, arg_hints) = foreignTargetHints t
    let args_hints' = zip args arg_hints
    argVars <- arg_varsW args_hints' ([], nilOL, [])
    fptr    <- liftExprData $ getFunPtr funTy t
    argVars' <- castVarsW Signed $ zip argVars argTy

    let argSuffix = [mkIntLit i32 0, mkIntLit i32 localityInt, mkIntLit i32 1]
    statement $ Expr $ Call StdCall fptr (argVars' ++ argSuffix) []
  | otherwise = panic $ "prefetch locality level integer must be between 0 and 3, given: " ++ (show localityInt)

-- Handle PopCnt, Clz, Ctz, and BSwap that need to only convert arg
-- and return types
genCall t@(PrimTarget (MO_PopCnt w)) dsts args =
    genCallSimpleCast w t dsts args

genCall t@(PrimTarget (MO_Pdep w)) dsts args =
    genCallSimpleCast2 w t dsts args
genCall t@(PrimTarget (MO_Pext w)) dsts args =
    genCallSimpleCast2 w t dsts args
genCall t@(PrimTarget (MO_Clz w)) dsts args =
    genCallSimpleCast w t dsts args
genCall t@(PrimTarget (MO_Ctz w)) dsts args =
    genCallSimpleCast w t dsts args
genCall t@(PrimTarget (MO_BSwap w)) dsts args =
    genCallSimpleCast w t dsts args
genCall t@(PrimTarget (MO_BRev w)) dsts args =
    genCallSimpleCast w t dsts args

genCall (PrimTarget (MO_AtomicRMW width amop)) [dst] [addr, n] = runStmtsDecls $ do
    addrVar <- exprToVarW addr
    nVar <- exprToVarW n
    let targetTy = widthToLlvmInt width
        ptrExpr = Cast LM_Inttoptr addrVar (pLift targetTy)
    ptrVar <- doExprW (pLift targetTy) ptrExpr
    (dstVar, _dst_ty) <- getCmmRegW (CmmLocal dst)
    let op = case amop of
               AMO_Add  -> LAO_Add
               AMO_Sub  -> LAO_Sub
               AMO_And  -> LAO_And
               AMO_Nand -> LAO_Nand
               AMO_Or   -> LAO_Or
               AMO_Xor  -> LAO_Xor
    retVar <- doExprW targetTy $ AtomicRMW op ptrVar nVar SyncSeqCst
    statement $ Store retVar dstVar Nothing []

genCall (PrimTarget (MO_AtomicRead _ mem_ord)) [dst] [addr] = runStmtsDecls $ do
    (dstV, _dst_ty) <- getCmmRegW (CmmLocal dst)
    v1 <- genLoadW (Just mem_ord) addr (localRegType dst) NaturallyAligned
    statement $ Store v1 dstV Nothing []

genCall (PrimTarget (MO_Cmpxchg _width))
        [dst] [addr, old, new] = runStmtsDecls $ do
    addrVar <- exprToVarW addr
    oldVar <- exprToVarW old
    newVar <- exprToVarW new
    let targetTy = getVarType oldVar
        ptrExpr = Cast LM_Inttoptr addrVar (pLift targetTy)
    ptrVar <- doExprW (pLift targetTy) ptrExpr
    (dstVar, _dst_ty) <- getCmmRegW (CmmLocal dst)
    retVar <- doExprW (LMStructU [targetTy,i1])
              $ CmpXChg ptrVar oldVar newVar SyncSeqCst SyncSeqCst
    retVar' <- doExprW targetTy $ ExtractV retVar 0
    statement $ Store retVar' dstVar Nothing []

genCall (PrimTarget (MO_Xchg _width)) [dst] [addr, val] = runStmtsDecls $ do
    (dstV, _dst_ty) <- getCmmRegW (CmmLocal dst)
    addrVar <- exprToVarW addr
    valVar <- exprToVarW val
    let ptrTy = pLift $ getVarType valVar
        ptrExpr = Cast LM_Inttoptr addrVar ptrTy
    ptrVar <- doExprW ptrTy ptrExpr
    resVar <- doExprW (getVarType valVar) (AtomicRMW LAO_Xchg ptrVar valVar SyncSeqCst)
    statement $ Store resVar dstV Nothing []

genCall (PrimTarget (MO_AtomicWrite _width mem_ord)) [] [addr, val] = runStmtsDecls $ do
    addrVar <- exprToVarW addr
    valVar <- exprToVarW val
    let ptrTy = pLift $ getVarType valVar
        ptrExpr = Cast LM_Inttoptr addrVar ptrTy
    ptrVar <- doExprW ptrTy ptrExpr
    let ordering = convertMemoryOrdering mem_ord
    statement $ Expr $ AtomicRMW LAO_Xchg ptrVar valVar ordering

-- Handle memcpy function specifically since llvm's intrinsic version takes
-- some extra parameters.
genCall t@(PrimTarget op) [] args
 | Just align <- machOpMemcpyishAlign op
 = do
   platform <- getPlatform
   runStmtsDecls $ do
    let isVolTy = [i1]
        isVolVal = [mkIntLit i1 0]
        argTy | MO_Memset _ <- op = [i8Ptr, i8,    llvmWord platform, i32] ++ isVolTy
              | otherwise         = [i8Ptr, i8Ptr, llvmWord platform, i32] ++ isVolTy
        funTy = \name -> LMFunction $ LlvmFunctionDecl name ExternallyVisible
                             CC_Ccc LMVoid FixedArgs (tysToParams argTy) Nothing

    let (_, arg_hints) = foreignTargetHints t
    let args_hints = zip args arg_hints
    argVars       <- arg_varsW args_hints ([], nilOL, [])
    fptr          <- getFunPtrW funTy t
    argVars' <- castVarsW Signed $ zip argVars argTy

    let alignVal = mkIntLit i32 align
        arguments = argVars' ++ (alignVal:isVolVal)
    statement $ Expr $ Call StdCall fptr arguments []

-- We handle MO_U_Mul2 by simply using a 'mul' instruction, but with operands
-- twice the width (we first zero-extend them), e.g., on 64-bit arch we will
-- generate 'mul' on 128-bit operands. Then we only need some plumbing to
-- extract the two 64-bit values out of 128-bit result.
genCall (PrimTarget (MO_U_Mul2 w)) [dstH, dstL] [lhs, rhs] = runStmtsDecls $ do
    let width = widthToLlvmInt w
        bitWidth = widthInBits w
        width2x = LMInt (bitWidth * 2)
    -- First zero-extend the operands ('mul' instruction requires the operands
    -- and the result to be of the same type). Note that we don't use 'castVars'
    -- because it tries to do LM_Sext.
    lhsVar <- exprToVarW lhs
    rhsVar <- exprToVarW rhs
    lhsExt <- doExprW width2x $ Cast LM_Zext lhsVar width2x
    rhsExt <- doExprW width2x $ Cast LM_Zext rhsVar width2x
    -- Do the actual multiplication (note that the result is also 2x width).
    retV <- doExprW width2x $ LlvmOp LM_MO_Mul lhsExt rhsExt
    -- Extract the lower bits of the result into retL.
    retL <- doExprW width $ Cast LM_Trunc retV width
    -- Now we unsigned right-shift the higher bits by width.
    let widthLlvmLit = LMLitVar $ LMIntLit (fromIntegral bitWidth) width
    retShifted <- doExprW width2x $ LlvmOp LM_MO_LShr retV widthLlvmLit
    -- And extract them into retH.
    retH <- doExprW width $ Cast LM_Trunc retShifted width
    (dstRegL, _dstL_ty) <- getCmmRegW (CmmLocal dstL)
    (dstRegH, _dstH_ty) <- getCmmRegW (CmmLocal dstH)
    statement $ Store retL dstRegL Nothing []
    statement $ Store retH dstRegH Nothing []

genCall (PrimTarget (MO_S_Mul2 w)) [dstC, dstH, dstL] [lhs, rhs] = runStmtsDecls $ do
    let width = widthToLlvmInt w
        bitWidth = widthInBits w
        width2x = LMInt (bitWidth * 2)
    -- First sign-extend the operands ('mul' instruction requires the operands
    -- and the result to be of the same type). Note that we don't use 'castVars'
    -- because it tries to do LM_Sext.
    lhsVar <- exprToVarW lhs
    rhsVar <- exprToVarW rhs
    lhsExt <- doExprW width2x $ Cast LM_Sext lhsVar width2x
    rhsExt <- doExprW width2x $ Cast LM_Sext rhsVar width2x
    -- Do the actual multiplication (note that the result is also 2x width).
    retV <- doExprW width2x $ LlvmOp LM_MO_Mul lhsExt rhsExt
    -- Extract the lower bits of the result into retL.
    retL <- doExprW width $ Cast LM_Trunc retV width
    -- Now we signed right-shift the higher bits by width.
    let widthLlvmLit = LMLitVar $ LMIntLit (fromIntegral bitWidth) width
    retShifted <- doExprW width2x $ LlvmOp LM_MO_AShr retV widthLlvmLit
    -- And extract them into retH.
    retH <- doExprW width $ Cast LM_Trunc retShifted width
    -- Check if the carry is useful by doing a full arithmetic right shift on
    -- retL and comparing the result with retH
    let widthLlvmLitm1 = LMLitVar $ LMIntLit (fromIntegral bitWidth - 1) width
    retH' <- doExprW width $ LlvmOp LM_MO_AShr retL widthLlvmLitm1
    retC1  <- doExprW i1 $ Compare LM_CMP_Ne retH retH' -- Compare op returns a 1-bit value (i1)
    retC   <- doExprW width $ Cast LM_Zext retC1 width  -- so we zero-extend it
    (dstRegL, _dstL_ty) <- getCmmRegW (CmmLocal dstL)
    (dstRegH, _dstH_ty) <- getCmmRegW (CmmLocal dstH)
    (dstRegC, _dstC_ty) <- getCmmRegW (CmmLocal dstC)
    statement $ Store retL dstRegL Nothing []
    statement $ Store retH dstRegH Nothing []
    statement $ Store retC dstRegC Nothing []

-- MO_U_QuotRem2 is another case we handle by widening the registers to double
-- the width and use normal LLVM instructions (similarly to the MO_U_Mul2). The
-- main difference here is that we need to combine two words into one register
-- and then use both 'udiv' and 'urem' instructions to compute the result.
genCall (PrimTarget (MO_U_QuotRem2 w))
        [dstQ, dstR] [lhsH, lhsL, rhs] = runStmtsDecls $ do
    let width = widthToLlvmInt w
        bitWidth = widthInBits w
        width2x = LMInt (bitWidth * 2)
    -- First zero-extend all parameters to double width.
    let zeroExtend expr = do
            var <- exprToVarW expr
            doExprW width2x $ Cast LM_Zext var width2x
    lhsExtH <- zeroExtend lhsH
    lhsExtL <- zeroExtend lhsL
    rhsExt <- zeroExtend rhs
    -- Now we combine the first two parameters (that represent the high and low
    -- bits of the value). So first left-shift the high bits to their position
    -- and then bit-or them with the low bits.
    let widthLlvmLit = LMLitVar $ LMIntLit (fromIntegral bitWidth) width
    lhsExtHShifted <- doExprW width2x $ LlvmOp LM_MO_Shl lhsExtH widthLlvmLit
    lhsExt <- doExprW width2x $ LlvmOp LM_MO_Or lhsExtHShifted lhsExtL
    -- Finally, we can call 'udiv' and 'urem' to compute the results.
    retExtDiv <- doExprW width2x $ LlvmOp LM_MO_UDiv lhsExt rhsExt
    retExtRem <- doExprW width2x $ LlvmOp LM_MO_URem lhsExt rhsExt
    -- And since everything is in 2x width, we need to truncate the results and
    -- then return them.
    let narrow var = doExprW width $ Cast LM_Trunc var width
    retDiv <- narrow retExtDiv
    retRem <- narrow retExtRem
    (dstRegQ, _dstQ_ty) <- lift $ getCmmReg (CmmLocal dstQ)
    (dstRegR, _dstR_ty) <- lift $ getCmmReg (CmmLocal dstR)
    statement $ Store retDiv dstRegQ Nothing []
    statement $ Store retRem dstRegR Nothing []

-- Handle the MO_{Add,Sub}IntC separately. LLVM versions return a record from
-- which we need to extract the actual values.
genCall t@(PrimTarget (MO_AddIntC w)) [dstV, dstO] [lhs, rhs] =
    genCallWithOverflow t w [dstV, dstO] [lhs, rhs]
genCall t@(PrimTarget (MO_SubIntC w)) [dstV, dstO] [lhs, rhs] =
    genCallWithOverflow t w [dstV, dstO] [lhs, rhs]

-- Similar to MO_{Add,Sub}IntC, but MO_Add2 expects the first element of the
-- return tuple to be the overflow bit and the second element to contain the
-- actual result of the addition. So we still use genCallWithOverflow but swap
-- the return registers.
genCall t@(PrimTarget (MO_Add2 w)) [dstO, dstV] [lhs, rhs] =
    genCallWithOverflow t w [dstV, dstO] [lhs, rhs]

genCall t@(PrimTarget (MO_AddWordC w)) [dstV, dstO] [lhs, rhs] =
    genCallWithOverflow t w [dstV, dstO] [lhs, rhs]

genCall t@(PrimTarget (MO_SubWordC w)) [dstV, dstO] [lhs, rhs] =
    genCallWithOverflow t w [dstV, dstO] [lhs, rhs]

-- Handle all other foreign calls and prim ops.
genCall target res args = do
  platform <- getPlatform
  runStmtsDecls $ do

    -- extract Cmm call convention, and translate to LLVM call convention
    let lmconv = case target of
            ForeignTarget _ (ForeignConvention conv _ _ _) ->
              case conv of
                 StdCallConv  -> panic "GHC.CmmToLlvm.CodeGen.genCall: StdCallConv"
                 CCallConv    -> CC_Ccc
                 CApiConv     -> CC_Ccc
                 PrimCallConv       -> panic "GHC.CmmToLlvm.CodeGen.genCall: PrimCallConv"
                 JavaScriptCallConv -> panic "GHC.CmmToLlvm.CodeGen.genCall: JavaScriptCallConv"

            PrimTarget   _ -> CC_Ccc

    {-
        CC_Ccc of the possibilities here are a worry with the use of a custom
        calling convention for passing STG args. In practice the more
        dangerous combinations (e.g StdCall + llvmGhcCC) don't occur.

        The native code generator only handles StdCall and CCallConv.
    -}

    -- parameter types
    let arg_type (_, AddrHint) = (i8Ptr, [])
        -- cast pointers to i8*. Llvm equivalent of void*
        arg_type (expr, hint) =
            case cmmToLlvmType $ cmmExprType platform expr of
              ty@(LMInt n) | n < 64 && lmconv == CC_Ccc && platformCConvNeedsExtension platform
                 -> (ty, if hint == SignedHint then [SignExt] else [ZeroExt])
              ty -> (ty, [])

    -- ret type
    let ret_type [] = LMVoid
        ret_type [(_, AddrHint)] = i8Ptr
        ret_type [(reg, _)]      = cmmToLlvmType $ localRegType reg
        ret_type t = panic $ "genCall: Too many return values! Can only handle"
                        ++ " 0 or 1, given " ++ show (length t) ++ "."

    -- call attributes
    let fnAttrs | never_returns = NoReturn : llvmStdFunAttrs
                | otherwise     = llvmStdFunAttrs

        never_returns = case target of
             ForeignTarget _ (ForeignConvention _ _ _ CmmNeverReturns) -> True
             _ -> False

    -- fun type
    let (res_hints, arg_hints) = foreignTargetHints target
    let args_hints = zip args arg_hints
    let ress_hints = zip res  res_hints
    let ccTy  = StdCall -- tail calls should be done through CmmJump
    let retTy = ret_type ress_hints
    let argTy = map arg_type args_hints
    let funTy = \name -> LMFunction $ LlvmFunctionDecl name ExternallyVisible
                             lmconv retTy FixedArgs argTy (llvmFunAlign platform)

    argVars <- arg_varsW args_hints ([], nilOL, [])
    fptr    <- getFunPtrW funTy target

    let doReturn | ccTy == TailCall  = statement $ Return Nothing
                 | never_returns     = statement $ Unreachable
                 | otherwise         = return ()


    -- make the actual call
    case retTy of
        LMVoid ->
            statement $ Expr $ Call ccTy fptr argVars fnAttrs
        _ -> do
            v1 <- doExprW retTy $ Call ccTy fptr argVars fnAttrs
            -- get the return register
            let ret_reg [reg] = reg
                ret_reg t = panic $ "genCall: Bad number of registers! Can only handle"
                                ++ " 1, given " ++ show (length t) ++ "."
            let creg = ret_reg res
            (vreg, ty) <- getCmmRegW (CmmLocal creg)
            if retTy == ty
            then do
                statement $ Store v1 vreg Nothing []
                doReturn
            else do
                let op = case ty of
                        vt | isPointer vt -> LM_Bitcast
                           | isInt     vt -> LM_Ptrtoint
                           | otherwise    ->
                               panic $ "genCall: CmmReg bad match for"
                                    ++ " returned type!"
                v2 <- doExprW ty $ Cast op v1 ty
                statement $ Store v2 vreg Nothing []
                doReturn

-- | Generate a call to an LLVM intrinsic that performs arithmetic operation
-- with overflow bit (i.e., returns a struct containing the actual result of the
-- operation and an overflow bit). This function will also extract the overflow
-- bit and zero-extend it (all the corresponding Cmm PrimOps represent the
-- overflow "bit" as a usual Int# or Word#).
genCallWithOverflow
  :: ForeignTarget -> Width -> [CmmFormal] -> [CmmActual] -> LlvmM StmtData
genCallWithOverflow t@(PrimTarget op) w [dstV, dstO] [lhs, rhs] = do
    -- So far this was only tested for the following four CallishMachOps.
    let valid = op `elem`   [ MO_Add2 w
                            , MO_AddIntC w
                            , MO_SubIntC w
                            , MO_AddWordC w
                            , MO_SubWordC w
                            ]
    Panic.massert valid
    let width = widthToLlvmInt w
    -- This will do most of the work of generating the call to the intrinsic and
    -- extracting the values from the struct.
    (value, overflowBit, (stmts, top)) <-
      genCallExtract t w (lhs, rhs) (width, i1)
    -- value is i<width>, but overflowBit is i1, so we need to cast (Cmm expects
    -- both to be i<width>)
    (overflow, zext) <- doExpr width $ Cast LM_Zext overflowBit width
    (dstRegV, _dstV_ty) <- getCmmReg (CmmLocal dstV)
    (dstRegO, _dstO_ty) <- getCmmReg (CmmLocal dstO)
    let storeV = Store value dstRegV Nothing []
        storeO = Store overflow dstRegO Nothing []
    return (stmts `snocOL` zext `snocOL` storeV `snocOL` storeO, top)
genCallWithOverflow _ _ _ _ =
    panic "genCallExtract: wrong ForeignTarget or number of arguments"

-- | A helper function for genCallWithOverflow that handles generating the call
-- to the LLVM intrinsic and extracting the result from the struct to LlvmVars.
genCallExtract
    :: ForeignTarget           -- ^ PrimOp
    -> Width                   -- ^ Width of the operands.
    -> (CmmActual, CmmActual)  -- ^ Actual arguments.
    -> (LlvmType, LlvmType)    -- ^ LLVM types of the returned struct.
    -> LlvmM (LlvmVar, LlvmVar, StmtData)
genCallExtract target@(PrimTarget op) w (argA, argB) (llvmTypeA, llvmTypeB) = do
    let width = widthToLlvmInt w
        argTy = [width, width]
        retTy = LMStructU [llvmTypeA, llvmTypeB]

    -- Process the arguments.
    let args_hints = zip [argA, argB] (snd $ foreignTargetHints target)
    (argsV1, args1, top1) <- arg_vars args_hints ([], nilOL, [])
    (argsV2, args2) <- castVars Signed $ zip argsV1 argTy

    -- Get the function and make the call.
    fname <- cmmPrimOpFunctions op
    (fptr, _, top2) <- getInstrinct fname retTy argTy
    -- We use StdCall for primops. See also the last case of genCall.
    (retV, call) <- doExpr retTy $ Call StdCall fptr argsV2 []

    -- This will result in a two element struct, we need to use "extractvalue"
    -- to get them out of it.
    (res1, ext1) <- doExpr llvmTypeA (ExtractV retV 0)
    (res2, ext2) <- doExpr llvmTypeB (ExtractV retV 1)

    let stmts = args1 `appOL` args2 `snocOL` call `snocOL` ext1 `snocOL` ext2
        tops = top1 ++ top2
    return (res1, res2, (stmts, tops))

genCallExtract _ _ _ _ =
    panic "genCallExtract: unsupported ForeignTarget"

-- Handle simple function call that only need simple type casting, of the form:
--   truncate arg >>= \a -> call(a) >>= zext
--
-- since GHC only really has i32 and i64 types and things like Word8 are backed
-- by an i32 and just present a logical i8 range. So we must handle conversions
-- from i32 to i8 explicitly as LLVM is strict about types.
genCallSimpleCast :: Width -> ForeignTarget -> [CmmFormal] -> [CmmActual]
              -> LlvmM StmtData
genCallSimpleCast w t@(PrimTarget op) [dst] args = do
    let width = widthToLlvmInt w
        dstTy = cmmToLlvmType $ localRegType dst

    fname                       <- cmmPrimOpFunctions op
    (fptr, _, top3)             <- getInstrinct fname width [width]

    (dstV, _dst_ty)             <- getCmmReg (CmmLocal dst)

    let (_, arg_hints) = foreignTargetHints t
    let args_hints = zip args arg_hints
    (argsV, stmts2, top2)       <- arg_vars args_hints ([], nilOL, [])
    (argsV', stmts4)            <- castVars Signed $ zip argsV [width]
    (retV, s1)                  <- doExpr width $ Call StdCall fptr argsV' []
    (retVs', stmts5)            <- castVars (cmmPrimOpRetValSignage op) [(retV,dstTy)]
    let retV'                    = singletonPanic "genCallSimpleCast" retVs'
    let s2                       = Store retV' dstV Nothing []

    let stmts = stmts2 `appOL` stmts4 `snocOL`
                s1 `appOL` stmts5 `snocOL` s2
    return (stmts, top2 ++ top3)
genCallSimpleCast _ _ dsts _ =
    panic ("genCallSimpleCast: " ++ show (length dsts) ++ " dsts")

-- Handle simple function call that only need simple type casting, of the form:
--   truncate arg >>= \a -> call(a) >>= zext
--
-- since GHC only really has i32 and i64 types and things like Word8 are backed
-- by an i32 and just present a logical i8 range. So we must handle conversions
-- from i32 to i8 explicitly as LLVM is strict about types.
genCallSimpleCast2 :: Width -> ForeignTarget -> [CmmFormal] -> [CmmActual]
              -> LlvmM StmtData
genCallSimpleCast2 w t@(PrimTarget op) [dst] args = do
    let width = widthToLlvmInt w
        dstTy = cmmToLlvmType $ localRegType dst

    fname                       <- cmmPrimOpFunctions op
    (fptr, _, top3)             <- getInstrinct fname width (const width <$> args)

    (dstV, _dst_ty)             <- getCmmReg (CmmLocal dst)

    let (_, arg_hints) = foreignTargetHints t
    let args_hints = zip args arg_hints
    (argsV, stmts2, top2)       <- arg_vars args_hints ([], nilOL, [])
    (argsV', stmts4)            <- castVars Signed $ zip argsV (const width <$> argsV)
    (retV, s1)                  <- doExpr width $ Call StdCall fptr argsV' []
    (retVs', stmts5)             <- castVars (cmmPrimOpRetValSignage op) [(retV,dstTy)]
    let retV'                    = singletonPanic "genCallSimpleCast2" retVs'
    let s2                       = Store retV' dstV Nothing []

    let stmts = stmts2 `appOL` stmts4 `snocOL`
                s1 `appOL` stmts5 `snocOL` s2
    return (stmts, top2 ++ top3)
genCallSimpleCast2 _ _ dsts _ =
    panic ("genCallSimpleCast2: " ++ show (length dsts) ++ " dsts")

-- | Create a function pointer from a target.
getFunPtrW :: (LMString -> LlvmType) -> ForeignTarget
           -> WriterT LlvmAccum LlvmM LlvmVar
getFunPtrW funTy targ = liftExprData $ getFunPtr funTy targ

-- | Create a function pointer from a target.
getFunPtr :: (LMString -> LlvmType) -> ForeignTarget
          -> LlvmM ExprData
getFunPtr funTy targ = case targ of
    ForeignTarget (CmmLit (CmmLabel lbl)) _ -> do
        name <- strCLabel_llvm lbl
        getHsFunc' name (funTy name)

    ForeignTarget expr _ -> do
        (v1, stmts, top) <- exprToVar expr
        let fty = funTy $ fsLit "dynamic"
            cast = case getVarType v1 of
                ty | isPointer ty -> LM_Bitcast
                ty | isInt ty     -> LM_Inttoptr

                ty -> pprPanic "genCall: Expr is of bad type for function" $
                  text " call! " <> lparen <> ppr ty <> rparen

        (v2,s1) <- doExpr (pLift fty) $ Cast cast v1 (pLift fty)
        return (v2, stmts `snocOL` s1, top)

    PrimTarget mop -> do
        name <- cmmPrimOpFunctions mop
        let fty = funTy name
        getInstrinct2 name fty

-- | Conversion of call arguments.
arg_varsW :: [(CmmActual, ForeignHint)]
          -> ([LlvmVar], LlvmStatements, [LlvmCmmDecl])
          -> WriterT LlvmAccum LlvmM [LlvmVar]
arg_varsW xs ys = do
    (vars, stmts, decls) <- lift $ arg_vars xs ys
    tell $ LlvmAccum stmts decls
    return vars

-- | Conversion of call arguments.
arg_vars :: [(CmmActual, ForeignHint)]
         -> ([LlvmVar], LlvmStatements, [LlvmCmmDecl])
         -> LlvmM ([LlvmVar], LlvmStatements, [LlvmCmmDecl])

arg_vars [] (vars, stmts, tops)
  = return (vars, stmts, tops)

arg_vars ((e, AddrHint):rest) (vars, stmts, tops)
  = do (v1, stmts', top') <- exprToVar e
       let op = case getVarType v1 of
               ty | isPointer ty -> LM_Bitcast
               ty | isInt ty     -> LM_Inttoptr

               a  -> pprPanic "genCall: Can't cast llvmType to i8*! " $
                lparen <>  ppr a <> rparen

       (v2, s1) <- doExpr i8Ptr $ Cast op v1 i8Ptr
       arg_vars rest (vars ++ [v2], stmts `appOL` stmts' `snocOL` s1,
                               tops ++ top')

arg_vars ((e, _):rest) (vars, stmts, tops)
  = do (v1, stmts', top') <- exprToVar e
       arg_vars rest (vars ++ [v1], stmts `appOL` stmts', tops ++ top')


-- | Cast a collection of LLVM variables to specific types.
castVarsW :: Signage
          -> [(LlvmVar, LlvmType)]
          -> WriterT LlvmAccum LlvmM [LlvmVar]
castVarsW signage vars = do
    (vars, stmts) <- lift $ castVars signage vars
    tell $ LlvmAccum stmts mempty
    return vars

-- | Cast a collection of LLVM variables to specific types.
castVars :: Signage -> [(LlvmVar, LlvmType)]
         -> LlvmM ([LlvmVar], LlvmStatements)
castVars signage vars = do
                done <- mapM (uncurry (castVar signage)) vars
                let (vars', stmts) = unzip done
                return (vars', toOL stmts)

-- | Cast an LLVM variable to a specific type, panicking if it can't be done.
castVar :: Signage -> LlvmVar -> LlvmType -> LlvmM (LlvmVar, LlvmStatement)
castVar signage v t | getVarType v == t
            = return (v, Nop)

            | otherwise
            = do platform <- getPlatform
                 let op = case (getVarType v, t) of
                      (LMInt n, LMInt m)
                          -> if n < m then extend else LM_Trunc
                      (vt, _) | isFloat vt && isFloat t
                          -> if llvmWidthInBits platform vt < llvmWidthInBits platform t
                                then LM_Fpext else LM_Fptrunc
                      (vt, _) | isInt vt && isFloat t       -> LM_Sitofp
                      (vt, _) | isFloat vt && isInt t       -> LM_Fptosi
                      (vt, _) | isInt vt && isPointer t     -> LM_Inttoptr
                      (vt, _) | isPointer vt && isInt t     -> LM_Ptrtoint
                      (vt, _) | isPointer vt && isPointer t -> LM_Bitcast
                      (vt, _) | isVector vt && isVector t   -> LM_Bitcast

                      (vt, _) -> pprPanic "castVars: Can't cast this type " $
                                lparen <> ppr vt <> rparen
                                <> text " to " <>
                                lparen <> ppr t <> rparen

                 doExpr t $ Cast op v t
    where extend = case signage of
            Signed      -> LM_Sext
            Unsigned    -> LM_Zext


cmmPrimOpRetValSignage :: CallishMachOp -> Signage
cmmPrimOpRetValSignage mop = case mop of
    MO_Pdep _   -> Unsigned
    MO_Pext _   -> Unsigned
    _           -> Signed

-- | Decide what C function to use to implement a CallishMachOp
cmmPrimOpFunctions :: CallishMachOp -> LlvmM LMString
cmmPrimOpFunctions mop = do
  cfg      <- getConfig
  platform <- getPlatform
  let !isBmi2Enabled = llvmCgBmiVersion cfg >= Just BMI2
      !is32bit       = platformWordSize platform == PW4
      unsupported = panic ("cmmPrimOpFunctions: " ++ show mop
                        ++ " not supported here")
      dontReach64 = panic ("cmmPrimOpFunctions: " ++ show mop
                        ++ " should be not be encountered because the regular primop for this 64-bit operation is used instead.")

  return $ case mop of
    MO_F32_Exp    -> fsLit "expf"
    MO_F32_ExpM1  -> fsLit "expm1f"
    MO_F32_Log    -> fsLit "logf"
    MO_F32_Log1P  -> fsLit "log1pf"
    MO_F32_Sqrt   -> fsLit "llvm.sqrt.f32"
    MO_F32_Fabs   -> fsLit "llvm.fabs.f32"
    MO_F32_Pwr    -> fsLit "llvm.pow.f32"

    MO_F32_Sin    -> fsLit "llvm.sin.f32"
    MO_F32_Cos    -> fsLit "llvm.cos.f32"
    MO_F32_Tan    -> fsLit "tanf"

    MO_F32_Asin   -> fsLit "asinf"
    MO_F32_Acos   -> fsLit "acosf"
    MO_F32_Atan   -> fsLit "atanf"

    MO_F32_Sinh   -> fsLit "sinhf"
    MO_F32_Cosh   -> fsLit "coshf"
    MO_F32_Tanh   -> fsLit "tanhf"

    MO_F32_Asinh  -> fsLit "asinhf"
    MO_F32_Acosh  -> fsLit "acoshf"
    MO_F32_Atanh  -> fsLit "atanhf"

    MO_F64_Exp    -> fsLit "exp"
    MO_F64_ExpM1  -> fsLit "expm1"
    MO_F64_Log    -> fsLit "log"
    MO_F64_Log1P  -> fsLit "log1p"
    MO_F64_Sqrt   -> fsLit "llvm.sqrt.f64"
    MO_F64_Fabs   -> fsLit "llvm.fabs.f64"
    MO_F64_Pwr    -> fsLit "llvm.pow.f64"

    MO_F64_Sin    -> fsLit "llvm.sin.f64"
    MO_F64_Cos    -> fsLit "llvm.cos.f64"
    MO_F64_Tan    -> fsLit "tan"

    MO_F64_Asin   -> fsLit "asin"
    MO_F64_Acos   -> fsLit "acos"
    MO_F64_Atan   -> fsLit "atan"

    MO_F64_Sinh   -> fsLit "sinh"
    MO_F64_Cosh   -> fsLit "cosh"
    MO_F64_Tanh   -> fsLit "tanh"

    MO_F64_Asinh  -> fsLit "asinh"
    MO_F64_Acosh  -> fsLit "acosh"
    MO_F64_Atanh  -> fsLit "atanh"

    -- In the following ops, it looks like we could factorize the concatenation
    -- of the bit size, and indeed it was like this before, e.g.
    --
    --     MO_PopCnt w -> fsLit $ "llvm.ctpop.i" ++ wbits w
    -- or
    --     MO_Memcpy _ -> fsLit $ "llvm.memcpy."  ++ intrinTy1
    --
    -- however it meant that FastStrings were not built from constant string
    -- literals, hence they weren't matching the "fslit" rewrite rule in
    -- GHC.Data.FastString that computes the string size at compilation time.

    MO_Memcpy _
      | is32bit   -> fsLit "llvm.memcpy.p0i8.p0i8.i32"
      | otherwise -> fsLit "llvm.memcpy.p0i8.p0i8.i64"
    MO_Memmove _
      | is32bit   -> fsLit "llvm.memmove.p0i8.p0i8.i32"
      | otherwise -> fsLit "llvm.memmove.p0i8.p0i8.i64"
    MO_Memset _
       | is32bit   -> fsLit "llvm.memset.p0i8.i32"
       | otherwise -> fsLit "llvm.memset.p0i8.i64"
    MO_Memcmp _   -> fsLit "memcmp"

    MO_SuspendThread -> fsLit "suspendThread"
    MO_ResumeThread  -> fsLit "resumeThread"

    MO_PopCnt w -> case w of
      W8   -> fsLit "llvm.ctpop.i8"
      W16  -> fsLit "llvm.ctpop.i16"
      W32  -> fsLit "llvm.ctpop.i32"
      W64  -> fsLit "llvm.ctpop.i64"
      W128 -> fsLit "llvm.ctpop.i128"
      W256 -> fsLit "llvm.ctpop.i256"
      W512 -> fsLit "llvm.ctpop.i512"
    MO_BSwap w  -> case w of
      W8   -> fsLit "llvm.bswap.i8"
      W16  -> fsLit "llvm.bswap.i16"
      W32  -> fsLit "llvm.bswap.i32"
      W64  -> fsLit "llvm.bswap.i64"
      W128 -> fsLit "llvm.bswap.i128"
      W256 -> fsLit "llvm.bswap.i256"
      W512 -> fsLit "llvm.bswap.i512"
    MO_BRev w   -> case w of
      W8   -> fsLit "llvm.bitreverse.i8"
      W16  -> fsLit "llvm.bitreverse.i16"
      W32  -> fsLit "llvm.bitreverse.i32"
      W64  -> fsLit "llvm.bitreverse.i64"
      W128 -> fsLit "llvm.bitreverse.i128"
      W256 -> fsLit "llvm.bitreverse.i256"
      W512 -> fsLit "llvm.bitreverse.i512"
    MO_Clz w    -> case w of
      W8   -> fsLit "llvm.ctlz.i8"
      W16  -> fsLit "llvm.ctlz.i16"
      W32  -> fsLit "llvm.ctlz.i32"
      W64  -> fsLit "llvm.ctlz.i64"
      W128 -> fsLit "llvm.ctlz.i128"
      W256 -> fsLit "llvm.ctlz.i256"
      W512 -> fsLit "llvm.ctlz.i512"
    MO_Ctz w    -> case w of
      W8   -> fsLit "llvm.cttz.i8"
      W16  -> fsLit "llvm.cttz.i16"
      W32  -> fsLit "llvm.cttz.i32"
      W64  -> fsLit "llvm.cttz.i64"
      W128 -> fsLit "llvm.cttz.i128"
      W256 -> fsLit "llvm.cttz.i256"
      W512 -> fsLit "llvm.cttz.i512"
    MO_Pdep w
      | isBmi2Enabled -> case w of
          W8   -> fsLit "llvm.x86.bmi.pdep.8"
          W16  -> fsLit "llvm.x86.bmi.pdep.16"
          W32  -> fsLit "llvm.x86.bmi.pdep.32"
          W64  -> fsLit "llvm.x86.bmi.pdep.64"
          W128 -> fsLit "llvm.x86.bmi.pdep.128"
          W256 -> fsLit "llvm.x86.bmi.pdep.256"
          W512 -> fsLit "llvm.x86.bmi.pdep.512"
      | otherwise -> case w of
          W8   -> fsLit "hs_pdep8"
          W16  -> fsLit "hs_pdep16"
          W32  -> fsLit "hs_pdep32"
          W64  -> fsLit "hs_pdep64"
          W128 -> fsLit "hs_pdep128"
          W256 -> fsLit "hs_pdep256"
          W512 -> fsLit "hs_pdep512"
    MO_Pext w
      | isBmi2Enabled -> case w of
          W8   -> fsLit "llvm.x86.bmi.pext.8"
          W16  -> fsLit "llvm.x86.bmi.pext.16"
          W32  -> fsLit "llvm.x86.bmi.pext.32"
          W64  -> fsLit "llvm.x86.bmi.pext.64"
          W128 -> fsLit "llvm.x86.bmi.pext.128"
          W256 -> fsLit "llvm.x86.bmi.pext.256"
          W512 -> fsLit "llvm.x86.bmi.pext.512"
      | otherwise -> case w of
          W8   -> fsLit "hs_pext8"
          W16  -> fsLit "hs_pext16"
          W32  -> fsLit "hs_pext32"
          W64  -> fsLit "hs_pext64"
          W128 -> fsLit "hs_pext128"
          W256 -> fsLit "hs_pext256"
          W512 -> fsLit "hs_pext512"

    MO_AddIntC w    -> case w of
      W8   -> fsLit "llvm.sadd.with.overflow.i8"
      W16  -> fsLit "llvm.sadd.with.overflow.i16"
      W32  -> fsLit "llvm.sadd.with.overflow.i32"
      W64  -> fsLit "llvm.sadd.with.overflow.i64"
      W128 -> fsLit "llvm.sadd.with.overflow.i128"
      W256 -> fsLit "llvm.sadd.with.overflow.i256"
      W512 -> fsLit "llvm.sadd.with.overflow.i512"
    MO_SubIntC w    -> case w of
      W8   -> fsLit "llvm.ssub.with.overflow.i8"
      W16  -> fsLit "llvm.ssub.with.overflow.i16"
      W32  -> fsLit "llvm.ssub.with.overflow.i32"
      W64  -> fsLit "llvm.ssub.with.overflow.i64"
      W128 -> fsLit "llvm.ssub.with.overflow.i128"
      W256 -> fsLit "llvm.ssub.with.overflow.i256"
      W512 -> fsLit "llvm.ssub.with.overflow.i512"
    MO_Add2 w       -> case w of
      W8   -> fsLit "llvm.uadd.with.overflow.i8"
      W16  -> fsLit "llvm.uadd.with.overflow.i16"
      W32  -> fsLit "llvm.uadd.with.overflow.i32"
      W64  -> fsLit "llvm.uadd.with.overflow.i64"
      W128 -> fsLit "llvm.uadd.with.overflow.i128"
      W256 -> fsLit "llvm.uadd.with.overflow.i256"
      W512 -> fsLit "llvm.uadd.with.overflow.i512"
    MO_AddWordC w   -> case w of
      W8   -> fsLit "llvm.uadd.with.overflow.i8"
      W16  -> fsLit "llvm.uadd.with.overflow.i16"
      W32  -> fsLit "llvm.uadd.with.overflow.i32"
      W64  -> fsLit "llvm.uadd.with.overflow.i64"
      W128 -> fsLit "llvm.uadd.with.overflow.i128"
      W256 -> fsLit "llvm.uadd.with.overflow.i256"
      W512 -> fsLit "llvm.uadd.with.overflow.i512"
    MO_SubWordC w   -> case w of
      W8   -> fsLit "llvm.usub.with.overflow.i8"
      W16  -> fsLit "llvm.usub.with.overflow.i16"
      W32  -> fsLit "llvm.usub.with.overflow.i32"
      W64  -> fsLit "llvm.usub.with.overflow.i64"
      W128 -> fsLit "llvm.usub.with.overflow.i128"
      W256 -> fsLit "llvm.usub.with.overflow.i256"
      W512 -> fsLit "llvm.usub.with.overflow.i512"


    MO_Prefetch_Data _ -> fsLit "llvm.prefetch"

    MO_S_Mul2    {}  -> unsupported
    MO_S_QuotRem {}  -> unsupported
    MO_U_QuotRem {}  -> unsupported
    MO_U_QuotRem2 {} -> unsupported
    -- We support MO_U_Mul2 through ordinary LLVM mul instruction, see the
    -- appropriate case of genCall.
    MO_U_Mul2 {}     -> unsupported

    MO_ReleaseFence  -> unsupported
    MO_AcquireFence  -> unsupported
    MO_SeqCstFence   -> unsupported

    MO_Touch         -> unsupported
    MO_UF_Conv _     -> unsupported

    MO_AtomicRead _ _  -> unsupported
    MO_AtomicRMW _ _   -> unsupported
    MO_AtomicWrite _ _ -> unsupported
    MO_Cmpxchg _       -> unsupported
    MO_Xchg _          -> unsupported

    MO_I64_ToI       -> dontReach64
    MO_I64_FromI     -> dontReach64
    MO_W64_ToW       -> dontReach64
    MO_W64_FromW     -> dontReach64
    MO_x64_Neg       -> dontReach64
    MO_x64_Add       -> dontReach64
    MO_x64_Sub       -> dontReach64
    MO_x64_Mul       -> dontReach64
    MO_I64_Quot      -> dontReach64
    MO_I64_Rem       -> dontReach64
    MO_W64_Quot      -> dontReach64
    MO_W64_Rem       -> dontReach64
    MO_x64_And       -> dontReach64
    MO_x64_Or        -> dontReach64
    MO_x64_Xor       -> dontReach64
    MO_x64_Not       -> dontReach64
    MO_x64_Shl       -> dontReach64
    MO_I64_Shr       -> dontReach64
    MO_W64_Shr       -> dontReach64
    MO_x64_Eq        -> dontReach64
    MO_x64_Ne        -> dontReach64
    MO_I64_Ge        -> dontReach64
    MO_I64_Gt        -> dontReach64
    MO_I64_Le        -> dontReach64
    MO_I64_Lt        -> dontReach64
    MO_W64_Ge        -> dontReach64
    MO_W64_Gt        -> dontReach64
    MO_W64_Le        -> dontReach64
    MO_W64_Lt        -> dontReach64


-- | Tail function calls
genJump :: CmmExpr -> LiveGlobalRegUses -> LlvmM StmtData

-- Call to known function
genJump (CmmLit (CmmLabel lbl)) live = do
    (vf, stmts, top) <- getHsFunc live lbl
    (stgRegs, stgStmts) <- funEpilogue live
    let s1  = Expr $ Call TailCall vf stgRegs llvmStdFunAttrs
    let s2  = Return Nothing
    return (stmts `appOL` stgStmts `snocOL` s1 `snocOL` s2, top)


-- Call to unknown function / address
genJump expr live = do
    fty <- llvmFunTy live
    (vf, stmts, top) <- exprToVar expr

    let cast = case getVarType vf of
         ty | isPointer ty -> LM_Bitcast
         ty | isInt ty     -> LM_Inttoptr

         ty -> pprPanic "genJump: Expr is of bad type for function call! "
                $ lparen <> ppr ty <> rparen

    (v1, s1) <- doExpr (pLift fty) $ Cast cast vf (pLift fty)
    (stgRegs, stgStmts) <- funEpilogue live
    let s2 = Expr $ Call TailCall v1 stgRegs llvmStdFunAttrs
    let s3 = Return Nothing
    return (stmts `snocOL` s1 `appOL` stgStmts `snocOL` s2 `snocOL` s3,
            top)


-- | CmmAssign operation
--
-- We use stack allocated variables for CmmReg. The optimiser will replace
-- these with registers when possible.
genAssign :: CmmReg -> CmmExpr -> LlvmM StmtData
genAssign reg val = do
    (vreg, ty) <- getCmmReg reg
    (vval, stmts2, top2) <- exprToVar val
    let stmts = stmts2
    platform <- getPlatform
    case ty of
      -- Some registers are pointer types, so need to cast value to pointer
      LMPointer _ | getVarType vval == llvmWord platform -> do
          (v, s1) <- doExpr ty $ Cast LM_Inttoptr vval ty
          let s2 = Store v vreg Nothing []
          return (stmts `snocOL` s1 `snocOL` s2, top2)

      LMVector _ _ -> do
          (v, s1) <- doExpr ty $ Cast LM_Bitcast vval ty
          let s2 = mkStore v vreg NaturallyAligned []
          return (stmts `snocOL` s1 `snocOL` s2, top2)

      _ -> do
          let s1 = Store vval vreg Nothing []
          return (stmts `snocOL` s1, top2)


-- | CmmStore operation
genStore :: CmmExpr -> CmmExpr -> AlignmentSpec -> LlvmM StmtData

-- First we try to detect a few common cases and produce better code for
-- these then the default case. We are mostly trying to detect Cmm code
-- like I32[Sp + n] and use 'getelementptr' operations instead of the
-- generic case that uses casts and pointer arithmetic
genStore addr@(CmmReg (CmmGlobal r)) val alignment
    = genStore_fast addr r 0 val alignment

genStore addr@(CmmRegOff (CmmGlobal r) n) val alignment
    = genStore_fast addr r n val alignment

genStore addr@(CmmMachOp (MO_Add _) [
                            (CmmReg (CmmGlobal r)),
                            (CmmLit (CmmInt n _))])
                val alignment
    = genStore_fast addr r (fromInteger n) val alignment

genStore addr@(CmmMachOp (MO_Sub _) [
                            (CmmReg (CmmGlobal r)),
                            (CmmLit (CmmInt n _))])
                val alignment
    = genStore_fast addr r (negate $ fromInteger n) val alignment

-- generic case
genStore addr val alignment
    = getTBAAMeta topN >>= genStore_slow addr val alignment

-- | CmmStore operation
-- This is a special case for storing to a global register pointer
-- offset such as I32[Sp+8].
genStore_fast :: CmmExpr -> GlobalRegUse -> Int -> CmmExpr -> AlignmentSpec
              -> LlvmM StmtData
genStore_fast addr r n val alignment
  = do platform <- getPlatform
       (gv, grt, s1) <- getCmmRegVal (CmmGlobal r)
       meta          <- getTBAARegMeta (globalRegUse_reg r)
       let (ix,rem) = n `divMod` ((llvmWidthInBits platform . pLower) grt  `div` 8)
       case isPointer grt && rem == 0 of
            True -> do
                (vval,  stmts, top) <- exprToVar val
                (ptr, s2) <- doExpr grt $ GetElemPtr True gv [toI32 ix]
                -- We might need a different pointer type, so check
                case pLower grt == getVarType vval of
                     -- were fine
                     True  -> do
                         let s3 = mkStore vval ptr alignment meta
                         return (stmts `appOL` s1 `snocOL` s2
                                 `snocOL` s3, top)

                     -- cast to pointer type needed
                     False -> do
                         let ty = (pLift . getVarType) vval
                         (ptr', s3) <- doExpr ty $ Cast LM_Bitcast ptr ty
                         let s4 = mkStore vval ptr' alignment meta
                         return (stmts `appOL` s1 `snocOL` s2
                                 `snocOL` s3 `snocOL` s4, top)

            -- If its a bit type then we use the slow method since
            -- we can't avoid casting anyway.
            False -> genStore_slow addr val alignment meta


-- | CmmStore operation
-- Generic case. Uses casts and pointer arithmetic if needed.
genStore_slow :: CmmExpr -> CmmExpr -> AlignmentSpec -> [MetaAnnot] -> LlvmM StmtData
genStore_slow addr val alignment meta = do
    (vaddr, stmts1, top1) <- exprToVar addr
    (vval,  stmts2, top2) <- exprToVar val

    let stmts = stmts1 `appOL` stmts2
    platform <- getPlatform
    cfg      <- getConfig
    case getVarType vaddr of
        -- sometimes we need to cast an int to a pointer before storing
        LMPointer ty@(LMPointer _) | getVarType vval == llvmWord platform -> do
            (v, s1) <- doExpr ty $ Cast LM_Inttoptr vval ty
            let s2 = mkStore v vaddr alignment meta
            return (stmts `snocOL` s1 `snocOL` s2, top1 ++ top2)

        LMPointer _ -> do
            let s1 = mkStore vval vaddr alignment meta
            return (stmts `snocOL` s1, top1 ++ top2)

        i@(LMInt _) | i == llvmWord platform -> do
            let vty = pLift $ getVarType vval
            (vptr, s1) <- doExpr vty $ Cast LM_Inttoptr vaddr vty
            let s2 = mkStore vval vptr alignment meta
            return (stmts `snocOL` s1 `snocOL` s2, top1 ++ top2)

        other ->
            pprPanic "genStore: ptr not right type!"
                    (pdoc platform addr $$
                     text "Size of Ptr:" <+> ppr (llvmPtrBits platform) $$
                     text "Size of var:" <+> ppr (llvmWidthInBits platform other) $$
                     text "Var:"         <+> ppVar cfg vaddr)

mkStore :: LlvmVar -> LlvmVar -> AlignmentSpec -> [MetaAnnot] -> LlvmStatement
mkStore vval vptr alignment metas =
    Store vval vptr align metas
  where
    ty = pLower (getVarType vptr)
    align = case alignment of
              -- See Note [Alignment of vector-typed values]
              _ | isVector ty  -> Just 1
              Unaligned        -> Just 1
              NaturallyAligned -> Nothing

-- | Unconditional branch
genBranch :: BlockId -> LlvmM StmtData
genBranch id =
    let label = blockIdToLlvm id
    in return (unitOL $ Branch label, [])


-- | Conditional branch
genCondBranch :: CmmExpr -> BlockId -> BlockId -> Maybe Bool -> LlvmM StmtData
genCondBranch cond idT idF likely = do
    let labelT = blockIdToLlvm idT
    let labelF = blockIdToLlvm idF
    -- See Note [Literals and branch conditions].
    (vc, stmts1, top1) <- exprToVarOpt i1Option cond
    if getVarType vc == i1
        then do
            (vc', (stmts2, top2)) <- case likely of
              Just b -> genExpectLit (if b then 1 else 0) i1  vc
              _      -> pure (vc, (nilOL, []))
            let s1 = BranchIf vc' labelT labelF
            return (stmts1 `appOL` stmts2 `snocOL` s1, top1 ++ top2)
        else do
            cfg <- getConfig
            pprPanic "genCondBranch: Cond expr not bool! " $
              lparen <> ppVar cfg vc <> rparen


-- | Generate call to llvm.expect.x intrinsic. Assigning result to a new var.
genExpectLit :: Integer -> LlvmType -> LlvmVar -> LlvmM (LlvmVar, StmtData)
genExpectLit expLit expTy var = do
  cfg <- getConfig

  let
    lit = LMLitVar $ LMIntLit expLit expTy

    llvmExpectName
      | isInt expTy = fsLit $ "llvm.expect." ++ showSDocOneLine (llvmCgContext cfg) (ppr expTy)
      | otherwise   = panic "genExpectedLit: Type not an int!"

  (llvmExpect, stmts, top) <-
    getInstrinct llvmExpectName expTy [expTy, expTy]
  (var', call) <- doExpr expTy $ Call StdCall llvmExpect [var, lit] []
  return (var', (stmts `snocOL` call, top))

{- Note [Literals and branch conditions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is important that whenever we generate branch conditions for
literals like '1', they are properly narrowed to an LLVM expression of
type 'i1' (for bools.) Otherwise, nobody is happy. So when we convert
a CmmExpr to an LLVM expression for a branch conditional, exprToVarOpt
must be certain to return a properly narrowed type. genLit is
responsible for this, in the case of literal integers.

Often, we won't see direct statements like:

    if(1) {
      ...
    } else {
      ...
    }

at this point in the pipeline, because the Glorious Code Generator
will do trivial branch elimination in the sinking pass (among others,)
which will eliminate the expression entirely.

However, it's certainly possible and reasonable for this to occur in
hand-written C-- code. Consider something like:

    #if !defined(SOME_CONDITIONAL)
    #define CHECK_THING(x) 1
    #else
    #define CHECK_THING(x) some_operation((x))
    #endif

    f() {

      if (CHECK_THING(xyz)) {
        ...
      } else {
        ...
      }

    }

In such an instance, CHECK_THING might result in an *expression* in
one case, and a *literal* in the other, depending on what in
particular was #define'd. So we must be sure to properly narrow the
literal in this case to i1 as it won't be eliminated beforehand.

For a real example of this, see ./rts/StgStdThunks.cmm

-}



-- | Switch branch
genSwitch :: UnreachableBlockId -> CmmExpr -> SwitchTargets -> LlvmM StmtData
genSwitch (UnreachableBlockId ubid) cond ids = do
    (vc, stmts, top) <- exprToVar cond
    let ty = getVarType vc

    let labels = [ (mkIntLit ty ix, blockIdToLlvm b)
                 | (ix, b) <- switchTargetsCases ids ]
    let defLbl | Just l <- switchTargetsDefault ids = blockIdToLlvm l
               | otherwise                          = blockIdToLlvm ubid
                 -- switch to an unreachable basic block for exhaustive
                 -- switches. See Note [Unreachable block as default destination
                 -- in Switch]

    let s1 = Switch vc defLbl labels
    return $ (stmts `snocOL` s1, top)


-- Note [Unreachable block as default destination in Switch]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- LLVM IR requires a default destination (a block label) for its Switch
-- operation, even if the switch is exhaustive. An LLVM switch is considered
-- exhausitve (e.g. to omit range checks for bit tests [1]) if the default
-- destination is unreachable.
--
-- When we codegen a Cmm function, we always reserve an unreachable basic block
-- that is used as a default destination for exhaustive Cmm switches in
-- genSwitch. See #24717
--
-- [1] https://reviews.llvm.org/D68131



-- -----------------------------------------------------------------------------
-- * CmmExpr code generation
--

-- | An expression conversion return data:
--   * LlvmVar: The var holding the result of the expression
--   * LlvmStatements: Any statements needed to evaluate the expression
--   * LlvmCmmDecl: Any global data needed for this expression
type ExprData = (LlvmVar, LlvmStatements, [LlvmCmmDecl])

-- | Values which can be passed to 'exprToVar' to configure its
-- behaviour in certain circumstances.
--
-- Currently just used for determining if a comparison should return
-- a boolean (i1) or a word. See Note [Literals and branch conditions].
newtype EOption = EOption { i1Expected :: Bool }
-- XXX: EOption is an ugly and inefficient solution to this problem.

-- | i1 type expected (condition scrutinee).
i1Option :: EOption
i1Option = EOption True

-- | Word type expected (usual).
wordOption :: EOption
wordOption = EOption False

-- | Convert a CmmExpr to a list of LlvmStatements with the result of the
-- expression being stored in the returned LlvmVar.
exprToVar :: CmmExpr -> LlvmM ExprData
exprToVar = exprToVarOpt wordOption

exprToVarOpt :: EOption -> CmmExpr -> LlvmM ExprData
exprToVarOpt opt e = case e of

    CmmLit lit
        -> genLit opt lit

    CmmLoad e' ty align
        -> genLoad Nothing e' ty align

    -- Cmmreg in expression is the value, so must load. If you want actual
    -- reg pointer, call getCmmReg directly.
    CmmReg r -> do
        (v1, ty, s1) <- getCmmRegVal r
        case isPointer ty of
             True  -> do
                 -- Cmm wants the value, so pointer types must be cast to ints
                 platform <- getPlatform
                 (v2, s2) <- doExpr (llvmWord platform) $ Cast LM_Ptrtoint v1 (llvmWord platform)
                 return (v2, s1 `snocOL` s2, [])

             False -> return (v1, s1, [])

    CmmMachOp op exprs
        -> genMachOp opt op exprs

    CmmRegOff r i
        -> exprToVar $ expandCmmReg (r, i)

    CmmStackSlot _ _
        -> panic "exprToVar: CmmStackSlot not supported!"


-- | Handle CmmMachOp expressions
genMachOp :: EOption -> MachOp -> [CmmExpr] -> LlvmM ExprData

-- Unary Machop
genMachOp _ op [x] = case op of

    MO_Not w ->
        let all1 = mkIntLit (widthToLlvmInt w) (-1)
        in negate (widthToLlvmInt w) all1 LM_MO_Xor

    MO_S_Neg w ->
        let all0 = mkIntLit (widthToLlvmInt w) 0
        in negate (widthToLlvmInt w) all0 LM_MO_Sub

    MO_F_Neg w ->
        let all0 = LMLitVar $ LMFloatLit (-0) (widthToLlvmFloat w)
        in negate (widthToLlvmFloat w) all0 LM_MO_FSub

    MO_SF_Round    _ w -> fiConv (widthToLlvmFloat w) LM_Sitofp
    MO_FS_Truncate _ w -> fiConv (widthToLlvmInt w) LM_Fptosi

    MO_SS_Conv from to
        -> sameConv from (widthToLlvmInt to) LM_Trunc LM_Sext

    MO_UU_Conv from to
        -> sameConv from (widthToLlvmInt to) LM_Trunc LM_Zext

    MO_XX_Conv from to
        -> sameConv from (widthToLlvmInt to) LM_Trunc LM_Zext

    MO_FF_Conv from to
        -> sameConv from (widthToLlvmFloat to) LM_Fptrunc LM_Fpext

    MO_WF_Bitcast w -> fiConv (widthToLlvmFloat w) LM_Bitcast
    MO_FW_Bitcast w -> fiConv (widthToLlvmInt w)   LM_Bitcast

    MO_VS_Neg len w ->
        let ty    = widthToLlvmInt w
            vecty = LMVector len ty
            all0  = LMIntLit (-0) ty
            all0s = LMLitVar $ LMVectorLit (replicate len all0)
        in negateVec vecty all0s LM_MO_Sub

    MO_VF_Neg len w ->
        let ty    = widthToLlvmFloat w
            vecty = LMVector len ty
            all0  = LMFloatLit (-0) ty
            all0s = LMLitVar $ LMVectorLit (replicate len all0)
        in negateVec vecty all0s LM_MO_FSub

    MO_V_Broadcast  l w -> genBroadcastOp l w x
    MO_VF_Broadcast l w -> genBroadcastOp l w x

    MO_RelaxedRead w -> exprToVar (CmmLoad x (cmmBits w) NaturallyAligned)

    MO_AlignmentCheck _ _ -> panic "-falignment-sanitisation is not supported by -fllvm"

    -- Handle unsupported cases explicitly so we get a warning
    -- of missing case when new MachOps added
    MO_Add _          -> panicOp
    MO_Mul _          -> panicOp
    MO_Sub _          -> panicOp
    MO_S_MulMayOflo _ -> panicOp
    MO_S_Quot _       -> panicOp
    MO_S_Rem _        -> panicOp
    MO_U_Quot _       -> panicOp
    MO_U_Rem _        -> panicOp

    MO_Eq  _          -> panicOp
    MO_Ne  _          -> panicOp
    MO_S_Ge _         -> panicOp
    MO_S_Gt _         -> panicOp
    MO_S_Le _         -> panicOp
    MO_S_Lt _         -> panicOp
    MO_U_Ge _         -> panicOp
    MO_U_Gt _         -> panicOp
    MO_U_Le _         -> panicOp
    MO_U_Lt _         -> panicOp

    MO_F_Add        _ -> panicOp
    MO_F_Sub        _ -> panicOp
    MO_F_Mul        _ -> panicOp
    MO_F_Quot       _ -> panicOp
    MO_F_Min        _ -> panicOp
    MO_F_Max        _ -> panicOp

    MO_FMA _ _ _      -> panicOp

    MO_F_Eq         _ -> panicOp
    MO_F_Ne         _ -> panicOp
    MO_F_Ge         _ -> panicOp
    MO_F_Gt         _ -> panicOp
    MO_F_Le         _ -> panicOp
    MO_F_Lt         _ -> panicOp

    MO_And          _ -> panicOp
    MO_Or           _ -> panicOp
    MO_Xor          _ -> panicOp
    MO_Shl          _ -> panicOp
    MO_U_Shr        _ -> panicOp
    MO_S_Shr        _ -> panicOp

    MO_V_Insert   _ _ -> panicOp
    MO_V_Extract  _ _ -> panicOp

    MO_V_Add      _ _ -> panicOp
    MO_V_Sub      _ _ -> panicOp
    MO_V_Mul      _ _ -> panicOp

    MO_VS_Quot    _ _ -> panicOp
    MO_VS_Rem     _ _ -> panicOp
    MO_VS_Min     _ _ -> panicOp
    MO_VS_Max     _ _ -> panicOp

    MO_VU_Quot    _ _ -> panicOp
    MO_VU_Rem     _ _ -> panicOp
    MO_VU_Min     _ _ -> panicOp
    MO_VU_Max     _ _ -> panicOp

    MO_VF_Insert  _ _ -> panicOp
    MO_VF_Extract _ _ -> panicOp

    MO_V_Shuffle {} -> panicOp
    MO_VF_Shuffle {} -> panicOp

    MO_VF_Add     _ _ -> panicOp
    MO_VF_Sub     _ _ -> panicOp
    MO_VF_Mul     _ _ -> panicOp
    MO_VF_Quot    _ _ -> panicOp
    MO_VF_Min     _ _ -> panicOp
    MO_VF_Max     _ _ -> panicOp

    where
        negate ty v2 negOp = do
            (vx, stmts, top) <- exprToVar x
            (v1, s1) <- doExpr ty $ LlvmOp negOp v2 vx
            return (v1, stmts `snocOL` s1, top)

        negateVec ty v2 negOp = do
            (vx, stmts1, top) <- exprToVar x
            (vxs', stmts2) <- castVars Signed [(vx, ty)]
            let vx' = singletonPanic "genMachOp: negateVec" vxs'
            (v1, s1) <- doExpr ty $ LlvmOp negOp v2 vx'
            return (v1, stmts1 `appOL` stmts2 `snocOL` s1, top)

        fiConv ty convOp = do
            (vx, stmts, top) <- exprToVar x
            (v1, s1) <- doExpr ty $ Cast convOp vx ty
            return (v1, stmts `snocOL` s1, top)

        sameConv from ty reduce expand = do
            x'@(vx, stmts, top) <- exprToVar x
            let sameConv' op = do
                    (v1, s1) <- doExpr ty $ Cast op vx ty
                    return (v1, stmts `snocOL` s1, top)
            platform <- getPlatform
            let toWidth = llvmWidthInBits platform ty
            -- LLVM doesn't like trying to convert to same width, so
            -- need to check for that as we do get Cmm code doing it.
            case widthInBits from  of
                 w | w < toWidth -> sameConv' expand
                 w | w > toWidth -> sameConv' reduce
                 _w              -> return x'

        panicOp = panic $ "LLVM.CodeGen.genMachOp: non unary op encountered"
                       ++ "with one argument! (" ++ show op ++ ")"

-- Handle GlobalRegs pointers
genMachOp opt o@(MO_Add _) e@[(CmmReg (CmmGlobal r)), (CmmLit (CmmInt n _))]
    = genMachOp_fast opt o r (fromInteger n) e

genMachOp opt o@(MO_Sub _) e@[(CmmReg (CmmGlobal r)), (CmmLit (CmmInt n _))]
    = genMachOp_fast opt o r (negate . fromInteger $ n) e

-- Generic case
genMachOp opt op e = genMachOp_slow opt op e


-- | Handle CmmMachOp expressions
-- This is a specialised method that handles Global register manipulations like
-- 'Sp - 16', using the getelementptr instruction.
genMachOp_fast :: EOption -> MachOp -> GlobalRegUse -> Int -> [CmmExpr]
               -> LlvmM ExprData
genMachOp_fast opt op r n e
  = do (gv, grt, s1) <- getCmmRegVal (CmmGlobal r)
       platform <- getPlatform
       let (ix,rem) = n `divMod` ((llvmWidthInBits platform . pLower) grt  `div` 8)
       case isPointer grt && rem == 0 of
            True -> do
                (ptr, s2) <- doExpr grt $ GetElemPtr True gv [toI32 ix]
                (var, s3) <- doExpr (llvmWord platform) $ Cast LM_Ptrtoint ptr (llvmWord platform)
                return (var, s1 `snocOL` s2 `snocOL` s3, [])

            False -> genMachOp_slow opt op e


-- | Handle CmmMachOp expressions
-- This handles all the cases not handle by the specialised genMachOp_fast.
genMachOp_slow :: EOption -> MachOp -> [CmmExpr] -> LlvmM ExprData

-- Element extraction
genMachOp_slow _ (MO_V_Extract l w) [val, idx] = runExprData $ do
    vval <- exprToVarW val
    vidx <- exprToVarW idx
    vval' <- singletonPanic "genMachOp_slow" <$>
             castVarsW Signed [(vval, LMVector l ty)]
    doExprW ty $ Extract vval' vidx
  where
    ty = widthToLlvmInt w

genMachOp_slow _ (MO_VF_Extract l w) [val, idx] = runExprData $ do
    vval <- exprToVarW val
    vidx <- exprToVarW idx
    vval' <- singletonPanic "genMachOp_slow" <$>
             castVarsW Signed [(vval, LMVector l ty)]
    doExprW ty $ Extract vval' vidx
  where
    ty = widthToLlvmFloat w

-- Element insertion
genMachOp_slow _ (MO_V_Insert l w) [val, elt, idx] = runExprData $ do
    vval <- exprToVarW val
    velt <- exprToVarW elt
    vidx <- exprToVarW idx
    vval' <- singletonPanic "genMachOp_slow" <$>
             castVarsW Signed [(vval, ty)]
    doExprW ty $ Insert vval' velt vidx
  where
    ty = LMVector l (widthToLlvmInt w)

genMachOp_slow _ (MO_VF_Insert l w) [val, elt, idx] = runExprData $ do
    vval <- exprToVarW val
    velt <- exprToVarW elt
    vidx <- exprToVarW idx
    vval' <- singletonPanic "genMachOp_slow" <$>
             castVarsW Signed [(vval, ty)]
    doExprW ty $ Insert vval' velt vidx
  where
    ty = LMVector l (widthToLlvmFloat w)

-- Binary MachOp
genMachOp_slow opt op [x, y] = case op of

    MO_Eq _   -> genBinComp opt LM_CMP_Eq
    MO_Ne _   -> genBinComp opt LM_CMP_Ne

    MO_S_Gt _ -> genBinComp opt LM_CMP_Sgt
    MO_S_Ge _ -> genBinComp opt LM_CMP_Sge
    MO_S_Lt _ -> genBinComp opt LM_CMP_Slt
    MO_S_Le _ -> genBinComp opt LM_CMP_Sle

    MO_U_Gt _ -> genBinComp opt LM_CMP_Ugt
    MO_U_Ge _ -> genBinComp opt LM_CMP_Uge
    MO_U_Lt _ -> genBinComp opt LM_CMP_Ult
    MO_U_Le _ -> genBinComp opt LM_CMP_Ule

    MO_Add _ -> genBinMach LM_MO_Add
    MO_Sub _ -> genBinMach LM_MO_Sub
    MO_Mul _ -> genBinMach LM_MO_Mul

    MO_S_MulMayOflo w -> isSMulOK w x y

    MO_S_Quot _ -> genBinMach LM_MO_SDiv
    MO_S_Rem  _ -> genBinMach LM_MO_SRem

    MO_U_Quot _ -> genBinMach LM_MO_UDiv
    MO_U_Rem  _ -> genBinMach LM_MO_URem

    MO_F_Eq _ -> genBinComp opt LM_CMP_Feq
    MO_F_Ne _ -> genBinComp opt LM_CMP_Fne
    MO_F_Gt _ -> genBinComp opt LM_CMP_Fgt
    MO_F_Ge _ -> genBinComp opt LM_CMP_Fge
    MO_F_Lt _ -> genBinComp opt LM_CMP_Flt
    MO_F_Le _ -> genBinComp opt LM_CMP_Fle

    MO_F_Add  _ -> genBinMach LM_MO_FAdd
    MO_F_Sub  _ -> genBinMach LM_MO_FSub
    MO_F_Mul  _ -> genBinMach LM_MO_FMul
    MO_F_Quot _ -> genBinMach LM_MO_FDiv

    MO_FMA _ _ _ -> panicOp

    MO_And _   -> genBinMach LM_MO_And
    MO_Or  _   -> genBinMach LM_MO_Or
    MO_Xor _   -> genBinMach LM_MO_Xor
    MO_Shl _   -> genBinCastYMach LM_MO_Shl
    MO_U_Shr _ -> genBinCastYMach LM_MO_LShr
    MO_S_Shr _ -> genBinCastYMach LM_MO_AShr

    MO_V_Add l w   -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_Add
    MO_V_Sub l w   -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_Sub
    MO_V_Mul l w   -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_Mul

    MO_VS_Quot l w -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_SDiv
    MO_VS_Rem  l w -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_SRem

    MO_VU_Quot l w -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_UDiv
    MO_VU_Rem  l w -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_URem

    MO_VF_Add  l w -> genCastBinMach (LMVector l (widthToLlvmFloat w)) LM_MO_FAdd
    MO_VF_Sub  l w -> genCastBinMach (LMVector l (widthToLlvmFloat w)) LM_MO_FSub
    MO_VF_Mul  l w -> genCastBinMach (LMVector l (widthToLlvmFloat w)) LM_MO_FMul
    MO_VF_Quot l w -> genCastBinMach (LMVector l (widthToLlvmFloat w)) LM_MO_FDiv

    MO_Not _       -> panicOp
    MO_S_Neg _     -> panicOp
    MO_F_Neg _     -> panicOp

    MO_SF_Round    _ _ -> panicOp
    MO_FS_Truncate _ _ -> panicOp
    MO_SS_Conv _ _ -> panicOp
    MO_UU_Conv _ _ -> panicOp
    MO_XX_Conv _ _ -> panicOp
    MO_FF_Conv _ _ -> panicOp

    MO_WF_Bitcast _to ->  panicOp
    MO_FW_Bitcast _to ->  panicOp

    MO_VS_Neg {} -> panicOp

    MO_VF_Broadcast {} -> panicOp
    MO_V_Broadcast {} -> panicOp
    MO_V_Insert  {} -> panicOp
    MO_VF_Insert  {} -> panicOp

    MO_V_Shuffle _ _ is -> genShuffleOp is x y
    MO_VF_Shuffle _ _ is -> genShuffleOp is x y

    MO_VF_Neg {} -> panicOp

    -- Min/max
    MO_F_Min  {} -> genMinMaxOp "minnum" x y
    MO_F_Max  {} -> genMinMaxOp "maxnum" x y
    MO_VF_Min {} -> genMinMaxOp "minnum" x y
    MO_VF_Max {} -> genMinMaxOp "maxnum" x y
    MO_VU_Min {} -> genMinMaxOp "umin"   x y
    MO_VU_Max {} -> genMinMaxOp "umax"   x y
    MO_VS_Min {} -> genMinMaxOp "smin"   x y
    MO_VS_Max {} -> genMinMaxOp "smax"   x y

    MO_RelaxedRead {} -> panicOp

    MO_AlignmentCheck {} -> panicOp

    where
        binLlvmOp ty binOp allow_y_cast = do
          platform <- getPlatform
          runExprData $ do
            vx <- exprToVarW x
            vy <- exprToVarW y

            if | getVarType vx == getVarType vy
               -> doExprW (ty vx) $ binOp vx vy

               | allow_y_cast
               -> do
                    vy' <- singletonPanic "binLlvmOp cast"<$>
                            castVarsW Signed [(vy, (ty vx))]
                    doExprW (ty vx) $ binOp vx vy'

               | otherwise
               -> pprPanic "binLlvmOp types" (pdoc platform x $$ pdoc platform y)

        binCastLlvmOp ty binOp = runExprData $ do
            vx <- exprToVarW x
            vy <- exprToVarW y
            vxy' <- castVarsW Signed [(vx, ty), (vy, ty)]
            case vxy' of
              [vx',vy'] -> doExprW ty $ binOp vx' vy'
              _         -> panic "genMachOp_slow: binCastLlvmOp"

        -- Need to use EOption here as Cmm expects word size results from
        -- comparisons while LLVM return i1. Need to extend to llvmWord type
        -- if expected. See Note [Literals and branch conditions].
        genBinComp opt cmp = do
            ed@(v1, stmts, top) <- binLlvmOp (const i1) (Compare cmp) False
            platform <- getPlatform
            if getVarType v1 == i1
                then case i1Expected opt of
                    True  -> return ed
                    False -> do
                        let w_ = llvmWord platform
                        (v2, s1) <- doExpr w_ $ Cast LM_Zext v1 w_
                        return (v2, stmts `snocOL` s1, top)
                else
                    pprPanic "genBinComp: Compare returned type other then i1! "
                               (ppr $ getVarType v1)

        genBinMach op = binLlvmOp getVarType (LlvmOp op) False

        genBinCastYMach op = binLlvmOp getVarType (LlvmOp op) True

        genCastBinMach ty op = binCastLlvmOp ty (LlvmOp op)

        genMinMaxOp intrin x y = runExprData $ do
            vx <- exprToVarW x
            vy <- exprToVarW y
            let tx = getVarType vx
                ty = getVarType vy
                fname = "llvm." ++ intrin ++ "." ++ ppLlvmTypeShort ty
            Panic.massertPpr
              (tx == ty)
              (vcat [ text (fname ++ ": mismatched arg types")
                    , ppLlvmType tx, ppLlvmType ty ])
            fptr <- liftExprData $ getInstrinct (fsLit fname) ty [tx, ty]
            doExprW tx $ Call StdCall fptr [vx, vy] [ReadNone, NoUnwind]

        -- Detect if overflow will occur in signed multiply of the two
        -- CmmExpr's. This is the LLVM assembly equivalent of the NCG
        -- implementation. Its much longer due to type information/safety.
        -- This should actually compile to only about 3 asm instructions.
        isSMulOK :: Width -> CmmExpr -> CmmExpr -> LlvmM ExprData
        isSMulOK _ x y = do
          platform <- getPlatform
          runExprData $ do
            vx <- exprToVarW x
            vy <- exprToVarW y

            let word  = getVarType vx
            let word2 = LMInt $ 2 * llvmWidthInBits platform (getVarType vx)
            let shift = llvmWidthInBits platform word
            let shift1 = toIWord platform (shift - 1)
            let shift2 = toIWord platform shift

            if isInt word
                then do
                    x1     <- doExprW word2 $ Cast LM_Sext vx word2
                    y1     <- doExprW word2 $ Cast LM_Sext vy word2
                    r1     <- doExprW word2 $ LlvmOp LM_MO_Mul x1 y1
                    rlow1  <- doExprW word $ Cast LM_Trunc r1 word
                    rlow2  <- doExprW word $ LlvmOp LM_MO_AShr rlow1 shift1
                    rhigh1 <- doExprW word2 $ LlvmOp LM_MO_AShr r1 shift2
                    rhigh2 <- doExprW word $ Cast LM_Trunc rhigh1 word
                    doExprW word $ LlvmOp LM_MO_Sub rlow2 rhigh2

                else
                    pprPanic "isSMulOK: Not bit type! " $
                        lparen <> ppr word <> rparen

        panicOp = panic $ "LLVM.CodeGen.genMachOp_slow: non-binary op encountered "
                       ++ "with two arguments! (" ++ show op ++ ")"

genMachOp_slow _opt op [x, y, z] = do
  let
    panicOp = panic $ "LLVM.CodeGen.genMachOp_slow: non-ternary op encountered "
                   ++ "with three arguments! (" ++ show op ++ ")"
  case op of
    MO_FMA var lg width ->
      case var of
        -- LLVM only has the fmadd variant.
        FMAdd   -> genFmaOp x y z
        -- Other fused multiply-add operations are implemented in terms of fmadd
        -- This is sound: it does not lose any precision.
        FMSub   -> genFmaOp x y (neg z)
        FNMAdd  -> genFmaOp (neg x) y z
        FNMSub  -> genFmaOp (neg x) y (neg z)
      where
        neg x
          | lg == 1
          = CmmMachOp (MO_F_Neg width) [x]
          | otherwise
          = CmmMachOp (MO_VF_Neg lg width) [x]
    _ -> panicOp

-- More than three expressions, invalid!
genMachOp_slow _ _ _ = panic "genMachOp_slow: More than 3 expressions in MachOp!"

genBroadcastOp :: Int -> Width -> CmmExpr -> LlvmM ExprData
genBroadcastOp lg _width x = runExprData $ do
  -- To broadcast a scalar x as a vector v:
  --   1. insert x at the 0 position of the zero vector
  --   2. shuffle x into all positions
  var_x <- exprToVarW x
  let tx = getVarType var_x
      tv = LMVector lg tx
      z = if isFloat tx
          then LMFloatLit 0 tx
          else LMIntLit   0 tx
      zs = LMLitVar $ LMVectorLit $ replicate lg z
  w <- doExprW tv $ Insert zs var_x (LMLitVar $ LMIntLit 0 (LMInt 32))
  doExprW tv $ Shuffle w w (replicate lg 0)

genShuffleOp :: [Int] -> CmmExpr -> CmmExpr -> LlvmM ExprData
genShuffleOp is x y = runExprData $ do
  vx <- exprToVarW x
  vy <- exprToVarW y
  let tx = getVarType vx
      ty = getVarType vy
  Panic.massertPpr
    (tx == ty)
    (vcat [ text "shuffle: mismatched arg types"
          , ppLlvmType tx, ppLlvmType ty ])
  doExprW tx $ Shuffle vx vy is

-- | Generate code for a fused multiply-add operation.
genFmaOp :: CmmExpr -> CmmExpr -> CmmExpr -> LlvmM ExprData
genFmaOp x y z = runExprData $ do
  vx <- exprToVarW x
  vy <- exprToVarW y
  vz <- exprToVarW z
  let tx = getVarType vx
      ty = getVarType vy
      tz = getVarType vz
  Panic.massertPpr
    (tx == ty && tx == tz)
    (vcat [ text "fma: mismatched arg types"
          , ppLlvmType tx, ppLlvmType ty, ppLlvmType tz ])
  let fname = case tx of
        LMFloat  -> fsLit "llvm.fma.f32"
        LMDouble -> fsLit "llvm.fma.f64"
        LMVector 4 LMFloat -> fsLit "llvm.fma.v4f32"
        LMVector 8 LMFloat -> fsLit "llvm.fma.v8f32"
        LMVector 16 LMFloat -> fsLit "llvm.fma.v16f32"
        LMVector 2 LMDouble -> fsLit "llvm.fma.v2f64"
        LMVector 4 LMDouble -> fsLit "llvm.fma.v4f64"
        LMVector 8 LMDouble -> fsLit "llvm.fma.v8f64"
        _ -> pprPanic "CmmToLlvm.genFmaOp: unsupported type" (ppLlvmType tx)
  fptr <- liftExprData $ getInstrinct fname ty [tx, ty, tz]
  doExprW tx $ Call StdCall fptr [vx, vy, vz] [ReadNone, NoUnwind]

-- | Handle CmmLoad expression.
genLoad :: Atomic -> CmmExpr -> CmmType -> AlignmentSpec -> LlvmM ExprData

-- First we try to detect a few common cases and produce better code for
-- these then the default case. We are mostly trying to detect Cmm code
-- like I32[Sp + n] and use 'getelementptr' operations instead of the
-- generic case that uses casts and pointer arithmetic
genLoad atomic e@(CmmReg (CmmGlobal r)) ty align
    = genLoad_fast atomic e r 0 ty align

genLoad atomic e@(CmmRegOff (CmmGlobal r) n) ty align
    = genLoad_fast atomic e r n ty align

genLoad atomic e@(CmmMachOp (MO_Add _) [
                            (CmmReg (CmmGlobal r)),
                            (CmmLit (CmmInt n _))])
                ty align
    = genLoad_fast atomic e r (fromInteger n) ty align

genLoad atomic e@(CmmMachOp (MO_Sub _) [
                            (CmmReg (CmmGlobal r)),
                            (CmmLit (CmmInt n _))])
                ty align
    = genLoad_fast atomic e r (negate $ fromInteger n) ty align

-- generic case
genLoad atomic e ty align
    = getTBAAMeta topN >>= genLoad_slow atomic e ty align

-- | Handle CmmLoad expression.
-- This is a special case for loading from a global register pointer
-- offset such as I32[Sp+8].
genLoad_fast :: Atomic -> CmmExpr -> GlobalRegUse -> Int -> CmmType
             -> AlignmentSpec -> LlvmM ExprData
genLoad_fast atomic e r n ty align = do
    platform <- getPlatform
    (gv, grt, s1) <- getCmmRegVal (CmmGlobal r)
    meta          <- getTBAARegMeta (globalRegUse_reg r)
    let ty'      = cmmToLlvmType ty
        (ix,rem) = n `divMod` ((llvmWidthInBits platform . pLower) grt  `div` 8)
    case isPointer grt && rem == 0 of
            True  -> do
                (ptr, s2) <- doExpr grt $ GetElemPtr True gv [toI32 ix]
                -- We might need a different pointer type, so check
                case grt == ty' of
                     -- were fine
                     True -> do
                         (var, s3) <- doExpr ty' (MExpr meta $ mkLoad atomic ptr align)
                         return (var, s1 `snocOL` s2 `snocOL` s3,
                                     [])

                     -- cast to pointer type needed
                     False -> do
                         let pty = pLift ty'
                         (ptr', s3) <- doExpr pty $ Cast LM_Bitcast ptr pty
                         (var, s4) <- doExpr ty' (MExpr meta $ mkLoad atomic ptr' align)
                         return (var, s1 `snocOL` s2 `snocOL` s3
                                    `snocOL` s4, [])

            -- If its a bit type then we use the slow method since
            -- we can't avoid casting anyway.
            False -> genLoad_slow atomic  e ty align meta

-- | Handle Cmm load expression.
-- Generic case. Uses casts and pointer arithmetic if needed.
genLoad_slow :: Atomic -> CmmExpr -> CmmType -> AlignmentSpec -> [MetaAnnot]
             -> LlvmM ExprData
genLoad_slow atomic e ty align meta = do
  platform <- getPlatform
  cfg      <- getConfig
  runExprData $ do
    iptr <- exprToVarW e
    case getVarType iptr of
        LMPointer _ ->
                    doExprW (cmmToLlvmType ty) (MExpr meta $ mkLoad atomic iptr align)

        i@(LMInt _) | i == llvmWord platform -> do
                    let pty = LMPointer $ cmmToLlvmType ty
                    ptr <- doExprW pty $ Cast LM_Inttoptr iptr pty
                    doExprW (cmmToLlvmType ty) (MExpr meta $ mkLoad atomic ptr align)

        other -> pprPanic "exprToVar: CmmLoad expression is not right type!"
                     (pdoc platform e $$
                      text "Size of Ptr:" <+> ppr (llvmPtrBits platform) $$
                      text "Size of var:" <+> ppr (llvmWidthInBits platform other) $$
                      text "Var:" <+> (ppVar cfg iptr))

{-
Note [Alignment of vector-typed values]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On x86, vector types need to be 16-byte aligned for aligned
access, but we have no way of guaranteeing that this is true with GHC
(we would need to modify the layout of the stack and closures, change
the storage manager, etc.). So, we blindly tell LLVM that *any* vector
store or load could be unaligned. In the future we may be able to
guarantee that certain vector access patterns are aligned, in which
case we will need a more granular way of specifying alignment.
-}

mkLoad :: Atomic -> LlvmVar -> AlignmentSpec -> LlvmExpression
mkLoad atomic vptr alignment
  | Just mem_ord <- atomic
                = ALoad (convertMemoryOrdering mem_ord) False vptr
  | otherwise   = Load vptr align
  where
    ty = pLower (getVarType vptr)
    align = case alignment of
              -- See Note [Alignment of vector-typed values]
              _ | isVector ty  -> Just 1
              Unaligned        -> Just 1
              NaturallyAligned -> Nothing

-- | Handle CmmReg expression. This will return a pointer to the stack
-- location of the register. Throws an error if it isn't allocated on
-- the stack.
getCmmReg :: CmmReg -> LlvmM (LlvmVar, LlvmType)
getCmmReg (CmmLocal (LocalReg un _))
  = do exists <- varLookup un
       case exists of
         Just ety -> return (LMLocalVar un $ pLift ety, ety)
         Nothing  -> pprPanic "getCmmReg: Cmm register " $
                        ppr un <> text " was not allocated!"
           -- This should never happen, as every local variable should
           -- have been assigned a value at some point, triggering
           -- "funPrologue" to allocate it on the stack.

getCmmReg (CmmGlobal (GlobalRegUse reg _reg_ty))
  = do onStack  <- checkStackReg reg
       platform <- getPlatform
       case onStack of
          Just stack_ty -> do
            let var = lmGlobalRegVar platform (GlobalRegUse reg stack_ty)
            return (var, pLower $ getVarType var)
          Nothing ->
            pprPanic "getCmmReg: Cmm register " $
              ppr reg <> text " not stack-allocated!"

-- | Return the value of a given register, as well as its type. Might
-- need to be load from stack.
getCmmRegVal :: CmmReg -> LlvmM (LlvmVar, LlvmType, LlvmStatements)
getCmmRegVal reg =
  case reg of
    CmmGlobal gu@(GlobalRegUse g _) -> do
      onStack <- checkStackReg g
      platform <- getPlatform
      case onStack of
        Just {} ->
          loadFromStack
        Nothing -> do
          let r = lmGlobalRegArg platform gu
          return (r, getVarType r, nilOL)
    _ -> loadFromStack
 where
    loadFromStack = do
      platform <- getPlatform
      (ptr, stack_reg_ty) <- getCmmReg reg
      let reg_ty = case reg of
            CmmGlobal g -> pLower $ getVarType $ lmGlobalRegVar platform g
            CmmLocal {} -> stack_reg_ty
      if reg_ty /= stack_reg_ty
      then do
        (v1, s1) <- doExpr stack_reg_ty (Load ptr Nothing)
        (v2, s2) <- doExpr reg_ty (Cast LM_Bitcast v1 reg_ty)
        return (v2, reg_ty, toOL [s1, s2])
      else do
        (v, s) <- doExpr reg_ty (Load ptr Nothing)
        return (v, reg_ty, unitOL s)

-- | Allocate a local CmmReg on the stack
allocReg :: CmmReg -> (LlvmVar, LlvmStatements)
allocReg (CmmLocal (LocalReg un ty))
  = let ty' = cmmToLlvmType ty
        var = LMLocalVar un (LMPointer ty')
        alc = Alloca ty' 1
    in (var, unitOL $ Assignment var alc)

allocReg _ = panic $ "allocReg: Global reg encountered! Global registers should"
                    ++ " have been handled elsewhere!"


-- | Generate code for a literal
genLit :: EOption -> CmmLit -> LlvmM ExprData
genLit opt (CmmInt i w)
  -- See Note [Literals and branch conditions].
  = let width | i1Expected opt = i1
              | otherwise      = LMInt (widthInBits w)
        -- comm  = Comment [ fsLit $ "EOption: " ++ show opt
        --                 , fsLit $ "Width  : " ++ show w
        --                 , fsLit $ "Width' : " ++ show (widthInBits w)
        --                 ]
    in return (mkIntLit width i, nilOL, [])

genLit _ (CmmFloat r W32)
  = return (LMLitVar $ LMFloatLit (widenFp (fromRational r :: Float)) (widthToLlvmFloat W32),
              nilOL, [])

genLit _ (CmmFloat r W64)
  = return (LMLitVar $ LMFloatLit (fromRational r :: Double) (widthToLlvmFloat W64),
              nilOL, [])

genLit _ (CmmFloat _r _w)
  = panic "genLit (CmmLit:CmmFloat), unsupported float lit"

genLit opt (CmmVec ls)
  = do llvmLits <- mapM toLlvmLit ls
       return (LMLitVar $ LMVectorLit llvmLits, nilOL, [])
  where
    toLlvmLit :: CmmLit -> LlvmM LlvmLit
    toLlvmLit lit = do
        (llvmLitVar, _, _) <- genLit opt lit
        case llvmLitVar of
          LMLitVar llvmLit -> return llvmLit
          _ -> panic "genLit"

genLit _ cmm@(CmmLabel l)
  = do var <- getGlobalPtr =<< strCLabel_llvm l
       platform <- getPlatform
       let lmty = cmmToLlvmType $ cmmLitType platform cmm
       (v1, s1) <- doExpr lmty $ Cast LM_Ptrtoint var (llvmWord platform)
       return (v1, unitOL s1, [])

genLit opt (CmmLabelOff label off) = do
    platform <- getPlatform
    (vlbl, stmts, stat) <- genLit opt (CmmLabel label)
    let voff = toIWord platform off
    (v1, s1) <- doExpr (getVarType vlbl) $ LlvmOp LM_MO_Add vlbl voff
    return (v1, stmts `snocOL` s1, stat)

genLit opt (CmmLabelDiffOff l1 l2 off w) = do
    platform <- getPlatform
    (vl1, stmts1, stat1) <- genLit opt (CmmLabel l1)
    (vl2, stmts2, stat2) <- genLit opt (CmmLabel l2)
    let voff = toIWord platform off
    let ty1 = getVarType vl1
    let ty2 = getVarType vl2
    if (isInt ty1) && (isInt ty2)
       && (llvmWidthInBits platform ty1 == llvmWidthInBits platform ty2)
       then do
            (v1, s1) <- doExpr (getVarType vl1) $ LlvmOp LM_MO_Sub vl1 vl2
            (v2, s2) <- doExpr (getVarType v1 ) $ LlvmOp LM_MO_Add v1 voff
            let ty = widthToLlvmInt w
            let stmts = stmts1 `appOL` stmts2 `snocOL` s1 `snocOL` s2
            if w /= wordWidth platform
              then do
                (v3, s3) <- doExpr ty $ Cast LM_Trunc v2 ty
                return (v3, stmts `snocOL` s3, stat1 ++ stat2)
              else
                return (v2, stmts, stat1 ++ stat2)
        else
            panic "genLit: CmmLabelDiffOff encountered with different label ty!"

genLit opt (CmmBlock b)
  = genLit opt (CmmLabel $ infoTblLbl b)

genLit _ CmmHighStackMark
  = panic "genStaticLit - CmmHighStackMark unsupported!"


-- -----------------------------------------------------------------------------
-- * Misc
--

convertMemoryOrdering :: MemoryOrdering -> LlvmSyncOrdering
convertMemoryOrdering MemOrderRelaxed = SyncMonotonic
convertMemoryOrdering MemOrderAcquire = SyncAcquire
convertMemoryOrdering MemOrderRelease = SyncRelease
convertMemoryOrdering MemOrderSeqCst  = SyncSeqCst

-- | Find CmmRegs that get assigned and allocate them on the stack
--
-- Any register that gets written needs to be allocated on the
-- stack. This avoids having to map a CmmReg to an equivalent SSA form
-- and avoids having to deal with Phi node insertion.  This is also
-- the approach recommended by LLVM developers.
--
-- On the other hand, this is unnecessarily verbose if the register in
-- question is never written. Therefore we skip it where we can to
-- save a few lines in the output and hopefully speed compilation up a
-- bit.
funPrologue :: LiveGlobalRegUses -> NonEmpty CmmBlock -> LlvmM StmtData
funPrologue live cmmBlocks = do
  platform <- getPlatform

  let getAssignedRegs :: CmmNode O O -> [CmmReg]
      getAssignedRegs (CmmAssign reg _)  = [reg]
      getAssignedRegs (CmmUnsafeForeignCall _ rs _) = map CmmLocal rs
      getAssignedRegs _                  = []
      getRegsBlock (_, body, _)          = concatMap getAssignedRegs $ blockToList body
      assignedRegs = nub $ concatMap (getRegsBlock . blockSplit) cmmBlocks
      mbLive r     =
        lookupRegUse r (alwaysLive platform) <|> lookupRegUse r live

  platform <- getPlatform
  stmtss <- forM assignedRegs $ \reg ->
    case reg of
      CmmLocal (LocalReg un _) -> do
        let (newv, stmts) = allocReg reg
        varInsert un (pLower $ getVarType newv)
        return stmts
      CmmGlobal ru@(GlobalRegUse r ty0) -> do
        let reg   = lmGlobalRegVar platform ru
            ty    = (pLower . getVarType) reg
            trash = LMLitVar $ LMUndefLit ty
            rval  = case mbLive r of
              Just (GlobalRegUse _ ty') ->
                lmGlobalRegArg platform (GlobalRegUse r ty')
              _ -> trash
            alloc = Assignment reg $ Alloca (pLower $ getVarType reg) 1
        markStackReg ru
        case mbLive r of
          Just (GlobalRegUse _ ty')
            | let llvm_ty  = cmmToLlvmType ty0
                  llvm_ty' = cmmToLlvmType ty'
            , llvm_ty /= llvm_ty'
            -> do castV <- mkLocalVar (pLift llvm_ty')
                  return $
                    toOL [ alloc
                         , Assignment castV $ Cast LM_Bitcast reg (pLift llvm_ty')
                         , Store rval castV Nothing []
                         ]
          _ ->
            return $ toOL [alloc, Store rval reg Nothing []]

  return (concatOL stmtss `snocOL` jumpToEntry, [])
  where
    entryBlk :| _ = cmmBlocks
    jumpToEntry = Branch $ blockIdToLlvm (entryLabel entryBlk)

-- | Function epilogue. Load STG variables to use as argument for call.
-- STG Liveness optimisation done here.
funEpilogue :: LiveGlobalRegUses -> LlvmM ([LlvmVar], LlvmStatements)
funEpilogue live = do
    platform <- getPlatform

    let paddingRegs = padLiveArgs platform live

    -- Set to value or "undef" depending on whether the register is
    -- actually live
    let loadExpr r = do
          (v, _, s) <- getCmmRegVal (CmmGlobal r)
          return (Just $ v, s)
        loadUndef r = do
          let ty = (pLower . getVarType $ lmGlobalRegVar platform r)
          return (Just $ LMLitVar $ LMUndefLit ty, nilOL)

    -- Note that floating-point registers in `activeStgRegs` must be sorted
    -- according to the calling convention.
    --  E.g. for X86:
    --     GOOD: F1,D1,XMM1,F2,D2,XMM2,...
    --     BAD : F1,F2,F3,D1,D2,D3,XMM1,XMM2,XMM3,...
    --  As Fn, Dn and XMMn use the same register (XMMn) to be passed, we don't
    --  want to pass F2 before D1 for example, otherwise we could get F2 -> XMM1
    --  and D1 -> XMM2.
    let allRegs = activeStgRegs platform
    loads <- forM allRegs $ \r -> if
      -- load live registers
      | Just ru <- lookupRegUse r (alwaysLive platform)
      -> loadExpr ru
      | Just ru <- lookupRegUse r live
      -> loadExpr ru
      -- load all non Floating-Point Registers
      | not (isFPR r)
      -> loadUndef (GlobalRegUse r (globalRegSpillType platform r))
      -- load padding Floating-Point Registers
      | Just ru <- lookupRegUse r paddingRegs
      -> loadUndef ru
      | otherwise            -> return (Nothing, nilOL)

    let (vars, stmts) = unzip loads
    return (catMaybes vars, concatOL stmts)

-- | Get a function pointer to the CLabel specified.
--
-- This is for Haskell functions, function type is assumed, so doesn't work
-- with foreign functions.
getHsFunc :: LiveGlobalRegUses -> CLabel -> LlvmM ExprData
getHsFunc live lbl
  = do fty <- llvmFunTy live
       name <- strCLabel_llvm lbl
       getHsFunc' name fty

getHsFunc' :: LMString -> LlvmType -> LlvmM ExprData
getHsFunc' name fty
  = do fun <- getGlobalPtr name
       if getVarType fun == fty
         then return (fun, nilOL, [])
         else do (v1, s1) <- doExpr (pLift fty)
                               $ Cast LM_Bitcast fun (pLift fty)
                 return  (v1, unitOL s1, [])

-- | Create a new local var
mkLocalVar :: LlvmType -> LlvmM LlvmVar
mkLocalVar ty = do
    un <- getUniqueM
    return $ LMLocalVar un ty


-- | Execute an expression, assigning result to a var
doExpr :: LlvmType -> LlvmExpression -> LlvmM (LlvmVar, LlvmStatement)
doExpr ty expr = do
    v <- mkLocalVar ty
    return (v, Assignment v expr)


-- | Expand CmmRegOff
expandCmmReg :: (CmmReg, Int) -> CmmExpr
expandCmmReg (reg, off)
  = let width = typeWidth (cmmRegType reg)
        voff  = CmmLit $ CmmInt (fromIntegral off) width
    in CmmMachOp (MO_Add width) [CmmReg reg, voff]


-- | Convert a block id into a appropriate Llvm label
blockIdToLlvm :: BlockId -> LlvmVar
blockIdToLlvm bid = LMLocalVar (getUnique bid) LMLabel

-- | Create Llvm int Literal
mkIntLit :: Integral a => LlvmType -> a -> LlvmVar
mkIntLit ty i = LMLitVar $ LMIntLit (toInteger i) ty

-- | Convert int type to a LLvmVar of word or i32 size
toI32 :: Integral a => a -> LlvmVar
toI32 = mkIntLit i32

toIWord :: Integral a => Platform -> a -> LlvmVar
toIWord platform = mkIntLit (llvmWord platform)


-- | Error functions
panic :: HasCallStack => String -> a
panic s = Panic.panic $ "GHC.CmmToLlvm.CodeGen." ++ s

pprPanic :: HasCallStack => String -> SDoc -> a
pprPanic s d = Panic.pprPanic ("GHC.CmmToLlvm.CodeGen." ++ s) d


-- | Returns TBAA meta data by unique
getTBAAMeta :: Unique -> LlvmM [MetaAnnot]
getTBAAMeta u =
    List.singleton . MetaAnnot tbaa . MetaNode . expectJust "getTBAAMeta" <$> getUniqMeta u

-- | Returns TBAA meta data for given register
getTBAARegMeta :: GlobalReg -> LlvmM [MetaAnnot]
getTBAARegMeta = getTBAAMeta . getTBAA


-- | A more convenient way of accumulating LLVM statements and declarations.
data LlvmAccum = LlvmAccum LlvmStatements [LlvmCmmDecl]

instance Semigroup LlvmAccum where
  LlvmAccum stmtsA declsA <> LlvmAccum stmtsB declsB =
        LlvmAccum (stmtsA Semigroup.<> stmtsB) (declsA Semigroup.<> declsB)

instance Monoid LlvmAccum where
    mempty = LlvmAccum nilOL []
    mappend = (Semigroup.<>)

liftExprData :: LlvmM ExprData -> WriterT LlvmAccum LlvmM LlvmVar
liftExprData action = do
    (var, stmts, decls) <- lift action
    tell $ LlvmAccum stmts decls
    return var

statement :: LlvmStatement -> WriterT LlvmAccum LlvmM ()
statement stmt = tell $ LlvmAccum (unitOL stmt) []

doExprW :: LlvmType -> LlvmExpression -> WriterT LlvmAccum LlvmM LlvmVar
doExprW a b = do
    (var, stmt) <- lift $ doExpr a b
    statement stmt
    return var

exprToVarW :: CmmExpr -> WriterT LlvmAccum LlvmM LlvmVar
exprToVarW = liftExprData . exprToVar

runExprData :: WriterT LlvmAccum LlvmM LlvmVar -> LlvmM ExprData
runExprData action = do
    (var, LlvmAccum stmts decls) <- runWriterT action
    return (var, stmts, decls)

runStmtsDecls :: WriterT LlvmAccum LlvmM () -> LlvmM (LlvmStatements, [LlvmCmmDecl])
runStmtsDecls action = do
    LlvmAccum stmts decls <- execWriterT action
    return (stmts, decls)

getCmmRegW :: CmmReg -> WriterT LlvmAccum LlvmM (LlvmVar, LlvmType)
getCmmRegW = lift . getCmmReg

genLoadW :: Atomic -> CmmExpr -> CmmType -> AlignmentSpec -> WriterT LlvmAccum LlvmM LlvmVar
genLoadW atomic e ty alignment = liftExprData $ genLoad atomic e ty alignment

-- | Return element of single-element list; 'panic' if list is not a single-element list
singletonPanic :: String -> [a] -> a
singletonPanic _ [x] = x
singletonPanic s _ = panic s
