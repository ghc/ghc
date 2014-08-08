{-# LANGUAGE CPP, GADTs #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- ----------------------------------------------------------------------------
-- | Handle conversion of CmmProc to LLVM code.
--
module LlvmCodeGen.CodeGen ( genLlvmProc ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Base
import LlvmCodeGen.Regs

import BlockId
import CodeGen.Platform ( activeStgRegs, callerSaves )
import CLabel
import Cmm
import CPrim
import PprCmm
import CmmUtils
import Hoopl

import DynFlags
import FastString
import ForeignCall
import Outputable hiding ( panic, pprPanic )
import qualified Outputable
import Platform
import OrdList
import UniqSupply
import Unique

import Data.List ( nub )
import Data.Maybe ( catMaybes )

type Atomic = Bool
type LlvmStatements = OrdList LlvmStatement

-- -----------------------------------------------------------------------------
-- | Top-level of the LLVM proc Code generator
--
genLlvmProc :: RawCmmDecl -> LlvmM [LlvmCmmDecl]
genLlvmProc (CmmProc infos lbl live graph) = do
    let blocks = toBlockListEntryFirstFalseFallthrough graph
    (lmblocks, lmdata) <- basicBlocksCodeGen live blocks
    let info = mapLookup (g_entry graph) infos
        proc = CmmProc info lbl live (ListGraph lmblocks)
    return (proc:lmdata)

genLlvmProc _ = panic "genLlvmProc: case that shouldn't reach here!"

-- -----------------------------------------------------------------------------
-- * Block code generation
--

-- | Generate code for a list of blocks that make up a complete
-- procedure. The first block in the list is exepected to be the entry
-- point and will get the prologue.
basicBlocksCodeGen :: LiveGlobalRegs -> [CmmBlock]
                      -> LlvmM ([LlvmBasicBlock], [LlvmCmmDecl])
basicBlocksCodeGen _    []                     = panic "no entry block!"
basicBlocksCodeGen live (entryBlock:cmmBlocks)
  = do (prologue, prologueTops) <- funPrologue live (entryBlock:cmmBlocks)

       -- Generate code
       (BasicBlock bid entry, entryTops) <- basicBlockCodeGen entryBlock
       (blocks, topss) <- fmap unzip $ mapM basicBlockCodeGen cmmBlocks

       -- Compose
       let entryBlock = BasicBlock bid (fromOL prologue ++ entry)
       return (entryBlock : blocks, prologueTops ++ entryTops ++ concat topss)


-- | Generate code for one block
basicBlockCodeGen :: CmmBlock -> LlvmM ( LlvmBasicBlock, [LlvmCmmDecl] )
basicBlockCodeGen block
  = do let (CmmEntry id, nodes, tail)  = blockSplit block
       (mid_instrs, top) <- stmtsToInstrs $ blockToList nodes
       (tail_instrs, top')  <- stmtToInstrs tail
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
stmtsToInstrs :: [CmmNode e x] -> LlvmM StmtData
stmtsToInstrs stmts
   = do (instrss, topss) <- fmap unzip $ mapM stmtToInstrs stmts
        return (concatOL instrss, concat topss)


-- | Convert a CmmStmt to a list of LlvmStatement's
stmtToInstrs :: CmmNode e x -> LlvmM StmtData
stmtToInstrs stmt = case stmt of

    CmmComment _         -> return (nilOL, []) -- nuke comments

    CmmAssign reg src    -> genAssign reg src
    CmmStore addr src    -> genStore addr src

    CmmBranch id         -> genBranch id
    CmmCondBranch arg true false
                         -> genCondBranch arg true false
    CmmSwitch arg ids    -> genSwitch arg ids

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
        return [CmmData Data [([],[fty])]]

    return (fv, nilOL, tops)

getInstrinct2 _ _ = error "getInstrinct2: Non-function type!"

-- | Declares an instrinct function by return and parameter types
getInstrinct :: LMString -> LlvmType -> [LlvmType] -> LlvmM ExprData
getInstrinct fname retTy parTys =
    let funSig = LlvmFunctionDecl fname ExternallyVisible CC_Ccc retTy
                    FixedArgs (tysToParams parTys) Nothing
        fty = LMFunction funSig
    in getInstrinct2 fname fty

-- | Memory barrier instruction for LLVM >= 3.0
barrier :: LlvmM StmtData
barrier = do
    let s = Fence False SyncSeqCst
    return (unitOL s, [])

-- | Memory barrier instruction for LLVM < 3.0
oldBarrier :: LlvmM StmtData
oldBarrier = do

    (fv, _, tops) <- getInstrinct (fsLit "llvm.memory.barrier") LMVoid [i1, i1, i1, i1, i1]

    let args = [lmTrue, lmTrue, lmTrue, lmTrue, lmTrue]
    let s1 = Expr $ Call StdCall fv args llvmStdFunAttrs

    return (unitOL s1, tops)

    where
        lmTrue :: LlvmVar
        lmTrue  = mkIntLit i1 (-1)

-- | Foreign Calls
genCall :: ForeignTarget -> [CmmFormal] -> [CmmActual]
              -> LlvmM StmtData

-- Write barrier needs to be handled specially as it is implemented as an LLVM
-- intrinsic function.
genCall (PrimTarget MO_WriteBarrier) _ _ = do
    platform <- getLlvmPlatform
    ver <- getLlvmVer
    case () of
     _ | platformArch platform `elem` [ArchX86, ArchX86_64, ArchSPARC]
                    -> return (nilOL, [])
       | ver > 29   -> barrier
       | otherwise  -> oldBarrier

genCall (PrimTarget MO_Touch) _ _
 = return (nilOL, [])

genCall (PrimTarget (MO_UF_Conv w)) [dst] [e] = do
    dstV <- getCmmReg (CmmLocal dst)
    let ty = cmmToLlvmType $ localRegType dst
        width = widthToLlvmFloat w
    castV <- mkLocalVar ty
    (ve, stmts, top) <- exprToVar e
    let stmt3 = Assignment castV $ Cast LM_Uitofp ve width
        stmt4 = Store castV dstV
    return (stmts `snocOL` stmt3 `snocOL` stmt4, top)

genCall (PrimTarget (MO_UF_Conv _)) [_] args =
    panic $ "genCall: Too many arguments to MO_UF_Conv. " ++
    "Can only handle 1, given" ++ show (length args) ++ "."

-- Handle prefetching data
genCall t@(PrimTarget (MO_Prefetch_Data localityInt)) [] args
  | 0 <= localityInt && localityInt <= 3 = do
    ver <- getLlvmVer
    let argTy | ver <= 29  = [i8Ptr, i32, i32]
              | otherwise  = [i8Ptr, i32, i32, i32]
        funTy = \name -> LMFunction $ LlvmFunctionDecl name ExternallyVisible
                             CC_Ccc LMVoid FixedArgs (tysToParams argTy) Nothing

    let (_, arg_hints) = foreignTargetHints t
    let args_hints' = zip args arg_hints
    (argVars, stmts1, top1) <- arg_vars args_hints' ([], nilOL, [])
    (fptr, stmts2, top2)    <- getFunPtr funTy t
    (argVars', stmts3)      <- castVars $ zip argVars argTy

    trash <- getTrashStmts
    let argSuffix | ver <= 29  = [mkIntLit i32 0, mkIntLit i32 localityInt]
                  | otherwise  = [mkIntLit i32 0, mkIntLit i32 localityInt, mkIntLit i32 1]
        call = Expr $ Call StdCall fptr (argVars' ++ argSuffix) []
        stmts = stmts1 `appOL` stmts2 `appOL` stmts3
                `appOL` trash `snocOL` call
    return (stmts, top1 ++ top2)
  | otherwise = panic $ "prefetch locality level integer must be between 0 and 3, given: " ++ (show localityInt)

-- Handle PopCnt and BSwap that need to only convert arg and return types
genCall t@(PrimTarget (MO_PopCnt w)) dsts args =
    genCallSimpleCast w t dsts args
genCall t@(PrimTarget (MO_BSwap w)) dsts args =
    genCallSimpleCast w t dsts args

genCall (PrimTarget (MO_AtomicRead _)) [dst] [addr] = do
  dstV <- getCmmReg (CmmLocal dst)
  (v1, stmts, top) <- genLoad True addr (localRegType dst)
  let stmt1 = Store v1 dstV
  return (stmts `snocOL` stmt1, top)

-- TODO: implement these properly rather than calling to RTS functions.
-- genCall t@(PrimTarget (MO_AtomicWrite width)) [] [addr, val] = undefined
-- genCall t@(PrimTarget (MO_AtomicRMW width amop)) [dst] [addr, n] = undefined
-- genCall t@(PrimTarget (MO_Cmpxchg width)) [dst] [addr, old, new] = undefined

-- Handle memcpy function specifically since llvm's intrinsic version takes
-- some extra parameters.
genCall t@(PrimTarget op) [] args'
 | op == MO_Memcpy ||
   op == MO_Memset ||
   op == MO_Memmove = do
    ver <- getLlvmVer
    dflags <- getDynFlags
    let (args, alignVal) = splitAlignVal args'
        (isVolTy, isVolVal)
              | ver >= 28       = ([i1], [mkIntLit i1 0])
              | otherwise       = ([], [])
        argTy | op == MO_Memset = [i8Ptr, i8,    llvmWord dflags, i32] ++ isVolTy
              | otherwise       = [i8Ptr, i8Ptr, llvmWord dflags, i32] ++ isVolTy
        funTy = \name -> LMFunction $ LlvmFunctionDecl name ExternallyVisible
                             CC_Ccc LMVoid FixedArgs (tysToParams argTy) Nothing

    let (_, arg_hints) = foreignTargetHints t
    let args_hints = zip args arg_hints
    (argVars, stmts1, top1)       <- arg_vars args_hints ([], nilOL, [])
    (fptr, stmts2, top2)          <- getFunPtr funTy t
    (argVars', stmts3)            <- castVars $ zip argVars argTy

    stmts4 <- getTrashStmts
    let arguments = argVars' ++ (alignVal:isVolVal)
        call = Expr $ Call StdCall fptr arguments []
        stmts = stmts1 `appOL` stmts2 `appOL` stmts3
                `appOL` stmts4 `snocOL` call
    return (stmts, top1 ++ top2)
  where
    splitAlignVal xs = (init xs, extractLit $ last xs)

    -- Fix for trac #6158. Since LLVM 3.1, opt fails when given anything other
    -- than a direct constant (i.e. 'i32 8') as the alignment argument for the
    -- memcpy & co llvm intrinsic functions. So we handle this directly now.
    extractLit (CmmLit (CmmInt i _)) = mkIntLit i32 i
    extractLit _other = trace ("WARNING: Non constant alignment value given" ++ 
                               " for memcpy! Please report to GHC developers")
                        mkIntLit i32 0

-- Handle all other foreign calls and prim ops.
genCall target res args = do

    dflags <- getDynFlags

    -- parameter types
    let arg_type (_, AddrHint) = i8Ptr
        -- cast pointers to i8*. Llvm equivalent of void*
        arg_type (expr, _) = cmmToLlvmType $ cmmExprType dflags expr

    -- ret type
    let ret_type [] = LMVoid
        ret_type [(_, AddrHint)] = i8Ptr
        ret_type [(reg, _)]      = cmmToLlvmType $ localRegType reg
        ret_type t = panic $ "genCall: Too many return values! Can only handle"
                        ++ " 0 or 1, given " ++ show (length t) ++ "."

    -- extract Cmm call convention, and translate to LLVM call convention
    platform <- getLlvmPlatform
    let lmconv = case target of
            ForeignTarget _ (ForeignConvention conv _ _ _) ->
              case conv of
                 StdCallConv  -> case platformArch platform of
                                 ArchX86    -> CC_X86_Stdcc
                                 ArchX86_64 -> CC_X86_Stdcc
                                 _          -> CC_Ccc
                 CCallConv    -> CC_Ccc
                 CApiConv     -> CC_Ccc
                 PrimCallConv -> panic "LlvmCodeGen.CodeGen.genCall: PrimCallConv"
                 JavaScriptCallConv -> panic "LlvmCodeGen.CodeGen.genCall: JavaScriptCallConv"

            PrimTarget   _ -> CC_Ccc

    {-
        CC_Ccc of the possibilities here are a worry with the use of a custom
        calling convention for passing STG args. In practice the more
        dangerous combinations (e.g StdCall + llvmGhcCC) don't occur.

        The native code generator only handles StdCall and CCallConv.
    -}

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
    let argTy = tysToParams $ map arg_type args_hints
    let funTy = \name -> LMFunction $ LlvmFunctionDecl name ExternallyVisible
                             lmconv retTy FixedArgs argTy (llvmFunAlign dflags)


    (argVars, stmts1, top1) <- arg_vars args_hints ([], nilOL, [])
    (fptr, stmts2, top2)    <- getFunPtr funTy target

    let retStmt | ccTy == TailCall  = unitOL $ Return Nothing
                | never_returns     = unitOL $ Unreachable
                | otherwise         = nilOL

    stmts3 <- getTrashStmts
    let stmts = stmts1 `appOL` stmts2 `appOL` stmts3

    -- make the actual call
    case retTy of
        LMVoid -> do
            let s1 = Expr $ Call ccTy fptr argVars fnAttrs
            let allStmts = stmts `snocOL` s1 `appOL` retStmt
            return (allStmts, top1 ++ top2)

        _ -> do
            (v1, s1) <- doExpr retTy $ Call ccTy fptr argVars fnAttrs
            -- get the return register
            let ret_reg [reg] = reg
                ret_reg t = panic $ "genCall: Bad number of registers! Can only handle"
                                ++ " 1, given " ++ show (length t) ++ "."
            let creg = ret_reg res
            vreg <- getCmmReg (CmmLocal creg)
            let allStmts = stmts `snocOL` s1
            if retTy == pLower (getVarType vreg)
                then do
                    let s2 = Store v1 vreg
                    return (allStmts `snocOL` s2 `appOL` retStmt,
                                top1 ++ top2)
                else do
                    let ty = pLower $ getVarType vreg
                    let op = case ty of
                            vt | isPointer vt -> LM_Bitcast
                               | isInt     vt -> LM_Ptrtoint
                               | otherwise    ->
                                   panic $ "genCall: CmmReg bad match for"
                                        ++ " returned type!"

                    (v2, s2) <- doExpr ty $ Cast op v1 ty
                    let s3 = Store v2 vreg
                    return (allStmts `snocOL` s2 `snocOL` s3
                                `appOL` retStmt, top1 ++ top2)

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

    dstV                        <- getCmmReg (CmmLocal dst)

    let (_, arg_hints) = foreignTargetHints t
    let args_hints = zip args arg_hints
    (argsV, stmts2, top2)       <- arg_vars args_hints ([], nilOL, [])
    (argsV', stmts4)            <- castVars $ zip argsV [width]
    (retV, s1)                  <- doExpr width $ Call StdCall fptr argsV' []
    ([retV'], stmts5)           <- castVars [(retV,dstTy)]
    let s2                       = Store retV' dstV

    let stmts = stmts2 `appOL` stmts4 `snocOL`
                s1 `appOL` stmts5 `snocOL` s2
    return (stmts, top2 ++ top3)
genCallSimpleCast _ _ dsts _ =
    panic ("genCallSimpleCast: " ++ show (length dsts) ++ " dsts")

-- | Create a function pointer from a target.
getFunPtr :: (LMString -> LlvmType) -> ForeignTarget
          -> LlvmM ExprData
getFunPtr funTy targ = case targ of
    ForeignTarget (CmmLit (CmmLabel lbl)) _ -> do
        name <- strCLabel_llvm lbl
        getHsFunc' name (funTy name)

    ForeignTarget expr _ -> do
        (v1, stmts, top) <- exprToVar expr
        dflags <- getDynFlags
        let fty = funTy $ fsLit "dynamic"
            cast = case getVarType v1 of
                ty | isPointer ty -> LM_Bitcast
                ty | isInt ty     -> LM_Inttoptr

                ty -> panic $ "genCall: Expr is of bad type for function"
                              ++ " call! (" ++ showSDoc dflags (ppr ty) ++ ")"

        (v2,s1) <- doExpr (pLift fty) $ Cast cast v1 (pLift fty)
        return (v2, stmts `snocOL` s1, top)

    PrimTarget mop -> do
        name <- cmmPrimOpFunctions mop
        let fty = funTy name
        getInstrinct2 name fty

-- | Conversion of call arguments.
arg_vars :: [(CmmActual, ForeignHint)]
         -> ([LlvmVar], LlvmStatements, [LlvmCmmDecl])
         -> LlvmM ([LlvmVar], LlvmStatements, [LlvmCmmDecl])

arg_vars [] (vars, stmts, tops)
  = return (vars, stmts, tops)

arg_vars ((e, AddrHint):rest) (vars, stmts, tops)
  = do (v1, stmts', top') <- exprToVar e
       dflags <- getDynFlags
       let op = case getVarType v1 of
               ty | isPointer ty -> LM_Bitcast
               ty | isInt ty     -> LM_Inttoptr

               a  -> panic $ "genCall: Can't cast llvmType to i8*! ("
                           ++ showSDoc dflags (ppr a) ++ ")"

       (v2, s1) <- doExpr i8Ptr $ Cast op v1 i8Ptr
       arg_vars rest (vars ++ [v2], stmts `appOL` stmts' `snocOL` s1,
                               tops ++ top')

arg_vars ((e, _):rest) (vars, stmts, tops)
  = do (v1, stmts', top') <- exprToVar e
       arg_vars rest (vars ++ [v1], stmts `appOL` stmts', tops ++ top')


-- | Cast a collection of LLVM variables to specific types.
castVars :: [(LlvmVar, LlvmType)]
         -> LlvmM ([LlvmVar], LlvmStatements)
castVars vars = do
                done <- mapM (uncurry castVar) vars
                let (vars', stmts) = unzip done
                return (vars', toOL stmts)

-- | Cast an LLVM variable to a specific type, panicing if it can't be done.
castVar :: LlvmVar -> LlvmType -> LlvmM (LlvmVar, LlvmStatement)
castVar v t | getVarType v == t
            = return (v, Nop)

            | otherwise
            = do dflags <- getDynFlags
                 let op = case (getVarType v, t) of
                      (LMInt n, LMInt m)
                          -> if n < m then LM_Sext else LM_Trunc
                      (vt, _) | isFloat vt && isFloat t
                          -> if llvmWidthInBits dflags vt < llvmWidthInBits dflags t
                                then LM_Fpext else LM_Fptrunc
                      (vt, _) | isInt vt && isFloat t       -> LM_Sitofp
                      (vt, _) | isFloat vt && isInt t       -> LM_Fptosi
                      (vt, _) | isInt vt && isPointer t     -> LM_Inttoptr
                      (vt, _) | isPointer vt && isInt t     -> LM_Ptrtoint
                      (vt, _) | isPointer vt && isPointer t -> LM_Bitcast
                      (vt, _) | isVector vt && isVector t   -> LM_Bitcast

                      (vt, _) -> panic $ "castVars: Can't cast this type ("
                                  ++ showSDoc dflags (ppr vt) ++ ") to (" ++ showSDoc dflags (ppr t) ++ ")"
                 doExpr t $ Cast op v t


-- | Decide what C function to use to implement a CallishMachOp
cmmPrimOpFunctions :: CallishMachOp -> LlvmM LMString
cmmPrimOpFunctions mop = do

  ver <- getLlvmVer
  dflags <- getDynFlags
  let intrinTy1 = (if ver >= 28
                       then "p0i8.p0i8." else "") ++ showSDoc dflags (ppr $ llvmWord dflags)
      intrinTy2 = (if ver >= 28
                       then "p0i8." else "") ++ showSDoc dflags (ppr $ llvmWord dflags)
      unsupported = panic ("cmmPrimOpFunctions: " ++ show mop
                        ++ " not supported here")

  return $ case mop of
    MO_F32_Exp    -> fsLit "expf"
    MO_F32_Log    -> fsLit "logf"
    MO_F32_Sqrt   -> fsLit "llvm.sqrt.f32"
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

    MO_F64_Exp    -> fsLit "exp"
    MO_F64_Log    -> fsLit "log"
    MO_F64_Sqrt   -> fsLit "llvm.sqrt.f64"
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

    MO_Memcpy     -> fsLit $ "llvm.memcpy."  ++ intrinTy1
    MO_Memmove    -> fsLit $ "llvm.memmove." ++ intrinTy1
    MO_Memset     -> fsLit $ "llvm.memset."  ++ intrinTy2

    (MO_PopCnt w) -> fsLit $ "llvm.ctpop."  ++ showSDoc dflags (ppr $ widthToLlvmInt w)
    (MO_BSwap w)  -> fsLit $ "llvm.bswap."  ++ showSDoc dflags (ppr $ widthToLlvmInt w)

    (MO_Prefetch_Data _ )-> fsLit "llvm.prefetch"

    MO_S_QuotRem {}  -> unsupported
    MO_U_QuotRem {}  -> unsupported
    MO_U_QuotRem2 {} -> unsupported
    MO_Add2 {}       -> unsupported
    MO_U_Mul2 {}     -> unsupported
    MO_WriteBarrier  -> unsupported
    MO_Touch         -> unsupported
    MO_UF_Conv _     -> unsupported

    MO_AtomicRead _  -> unsupported

    MO_AtomicRMW w amop -> fsLit $ atomicRMWLabel w amop
    MO_Cmpxchg w        -> fsLit $ cmpxchgLabel w
    MO_AtomicWrite w    -> fsLit $ atomicWriteLabel w

-- | Tail function calls
genJump :: CmmExpr -> [GlobalReg] -> LlvmM StmtData

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
    dflags <- getDynFlags

    let cast = case getVarType vf of
         ty | isPointer ty -> LM_Bitcast
         ty | isInt ty     -> LM_Inttoptr

         ty -> panic $ "genJump: Expr is of bad type for function call! ("
                     ++ showSDoc dflags (ppr ty) ++ ")"

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
    vreg <- getCmmReg reg
    (vval, stmts2, top2) <- exprToVar val
    let stmts = stmts2

    let ty = (pLower . getVarType) vreg
    dflags <- getDynFlags
    case ty of
      -- Some registers are pointer types, so need to cast value to pointer
      LMPointer _ | getVarType vval == llvmWord dflags -> do
          (v, s1) <- doExpr ty $ Cast LM_Inttoptr vval ty
          let s2 = Store v vreg
          return (stmts `snocOL` s1 `snocOL` s2, top2)

      LMVector _ _ -> do
          (v, s1) <- doExpr ty $ Cast LM_Bitcast vval ty
          let s2 = Store v vreg
          return (stmts `snocOL` s1 `snocOL` s2, top2)

      _ -> do
          let s1 = Store vval vreg
          return (stmts `snocOL` s1, top2)


-- | CmmStore operation
genStore :: CmmExpr -> CmmExpr -> LlvmM StmtData

-- First we try to detect a few common cases and produce better code for
-- these then the default case. We are mostly trying to detect Cmm code
-- like I32[Sp + n] and use 'getelementptr' operations instead of the
-- generic case that uses casts and pointer arithmetic
genStore addr@(CmmReg (CmmGlobal r)) val
    = genStore_fast addr r 0 val

genStore addr@(CmmRegOff (CmmGlobal r) n) val
    = genStore_fast addr r n val

genStore addr@(CmmMachOp (MO_Add _) [
                            (CmmReg (CmmGlobal r)),
                            (CmmLit (CmmInt n _))])
                val
    = genStore_fast addr r (fromInteger n) val

genStore addr@(CmmMachOp (MO_Sub _) [
                            (CmmReg (CmmGlobal r)),
                            (CmmLit (CmmInt n _))])
                val
    = genStore_fast addr r (negate $ fromInteger n) val

-- generic case
genStore addr val
    = do other <- getTBAAMeta otherN
         genStore_slow addr val other

-- | CmmStore operation
-- This is a special case for storing to a global register pointer
-- offset such as I32[Sp+8].
genStore_fast :: CmmExpr -> GlobalReg -> Int -> CmmExpr
              -> LlvmM StmtData
genStore_fast addr r n val
  = do dflags <- getDynFlags
       (gv, grt, s1) <- getCmmRegVal (CmmGlobal r)
       meta          <- getTBAARegMeta r
       let (ix,rem) = n `divMod` ((llvmWidthInBits dflags . pLower) grt  `div` 8)
       case isPointer grt && rem == 0 of
            True -> do
                (vval,  stmts, top) <- exprToVar val
                (ptr, s2) <- doExpr grt $ GetElemPtr True gv [toI32 ix]
                -- We might need a different pointer type, so check
                case pLower grt == getVarType vval of
                     -- were fine
                     True  -> do
                         let s3 = MetaStmt meta $ Store vval ptr
                         return (stmts `appOL` s1 `snocOL` s2
                                 `snocOL` s3, top)

                     -- cast to pointer type needed
                     False -> do
                         let ty = (pLift . getVarType) vval
                         (ptr', s3) <- doExpr ty $ Cast LM_Bitcast ptr ty
                         let s4 = MetaStmt meta $ Store vval ptr'
                         return (stmts `appOL` s1 `snocOL` s2
                                 `snocOL` s3 `snocOL` s4, top)

            -- If its a bit type then we use the slow method since
            -- we can't avoid casting anyway.
            False -> genStore_slow addr val meta


-- | CmmStore operation
-- Generic case. Uses casts and pointer arithmetic if needed.
genStore_slow :: CmmExpr -> CmmExpr -> [MetaAnnot] -> LlvmM StmtData
genStore_slow addr val meta = do
    (vaddr, stmts1, top1) <- exprToVar addr
    (vval,  stmts2, top2) <- exprToVar val

    let stmts = stmts1 `appOL` stmts2
    dflags <- getDynFlags
    case getVarType vaddr of
        -- sometimes we need to cast an int to a pointer before storing
        LMPointer ty@(LMPointer _) | getVarType vval == llvmWord dflags -> do
            (v, s1) <- doExpr ty $ Cast LM_Inttoptr vval ty
            let s2 = MetaStmt meta $ Store v vaddr
            return (stmts `snocOL` s1 `snocOL` s2, top1 ++ top2)

        LMPointer _ -> do
            let s1 = MetaStmt meta $ Store vval vaddr
            return (stmts `snocOL` s1, top1 ++ top2)

        i@(LMInt _) | i == llvmWord dflags -> do
            let vty = pLift $ getVarType vval
            (vptr, s1) <- doExpr vty $ Cast LM_Inttoptr vaddr vty
            let s2 = MetaStmt meta $ Store vval vptr
            return (stmts `snocOL` s1 `snocOL` s2, top1 ++ top2)

        other ->
            pprPanic "genStore: ptr not right type!"
                    (PprCmm.pprExpr addr <+> text (
                        "Size of Ptr: " ++ show (llvmPtrBits dflags) ++
                        ", Size of var: " ++ show (llvmWidthInBits dflags other) ++
                        ", Var: " ++ showSDoc dflags (ppr vaddr)))


-- | Unconditional branch
genBranch :: BlockId -> LlvmM StmtData
genBranch id =
    let label = blockIdToLlvm id
    in return (unitOL $ Branch label, [])


-- | Conditional branch
genCondBranch :: CmmExpr -> BlockId -> BlockId -> LlvmM StmtData
genCondBranch cond idT idF = do
    let labelT = blockIdToLlvm idT
    let labelF = blockIdToLlvm idF
    -- See Note [Literals and branch conditions].
    (vc, stmts, top) <- exprToVarOpt i1Option cond
    if getVarType vc == i1
        then do
            let s1 = BranchIf vc labelT labelF
            return (stmts `snocOL` s1, top)
        else do
            dflags <- getDynFlags
            panic $ "genCondBranch: Cond expr not bool! (" ++ showSDoc dflags (ppr vc) ++ ")"

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

    #ifndef SOME_CONDITIONAL
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
--
-- N.B. We remove Nothing's from the list of branches, as they are 'undefined'.
-- However, they may be defined one day, so we better document this behaviour.
genSwitch :: CmmExpr -> [Maybe BlockId] -> LlvmM StmtData
genSwitch cond maybe_ids = do
    (vc, stmts, top) <- exprToVar cond
    let ty = getVarType vc

    let pairs = [ (ix, id) | (ix,Just id) <- zip [0..] maybe_ids ]
    let labels = map (\(ix, b) -> (mkIntLit ty ix, blockIdToLlvm b)) pairs
    -- out of range is undefined, so let's just branch to first label
    let (_, defLbl) = head labels

    let s1 = Switch vc defLbl labels
    return $ (stmts `snocOL` s1, top)


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

    CmmLoad e' ty
        -> genLoad False e' ty

    -- Cmmreg in expression is the value, so must load. If you want actual
    -- reg pointer, call getCmmReg directly.
    CmmReg r -> do
        (v1, ty, s1) <- getCmmRegVal r
        case isPointer ty of
             True  -> do
                 -- Cmm wants the value, so pointer types must be cast to ints
                 dflags <- getDynFlags
                 (v2, s2) <- doExpr (llvmWord dflags) $ Cast LM_Ptrtoint v1 (llvmWord dflags)
                 return (v2, s1 `snocOL` s2, [])

             False -> return (v1, s1, [])

    CmmMachOp op exprs
        -> genMachOp opt op exprs

    CmmRegOff r i
        -> do dflags <- getDynFlags
              exprToVar $ expandCmmReg dflags (r, i)

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

    MO_SF_Conv _ w -> fiConv (widthToLlvmFloat w) LM_Sitofp
    MO_FS_Conv _ w -> fiConv (widthToLlvmInt w) LM_Fptosi

    MO_SS_Conv from to
        -> sameConv from (widthToLlvmInt to) LM_Trunc LM_Sext

    MO_UU_Conv from to
        -> sameConv from (widthToLlvmInt to) LM_Trunc LM_Zext

    MO_FF_Conv from to
        -> sameConv from (widthToLlvmFloat to) LM_Fptrunc LM_Fpext

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

    -- Handle unsupported cases explicitly so we get a warning
    -- of missing case when new MachOps added
    MO_Add _          -> panicOp
    MO_Mul _          -> panicOp
    MO_Sub _          -> panicOp
    MO_S_MulMayOflo _ -> panicOp
    MO_S_Quot _       -> panicOp
    MO_S_Rem _        -> panicOp
    MO_U_MulMayOflo _ -> panicOp
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

    MO_VU_Quot    _ _ -> panicOp
    MO_VU_Rem     _ _ -> panicOp
 
    MO_VF_Insert  _ _ -> panicOp
    MO_VF_Extract _ _ -> panicOp

    MO_VF_Add     _ _ -> panicOp
    MO_VF_Sub     _ _ -> panicOp
    MO_VF_Mul     _ _ -> panicOp
    MO_VF_Quot    _ _ -> panicOp

    where
        negate ty v2 negOp = do
            (vx, stmts, top) <- exprToVar x
            (v1, s1) <- doExpr ty $ LlvmOp negOp v2 vx
            return (v1, stmts `snocOL` s1, top)

        negateVec ty v2 negOp = do
            (vx, stmts1, top) <- exprToVar x
            ([vx'], stmts2) <- castVars [(vx, ty)]
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
            dflags <- getDynFlags
            let toWidth = llvmWidthInBits dflags ty
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
genMachOp_fast :: EOption -> MachOp -> GlobalReg -> Int -> [CmmExpr]
               -> LlvmM ExprData
genMachOp_fast opt op r n e
  = do (gv, grt, s1) <- getCmmRegVal (CmmGlobal r)
       dflags <- getDynFlags
       let (ix,rem) = n `divMod` ((llvmWidthInBits dflags . pLower) grt  `div` 8)
       case isPointer grt && rem == 0 of
            True -> do
                (ptr, s2) <- doExpr grt $ GetElemPtr True gv [toI32 ix]
                (var, s3) <- doExpr (llvmWord dflags) $ Cast LM_Ptrtoint ptr (llvmWord dflags)
                return (var, s1 `snocOL` s2 `snocOL` s3, [])

            False -> genMachOp_slow opt op e


-- | Handle CmmMachOp expressions
-- This handles all the cases not handle by the specialised genMachOp_fast.
genMachOp_slow :: EOption -> MachOp -> [CmmExpr] -> LlvmM ExprData

-- Element extraction
genMachOp_slow _ (MO_V_Extract l w) [val, idx] = do
    (vval, stmts1, top1) <- exprToVar val
    (vidx, stmts2, top2) <- exprToVar idx
    ([vval'], stmts3)    <- castVars [(vval, LMVector l ty)]
    (v1, s1)             <- doExpr ty $ Extract vval' vidx
    return (v1, stmts1 `appOL` stmts2 `appOL` stmts3 `snocOL` s1, top1 ++ top2)
  where
    ty = widthToLlvmInt w

genMachOp_slow _ (MO_VF_Extract l w) [val, idx] = do
    (vval, stmts1, top1) <- exprToVar val
    (vidx, stmts2, top2) <- exprToVar idx
    ([vval'], stmts3)    <- castVars [(vval, LMVector l ty)]
    (v1, s1)             <- doExpr ty $ Extract vval' vidx
    return (v1, stmts1 `appOL` stmts2 `appOL` stmts3 `snocOL` s1, top1 ++ top2)
  where
    ty = widthToLlvmFloat w

-- Element insertion
genMachOp_slow _ (MO_V_Insert l w) [val, elt, idx] = do
    (vval, stmts1, top1) <- exprToVar val
    (velt, stmts2, top2) <- exprToVar elt
    (vidx, stmts3, top3) <- exprToVar idx
    ([vval'], stmts4)    <- castVars [(vval, ty)]
    (v1, s1)             <- doExpr ty $ Insert vval' velt vidx
    return (v1, stmts1 `appOL` stmts2 `appOL` stmts3 `appOL` stmts4 `snocOL` s1,
            top1 ++ top2 ++ top3)
  where
    ty = LMVector l (widthToLlvmInt w)

genMachOp_slow _ (MO_VF_Insert l w) [val, elt, idx] = do
    (vval, stmts1, top1) <- exprToVar val
    (velt, stmts2, top2) <- exprToVar elt
    (vidx, stmts3, top3) <- exprToVar idx
    ([vval'], stmts4)    <- castVars [(vval, ty)]
    (v1, s1)             <- doExpr ty $ Insert vval' velt vidx
    return (v1, stmts1 `appOL` stmts2 `appOL` stmts3 `appOL` stmts4 `snocOL` s1,
            top1 ++ top2 ++ top3)
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

    MO_U_MulMayOflo _ -> panic "genMachOp: MO_U_MulMayOflo unsupported!"

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

    MO_And _   -> genBinMach LM_MO_And
    MO_Or  _   -> genBinMach LM_MO_Or
    MO_Xor _   -> genBinMach LM_MO_Xor
    MO_Shl _   -> genBinMach LM_MO_Shl
    MO_U_Shr _ -> genBinMach LM_MO_LShr
    MO_S_Shr _ -> genBinMach LM_MO_AShr

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

    MO_SF_Conv _ _ -> panicOp
    MO_FS_Conv _ _ -> panicOp
    MO_SS_Conv _ _ -> panicOp
    MO_UU_Conv _ _ -> panicOp
    MO_FF_Conv _ _ -> panicOp

    MO_V_Insert  {} -> panicOp
    MO_V_Extract {} -> panicOp

    MO_VS_Neg {} -> panicOp

    MO_VF_Insert  {} -> panicOp
    MO_VF_Extract {} -> panicOp

    MO_VF_Neg {} -> panicOp

    where
        binLlvmOp ty binOp = do
            (vx, stmts1, top1) <- exprToVar x
            (vy, stmts2, top2) <- exprToVar y
            if getVarType vx == getVarType vy
                then do
                    (v1, s1) <- doExpr (ty vx) $ binOp vx vy
                    return (v1, stmts1 `appOL` stmts2 `snocOL` s1,
                            top1 ++ top2)

                else do
                    -- Error. Continue anyway so we can debug the generated ll file.
                    dflags <- getDynFlags
                    let style = mkCodeStyle CStyle
                        toString doc = renderWithStyle dflags doc style
                        cmmToStr = (lines . toString . PprCmm.pprExpr)
                    let dx = Comment $ map fsLit $ cmmToStr x
                    let dy = Comment $ map fsLit $ cmmToStr y
                    (v1, s1) <- doExpr (ty vx) $ binOp vx vy
                    let allStmts = stmts1 `appOL` stmts2 `snocOL` dx
                                    `snocOL` dy `snocOL` s1
                    return (v1, allStmts, top1 ++ top2)

        binCastLlvmOp ty binOp = do
            (vx, stmts1, top1) <- exprToVar x
            (vy, stmts2, top2) <- exprToVar y
            ([vx', vy'], stmts3) <- castVars [(vx, ty), (vy, ty)]
            (v1, s1) <- doExpr ty $ binOp vx' vy'
            return (v1, stmts1 `appOL` stmts2 `appOL` stmts3 `snocOL` s1,
                    top1 ++ top2)

        -- | Need to use EOption here as Cmm expects word size results from
        -- comparisons while LLVM return i1. Need to extend to llvmWord type
        -- if expected. See Note [Literals and branch conditions].
        genBinComp opt cmp = do
            ed@(v1, stmts, top) <- binLlvmOp (\_ -> i1) (Compare cmp)
            dflags <- getDynFlags
            if getVarType v1 == i1
                then case i1Expected opt of
                    True  -> return ed
                    False -> do
                        let w_ = llvmWord dflags
                        (v2, s1) <- doExpr w_ $ Cast LM_Zext v1 w_
                        return (v2, stmts `snocOL` s1, top)
                else
                    panic $ "genBinComp: Compare returned type other then i1! "
                        ++ (showSDoc dflags $ ppr $ getVarType v1)

        genBinMach op = binLlvmOp getVarType (LlvmOp op)

        genCastBinMach ty op = binCastLlvmOp ty (LlvmOp op)

        -- | Detect if overflow will occur in signed multiply of the two
        -- CmmExpr's. This is the LLVM assembly equivalent of the NCG
        -- implementation. Its much longer due to type information/safety.
        -- This should actually compile to only about 3 asm instructions.
        isSMulOK :: Width -> CmmExpr -> CmmExpr -> LlvmM ExprData
        isSMulOK _ x y = do
            (vx, stmts1, top1) <- exprToVar x
            (vy, stmts2, top2) <- exprToVar y

            dflags <- getDynFlags
            let word  = getVarType vx
            let word2 = LMInt $ 2 * (llvmWidthInBits dflags $ getVarType vx)
            let shift = llvmWidthInBits dflags word
            let shift1 = toIWord dflags (shift - 1)
            let shift2 = toIWord dflags shift

            if isInt word
                then do
                    (x1, s1)     <- doExpr word2 $ Cast LM_Sext vx word2
                    (y1, s2)     <- doExpr word2 $ Cast LM_Sext vy word2
                    (r1, s3)     <- doExpr word2 $ LlvmOp LM_MO_Mul x1 y1
                    (rlow1, s4)  <- doExpr word $ Cast LM_Trunc r1 word
                    (rlow2, s5)  <- doExpr word $ LlvmOp LM_MO_AShr rlow1 shift1
                    (rhigh1, s6) <- doExpr word2 $ LlvmOp LM_MO_AShr r1 shift2
                    (rhigh2, s7) <- doExpr word $ Cast LM_Trunc rhigh1 word
                    (dst, s8)    <- doExpr word $ LlvmOp LM_MO_Sub rlow2 rhigh2
                    let stmts = (unitOL s1) `snocOL` s2 `snocOL` s3 `snocOL` s4
                            `snocOL` s5 `snocOL` s6 `snocOL` s7 `snocOL` s8
                    return (dst, stmts1 `appOL` stmts2 `appOL` stmts,
                        top1 ++ top2)

                else
                    panic $ "isSMulOK: Not bit type! (" ++ showSDoc dflags (ppr word) ++ ")"

        panicOp = panic $ "LLVM.CodeGen.genMachOp_slow: unary op encountered"
                       ++ "with two arguments! (" ++ show op ++ ")"

-- More then two expression, invalid!
genMachOp_slow _ _ _ = panic "genMachOp: More then 2 expressions in MachOp!"


-- | Handle CmmLoad expression.
genLoad :: Atomic -> CmmExpr -> CmmType -> LlvmM ExprData

-- First we try to detect a few common cases and produce better code for
-- these then the default case. We are mostly trying to detect Cmm code
-- like I32[Sp + n] and use 'getelementptr' operations instead of the
-- generic case that uses casts and pointer arithmetic
genLoad atomic e@(CmmReg (CmmGlobal r)) ty
    = genLoad_fast atomic e r 0 ty

genLoad atomic e@(CmmRegOff (CmmGlobal r) n) ty
    = genLoad_fast atomic e r n ty

genLoad atomic e@(CmmMachOp (MO_Add _) [
                            (CmmReg (CmmGlobal r)),
                            (CmmLit (CmmInt n _))])
                ty
    = genLoad_fast atomic e r (fromInteger n) ty

genLoad atomic e@(CmmMachOp (MO_Sub _) [
                            (CmmReg (CmmGlobal r)),
                            (CmmLit (CmmInt n _))])
                ty
    = genLoad_fast atomic e r (negate $ fromInteger n) ty

-- generic case
genLoad atomic e ty
    = do other <- getTBAAMeta otherN
         genLoad_slow atomic e ty other

-- | Handle CmmLoad expression.
-- This is a special case for loading from a global register pointer
-- offset such as I32[Sp+8].
genLoad_fast :: Atomic -> CmmExpr -> GlobalReg -> Int -> CmmType
             -> LlvmM ExprData
genLoad_fast atomic e r n ty = do
    dflags <- getDynFlags
    (gv, grt, s1) <- getCmmRegVal (CmmGlobal r)
    meta          <- getTBAARegMeta r
    let ty'      = cmmToLlvmType ty
        (ix,rem) = n `divMod` ((llvmWidthInBits dflags . pLower) grt  `div` 8)
    case isPointer grt && rem == 0 of
            True  -> do
                (ptr, s2) <- doExpr grt $ GetElemPtr True gv [toI32 ix]
                -- We might need a different pointer type, so check
                case grt == ty' of
                     -- were fine
                     True -> do
                         (var, s3) <- doExpr ty' (MExpr meta $ loadInstr ptr)
                         return (var, s1 `snocOL` s2 `snocOL` s3,
                                     [])

                     -- cast to pointer type needed
                     False -> do
                         let pty = pLift ty'
                         (ptr', s3) <- doExpr pty $ Cast LM_Bitcast ptr pty
                         (var, s4) <- doExpr ty' (MExpr meta $ loadInstr ptr')
                         return (var, s1 `snocOL` s2 `snocOL` s3
                                    `snocOL` s4, [])

            -- If its a bit type then we use the slow method since
            -- we can't avoid casting anyway.
            False -> genLoad_slow atomic  e ty meta
  where
    loadInstr ptr | atomic    = ALoad SyncSeqCst False ptr
                  | otherwise = Load ptr

-- | Handle Cmm load expression.
-- Generic case. Uses casts and pointer arithmetic if needed.
genLoad_slow :: Atomic -> CmmExpr -> CmmType -> [MetaAnnot] -> LlvmM ExprData
genLoad_slow atomic e ty meta = do
    (iptr, stmts, tops) <- exprToVar e
    dflags <- getDynFlags
    case getVarType iptr of
         LMPointer _ -> do
                    (dvar, load) <- doExpr (cmmToLlvmType ty)
                                           (MExpr meta $ loadInstr iptr)
                    return (dvar, stmts `snocOL` load, tops)

         i@(LMInt _) | i == llvmWord dflags -> do
                    let pty = LMPointer $ cmmToLlvmType ty
                    (ptr, cast)  <- doExpr pty $ Cast LM_Inttoptr iptr pty
                    (dvar, load) <- doExpr (cmmToLlvmType ty)
                                           (MExpr meta $ loadInstr ptr)
                    return (dvar, stmts `snocOL` cast `snocOL` load, tops)

         other -> do dflags <- getDynFlags
                     pprPanic "exprToVar: CmmLoad expression is not right type!"
                        (PprCmm.pprExpr e <+> text (
                            "Size of Ptr: " ++ show (llvmPtrBits dflags) ++
                            ", Size of var: " ++ show (llvmWidthInBits dflags other) ++
                            ", Var: " ++ showSDoc dflags (ppr iptr)))
  where
    loadInstr ptr | atomic    = ALoad SyncSeqCst False ptr
                  | otherwise = Load ptr


-- | Handle CmmReg expression. This will return a pointer to the stack
-- location of the register. Throws an error if it isn't allocated on
-- the stack.
getCmmReg :: CmmReg -> LlvmM LlvmVar
getCmmReg (CmmLocal (LocalReg un _))
  = do exists <- varLookup un
       dflags <- getDynFlags
       case exists of
         Just ety -> return (LMLocalVar un $ pLift ety)
         Nothing  -> fail $ "getCmmReg: Cmm register " ++ showSDoc dflags (ppr un) ++ " was not allocated!"
           -- This should never happen, as every local variable should
           -- have been assigned a value at some point, triggering
           -- "funPrologue" to allocate it on the stack.

getCmmReg (CmmGlobal g)
  = do onStack <- checkStackReg g
       dflags <- getDynFlags
       if onStack
         then return (lmGlobalRegVar dflags g)
         else fail $ "getCmmReg: Cmm register " ++ showSDoc dflags (ppr g) ++ " not stack-allocated!"

-- | Return the value of a given register, as well as its type. Might
-- need to be load from stack.
getCmmRegVal :: CmmReg -> LlvmM (LlvmVar, LlvmType, LlvmStatements)
getCmmRegVal reg =
  case reg of
    CmmGlobal g -> do
      onStack <- checkStackReg g
      dflags <- getDynFlags
      if onStack then loadFromStack else do
        let r = lmGlobalRegArg dflags g
        return (r, getVarType r, nilOL)
    _ -> loadFromStack
 where loadFromStack = do
         ptr <- getCmmReg reg
         let ty = pLower $ getVarType ptr
         (v, s) <- doExpr ty (Load ptr)
         return (v, ty, unitOL s)

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

genLit _ (CmmFloat r w)
  = return (LMLitVar $ LMFloatLit (fromRational r) (widthToLlvmFloat w),
              nilOL, [])

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
       dflags <- getDynFlags
       let lmty = cmmToLlvmType $ cmmLitType dflags cmm
       (v1, s1) <- doExpr lmty $ Cast LM_Ptrtoint var (llvmWord dflags)
       return (v1, unitOL s1, [])

genLit opt (CmmLabelOff label off) = do
    dflags <- getDynFlags
    (vlbl, stmts, stat) <- genLit opt (CmmLabel label)
    let voff = toIWord dflags off
    (v1, s1) <- doExpr (getVarType vlbl) $ LlvmOp LM_MO_Add vlbl voff
    return (v1, stmts `snocOL` s1, stat)

genLit opt (CmmLabelDiffOff l1 l2 off) = do
    dflags <- getDynFlags
    (vl1, stmts1, stat1) <- genLit opt (CmmLabel l1)
    (vl2, stmts2, stat2) <- genLit opt (CmmLabel l2)
    let voff = toIWord dflags off
    let ty1 = getVarType vl1
    let ty2 = getVarType vl2
    if (isInt ty1) && (isInt ty2)
       && (llvmWidthInBits dflags ty1 == llvmWidthInBits dflags ty2)

       then do
            (v1, s1) <- doExpr (getVarType vl1) $ LlvmOp LM_MO_Sub vl1 vl2
            (v2, s2) <- doExpr (getVarType v1 ) $ LlvmOp LM_MO_Add v1 voff
            return (v2, stmts1 `appOL` stmts2 `snocOL` s1 `snocOL` s2,
                        stat1 ++ stat2)

        else
            panic "genLit: CmmLabelDiffOff encountered with different label ty!"

genLit opt (CmmBlock b)
  = genLit opt (CmmLabel $ infoTblLbl b)

genLit _ CmmHighStackMark
  = panic "genStaticLit - CmmHighStackMark unsupported!"


-- -----------------------------------------------------------------------------
-- * Misc
--

-- | Find CmmRegs that get assigned and allocate them on the stack
--
-- Any register that gets written needs to be allcoated on the
-- stack. This avoids having to map a CmmReg to an equivalent SSA form
-- and avoids having to deal with Phi node insertion.  This is also
-- the approach recommended by LLVM developers.
--
-- On the other hand, this is unecessarily verbose if the register in
-- question is never written. Therefore we skip it where we can to
-- save a few lines in the output and hopefully speed compilation up a
-- bit.
funPrologue :: LiveGlobalRegs -> [CmmBlock] -> LlvmM StmtData
funPrologue live cmmBlocks = do

  trash <- getTrashRegs
  let getAssignedRegs :: CmmNode O O -> [CmmReg]
      getAssignedRegs (CmmAssign reg _)  = [reg]
      -- Calls will trash all registers. Unfortunately, this needs them to
      -- be stack-allocated in the first place.
      getAssignedRegs (CmmUnsafeForeignCall _ rs _) = map CmmGlobal trash ++ map CmmLocal rs
      getAssignedRegs _                  = []
      getRegsBlock (_, body, _)          = concatMap getAssignedRegs $ blockToList body
      assignedRegs = nub $ concatMap (getRegsBlock . blockSplit) cmmBlocks
      isLive r     = r `elem` alwaysLive || r `elem` live

  dflags <- getDynFlags
  stmtss <- flip mapM assignedRegs $ \reg ->
    case reg of
      CmmLocal (LocalReg un _) -> do
        let (newv, stmts) = allocReg reg
        varInsert un (pLower $ getVarType newv)
        return stmts
      CmmGlobal r -> do
        let reg   = lmGlobalRegVar dflags r
            arg   = lmGlobalRegArg dflags r
            ty    = (pLower . getVarType) reg
            trash = LMLitVar $ LMUndefLit ty
            rval  = if isLive r then arg else trash
            alloc = Assignment reg $ Alloca (pLower $ getVarType reg) 1
        markStackReg r
        return $ toOL [alloc, Store rval reg]

  return (concatOL stmtss, [])

-- | Function epilogue. Load STG variables to use as argument for call.
-- STG Liveness optimisation done here.
funEpilogue :: LiveGlobalRegs -> LlvmM ([LlvmVar], LlvmStatements)
funEpilogue live = do

    -- Have information and liveness optimisation is enabled?
    let liveRegs = alwaysLive ++ live
        isSSE (FloatReg _)  = True
        isSSE (DoubleReg _) = True
        isSSE (XmmReg _)    = True
        isSSE (YmmReg _)    = True
        isSSE (ZmmReg _)    = True
        isSSE _             = False

    -- Set to value or "undef" depending on whether the register is
    -- actually live
    dflags <- getDynFlags
    let loadExpr r = do
          (v, _, s) <- getCmmRegVal (CmmGlobal r)
          return (Just $ v, s)
        loadUndef r = do
          let ty = (pLower . getVarType $ lmGlobalRegVar dflags r)
          return (Just $ LMLitVar $ LMUndefLit ty, nilOL)
    platform <- getDynFlag targetPlatform
    loads <- flip mapM (activeStgRegs platform) $ \r -> case () of
      _ | r `elem` liveRegs  -> loadExpr r
        | not (isSSE r)      -> loadUndef r
        | otherwise          -> return (Nothing, nilOL)

    let (vars, stmts) = unzip loads
    return (catMaybes vars, concatOL stmts)


-- | A series of statements to trash all the STG registers.
--
-- In LLVM we pass the STG registers around everywhere in function calls.
-- So this means LLVM considers them live across the entire function, when
-- in reality they usually aren't. For Caller save registers across C calls
-- the saving and restoring of them is done by the Cmm code generator,
-- using Cmm local vars. So to stop LLVM saving them as well (and saving
-- all of them since it thinks they're always live, we trash them just
-- before the call by assigning the 'undef' value to them. The ones we
-- need are restored from the Cmm local var and the ones we don't need
-- are fine to be trashed.
getTrashStmts :: LlvmM LlvmStatements
getTrashStmts = do
  regs <- getTrashRegs
  stmts <- flip mapM regs $ \ r -> do
    reg <- getCmmReg (CmmGlobal r)
    let ty = (pLower . getVarType) reg
    return $ Store (LMLitVar $ LMUndefLit ty) reg
  return $ toOL stmts

getTrashRegs :: LlvmM [GlobalReg]
getTrashRegs = do plat <- getLlvmPlatform
                  return $ filter (callerSaves plat) (activeStgRegs plat)

-- | Get a function pointer to the CLabel specified.
--
-- This is for Haskell functions, function type is assumed, so doesn't work
-- with foreign functions.
getHsFunc :: LiveGlobalRegs -> CLabel -> LlvmM ExprData
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
    un <- runUs getUniqueUs
    return $ LMLocalVar un ty


-- | Execute an expression, assigning result to a var
doExpr :: LlvmType -> LlvmExpression -> LlvmM (LlvmVar, LlvmStatement)
doExpr ty expr = do
    v <- mkLocalVar ty
    return (v, Assignment v expr)


-- | Expand CmmRegOff
expandCmmReg :: DynFlags -> (CmmReg, Int) -> CmmExpr
expandCmmReg dflags (reg, off)
  = let width = typeWidth (cmmRegType dflags reg)
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

toIWord :: Integral a => DynFlags -> a -> LlvmVar
toIWord dflags = mkIntLit (llvmWord dflags)


-- | Error functions
panic :: String -> a
panic s = Outputable.panic $ "LlvmCodeGen.CodeGen." ++ s

pprPanic :: String -> SDoc -> a
pprPanic s d = Outputable.pprPanic ("LlvmCodeGen.CodeGen." ++ s) d


-- | Returns TBAA meta data by unique
getTBAAMeta :: Unique -> LlvmM [MetaAnnot]
getTBAAMeta u = do
    mi <- getUniqMeta u
    return [MetaAnnot tbaa (MetaNode i) | let Just i = mi]

-- | Returns TBAA meta data for given register
getTBAARegMeta :: GlobalReg -> LlvmM [MetaAnnot]
getTBAARegMeta = getTBAAMeta . getTBAA
