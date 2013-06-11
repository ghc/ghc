{-# OPTIONS -fno-warn-type-defaults #-}
-- ----------------------------------------------------------------------------
-- | Handle conversion of CmmProc to LLVM code.
--

{-# LANGUAGE GADTs #-}
module LlvmCodeGen.CodeGen ( genLlvmProc ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Base
import LlvmCodeGen.Regs

import BlockId
import CodeGen.Platform ( activeStgRegs, callerSaves )
import CLabel
import Cmm
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
import Util

import Data.List  ( partition )

type LlvmStatements = OrdList LlvmStatement

-- -----------------------------------------------------------------------------
-- | Top-level of the LLVM proc Code generator
--
genLlvmProc :: LlvmEnv -> RawCmmDecl -> UniqSM (LlvmEnv, [LlvmCmmDecl])
genLlvmProc env (CmmProc infos lbl live graph) = do
    let blocks = toBlockListEntryFirstFalseFallthrough graph
    (env', lmblocks, lmdata) <- basicBlocksCodeGen env live blocks ([], [])
    let info = mapLookup (g_entry graph) infos
        proc = CmmProc info lbl live (ListGraph lmblocks)
    return (env', proc:lmdata)

genLlvmProc _ _ = panic "genLlvmProc: case that shouldn't reach here!"

-- -----------------------------------------------------------------------------
-- * Block code generation
--

-- | Generate code for a list of blocks that make up a complete procedure.
basicBlocksCodeGen :: LlvmEnv
                   -> LiveGlobalRegs
                   -> [CmmBlock]
                   -> ( [LlvmBasicBlock] , [LlvmCmmDecl] )
                   -> UniqSM (LlvmEnv, [LlvmBasicBlock] , [LlvmCmmDecl] )
basicBlocksCodeGen env live [] (blocks0, tops0)
  = return (env, fblocks, tops)
  where
     dflags   = getDflags env
     blocks   = reverse blocks0
     tops     = reverse tops0
     (blocks', allocs) = mapAndUnzip dominateAllocs blocks
     allocs'  = concat allocs
     (BasicBlock id fstmts : rblks) = blocks'
     fblocks  = (BasicBlock id $ funPrologue dflags live ++ allocs' ++ fstmts):rblks

basicBlocksCodeGen env live (block:blocks) (lblocks, ltops)
  = do (env', lb, lt) <- basicBlockCodeGen env block
       basicBlocksCodeGen env' live blocks (lb : lblocks, reverse lt ++ ltops)


-- | Allocations need to be extracted so they can be moved to the entry
-- of a function to make sure they dominate all possible paths in the CFG.
dominateAllocs :: LlvmBasicBlock -> (LlvmBasicBlock, [LlvmStatement])
dominateAllocs (BasicBlock id stmts)
  = let (allocs, stmts') = partition isAlloc stmts
        isAlloc (Assignment _ (Alloca _ _)) = True
        isAlloc _other                      = False
    in (BasicBlock id stmts', allocs)


-- | Generate code for one block
basicBlockCodeGen :: LlvmEnv
                  -> CmmBlock
                  -> UniqSM ( LlvmEnv, LlvmBasicBlock, [LlvmCmmDecl] )
basicBlockCodeGen env block
  = do let (CmmEntry id, nodes, tail)  = blockSplit block
       let stmts = blockToList nodes
       (env', mid_instrs, top) <- stmtsToInstrs env stmts (nilOL, [])
       (env'', tail_instrs, top')  <- stmtToInstrs env' tail
       let instrs = fromOL (mid_instrs `appOL` tail_instrs)
       return (env'', BasicBlock id instrs, top' ++ top)

-- -----------------------------------------------------------------------------
-- * CmmNode code generation
--

-- A statement conversion return data.
--   * LlvmEnv: The new environment
--   * LlvmStatements: The compiled LLVM statements.
--   * LlvmCmmDecl: Any global data needed.
type StmtData = (LlvmEnv, LlvmStatements, [LlvmCmmDecl])


-- | Convert a list of CmmNode's to LlvmStatement's
stmtsToInstrs :: LlvmEnv -> [CmmNode e x] -> (LlvmStatements, [LlvmCmmDecl])
              -> UniqSM StmtData
stmtsToInstrs env [] (llvm, top)
  = return (env, llvm, top)

stmtsToInstrs env (stmt : stmts) (llvm, top)
   = do (env', instrs, tops) <- stmtToInstrs env stmt
        stmtsToInstrs env' stmts (llvm `appOL` instrs, top ++ tops)


-- | Convert a CmmNode to a list of LlvmStatement's
stmtToInstrs :: LlvmEnv -> CmmNode e x
             -> UniqSM StmtData
stmtToInstrs env stmt = case stmt of

    CmmComment _         -> return (env, nilOL, []) -- nuke comments

    CmmAssign reg src    -> genAssign env reg src
    CmmStore addr src    -> genStore env addr src

    CmmBranch id         -> genBranch env id
    CmmCondBranch arg true false -> genCondBranch env arg true false
    CmmSwitch arg ids    -> genSwitch env arg ids

    -- Foreign Call
    CmmUnsafeForeignCall target res args -> genCall env target res args

    -- Tail call
    CmmCall { cml_target = arg,
              cml_args_regs = live } -> genJump env arg live

    _ -> panic "Llvm.CodeGen.stmtToInstrs"

-- | Memory barrier instruction for LLVM >= 3.0
barrier :: LlvmEnv -> UniqSM StmtData
barrier env = do
    let s = Fence False SyncSeqCst
    return (env, unitOL s, [])

-- | Memory barrier instruction for LLVM < 3.0
oldBarrier :: LlvmEnv -> UniqSM StmtData
oldBarrier env = do
    let dflags = getDflags env
    let fname = fsLit "llvm.memory.barrier"
    let funSig = LlvmFunctionDecl fname ExternallyVisible CC_Ccc LMVoid
                    FixedArgs (tysToParams [i1, i1, i1, i1, i1]) (llvmFunAlign dflags)
    let fty = LMFunction funSig

    let fv   = LMGlobalVar fname fty (funcLinkage funSig) Nothing Nothing False
    let tops = case funLookup fname env of
                    Just _  -> []
                    Nothing -> [CmmData Data [([],[fty])]]

    let args = [lmTrue, lmTrue, lmTrue, lmTrue, lmTrue]
    let s1 = Expr $ Call StdCall fv args llvmStdFunAttrs
    let env' = funInsert fname fty env

    return (env', unitOL s1, tops)

    where
        lmTrue :: LlvmVar
        lmTrue  = mkIntLit i1 (-1)

-- | Foreign Calls
genCall :: LlvmEnv -> ForeignTarget -> [CmmFormal] -> [CmmActual]
              -> UniqSM StmtData

-- Write barrier needs to be handled specially as it is implemented as an LLVM
-- intrinsic function.
genCall env (PrimTarget MO_WriteBarrier) _ _
 | platformArch (getLlvmPlatform env) `elem` [ArchX86, ArchX86_64, ArchSPARC]
    = return (env, nilOL, [])
 | getLlvmVer env > 29 = barrier env
 | otherwise           = oldBarrier env

genCall env (PrimTarget MO_Touch) _ _
 = return (env, nilOL, [])

genCall env (PrimTarget (MO_UF_Conv w)) [dst] [e] = do
    let (env1, dstV, stmts1, top1) = getCmmReg env (CmmLocal dst)
        ty = cmmToLlvmType $ localRegType dst
        width = widthToLlvmFloat w
    castV <- mkLocalVar ty
    (env2, ve, stmts2, top2) <- exprToVar env1 e
    let stmt3 = Assignment castV $ Cast LM_Uitofp ve width
        stmt4 = Store castV dstV
        stmts = stmts1 `appOL` stmts2 `snocOL` stmt3 `snocOL` stmt4
    return (env2, stmts, top1 ++ top2)

genCall _ (PrimTarget (MO_UF_Conv _)) [_] args =
    panic $ "genCall: Too many arguments to MO_UF_Conv. " ++
    "Can only handle 1, given" ++ show (length args) ++ "."

-- Handle prefetching data
genCall env t@(PrimTarget MO_Prefetch_Data) [] args = do
    let dflags = getDflags env
        argTy = [i8Ptr, i32, i32, i32]
        funTy = \name -> LMFunction $ LlvmFunctionDecl name ExternallyVisible
                             CC_Ccc LMVoid FixedArgs (tysToParams argTy) Nothing

    let (_, arg_hints) = foreignTargetHints t
    let args_hints' = zip args arg_hints
    (env1, argVars, stmts1, top1) <- arg_vars env args_hints' ([], nilOL, [])
    (env2, fptr, stmts2, top2)    <- getFunPtr env1 funTy t
    (argVars', stmts3)            <- castVars dflags $ zip argVars argTy

    let arguments = argVars' ++ [mkIntLit i32 0, mkIntLit i32 3, mkIntLit i32 1]
        call = Expr $ Call StdCall fptr arguments []
        stmts = stmts1 `appOL` stmts2 `appOL` stmts3
                `appOL` trashStmts (getDflags env) `snocOL` call
    return (env2, stmts, top1 ++ top2)

-- Handle popcnt function specifically since GHC only really has i32 and i64
-- types and things like Word8 are backed by an i32 and just present a logical
-- i8 range. So we must handle conversions from i32 to i8 explicitly as LLVM
-- is strict about types.
genCall env t@(PrimTarget (MO_PopCnt w)) [dst] args = do
    let dflags = getDflags env
        width = widthToLlvmInt w
        dstTy = cmmToLlvmType $ localRegType dst
        funTy = \n -> LMFunction $ LlvmFunctionDecl n ExternallyVisible
                          CC_Ccc width FixedArgs (tysToParams [width]) Nothing
        (env1, dstV, stmts1, top1) = getCmmReg env (CmmLocal dst)

    let (_, arg_hints) = foreignTargetHints t
    let args_hints = zip args arg_hints
    (env2, argsV, stmts2, top2) <- arg_vars env1 args_hints ([], nilOL, [])
    (env3, fptr, stmts3, top3)  <- getFunPtr env2 funTy t
    (argsV', stmts4)            <- castVars dflags $ zip argsV [width]
    (retV, s1)                  <- doExpr width $ Call StdCall fptr argsV' []
    ([retV'], stmts5)           <- castVars dflags [(retV,dstTy)]
    let s2                       = Store retV' dstV

    let stmts = stmts1 `appOL` stmts2 `appOL` stmts3 `appOL` stmts4 `snocOL`
                s1 `appOL` stmts5 `snocOL` s2
    return (env3, stmts, top1 ++ top2 ++ top3)

-- Handle memcpy function specifically since llvm's intrinsic version takes
-- some extra parameters.
genCall env t@(PrimTarget op) [] args'
 | op == MO_Memcpy ||
   op == MO_Memset ||
   op == MO_Memmove = do
    let dflags = getDflags env
        (args, alignVal) = splitAlignVal args'
        (isVolTy, isVolVal) = if getLlvmVer env >= 28
                                 then ([i1], [mkIntLit i1 0]) else ([], [])
        argTy | op == MO_Memset = [i8Ptr, i8,    llvmWord dflags, i32] ++ isVolTy
              | otherwise       = [i8Ptr, i8Ptr, llvmWord dflags, i32] ++ isVolTy
        funTy = \name -> LMFunction $ LlvmFunctionDecl name ExternallyVisible
                             CC_Ccc LMVoid FixedArgs (tysToParams argTy) Nothing

    let (_, arg_hints) = foreignTargetHints t
    let args_hints = zip args arg_hints
    (env1, argVars, stmts1, top1) <- arg_vars env args_hints ([], nilOL, [])
    (env2, fptr, stmts2, top2)    <- getFunPtr env1 funTy t
    (argVars', stmts3)            <- castVars dflags $ zip argVars argTy

    let arguments = argVars' ++ (alignVal:isVolVal)
        call = Expr $ Call StdCall fptr arguments []
        stmts = stmts1 `appOL` stmts2 `appOL` stmts3
                `appOL` trashStmts (getDflags env) `snocOL` call
    return (env2, stmts, top1 ++ top2)
  
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
genCall env target res args = do

    let dflags = getDflags env

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
    let lmconv = case target of
            ForeignTarget _ (ForeignConvention conv _ _ _) ->
              case conv of
                 StdCallConv  -> case platformArch (getLlvmPlatform env) of
                                 ArchX86    -> CC_X86_Stdcc
                                 ArchX86_64 -> CC_X86_Stdcc
                                 _          -> CC_Ccc
                 CCallConv    -> CC_Ccc
                 CApiConv     -> CC_Ccc
                 PrimCallConv -> panic "LlvmCodeGen.CodeGen.genCall: PrimCallConv"

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



    (env1, argVars, stmts1, top1) <- arg_vars env args_hints ([], nilOL, [])
    (env2, fptr, stmts2, top2)    <- getFunPtr env1 funTy target

    let retStmt | ccTy == TailCall  = unitOL $ Return Nothing
                | never_returns     = unitOL $ Unreachable
                | otherwise         = nilOL

    let stmts = stmts1 `appOL` stmts2 `appOL` trashStmts (getDflags env)

    -- make the actual call
    case retTy of
        LMVoid -> do
            let s1 = Expr $ Call ccTy fptr argVars fnAttrs
            let allStmts = stmts `snocOL` s1 `appOL` retStmt
            return (env2, allStmts, top1 ++ top2)

        _ -> do
            (v1, s1) <- doExpr retTy $ Call ccTy fptr argVars fnAttrs
            -- get the return register
            let ret_reg [reg] = reg
                ret_reg t = panic $ "genCall: Bad number of registers! Can only handle"
                                ++ " 1, given " ++ show (length t) ++ "."
            let creg = ret_reg res
            let (env3, vreg, stmts3, top3) = getCmmReg env2 (CmmLocal creg)
            let allStmts = stmts `snocOL` s1 `appOL` stmts3
            if retTy == pLower (getVarType vreg)
                then do
                    let s2 = Store v1 vreg
                    return (env3, allStmts `snocOL` s2 `appOL` retStmt,
                                top1 ++ top2 ++ top3)
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
                    return (env3, allStmts `snocOL` s2 `snocOL` s3
                                `appOL` retStmt, top1 ++ top2 ++ top3)


-- genCallSimpleCast _ _ _ dsts _ =
--    panic ("genCallSimpleCast: " ++ show (length dsts) ++ " dsts")

-- | Create a function pointer from a target.
getFunPtr :: LlvmEnv -> (LMString -> LlvmType) -> ForeignTarget
          -> UniqSM ExprData
getFunPtr env funTy targ = case targ of
    ForeignTarget (CmmLit (CmmLabel lbl)) _ -> litCase $ strCLabel_llvm env lbl

    ForeignTarget expr _ -> do
        (env', v1, stmts, top) <- exprToVar env expr
        let fty = funTy $ fsLit "dynamic"
            cast = case getVarType v1 of
                ty | isPointer ty -> LM_Bitcast
                ty | isInt ty     -> LM_Inttoptr

                ty -> panic $ "genCall: Expr is of bad type for function"
                              ++ " call! (" ++ show (ty) ++ ")"

        (v2,s1) <- doExpr (pLift fty) $ Cast cast v1 (pLift fty)
        return (env', v2, stmts `snocOL` s1, top)

    PrimTarget mop -> litCase $ cmmPrimOpFunctions env mop

    where
        litCase name = do
            case funLookup name env of
                Just ty'@(LMFunction sig) -> do
                    -- Function in module in right form
                    let fun = LMGlobalVar name ty' (funcLinkage sig)
                                    Nothing Nothing False
                    return (env, fun, nilOL, [])

                Just ty' -> do
                    -- label in module but not function pointer, convert
                    let fty@(LMFunction sig) = funTy name
                        fun = LMGlobalVar name (pLift ty') (funcLinkage sig)
                                    Nothing Nothing False
                    (v1, s1) <- doExpr (pLift fty)
                                    $ Cast LM_Bitcast fun (pLift fty)
                    return  (env, v1, unitOL s1, [])

                Nothing -> do
                    -- label not in module, create external reference
                    let fty@(LMFunction sig) = funTy name
                        fun = LMGlobalVar name fty (funcLinkage sig)
                                    Nothing Nothing False
                        top = [CmmData Data [([],[fty])]]
                        env' = funInsert name fty env
                    return (env', fun, nilOL, top)


-- | Conversion of call arguments.
arg_vars :: LlvmEnv
         -> [(CmmActual, ForeignHint)]
         -> ([LlvmVar], LlvmStatements, [LlvmCmmDecl])
         -> UniqSM (LlvmEnv, [LlvmVar], LlvmStatements, [LlvmCmmDecl])

arg_vars env [] (vars, stmts, tops)
  = return (env, vars, stmts, tops)

arg_vars env ((e, AddrHint):rest) (vars, stmts, tops)
  = do (env', v1, stmts', top') <- exprToVar env e
       let op = case getVarType v1 of
               ty | isPointer ty -> LM_Bitcast
               ty | isInt ty     -> LM_Inttoptr

               a  -> panic $ "genCall: Can't cast llvmType to i8*! ("
                           ++ show a ++ ")"

       (v2, s1) <- doExpr i8Ptr $ Cast op v1 i8Ptr
       arg_vars env' rest (vars ++ [v2], stmts `appOL` stmts' `snocOL` s1,
                               tops ++ top')

arg_vars env ((e, _):rest) (vars, stmts, tops)
  = do (env', v1, stmts', top') <- exprToVar env e
       arg_vars env' rest (vars ++ [v1], stmts `appOL` stmts', tops ++ top')


-- | Cast a collection of LLVM variables to specific types.
castVars :: DynFlags -> [(LlvmVar, LlvmType)]
         -> UniqSM ([LlvmVar], LlvmStatements)
castVars dflags vars = do
                done <- mapM (uncurry (castVar dflags)) vars
                let (vars', stmts) = unzip done
                return (vars', toOL stmts)

-- | Cast an LLVM variable to a specific type, panicing if it can't be done.
castVar :: DynFlags -> LlvmVar -> LlvmType -> UniqSM (LlvmVar, LlvmStatement)
castVar dflags v t
            | getVarType v == t
            = return (v, Nop)

            | otherwise
            = let op = case (getVarType v, t) of
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
                                  ++ show vt ++ ") to (" ++ show t ++ ")"
              in doExpr t $ Cast op v t


-- | Decide what C function to use to implement a CallishMachOp
cmmPrimOpFunctions :: LlvmEnv -> CallishMachOp -> LMString
cmmPrimOpFunctions env mop
 = case mop of
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

    (MO_PopCnt w) -> fsLit $ "llvm.ctpop."  ++ show (widthToLlvmInt w)

    MO_Prefetch_Data -> fsLit "llvm.prefetch"

    MO_S_QuotRem {}  -> unsupported
    MO_U_QuotRem {}  -> unsupported
    MO_U_QuotRem2 {} -> unsupported
    MO_Add2 {}       -> unsupported
    MO_U_Mul2 {}     -> unsupported
    MO_WriteBarrier  -> unsupported
    MO_Touch         -> unsupported
    MO_UF_Conv _     -> unsupported

    where
        dflags = getDflags env
        intrinTy1 = (if getLlvmVer env >= 28
                       then "p0i8.p0i8." else "") ++ show (llvmWord dflags)
        intrinTy2 = (if getLlvmVer env >= 28
                       then "p0i8." else "") ++ show (llvmWord dflags)
        unsupported = panic ("cmmPrimOpFunctions: " ++ show mop
                          ++ " not supported here")

-- | Tail function calls
genJump :: LlvmEnv -> CmmExpr -> [GlobalReg] -> UniqSM StmtData

-- Call to known function
genJump env (CmmLit (CmmLabel lbl)) live = do
    (env', vf, stmts, top) <- getHsFunc env live lbl
    (stgRegs, stgStmts) <- funEpilogue env live
    let s1  = Expr $ Call TailCall vf stgRegs llvmStdFunAttrs
    let s2  = Return Nothing
    return (env', stmts `appOL` stgStmts `snocOL` s1 `snocOL` s2, top)


-- Call to unknown function / address
genJump env expr live = do
    let fty = llvmFunTy (getDflags env) live
    (env', vf, stmts, top) <- exprToVar env expr

    let cast = case getVarType vf of
         ty | isPointer ty -> LM_Bitcast
         ty | isInt ty     -> LM_Inttoptr

         ty -> panic $ "genJump: Expr is of bad type for function call! ("
                     ++ show (ty) ++ ")"

    (v1, s1) <- doExpr (pLift fty) $ Cast cast vf (pLift fty)
    (stgRegs, stgStmts) <- funEpilogue env live
    let s2 = Expr $ Call TailCall v1 stgRegs llvmStdFunAttrs
    let s3 = Return Nothing
    return (env', stmts `snocOL` s1 `appOL` stgStmts `snocOL` s2 `snocOL` s3,
            top)


-- | CmmAssign operation
--
-- We use stack allocated variables for CmmReg. The optimiser will replace
-- these with registers when possible.
genAssign :: LlvmEnv -> CmmReg -> CmmExpr -> UniqSM StmtData
genAssign env reg val = do
    let dflags = getDflags env
        (env1, vreg, stmts1, top1) = getCmmReg env reg
    (env2, vval, stmts2, top2) <- exprToVar env1 val
    let stmts = stmts1 `appOL` stmts2

    let ty = (pLower . getVarType) vreg
    case ty of
      -- Some registers are pointer types, so need to cast value to pointer
      LMPointer _ | getVarType vval == llvmWord dflags -> do
          (v, s1) <- doExpr ty $ Cast LM_Inttoptr vval ty
          let s2 = Store v vreg
          return (env2, stmts `snocOL` s1 `snocOL` s2, top1 ++ top2)

      LMVector _ _ -> do
          (v, s1) <- doExpr ty $ Cast LM_Bitcast vval ty
          let s2 = Store v vreg
          return (env2, stmts `snocOL` s1 `snocOL` s2, top1 ++ top2)

      _ -> do
          let s1 = Store vval vreg
          return (env2, stmts `snocOL` s1, top1 ++ top2)


-- | CmmStore operation
genStore :: LlvmEnv -> CmmExpr -> CmmExpr -> UniqSM StmtData

-- First we try to detect a few common cases and produce better code for
-- these then the default case. We are mostly trying to detect Cmm code
-- like I32[Sp + n] and use 'getelementptr' operations instead of the
-- generic case that uses casts and pointer arithmetic
genStore env addr@(CmmReg (CmmGlobal r)) val
    = genStore_fast env addr r 0 val

genStore env addr@(CmmRegOff (CmmGlobal r) n) val
    = genStore_fast env addr r n val

genStore env addr@(CmmMachOp (MO_Add _) [
                            (CmmReg (CmmGlobal r)),
                            (CmmLit (CmmInt n _))])
                val
    = genStore_fast env addr r (fromInteger n) val

genStore env addr@(CmmMachOp (MO_Sub _) [
                            (CmmReg (CmmGlobal r)),
                            (CmmLit (CmmInt n _))])
                val
    = genStore_fast env addr r (negate $ fromInteger n) val

-- generic case
genStore env addr val = genStore_slow env addr val [other]

-- | CmmStore operation
-- This is a special case for storing to a global register pointer
-- offset such as I32[Sp+8].
genStore_fast :: LlvmEnv -> CmmExpr -> GlobalReg -> Int -> CmmExpr
              -> UniqSM StmtData
genStore_fast env addr r n val
  = let dflags = getDflags env
        gr   = lmGlobalRegVar (getDflags env) r
        meta = [getTBAA r]
        grt  = (pLower . getVarType) gr
        (ix,rem) = n `divMod` ((llvmWidthInBits dflags . pLower) grt  `div` 8)
    in case isPointer grt && rem == 0 of
            True -> do
                (env', vval,  stmts, top) <- exprToVar env val
                (gv,  s1) <- doExpr grt $ Load gr
                (ptr, s2) <- doExpr grt $ GetElemPtr True gv [toI32 ix]
                -- We might need a different pointer type, so check
                case pLower grt == getVarType vval of
                     -- were fine
                     True  -> do
                         let s3 = MetaStmt meta $ Store vval ptr
                         return (env',  stmts `snocOL` s1 `snocOL` s2
                                 `snocOL` s3, top)

                     -- cast to pointer type needed
                     False -> do
                         let ty = (pLift . getVarType) vval
                         (ptr', s3) <- doExpr ty $ Cast LM_Bitcast ptr ty
                         let s4 = MetaStmt meta $ Store vval ptr'
                         return (env',  stmts `snocOL` s1 `snocOL` s2
                                 `snocOL` s3 `snocOL` s4, top)

            -- If its a bit type then we use the slow method since
            -- we can't avoid casting anyway.
            False -> genStore_slow env addr val meta


-- | CmmStore operation
-- Generic case. Uses casts and pointer arithmetic if needed.
genStore_slow :: LlvmEnv -> CmmExpr -> CmmExpr -> [MetaData] -> UniqSM StmtData
genStore_slow env addr val meta = do
    (env1, vaddr, stmts1, top1) <- exprToVar env addr
    (env2, vval,  stmts2, top2) <- exprToVar env1 val

    let stmts = stmts1 `appOL` stmts2
    case getVarType vaddr of
        -- sometimes we need to cast an int to a pointer before storing
        LMPointer ty@(LMPointer _) | getVarType vval == llvmWord dflags -> do
            (v, s1) <- doExpr ty $ Cast LM_Inttoptr vval ty
            let s2 = MetaStmt meta $ Store v vaddr
            return (env2, stmts `snocOL` s1 `snocOL` s2, top1 ++ top2)

        LMPointer _ -> do
            let s1 = MetaStmt meta $ Store vval vaddr
            return (env2, stmts `snocOL` s1, top1 ++ top2)

        i@(LMInt _) | i == llvmWord dflags -> do
            let vty = pLift $ getVarType vval
            (vptr, s1) <- doExpr vty $ Cast LM_Inttoptr vaddr vty
            let s2 = MetaStmt meta $ Store vval vptr
            return (env2, stmts `snocOL` s1 `snocOL` s2, top1 ++ top2)

        other ->
            pprPanic "genStore: ptr not right type!"
                    (PprCmm.pprExpr addr <+> text (
                        "Size of Ptr: " ++ show (llvmPtrBits dflags) ++
                        ", Size of var: " ++ show (llvmWidthInBits dflags other) ++
                        ", Var: " ++ show vaddr))
    where dflags = getDflags env


-- | Unconditional branch
genBranch :: LlvmEnv -> BlockId -> UniqSM StmtData
genBranch env id =
    let label = blockIdToLlvm id
    in return (env, unitOL $ Branch label, [])


-- | Conditional branch
genCondBranch :: LlvmEnv -> CmmExpr -> BlockId -> BlockId -> UniqSM StmtData
genCondBranch env cond idT idF = do
    let labelT = blockIdToLlvm idT
    let labelF = blockIdToLlvm idF
    -- See Note [Literals and branch conditions].
    (env', vc, stmts, top) <- exprToVarOpt env i1Option cond
    if getVarType vc == i1
        then do
            let s1 = BranchIf vc labelT labelF
            return $ (env', stmts `snocOL` s1, top)
        else
            panic $ "genCondBranch: Cond expr not bool! (" ++ show vc ++ ")"

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
genSwitch :: LlvmEnv -> CmmExpr -> [Maybe BlockId] -> UniqSM StmtData
genSwitch env cond maybe_ids = do
    (env', vc, stmts, top) <- exprToVar env cond
    let ty = getVarType vc

    let pairs = [ (ix, id) | (ix,Just id) <- zip [0..] maybe_ids ]
    let labels = map (\(ix, b) -> (mkIntLit ty ix, blockIdToLlvm b)) pairs
    -- out of range is undefied, so lets just branch to first label
    let (_, defLbl) = head labels

    let s1 = Switch vc defLbl labels
    return $ (env', stmts `snocOL` s1, top)


-- -----------------------------------------------------------------------------
-- * CmmExpr code generation
--

-- | An expression conversion return data:
--   * LlvmEnv: The new enviornment
--   * LlvmVar: The var holding the result of the expression
--   * LlvmStatements: Any statements needed to evaluate the expression
--   * LlvmCmmDecl: Any global data needed for this expression
type ExprData = (LlvmEnv, LlvmVar, LlvmStatements, [LlvmCmmDecl])

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
exprToVar :: LlvmEnv -> CmmExpr -> UniqSM ExprData
exprToVar env = exprToVarOpt env wordOption

exprToVarOpt :: LlvmEnv -> EOption -> CmmExpr -> UniqSM ExprData
exprToVarOpt env opt e = case e of

    CmmLit lit
        -> genLit opt env lit

    CmmLoad e' ty
        -> genLoad env e' ty

    -- Cmmreg in expression is the value, so must load. If you want actual
    -- reg pointer, call getCmmReg directly.
    CmmReg r -> do
        let (env', vreg, stmts, top) = getCmmReg env r
        (v1, s1) <- doExpr (pLower $ getVarType vreg) $ Load vreg
        case (isPointer . getVarType) v1 of
             True  -> do
                 -- Cmm wants the value, so pointer types must be cast to ints
                 (v2, s2) <- doExpr (llvmWord dflags) $ Cast LM_Ptrtoint v1 (llvmWord dflags)
                 return (env', v2, stmts `snocOL` s1 `snocOL` s2, top)

             False -> return (env', v1, stmts `snocOL` s1, top)

    CmmMachOp op exprs
        -> genMachOp env opt op exprs

    CmmRegOff r i
        -> exprToVar env $ expandCmmReg dflags (r, i)

    CmmStackSlot _ _
        -> panic "exprToVar: CmmStackSlot not supported!"

  where dflags = getDflags env

-- | Handle CmmMachOp expressions
genMachOp :: LlvmEnv -> EOption -> MachOp -> [CmmExpr] -> UniqSM ExprData

-- Unary Machop
genMachOp env _ op [x] = case op of

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
 
    MO_VF_Insert  _ _ -> panicOp
    MO_VF_Extract _ _ -> panicOp

    MO_VF_Add     _ _ -> panicOp
    MO_VF_Sub     _ _ -> panicOp
    MO_VF_Mul     _ _ -> panicOp
    MO_VF_Quot    _ _ -> panicOp

    where
        dflags = getDflags env

        negate ty v2 negOp = do
            (env', vx, stmts, top) <- exprToVar env x
            (v1, s1) <- doExpr ty $ LlvmOp negOp v2 vx
            return (env', v1, stmts `snocOL` s1, top)

        negateVec ty v2 negOp = do
            (env', vx, stmts1, top) <- exprToVar env x
            ([vx'], stmts2) <- castVars dflags [(vx, ty)]
            (v1, s1) <- doExpr ty $ LlvmOp negOp v2 vx'
            return (env', v1, stmts1 `appOL` stmts2 `snocOL` s1, top)

        fiConv ty convOp = do
            (env', vx, stmts, top) <- exprToVar env x
            (v1, s1) <- doExpr ty $ Cast convOp vx ty
            return (env', v1, stmts `snocOL` s1, top)

        sameConv from ty reduce expand = do
            x'@(env', vx, stmts, top) <- exprToVar env x
            let sameConv' op = do
                (v1, s1) <- doExpr ty $ Cast op vx ty
                return (env', v1, stmts `snocOL` s1, top)
            let toWidth = llvmWidthInBits dflags ty
            -- LLVM doesn't like trying to convert to same width, so
            -- need to check for that as we do get Cmm code doing it.
            case widthInBits from  of
                 w | w < toWidth -> sameConv' expand
                 w | w > toWidth -> sameConv' reduce
                 _w              -> return x'
        
        panicOp = panic $ "LLVM.CodeGen.genMachOp: non unary op encourntered"
                       ++ "with one argument! (" ++ show op ++ ")"

-- Handle GlobalRegs pointers
genMachOp env opt o@(MO_Add _) e@[(CmmReg (CmmGlobal r)), (CmmLit (CmmInt n _))]
    = genMachOp_fast env opt o r (fromInteger n) e

genMachOp env opt o@(MO_Sub _) e@[(CmmReg (CmmGlobal r)), (CmmLit (CmmInt n _))]
    = genMachOp_fast env opt o r (negate . fromInteger $ n) e

-- Generic case
genMachOp env opt op e = genMachOp_slow env opt op e


-- | Handle CmmMachOp expressions
-- This is a specialised method that handles Global register manipulations like
-- 'Sp - 16', using the getelementptr instruction.
genMachOp_fast :: LlvmEnv -> EOption -> MachOp -> GlobalReg -> Int -> [CmmExpr]
               -> UniqSM ExprData
genMachOp_fast env opt op r n e
  = let dflags = getDflags env
        gr   = lmGlobalRegVar dflags r
        grt  = (pLower . getVarType) gr
        (ix,rem) = n `divMod` ((llvmWidthInBits dflags . pLower) grt  `div` 8)
    in case isPointer grt && rem == 0 of
            True -> do
                (gv,  s1) <- doExpr grt $ Load gr
                (ptr, s2) <- doExpr grt $ GetElemPtr True gv [toI32 ix]
                (var, s3) <- doExpr (llvmWord dflags) $ Cast LM_Ptrtoint ptr (llvmWord dflags)
                return (env, var, unitOL s1 `snocOL` s2 `snocOL` s3, [])

            False -> genMachOp_slow env opt op e


-- | Handle CmmMachOp expressions
-- This handles all the cases not handle by the specialised genMachOp_fast.
genMachOp_slow :: LlvmEnv -> EOption -> MachOp -> [CmmExpr] -> UniqSM ExprData

-- Element extraction
genMachOp_slow env _ (MO_V_Extract l w) [val, idx] = do
    (env1, vval, stmts1, top1) <- exprToVar env  val
    (env2, vidx, stmts2, top2) <- exprToVar env1 idx
    ([vval'], stmts3)          <- castVars dflags [(vval, LMVector l ty)]
    (v1, s1)                   <- doExpr ty $ Extract vval' vidx
    return (env2, v1, stmts1 `appOL` stmts2 `appOL` stmts3 `snocOL` s1, top1 ++ top2)
  where
    dflags = getDflags env
    ty = widthToLlvmInt w

genMachOp_slow env _ (MO_VF_Extract l w) [val, idx] = do
    (env1, vval, stmts1, top1) <- exprToVar env  val
    (env2, vidx, stmts2, top2) <- exprToVar env1 idx
    ([vval'], stmts3)          <- castVars dflags [(vval, LMVector l ty)]
    (v1, s1)                   <- doExpr ty $ Extract vval' vidx
    return (env2, v1, stmts1 `appOL` stmts2 `appOL` stmts3 `snocOL` s1, top1 ++ top2)
  where
    dflags = getDflags env
    ty = widthToLlvmFloat w

-- Element insertion
genMachOp_slow env _ (MO_V_Insert l w) [val, elt, idx] = do
    (env1, vval, stmts1, top1) <- exprToVar env  val
    (env2, velt, stmts2, top2) <- exprToVar env1 elt
    (env3, vidx, stmts3, top3) <- exprToVar env2 idx
    ([vval'], stmts4)          <- castVars dflags [(vval, ty)]
    (v1, s1)                   <- doExpr ty $ Insert vval' velt vidx
    return (env3, v1, stmts1 `appOL` stmts2 `appOL` stmts3 `appOL` stmts4 `snocOL` s1,
            top1 ++ top2 ++ top3)
  where
    dflags = getDflags env
    ty = LMVector l (widthToLlvmInt w)

genMachOp_slow env _ (MO_VF_Insert l w) [val, elt, idx] = do
    (env1, vval, stmts1, top1) <- exprToVar env  val
    (env2, velt, stmts2, top2) <- exprToVar env1 elt
    (env3, vidx, stmts3, top3) <- exprToVar env2 idx
    ([vval'], stmts4)          <- castVars dflags [(vval, ty)]
    (v1, s1)                   <- doExpr ty $ Insert vval' velt vidx
    return (env3, v1, stmts1 `appOL` stmts2 `appOL` stmts3 `appOL` stmts4 `snocOL` s1,
            top1 ++ top2 ++ top3)
  where
    dflags = getDflags env
    ty = LMVector l (widthToLlvmFloat w)
    
-- Binary MachOp
genMachOp_slow env opt op [x, y] = case op of

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
        dflags = getDflags env

        binLlvmOp ty binOp = do
            (env1, vx, stmts1, top1) <- exprToVar env x
            (env2, vy, stmts2, top2) <- exprToVar env1 y
            if getVarType vx == getVarType vy
                then do
                    (v1, s1) <- doExpr (ty vx) $ binOp vx vy
                    return (env2, v1, stmts1 `appOL` stmts2 `snocOL` s1,
                            top1 ++ top2)

                else do
                    -- Error. Continue anyway so we can debug the generated ll file.
                    let dflags = getDflags env
                        style = mkCodeStyle CStyle
                        toString doc = renderWithStyle dflags doc style
                        cmmToStr = (lines . toString . PprCmm.pprExpr)
                    let dx = Comment $ map fsLit $ cmmToStr x
                    let dy = Comment $ map fsLit $ cmmToStr y
                    (v1, s1) <- doExpr (ty vx) $ binOp vx vy
                    let allStmts = stmts1 `appOL` stmts2 `snocOL` dx
                                    `snocOL` dy `snocOL` s1
                    return (env2, v1, allStmts, top1 ++ top2)

        binCastLlvmOp ty binOp = do
            (env1, vx, stmts1, top1) <- exprToVar env x
            (env2, vy, stmts2, top2) <- exprToVar env1 y
            ([vx', vy'], stmts3) <- castVars dflags [(vx, ty), (vy, ty)]
            (v1, s1) <- doExpr ty $ binOp vx' vy'
            return (env2, v1, stmts1 `appOL` stmts2 `appOL` stmts3 `snocOL` s1,
                    top1 ++ top2)

        -- | Need to use EOption here as Cmm expects word size results from
        -- comparisons while LLVM return i1. Need to extend to llvmWord type
        -- if expected. See Note [Literals and branch conditions].
        genBinComp opt cmp = do
            ed@(env', v1, stmts, top) <- binLlvmOp (\_ -> i1) (Compare cmp)
            if getVarType v1 == i1
                then case i1Expected opt of
                    True  -> return ed
                    False -> do
                        let w_ = llvmWord dflags
                        (v2, s1) <- doExpr w_ $ Cast LM_Zext v1 w_
                        return (env', v2, stmts `snocOL` s1, top)
                else
                    panic $ "genBinComp: Compare returned type other then i1! "
                        ++ (show $ getVarType v1)

        genBinMach op = binLlvmOp getVarType (LlvmOp op)

        genCastBinMach ty op = binCastLlvmOp ty (LlvmOp op)

        -- | Detect if overflow will occur in signed multiply of the two
        -- CmmExpr's. This is the LLVM assembly equivalent of the NCG
        -- implementation. Its much longer due to type information/safety.
        -- This should actually compile to only about 3 asm instructions.
        isSMulOK :: Width -> CmmExpr -> CmmExpr -> UniqSM ExprData
        isSMulOK _ x y = do
            (env1, vx, stmts1, top1) <- exprToVar env x
            (env2, vy, stmts2, top2) <- exprToVar env1 y

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
                    return (env2, dst, stmts1 `appOL` stmts2 `appOL` stmts,
                        top1 ++ top2)

                else
                    panic $ "isSMulOK: Not bit type! (" ++ show word ++ ")"

        panicOp = panic $ "LLVM.CodeGen.genMachOp_slow: unary op encourntered"
                       ++ "with two arguments! (" ++ show op ++ ")"

-- More then two expression, invalid!
genMachOp_slow _ _ _ _ = panic "genMachOp: More then 2 expressions in MachOp!"


-- | Handle CmmLoad expression.
genLoad :: LlvmEnv -> CmmExpr -> CmmType -> UniqSM ExprData

-- First we try to detect a few common cases and produce better code for
-- these then the default case. We are mostly trying to detect Cmm code
-- like I32[Sp + n] and use 'getelementptr' operations instead of the
-- generic case that uses casts and pointer arithmetic
genLoad env e@(CmmReg (CmmGlobal r)) ty
    = genLoad_fast env e r 0 ty

genLoad env e@(CmmRegOff (CmmGlobal r) n) ty
    = genLoad_fast env e r n ty

genLoad env e@(CmmMachOp (MO_Add _) [
                            (CmmReg (CmmGlobal r)),
                            (CmmLit (CmmInt n _))])
                ty
    = genLoad_fast env e r (fromInteger n) ty

genLoad env e@(CmmMachOp (MO_Sub _) [
                            (CmmReg (CmmGlobal r)),
                            (CmmLit (CmmInt n _))])
                ty
    = genLoad_fast env e r (negate $ fromInteger n) ty

-- generic case
genLoad env e ty = genLoad_slow env e ty [other]

-- | Handle CmmLoad expression.
-- This is a special case for loading from a global register pointer
-- offset such as I32[Sp+8].
genLoad_fast :: LlvmEnv -> CmmExpr -> GlobalReg -> Int -> CmmType
                -> UniqSM ExprData
genLoad_fast env e r n ty =
    let dflags = getDflags env
        gr   = lmGlobalRegVar dflags r
        meta = [getTBAA r]
        grt  = (pLower . getVarType) gr
        ty'  = cmmToLlvmType ty
        (ix,rem) = n `divMod` ((llvmWidthInBits dflags . pLower) grt  `div` 8)
    in case isPointer grt && rem == 0 of
            True  -> do
                (gv,  s1) <- doExpr grt $ Load gr
                (ptr, s2) <- doExpr grt $ GetElemPtr True gv [toI32 ix]
                -- We might need a different pointer type, so check
                case grt == ty' of
                     -- were fine
                     True -> do
                         (var, s3) <- doExpr ty' (MetaExpr meta $ Load ptr)
                         return (env, var, unitOL s1 `snocOL` s2 `snocOL` s3,
                                     [])

                     -- cast to pointer type needed
                     False -> do
                         let pty = pLift ty'
                         (ptr', s3) <- doExpr pty $ Cast LM_Bitcast ptr pty
                         (var, s4) <- doExpr ty' (MetaExpr meta $ Load ptr')
                         return (env, var, unitOL s1 `snocOL` s2 `snocOL` s3
                                    `snocOL` s4, [])

            -- If its a bit type then we use the slow method since
            -- we can't avoid casting anyway.
            False -> genLoad_slow env e ty meta


-- | Handle Cmm load expression.
-- Generic case. Uses casts and pointer arithmetic if needed.
genLoad_slow :: LlvmEnv -> CmmExpr -> CmmType -> [MetaData] -> UniqSM ExprData
genLoad_slow env e ty meta = do
    (env', iptr, stmts, tops) <- exprToVar env e
    case getVarType iptr of
         LMPointer _ -> do
                    (dvar, load) <- doExpr (cmmToLlvmType ty)
                                           (MetaExpr meta $ Load iptr)
                    return (env', dvar, stmts `snocOL` load, tops)

         i@(LMInt _) | i == llvmWord dflags -> do
                    let pty = LMPointer $ cmmToLlvmType ty
                    (ptr, cast)  <- doExpr pty $ Cast LM_Inttoptr iptr pty
                    (dvar, load) <- doExpr (cmmToLlvmType ty)
                                           (MetaExpr meta $ Load ptr)
                    return (env', dvar, stmts `snocOL` cast `snocOL` load, tops)

         other -> pprPanic "exprToVar: CmmLoad expression is not right type!"
                        (PprCmm.pprExpr e <+> text (
                            "Size of Ptr: " ++ show (llvmPtrBits dflags) ++
                            ", Size of var: " ++ show (llvmWidthInBits dflags other) ++
                            ", Var: " ++ show iptr))
    where dflags = getDflags env

-- | Handle CmmReg expression
--
-- We allocate CmmReg on the stack. This avoids having to map a CmmReg to an
-- equivalent SSA form and avoids having to deal with Phi node insertion.
-- This is also the approach recommended by LLVM developers.
getCmmReg :: LlvmEnv -> CmmReg -> ExprData
getCmmReg env r@(CmmLocal (LocalReg un _))
  = let exists = varLookup un env
        (newv, stmts) = allocReg r
        nenv = varInsert un (pLower $ getVarType newv) env
    in case exists of
            Just ety -> (env, (LMLocalVar un $ pLift ety), nilOL, [])
            Nothing  -> (nenv, newv, stmts, [])

getCmmReg env (CmmGlobal g) = (env, lmGlobalRegVar (getDflags env) g, nilOL, [])


-- | Allocate a CmmReg on the stack
allocReg :: CmmReg -> (LlvmVar, LlvmStatements)
allocReg (CmmLocal (LocalReg un ty))
  = let ty' = cmmToLlvmType ty
        var = LMLocalVar un (LMPointer ty')
        alc = Alloca ty' 1
    in (var, unitOL $ Assignment var alc)

allocReg _ = panic $ "allocReg: Global reg encountered! Global registers should"
                    ++ " have been handled elsewhere!"


-- | Generate code for a literal
genLit :: EOption -> LlvmEnv -> CmmLit -> UniqSM ExprData
genLit opt env (CmmInt i w)
  -- See Note [Literals and branch conditions].
  = let width | i1Expected opt = i1
              | otherwise      = LMInt (widthInBits w)
        -- comm  = Comment [ fsLit $ "EOption: " ++ show opt
        --                 , fsLit $ "Width  : " ++ show w
        --                 , fsLit $ "Width' : " ++ show (widthInBits w)
        --                 ]
    in return (env, mkIntLit width i, nilOL, [])

genLit _ env (CmmFloat r w)
  = return (env, LMLitVar $ LMFloatLit (fromRational r) (widthToLlvmFloat w),
              nilOL, [])
 
genLit opt env (CmmVec ls)
  = do llvmLits <- mapM toLlvmLit ls
       return (env, LMLitVar $ LMVectorLit llvmLits, nilOL, [])
  where
    toLlvmLit :: CmmLit -> UniqSM LlvmLit
    toLlvmLit lit = do
        (_, llvmLitVar, _, _) <- genLit opt env lit
        case llvmLitVar of
          LMLitVar llvmLit -> return llvmLit
          _ -> panic "genLit"

genLit _ env cmm@(CmmLabel l)
  = let dflags = getDflags env
        label = strCLabel_llvm env l
        ty = funLookup label env
        lmty = cmmToLlvmType $ cmmLitType dflags cmm
    in case ty of
            -- Make generic external label definition and then pointer to it
            Nothing -> do
                let glob@(var, _) = genStringLabelRef dflags label
                let ldata = [CmmData Data [([glob], [])]]
                let env' = funInsert label (pLower $ getVarType var) env
                (v1, s1) <- doExpr lmty $ Cast LM_Ptrtoint var (llvmWord dflags)
                return (env', v1, unitOL s1, ldata)

            -- Referenced data exists in this module, retrieve type and make
            -- pointer to it.
            Just ty' -> do
                let var = LMGlobalVar label (LMPointer ty')
                            ExternallyVisible Nothing Nothing False
                (v1, s1) <- doExpr lmty $ Cast LM_Ptrtoint var (llvmWord dflags)
                return (env, v1, unitOL s1, [])

genLit opt env (CmmLabelOff label off) = do
    let dflags = getDflags env
    (env', vlbl, stmts, stat) <- genLit opt env (CmmLabel label)
    let voff = toIWord dflags off
    (v1, s1) <- doExpr (getVarType vlbl) $ LlvmOp LM_MO_Add vlbl voff
    return (env', v1, stmts `snocOL` s1, stat)

genLit opt env (CmmLabelDiffOff l1 l2 off) = do
    let dflags = getDflags env
    (env1, vl1, stmts1, stat1) <- genLit opt env (CmmLabel l1)
    (env2, vl2, stmts2, stat2) <- genLit opt env1 (CmmLabel l2)
    let voff = toIWord dflags off
    let ty1 = getVarType vl1
    let ty2 = getVarType vl2
    if (isInt ty1) && (isInt ty2)
       && (llvmWidthInBits dflags ty1 == llvmWidthInBits dflags ty2)

       then do
            (v1, s1) <- doExpr (getVarType vl1) $ LlvmOp LM_MO_Sub vl1 vl2
            (v2, s2) <- doExpr (getVarType v1 ) $ LlvmOp LM_MO_Add v1 voff
            return (env2, v2, stmts1 `appOL` stmts2 `snocOL` s1 `snocOL` s2,
                        stat1 ++ stat2)

        else
            panic "genLit: CmmLabelDiffOff encountered with different label ty!"

genLit opt env (CmmBlock b)
  = genLit opt env (CmmLabel $ infoTblLbl b)

genLit _ _ CmmHighStackMark
  = panic "genStaticLit - CmmHighStackMark unsupported!"


-- -----------------------------------------------------------------------------
-- * Misc
--

-- | Function prologue. Load STG arguments into variables for function.
funPrologue :: DynFlags -> LiveGlobalRegs -> [LlvmStatement]
funPrologue dflags live = concat $ map getReg $ activeStgRegs platform
    where platform = targetPlatform dflags
          isLive r = r `elem` alwaysLive || r `elem` live
          getReg rr =
            let reg   = lmGlobalRegVar dflags rr
                arg   = lmGlobalRegArg dflags rr
                ty    = (pLower . getVarType) reg
                trash = LMLitVar $ LMUndefLit ty
                alloc = Assignment reg $ Alloca (pLower $ getVarType reg) 1
            in 
              if isLive rr
              then [alloc, Store arg reg]
              else [alloc, Store trash reg]


-- | Function epilogue. Load STG variables to use as argument for call.
-- STG Liveness optimisation done here.
funEpilogue :: LlvmEnv -> LiveGlobalRegs -> UniqSM ([LlvmVar], LlvmStatements)

-- Have information and liveness optimisation is enabled
funEpilogue env live = do
    loads <- mapM loadExpr (filter isPassed (activeStgRegs platform))
    let (vars, stmts) = unzip loads
    return (vars, concatOL stmts)
  where
    dflags = getDflags env
    platform = targetPlatform dflags
    isLive r = r `elem` alwaysLive || r `elem` live
    isPassed r = not (isSSE r) || isLive r
    isSSE (FloatReg _)  = True
    isSSE (DoubleReg _) = True
    isSSE (XmmReg _)    = True
    isSSE _             = False
    loadExpr r | isLive r = do
        let reg  = lmGlobalRegVar dflags r
        (v,s) <- doExpr (pLower $ getVarType reg) $ Load reg
        return (v, unitOL s)
    loadExpr r = do
        let ty = (pLower . getVarType $ lmGlobalRegVar dflags r)
        return (LMLitVar $ LMUndefLit ty, unitOL Nop)


-- | A serries of statements to trash all the STG registers.
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
trashStmts :: DynFlags -> LlvmStatements
trashStmts dflags = concatOL $ map trashReg $ activeStgRegs platform
    where platform = targetPlatform dflags
          trashReg r =
            let reg   = lmGlobalRegVar dflags r
                ty    = (pLower . getVarType) reg
                trash = unitOL $ Store (LMLitVar $ LMUndefLit ty) reg
            in case callerSaves (targetPlatform dflags) r of
                      True  -> trash
                      False -> nilOL


-- | Get a function pointer to the CLabel specified.
--
-- This is for Haskell functions, function type is assumed, so doesn't work
-- with foreign functions.
getHsFunc :: LlvmEnv -> LiveGlobalRegs -> CLabel -> UniqSM ExprData
getHsFunc env live lbl
  = let dflags = getDflags env
        fn = strCLabel_llvm env lbl
        ty    = funLookup fn env
    in case ty of
        -- Function in module in right form
        Just ty'@(LMFunction sig) -> do
            let fun = LMGlobalVar fn ty' (funcLinkage sig) Nothing Nothing False
            return (env, fun, nilOL, [])

        -- label in module but not function pointer, convert
        Just ty' -> do
            let fun = LMGlobalVar fn (pLift ty') ExternallyVisible
                            Nothing Nothing False
            (v1, s1) <- doExpr (pLift (llvmFunTy dflags live)) $
                            Cast LM_Bitcast fun (pLift (llvmFunTy dflags live))
            return (env, v1, unitOL s1, [])

        -- label not in module, create external reference
        Nothing  -> do
            let ty' = LMFunction $ llvmFunSig env live lbl ExternallyVisible
            let fun = LMGlobalVar fn ty' ExternallyVisible Nothing Nothing False
            let top = CmmData Data [([],[ty'])]
            let env' = funInsert fn ty' env
            return (env', fun, nilOL, [top])


-- | Create a new local var
mkLocalVar :: LlvmType -> UniqSM LlvmVar
mkLocalVar ty = do
    un <- getUniqueUs
    return $ LMLocalVar un ty


-- | Execute an expression, assigning result to a var
doExpr :: LlvmType -> LlvmExpression -> UniqSM (LlvmVar, LlvmStatement)
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

