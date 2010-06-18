-- -----------------------------------------------------------------------------
-- | This is the top-level module in the LLVM code generator.
--

module LlvmCodeGen ( llvmCodeGen ) where

#include "HsVersions.h"

import Llvm

import LlvmCodeGen.Base
import LlvmCodeGen.CodeGen
import LlvmCodeGen.Data
import LlvmCodeGen.Ppr

import CLabel
import Cmm
import CgUtils ( fixStgRegisters )
import PprCmm

import BufWrite
import DynFlags
import ErrUtils
import FastString
import Outputable
import qualified Pretty as Prt
import UniqSupply
import Util

import System.IO

-- -----------------------------------------------------------------------------
-- | Top-level of the llvm codegen
--
llvmCodeGen :: DynFlags -> Handle -> UniqSupply -> [RawCmm] -> IO ()
llvmCodeGen dflags h us cmms
  = do
      let cmm = concat $ map (\(Cmm top) -> top) cmms

      bufh <- newBufHandle h

      Prt.bufLeftRender bufh $ pprLlvmHeader

      env <- cmmDataLlvmGens dflags bufh cmm
      cmmProcLlvmGens dflags bufh us env cmm 1 []

      bFlush bufh

      return  ()


-- -----------------------------------------------------------------------------
-- | Do llvm code generation on all these cmms data sections.
--
cmmDataLlvmGens
      :: DynFlags
      -> BufHandle
      -> [RawCmmTop]
      -> IO ( LlvmEnv )

cmmDataLlvmGens _ _ []
  = return ( initLlvmEnv )

cmmDataLlvmGens dflags h cmm =
    let exData (CmmData s d) = [(s,d)]
        exData  _            = []

        exProclbl (CmmProc i l _ _)
                | not (null i) = [strCLabel_llvm $ entryLblToInfoLbl l]
        exProclbl (CmmProc _ l _ _) | otherwise = [strCLabel_llvm l]
        exProclbl _                             = []

        cproc = concat $ map exProclbl cmm
        cdata = concat $ map exData cmm
        env = foldl (\e l -> funInsert l llvmFunTy e) initLlvmEnv cproc
    in cmmDataLlvmGens' dflags h env cdata []

cmmDataLlvmGens'
      :: DynFlags
      -> BufHandle
      -> LlvmEnv
      -> [(Section, [CmmStatic])]
      -> [LlvmUnresData]
      -> IO ( LlvmEnv )

cmmDataLlvmGens' dflags h env [] lmdata
    = do
        let (env', lmdata') = resolveLlvmDatas dflags env lmdata []
        let lmdoc = Prt.vcat $ map (pprLlvmData dflags) lmdata'

        dumpIfSet_dyn dflags Opt_D_dump_llvm "LLVM Code" $ docToSDoc lmdoc

        Prt.bufLeftRender h lmdoc
        return env'

cmmDataLlvmGens' dflags h env (cmm:cmms) lmdata
    = do
        let lmdata'@(l, ty, _) = genLlvmData dflags cmm
        let env' = funInsert (strCLabel_llvm l) ty env
        cmmDataLlvmGens' dflags h env' cmms (lmdata ++ [lmdata'])


-- -----------------------------------------------------------------------------
-- | Do llvm code generation on all these cmms procs.
--
cmmProcLlvmGens
      :: DynFlags
      -> BufHandle
      -> UniqSupply
      -> LlvmEnv
      -> [RawCmmTop]
      -> Int          -- ^ count, used for generating unique subsections
      -> [LlvmVar]    -- ^ info tables that need to be marked as 'used'
      -> IO ()

cmmProcLlvmGens _ _ _ _ [] _ []
  = return ()

cmmProcLlvmGens dflags h _ _ [] _ ivars
  = do
      let cast x = LMBitc (LMStaticPointer (pVarLift x)) i8Ptr
      let ty = (LMArray (length ivars) i8Ptr)
      let usedArray = LMStaticArray (map cast ivars) ty
      let lmUsed = (LMGlobalVar (fsLit "llvm.used") ty Appending
                      (Just $ fsLit "llvm.metadata") Nothing, Just usedArray)
      Prt.bufLeftRender h $ pprLlvmData dflags ([lmUsed], [])

cmmProcLlvmGens dflags h us env (cmm : cmms) count ivars
  = do
      (us', env', llvm) <- cmmLlvmGen dflags us (clearVars env) cmm

      let (docs, ivar) = mapAndUnzip (pprLlvmCmmTop dflags env' count) llvm
      Prt.bufLeftRender h $ Prt.vcat docs

      cmmProcLlvmGens dflags h us' env' cmms (count + 2) (concat ivar ++ ivars)


-- | Complete llvm code generation phase for a single top-level chunk of Cmm.
cmmLlvmGen
      :: DynFlags
      -> UniqSupply
      -> LlvmEnv
      -> RawCmmTop              -- ^ the cmm to generate code for
      -> IO ( UniqSupply,
              LlvmEnv,
              [LlvmCmmTop] )   -- llvm code

cmmLlvmGen dflags us env cmm
  = do
    -- rewrite assignments to global regs
    let fixed_cmm = fixStgRegisters cmm

    dumpIfSet_dyn dflags Opt_D_dump_opt_cmm "Optimised Cmm"
        (pprCmm $ Cmm [fixed_cmm])

    -- generate llvm code from cmm
    let ((env', llvmBC), usGen) = initUs us $ genLlvmCode dflags env fixed_cmm

    dumpIfSet_dyn dflags Opt_D_dump_llvm "LLVM Code"
        (vcat $ map (docToSDoc . fst . pprLlvmCmmTop dflags env' 0) llvmBC)

    return (usGen, env', llvmBC)


-- -----------------------------------------------------------------------------
-- | Instruction selection
--
genLlvmCode
    :: DynFlags
    -> LlvmEnv
    -> RawCmmTop
    -> UniqSM (LlvmEnv, [LlvmCmmTop])

genLlvmCode _ env (CmmData _ _)
    = return (env, [])

genLlvmCode _ env (CmmProc _ _ _ (ListGraph []))
    = return (env, [])

genLlvmCode _ env cp@(CmmProc _ _ _ _)
    = genLlvmProc env cp

