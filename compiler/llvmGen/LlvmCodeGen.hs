-- -----------------------------------------------------------------------------
-- | This is the top-level module in the LLVM code generator.
--

module LlvmCodeGen ( llvmCodeGen ) where

#include "HsVersions.h"

import LlvmCodeGen.Base
import LlvmCodeGen.CodeGen
import LlvmCodeGen.Data
import LlvmCodeGen.Ppr

import Cmm
import CgUtils ( fixStgRegisters )
import PprCmm

import BufWrite
import DynFlags
import ErrUtils
import Outputable
import qualified Pretty as Prt
import UniqSupply

import System.IO

-- -----------------------------------------------------------------------------
-- | Top-level of the llvm codegen
--
llvmCodeGen :: DynFlags -> Handle -> UniqSupply -> [RawCmm] -> IO ()
llvmCodeGen dflags h us cmms
  = do
      let cmm = concat $ map extractRawCmm cmms

      bufh <- newBufHandle h

      Prt.bufLeftRender bufh $ pprLlvmHeader

      env <- cmmDataLlvmGens dflags bufh cmm
      cmmProcLlvmGens dflags bufh us env cmm

      bFlush bufh

      return  ()

  where extractRawCmm (Cmm tops) = tops


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

        exProclbl (CmmProc _ l _ _) = [(strCLabel_llvm l)]
        exProclbl  _                = []

        cdata = concat $ map exData cmm
        -- put the functions into the enviornment
        cproc = concat $ map exProclbl cmm
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
      -> IO ()

cmmProcLlvmGens _ _ _ _ []
    = return ()

cmmProcLlvmGens dflags h us env (cmm : cmms)
  = do
      (us', env', llvm) <- cmmLlvmGen dflags us (clearVars env) cmm

      Prt.bufLeftRender h $ Prt.vcat $ map (pprLlvmCmmTop dflags) llvm

      cmmProcLlvmGens dflags h us' env' cmms


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
        (vcat $ map (docToSDoc . pprLlvmCmmTop dflags) llvmBC)

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

