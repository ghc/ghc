-- -----------------------------------------------------------------------------
-- | This is the top-level module in the LLVM code generator.
--

module LlvmCodeGen ( llvmCodeGen, llvmFixupAsm ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Base
import LlvmCodeGen.CodeGen
import LlvmCodeGen.Data
import LlvmCodeGen.Ppr
import LlvmMangler

import CgUtils ( fixStgRegisters )
import OldCmm
import OldPprCmm

import BufWrite
import DynFlags
import ErrUtils
import FastString
import Outputable
import qualified Pretty as Prt
import UniqSupply
import Util
import SysTools ( figureLlvmVersion )

import Data.Maybe ( fromMaybe )
import System.IO

-- -----------------------------------------------------------------------------
-- | Top-level of the LLVM Code generator
--
llvmCodeGen :: DynFlags -> Handle -> UniqSupply -> [RawCmmGroup] -> IO ()
llvmCodeGen dflags h us cmms
  = let cmm = concat cmms
        (cdata,env) = {-# SCC "llvm_split" #-}
                      foldr split ([],initLlvmEnv (targetPlatform dflags)) cmm
        split (CmmData s d' ) (d,e) = ((s,d'):d,e)
        split (CmmProc i l _) (d,e) =
            let lbl = strCLabel_llvm env $ case i of
                        Nothing                   -> l
                        Just (Statics info_lbl _) -> info_lbl
                env' = funInsert lbl llvmFunTy e
            in (d,env')
    in do
        showPass dflags "LlVM CodeGen"
        bufh <- newBufHandle h
        Prt.bufLeftRender bufh $ pprLlvmHeader
        ver  <- (fromMaybe defaultLlvmVersion) `fmap` figureLlvmVersion dflags
        env' <- {-# SCC "llvm_datas_gen" #-}
                cmmDataLlvmGens dflags bufh (setLlvmVer ver env) cdata []
        {-# SCC "llvm_procs_gen" #-}
             cmmProcLlvmGens dflags bufh us env' cmm 1 []
        bFlush bufh
        return  ()


-- -----------------------------------------------------------------------------
-- | Do LLVM code generation on all these Cmms data sections.
--
cmmDataLlvmGens :: DynFlags -> BufHandle -> LlvmEnv -> [(Section,CmmStatics)]
                -> [LlvmUnresData] -> IO ( LlvmEnv )

cmmDataLlvmGens dflags h env [] lmdata
  = let (env', lmdata') = {-# SCC "llvm_resolve" #-}
                          resolveLlvmDatas env lmdata
        lmdoc = {-# SCC "llvm_data_ppr" #-}
                Prt.vcat $ map pprLlvmData lmdata'
    in do
        dumpIfSet_dyn dflags Opt_D_dump_llvm "LLVM Code" $ docToSDoc lmdoc
        {-# SCC "llvm_data_out" #-}
            Prt.bufLeftRender h lmdoc
        return env'

cmmDataLlvmGens dflags h env (cmm:cmms) lmdata
  = let lm@(l, _, ty, _) = {-# SCC "llvm_data_gen" #-}
                           genLlvmData env cmm
        env' = {-# SCC "llvm_data_insert" #-}
               funInsert (strCLabel_llvm env l) ty env
        lmdata' = {-# SCC "llvm_data_append" #-}
                  lm:lmdata
    in cmmDataLlvmGens dflags h env' cmms lmdata'


-- -----------------------------------------------------------------------------
-- | Do LLVM code generation on all these Cmms procs.
--
cmmProcLlvmGens :: DynFlags -> BufHandle -> UniqSupply -> LlvmEnv -> [RawCmmDecl]
      -> Int         -- ^ count, used for generating unique subsections
      -> [[LlvmVar]] -- ^ info tables that need to be marked as 'used'
      -> IO ()

cmmProcLlvmGens _ _ _ _ [] _ []
  = return ()

cmmProcLlvmGens _ h _ _ [] _ ivars
  = let ivars' = concat ivars
        cast x = LMBitc (LMStaticPointer (pVarLift x)) i8Ptr
        ty     = (LMArray (length ivars') i8Ptr)
        usedArray = LMStaticArray (map cast ivars') ty
        lmUsed = (LMGlobalVar (fsLit "llvm.used") ty Appending
                  (Just $ fsLit "llvm.metadata") Nothing False, Just usedArray)
    in Prt.bufLeftRender h $ {-# SCC "llvm_used_ppr" #-}
                             pprLlvmData ([lmUsed], [])

cmmProcLlvmGens dflags h us env ((CmmData _ _) : cmms) count ivars
 = cmmProcLlvmGens dflags h us env cmms count ivars

cmmProcLlvmGens dflags h us env ((CmmProc _ _ (ListGraph [])) : cmms) count ivars
 = cmmProcLlvmGens dflags h us env cmms count ivars

cmmProcLlvmGens dflags h us env (cmm : cmms) count ivars = do
    (us', env', llvm) <- cmmLlvmGen dflags us (clearVars env) cmm
    let (docs, ivar) = mapAndUnzip (pprLlvmCmmDecl env' count) llvm
    Prt.bufLeftRender h $ {-# SCC "llvm_proc_ppr" #-} Prt.vcat docs
    cmmProcLlvmGens dflags h us' env' cmms (count + 2) (ivar ++ ivars)


-- | Complete LLVM code generation phase for a single top-level chunk of Cmm.
cmmLlvmGen :: DynFlags -> UniqSupply -> LlvmEnv -> RawCmmDecl
            -> IO ( UniqSupply, LlvmEnv, [LlvmCmmDecl] )
cmmLlvmGen dflags us env cmm = do
    -- rewrite assignments to global regs
    let fixed_cmm = {-# SCC "llvm_fix_regs" #-}
                    fixStgRegisters cmm

    dumpIfSet_dyn dflags Opt_D_dump_opt_cmm "Optimised Cmm"
        (pprCmmGroup (targetPlatform dflags) [fixed_cmm])

    -- generate llvm code from cmm
    let ((env', llvmBC), usGen) = {-# SCC "llvm_proc_gen" #-}
                                  initUs us $ genLlvmProc env fixed_cmm

    dumpIfSet_dyn dflags Opt_D_dump_llvm "LLVM Code"
        (vcat $ map (docToSDoc . fst . pprLlvmCmmDecl env' 0) llvmBC)

    return (usGen, env', llvmBC)

