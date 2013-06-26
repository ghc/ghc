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
import Cmm
import Hoopl
import PprCmm

import BufWrite
import DynFlags
import ErrUtils
import FastString
import Outputable
import qualified Pretty as Prt
import UniqSupply
import Util
import SysTools ( figureLlvmVersion )

import Control.Monad ( when )
import Data.IORef ( writeIORef )
import Data.Maybe ( fromMaybe )
import System.IO

-- -----------------------------------------------------------------------------
-- | Top-level of the LLVM Code generator
--
llvmCodeGen :: DynFlags -> Handle -> UniqSupply -> [RawCmmGroup] -> IO ()
llvmCodeGen dflags h us cmms
  = let cmm = concat cmms
        (cdata,env) = {-# SCC "llvm_split" #-}
                      foldr split ([], initLlvmEnv dflags) cmm
        split (CmmData s d' ) (d,e) = ((s,d'):d,e)
        split (CmmProc h l live g) (d,e) =
            let lbl = strCLabel_llvm env $
                        case mapLookup (g_entry g) h of
                          Nothing                   -> l
                          Just (Statics info_lbl _) -> info_lbl
                env' = funInsert lbl (llvmFunTy dflags live) e
            in (d,env')
    in do
        showPass dflags "LlVM CodeGen"
        dumpIfSet_dyn dflags Opt_D_dump_llvm "LLVM Code" pprLlvmHeader
        bufh <- newBufHandle h
        Prt.bufLeftRender bufh $ withPprStyleDoc dflags (mkCodeStyle CStyle) pprLlvmHeader
        ver  <- getLlvmVersion
        env' <- {-# SCC "llvm_datas_gen" #-}
                cmmDataLlvmGens dflags bufh (setLlvmVer ver env) cdata []
        {-# SCC "llvm_procs_gen" #-}
             cmmProcLlvmGens dflags bufh us env' cmm 1 []
        bFlush bufh
        return  ()

  where
    -- | Handle setting up the LLVM version.
    getLlvmVersion = do
        ver <- (fromMaybe defaultLlvmVersion) `fmap` figureLlvmVersion dflags
        -- cache llvm version for later use
        writeIORef (llvmVersion dflags) ver
        debugTraceMsg dflags 2
            (text "Using LLVM version:" <+> text (show ver))
        let doWarn = wopt Opt_WarnUnsupportedLlvmVersion dflags
        when (ver < minSupportLlvmVersion && doWarn) $
            errorMsg dflags (text "You are using an old version of LLVM that"
                             <> text " isn't supported anymore!"
                             $+$ text "We will try though...")
        when (ver > maxSupportLlvmVersion && doWarn) $
            putMsg dflags (text "You are using a new version of LLVM that"
                           <> text " hasn't been tested yet!"
                           $+$ text "We will try though...")
        return ver


-- -----------------------------------------------------------------------------
-- | Do LLVM code generation on all these Cmms data sections.
--
cmmDataLlvmGens :: DynFlags -> BufHandle -> LlvmEnv -> [(Section,CmmStatics)]
                -> [LlvmUnresData] -> IO ( LlvmEnv )

cmmDataLlvmGens dflags h env [] lmdata
  = let (env', lmdata') = {-# SCC "llvm_resolve" #-}
                          resolveLlvmDatas env lmdata
        lmdoc = {-# SCC "llvm_data_ppr" #-}
                vcat $ map pprLlvmData lmdata'
    in do
        dumpIfSet_dyn dflags Opt_D_dump_llvm "LLVM Code" lmdoc
        {-# SCC "llvm_data_out" #-}
            Prt.bufLeftRender h $ withPprStyleDoc dflags (mkCodeStyle CStyle) lmdoc
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

cmmProcLlvmGens dflags h _ _ [] _ ivars
    | null ivars' = return ()
    | otherwise   = Prt.bufLeftRender h $
                        {-# SCC "llvm_used_ppr" #-}
                        withPprStyleDoc dflags (mkCodeStyle CStyle) $
                        pprLlvmData ([lmUsed], [])
  where
    ivars' = concat ivars
    cast x = LMBitc (LMStaticPointer (pVarLift x)) i8Ptr
    ty     = (LMArray (length ivars') i8Ptr)
    usedArray = LMStaticArray (map cast ivars') ty
    lmUsedVar = LMGlobalVar (fsLit "llvm.used") ty Appending
                  (Just $ fsLit "llvm.metadata") Nothing Global
    lmUsed    = LMGlobal lmUsedVar (Just usedArray)

cmmProcLlvmGens dflags h us env ((CmmData _ _) : cmms) count ivars
 = cmmProcLlvmGens dflags h us env cmms count ivars

cmmProcLlvmGens dflags h us env (cmm : cmms) count ivars = do
    (us', env', llvm) <- cmmLlvmGen dflags us (clearVars env) cmm
    let (docs, ivar) = mapAndUnzip (pprLlvmCmmDecl env' count) llvm
    Prt.bufLeftRender h $ {-# SCC "llvm_proc_ppr" #-}
                          withPprStyleDoc dflags (mkCodeStyle CStyle) $ vcat docs
    cmmProcLlvmGens dflags h us' env' cmms (count + 2) (ivar ++ ivars)


-- | Complete LLVM code generation phase for a single top-level chunk of Cmm.
cmmLlvmGen :: DynFlags -> UniqSupply -> LlvmEnv -> RawCmmDecl
            -> IO ( UniqSupply, LlvmEnv, [LlvmCmmDecl] )
cmmLlvmGen dflags us env cmm = do
    -- rewrite assignments to global regs
    let fixed_cmm = {-# SCC "llvm_fix_regs" #-}
                    fixStgRegisters dflags cmm

    dumpIfSet_dyn dflags Opt_D_dump_opt_cmm "Optimised Cmm"
        (pprCmmGroup [fixed_cmm])

    -- generate llvm code from cmm
    let ((env', llvmBC), usGen) = {-# SCC "llvm_proc_gen" #-}
                                  initUs us $ genLlvmProc env fixed_cmm

    dumpIfSet_dyn dflags Opt_D_dump_llvm "LLVM Code"
        (vcat $ map (fst . pprLlvmCmmDecl env' 0) llvmBC)

    return (usGen, env', llvmBC)

