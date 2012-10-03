-----------------------------------------------------------------------------
--
-- Code generation for profiling
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module CgProf (
        mkCCostCentre, mkCCostCentreStack,

        -- Cost-centre Profiling
        dynProfHdr, profDynAlloc, profAlloc, staticProfHdr, initUpdFrameProf,
        enterCostCentreThunk,
        enterCostCentreFun,
        costCentreFrom,
        curCCS, storeCurCCS,
        emitCostCentreDecl, emitCostCentreStackDecl,
        emitSetCCC,

        -- Lag/drag/void stuff
        ldvEnter, ldvEnterClosure, ldvRecordCreate
  ) where

#include "HsVersions.h"

import ClosureInfo
import CgUtils
import CgMonad
import SMRep

import OldCmm
import OldCmmUtils
import CLabel

import qualified Module
import CostCentre
import DynFlags
import FastString
import Module
import Outputable

import Data.Char
import Control.Monad

-----------------------------------------------------------------------------
--
-- Cost-centre-stack Profiling
--
-----------------------------------------------------------------------------

-- Expression representing the current cost centre stack
curCCS :: CmmExpr
curCCS = CmmReg (CmmGlobal CCCS)

storeCurCCS :: CmmExpr -> CmmStmt
storeCurCCS e = CmmAssign (CmmGlobal CCCS) e

mkCCostCentre :: CostCentre -> CmmLit
mkCCostCentre cc = CmmLabel (mkCCLabel cc)

mkCCostCentreStack :: CostCentreStack -> CmmLit
mkCCostCentreStack ccs = CmmLabel (mkCCSLabel ccs)

costCentreFrom :: DynFlags
               -> CmmExpr       -- A closure pointer
               -> CmmExpr       -- The cost centre from that closure
costCentreFrom dflags cl
    = CmmLoad (cmmOffsetB dflags cl (oFFSET_StgHeader_ccs dflags)) (bWord dflags)

staticProfHdr :: DynFlags -> CostCentreStack -> [CmmLit]
-- The profiling header words in a static closure
-- Was SET_STATIC_PROF_HDR
staticProfHdr dflags ccs = ifProfilingL dflags [mkCCostCentreStack ccs,
                                                staticLdvInit dflags]

dynProfHdr :: DynFlags -> CmmExpr -> [CmmExpr]
-- Profiling header words in a dynamic closure
dynProfHdr dflags ccs = ifProfilingL dflags [ccs, dynLdvInit dflags]

initUpdFrameProf :: CmmExpr -> Code
-- Initialise the profiling field of an update frame
initUpdFrameProf frame_amode
  = ifProfiling $       -- frame->header.prof.ccs = CCCS
    do dflags <- getDynFlags
       stmtC (CmmStore (cmmOffsetB dflags frame_amode (oFFSET_StgHeader_ccs dflags)) curCCS)
        -- frame->header.prof.hp.rs = NULL (or frame-header.prof.hp.ldvw = 0)
        -- is unnecessary because it is not used anyhow.

-- -----------------------------------------------------------------------------
-- Recording allocation in a cost centre

-- | Record the allocation of a closure.  The CmmExpr is the cost
-- centre stack to which to attribute the allocation.
profDynAlloc :: ClosureInfo -> CmmExpr -> Code
profDynAlloc cl_info ccs
  = ifProfiling $
    do dflags <- getDynFlags
       profAlloc (mkIntExpr dflags (closureSize dflags cl_info)) ccs

-- | Record the allocation of a closure (size is given by a CmmExpr)
-- The size must be in words, because the allocation counter in a CCS counts
-- in words.
--
-- This API is used by the @CCS_ALLOC()@ macro in @.cmm@ code.
--
profAlloc :: CmmExpr -> CmmExpr -> Code
profAlloc words ccs
  = ifProfiling $
    do dflags <- getDynFlags
       let alloc_rep = typeWidth (rEP_CostCentreStack_mem_alloc dflags)
       stmtC (addToMemE alloc_rep
                   (cmmOffsetB dflags ccs (oFFSET_CostCentreStack_mem_alloc dflags))
                   (CmmMachOp (MO_UU_Conv (wordWidth dflags) alloc_rep) $
                     [CmmMachOp (mo_wordSub dflags) [words,
                                                     mkIntExpr dflags (profHdrSize dflags)]]))
                   -- subtract the "profiling overhead", which is the
                   -- profiling header in a closure.

-- -----------------------------------------------------------------------
-- Setting the current cost centre on entry to a closure

enterCostCentreThunk :: CmmExpr -> Code
enterCostCentreThunk closure =
  ifProfiling $ do
    dflags <- getDynFlags
    stmtC $ storeCurCCS (costCentreFrom dflags closure)

enterCostCentreFun :: CostCentreStack -> CmmExpr -> [GlobalReg] -> Code
enterCostCentreFun ccs closure vols =
  ifProfiling $ do
    if isCurrentCCS ccs
       then do dflags <- getDynFlags
               emitRtsCallWithVols rtsPackageId (fsLit "enterFunCCS")
                   [CmmHinted (CmmReg (CmmGlobal BaseReg)) AddrHint,
                    CmmHinted (costCentreFrom dflags closure) AddrHint] vols
       else return () -- top-level function, nothing to do

ifProfiling :: Code -> Code
ifProfiling code
    = do dflags <- getDynFlags
         if dopt Opt_SccProfilingOn dflags then code else nopC

ifProfilingL :: DynFlags -> [a] -> [a]
ifProfilingL dflags xs
  | dopt Opt_SccProfilingOn dflags = xs
  | otherwise                      = []

-- ---------------------------------------------------------------------------
-- Initialising Cost Centres & CCSs

emitCostCentreDecl
   :: CostCentre
   -> Code
emitCostCentreDecl cc = do
                        -- NB. bytesFS: we want the UTF-8 bytes here (#5559)
  { label <- newByteStringCLit (bytesFS $ costCentreUserNameFS cc)
  ; modl  <- newByteStringCLit (bytesFS $ Module.moduleNameFS
                                        $ Module.moduleName
                                        $ cc_mod cc)
                -- All cost centres will be in the main package, since we
                -- don't normally use -auto-all or add SCCs to other packages.
                -- Hence don't emit the package name in the module here.
  ; dflags <- getDynFlags
  ; loc <- newByteStringCLit $ bytesFS $ mkFastString $
                   showPpr dflags (costCentreSrcSpan cc)
           -- XXX going via FastString to get UTF-8 encoding is silly
  ; let
     is_caf | isCafCC cc = mkIntCLit dflags (ord 'c') -- 'c' == is a CAF
            | otherwise  = zero dflags
     lits = [ zero dflags,     -- StgInt ccID,
              label,    -- char *label,
              modl,     -- char *module,
              loc,      -- char *srcloc,
              zero64,   -- StgWord64 mem_alloc
              zero dflags,     -- StgWord time_ticks
              is_caf,   -- StgInt is_caf
              zero dflags      -- struct _CostCentre *link
            ]
  ; emitDataLits (mkCCLabel cc) lits
  }


emitCostCentreStackDecl
   :: CostCentreStack
   -> Code
emitCostCentreStackDecl ccs
  | Just cc <- maybeSingletonCCS ccs = do
  { dflags <- getDynFlags
  ; let
        -- Note: to avoid making any assumptions about how the
        -- C compiler (that compiles the RTS, in particular) does
        -- layouts of structs containing long-longs, simply
        -- pad out the struct with zero words until we hit the
        -- size of the overall struct (which we get via DerivedConstants.h)
        --
     lits = zero dflags
          : mkCCostCentre cc
          : replicate (sizeof_ccs_words dflags - 2) (zero dflags)
  ; emitDataLits (mkCCSLabel ccs) lits
  }
  | otherwise = pprPanic "emitCostCentreStackDecl" (ppr ccs)

zero :: DynFlags -> CmmLit
zero dflags = mkIntCLit dflags 0
zero64 :: CmmLit
zero64 = CmmInt 0 W64

sizeof_ccs_words :: DynFlags -> Int
sizeof_ccs_words dflags
    -- round up to the next word.
  | ms == 0   = ws
  | otherwise = ws + 1
  where
   (ws,ms) = sIZEOF_CostCentreStack dflags `divMod` wORD_SIZE dflags

-- ---------------------------------------------------------------------------
-- Set the current cost centre stack

emitSetCCC :: CostCentre -> Bool -> Bool -> Code
emitSetCCC cc tick push
 = do dflags <- getDynFlags
      if dopt Opt_SccProfilingOn dflags
          then do tmp <- newTemp (bWord dflags) -- TODO FIXME NOW
                  pushCostCentre tmp curCCS cc
                  when tick $ stmtC (bumpSccCount dflags (CmmReg (CmmLocal tmp)))
                  when push $ stmtC (storeCurCCS (CmmReg (CmmLocal tmp)))
          else nopC

pushCostCentre :: LocalReg -> CmmExpr -> CostCentre -> Code
pushCostCentre result ccs cc
  = emitRtsCallWithResult result AddrHint
        rtsPackageId
        (fsLit "pushCostCentre") [CmmHinted ccs AddrHint,
                                  CmmHinted (CmmLit (mkCCostCentre cc)) AddrHint]

bumpSccCount :: DynFlags -> CmmExpr -> CmmStmt
bumpSccCount dflags ccs
  = addToMem (typeWidth (rEP_CostCentreStack_scc_count dflags))
         (cmmOffsetB dflags ccs (oFFSET_CostCentreStack_scc_count dflags)) 1

-----------------------------------------------------------------------------
--
--              Lag/drag/void stuff
--
-----------------------------------------------------------------------------

--
-- Initial value for the LDV field in a static closure
--
staticLdvInit :: DynFlags -> CmmLit
staticLdvInit = zeroCLit

--
-- Initial value of the LDV field in a dynamic closure
--
dynLdvInit :: DynFlags -> CmmExpr
dynLdvInit dflags =     -- (era << LDV_SHIFT) | LDV_STATE_CREATE
  CmmMachOp (mo_wordOr dflags) [
      CmmMachOp (mo_wordShl dflags) [loadEra dflags, mkIntExpr dflags (lDV_SHIFT dflags)],
      CmmLit (mkWordCLit dflags (iLDV_STATE_CREATE dflags))
  ]

--
-- Initialise the LDV word of a new closure
--
ldvRecordCreate :: CmmExpr -> Code
ldvRecordCreate closure = do dflags <- getDynFlags
                             stmtC $ CmmStore (ldvWord dflags closure) (dynLdvInit dflags)

--
-- Called when a closure is entered, marks the closure as having been "used".
-- The closure is not an 'inherently used' one.
-- The closure is not IND or IND_OLDGEN because neither is considered for LDV
-- profiling.
--
ldvEnterClosure :: ClosureInfo -> Code
ldvEnterClosure closure_info
    = do dflags <- getDynFlags
         let tag = funTag dflags closure_info
         ldvEnter (cmmOffsetB dflags (CmmReg nodeReg) (-tag))
        -- don't forget to substract node's tag

ldvEnter :: CmmExpr -> Code
-- Argument is a closure pointer
ldvEnter cl_ptr = do
  dflags <- getDynFlags
  let
        -- don't forget to substract node's tag
    ldv_wd = ldvWord dflags cl_ptr
    new_ldv_wd = cmmOrWord dflags (cmmAndWord dflags (CmmLoad ldv_wd (bWord dflags))
                                                     (CmmLit (mkWordCLit dflags (iLDV_CREATE_MASK dflags))))
                 (cmmOrWord dflags (loadEra dflags) (CmmLit (mkWordCLit dflags (iLDV_STATE_USE dflags))))
  ifProfiling $
     -- if (era > 0) {
     --    LDVW((c)) = (LDVW((c)) & LDV_CREATE_MASK) |
     --                era | LDV_STATE_USE }
    emitIf (CmmMachOp (mo_wordUGt dflags) [loadEra dflags, CmmLit (zeroCLit dflags)])
           (stmtC (CmmStore ldv_wd new_ldv_wd))

loadEra :: DynFlags -> CmmExpr
loadEra dflags = CmmMachOp (MO_UU_Conv (cIntWidth dflags) (wordWidth dflags))
                           [CmmLoad (mkLblExpr (mkCmmDataLabel rtsPackageId $ fsLit("era"))) (cInt dflags)]

ldvWord :: DynFlags -> CmmExpr -> CmmExpr
-- Takes the address of a closure, and returns
-- the address of the LDV word in the closure
ldvWord dflags closure_ptr
    = cmmOffsetB dflags closure_ptr (oFFSET_StgHeader_ldvw dflags)

