-----------------------------------------------------------------------------
--
-- Code generation for profiling
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

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
#include "../includes/MachDeps.h"
 -- For WORD_SIZE_IN_BITS only.
#include "../includes/rts/Constants.h"
	-- For LDV_CREATE_MASK, LDV_STATE_USE
	-- which are StgWords
#include "../includes/dist-derivedconstants/header/DerivedConstants.h"
	-- For REP_xxx constants, which are MachReps

import ClosureInfo
import CgUtils
import CgMonad
import SMRep

import OldCmm
import OldCmmUtils
import CLabel

import qualified Module
import CostCentre
import StaticFlags
import FastString
import Module
import Constants	-- Lots of field offsets
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

costCentreFrom :: CmmExpr 	-- A closure pointer
	       -> CmmExpr	-- The cost centre from that closure
costCentreFrom cl = CmmLoad (cmmOffsetB cl oFFSET_StgHeader_ccs) bWord

staticProfHdr :: CostCentreStack -> [CmmLit]
-- The profiling header words in a static closure
-- Was SET_STATIC_PROF_HDR
staticProfHdr ccs = ifProfilingL [mkCCostCentreStack ccs, 
			  	  staticLdvInit]

dynProfHdr :: CmmExpr -> [CmmExpr]
-- Profiling header words in a dynamic closure
dynProfHdr ccs = ifProfilingL [ccs, dynLdvInit]

initUpdFrameProf :: CmmExpr -> Code
-- Initialise the profiling field of an update frame
initUpdFrameProf frame_amode 
  = ifProfiling $	-- frame->header.prof.ccs = CCCS
    stmtC (CmmStore (cmmOffsetB frame_amode oFFSET_StgHeader_ccs) curCCS)
	-- frame->header.prof.hp.rs = NULL (or frame-header.prof.hp.ldvw = 0) 
	-- is unnecessary because it is not used anyhow.

-- -----------------------------------------------------------------------------
-- Recording allocation in a cost centre

-- | Record the allocation of a closure.  The CmmExpr is the cost
-- centre stack to which to attribute the allocation.
profDynAlloc :: ClosureInfo -> CmmExpr -> Code
profDynAlloc cl_info ccs
  = ifProfiling $
    profAlloc (CmmLit (mkIntCLit (closureSize cl_info))) ccs

-- | Record the allocation of a closure (size is given by a CmmExpr)
-- The size must be in words, because the allocation counter in a CCS counts
-- in words.
--
-- This API is used by the @CCS_ALLOC()@ macro in @.cmm@ code.
--
profAlloc :: CmmExpr -> CmmExpr -> Code
profAlloc words ccs
  = ifProfiling $
    stmtC (addToMemE alloc_rep
		(cmmOffsetB ccs oFFSET_CostCentreStack_mem_alloc)
	  	(CmmMachOp (MO_UU_Conv wordWidth alloc_rep) $
		  [CmmMachOp mo_wordSub [words, 
					 CmmLit (mkIntCLit profHdrSize)]]))
		-- subtract the "profiling overhead", which is the
		-- profiling header in a closure.
 where 
   alloc_rep = typeWidth REP_CostCentreStack_mem_alloc

-- -----------------------------------------------------------------------
-- Setting the current cost centre on entry to a closure

enterCostCentreThunk :: CmmExpr -> Code
enterCostCentreThunk closure = 
  ifProfiling $ do 
    stmtC $ storeCurCCS (costCentreFrom closure)

enterCostCentreFun :: CostCentreStack -> CmmExpr -> [GlobalReg] -> Code
enterCostCentreFun ccs closure vols =
  ifProfiling $ do
    if isCurrentCCS ccs
       then emitRtsCallWithVols rtsPackageId (fsLit "enterFunCCS")
               [CmmHinted (CmmReg (CmmGlobal BaseReg)) AddrHint,
                CmmHinted (costCentreFrom closure) AddrHint] vols
       else return () -- top-level function, nothing to do

ifProfiling :: Code -> Code
ifProfiling code
  | opt_SccProfilingOn = code
  | otherwise	       = nopC

ifProfilingL :: [a] -> [a]
ifProfilingL xs
  | opt_SccProfilingOn = xs
  | otherwise	       = []

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
     lits = [ zero,   	-- StgInt ccID,
	      label,	-- char *label,
	      modl,	-- char *module,
              loc,      -- char *srcloc,
              zero64,   -- StgWord64 mem_alloc
              zero,     -- StgWord time_ticks
              is_caf,   -- StgInt is_caf
              zero      -- struct _CostCentre *link
	    ] 
  ; emitDataLits (mkCCLabel cc) lits
  }
  where
     is_caf | isCafCC cc = mkIntCLit (ord 'c') -- 'c' == is a CAF
            | otherwise  = zero


emitCostCentreStackDecl
   :: CostCentreStack
   -> Code
emitCostCentreStackDecl ccs 
  | Just cc <- maybeSingletonCCS ccs = do
  { let
	-- Note: to avoid making any assumptions about how the
	-- C compiler (that compiles the RTS, in particular) does
	-- layouts of structs containing long-longs, simply
	-- pad out the struct with zero words until we hit the
	-- size of the overall struct (which we get via DerivedConstants.h)
	--
     lits = zero : mkCCostCentre cc : replicate (sizeof_ccs_words - 2) zero
  ; emitDataLits (mkCCSLabel ccs) lits
  }
  | otherwise = pprPanic "emitCostCentreStackDecl" (ppr ccs)

zero :: CmmLit
zero = mkIntCLit 0
zero64 :: CmmLit
zero64 = CmmInt 0 W64

sizeof_ccs_words :: Int
sizeof_ccs_words 
    -- round up to the next word.
  | ms == 0   = ws
  | otherwise = ws + 1
  where
   (ws,ms) = SIZEOF_CostCentreStack `divMod` wORD_SIZE

-- ---------------------------------------------------------------------------
-- Set the current cost centre stack

emitSetCCC :: CostCentre -> Bool -> Bool -> Code
emitSetCCC cc tick push
  | not opt_SccProfilingOn = nopC
  | otherwise = do 
    tmp <- newTemp bWord -- TODO FIXME NOW
    pushCostCentre tmp curCCS cc
    when tick $ stmtC (bumpSccCount (CmmReg (CmmLocal tmp)))
    when push $ stmtC (storeCurCCS (CmmReg (CmmLocal tmp)))

pushCostCentre :: LocalReg -> CmmExpr -> CostCentre -> Code
pushCostCentre result ccs cc
  = emitRtsCallWithResult result AddrHint
	rtsPackageId 
        (fsLit "pushCostCentre") [CmmHinted ccs AddrHint,
                                  CmmHinted (CmmLit (mkCCostCentre cc)) AddrHint]

bumpSccCount :: CmmExpr -> CmmStmt
bumpSccCount ccs
  = addToMem (typeWidth REP_CostCentreStack_scc_count)
	 (cmmOffsetB ccs oFFSET_CostCentreStack_scc_count) 1

-----------------------------------------------------------------------------
--
--		Lag/drag/void stuff
--
-----------------------------------------------------------------------------

--
-- Initial value for the LDV field in a static closure
--
staticLdvInit :: CmmLit
staticLdvInit = zeroCLit

--
-- Initial value of the LDV field in a dynamic closure
--
dynLdvInit :: CmmExpr
dynLdvInit =     -- (era << LDV_SHIFT) | LDV_STATE_CREATE  
  CmmMachOp mo_wordOr [
      CmmMachOp mo_wordShl [loadEra, CmmLit (mkIntCLit lDV_SHIFT) ],
      CmmLit (mkWordCLit lDV_STATE_CREATE)
  ]
        
--
-- Initialise the LDV word of a new closure
--
ldvRecordCreate :: CmmExpr -> Code
ldvRecordCreate closure = stmtC $ CmmStore (ldvWord closure) dynLdvInit

--
-- Called when a closure is entered, marks the closure as having been "used".
-- The closure is not an 'inherently used' one.
-- The closure is not IND or IND_OLDGEN because neither is considered for LDV
-- profiling.
--
ldvEnterClosure :: ClosureInfo -> Code
ldvEnterClosure closure_info = ldvEnter (cmmOffsetB (CmmReg nodeReg) (-tag))
  where tag = funTag closure_info
        -- don't forget to substract node's tag
  
ldvEnter :: CmmExpr -> Code
-- Argument is a closure pointer
ldvEnter cl_ptr
  =  ifProfiling $
     -- if (era > 0) {
     --    LDVW((c)) = (LDVW((c)) & LDV_CREATE_MASK) |
     --                era | LDV_STATE_USE }
    emitIf (CmmMachOp mo_wordUGt [loadEra, CmmLit zeroCLit])
	   (stmtC (CmmStore ldv_wd new_ldv_wd))
  where
        -- don't forget to substract node's tag
    ldv_wd = ldvWord cl_ptr
    new_ldv_wd = cmmOrWord (cmmAndWord (CmmLoad ldv_wd bWord)
				       (CmmLit (mkWordCLit lDV_CREATE_MASK)))
		 (cmmOrWord loadEra (CmmLit (mkWordCLit lDV_STATE_USE)))

loadEra :: CmmExpr 
loadEra = CmmMachOp (MO_UU_Conv cIntWidth wordWidth)
	  [CmmLoad (mkLblExpr (mkCmmDataLabel rtsPackageId $ fsLit("era"))) cInt]

ldvWord :: CmmExpr -> CmmExpr
-- Takes the address of a closure, and returns 
-- the address of the LDV word in the closure
ldvWord closure_ptr = cmmOffsetB closure_ptr oFFSET_StgHeader_ldvw

-- LDV constants, from ghc/includes/Constants.h
lDV_SHIFT :: Int
lDV_SHIFT = LDV_SHIFT
--lDV_STATE_MASK :: StgWord
--lDV_STATE_MASK   = LDV_STATE_MASK
lDV_CREATE_MASK :: StgWord
lDV_CREATE_MASK  = LDV_CREATE_MASK
--lDV_LAST_MASK    :: StgWord
--lDV_LAST_MASK    = LDV_LAST_MASK
lDV_STATE_CREATE :: StgWord
lDV_STATE_CREATE = LDV_STATE_CREATE
lDV_STATE_USE    :: StgWord
lDV_STATE_USE    = LDV_STATE_USE

