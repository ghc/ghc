-----------------------------------------------------------------------------
--
-- Code generation for profiling
--
-- (c) The University of Glasgow 2004
--
-----------------------------------------------------------------------------

module CgProf (
	mkCCostCentre, mkCCostCentreStack,

	-- Cost-centre Profiling
	dynProfHdr, profDynAlloc, profAlloc, staticProfHdr, initUpdFrameProf,
	enterCostCentre, enterCostCentrePAP, enterCostCentreThunk, 
	chooseDynCostCentres, 
	costCentreFrom, 
	curCCS, curCCSAddr,
	emitCostCentreDecl, emitCostCentreStackDecl, 
	emitRegisterCC, emitRegisterCCS,
	emitSetCCC, emitCCS,

	-- Lag/drag/void stuff
	ldvEnter, ldvRecordCreate
  ) where

#include "HsVersions.h"
#include "MachDeps.h"
 -- For WORD_SIZE_IN_BITS only.
#include "../includes/Constants.h"
	-- For LDV_CREATE_MASK, LDV_STATE_USE
	-- which are StgWords
#include "../includes/DerivedConstants.h"
	-- For REP_xxx constants, which are MachReps

import ClosureInfo	( ClosureInfo, closureSize,
			  closureName, isToplevClosure, closureReEntrant, )
import CgUtils
import CgMonad
import SMRep		( StgWord, profHdrSize )

import Cmm
import MachOp
import CmmUtils		( zeroCLit, mkIntCLit, mkLblExpr )
import CLabel		( mkCCLabel, mkCCSLabel, mkRtsDataLabel )

import Module		( moduleUserString )
import Id		( Id )
import CostCentre
import StgSyn		( GenStgExpr(..), StgExpr )
import CmdLineOpts	( opt_SccProfilingOn )
import FastString	( FastString, mkFastString, LitString )	
import Constants	-- Lots of field offsets
import Outputable

import Maybe
import Char		( ord )
import Monad		( when )

-----------------------------------------------------------------------------
--
-- Cost-centre-stack Profiling
--
-----------------------------------------------------------------------------

-- Expression representing the current cost centre stack
curCCS :: CmmExpr
curCCS = CmmLoad curCCSAddr wordRep

-- Address of current CCS variable, for storing into
curCCSAddr :: CmmExpr
curCCSAddr = CmmLit (CmmLabel (mkRtsDataLabel SLIT("CCCS")))

mkCCostCentre :: CostCentre -> CmmLit
mkCCostCentre cc = CmmLabel (mkCCLabel cc)

mkCCostCentreStack :: CostCentreStack -> CmmLit
mkCCostCentreStack ccs = CmmLabel (mkCCSLabel ccs)

costCentreFrom :: CmmExpr 	-- A closure pointer
	       -> CmmExpr	-- The cost centre from that closure
costCentreFrom cl = CmmLoad (cmmOffsetB cl oFFSET_StgHeader_ccs) wordRep

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
profAlloc :: CmmExpr -> CmmExpr -> Code
profAlloc words ccs
  = ifProfiling $
    stmtC (addToMemE alloc_rep
		(cmmOffsetB ccs oFFSET_CostCentreStack_mem_alloc)
	  	(CmmMachOp (MO_U_Conv wordRep alloc_rep) $
		  [CmmMachOp mo_wordSub [words, 
					 CmmLit (mkIntCLit profHdrSize)]]))
		-- subtract the "profiling overhead", which is the
		-- profiling header in a closure.
 where 
	alloc_rep =  REP_CostCentreStack_mem_alloc

-- ----------------------------------------------------------------------
-- Setting the cost centre in a new closure

chooseDynCostCentres :: CostCentreStack
		     -> [Id] 	 	-- Args
		     -> StgExpr		-- Body
		     -> FCode (CmmExpr, CmmExpr)
-- Called when alllcating a closure
-- Tells which cost centre to put in the object, and which
-- to blame the cost of allocation on
chooseDynCostCentres ccs args body = do
  -- Cost-centre we record in the object
  use_ccs <- emitCCS ccs

  -- Cost-centre on whom we blame the allocation
  let blame_ccs
	| null args && isBox body = CmmLit (mkCCostCentreStack overheadCCS)
	| otherwise		  = use_ccs

  return (use_ccs, blame_ccs)


-- Some CostCentreStacks are a sequence of pushes on top of CCCS.
-- These pushes must be performed before we can refer to the stack in
-- an expression.
emitCCS :: CostCentreStack -> FCode CmmExpr
emitCCS ccs = push_em (ccsExpr ccs') (reverse cc's)
  where
	(cc's, ccs') = decomposeCCS ccs

	push_em ccs [] = return ccs
	push_em ccs (cc:rest) = do
  	  tmp <- newTemp wordRep
	  pushCostCentre tmp ccs cc
	  push_em (CmmReg tmp) rest

ccsExpr :: CostCentreStack -> CmmExpr
ccsExpr ccs
  | isCurrentCCS ccs = curCCS
  | otherwise        = CmmLit (mkCCostCentreStack ccs)


isBox :: StgExpr -> Bool
-- If it's an utterly trivial RHS, then it must be
-- one introduced by boxHigherOrderArgs for profiling,
-- so we charge it to "OVERHEAD".
-- This looks like a GROSS HACK to me --SDM
isBox (StgApp fun []) = True
isBox other	      = False


-- -----------------------------------------------------------------------
-- Setting the current cost centre on entry to a closure

-- For lexically scoped profiling we have to load the cost centre from
-- the closure entered, if the costs are not supposed to be inherited.
-- This is done immediately on entering the fast entry point.

-- Load current cost centre from closure, if not inherited.
-- Node is guaranteed to point to it, if profiling and not inherited.

enterCostCentre
   :: ClosureInfo 
   -> CostCentreStack
   -> StgExpr	-- The RHS of the closure
   -> Code

-- We used to have a special case for bindings of form
--	f = g True
-- where g has arity 2.  The RHS is a thunk, but we don't
-- need to update it; and we want to subsume costs.
-- We don't have these sort of PAPs any more, so the special
-- case has gone away.

enterCostCentre closure_info ccs body
  = ifProfiling $
    ASSERT2(not (noCCSAttached ccs), ppr (closureName closure_info) <+> ppr ccs)
    enter_cost_centre closure_info ccs body

enter_cost_centre closure_info ccs body
  | isSubsumedCCS ccs
  = ASSERT(isToplevClosure closure_info)
    ASSERT(re_entrant)
    enter_ccs_fsub
	
  | isDerivedFromCurrentCCS ccs
  = do {
	if re_entrant && not is_box
    	  then
		enter_ccs_fun node_ccs
    	  else
	  	stmtC (CmmStore curCCSAddr node_ccs)

	-- don't forget to bump the scc count.  This closure might have been
	-- of the form   let x = _scc_ "x" e in ...x..., which the SCCfinal
	-- pass has turned into simply  let x = e in ...x... and attached
	-- the _scc_ as PushCostCentre(x,CCCS) on the x closure.  So that
	-- we don't lose the scc counter, bump it in the entry code for x.
	-- ToDo: for a multi-push we should really bump the counter for
	-- each of the intervening CCSs, not just the top one.
       ; when (not (isCurrentCCS ccs)) $
		stmtC (bumpSccCount curCCS)
       }

  | isCafCCS ccs
  = ASSERT(isToplevClosure closure_info)
    ASSERT(not re_entrant)
    do	{ 	-- This is just a special case of the isDerivedFromCurrentCCS
		-- case above.  We could delete this, but it's a micro
		-- optimisation and saves a bit of code.
	  stmtC (CmmStore curCCSAddr enc_ccs)
	; stmtC (bumpSccCount node_ccs)
	}

  | otherwise
  = panic "enterCostCentre"
  where
    enc_ccs    = CmmLit (mkCCostCentreStack ccs)
    re_entrant = closureReEntrant closure_info
    node_ccs   = costCentreFrom (CmmReg nodeReg)
    is_box     = isBox body

-- set the current CCS when entering a PAP
enterCostCentrePAP :: CmmExpr -> Code
enterCostCentrePAP closure = 
  ifProfiling $ do 
    enter_ccs_fun (costCentreFrom closure)
    enteringPAP 1
  
enterCostCentreThunk :: CmmExpr -> Code
enterCostCentreThunk closure = 
  ifProfiling $ do 
    stmtC $ CmmStore curCCSAddr (costCentreFrom closure)

enter_ccs_fun stack = emitRtsCall SLIT("EnterFunCCS") [(stack,PtrHint)]

enter_ccs_fsub = enteringPAP 0

-- When entering a PAP, EnterFunCCS is called by both the PAP entry
-- code and the function entry code; we don't want the function's
-- entry code to also update CCCS in the event that it was called via
-- a PAP, so we set the flag entering_PAP to indicate that we are
-- entering via a PAP.
enteringPAP :: Integer -> Code
enteringPAP n
  = stmtC (CmmStore (CmmLit (CmmLabel (mkRtsDataLabel SLIT("entering_PAP"))))
		(CmmLit (CmmInt n cIntRep)))

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
  { label <- mkStringCLit (costCentreUserName cc)
  ; modl  <- mkStringCLit (moduleUserString (cc_mod cc))
  ; let
     lits = [ zero,   	-- StgInt ccID,
	      label,	-- char *label,
	      modl,	-- char *module,
              zero,	-- StgWord time_ticks
              zero64,	-- StgWord64 mem_alloc
	      subsumed, -- StgInt is_caf
	      zero	-- struct _CostCentre *link
	    ] 
  ; emitDataLits (mkCCLabel cc) lits
  }
  where
	subsumed | isCafCC cc = mkIntCLit (ord 'c')  -- 'c' == is a CAF
		 | otherwise  = mkIntCLit (ord 'B')  -- 'B' == is boring
	    

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

zero = mkIntCLit 0
zero64 = CmmInt 0 I64

sizeof_ccs_words :: Int
sizeof_ccs_words 
    -- round up to the next word.
  | ms == 0   = ws
  | otherwise = ws + 1
  where
   (ws,ms) = SIZEOF_CostCentreStack `divMod` wORD_SIZE

-- ---------------------------------------------------------------------------
-- Registering CCs and CCSs

--   (cc)->link = CC_LIST;
--   CC_LIST = (cc);
--   (cc)->ccID = CC_ID++;

emitRegisterCC :: CostCentre -> Code
emitRegisterCC cc = do
  { tmp <- newTemp cIntRep
  ; stmtsC [
     CmmStore (cmmOffsetB cc_lit oFFSET_CostCentre_link)
		 (CmmLoad cC_LIST wordRep),
     CmmStore cC_LIST cc_lit,
     CmmAssign tmp (CmmLoad cC_ID cIntRep),
     CmmStore (cmmOffsetB cc_lit oFFSET_CostCentre_ccID) (CmmReg tmp),
     CmmStore cC_ID (cmmRegOffB tmp 1)
   ]
  }
  where
    cc_lit = CmmLit (CmmLabel (mkCCLabel cc))

--  (ccs)->prevStack = CCS_LIST;
--  CCS_LIST = (ccs);
--  (ccs)->ccsID = CCS_ID++;

emitRegisterCCS :: CostCentreStack -> Code
emitRegisterCCS ccs = do
  { tmp <- newTemp cIntRep
  ; stmtsC [
     CmmStore (cmmOffsetB ccs_lit oFFSET_CostCentreStack_prevStack) 
			(CmmLoad cCS_LIST wordRep),
     CmmStore cCS_LIST ccs_lit,
     CmmAssign tmp (CmmLoad cCS_ID cIntRep),
     CmmStore (cmmOffsetB ccs_lit oFFSET_CostCentreStack_ccsID) (CmmReg tmp),
     CmmStore cCS_ID (cmmRegOffB tmp 1)
   ]
  }
  where
    ccs_lit = CmmLit (CmmLabel (mkCCSLabel ccs))


cC_LIST = CmmLit (CmmLabel (mkRtsDataLabel SLIT("CC_LIST")))
cC_ID   = CmmLit (CmmLabel (mkRtsDataLabel SLIT("CC_ID")))

cCS_LIST = CmmLit (CmmLabel (mkRtsDataLabel SLIT("CCS_LIST")))
cCS_ID   = CmmLit (CmmLabel (mkRtsDataLabel SLIT("CCS_ID")))

-- ---------------------------------------------------------------------------
-- Set the current cost centre stack

emitSetCCC :: CostCentre -> Code
emitSetCCC cc
  | not opt_SccProfilingOn = nopC
  | otherwise = do 
    tmp <- newTemp wordRep
    ASSERT( sccAbleCostCentre cc )
      pushCostCentre tmp curCCS cc
    stmtC (CmmStore curCCSAddr (CmmReg tmp))
    when (isSccCountCostCentre cc) $ 
	stmtC (bumpSccCount curCCS)

pushCostCentre :: CmmReg -> CmmExpr -> CostCentre -> Code
pushCostCentre result ccs cc
  = emitRtsCallWithResult result PtrHint
	SLIT("PushCostCentre") [(ccs,PtrHint), 
				(CmmLit (mkCCostCentre cc), PtrHint)]

bumpSccCount :: CmmExpr -> CmmStmt
bumpSccCount ccs
  = addToMem REP_CostCentreStack_scc_count
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
    ldv_wd = ldvWord cl_ptr
    new_ldv_wd = cmmOrWord (cmmAndWord (CmmLoad ldv_wd wordRep)
				       (CmmLit (mkWordCLit lDV_CREATE_MASK)))
		 (cmmOrWord loadEra (CmmLit (mkWordCLit lDV_STATE_USE)))

loadEra :: CmmExpr 
loadEra = CmmMachOp (MO_U_Conv cIntRep wordRep)
	  [CmmLoad (mkLblExpr (mkRtsDataLabel SLIT("era"))) cIntRep]

ldvWord :: CmmExpr -> CmmExpr
-- Takes the address of a closure, and returns 
-- the address of the LDV word in the closure
ldvWord closure_ptr = cmmOffsetB closure_ptr oFFSET_StgHeader_ldvw

-- LDV constants, from ghc/includes/Constants.h
lDV_SHIFT	 = (LDV_SHIFT :: Int)
--lDV_STATE_MASK   = (LDV_STATE_MASK :: StgWord)
lDV_CREATE_MASK  = (LDV_CREATE_MASK :: StgWord)
--lDV_LAST_MASK    = (LDV_LAST_MASK :: StgWord)
lDV_STATE_CREATE = (LDV_STATE_CREATE :: StgWord)
lDV_STATE_USE    = (LDV_STATE_USE :: StgWord)

