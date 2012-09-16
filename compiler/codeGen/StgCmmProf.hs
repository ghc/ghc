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

module StgCmmProf (
	initCostCentres, ccType, ccsType,
	mkCCostCentre, mkCCostCentreStack,

	-- Cost-centre Profiling
	dynProfHdr, profDynAlloc, profAlloc, staticProfHdr, initUpdFrameProf,
        enterCostCentreThunk, enterCostCentreFun,
        costCentreFrom,
        curCCS, storeCurCCS,
        emitSetCCC,

	saveCurrentCostCentre, restoreCurrentCostCentre,

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

import StgCmmClosure
import StgCmmUtils
import StgCmmMonad
import SMRep

import MkGraph
import Cmm
import CmmUtils
import CLabel

import qualified Module
import CostCentre
import DynFlags
import FastString
import Module
import Outputable

import Control.Monad
import Data.Char (ord)

-----------------------------------------------------------------------------
--
-- Cost-centre-stack Profiling
--
-----------------------------------------------------------------------------

-- Expression representing the current cost centre stack
ccsType :: DynFlags -> CmmType -- Type of a cost-centre stack
ccsType = bWord

ccType :: DynFlags -> CmmType -- Type of a cost centre
ccType = bWord

curCCS :: CmmExpr
curCCS = CmmReg (CmmGlobal CCCS)

storeCurCCS :: CmmExpr -> CmmAGraph
storeCurCCS e = mkAssign (CmmGlobal CCCS) e

mkCCostCentre :: CostCentre -> CmmLit
mkCCostCentre cc = CmmLabel (mkCCLabel cc)

mkCCostCentreStack :: CostCentreStack -> CmmLit
mkCCostCentreStack ccs = CmmLabel (mkCCSLabel ccs)

costCentreFrom :: DynFlags
               -> CmmExpr 	-- A closure pointer
	       -> CmmExpr	-- The cost centre from that closure
costCentreFrom dflags cl = CmmLoad (cmmOffsetB dflags cl (oFFSET_StgHeader_ccs dflags)) (ccsType dflags)

staticProfHdr :: DynFlags -> CostCentreStack -> [CmmLit]
-- The profiling header words in a static closure
-- Was SET_STATIC_PROF_HDR
staticProfHdr dflags ccs
 = ifProfilingL dflags [mkCCostCentreStack ccs, staticLdvInit dflags]

dynProfHdr :: DynFlags -> CmmExpr -> [CmmExpr]
-- Profiling header words in a dynamic closure
dynProfHdr dflags ccs = ifProfilingL dflags [ccs, dynLdvInit dflags]

initUpdFrameProf :: ByteOff -> FCode ()
-- Initialise the profiling field of an update frame
initUpdFrameProf frame_off
  = ifProfiling $	-- frame->header.prof.ccs = CCCS
    do dflags <- getDynFlags
       emitStore (CmmStackSlot Old (frame_off - oFFSET_StgHeader_ccs dflags))
                 curCCS
	-- frame->header.prof.hp.rs = NULL (or frame-header.prof.hp.ldvw = 0) 
	-- is unnecessary because it is not used anyhow.

---------------------------------------------------------------------------
-- 	Saving and restoring the current cost centre
---------------------------------------------------------------------------

{-	Note [Saving the current cost centre]
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The current cost centre is like a global register.  Like other 
global registers, it's a caller-saves one.  But consider
	case (f x) of (p,q) -> rhs
Since 'f' may set the cost centre, we must restore it 
before resuming rhs.  So we want code like this:
	local_cc = CCC  -- save
	r = f( x )
	CCC = local_cc  -- restore
That is, we explicitly "save" the current cost centre in
a LocalReg, local_cc; and restore it after the call. The
C-- infrastructure will arrange to save local_cc across the
call. 

The same goes for join points;
	let j x = join-stuff
	in blah-blah
We want this kind of code:
	local_cc = CCC  -- save
	blah-blah
     J: 
        CCC = local_cc  -- restore
-}

saveCurrentCostCentre :: FCode (Maybe LocalReg)
	-- Returns Nothing if profiling is off
saveCurrentCostCentre
  = do dflags <- getDynFlags
       if not (dopt Opt_SccProfilingOn dflags)
           then return Nothing
           else do local_cc <- newTemp (ccType dflags)
                   emitAssign (CmmLocal local_cc) curCCS
                   return (Just local_cc)

restoreCurrentCostCentre :: Maybe LocalReg -> FCode ()
restoreCurrentCostCentre Nothing 
  = return ()
restoreCurrentCostCentre (Just local_cc)
  = emit (storeCurCCS (CmmReg (CmmLocal local_cc)))


-------------------------------------------------------------------------------
-- Recording allocation in a cost centre
-------------------------------------------------------------------------------

-- | Record the allocation of a closure.  The CmmExpr is the cost
-- centre stack to which to attribute the allocation.
profDynAlloc :: SMRep -> CmmExpr -> FCode ()
profDynAlloc rep ccs
  = ifProfiling $
    do dflags <- getDynFlags
       profAlloc (mkIntExpr dflags (heapClosureSize dflags rep)) ccs

-- | Record the allocation of a closure (size is given by a CmmExpr)
-- The size must be in words, because the allocation counter in a CCS counts
-- in words.
profAlloc :: CmmExpr -> CmmExpr -> FCode ()
profAlloc words ccs
  = ifProfiling $
        do dflags <- getDynFlags
           emit (addToMemE alloc_rep
                       (cmmOffsetB dflags ccs (oFFSET_CostCentreStack_mem_alloc dflags))
                       (CmmMachOp (MO_UU_Conv (wordWidth dflags) (typeWidth alloc_rep)) $
                         [CmmMachOp (mo_wordSub dflags) [words,
                                                         mkIntExpr dflags (profHdrSize dflags)]]))
                       -- subtract the "profiling overhead", which is the
                       -- profiling header in a closure.
 where 
        alloc_rep =  REP_CostCentreStack_mem_alloc

-- -----------------------------------------------------------------------
-- Setting the current cost centre on entry to a closure

enterCostCentreThunk :: CmmExpr -> FCode ()
enterCostCentreThunk closure = 
  ifProfiling $ do
      dflags <- getDynFlags
      emit $ storeCurCCS (costCentreFrom dflags closure)

enterCostCentreFun :: CostCentreStack -> CmmExpr -> FCode ()
enterCostCentreFun ccs closure =
  ifProfiling $ do
    if isCurrentCCS ccs
       then do dflags <- getDynFlags
               emitRtsCall rtsPackageId (fsLit "enterFunCCS")
                   [(CmmReg (CmmGlobal BaseReg), AddrHint),
                    (costCentreFrom dflags closure, AddrHint)] False
       else return () -- top-level function, nothing to do

ifProfiling :: FCode () -> FCode ()
ifProfiling code
  = do dflags <- getDynFlags
       if dopt Opt_SccProfilingOn dflags
           then code
           else nopC

ifProfilingL :: DynFlags -> [a] -> [a]
ifProfilingL dflags xs
  | dopt Opt_SccProfilingOn dflags = xs
  | otherwise                      = []


---------------------------------------------------------------
--	Initialising Cost Centres & CCSs
---------------------------------------------------------------

initCostCentres :: CollectedCCs -> FCode ()
-- Emit the declarations
initCostCentres (local_CCs, ___extern_CCs, singleton_CCSs)
  = do dflags <- getDynFlags
       whenC (dopt Opt_SccProfilingOn dflags) $
           do mapM_ emitCostCentreDecl local_CCs
              mapM_ emitCostCentreStackDecl singleton_CCSs


emitCostCentreDecl :: CostCentre -> FCode ()
emitCostCentreDecl cc = do 
  { dflags <- getDynFlags
  ; let is_caf | isCafCC cc = mkIntCLit dflags (ord 'c') -- 'c' == is a CAF
               | otherwise  = zero dflags
                        -- NB. bytesFS: we want the UTF-8 bytes here (#5559)
  ; label <- newByteStringCLit (bytesFS $ costCentreUserNameFS cc)
  ; modl  <- newByteStringCLit (bytesFS $ Module.moduleNameFS
                                        $ Module.moduleName
                                        $ cc_mod cc)
  ; loc <- newByteStringCLit $ bytesFS $ mkFastString $
                   showPpr dflags (costCentreSrcSpan cc)
           -- XXX going via FastString to get UTF-8 encoding is silly
  ; let
     lits = [ zero dflags,   	-- StgInt ccID,
	      label,	-- char *label,
	      modl,	-- char *module,
              loc,      -- char *srcloc,
              zero64,   -- StgWord64 mem_alloc
              zero dflags,     -- StgWord time_ticks
              is_caf,   -- StgInt is_caf
              zero dflags      -- struct _CostCentre *link
	    ] 
  ; emitDataLits (mkCCLabel cc) lits
  }

emitCostCentreStackDecl :: CostCentreStack -> FCode ()
emitCostCentreStackDecl ccs 
  = case maybeSingletonCCS ccs of
    Just cc ->
        do dflags <- getDynFlags
           let mk_lits cc = zero dflags :
                            mkCCostCentre cc :
                            replicate (sizeof_ccs_words dflags - 2) (zero dflags)
                -- Note: to avoid making any assumptions about how the
                -- C compiler (that compiles the RTS, in particular) does
                -- layouts of structs containing long-longs, simply
                -- pad out the struct with zero words until we hit the
                -- size of the overall struct (which we get via DerivedConstants.h)
           emitDataLits (mkCCSLabel ccs) (mk_lits cc)
    Nothing -> pprPanic "emitCostCentreStackDecl" (ppr ccs)

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
   (ws,ms) = SIZEOF_CostCentreStack `divMod` wORD_SIZE dflags

-- ---------------------------------------------------------------------------
-- Set the current cost centre stack

emitSetCCC :: CostCentre -> Bool -> Bool -> FCode ()
emitSetCCC cc tick push
 = do dflags <- getDynFlags
      if not (dopt Opt_SccProfilingOn dflags)
          then nopC
          else do tmp <- newTemp (ccsType dflags) -- TODO FIXME NOW
                  pushCostCentre tmp curCCS cc
                  when tick $ emit (bumpSccCount dflags (CmmReg (CmmLocal tmp)))
                  when push $ emit (storeCurCCS (CmmReg (CmmLocal tmp)))

pushCostCentre :: LocalReg -> CmmExpr -> CostCentre -> FCode ()
pushCostCentre result ccs cc
  = emitRtsCallWithResult result AddrHint
	rtsPackageId
        (fsLit "pushCostCentre") [(ccs,AddrHint),
				(CmmLit (mkCCostCentre cc), AddrHint)]
        False

bumpSccCount :: DynFlags -> CmmExpr -> CmmAGraph
bumpSccCount dflags ccs
  = addToMem REP_CostCentreStack_scc_count
	 (cmmOffsetB dflags ccs (oFFSET_CostCentreStack_scc_count dflags)) 1

-----------------------------------------------------------------------------
--
--		Lag/drag/void stuff
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
      CmmMachOp (mo_wordShl dflags) [loadEra dflags, mkIntExpr dflags lDV_SHIFT ],
      CmmLit (mkWordCLit dflags lDV_STATE_CREATE)
  ]
        
--
-- Initialise the LDV word of a new closure
--
ldvRecordCreate :: CmmExpr -> FCode ()
ldvRecordCreate closure = do dflags <- getDynFlags
                             emit $ mkStore (ldvWord dflags closure) (dynLdvInit dflags)

--
-- Called when a closure is entered, marks the closure as having been "used".
-- The closure is not an 'inherently used' one.
-- The closure is not IND or IND_OLDGEN because neither is considered for LDV
-- profiling.
--
ldvEnterClosure :: ClosureInfo -> FCode ()
ldvEnterClosure closure_info = do dflags <- getDynFlags
                                  ldvEnter (cmmOffsetB dflags (CmmReg nodeReg) (-tag))
  where tag = funTag closure_info
        -- don't forget to substract node's tag
  
ldvEnter :: CmmExpr -> FCode ()
-- Argument is a closure pointer
ldvEnter cl_ptr = do
    dflags <- getDynFlags
    let -- don't forget to substract node's tag
        ldv_wd = ldvWord dflags cl_ptr
        new_ldv_wd = cmmOrWord dflags (cmmAndWord dflags (CmmLoad ldv_wd (bWord dflags))
                                                         (CmmLit (mkWordCLit dflags lDV_CREATE_MASK)))
                                      (cmmOrWord dflags (loadEra dflags) (CmmLit (mkWordCLit dflags lDV_STATE_USE)))
    ifProfiling $
         -- if (era > 0) {
         --    LDVW((c)) = (LDVW((c)) & LDV_CREATE_MASK) |
         --                era | LDV_STATE_USE }
        emit =<< mkCmmIfThenElse (CmmMachOp (mo_wordUGt dflags) [loadEra dflags, CmmLit (zeroCLit dflags)])
                     (mkStore ldv_wd new_ldv_wd)
                     mkNop

loadEra :: DynFlags -> CmmExpr
loadEra dflags = CmmMachOp (MO_UU_Conv cIntWidth (wordWidth dflags))
	  [CmmLoad (mkLblExpr (mkCmmDataLabel rtsPackageId (fsLit "era"))) cInt]

ldvWord :: DynFlags -> CmmExpr -> CmmExpr
-- Takes the address of a closure, and returns 
-- the address of the LDV word in the closure
ldvWord dflags closure_ptr
    = cmmOffsetB dflags closure_ptr (oFFSET_StgHeader_ldvw dflags)

-- LDV constants, from ghc/includes/Constants.h
lDV_SHIFT :: Int
lDV_SHIFT = LDV_SHIFT
--lDV_STATE_MASK :: StgWord
--lDV_STATE_MASK   = LDV_STATE_MASK
lDV_CREATE_MASK :: StgWord
lDV_CREATE_MASK  = LDV_CREATE_MASK
--lDV_LAST_MASK :: StgWord
--lDV_LAST_MASK    = LDV_LAST_MASK
lDV_STATE_CREATE :: StgWord
lDV_STATE_CREATE = LDV_STATE_CREATE
lDV_STATE_USE :: StgWord
lDV_STATE_USE    = LDV_STATE_USE

