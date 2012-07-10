{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
--
-- Monad for Stg to C-- code generation
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

module StgCmmMonad (
	FCode,	-- type

        initC, runC, thenC, thenFC, listCs, listFCs, mapCs, mapFCs,
	returnFC, fixC, fixC_, nopC, whenC, 
	newUnique, newUniqSupply, 

        newLabelC, emitLabel,

	emit, emitDecl, emitProc, emitProcWithConvention, emitSimpleProc,
        emitOutOfLine, emitAssign, emitStore, emitComment,

	getCmm, cgStmtsToBlocks,
	getCodeR, getCode, getHeapUsage,

        mkCmmIfThenElse, mkCmmIfThen, mkCmmIfGoto,
        mkCall, mkCmmCall, mkSafeCall,

        forkClosureBody, forkStatics, forkAlts, forkProc, codeOnly,

	ConTagZ,

	Sequel(..),
	withSequel, getSequel,

	setSRTLabel, getSRTLabel, 
	setTickyCtrLabel, getTickyCtrLabel,

	withUpdFrameOff, getUpdFrameOff, initUpdFrameOff,

	HeapUsage(..), VirtualHpOffset,	initHpUsage,
	getHpUsage,  setHpUsage, heapHWM,
	setVirtHp, getVirtHp, setRealHp,

	getModuleName,

	-- ideally we wouldn't export these, but some other modules access internal state
	getState, setState, getInfoDown, getDynFlags, getThisPackage,

	-- more localised access to monad state	
	CgIdInfo(..), CgLoc(..),
	getBinds, setBinds, getStaticBinds,

	-- out of general friendliness, we also export ...
	CgInfoDownwards(..), CgState(..)	-- non-abstract
    ) where

#include "HsVersions.h"

import Cmm
import StgCmmClosure
import DynFlags
import MkGraph
import BlockId
import CLabel
import SMRep
import Module
import Id
import VarEnv
import OrdList
import Unique
import UniqSupply
import FastString
import Outputable

import Control.Monad
import Data.List
import Prelude hiding( sequence, succ )
import qualified Prelude( sequence )

infixr 9 `thenC`	-- Right-associative!
infixr 9 `thenFC`


--------------------------------------------------------
--	The FCode monad and its types
--------------------------------------------------------

newtype FCode a = FCode (CgInfoDownwards -> CgState -> (a, CgState))

instance Monad FCode where
	(>>=) = thenFC
	return = returnFC

{-# INLINE thenC #-}
{-# INLINE thenFC #-}
{-# INLINE returnFC #-}

initC :: IO CgState
initC  = do { uniqs <- mkSplitUniqSupply 'c'
            ; return (initCgState uniqs) }

runC :: DynFlags -> Module -> CgState -> FCode a -> (a,CgState)
runC dflags mod st (FCode code) = code (initCgInfoDown dflags mod) st

returnFC :: a -> FCode a
returnFC val = FCode (\_info_down state -> (val, state))

thenC :: FCode () -> FCode a -> FCode a
thenC (FCode m) (FCode k) = 
  	FCode (\info_down state -> let (_,new_state) = m info_down state in 
  		k info_down new_state)

nopC :: FCode ()
nopC = return ()

whenC :: Bool -> FCode () -> FCode ()
whenC True  code  = code
whenC False _code = nopC

listCs :: [FCode ()] -> FCode ()
listCs [] = return ()
listCs (fc:fcs) = do
	fc
	listCs fcs
   	
mapCs :: (a -> FCode ()) -> [a] -> FCode ()
mapCs = mapM_

thenFC	:: FCode a -> (a -> FCode c) -> FCode c
thenFC (FCode m) k = FCode (
	\info_down state ->
		let 
			(m_result, new_state) = m info_down state
			(FCode kcode) = k m_result
		in 
			kcode info_down new_state
	)

listFCs :: [FCode a] -> FCode [a]
listFCs = Prelude.sequence

mapFCs :: (a -> FCode b) -> [a] -> FCode [b]
mapFCs = mapM

fixC :: (a -> FCode a) -> FCode a
fixC fcode = FCode (
	\info_down state -> 
		let
			FCode fc = fcode v
			result@(v,_) = fc info_down state
			--	    ^--------^
		in
			result
	)

fixC_ :: (a -> FCode a) -> FCode ()
fixC_ fcode = fixC fcode >> return ()

--------------------------------------------------------
--	The code generator environment
--------------------------------------------------------

-- This monadery has some information that it only passes 
-- *downwards*, as well as some ``state'' which is modified 
-- as we go along.

data CgInfoDownwards	-- information only passed *downwards* by the monad
  = MkCgInfoDown {
	cgd_dflags     :: DynFlags,
	cgd_mod        :: Module,	  -- Module being compiled
	cgd_statics    :: CgBindings,	  -- [Id -> info] : static environment
	cgd_srt_lbl    :: CLabel,	  -- Label of the current top-level SRT
	cgd_updfr_off  :: UpdFrameOffset, -- Size of current update frame
	cgd_ticky      :: CLabel,	  -- Current destination for ticky counts
	cgd_sequel     :: Sequel	  -- What to do at end of basic block
  }

type CgBindings = IdEnv CgIdInfo

data CgIdInfo
  = CgIdInfo	
	{ cg_id :: Id	-- Id that this is the info for
			-- Can differ from the Id at occurrence sites by 
			-- virtue of being externalised, for splittable C
	, cg_lf  :: LambdaFormInfo 
	, cg_loc :: CgLoc		     -- CmmExpr for the *tagged* value
        , cg_tag :: {-# UNPACK #-} !DynTag   -- Cache for (lfDynTag cg_lf)
        }

data CgLoc
  = CmmLoc CmmExpr	-- A stable CmmExpr; that is, one not mentioning
			-- Hp, so that it remains valid across calls

  | LneLoc BlockId [LocalReg]  	   -- A join point
	-- A join point (= let-no-escape) should only 
	-- be tail-called, and in a saturated way.
	-- To tail-call it, assign to these locals, 
	-- and branch to the block id

instance Outputable CgIdInfo where
  ppr (CgIdInfo { cg_id = id, cg_loc = loc })
    = ppr id <+> ptext (sLit "-->") <+> ppr loc

instance Outputable CgLoc where
  ppr (CmmLoc e)    = ptext (sLit "cmm") <+> ppr e
  ppr (LneLoc b rs) = ptext (sLit "lne") <+> ppr b <+> ppr rs


-- Sequel tells what to do with the result of this expression
data Sequel
  = Return Bool		  -- Return result(s) to continuation found on the stack
			  -- 	True <=> the continuation is update code (???)

  | AssignTo 
	[LocalReg]	-- Put result(s) in these regs and fall through
			-- 	NB: no void arguments here
        Bool            -- Should we adjust the heap pointer back to recover
                        -- space that's unused on this path?
                        -- We need to do this only if the expression may
                        -- allocate (e.g. it's a foreign call or allocating primOp)
instance Show Sequel where
  show (Return _) = "Sequel: Return"
  show (AssignTo _ _) = "Sequel: Assign"

initCgInfoDown :: DynFlags -> Module -> CgInfoDownwards
initCgInfoDown dflags mod
  = MkCgInfoDown {	cgd_dflags    = dflags,
			cgd_mod       = mod,
			cgd_statics   = emptyVarEnv,
			cgd_srt_lbl   = error "initC: srt_lbl",
			cgd_updfr_off = initUpdFrameOff,
			cgd_ticky     = mkTopTickyCtrLabel,
			cgd_sequel    = initSequel }

initSequel :: Sequel
initSequel = Return False

initUpdFrameOff :: UpdFrameOffset
initUpdFrameOff = widthInBytes wordWidth -- space for the RA


--------------------------------------------------------
--	The code generator state
--------------------------------------------------------

data CgState
  = MkCgState {
     cgs_stmts :: CmmAGraph,	  -- Current procedure

     cgs_tops  :: OrdList CmmDecl,
	-- Other procedures and data blocks in this compilation unit
	-- Both are ordered only so that we can 
	-- reduce forward references, when it's easy to do so
     
     cgs_binds :: CgBindings,	-- [Id -> info] : *local* bindings environment
     				-- Bindings for top-level things are given in
				-- the info-down part

     cgs_hp_usg  :: HeapUsage,

     cgs_uniqs :: UniqSupply }

data HeapUsage =
  HeapUsage {
	virtHp :: VirtualHpOffset,	-- Virtual offset of highest-allocated word
	       	  			--   Incremented whenever we allocate
	realHp :: VirtualHpOffset	-- realHp: Virtual offset of real heap ptr
	       	  			--   Used in instruction addressing modes
  }

type VirtualHpOffset = WordOff



initCgState :: UniqSupply -> CgState
initCgState uniqs
  = MkCgState { cgs_stmts      = mkNop, cgs_tops = nilOL,
		cgs_binds      = emptyVarEnv, 
		cgs_hp_usg     = initHpUsage,
		cgs_uniqs      = uniqs }

stateIncUsage :: CgState -> CgState -> CgState
-- stateIncUsage@ e1 e2 incorporates in e1 
-- the heap high water mark found in e2.
stateIncUsage s1 s2@(MkCgState { cgs_hp_usg = hp_usg })
     = s1 { cgs_hp_usg  = cgs_hp_usg  s1 `maxHpHw`  virtHp hp_usg }
       `addCodeBlocksFrom` s2
		
addCodeBlocksFrom :: CgState -> CgState -> CgState
-- Add code blocks from the latter to the former
-- (The cgs_stmts will often be empty, but not always; see codeOnly)
s1 `addCodeBlocksFrom` s2
  = s1 { cgs_stmts = cgs_stmts s1 <*> cgs_stmts s2,
	 cgs_tops  = cgs_tops  s1 `appOL` cgs_tops  s2 }


-- The heap high water mark is the larger of virtHp and hwHp.  The latter is
-- only records the high water marks of forked-off branches, so to find the
-- heap high water mark you have to take the max of virtHp and hwHp.  Remember,
-- virtHp never retreats!
-- 
-- Note Jan 04: ok, so why do we only look at the virtual Hp??

heapHWM :: HeapUsage -> VirtualHpOffset
heapHWM = virtHp

initHpUsage :: HeapUsage 
initHpUsage = HeapUsage { virtHp = 0, realHp = 0 }

maxHpHw :: HeapUsage -> VirtualHpOffset -> HeapUsage
hp_usg `maxHpHw` hw = hp_usg { virtHp = virtHp hp_usg `max` hw }

--------------------------------------------------------
-- Operators for getting and setting the state and "info_down".
--------------------------------------------------------

getState :: FCode CgState
getState = FCode $ \_info_down state -> (state,state)

setState :: CgState -> FCode ()
setState state = FCode $ \_info_down _ -> ((),state)

getHpUsage :: FCode HeapUsage
getHpUsage = do
	state <- getState
	return $ cgs_hp_usg state
	
setHpUsage :: HeapUsage -> FCode ()
setHpUsage new_hp_usg = do
	state <- getState
	setState $ state {cgs_hp_usg = new_hp_usg}

setVirtHp :: VirtualHpOffset -> FCode ()
setVirtHp new_virtHp
  = do	{ hp_usage <- getHpUsage
	; setHpUsage (hp_usage {virtHp = new_virtHp}) }

getVirtHp :: FCode VirtualHpOffset
getVirtHp 
  = do	{ hp_usage <- getHpUsage
	; return (virtHp hp_usage) }

setRealHp ::  VirtualHpOffset -> FCode ()
setRealHp new_realHp
  = do	{ hp_usage <- getHpUsage
	; setHpUsage (hp_usage {realHp = new_realHp}) }

getBinds :: FCode CgBindings
getBinds = do
	state <- getState
	return $ cgs_binds state
	
setBinds :: CgBindings -> FCode ()
setBinds new_binds = do
	state <- getState
	setState $ state {cgs_binds = new_binds}

getStaticBinds :: FCode CgBindings
getStaticBinds = do
	info  <- getInfoDown
	return (cgd_statics info)

withState :: FCode a -> CgState -> FCode (a,CgState)
withState (FCode fcode) newstate = FCode $ \info_down state -> 
	let (retval, state2) = fcode info_down newstate in ((retval,state2), state)

newUniqSupply :: FCode UniqSupply
newUniqSupply = do
	state <- getState
	let (us1, us2) = splitUniqSupply (cgs_uniqs state)
	setState $ state { cgs_uniqs = us1 }
	return us2

newUnique :: FCode Unique
newUnique = do
	us <- newUniqSupply
	return (uniqFromSupply us)

------------------
getInfoDown :: FCode CgInfoDownwards
getInfoDown = FCode $ \info_down state -> (info_down,state)

instance HasDynFlags FCode where
    getDynFlags = liftM cgd_dflags getInfoDown

getThisPackage :: FCode PackageId
getThisPackage = liftM thisPackage getDynFlags

withInfoDown :: FCode a -> CgInfoDownwards -> FCode a
withInfoDown (FCode fcode) info_down = FCode $ \_ state -> fcode info_down state 

doFCode :: FCode a -> CgInfoDownwards -> CgState -> (a,CgState)
doFCode (FCode fcode) info_down state = fcode info_down state


-- ----------------------------------------------------------------------------
-- Get the current module name

getModuleName :: FCode Module
getModuleName = do { info <- getInfoDown; return (cgd_mod info) }

-- ----------------------------------------------------------------------------
-- Get/set the end-of-block info

withSequel :: Sequel -> FCode () -> FCode ()
withSequel sequel code
  = do	{ info  <- getInfoDown
	; withInfoDown code (info {cgd_sequel = sequel }) }

getSequel :: FCode Sequel
getSequel = do  { info <- getInfoDown
		; return (cgd_sequel info) }

-- ----------------------------------------------------------------------------
-- Get/set the current SRT label

-- There is just one SRT for each top level binding; all the nested
-- bindings use sub-sections of this SRT.  The label is passed down to
-- the nested bindings via the monad.

getSRTLabel :: FCode CLabel	-- Used only by cgPanic
getSRTLabel = do info  <- getInfoDown
		 return (cgd_srt_lbl info)

setSRTLabel :: CLabel -> FCode a -> FCode a
setSRTLabel srt_lbl code
  = do  info <- getInfoDown
	withInfoDown code (info { cgd_srt_lbl = srt_lbl})

-- ----------------------------------------------------------------------------
-- Get/set the size of the update frame

-- We keep track of the size of the update frame so that we
-- can set the stack pointer to the proper address on return
-- (or tail call) from the closure.
-- There should be at most one update frame for each closure.
-- Note: I'm including the size of the original return address
-- in the size of the update frame -- hence the default case on `get'.

withUpdFrameOff :: UpdFrameOffset -> FCode () -> FCode ()
withUpdFrameOff size code
  = do	{ info  <- getInfoDown
	; withInfoDown code (info {cgd_updfr_off = size }) }

getUpdFrameOff :: FCode UpdFrameOffset
getUpdFrameOff
  = do	{ info  <- getInfoDown
	; return $ cgd_updfr_off info }

-- ----------------------------------------------------------------------------
-- Get/set the current ticky counter label

getTickyCtrLabel :: FCode CLabel
getTickyCtrLabel = do
	info <- getInfoDown
	return (cgd_ticky info)

setTickyCtrLabel :: CLabel -> FCode () -> FCode ()
setTickyCtrLabel ticky code = do
	info <- getInfoDown
	withInfoDown code (info {cgd_ticky = ticky})


--------------------------------------------------------
-- 		Forking
--------------------------------------------------------

forkClosureBody :: FCode () -> FCode ()
-- forkClosureBody takes a code, $c$, and compiles it in a 
-- fresh environment, except that:
--	- compilation info and statics are passed in unchanged.
--	- local bindings are passed in unchanged
--	  (it's up to the enclosed code to re-bind the
--	   free variables to a field of the closure)
-- 
-- The current state is passed on completely unaltered, except that
-- C-- from the fork is incorporated.

forkClosureBody body_code
  = do	{ info <- getInfoDown
	; us   <- newUniqSupply
	; state <- getState
   	; let	body_info_down = info { cgd_sequel    = initSequel
                                      , cgd_updfr_off = initUpdFrameOff }
		fork_state_in = (initCgState us) { cgs_binds = cgs_binds state }
		((),fork_state_out)
		    = doFCode body_code body_info_down fork_state_in
	; setState $ state `addCodeBlocksFrom` fork_state_out }
	
forkStatics :: FCode a -> FCode a
-- @forkStatics@ $fc$ compiles $fc$ in an environment whose *statics* come
-- from the current *local bindings*, but which is otherwise freshly initialised.
-- The Abstract~C returned is attached to the current state, but the
-- bindings and usage information is otherwise unchanged.
forkStatics body_code
  = do	{ info  <- getInfoDown
	; us    <- newUniqSupply
	; state <- getState
	; let	rhs_info_down = info { cgd_statics = cgs_binds state
				     , cgd_sequel  = initSequel 
			             , cgd_updfr_off = initUpdFrameOff }
		(result, fork_state_out) = doFCode body_code rhs_info_down 
						   (initCgState us)
	; setState (state `addCodeBlocksFrom` fork_state_out)
	; return result }

forkProc :: FCode a -> FCode a
-- 'forkProc' takes a code and compiles it in the *current* environment,
-- returning the graph thus constructed. 
--
-- The current environment is passed on completely unchanged to
-- the successor.  In particular, any heap usage from the enclosed
-- code is discarded; it should deal with its own heap consumption
forkProc body_code
  = do	{ info_down <- getInfoDown
	; us    <- newUniqSupply
	; state <- getState
   	; let	info_down' = info_down -- { cgd_sequel = initSequel }
                fork_state_in = (initCgState us) { cgs_binds = cgs_binds state }
		(result, fork_state_out) = doFCode body_code info_down' fork_state_in
  	; setState $ state `addCodeBlocksFrom` fork_state_out
	; return result }

codeOnly :: FCode () -> FCode ()
-- Emit any code from the inner thing into the outer thing
-- Do not affect anything else in the outer state
-- Used in almost-circular code to prevent false loop dependencies
codeOnly body_code
  = do	{ info_down <- getInfoDown
	; us   <- newUniqSupply
	; state <- getState
	; let	fork_state_in = (initCgState us) { cgs_binds   = cgs_binds state,
					           cgs_hp_usg  = cgs_hp_usg state }
		((), fork_state_out) = doFCode body_code info_down fork_state_in
	; setState $ state `addCodeBlocksFrom` fork_state_out }

forkAlts :: [FCode a] -> FCode [a]
-- (forkAlts' bs d) takes fcodes 'bs' for the branches of a 'case', and
-- an fcode for the default case 'd', and compiles each in the current
-- environment.  The current environment is passed on unmodified, except
-- that the virtual Hp is moved on to the worst virtual Hp for the branches

forkAlts branch_fcodes
  = do  { info_down <- getInfoDown
        ; us <- newUniqSupply
        ; state <- getState
        ; let compile us branch
                = (us2, doFCode branch info_down branch_state)
                where
                  (us1,us2) = splitUniqSupply us
                  branch_state = (initCgState us1) {
                                        cgs_binds   = cgs_binds state,
                                        cgs_hp_usg  = cgs_hp_usg state }

              (_us, results) = mapAccumL compile us branch_fcodes
              (branch_results, branch_out_states) = unzip results
        ; setState $ foldl stateIncUsage state branch_out_states
                -- NB foldl.  state is the *left* argument to stateIncUsage
        ; return branch_results }

-- collect the code emitted by an FCode computation
getCodeR :: FCode a -> FCode (a, CmmAGraph)
getCodeR fcode
  = do	{ state1 <- getState
	; (a, state2) <- withState fcode (state1 { cgs_stmts = mkNop })
	; setState $ state2 { cgs_stmts = cgs_stmts state1  }
	; return (a, cgs_stmts state2) }

getCode :: FCode a -> FCode CmmAGraph
getCode fcode = do { (_,stmts) <- getCodeR fcode; return stmts }

-- 'getHeapUsage' applies a function to the amount of heap that it uses.
-- It initialises the heap usage to zeros, and passes on an unchanged
-- heap usage. 
--
-- It is usually a prelude to performing a GC check, so everything must
-- be in a tidy and consistent state.
-- 
-- Note the slightly subtle fixed point behaviour needed here

getHeapUsage :: (VirtualHpOffset -> FCode a) -> FCode a
getHeapUsage fcode
  = do	{ info_down <- getInfoDown
	; state <- getState
	; let	fstate_in = state { cgs_hp_usg  = initHpUsage }
		(r, fstate_out) = doFCode (fcode hp_hw) info_down fstate_in
		hp_hw = heapHWM (cgs_hp_usg fstate_out)	-- Loop here!
		
	; setState $ fstate_out { cgs_hp_usg = cgs_hp_usg state }
	; return r }

-- ----------------------------------------------------------------------------
-- Combinators for emitting code

emitCgStmt :: CgStmt -> FCode ()
emitCgStmt stmt
  = do  { state <- getState
        ; setState $ state { cgs_stmts = cgs_stmts state `snocOL` stmt }
        }

emitLabel :: BlockId -> FCode ()
emitLabel id = emitCgStmt (CgLabel id)

emitComment :: FastString -> FCode ()
#if 0 /* def DEBUG */
emitComment s = emitCgStmt (CgStmt (CmmComment s))
#else
emitComment _ = return ()
#endif

emitAssign :: CmmReg  -> CmmExpr -> FCode ()
emitAssign l r = emitCgStmt (CgStmt (CmmAssign l r))

emitStore :: CmmExpr  -> CmmExpr -> FCode ()
emitStore l r = emitCgStmt (CgStmt (CmmStore l r))


newLabelC :: FCode BlockId
newLabelC = do { u <- newUnique
               ; return $ mkBlockId u }

emit :: CmmAGraph -> FCode ()
emit ag
  = do	{ state <- getState
	; setState $ state { cgs_stmts = cgs_stmts state <*> ag } }

emitDecl :: CmmDecl -> FCode ()
emitDecl decl
  = do 	{ state <- getState
	; setState $ state { cgs_tops = cgs_tops state `snocOL` decl } }

emitOutOfLine :: BlockId -> CmmAGraph -> FCode ()
emitOutOfLine l stmts = emitCgStmt (CgFork l stmts)

emitProcWithConvention :: Convention -> CmmInfoTable -> CLabel -> [CmmFormal] ->
                          CmmAGraph -> FCode ()
emitProcWithConvention conv info lbl args blocks
  = do  { us <- newUniqSupply
        ; let (offset, entry) = mkCallEntry conv args
              blks = initUs_ us $ lgraphOfAGraph $ entry <*> blocks
        ; let sinfo = StackInfo {arg_space = offset, updfr_space = Just initUpdFrameOff}
              proc_block = CmmProc (TopInfo {info_tbl=info, stack_info=sinfo}) lbl blks
        ; state <- getState
        ; setState $ state { cgs_tops = cgs_tops state `snocOL` proc_block } }

emitProc :: CmmInfoTable -> CLabel -> [CmmFormal] -> CmmAGraph -> FCode ()
emitProc = emitProcWithConvention NativeNodeCall

emitSimpleProc :: CLabel -> CmmAGraph -> FCode ()
emitSimpleProc lbl code = 
  emitProc CmmNonInfoTable lbl [] code

getCmm :: FCode () -> FCode CmmGroup
-- Get all the CmmTops (there should be no stmts)
-- Return a single Cmm which may be split from other Cmms by
-- object splitting (at a later stage)
getCmm code 
  = do	{ state1 <- getState
	; ((), state2) <- withState code (state1 { cgs_tops  = nilOL })
	; setState $ state2 { cgs_tops = cgs_tops state1 } 
        ; return (fromOL (cgs_tops state2)) }


mkCmmIfThenElse :: CmmExpr -> CmmAGraph -> CmmAGraph -> FCode CmmAGraph
mkCmmIfThenElse e tbranch fbranch = do
  endif <- newLabelC
  tid   <- newLabelC
  fid   <- newLabelC
  return $ mkCbranch e tid fid <*>
            mkLabel tid <*> tbranch <*> mkBranch endif <*>
            mkLabel fid <*> fbranch <*> mkLabel endif

mkCmmIfGoto :: CmmExpr -> BlockId -> FCode CmmAGraph
mkCmmIfGoto e tid = do
  endif <- newLabelC
  return $ mkCbranch e tid endif <*> mkLabel endif

mkCmmIfThen :: CmmExpr -> CmmAGraph -> FCode CmmAGraph
mkCmmIfThen e tbranch = do
  endif <- newLabelC
  tid <- newLabelC
  return $ mkCbranch e tid endif <*>
         mkLabel tid <*> tbranch <*> mkLabel endif


mkCall :: CmmExpr -> (Convention, Convention) -> [CmmFormal] -> [CmmActual]
       -> UpdFrameOffset -> (ByteOff,[(CmmExpr,ByteOff)]) -> FCode CmmAGraph
mkCall f (callConv, retConv) results actuals updfr_off extra_stack = do
  k <- newLabelC
  let area = Young k
      (off, copyin) = copyInOflow retConv area results
      copyout = mkCallReturnsTo f callConv actuals k off updfr_off extra_stack
  return (copyout <*> mkLabel k <*> copyin)

mkCmmCall :: CmmExpr -> [CmmFormal] -> [CmmActual] -> UpdFrameOffset
          -> FCode CmmAGraph
mkCmmCall f results actuals updfr_off
   = mkCall f (NativeDirectCall, NativeReturn) results actuals updfr_off (0,[])


mkSafeCall :: ForeignTarget -> [CmmFormal] -> [CmmActual]
           -> UpdFrameOffset -> Bool
           -> FCode CmmAGraph
mkSafeCall   t fs as upd i = do
  k <- newLabelC
  let (_off, copyout) = copyInOflow NativeReturn (Young k) fs
    -- see Note [safe foreign call convention]
  return
     (    mkStore (CmmStackSlot (Young k) (widthInBytes wordWidth))
                  (CmmLit (CmmBlock k))
      <*> mkLast (CmmForeignCall { tgt=t, res=fs, args=as, succ=k
                                 , updfr=upd, intrbl=i })
      <*> mkLabel k
      <*> copyout
     )

-- ----------------------------------------------------------------------------
-- CgStmts

-- These functions deal in terms of CgStmts, which is an abstract type
-- representing the code in the current proc.

-- turn CgStmts into [CmmBasicBlock], for making a new proc.
cgStmtsToBlocks :: CmmAGraph -> FCode CmmGraph
cgStmtsToBlocks stmts
  = do  { us <- newUniqSupply
	; return (initUs_ us (lgraphOfAGraph stmts)) }	
