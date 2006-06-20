%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgMonad.lhs,v 1.45 2005/06/21 10:44:41 simonmar Exp $
%
\section[CgMonad]{The code generation monad}

See the beginning of the top-level @CodeGen@ module, to see how this
monadic stuff fits into the Big Picture.

\begin{code}
module CgMonad (
	Code,	-- type
	FCode,	-- type

	initC, thenC, thenFC, listCs, listFCs, mapCs, mapFCs,
	returnFC, fixC, checkedAbsC, 
	stmtC, stmtsC, labelC, emitStmts, nopC, whenC, newLabelC,
	newUnique, newUniqSupply, 

	CgStmts, emitCgStmts, forkCgStmts, cgStmtsToBlocks,
	getCgStmts', getCgStmts,
	noCgStmts, oneCgStmt, consCgStmt,

	getCmm,
	emitData, emitProc, emitSimpleProc,

	forkLabelledCode,
	forkClosureBody, forkStatics, forkAlts, forkEval,
	forkEvalHelp, forkProc, codeOnly,
	SemiTaggingStuff, ConTagZ,

	EndOfBlockInfo(..),
	setEndOfBlockInfo, getEndOfBlockInfo,

	setSRTLabel, getSRTLabel, 
	setTickyCtrLabel, getTickyCtrLabel,

	StackUsage(..), HeapUsage(..),
	VirtualSpOffset, VirtualHpOffset,
	initStkUsage, initHpUsage,
	getHpUsage,  setHpUsage,
	heapHWM,

	moduleName,

	Sequel(..), -- ToDo: unabstract?

	-- ideally we wouldn't export these, but some other modules access internal state
	getState, setState, getInfoDown, getDynFlags, getHomeModules,

	-- more localised access to monad state	
	getStkUsage, setStkUsage,
	getBinds, setBinds, getStaticBinds,

	-- out of general friendliness, we also export ...
	CgInfoDownwards(..), CgState(..)	-- non-abstract
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} CgBindery ( CgBindings, nukeVolatileBinds )

import DynFlags		( DynFlags )
import Packages		( HomeModules )
import Cmm
import CmmUtils		( CmmStmts, isNopStmt )
import CLabel
import SMRep		( WordOff )
import Module		( Module )
import Id		( Id )
import VarEnv
import OrdList
import Unique		( Unique )
import Util		( mapAccumL )
import UniqSupply	( UniqSupply, mkSplitUniqSupply, splitUniqSupply, uniqFromSupply )
import FastString
import Outputable

import Control.Monad	( liftM )

infixr 9 `thenC`	-- Right-associative!
infixr 9 `thenFC`
\end{code}

%************************************************************************
%*									*
\subsection[CgMonad-environment]{Stuff for manipulating environments}
%*									*
%************************************************************************

This monadery has some information that it only passes {\em
downwards}, as well as some ``state'' which is modified as we go
along.

\begin{code}
data CgInfoDownwards	-- information only passed *downwards* by the monad
  = MkCgInfoDown {
	cgd_dflags  :: DynFlags,
	cgd_hmods   :: HomeModules,	-- Packages we depend on
	cgd_mod     :: Module,		-- Module being compiled
	cgd_statics :: CgBindings,	-- [Id -> info] : static environment
	cgd_srt     :: CLabel,		-- label of the current SRT
	cgd_ticky   :: CLabel,		-- current destination for ticky counts
	cgd_eob     :: EndOfBlockInfo	-- Info for stuff to do at end of basic block:
  }

initCgInfoDown :: DynFlags -> HomeModules -> Module -> CgInfoDownwards
initCgInfoDown dflags hmods mod
  = MkCgInfoDown {	cgd_dflags  = dflags,
			cgd_hmods   = hmods,
			cgd_mod     = mod,
			cgd_statics = emptyVarEnv,
			cgd_srt     = error "initC: srt",
			cgd_ticky   = mkTopTickyCtrLabel,
			cgd_eob     = initEobInfo }

data CgState
  = MkCgState {
     cgs_stmts :: OrdList CgStmt,	  -- Current proc
     cgs_tops  :: OrdList CmmTop,
	-- Other procedures and data blocks in this compilation unit
	-- Both the latter two are ordered only so that we can 
	-- reduce forward references, when it's easy to do so
     
     cgs_binds :: CgBindings,	-- [Id -> info] : *local* bindings environment
     				-- Bindings for top-level things are given in
				-- the info-down part
     
     cgs_stk_usg :: StackUsage,
     cgs_hp_usg  :: HeapUsage,
     
     cgs_uniqs :: UniqSupply }

initCgState :: UniqSupply -> CgState
initCgState uniqs
  = MkCgState { cgs_stmts = nilOL, cgs_tops = nilOL,
		cgs_binds = emptyVarEnv, 
		cgs_stk_usg = initStkUsage, 
		cgs_hp_usg = initHpUsage,
		cgs_uniqs = uniqs }
\end{code}

@EndOfBlockInfo@ tells what to do at the end of this block of code or,
if the expression is a @case@, what to do at the end of each
alternative.

\begin{code}
data EndOfBlockInfo
  = EndOfBlockInfo
	VirtualSpOffset   -- Args Sp: trim the stack to this point at a
			  -- return; push arguments starting just
			  -- above this point on a tail call.
			  
			  -- This is therefore the stk ptr as seen
			  -- by a case alternative.
	Sequel

initEobInfo = EndOfBlockInfo 0 OnStack
\end{code}

Any addressing modes inside @Sequel@ must be ``robust,'' in the sense
that it must survive stack pointer adjustments at the end of the
block.

\begin{code}
data Sequel
  = OnStack 		-- Continuation is on the stack
  | UpdateCode		-- Continuation is update

  | CaseAlts
	  CLabel     -- Jump to this; if the continuation is for a vectored
		     -- case this might be the label of a return vector
	  SemiTaggingStuff
	  Id	      -- The case binder, only used to see if it's dead
	  Bool	      -- True <=> polymorphic, push a SEQ frame too

type SemiTaggingStuff
  = Maybe			-- Maybe[1] we don't have any semi-tagging stuff...
     ([(ConTagZ, CmmLit)],	-- Alternatives
      CmmLit)			-- Default (will be a can't happen RTS label if can't happen)

type ConTagZ = Int	-- A *zero-indexed* contructor tag

-- The case branch is executed only from a successful semitagging
-- venture, when a case has looked at a variable, found that it's
-- evaluated, and wants to load up the contents and go to the join
-- point.
\end{code}

%************************************************************************
%*									*
		CgStmt type
%*									*
%************************************************************************

The CgStmts type is what the code generator outputs: it is a tree of
statements, including in-line labels.  The job of flattenCgStmts is to
turn this into a list of basic blocks, each of which ends in a jump
statement (either a local branch or a non-local jump).

\begin{code}
type CgStmts = OrdList CgStmt

data CgStmt
  = CgStmt  CmmStmt
  | CgLabel BlockId
  | CgFork  BlockId CgStmts

flattenCgStmts :: BlockId -> CgStmts -> [CmmBasicBlock]
flattenCgStmts id stmts = 
	case flatten (fromOL stmts) of
	  ([],blocks)    -> blocks
	  (block,blocks) -> BasicBlock id block : blocks
 where
  flatten [] = ([],[])

  -- A label at the end of a function or fork: this label must not be reachable,
  -- but it might be referred to from another BB that also isn't reachable.
  -- Eliminating these has to be done with a dead-code analysis.  For now,
  -- we just make it into a well-formed block by adding a recursive jump.
  flatten [CgLabel id]
    = ( [CmmBranch id], [BasicBlock id [CmmBranch id]] )

  -- A jump/branch: throw away all the code up to the next label, because
  -- it is unreachable.  Be careful to keep forks that we find on the way.
  flatten (CgStmt stmt : stmts)
    | isJump stmt
    = case dropWhile isOrdinaryStmt stmts of
	[]                     -> ( [stmt], [] )
	[CgLabel id]	       -> ( [stmt], [BasicBlock id [CmmBranch id]])
	(CgLabel id : stmts)   -> ( [stmt], BasicBlock id block : blocks )
	    where (block,blocks) = flatten stmts
	(CgFork fork_id stmts : ss) -> 
	   flatten (CgFork fork_id stmts : CgStmt stmt : ss)

  flatten (s:ss) = 
	case s of
	  CgStmt stmt -> (stmt:block,blocks)
	  CgLabel id  -> ([CmmBranch id],BasicBlock id block:blocks)
	  CgFork fork_id stmts -> 
		(block, BasicBlock fork_id fork_block : fork_blocks ++ blocks)
		where (fork_block, fork_blocks) = flatten (fromOL stmts)
    where (block,blocks) = flatten ss

isJump (CmmJump _ _) = True
isJump (CmmBranch _) = True
isJump _ = False

isOrdinaryStmt (CgStmt _) = True
isOrdinaryStmt _ = False
\end{code}

%************************************************************************
%*									*
		Stack and heap models
%*									*
%************************************************************************

\begin{code}
type VirtualHpOffset = WordOff	-- Both are in
type VirtualSpOffset = WordOff	-- units of words

data StackUsage 
  = StackUsage {
	virtSp :: VirtualSpOffset,
		-- Virtual offset of topmost allocated slot

	frameSp :: VirtualSpOffset,
		-- Virtual offset of the return address of the enclosing frame.
		-- This RA describes the liveness/pointedness of
		-- all the stack from frameSp downwards
		-- INVARIANT: less than or equal to virtSp

	 freeStk :: [VirtualSpOffset], 
		-- List of free slots, in *increasing* order
		-- INVARIANT: all <= virtSp
		-- All slots <= virtSp are taken except these ones

	 realSp :: VirtualSpOffset,	
		-- Virtual offset of real stack pointer register

	 hwSp :: VirtualSpOffset
  }		   -- Highest value ever taken by virtSp

-- INVARIANT: The environment contains no Stable references to
-- 	      stack slots below (lower offset) frameSp
--	      It can contain volatile references to this area though.

data HeapUsage =
  HeapUsage {
	virtHp :: VirtualHpOffset,	-- Virtual offset of highest-allocated word
	realHp :: VirtualHpOffset	-- realHp: Virtual offset of real heap ptr
  }
\end{code}

The heap high water mark is the larger of virtHp and hwHp.  The latter is
only records the high water marks of forked-off branches, so to find the
heap high water mark you have to take the max of virtHp and hwHp.  Remember,
virtHp never retreats!

Note Jan 04: ok, so why do we only look at the virtual Hp??

\begin{code}
heapHWM :: HeapUsage -> VirtualHpOffset
heapHWM = virtHp
\end{code}

Initialisation.

\begin{code}
initStkUsage :: StackUsage
initStkUsage = StackUsage {
			virtSp = 0,
			frameSp = 0,
			freeStk = [],
			realSp = 0,
			hwSp = 0
	       }
		
initHpUsage :: HeapUsage 
initHpUsage = HeapUsage {
	      	virtHp = 0,
		realHp = 0
	      }
\end{code}

@stateIncUsage@$~e_1~e_2$ incorporates in $e_1$ the stack and heap high water
marks found in $e_2$.

\begin{code}
stateIncUsage :: CgState -> CgState -> CgState
stateIncUsage s1 s2@(MkCgState { cgs_stk_usg = stk_usg, cgs_hp_usg = hp_usg })
     = s1 { cgs_hp_usg  = cgs_hp_usg  s1 `maxHpHw`  virtHp hp_usg,
	    cgs_stk_usg = cgs_stk_usg s1 `maxStkHw` hwSp   stk_usg }
       `addCodeBlocksFrom` s2
		
stateIncUsageEval :: CgState -> CgState -> CgState
stateIncUsageEval s1 s2
     = s1 { cgs_stk_usg = cgs_stk_usg s1 `maxStkHw` hwSp (cgs_stk_usg s2) }
       `addCodeBlocksFrom` s2
	-- We don't max the heap high-watermark because stateIncUsageEval is
	-- used only in forkEval, which in turn is only used for blocks of code
	-- which do their own heap-check.

addCodeBlocksFrom :: CgState -> CgState -> CgState
-- Add code blocks from the latter to the former
-- (The cgs_stmts will often be empty, but not always; see codeOnly)
s1 `addCodeBlocksFrom` s2
  = s1 { cgs_stmts = cgs_stmts s1 `appOL` cgs_stmts s2,
	 cgs_tops  = cgs_tops  s1 `appOL` cgs_tops  s2 }

maxHpHw :: HeapUsage -> VirtualHpOffset -> HeapUsage
hp_usg `maxHpHw` hw = hp_usg { virtHp = virtHp hp_usg `max` hw }

maxStkHw :: StackUsage -> VirtualSpOffset -> StackUsage
stk_usg `maxStkHw` hw = stk_usg { hwSp = hwSp stk_usg `max` hw }
\end{code}

%************************************************************************
%*									*
		The FCode monad
%*									*
%************************************************************************

\begin{code}
newtype FCode a = FCode (CgInfoDownwards -> CgState -> (a, CgState))
type Code       = FCode ()

instance Monad FCode where
	(>>=) = thenFC
	return = returnFC

{-# INLINE thenC #-}
{-# INLINE thenFC #-}
{-# INLINE returnFC #-}
\end{code}
The Abstract~C is not in the environment so as to improve strictness.

\begin{code}
initC :: DynFlags -> HomeModules -> Module -> FCode a -> IO a

initC dflags hmods mod (FCode code)
  = do	{ uniqs <- mkSplitUniqSupply 'c'
	; case code (initCgInfoDown dflags hmods mod) (initCgState uniqs) of
	      (res, _) -> return res
	}

returnFC :: a -> FCode a
returnFC val = FCode (\info_down state -> (val, state))
\end{code}

\begin{code}
thenC :: Code -> FCode a -> FCode a
thenC (FCode m) (FCode k) = 
  	FCode (\info_down state -> let (_,new_state) = m info_down state in 
  		k info_down new_state)

listCs :: [Code] -> Code
listCs [] = return ()
listCs (fc:fcs) = do
	fc
	listCs fcs
   	
mapCs :: (a -> Code) -> [a] -> Code
mapCs = mapM_
\end{code}

\begin{code}
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
listFCs = sequence

mapFCs :: (a -> FCode b) -> [a] -> FCode [b]
mapFCs = mapM
\end{code}

And the knot-tying combinator:
\begin{code}
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
\end{code}

%************************************************************************
%*									*
	Operators for getting and setting the state and "info_down".

%*									*
%************************************************************************

\begin{code}
getState :: FCode CgState
getState = FCode $ \info_down state -> (state,state)

setState :: CgState -> FCode ()
setState state = FCode $ \info_down _ -> ((),state)

getStkUsage :: FCode StackUsage
getStkUsage = do
	state <- getState
	return $ cgs_stk_usg state

setStkUsage :: StackUsage -> Code
setStkUsage new_stk_usg = do
	state <- getState
	setState $ state {cgs_stk_usg = new_stk_usg}

getHpUsage :: FCode HeapUsage
getHpUsage = do
	state <- getState
	return $ cgs_hp_usg state
	
setHpUsage :: HeapUsage -> Code
setHpUsage new_hp_usg = do
	state <- getState
	setState $ state {cgs_hp_usg = new_hp_usg}

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

getDynFlags :: FCode DynFlags
getDynFlags = liftM cgd_dflags getInfoDown

getHomeModules :: FCode HomeModules
getHomeModules = liftM cgd_hmods getInfoDown

withInfoDown :: FCode a -> CgInfoDownwards -> FCode a
withInfoDown (FCode fcode) info_down = FCode $ \_ state -> fcode info_down state 

doFCode :: FCode a -> CgInfoDownwards -> CgState -> (a,CgState)
doFCode (FCode fcode) info_down state = fcode info_down state
\end{code}


%************************************************************************
%*									*
		Forking
%*									*
%************************************************************************

@forkClosureBody@ takes a code, $c$, and compiles it in a completely
fresh environment, except that:
	- compilation info and statics are passed in unchanged.
The current environment is passed on completely unaltered, except that
abstract C from the fork is incorporated.

@forkProc@ takes a code and compiles it in the current environment,
returning the basic blocks thus constructed.  The current environment
is passed on completely unchanged.  It is pretty similar to
@getBlocks@, except that the latter does affect the environment.

@forkStatics@ $fc$ compiles $fc$ in an environment whose statics come
from the current bindings, but which is otherwise freshly initialised.
The Abstract~C returned is attached to the current state, but the
bindings and usage information is otherwise unchanged.

\begin{code}
forkClosureBody :: Code -> Code
forkClosureBody body_code
  = do	{ info <- getInfoDown
	; us   <- newUniqSupply
	; state <- getState
   	; let	body_info_down = info { cgd_eob = initEobInfo }
		((),fork_state)	= doFCode body_code body_info_down 
					  (initCgState us)
	; ASSERT( isNilOL (cgs_stmts fork_state) )
	  setState $ state `addCodeBlocksFrom` fork_state }
	
forkStatics :: FCode a -> FCode a
forkStatics body_code
  = do	{ info  <- getInfoDown
	; us    <- newUniqSupply
	; state <- getState
	; let	rhs_info_down = info { cgd_statics = cgs_binds state,
				       cgd_eob     = initEobInfo }
		(result, fork_state_out) = doFCode body_code rhs_info_down 
						   (initCgState us)
	; ASSERT( isNilOL (cgs_stmts fork_state_out) )
	  setState (state `addCodeBlocksFrom` fork_state_out)
	; return result }

forkProc :: Code -> FCode CgStmts
forkProc body_code
  = do	{ info_down <- getInfoDown
	; us    <- newUniqSupply
	; state <- getState
	; let	fork_state_in = (initCgState us) 
					{ cgs_binds   = cgs_binds state,
					  cgs_stk_usg = cgs_stk_usg state,
					  cgs_hp_usg  = cgs_hp_usg state }
			-- ToDo: is the hp usage necesary?
		(code_blks, fork_state_out) = doFCode (getCgStmts body_code) 
						      info_down fork_state_in
	; setState $ state `stateIncUsageEval` fork_state_out
	; return code_blks }

codeOnly :: Code -> Code
-- Emit any code from the inner thing into the outer thing
-- Do not affect anything else in the outer state
-- Used in almost-circular code to prevent false loop dependencies
codeOnly body_code
  = do	{ info_down <- getInfoDown
	; us   <- newUniqSupply
	; state <- getState
	; let	fork_state_in = (initCgState us) { cgs_binds   = cgs_binds state,
					           cgs_stk_usg = cgs_stk_usg state,
					           cgs_hp_usg  = cgs_hp_usg state }
		((), fork_state_out) = doFCode body_code info_down fork_state_in
	; setState $ state `addCodeBlocksFrom` fork_state_out }
\end{code}

@forkAlts@ $bs~d$ takes fcodes $bs$ for the branches of a @case@, and
an fcode for the default case $d$, and compiles each in the current
environment.  The current environment is passed on unmodified, except
that
	- the worst stack high-water mark is incorporated
	- the virtual Hp is moved on to the worst virtual Hp for the branches

\begin{code}
forkAlts :: [FCode a] -> FCode [a]

forkAlts branch_fcodes
  = do	{ info_down <- getInfoDown
	; us <- newUniqSupply
	; state <- getState
	; let compile us branch 
		= (us2, doFCode branch info_down branch_state)
		where
		  (us1,us2) = splitUniqSupply us
	          branch_state = (initCgState us1) {
					cgs_binds   = cgs_binds state,
					cgs_stk_usg = cgs_stk_usg state,
					cgs_hp_usg  = cgs_hp_usg state }

	      (_us, results) = mapAccumL compile us branch_fcodes
	      (branch_results, branch_out_states) = unzip results
	; setState $ foldl stateIncUsage state branch_out_states
		-- NB foldl.  state is the *left* argument to stateIncUsage
	; return branch_results }
\end{code}

@forkEval@ takes two blocks of code.

   -  The first meddles with the environment to set it up as expected by
      the alternatives of a @case@ which does an eval (or gc-possible primop).
   -  The second block is the code for the alternatives.
      (plus info for semi-tagging purposes)

@forkEval@ picks up the virtual stack pointer and returns a suitable
@EndOfBlockInfo@ for the caller to use, together with whatever value
is returned by the second block.

It uses @initEnvForAlternatives@ to initialise the environment, and
@stateIncUsageAlt@ to incorporate usage; the latter ignores the heap
usage.

\begin{code}
forkEval :: EndOfBlockInfo              -- For the body
    	 -> Code			-- Code to set environment
	 -> FCode Sequel		-- Semi-tagging info to store
	 -> FCode EndOfBlockInfo	-- The new end of block info

forkEval body_eob_info env_code body_code
  = do  { (v, sequel) <- forkEvalHelp body_eob_info env_code body_code
 	; returnFC (EndOfBlockInfo v sequel) }

forkEvalHelp :: EndOfBlockInfo  -- For the body
    	     -> Code		-- Code to set environment
	     -> FCode a		-- The code to do after the eval
	     -> FCode (VirtualSpOffset,	-- Sp
		       a)		-- Result of the FCode
	-- A disturbingly complicated function
forkEvalHelp body_eob_info env_code body_code
  = do	{ info_down <- getInfoDown
	; us   <- newUniqSupply
	; state <- getState
	; let { info_down_for_body = info_down {cgd_eob = body_eob_info}
	      ; (_, env_state) = doFCode env_code info_down_for_body 
					 (state {cgs_uniqs = us})
	      ; state_for_body = (initCgState (cgs_uniqs env_state)) 
					{ cgs_binds   = binds_for_body,
	      				  cgs_stk_usg = stk_usg_for_body }
	      ; binds_for_body   = nukeVolatileBinds (cgs_binds env_state)
	      ; stk_usg_from_env = cgs_stk_usg env_state
	      ; virtSp_from_env  = virtSp stk_usg_from_env
	      ; stk_usg_for_body = stk_usg_from_env {realSp = virtSp_from_env,
	      					     hwSp   = virtSp_from_env}
	      ; (value_returned, state_at_end_return)
	        	= doFCode body_code info_down_for_body state_for_body		
	  } 
	; ASSERT( isNilOL (cgs_stmts state_at_end_return) )
		 -- The code coming back should consist only of nested declarations,
		 -- notably of the return vector!
	  setState $ state `stateIncUsageEval` state_at_end_return
	; return (virtSp_from_env, value_returned) }


-- ----------------------------------------------------------------------------
-- Combinators for emitting code

nopC :: Code
nopC = return ()

whenC :: Bool -> Code -> Code
whenC True  code = code
whenC False code = nopC

stmtC :: CmmStmt -> Code
stmtC stmt = emitCgStmt (CgStmt stmt)

labelC :: BlockId -> Code
labelC id = emitCgStmt (CgLabel id)

newLabelC :: FCode BlockId
newLabelC = do { id <- newUnique; return (BlockId id) }

checkedAbsC :: CmmStmt -> Code
-- Emit code, eliminating no-ops
checkedAbsC stmt = emitStmts (if isNopStmt stmt then nilOL
	 		      else unitOL stmt)

stmtsC :: [CmmStmt] -> Code
stmtsC stmts = emitStmts (toOL stmts)

-- Emit code; no no-op checking
emitStmts :: CmmStmts -> Code
emitStmts stmts = emitCgStmts (fmap CgStmt stmts)

-- forkLabelledCode is for emitting a chunk of code with a label, outside
-- of the current instruction stream.
forkLabelledCode :: Code -> FCode BlockId
forkLabelledCode code = getCgStmts code >>= forkCgStmts

emitCgStmt :: CgStmt -> Code
emitCgStmt stmt
  = do	{ state <- getState
	; setState $ state { cgs_stmts = cgs_stmts state `snocOL` stmt }
	}

emitData :: Section -> [CmmStatic] -> Code
emitData sect lits
  = do 	{ state <- getState
	; setState $ state { cgs_tops = cgs_tops state `snocOL` data_block } }
  where
    data_block = CmmData sect lits

emitProc :: [CmmLit] -> CLabel -> [LocalReg] -> [CmmBasicBlock] -> Code
emitProc lits lbl args blocks
  = do  { let proc_block = CmmProc (map CmmStaticLit lits) lbl args blocks
	; state <- getState
	; setState $ state { cgs_tops = cgs_tops state `snocOL` proc_block } }

emitSimpleProc :: CLabel -> Code -> Code
-- Emit a procedure whose body is the specified code; no info table
emitSimpleProc lbl code
  = do	{ stmts <- getCgStmts code
	; blks <- cgStmtsToBlocks stmts
	; emitProc [] lbl [] blks }

getCmm :: Code -> FCode Cmm
-- Get all the CmmTops (there should be no stmts)
getCmm code 
  = do	{ state1 <- getState
	; ((), state2) <- withState code (state1 { cgs_tops  = nilOL })
	; setState $ state2 { cgs_tops = cgs_tops state1 } 
	; return (Cmm (fromOL (cgs_tops state2))) }

-- ----------------------------------------------------------------------------
-- CgStmts

-- These functions deal in terms of CgStmts, which is an abstract type
-- representing the code in the current proc.


-- emit CgStmts into the current instruction stream
emitCgStmts :: CgStmts -> Code
emitCgStmts stmts
  = do	{ state <- getState
	; setState $ state { cgs_stmts = cgs_stmts state `appOL` stmts } }

-- emit CgStmts outside the current instruction stream, and return a label
forkCgStmts :: CgStmts -> FCode BlockId
forkCgStmts stmts
  = do  { id <- newLabelC
	; emitCgStmt (CgFork id stmts)
	; return id
	}

-- turn CgStmts into [CmmBasicBlock], for making a new proc.
cgStmtsToBlocks :: CgStmts -> FCode [CmmBasicBlock]
cgStmtsToBlocks stmts
  = do  { id <- newLabelC
	; return (flattenCgStmts id stmts)
	}	

-- collect the code emitted by an FCode computation
getCgStmts' :: FCode a -> FCode (a, CgStmts)
getCgStmts' fcode
  = do	{ state1 <- getState
	; (a, state2) <- withState fcode (state1 { cgs_stmts = nilOL })
	; setState $ state2 { cgs_stmts = cgs_stmts state1  }
	; return (a, cgs_stmts state2) }

getCgStmts :: FCode a -> FCode CgStmts
getCgStmts fcode = do { (_,stmts) <- getCgStmts' fcode; return stmts }

-- Simple ways to construct CgStmts:
noCgStmts :: CgStmts
noCgStmts = nilOL

oneCgStmt :: CmmStmt -> CgStmts
oneCgStmt stmt = unitOL (CgStmt stmt)

consCgStmt :: CmmStmt -> CgStmts -> CgStmts
consCgStmt stmt stmts = CgStmt stmt `consOL` stmts

-- ----------------------------------------------------------------------------
-- Get the current module name

moduleName :: FCode Module
moduleName = do { info <- getInfoDown; return (cgd_mod info) }

-- ----------------------------------------------------------------------------
-- Get/set the end-of-block info

setEndOfBlockInfo :: EndOfBlockInfo -> Code -> Code
setEndOfBlockInfo eob_info code	= do
	info  <- getInfoDown
	withInfoDown code (info {cgd_eob = eob_info})

getEndOfBlockInfo :: FCode EndOfBlockInfo
getEndOfBlockInfo = do
	info <- getInfoDown
	return (cgd_eob info)

-- ----------------------------------------------------------------------------
-- Get/set the current SRT label

-- There is just one SRT for each top level binding; all the nested
-- bindings use sub-sections of this SRT.  The label is passed down to
-- the nested bindings via the monad.

getSRTLabel :: FCode CLabel	-- Used only by cgPanic
getSRTLabel = do info  <- getInfoDown
		 return (cgd_srt info)

setSRTLabel :: CLabel -> FCode a -> FCode a
setSRTLabel srt_lbl code
  = do  info <- getInfoDown
	withInfoDown code (info { cgd_srt = srt_lbl})

-- ----------------------------------------------------------------------------
-- Get/set the current ticky counter label

getTickyCtrLabel :: FCode CLabel
getTickyCtrLabel = do
	info <- getInfoDown
	return (cgd_ticky info)

setTickyCtrLabel :: CLabel -> Code -> Code
setTickyCtrLabel ticky code = do
	info <- getInfoDown
	withInfoDown code (info {cgd_ticky = ticky})
\end{code}
