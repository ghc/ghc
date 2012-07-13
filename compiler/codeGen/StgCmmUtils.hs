-----------------------------------------------------------------------------
--
-- Code generator utilities; mostly monadic
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

module StgCmmUtils (
	cgLit, mkSimpleLit,
	emitDataLits, mkDataLits,
        emitRODataLits, mkRODataLits,
        emitRtsCall, emitRtsCallWithVols, emitRtsCallWithResult, emitRtsCallGen,
        assignTemp, newTemp,

	newUnboxedTupleRegs,

        emitMultiAssign, emitCmmLitSwitch, emitSwitch,

	tagToClosure, mkTaggedObjectLoad,

        callerSaves, callerSaveVolatileRegs, get_GlobalReg_addr,

	cmmAndWord, cmmOrWord, cmmNegate, cmmEqWord, cmmNeWord,
        cmmUGtWord, cmmSubWord, cmmMulWord, cmmAddWord, cmmUShrWord,
	cmmOffsetExprW, cmmOffsetExprB,
	cmmRegOffW, cmmRegOffB,
	cmmLabelOffW, cmmLabelOffB,
	cmmOffsetW, cmmOffsetB,
	cmmOffsetLitW, cmmOffsetLitB,
	cmmLoadIndexW,
        cmmConstrTag, cmmConstrTag1,

        cmmUntag, cmmIsTagged, cmmGetTag,

	addToMem, addToMemE, addToMemLbl,
	mkWordCLit,
	newStringCLit, newByteStringCLit,
	packHalfWordsCLit,
        blankWord,

        srt_escape
  ) where

#include "HsVersions.h"
#include "../includes/stg/MachRegs.h"

import StgCmmMonad
import StgCmmClosure
import Cmm
import BlockId
import MkGraph
import CLabel
import CmmUtils

import ForeignCall
import IdInfo
import Type
import TyCon
import Constants
import SMRep
import Module
import Literal
import Digraph
import ListSetOps
import Util
import Unique
import DynFlags
import FastString
import Outputable

import Data.Char
import Data.List
import Data.Ord
import Data.Word
import Data.Maybe


-------------------------------------------------------------------------
--
--	Literals
--
-------------------------------------------------------------------------

cgLit :: Literal -> FCode CmmLit
cgLit (MachStr s) = newByteStringCLit (bytesFS s)
 -- not unpackFS; we want the UTF-8 byte stream.
cgLit other_lit   = return (mkSimpleLit other_lit)

mkLtOp :: Literal -> MachOp
-- On signed literals we must do a signed comparison
mkLtOp (MachInt _)    = MO_S_Lt wordWidth
mkLtOp (MachFloat _)  = MO_F_Lt W32
mkLtOp (MachDouble _) = MO_F_Lt W64
mkLtOp lit	      = MO_U_Lt (typeWidth (cmmLitType (mkSimpleLit lit)))
				-- ToDo: seems terribly indirect!

mkSimpleLit :: Literal -> CmmLit
mkSimpleLit (MachChar	c)    = CmmInt (fromIntegral (ord c)) wordWidth
mkSimpleLit MachNullAddr      = zeroCLit
mkSimpleLit (MachInt i)       = CmmInt i wordWidth
mkSimpleLit (MachInt64 i)     = CmmInt i W64
mkSimpleLit (MachWord i)      = CmmInt i wordWidth
mkSimpleLit (MachWord64 i)    = CmmInt i W64
mkSimpleLit (MachFloat r)     = CmmFloat r W32
mkSimpleLit (MachDouble r)    = CmmFloat r W64
mkSimpleLit (MachLabel fs ms fod) 
	= CmmLabel (mkForeignLabel fs ms labelSrc fod)
	where
		-- TODO: Literal labels might not actually be in the current package...
		labelSrc = ForeignLabelInThisPackage	
mkSimpleLit other	      = pprPanic "mkSimpleLit" (ppr other)

--------------------------------------------------------------------------
--
-- Incrementing a memory location
--
--------------------------------------------------------------------------

addToMemLbl :: CmmType -> CLabel -> Int -> CmmAGraph
addToMemLbl rep lbl n = addToMem rep (CmmLit (CmmLabel lbl)) n

addToMem :: CmmType 	-- rep of the counter
	 -> CmmExpr	-- Address
	 -> Int		-- What to add (a word)
	 -> CmmAGraph
addToMem rep ptr n = addToMemE rep ptr (CmmLit (CmmInt (toInteger n) (typeWidth rep)))

addToMemE :: CmmType 	-- rep of the counter
	  -> CmmExpr	-- Address
	  -> CmmExpr	-- What to add (a word-typed expression)
	  -> CmmAGraph
addToMemE rep ptr n
  = mkStore ptr (CmmMachOp (MO_Add (typeWidth rep)) [CmmLoad ptr rep, n])


-------------------------------------------------------------------------
--
--	Loading a field from an object, 
--	where the object pointer is itself tagged
--
-------------------------------------------------------------------------

mkTaggedObjectLoad :: LocalReg -> LocalReg -> WordOff -> DynTag -> CmmAGraph
-- (loadTaggedObjectField reg base off tag) generates assignment
-- 	reg = bitsK[ base + off - tag ]
-- where K is fixed by 'reg'
mkTaggedObjectLoad reg base offset tag
  = mkAssign (CmmLocal reg)  
	     (CmmLoad (cmmOffsetB (CmmReg (CmmLocal base))
				  (wORD_SIZE*offset - tag))
                      (localRegType reg))

-------------------------------------------------------------------------
--
--	Converting a closure tag to a closure for enumeration types
--      (this is the implementation of tagToEnum#).
--
-------------------------------------------------------------------------

tagToClosure :: TyCon -> CmmExpr -> CmmExpr
tagToClosure tycon tag
  = CmmLoad (cmmOffsetExprW closure_tbl tag) bWord
  where closure_tbl = CmmLit (CmmLabel lbl)
	lbl = mkClosureTableLabel (tyConName tycon) NoCafRefs

-------------------------------------------------------------------------
--
--	Conditionals and rts calls
--
-------------------------------------------------------------------------

emitRtsCall :: PackageId -> FastString -> [(CmmExpr,ForeignHint)] -> Bool -> FCode ()
emitRtsCall pkg fun args safe = emitRtsCallGen [] pkg fun args Nothing safe
   -- The 'Nothing' says "save all global registers"

emitRtsCallWithVols :: PackageId -> FastString -> [(CmmExpr,ForeignHint)] -> [GlobalReg] -> Bool -> FCode ()
emitRtsCallWithVols pkg fun args vols safe
   = emitRtsCallGen [] pkg fun args (Just vols) safe

emitRtsCallWithResult :: LocalReg -> ForeignHint -> PackageId -> FastString
	-> [(CmmExpr,ForeignHint)] -> Bool -> FCode ()
emitRtsCallWithResult res hint pkg fun args safe
   = emitRtsCallGen [(res,hint)] pkg fun args Nothing safe

-- Make a call to an RTS C procedure
emitRtsCallGen
   :: [(LocalReg,ForeignHint)]
   -> PackageId
   -> FastString
   -> [(CmmExpr,ForeignHint)]
   -> Maybe [GlobalReg]
   -> Bool -- True <=> CmmSafe call
   -> FCode ()
emitRtsCallGen res pkg fun args _vols safe
  = do { updfr_off <- getUpdFrameOff
       ; emit caller_save
       ; call updfr_off
       ; emit caller_load }
  where
    call updfr_off =
      if safe then
        emit =<< mkCmmCall fun_expr res' args' updfr_off
      else
        emit $ mkUnsafeCall (ForeignTarget fun_expr
                         (ForeignConvention CCallConv arg_hints res_hints)) res' args'
    (args', arg_hints) = unzip args
    (res',  res_hints) = unzip res
    (caller_save, caller_load) = callerSaveVolatileRegs
    fun_expr = mkLblExpr (mkCmmCodeLabel pkg fun)


-----------------------------------------------------------------------------
--
--	Caller-Save Registers
--
-----------------------------------------------------------------------------

-- Here we generate the sequence of saves/restores required around a
-- foreign call instruction.

-- TODO: reconcile with includes/Regs.h
--  * Regs.h claims that BaseReg should be saved last and loaded first
--    * This might not have been tickled before since BaseReg is callee save
--  * Regs.h saves SparkHd, ParkT1, SparkBase and SparkLim
--
-- This code isn't actually used right now, because callerSaves
-- only ever returns true in the current universe for registers NOT in
-- system_regs (just do a grep for CALLER_SAVES in
-- includes/stg/MachRegs.h).  It's all one giant no-op, and for
-- good reason: having to save system registers on every foreign call
-- would be very expensive, so we avoid assigning them to those
-- registers when we add support for an architecture.
--
-- Note that the old code generator actually does more work here: it
-- also saves other global registers.  We can't (nor want) to do that
-- here, as we don't have liveness information.  And really, we
-- shouldn't be doing the workaround at this point in the pipeline, see
-- Note [Register parameter passing] and the ToDo on CmmCall in
-- cmm/CmmNode.hs.  Right now the workaround is to avoid inlining across
-- unsafe foreign calls in rewriteAssignments, but this is strictly
-- temporary.
callerSaveVolatileRegs :: (CmmAGraph, CmmAGraph)
callerSaveVolatileRegs = (caller_save, caller_load)
  where
    caller_save = catAGraphs (map callerSaveGlobalReg    regs_to_save)
    caller_load = catAGraphs (map callerRestoreGlobalReg regs_to_save)

    system_regs = [ Sp,SpLim,Hp,HpLim,CCCS,CurrentTSO,CurrentNursery
		    {- ,SparkHd,SparkTl,SparkBase,SparkLim -}
		  , BaseReg ]

    regs_to_save = filter callerSaves system_regs

    callerSaveGlobalReg reg
	= mkStore (get_GlobalReg_addr reg) (CmmReg (CmmGlobal reg))

    callerRestoreGlobalReg reg
	= mkAssign (CmmGlobal reg)
		    (CmmLoad (get_GlobalReg_addr reg) (globalRegType reg))

-- -----------------------------------------------------------------------------
-- Global registers

-- We map STG registers onto appropriate CmmExprs.  Either they map
-- to real machine registers or stored as offsets from BaseReg.  Given
-- a GlobalReg, get_GlobalReg_addr always produces the 
-- register table address for it.
-- (See also get_GlobalReg_reg_or_addr in MachRegs)

get_GlobalReg_addr              :: GlobalReg -> CmmExpr
get_GlobalReg_addr BaseReg = regTableOffset 0
get_GlobalReg_addr mid     = get_Regtable_addr_from_offset 
				(globalRegType mid) (baseRegOffset mid)

-- Calculate a literal representing an offset into the register table.
-- Used when we don't have an actual BaseReg to offset from.
regTableOffset :: Int -> CmmExpr
regTableOffset n = 
  CmmLit (CmmLabelOff mkMainCapabilityLabel (oFFSET_Capability_r + n))

get_Regtable_addr_from_offset :: CmmType -> Int -> CmmExpr
get_Regtable_addr_from_offset _rep offset =
#ifdef REG_Base
  CmmRegOff (CmmGlobal BaseReg) offset
#else
  regTableOffset offset
#endif


-- | Returns 'True' if this global register is stored in a caller-saves
-- machine register.

callerSaves :: GlobalReg -> Bool

#ifdef CALLER_SAVES_Base
callerSaves BaseReg		= True
#endif
#ifdef CALLER_SAVES_R1
callerSaves (VanillaReg 1 _)	= True
#endif
#ifdef CALLER_SAVES_R2
callerSaves (VanillaReg 2 _)	= True
#endif
#ifdef CALLER_SAVES_R3
callerSaves (VanillaReg 3 _)	= True
#endif
#ifdef CALLER_SAVES_R4
callerSaves (VanillaReg 4 _)	= True
#endif
#ifdef CALLER_SAVES_R5
callerSaves (VanillaReg 5 _)	= True
#endif
#ifdef CALLER_SAVES_R6
callerSaves (VanillaReg 6 _)	= True
#endif
#ifdef CALLER_SAVES_R7
callerSaves (VanillaReg 7 _)	= True
#endif
#ifdef CALLER_SAVES_R8
callerSaves (VanillaReg 8 _)	= True
#endif
#ifdef CALLER_SAVES_R9
callerSaves (VanillaReg 9 _)	= True
#endif
#ifdef CALLER_SAVES_R10
callerSaves (VanillaReg 10 _)	= True
#endif
#ifdef CALLER_SAVES_F1
callerSaves (FloatReg 1)	= True
#endif
#ifdef CALLER_SAVES_F2
callerSaves (FloatReg 2)	= True
#endif
#ifdef CALLER_SAVES_F3
callerSaves (FloatReg 3)	= True
#endif
#ifdef CALLER_SAVES_F4
callerSaves (FloatReg 4)	= True
#endif
#ifdef CALLER_SAVES_D1
callerSaves (DoubleReg 1)	= True
#endif
#ifdef CALLER_SAVES_D2
callerSaves (DoubleReg 2)	= True
#endif
#ifdef CALLER_SAVES_L1
callerSaves (LongReg 1)		= True
#endif
#ifdef CALLER_SAVES_Sp
callerSaves Sp			= True
#endif
#ifdef CALLER_SAVES_SpLim
callerSaves SpLim		= True
#endif
#ifdef CALLER_SAVES_Hp
callerSaves Hp			= True
#endif
#ifdef CALLER_SAVES_HpLim
callerSaves HpLim		= True
#endif
#ifdef CALLER_SAVES_CCCS
callerSaves CCCS                = True
#endif
#ifdef CALLER_SAVES_CurrentTSO
callerSaves CurrentTSO		= True
#endif
#ifdef CALLER_SAVES_CurrentNursery
callerSaves CurrentNursery	= True
#endif
callerSaves _			= False


-- -----------------------------------------------------------------------------
-- Information about global registers

baseRegOffset :: GlobalReg -> Int

baseRegOffset Sp		  = oFFSET_StgRegTable_rSp
baseRegOffset SpLim		  = oFFSET_StgRegTable_rSpLim
baseRegOffset (LongReg 1)         = oFFSET_StgRegTable_rL1
baseRegOffset Hp		  = oFFSET_StgRegTable_rHp
baseRegOffset HpLim		  = oFFSET_StgRegTable_rHpLim
baseRegOffset CCCS                = oFFSET_StgRegTable_rCCCS
baseRegOffset CurrentTSO          = oFFSET_StgRegTable_rCurrentTSO
baseRegOffset CurrentNursery	  = oFFSET_StgRegTable_rCurrentNursery
baseRegOffset HpAlloc		  = oFFSET_StgRegTable_rHpAlloc
baseRegOffset GCEnter1		  = oFFSET_stgGCEnter1
baseRegOffset GCFun		  = oFFSET_stgGCFun
baseRegOffset reg		  = pprPanic "baseRegOffset:" (ppr reg)

-------------------------------------------------------------------------
--
--	Strings generate a top-level data block
--
-------------------------------------------------------------------------

emitDataLits :: CLabel -> [CmmLit] -> FCode ()
-- Emit a data-segment data block
emitDataLits lbl lits = emitDecl (mkDataLits Data lbl lits)

emitRODataLits :: CLabel -> [CmmLit] -> FCode ()
-- Emit a read-only data block
emitRODataLits lbl lits = emitDecl (mkRODataLits lbl lits)

newStringCLit :: String -> FCode CmmLit
-- Make a global definition for the string,
-- and return its label
newStringCLit str = newByteStringCLit (map (fromIntegral . ord) str)

newByteStringCLit :: [Word8] -> FCode CmmLit
newByteStringCLit bytes
  = do 	{ uniq <- newUnique
	; let (lit, decl) = mkByteStringCLit uniq bytes
	; emitDecl decl
	; return lit }

-------------------------------------------------------------------------
--
--	Assigning expressions to temporaries
--
-------------------------------------------------------------------------

assignTemp :: CmmExpr -> FCode LocalReg
-- Make sure the argument is in a local register.
-- We don't bother being particularly aggressive with avoiding
-- unnecessary local registers, since we can rely on a later
-- optimization pass to inline as necessary (and skipping out
-- on things like global registers can be a little dangerous
-- due to them being trashed on foreign calls--though it means
-- the optimization pass doesn't have to do as much work)
assignTemp (CmmReg (CmmLocal reg)) = return reg
assignTemp e = do { uniq <- newUnique
		  ; let reg = LocalReg uniq (cmmExprType e)
                  ; emitAssign (CmmLocal reg) e
		  ; return reg }

newTemp :: CmmType -> FCode LocalReg
newTemp rep = do { uniq <- newUnique
		 ; return (LocalReg uniq rep) }

newUnboxedTupleRegs :: Type -> FCode ([LocalReg], [ForeignHint])
-- Choose suitable local regs to use for the components
-- of an unboxed tuple that we are about to return to 
-- the Sequel.  If the Sequel is a join point, using the
-- regs it wants will save later assignments.
newUnboxedTupleRegs res_ty 
  = ASSERT( isUnboxedTupleType res_ty )
    do	{ sequel <- getSequel
	; regs <- choose_regs sequel
	; ASSERT( regs `equalLength` reps )
	  return (regs, map primRepForeignHint reps) }
  where
    UbxTupleRep ty_args = repType res_ty
    reps = [ rep
	   | ty <- ty_args
    	   , let rep = typePrimRep ty
  	   , not (isVoidRep rep) ]
    choose_regs (AssignTo regs _) = return regs
    choose_regs _other		  = mapM (newTemp . primRepCmmType) reps



-------------------------------------------------------------------------
--      emitMultiAssign
-------------------------------------------------------------------------

emitMultiAssign :: [LocalReg] -> [CmmExpr] -> FCode ()
-- Emit code to perform the assignments in the
-- input simultaneously, using temporary variables when necessary.

type Key  = Int
type Vrtx = (Key, Stmt)	-- Give each vertex a unique number,
			-- for fast comparison
type Stmt = (LocalReg, CmmExpr)	-- r := e

-- We use the strongly-connected component algorithm, in which
--	* the vertices are the statements
--	* an edge goes from s1 to s2 iff
--		s1 assigns to something s2 uses
--	  that is, if s1 should *follow* s2 in the final order

emitMultiAssign []    []    = return ()
emitMultiAssign [reg] [rhs] = emitAssign (CmmLocal reg) rhs
emitMultiAssign regs  rhss  = ASSERT( equalLength regs rhss )
                              unscramble ([1..] `zip` (regs `zip` rhss))

unscramble :: [Vrtx] -> FCode ()
unscramble vertices = mapM_ do_component components
  where
	edges :: [ (Vrtx, Key, [Key]) ]
	edges = [ (vertex, key1, edges_from stmt1)
		| vertex@(key1, stmt1) <- vertices ]

	edges_from :: Stmt -> [Key]
	edges_from stmt1 = [ key2 | (key2, stmt2) <- vertices, 
				    stmt1 `mustFollow` stmt2 ]

	components :: [SCC Vrtx]
	components = stronglyConnCompFromEdgedVertices edges

	-- do_components deal with one strongly-connected component
	-- Not cyclic, or singleton?  Just do it
        do_component :: SCC Vrtx -> FCode ()
        do_component (AcyclicSCC (_,stmt))  = mk_graph stmt
	do_component (CyclicSCC []) 	    = panic "do_component"
	do_component (CyclicSCC [(_,stmt)]) = mk_graph stmt

		-- Cyclic?  Then go via temporaries.  Pick one to
		-- break the loop and try again with the rest.
        do_component (CyclicSCC ((_,first_stmt) : rest)) = do
            u <- newUnique
	    let (to_tmp, from_tmp) = split u first_stmt
            mk_graph to_tmp
            unscramble rest
            mk_graph from_tmp

	split :: Unique -> Stmt -> (Stmt, Stmt)
	split uniq (reg, rhs)
	  = ((tmp, rhs), (reg, CmmReg (CmmLocal tmp)))
	  where
	    rep = cmmExprType rhs
	    tmp = LocalReg uniq rep

        mk_graph :: Stmt -> FCode ()
        mk_graph (reg, rhs) = emitAssign (CmmLocal reg) rhs

mustFollow :: Stmt -> Stmt -> Bool
(reg, _) `mustFollow` (_, rhs) = CmmLocal reg `regUsedIn` rhs

-------------------------------------------------------------------------
--	mkSwitch
-------------------------------------------------------------------------


emitSwitch :: CmmExpr  		-- Tag to switch on
	   -> [(ConTagZ, CmmAGraph)]	-- Tagged branches
	   -> Maybe CmmAGraph	    	-- Default branch (if any)
	   -> ConTagZ -> ConTagZ	-- Min and Max possible values; behaviour
	    			        -- 	outside this range is undefined
	   -> FCode ()
emitSwitch tag_expr branches mb_deflt lo_tag hi_tag
  = do	{ dflags <- getDynFlags
        ; mkCmmSwitch (via_C dflags) tag_expr branches mb_deflt lo_tag hi_tag }
  where
    via_C dflags | HscC <- hscTarget dflags = True
		 | otherwise                = False


mkCmmSwitch :: Bool			-- True <=> never generate a conditional tree
	    -> CmmExpr  		-- Tag to switch on
	    -> [(ConTagZ, CmmAGraph)]	-- Tagged branches
	    -> Maybe CmmAGraph	    	-- Default branch (if any)
	    -> ConTagZ -> ConTagZ	-- Min and Max possible values; behaviour
	    			        -- 	outside this range is undefined
            -> FCode ()

-- First, two rather common cases in which there is no work to do
mkCmmSwitch _ _ []         (Just code) _ _ = emit code
mkCmmSwitch _ _ [(_,code)] Nothing     _ _ = emit code

-- Right, off we go
mkCmmSwitch via_C tag_expr branches mb_deflt lo_tag hi_tag = do
    join_lbl      <- newLabelC
    mb_deflt_lbl  <- label_default join_lbl mb_deflt
    branches_lbls <- label_branches join_lbl branches
    tag_expr'     <- assignTemp' tag_expr
    
    emit =<< mk_switch tag_expr' (sortBy (comparing fst) branches_lbls)
                mb_deflt_lbl lo_tag hi_tag via_C

          -- Sort the branches before calling mk_switch

    emitLabel join_lbl

mk_switch :: CmmExpr -> [(ConTagZ, BlockId)]
	  -> Maybe BlockId 
	  -> ConTagZ -> ConTagZ -> Bool
          -> FCode CmmAGraph

-- SINGLETON TAG RANGE: no case analysis to do
mk_switch _tag_expr [(tag, lbl)] _ lo_tag hi_tag _via_C
  | lo_tag == hi_tag
  = ASSERT( tag == lo_tag )
    return (mkBranch lbl)

-- SINGLETON BRANCH, NO DEFAULT: no case analysis to do
mk_switch _tag_expr [(_tag,lbl)] Nothing _ _ _
  = return (mkBranch lbl)
	-- The simplifier might have eliminated a case
	-- 	 so we may have e.g. case xs of 
	--				 [] -> e
	-- In that situation we can be sure the (:) case 
	-- can't happen, so no need to test

-- SINGLETON BRANCH: one equality check to do
mk_switch tag_expr [(tag,lbl)] (Just deflt) _ _ _
  = return (mkCbranch cond deflt lbl)
  where
    cond =  cmmNeWord tag_expr (CmmLit (mkIntCLit tag))
	-- We have lo_tag < hi_tag, but there's only one branch, 
	-- so there must be a default

-- ToDo: we might want to check for the two branch case, where one of
-- the branches is the tag 0, because comparing '== 0' is likely to be
-- more efficient than other kinds of comparison.

-- DENSE TAG RANGE: use a switch statment.
--
-- We also use a switch uncoditionally when compiling via C, because
-- this will get emitted as a C switch statement and the C compiler
-- should do a good job of optimising it.  Also, older GCC versions
-- (2.95 in particular) have problems compiling the complicated
-- if-trees generated by this code, so compiling to a switch every
-- time works around that problem.
--
mk_switch tag_expr branches mb_deflt lo_tag hi_tag via_C
  | use_switch 	-- Use a switch
  = let 
	find_branch :: ConTagZ -> Maybe BlockId
	find_branch i = case (assocMaybe branches i) of
			  Just lbl -> Just lbl
			  Nothing  -> mb_deflt

	-- NB. we have eliminated impossible branches at
	-- either end of the range (see below), so the first
	-- tag of a real branch is real_lo_tag (not lo_tag).
	arms :: [Maybe BlockId]
	arms = [ find_branch i | i <- [real_lo_tag..real_hi_tag]]
    in
    return (mkSwitch (cmmOffset tag_expr (- real_lo_tag)) arms)

  -- if we can knock off a bunch of default cases with one if, then do so
  | Just deflt <- mb_deflt, (lowest_branch - lo_tag) >= n_branches
  = do stmts <- mk_switch tag_expr branches mb_deflt
                        lowest_branch hi_tag via_C
       mkCmmIfThenElse
	(cmmULtWord tag_expr (CmmLit (mkIntCLit lowest_branch)))
	(mkBranch deflt)
        stmts

  | Just deflt <- mb_deflt, (hi_tag - highest_branch) >= n_branches
  = do stmts <- mk_switch tag_expr branches mb_deflt
                        lo_tag highest_branch via_C
       mkCmmIfThenElse
	(cmmUGtWord tag_expr (CmmLit (mkIntCLit highest_branch)))
	(mkBranch deflt)
        stmts

  | otherwise	-- Use an if-tree
  = do lo_stmts <- mk_switch tag_expr lo_branches mb_deflt
                             lo_tag (mid_tag-1) via_C
       hi_stmts <- mk_switch tag_expr hi_branches mb_deflt
                             mid_tag hi_tag via_C
       mkCmmIfThenElse
	(cmmUGeWord tag_expr (CmmLit (mkIntCLit mid_tag)))
        hi_stmts
        lo_stmts
	-- we test (e >= mid_tag) rather than (e < mid_tag), because
	-- the former works better when e is a comparison, and there
	-- are two tags 0 & 1 (mid_tag == 1).  In this case, the code
	-- generator can reduce the condition to e itself without
	-- having to reverse the sense of the comparison: comparisons
	-- can't always be easily reversed (eg. floating
	-- pt. comparisons).
  where
    use_switch 	 = {- pprTrace "mk_switch" (
			ppr tag_expr <+> text "n_tags:" <+> int n_tags <+>
                        text "branches:" <+> ppr (map fst branches) <+>
			text "n_branches:" <+> int n_branches <+>
			text "lo_tag:" <+> int lo_tag <+>
			text "hi_tag:" <+> int hi_tag <+>
			text "real_lo_tag:" <+> int real_lo_tag <+>
			text "real_hi_tag:" <+> int real_hi_tag) $ -}
		   ASSERT( n_branches > 1 && n_tags > 1 ) 
		   n_tags > 2 && (via_C || (dense && big_enough))
		 -- up to 4 branches we use a decision tree, otherwise
                 -- a switch (== jump table in the NCG).  This seems to be
                 -- optimal, and corresponds with what gcc does.
    big_enough 	 = n_branches > 4
    dense      	 = n_branches > (n_tags `div` 2)
    n_branches   = length branches
    
    -- ignore default slots at each end of the range if there's 
    -- no default branch defined.
    lowest_branch  = fst (head branches)
    highest_branch = fst (last branches)

    real_lo_tag
	| isNothing mb_deflt = lowest_branch
	| otherwise          = lo_tag

    real_hi_tag
	| isNothing mb_deflt = highest_branch
	| otherwise          = hi_tag

    n_tags = real_hi_tag - real_lo_tag + 1

	-- INVARIANT: Provided hi_tag > lo_tag (which is true)
	--	lo_tag <= mid_tag < hi_tag
	--	lo_branches have tags <  mid_tag
	--	hi_branches have tags >= mid_tag

    (mid_tag,_) = branches !! (n_branches `div` 2)
	-- 2 branches => n_branches `div` 2 = 1
	--	      => branches !! 1 give the *second* tag
	-- There are always at least 2 branches here

    (lo_branches, hi_branches) = span is_lo branches
    is_lo (t,_) = t < mid_tag

--------------
emitCmmLitSwitch :: CmmExpr               -- Tag to switch on
	       -> [(Literal, CmmAGraph)]  -- Tagged branches
	       -> CmmAGraph		  -- Default branch (always)
               -> FCode ()                -- Emit the code
-- Used for general literals, whose size might not be a word, 
-- where there is always a default case, and where we don't know
-- the range of values for certain.  For simplicity we always generate a tree.
--
-- ToDo: for integers we could do better here, perhaps by generalising
-- mk_switch and using that.  --SDM 15/09/2004
emitCmmLitSwitch _scrut []       deflt = emit deflt
emitCmmLitSwitch scrut  branches deflt = do
    scrut' <- assignTemp' scrut
    join_lbl <- newLabelC
    deflt_lbl <- label_code join_lbl deflt
    branches_lbls <- label_branches join_lbl branches
    emit =<< mk_lit_switch scrut' deflt_lbl
               (sortBy (comparing fst) branches_lbls)
    emitLabel join_lbl

mk_lit_switch :: CmmExpr -> BlockId 
 	      -> [(Literal,BlockId)]
              -> FCode CmmAGraph
mk_lit_switch scrut deflt [(lit,blk)] 
  = return (mkCbranch (CmmMachOp ne [scrut, CmmLit cmm_lit]) deflt blk)
  where
    cmm_lit = mkSimpleLit lit
    cmm_ty  = cmmLitType cmm_lit
    rep     = typeWidth cmm_ty
    ne      = if isFloatType cmm_ty then MO_F_Ne rep else MO_Ne rep

mk_lit_switch scrut deflt_blk_id branches
  = do lo_blk <- mk_lit_switch scrut deflt_blk_id lo_branches
       hi_blk <- mk_lit_switch scrut deflt_blk_id hi_branches
       mkCmmIfThenElse cond lo_blk hi_blk
  where
    n_branches = length branches
    (mid_lit,_) = branches !! (n_branches `div` 2)
	-- See notes above re mid_tag

    (lo_branches, hi_branches) = span is_lo branches
    is_lo (t,_) = t < mid_lit

    cond = CmmMachOp (mkLtOp mid_lit) 
			[scrut, CmmLit (mkSimpleLit mid_lit)]


--------------
label_default :: BlockId -> Maybe CmmAGraph -> FCode (Maybe BlockId)
label_default _ Nothing
  = return  Nothing
label_default join_lbl (Just code)
  = do lbl <- label_code join_lbl code
       return (Just lbl)

--------------
label_branches :: BlockId -> [(a,CmmAGraph)] -> FCode [(a,BlockId)]
label_branches _join_lbl []
  = return []
label_branches join_lbl ((tag,code):branches)
  = do lbl <- label_code join_lbl code
       branches' <- label_branches join_lbl branches
       return ((tag,lbl):branches')

--------------
label_code :: BlockId -> CmmAGraph -> FCode BlockId
--  label_code J code
--	generates
--  [L: code; goto J]
-- and returns L
label_code join_lbl code = do
    lbl <- newLabelC
    emitOutOfLine lbl (code <*> mkBranch join_lbl)
    return lbl

--------------
assignTemp' :: CmmExpr -> FCode CmmExpr
assignTemp' e
  | isTrivialCmmExpr e = return e
  | otherwise = do
       lreg <- newTemp (cmmExprType e)
       let reg = CmmLocal lreg
       emitAssign reg e
       return (CmmReg reg)

srt_escape :: StgHalfWord
srt_escape = -1
