%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[AbsCUtils]{Help functions for Abstract~C datatype}

\begin{code}
module AbsCUtils (
	nonemptyAbsC,
	mkAbstractCs, mkAbsCStmts,
	mkAlgAltsCSwitch,
	magicIdPrimRep,
	getAmodeRep,
	mixedTypeLocn, mixedPtrLocn,
	flattenAbsC,
	mkAbsCStmtList
	-- printing/forcing stuff comes from PprAbsC
    ) where

#include "HsVersions.h"
#include "../includes/config.h"

import AbsCSyn
import CLabel		( mkMAP_FROZEN_infoLabel )
import Digraph		( stronglyConnComp, SCC(..) )
import DataCon		( fIRST_TAG, dataConTag )
import Literal		( literalPrimRep, mkMachWord, mkMachInt )
import PrimRep		( getPrimRepSize, PrimRep(..) )
import PrimOp		( PrimOp(..) )
import MachOp		( MachOp(..), isDefinitelyInlineMachOp )
import Unique		( Unique{-instance Eq-} )
import UniqSupply	( uniqFromSupply, uniqsFromSupply, splitUniqSupply, 
			  UniqSupply )
import CmdLineOpts      ( opt_EmitCExternDecls, opt_Unregisterised )
import ForeignCall	( ForeignCall(..), CCallSpec(..), isDynamicTarget )
import StgSyn		( StgOp(..) )
import CoreSyn		( AltCon(..) )
import SMRep		( arrPtrsHdrSize, arrWordsHdrSize, fixedHdrSize )
import Outputable
import Panic		( panic )
import FastTypes
import Constants	( wORD_SIZE, wORD_SIZE_IN_BITS )

infixr 9 `thenFlt`
\end{code}

Check if there is any real code in some Abstract~C.  If so, return it
(@Just ...@); otherwise, return @Nothing@.  Don't be too strict!

It returns the "reduced" code in the Just part so that the work of
discarding AbsCNops isn't lost, and so that if the caller uses
the reduced version there's less danger of a big tree of AbsCNops getting
materialised and causing a space leak.

\begin{code}
nonemptyAbsC :: AbstractC -> Maybe AbstractC
nonemptyAbsC  AbsCNop		= Nothing
nonemptyAbsC (AbsCStmts s1 s2)	= case (nonemptyAbsC s1) of
				    Nothing -> nonemptyAbsC s2
				    Just x  -> Just (AbsCStmts x s2)
nonemptyAbsC s@(CSimultaneous c) = case (nonemptyAbsC c) of
				    Nothing -> Nothing
				    Just x  -> Just s
nonemptyAbsC other		= Just other
\end{code}

\begin{code}
mkAbstractCs :: [AbstractC] -> AbstractC
mkAbstractCs [] = AbsCNop
mkAbstractCs cs = foldr1 mkAbsCStmts cs

-- for fiddling around w/ killing off AbsCNops ... (ToDo)
mkAbsCStmts :: AbstractC -> AbstractC -> AbstractC
mkAbsCStmts AbsCNop c = c
mkAbsCStmts c AbsCNop = c
mkAbsCStmts c1 c2     = c1 `AbsCStmts` c2

{- Discarded SLPJ June 95; it calls nonemptyAbsC too much!
  = case (case (nonemptyAbsC abc2) of
	    Nothing -> AbsCNop
	    Just d2 -> d2)	of { abc2b ->

    case (nonemptyAbsC abc1) of {
      Nothing -> abc2b;
      Just d1 -> AbsCStmts d1 abc2b
    } }
-}
\end{code}

Get the sho' 'nuff statements out of an @AbstractC@.
\begin{code}
mkAbsCStmtList :: AbstractC -> [AbstractC]

mkAbsCStmtList absC = mkAbsCStmtList' absC []

-- Optimised a la foldr/build!

mkAbsCStmtList'  AbsCNop r = r

mkAbsCStmtList' (AbsCStmts s1 s2) r
  = mkAbsCStmtList' s1 (mkAbsCStmtList' s2 r)

mkAbsCStmtList' s@(CSimultaneous c) r
  = if null (mkAbsCStmtList c) then r else s : r

mkAbsCStmtList' other r = other : r
\end{code}

\begin{code}
mkAlgAltsCSwitch :: CAddrMode -> [(AltCon, AbstractC)] -> AbstractC

mkAlgAltsCSwitch scrutinee ((_,first_alt) : rest_alts)
 = CSwitch scrutinee (adjust rest_alts) first_alt
 where
   -- We use the first alt as the default.  Either it *is* the DEFAULT,
   -- (which is always first if present), or the case is exhaustive,
   -- in which case we can use the first as the default anyway

   -- Adjust the tags in the switch to start at zero.
   -- This is the convention used by primitive ops which return algebraic
   -- data types.  Why?	 Because for two-constructor types, zero is faster
   -- to create and distinguish from 1 than are 1 and 2.

   -- We also need to convert to Literals to keep the CSwitch happy
   adjust tagged_alts
     = [ (mkMachWord (toInteger (dataConTag dc - fIRST_TAG)), abs_c)
       | (DataAlt dc, abs_c) <- tagged_alts ]
\end{code}

%************************************************************************
%*									*
\subsubsection[AbsCUtils-kinds-from-MagicIds]{Kinds from MagicIds}
%*									*
%************************************************************************

\begin{code}
magicIdPrimRep BaseReg		    = PtrRep
magicIdPrimRep (VanillaReg kind _) = kind
magicIdPrimRep (FloatReg _)	    = FloatRep
magicIdPrimRep (DoubleReg _)	    = DoubleRep
magicIdPrimRep (LongReg kind _)	    = kind
magicIdPrimRep Sp		    = PtrRep
magicIdPrimRep SpLim		    = PtrRep
magicIdPrimRep Hp		    = PtrRep
magicIdPrimRep HpLim		    = PtrRep
magicIdPrimRep CurCostCentre	    = CostCentreRep
magicIdPrimRep VoidReg		    = VoidRep
magicIdPrimRep CurrentTSO	    = PtrRep
magicIdPrimRep CurrentNursery	    = PtrRep
magicIdPrimRep HpAlloc              = WordRep
\end{code}

%************************************************************************
%*									*
\subsection[AbsCUtils-amode-kinds]{Finding @PrimitiveKinds@ of amodes}
%*									*
%************************************************************************

See also the return conventions for unboxed things; currently living
in @CgCon@ (next to the constructor return conventions).

ToDo: tiny tweaking may be in order
\begin{code}
getAmodeRep :: CAddrMode -> PrimRep

getAmodeRep (CVal _ kind)	    	    = kind
getAmodeRep (CAddr _)		    	    = PtrRep
getAmodeRep (CReg magic_id)	    	    = magicIdPrimRep magic_id
getAmodeRep (CTemp uniq kind)	    	    = kind
getAmodeRep (CLbl _ kind)	    	    = kind
getAmodeRep (CCharLike _)	    	    = PtrRep
getAmodeRep (CIntLike _)	    	    = PtrRep
getAmodeRep (CLit lit)		    	    = literalPrimRep lit
getAmodeRep (CMacroExpr kind _ _)    	    = kind
getAmodeRep (CJoinPoint _)	    	    = panic "getAmodeRep:CJoinPoint"
\end{code}

@mixedTypeLocn@ tells whether an amode identifies an ``StgWord''
location; that is, one which can contain values of various types.

\begin{code}
mixedTypeLocn :: CAddrMode -> Bool

mixedTypeLocn (CVal (NodeRel _) _)	= True
mixedTypeLocn (CVal (SpRel _)   _)	= True
mixedTypeLocn (CVal (HpRel _)	_)	= True
mixedTypeLocn other			= False	-- All the rest
\end{code}

@mixedPtrLocn@ tells whether an amode identifies a
location which can contain values of various pointer types.

\begin{code}
mixedPtrLocn :: CAddrMode -> Bool

mixedPtrLocn (CVal (SpRel _)  _)	= True
mixedPtrLocn other			= False	-- All the rest
\end{code}

%************************************************************************
%*									*
\subsection[AbsCUtils-flattening]{Flatten Abstract~C}
%*									*
%************************************************************************

The following bits take ``raw'' Abstract~C, which may have all sorts of
nesting, and flattens it into one long @AbsCStmtList@.  Mainly,
@CClosureInfos@ and code for switches are pulled out to the top level.

The various functions herein tend to produce
\begin{enumerate}
\item
A {\em flattened} \tr{<something>} of interest for ``here'', and
\item
Some {\em unflattened} Abstract~C statements to be carried up to the
top-level.  The only real reason (now) that it is unflattened is
because it means the recursive flattening can be done in just one
place rather than having to remember lots of places.
\end{enumerate}

Care is taken to reduce the occurrence of forward references, while still
keeping laziness a much as possible.  Essentially, this means that:
\begin{itemize}
\item
{\em All} the top-level C statements resulting from flattening a
particular AbsC statement (whether the latter is nested or not) appear
before {\em any} of the code for a subsequent AbsC statement;
\item
but stuff nested within any AbsC statement comes
out before the code for the statement itself.
\end{itemize}

The ``stuff to be carried up'' always includes a label: a
@CStaticClosure@, @CRetDirect@, @CFlatRetVector@, or
@CCodeBlock@.  The latter turns into a C function, and is never
actually produced by the code generator.  Rather it always starts life
as a @CCodeBlock@ addressing mode; when such an addr mode is
flattened, the ``tops'' stuff is a @CCodeBlock@.

\begin{code}
flattenAbsC :: UniqSupply -> AbstractC -> AbstractC

flattenAbsC us abs_C
  = case (initFlt us (flatAbsC abs_C)) of { (here, tops) ->
    here `mkAbsCStmts` tops }
\end{code}

%************************************************************************
%*									*
\subsubsection{Flattening monadery}
%*									*
%************************************************************************

The flattener is monadised.  It's just a @UniqueSupply@.

\begin{code}
type FlatM result =  UniqSupply -> result

initFlt :: UniqSupply -> FlatM a -> a

initFlt init_us m = m init_us

{-# INLINE thenFlt #-}
{-# INLINE returnFlt #-}

thenFlt :: FlatM a -> (a -> FlatM b) -> FlatM b

thenFlt expr cont us
  = case (splitUniqSupply us)   of { (s1, s2) ->
    case (expr s1)	    	of { result ->
    cont result s2 }}

returnFlt :: a -> FlatM a
returnFlt result us = result

mapFlt :: (a -> FlatM b) -> [a] -> FlatM [b]

mapFlt f []     = returnFlt []
mapFlt f (x:xs)
  = f x         `thenFlt` \ r  ->
    mapFlt f xs `thenFlt` \ rs ->
    returnFlt (r:rs)

mapAndUnzipFlt  :: (a -> FlatM (b,c))   -> [a] -> FlatM ([b],[c])

mapAndUnzipFlt f [] = returnFlt ([],[])
mapAndUnzipFlt f (x:xs)
  = f x		    	`thenFlt` \ (r1,  r2)  ->
    mapAndUnzipFlt f xs	`thenFlt` \ (rs1, rs2) ->
    returnFlt (r1:rs1, r2:rs2)

getUniqFlt :: FlatM Unique
getUniqFlt us = uniqFromSupply us

getUniqsFlt :: FlatM [Unique]
getUniqsFlt us = uniqsFromSupply us
\end{code}

%************************************************************************
%*									*
\subsubsection{Flattening the top level}
%*									*
%************************************************************************

\begin{code}
flatAbsC :: AbstractC
	 -> FlatM (AbstractC,	-- Stuff to put inline		[Both are fully
		   AbstractC)	-- Stuff to put at top level	 flattened]

flatAbsC AbsCNop = returnFlt (AbsCNop, AbsCNop)

flatAbsC (AbsCStmts s1 s2)
  = flatAbsC s1	`thenFlt` \ (inline_s1, top_s1) ->
    flatAbsC s2	`thenFlt` \ (inline_s2, top_s2) ->
    returnFlt (mkAbsCStmts inline_s1 inline_s2,
	       mkAbsCStmts top_s1    top_s2)

flatAbsC (CClosureInfoAndCode cl_info entry)
  = flatAbsC entry		`thenFlt` \ (entry_heres, entry_tops) ->
    returnFlt (AbsCNop, mkAbstractCs [entry_tops, 
       CClosureInfoAndCode cl_info entry_heres]
    )

flatAbsC (CCodeBlock lbl abs_C)
  = flatAbsC abs_C	    `thenFlt` \ (absC_heres, absC_tops) ->
    returnFlt (AbsCNop, absC_tops `mkAbsCStmts` CCodeBlock lbl absC_heres)

flatAbsC (CRetDirect uniq slow_code srt liveness)
  = flatAbsC slow_code		`thenFlt` \ (heres, tops) ->
    returnFlt (AbsCNop, 
		mkAbstractCs [ tops, CRetDirect uniq heres srt liveness ])

flatAbsC (CSwitch discrim alts deflt)
  = mapAndUnzipFlt flat_alt alts `thenFlt` \ (flat_alts, flat_alts_tops) ->
    flatAbsC deflt		 `thenFlt` \ (flat_def_alt, def_tops) ->
    returnFlt (
      CSwitch discrim flat_alts flat_def_alt,
      mkAbstractCs (def_tops : flat_alts_tops)
    )
  where
    flat_alt (tag, absC)
      = flatAbsC absC	`thenFlt` \ (alt_heres, alt_tops) ->
	returnFlt ( (tag, alt_heres), alt_tops )

flatAbsC stmt@(COpStmt results (StgFCallOp (CCall ccall@(CCallSpec target _ _)) uniq) args _)
  |  is_dynamic	 	       -- Emit a typedef if its a dynamic call
     || (opt_EmitCExternDecls) -- or we want extern decls
  = returnFlt (stmt, CCallTypedef is_dynamic ccall uniq results args)
  where
    is_dynamic = isDynamicTarget target

flatAbsC stmt@(CSimultaneous abs_c)
  = flatAbsC abs_c		`thenFlt` \ (stmts_here, tops) ->
    doSimultaneously stmts_here	`thenFlt` \ new_stmts_here ->
    returnFlt (new_stmts_here, tops)

flatAbsC stmt@(CCheck macro amodes code)
  = flatAbsC code		`thenFlt` \ (code_here, code_tops) ->
    returnFlt (CCheck macro amodes code_here, code_tops)

-- the TICKY_CTR macro always needs to be hoisted out to the top level. 
-- This is a HACK.
flatAbsC stmt@(CCallProfCtrMacro str amodes)
  | str == FSLIT("TICK_CTR") 	= returnFlt (AbsCNop, stmt)
  | otherwise		 	= returnFlt (stmt, AbsCNop)

-- Some statements need no flattening at all:
flatAbsC stmt@(CMacroStmt macro amodes) 	 = returnFlt (stmt, AbsCNop)
flatAbsC stmt@(CCallProfCCMacro str amodes) 	 = returnFlt (stmt, AbsCNop)
flatAbsC stmt@(CAssign dest source) 		 = returnFlt (stmt, AbsCNop)
flatAbsC stmt@(CJump target) 			 = returnFlt (stmt, AbsCNop)
flatAbsC stmt@(CFallThrough target) 		 = returnFlt (stmt, AbsCNop)
flatAbsC stmt@(CReturn target return_info) 	 = returnFlt (stmt, AbsCNop)
flatAbsC stmt@(CInitHdr a b cc sz) 		 = returnFlt (stmt, AbsCNop)
flatAbsC stmt@(CMachOpStmt res mop args m_vols)  = returnFlt (stmt, AbsCNop)
flatAbsC stmt@(COpStmt results (StgFCallOp _ _) args vol_regs) 
                                                 = returnFlt (stmt, AbsCNop)
flatAbsC stmt@(COpStmt results (StgPrimOp op) args vol_regs) 
   = dscCOpStmt (filter non_void_amode results) op 
                (filter non_void_amode args) vol_regs	
				`thenFlt` \ simpl ->
     case simpl of
        COpStmt _ _ _ _ -> panic "flatAbsC - dscCOpStmt"   -- make sure we don't loop!
        other           -> flatAbsC other
     {-
        A gruesome hack for printing the names of inline primops when they
        are used. 
                                  oink other
     where
        oink xxx 
            = getUniqFlt `thenFlt` \ uu ->
              flatAbsC (CSequential [moo uu (showSDoc (ppr op)), xxx])

        moo uu op_str
           = COpStmt 
                []
                (StgFCallOp
                    (CCall (CCallSpec (CasmTarget (mkFastString (mktxt op_str))) 
                                      defaultCCallConv (PlaySafe False)))
                    uu
                )
                [CReg VoidReg]
                []
        mktxt op_str
            = " asm(\"pushal;\"); printf(\"%%s\\n\",\"" ++ op_str ++ "\"); asm(\"popal\"); "
     -}

flatAbsC (CSequential abcs)
  = mapAndUnzipFlt flatAbsC abcs `thenFlt` \ (inlines, tops) ->
    returnFlt (CSequential inlines, foldr AbsCStmts AbsCNop tops)


-- Some statements only make sense at the top level, so we always float
-- them.  This probably isn't necessary.
flatAbsC stmt@(CStaticClosure _ _ _ _) 		= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CClosureTbl _)			= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CSRT _ _)	  		= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CSRTDesc _ _ _ _ _)  		= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CBitmap _)	  		= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CCostCentreDecl _ _) 		= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CCostCentreStackDecl _)		= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CSplitMarker) 			= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CRetVector _ _ _ _)              = returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CModuleInitBlock _ _ _)          = returnFlt (AbsCNop, stmt)
\end{code}

\begin{code}
flat_maybe :: Maybe AbstractC -> FlatM (Maybe AbstractC, AbstractC)
flat_maybe Nothing      = returnFlt (Nothing, AbsCNop)
flat_maybe (Just abs_c) = flatAbsC abs_c `thenFlt` \ (heres, tops) ->
		          returnFlt (Just heres, tops)
\end{code}

%************************************************************************
%*									*
\subsection[flat-simultaneous]{Doing things simultaneously}
%*									*
%************************************************************************

\begin{code}
doSimultaneously :: AbstractC -> FlatM AbstractC
\end{code}

Generate code to perform the @CAssign@s and @COpStmt@s in the
input simultaneously, using temporary variables when necessary.

We use the strongly-connected component algorithm, in which
	* the vertices are the statements
	* an edge goes from s1 to s2 iff
		s1 assigns to something s2 uses
	  that is, if s1 should *follow* s2 in the final order

\begin{code}
type CVertex = (Int, AbstractC)  -- Give each vertex a unique number,
				 -- for fast comparison

doSimultaneously abs_c
  = let
	enlisted = en_list abs_c
    in
    case enlisted of -- it's often just one stmt
      []  -> returnFlt AbsCNop
      [x] -> returnFlt x
      _   -> doSimultaneously1 (zip [(1::Int)..] enlisted)

-- en_list puts all the assignments in a list, filtering out Nops and
-- assignments which do nothing
en_list AbsCNop				      = []
en_list (AbsCStmts a1 a2)		      = en_list a1 ++ en_list a2
en_list (CAssign am1 am2) | sameAmode am1 am2 = []
en_list other				      = [other]

sameAmode :: CAddrMode -> CAddrMode -> Bool
-- ToDo: Move this function, or make CAddrMode an instance of Eq
-- At the moment we put in just enough to catch the cases we want:
-- 	the second (destination) argument is always a CVal.
sameAmode (CReg r1)		     (CReg r2)		     = r1 == r2
sameAmode (CVal (SpRel r1) _) (CVal (SpRel r2) _) 	     = r1 ==# r2
sameAmode other1		     other2		     = False

doSimultaneously1 :: [CVertex] -> FlatM AbstractC
doSimultaneously1 vertices
  = let
	edges = [ (vertex, key1, edges_from stmt1)
		| vertex@(key1, stmt1) <- vertices
		]
	edges_from stmt1 = [ key2 | (key2, stmt2) <- vertices, 
				    stmt1 `should_follow` stmt2
			   ]
	components = stronglyConnComp edges

	-- do_components deal with one strongly-connected component
		-- Not cyclic, or singleton?  Just do it
	do_component (AcyclicSCC (n,abs_c))  = returnFlt abs_c
	do_component (CyclicSCC [(n,abs_c)]) = returnFlt abs_c

		-- Cyclic?  Then go via temporaries.  Pick one to
		-- break the loop and try again with the rest.
	do_component (CyclicSCC ((n,first_stmt) : rest))
	  = doSimultaneously1 rest	`thenFlt` \ abs_cs ->
	    go_via_temps first_stmt	`thenFlt` \ (to_temps, from_temps) ->
	    returnFlt (mkAbstractCs [to_temps, abs_cs, from_temps])

	go_via_temps (CAssign dest src)
	  = getUniqFlt 			`thenFlt` \ uniq ->
	    let
		the_temp = CTemp uniq (getAmodeRep dest)
	    in
	    returnFlt (CAssign the_temp src, CAssign dest the_temp)

	go_via_temps (COpStmt dests op srcs vol_regs)
	  = getUniqsFlt			`thenFlt` \ uniqs ->
	    let
		the_temps = zipWith (\ u d -> CTemp u (getAmodeRep d)) uniqs dests
	    in
	    returnFlt (COpStmt the_temps op srcs vol_regs,
		       mkAbstractCs (zipWith CAssign dests the_temps))
    in
    mapFlt do_component components `thenFlt` \ abs_cs ->
    returnFlt (mkAbstractCs abs_cs)

  where
    should_follow :: AbstractC -> AbstractC -> Bool
    (CAssign dest1 _) `should_follow` (CAssign _ src2)
      = dest1 `conflictsWith` src2
    (COpStmt dests1 _ _ _) `should_follow` (CAssign _ src2)
      = or [dest1 `conflictsWith` src2 | dest1 <- dests1]
    (CAssign dest1 _)`should_follow` (COpStmt _ _ srcs2 _)
      = or [dest1 `conflictsWith` src2 | src2 <- srcs2]
    (COpStmt dests1 _ _ _) `should_follow` (COpStmt _ _ srcs2 _)
      = or [dest1 `conflictsWith` src2 | dest1 <- dests1, src2 <- srcs2]
\end{code}

@conflictsWith@ tells whether an assignment to its first argument will
screw up an access to its second.

\begin{code}
conflictsWith :: CAddrMode -> CAddrMode -> Bool
(CReg reg1)	   `conflictsWith` (CReg reg2)		= reg1 == reg2
(CReg reg)	   `conflictsWith` (CVal reg_rel _)	= reg `regConflictsWithRR` reg_rel
(CReg reg)	   `conflictsWith` (CAddr reg_rel)	= reg `regConflictsWithRR` reg_rel
(CTemp u1 _)	   `conflictsWith` (CTemp u2 _)		= u1 == u2
(CVal reg_rel1 k1) `conflictsWith` (CVal reg_rel2 k2)
  = rrConflictsWithRR (getPrimRepSize k1) (getPrimRepSize k2) reg_rel1 reg_rel2

other1		  `conflictsWith` other2		= False
-- CAddr and literals are impossible on the LHS of an assignment

regConflictsWithRR :: MagicId -> RegRelative -> Bool

regConflictsWithRR (VanillaReg k n) (NodeRel _)	| n ==# (_ILIT 1)    = True
regConflictsWithRR Sp	(SpRel _)	= True
regConflictsWithRR Hp	(HpRel _)	= True
regConflictsWithRR _	_		= False

rrConflictsWithRR :: Int -> Int			-- Sizes of two things
		  -> RegRelative -> RegRelative -- The two amodes
		  -> Bool

rrConflictsWithRR s1b s2b rr1 rr2 = rr rr1 rr2
  where
    s1 = iUnbox s1b
    s2 = iUnbox s2b

    rr (SpRel o1)    (SpRel o2)
	| s1 ==# (_ILIT 0) || s2 ==# (_ILIT 0) = False -- No conflict if either is size zero
	| s1 ==# (_ILIT 1)  && s2 ==# (_ILIT 1) = o1 ==# o2
	| otherwise	     = (o1 +# s1) >=# o2  &&
			       (o2 +# s2) >=# o1

    rr (NodeRel o1)	 (NodeRel o2)
	| s1 ==# (_ILIT 0) || s2 ==# (_ILIT 0) = False -- No conflict if either is size zero
	| s1 ==# (_ILIT 1) && s2 ==# (_ILIT 1) = o1 ==# o2
	| otherwise	     = True		-- Give up

    rr (HpRel _)	 (HpRel _)    = True	-- Give up (ToDo)

    rr other1	         other2	      = False
\end{code}

%************************************************************************
%*									*
\subsection[flat-primops]{Translating COpStmts to CMachOpStmts}
%*									*
%************************************************************************

\begin{code}

-- We begin with some helper functions.  The main Dude here is
-- dscCOpStmt, defined a little further down.

------------------------------------------------------------------------------

-- Assumes no volatiles
-- Creates
--     res = arg >> (bits-per-word / 2)   when little-endian
-- or
--     res = arg & ((1 << (bits-per-word / 2)) - 1) when big-endian
--
-- In other words, if arg had been stored in memory, makes res the 
-- halfword of arg which would have had the higher address.  This is
-- why it needs to take into account endianness.
--
mkHalfWord_HIADDR res arg
   = mkTemp WordRep			`thenFlt` \ t_hw_mask1 ->
     mkTemp WordRep			`thenFlt` \ t_hw_mask2 ->
     let 
	 hw_shift = mkIntCLit (wORD_SIZE_IN_BITS `quot` 2)

         a_hw_mask1
            = CMachOpStmt t_hw_mask1
                          MO_Nat_Shl [CLit (mkMachWord 1), hw_shift] Nothing
         a_hw_mask2
            = CMachOpStmt t_hw_mask2
                          MO_Nat_Sub [t_hw_mask1, CLit (mkMachWord 1)] Nothing
         final
#        if WORDS_BIGENDIAN
            = CSequential [ a_hw_mask1, a_hw_mask2,
                 CMachOpStmt res MO_Nat_And [arg, t_hw_mask2] Nothing
              ]
#        else
            = CMachOpStmt res MO_Nat_Shr [arg, hw_shift] Nothing
#        endif
     in
         returnFlt final


mkTemp :: PrimRep -> FlatM CAddrMode
mkTemp rep 
   = getUniqFlt `thenFlt` \ uniq -> returnFlt (CTemp uniq rep)

mkTemps = mapFlt mkTemp

-- Sigh.  This is done in 3 seperate places.  Should be
-- commoned up (here, in pprAbsC of COpStmt, and presumably
-- somewhere in the NCG).
non_void_amode amode 
   = case getAmodeRep amode of
        VoidRep -> False
        k       -> True

-- Helpers for translating various minor variants of array indexing.

mkDerefOff :: PrimRep -> CAddrMode -> Int -> CAddrMode
mkDerefOff rep base off
   = CVal (CIndex base (CLit (mkMachInt (toInteger off))) rep) rep

mkNoDerefOff :: PrimRep -> CAddrMode -> Int -> CAddrMode
mkNoDerefOff rep base off
   = CAddr (CIndex base (CLit (mkMachInt (toInteger off))) rep)


-- Generates an address as follows
--    base + sizeof(machine_word)*offw + sizeof(rep)*idx
mk_OSBI_addr :: Int -> PrimRep -> CAddrMode -> CAddrMode -> RegRelative
mk_OSBI_addr offw rep base idx
   = CIndex (CAddr (CIndex base idx rep)) 
            (CLit (mkMachWord (fromIntegral offw))) 
            PtrRep

mk_OSBI_ref :: Int -> PrimRep -> CAddrMode -> CAddrMode -> CAddrMode
mk_OSBI_ref offw rep base idx
   = CVal (mk_OSBI_addr offw rep base idx) rep


doIndexOffForeignObjOp maybe_post_read_cast rep res addr idx
   = mkBasicIndexedRead 0 maybe_post_read_cast rep res (mkDerefOff WordRep addr fixedHdrSize) idx

doIndexOffAddrOp maybe_post_read_cast rep res addr idx
   = mkBasicIndexedRead 0 maybe_post_read_cast rep res addr idx

doIndexByteArrayOp maybe_post_read_cast rep res addr idx
   = mkBasicIndexedRead arrWordsHdrSize maybe_post_read_cast rep res addr idx

doReadPtrArrayOp res addr idx
   = mkBasicIndexedRead arrPtrsHdrSize Nothing PtrRep res addr idx


doWriteOffAddrOp maybe_pre_write_cast rep addr idx val
   = mkBasicIndexedWrite 0 maybe_pre_write_cast rep addr idx val

doWriteByteArrayOp maybe_pre_write_cast rep addr idx val
   = mkBasicIndexedWrite arrWordsHdrSize maybe_pre_write_cast rep addr idx val

doWritePtrArrayOp addr idx val
   = mkBasicIndexedWrite arrPtrsHdrSize Nothing PtrRep addr idx val



mkBasicIndexedRead offw Nothing read_rep res base idx
   = returnFlt (
        CAssign res (mk_OSBI_ref offw read_rep base idx)
     )
mkBasicIndexedRead offw (Just cast_to_mop) read_rep res base idx
   = mkTemp read_rep			`thenFlt` \ tmp ->
     (returnFlt . CSequential) [
        CAssign tmp (mk_OSBI_ref offw read_rep base idx),
        CMachOpStmt res cast_to_mop [tmp] Nothing
     ]

mkBasicIndexedWrite offw Nothing write_rep base idx val
   = returnFlt (
        CAssign (mk_OSBI_ref offw write_rep base idx) val
     )
mkBasicIndexedWrite offw (Just cast_to_mop) write_rep base idx val
   = mkTemp write_rep 			`thenFlt` \ tmp ->
     (returnFlt . CSequential) [
        CMachOpStmt tmp cast_to_mop [val] Nothing,
        CAssign (mk_OSBI_ref offw write_rep base idx) tmp
     ]


-- Simple dyadic op but one for which we need to cast first arg to
-- be sure of correctness
translateOp_dyadic_cast1 mop res cast_arg1_to arg1 arg2 vols
   = mkTemp cast_arg1_to		`thenFlt` \ arg1casted ->
     (returnFlt . CSequential) [
        CAssign arg1casted arg1,
        CMachOpStmt res mop [arg1casted,arg2]
           (if isDefinitelyInlineMachOp mop then Nothing else Just vols)
     ]

-- IA64 mangler doesn't place tables next to code
tablesNextToCode :: Bool
#ifdef ia64_TARGET_ARCH
tablesNextToCode = False
#else
tablesNextToCode = not opt_Unregisterised
#endif

------------------------------------------------------------------------------

-- This is the main top-level desugarer PrimOps into MachOps.  First we
-- handle various awkward cases specially.  The remaining easy cases are
-- then handled by translateOp, defined below.


dscCOpStmt :: [CAddrMode]	-- Results
           -> PrimOp
           -> [CAddrMode]	-- Arguments
           -> [MagicId]		-- Potentially volatile/live registers
                         	-- (to save/restore around the op)
           -> FlatM AbstractC


dscCOpStmt [res_r,res_c] IntAddCOp [aa,bb] vols
{- 
   With some bit-twiddling, we can define int{Add,Sub}Czh portably in
   C, and without needing any comparisons.  This may not be the
   fastest way to do it - if you have better code, please send it! --SDM
  
   Return : r = a + b,  c = 0 if no overflow, 1 on overflow.
  
   We currently don't make use of the r value if c is != 0 (i.e. 
   overflow), we just convert to big integers and try again.  This
   could be improved by making r and c the correct values for
   plugging into a new J#.  
   
   { r = ((I_)(a)) + ((I_)(b));					\
     c = ((StgWord)(~(((I_)(a))^((I_)(b))) & (((I_)(a))^r)))	\
         >> (BITS_IN (I_) - 1);					\
   } 
   Wading through the mass of bracketry, it seems to reduce to:
   c = ( (~(a^b)) & (a^r) ) >>unsigned (BITS_IN(I_)-1)

   SSA-form:
   t1 = a^b
   t2 = ~t1
   t3 = a^r
   t4 = t2 & t3
   c  = t4 >>unsigned BITS_IN(I_)-1
-}
   = mkTemps [IntRep,IntRep,IntRep,IntRep]	`thenFlt` \ [t1,t2,t3,t4] ->
     let bpw1 = mkIntCLit (wORD_SIZE_IN_BITS - 1) in
     (returnFlt . CSequential) [
        CMachOpStmt res_r MO_Nat_Add [aa,bb] Nothing,
        CMachOpStmt t1 MO_Nat_Xor [aa,bb] Nothing,
        CMachOpStmt t2 MO_Nat_Not [t1] Nothing,
        CMachOpStmt t3 MO_Nat_Xor [aa,res_r] Nothing,
        CMachOpStmt t4 MO_Nat_And [t2,t3] Nothing,
        CMachOpStmt res_c MO_Nat_Shr [t4, bpw1] Nothing
     ]


dscCOpStmt [res_r,res_c] IntSubCOp [aa,bb] vols
{- Similarly:
   #define subIntCzh(r,c,a,b)					\
   { r = ((I_)(a)) - ((I_)(b));					\
     c = ((StgWord)((((I_)(a))^((I_)(b))) & (((I_)(a))^r)))	\
         >> (BITS_IN (I_) - 1);					\
   }

   c =  ((a^b) & (a^r)) >>unsigned (BITS_IN(I_)-1)

   t1 = a^b
   t2 = a^r
   t3 = t1 & t2
   c  = t3 >>unsigned BITS_IN(I_)-1
-}
   = mkTemps [IntRep,IntRep,IntRep]		`thenFlt` \ [t1,t2,t3] ->
     let bpw1 = mkIntCLit (wORD_SIZE_IN_BITS - 1) in
     (returnFlt . CSequential) [
        CMachOpStmt res_r MO_Nat_Sub [aa,bb] Nothing,
        CMachOpStmt t1 MO_Nat_Xor [aa,bb] Nothing,
        CMachOpStmt t2 MO_Nat_Xor [aa,res_r] Nothing,
        CMachOpStmt t3 MO_Nat_And [t1,t2] Nothing,
        CMachOpStmt res_c MO_Nat_Shr [t3, bpw1] Nothing
     ]


-- #define parzh(r,node) r = 1
dscCOpStmt [res] ParOp [arg] vols
   = returnFlt
        (CAssign res (CLit (mkMachInt 1)))

-- #define readMutVarzh(r,a)	 r=(P_)(((StgMutVar *)(a))->var)
dscCOpStmt [res] ReadMutVarOp [mutv] vols
   = returnFlt
        (CAssign res (mkDerefOff PtrRep mutv fixedHdrSize))

-- #define writeMutVarzh(a,v)       (P_)(((StgMutVar *)(a))->var)=(v)
dscCOpStmt [] WriteMutVarOp [mutv,var] vols
   = returnFlt
        (CAssign (mkDerefOff PtrRep mutv fixedHdrSize) var)


-- #define ForeignObj_CLOSURE_DATA(c)  (((StgForeignObj *)c)->data)
-- #define foreignObjToAddrzh(r,fo)    r=ForeignObj_CLOSURE_DATA(fo)
dscCOpStmt [res] ForeignObjToAddrOp [fo] vols
   = returnFlt
        (CAssign res (mkDerefOff PtrRep fo fixedHdrSize))

-- #define writeForeignObjzh(res,datum) \
--    (ForeignObj_CLOSURE_DATA(res) = (P_)(datum))
dscCOpStmt [] WriteForeignObjOp [fo,addr] vols
   = returnFlt
        (CAssign (mkDerefOff PtrRep fo fixedHdrSize) addr)


-- #define sizzeofByteArrayzh(r,a) \
--     r = (((StgArrWords *)(a))->words * sizeof(W_))
dscCOpStmt [res] SizeofByteArrayOp [arg] vols
   = mkTemp WordRep 			`thenFlt` \ w ->
     (returnFlt . CSequential) [
        CAssign w (mkDerefOff WordRep arg fixedHdrSize),
        CMachOpStmt w MO_NatU_Mul [w, mkIntCLit wORD_SIZE] (Just vols),
        CAssign res w
     ]

-- #define sizzeofMutableByteArrayzh(r,a) \
--      r = (((StgArrWords *)(a))->words * sizeof(W_))
dscCOpStmt [res] SizeofMutableByteArrayOp [arg] vols
   = dscCOpStmt [res] SizeofByteArrayOp [arg] vols


-- #define touchzh(o)                  /* nothing */
dscCOpStmt [] TouchOp [arg] vols
   = returnFlt AbsCNop

-- #define byteArrayContentszh(r,a) r = BYTE_ARR_CTS(a)
dscCOpStmt [res] ByteArrayContents_Char [arg] vols
   = mkTemp PtrRep			`thenFlt` \ ptr ->
     (returnFlt . CSequential) [
         CMachOpStmt ptr MO_NatU_to_NatP [arg] Nothing,
         CAssign ptr (mkNoDerefOff WordRep ptr arrWordsHdrSize),
         CAssign res ptr
     ]

-- #define stableNameToIntzh(r,s)   (r = ((StgStableName *)s)->sn)
dscCOpStmt [res] StableNameToIntOp [arg] vols
   = returnFlt 
        (CAssign res (mkDerefOff WordRep arg fixedHdrSize))

-- #define eqStableNamezh(r,sn1,sn2)					\
--    (r = (((StgStableName *)sn1)->sn == ((StgStableName *)sn2)->sn))
dscCOpStmt [res] EqStableNameOp [arg1,arg2] vols
   = mkTemps [WordRep, WordRep] 	`thenFlt` \ [sn1,sn2] ->
     (returnFlt . CSequential) [
        CAssign sn1 (mkDerefOff WordRep arg1 fixedHdrSize),
        CAssign sn2 (mkDerefOff WordRep arg2 fixedHdrSize),
        CMachOpStmt res MO_Nat_Eq [sn1,sn2] Nothing
     ]

dscCOpStmt [res] ReallyUnsafePtrEqualityOp [arg1,arg2] vols
   = mkTemps [WordRep, WordRep] 	`thenFlt` \ [w1,w2] ->
     (returnFlt . CSequential) [
 	CMachOpStmt w1 MO_NatP_to_NatU [arg1] Nothing,
 	CMachOpStmt w2 MO_NatP_to_NatU [arg2] Nothing,
        CMachOpStmt res MO_Nat_Eq [w1,w2] Nothing{- because it's inline? -}
     ]

-- #define addrToHValuezh(r,a) r=(P_)a
dscCOpStmt [res] AddrToHValueOp [arg] vols
   = returnFlt 
        (CAssign res arg)

-- #define dataToTagzh(r,a)  r=(GET_TAG(((StgClosure *)a)->header.info))
-- 
--   In the unregisterised case, we don't attempt to compute the location
--   of the tag halfword, just a macro. For this build, fixing on layout
--   info has only got drawbacks.
--
--   Should this arrangement deeply offend you for some reason, code which
--   computes the offset can be found below also.
--      -- sof 3/02
-- 
dscCOpStmt [res] DataToTagOp [arg] vols
   | not tablesNextToCode
   = returnFlt (CMacroStmt DATA_TO_TAGZH [res,arg])
   | otherwise
   = mkTemps [PtrRep, WordRep]		`thenFlt` \ [t_infoptr, t_theword] ->
     mkHalfWord_HIADDR res t_theword	`thenFlt` \ select_ops ->
     (returnFlt . CSequential) [
        CAssign t_infoptr (mkDerefOff PtrRep arg 0),
	 {-
	   Get at the tag within the info table; two cases to consider:
	   
	      - reversed info tables next to the entry point code;
	        one word above the end of the info table (which is
		what t_infoptr is really pointing to).
	      - info tables with their entry points stored somewhere else,
	      	which is how the unregisterised (nee TABLES_NEXT_TO_CODE)
		world operates.
		
	        The t_infoptr points to the start of the info table, so add
		the length of the info table & subtract one word.
	 -}
        CAssign t_theword (mkDerefOff WordRep t_infoptr (-1)),
{- UNUSED - see above comment.
				     (if opt_Unregisterised then 
				     	 (fixedItblSize - 1)
				      else (-1))),
-}
        select_ops
     ]


{- Freezing arrays-of-ptrs requires changing an info table, for the
   benefit of the generational collector.  It needs to scavenge mutable
   objects, even if they are in old space.  When they become immutable,
   they can be removed from this scavenge list.	 -}

-- #define unsafeFreezzeArrayzh(r,a)					\
--	{								\
--        SET_INFO((StgClosure *)a,&stg_MUT_ARR_PTRS_FROZEN_info);        \
--	r = a;								\
--	}
dscCOpStmt [res] UnsafeFreezeArrayOp [arg] vols
   = (returnFlt . CSequential) [
        CAssign (mkDerefOff PtrRep arg 0) (CLbl mkMAP_FROZEN_infoLabel PtrRep),
        CAssign res arg
     ]

-- #define unsafeFreezzeByteArrayzh(r,a)	r=(a)
dscCOpStmt [res] UnsafeFreezeByteArrayOp [arg] vols
   = returnFlt
        (CAssign res arg)

-- This ought to be trivial, but it's difficult to insert the casts
-- required to keep the C compiler happy.
dscCOpStmt [r] AddrRemOp [a1,a2] vols 
   = mkTemp WordRep			`thenFlt` \ a1casted ->
     (returnFlt . CSequential) [
        CMachOpStmt a1casted MO_NatP_to_NatU [a1] Nothing,
        CMachOpStmt r MO_NatU_Rem [a1casted,a2] Nothing
     ]

-- not handled by translateOp because they need casts
dscCOpStmt [r] SllOp [a1,a2] vols 
   = translateOp_dyadic_cast1 MO_Nat_Shl r WordRep a1 a2 vols
dscCOpStmt [r] SrlOp [a1,a2] vols 
   = translateOp_dyadic_cast1 MO_Nat_Shr r WordRep a1 a2 vols

dscCOpStmt [r] ISllOp [a1,a2] vols 
   = translateOp_dyadic_cast1 MO_Nat_Shl r IntRep a1 a2 vols
dscCOpStmt [r] ISrlOp [a1,a2] vols 
   = translateOp_dyadic_cast1 MO_Nat_Shr r IntRep a1 a2 vols
dscCOpStmt [r] ISraOp [a1,a2] vols 
   = translateOp_dyadic_cast1 MO_Nat_Sar r IntRep a1 a2 vols

-- Reading/writing pointer arrays

dscCOpStmt [r] ReadArrayOp  [obj,ix]   vols  = doReadPtrArrayOp r obj ix
dscCOpStmt [r] IndexArrayOp [obj,ix]   vols  = doReadPtrArrayOp r obj ix
dscCOpStmt []  WriteArrayOp [obj,ix,v] vols  = doWritePtrArrayOp obj ix v

-- IndexXXXoffForeignObj

dscCOpStmt [r] IndexOffForeignObjOp_Char      [a,i] vols = doIndexOffForeignObjOp (Just MO_8U_to_32U) Word8Rep r a i
dscCOpStmt [r] IndexOffForeignObjOp_WideChar  [a,i] vols = doIndexOffForeignObjOp Nothing Word32Rep r a i
dscCOpStmt [r] IndexOffForeignObjOp_Int       [a,i] vols = doIndexOffForeignObjOp Nothing IntRep r a i
dscCOpStmt [r] IndexOffForeignObjOp_Word      [a,i] vols = doIndexOffForeignObjOp Nothing WordRep r a i
dscCOpStmt [r] IndexOffForeignObjOp_Addr      [a,i] vols = doIndexOffForeignObjOp Nothing AddrRep r a i
dscCOpStmt [r] IndexOffForeignObjOp_Float     [a,i] vols = doIndexOffForeignObjOp Nothing FloatRep r a i
dscCOpStmt [r] IndexOffForeignObjOp_Double    [a,i] vols = doIndexOffForeignObjOp Nothing DoubleRep r a i
dscCOpStmt [r] IndexOffForeignObjOp_StablePtr [a,i] vols = doIndexOffForeignObjOp Nothing StablePtrRep r a i

dscCOpStmt [r] IndexOffForeignObjOp_Int8      [a,i] vols = doIndexOffForeignObjOp Nothing Int8Rep  r a i
dscCOpStmt [r] IndexOffForeignObjOp_Int16     [a,i] vols = doIndexOffForeignObjOp Nothing Int16Rep r a i
dscCOpStmt [r] IndexOffForeignObjOp_Int32     [a,i] vols = doIndexOffForeignObjOp Nothing Int32Rep r a i
dscCOpStmt [r] IndexOffForeignObjOp_Int64     [a,i] vols = doIndexOffForeignObjOp Nothing Int64Rep r a i

dscCOpStmt [r] IndexOffForeignObjOp_Word8     [a,i] vols = doIndexOffForeignObjOp Nothing Word8Rep  r a i
dscCOpStmt [r] IndexOffForeignObjOp_Word16    [a,i] vols = doIndexOffForeignObjOp Nothing Word16Rep r a i
dscCOpStmt [r] IndexOffForeignObjOp_Word32    [a,i] vols = doIndexOffForeignObjOp Nothing Word32Rep r a i
dscCOpStmt [r] IndexOffForeignObjOp_Word64    [a,i] vols = doIndexOffForeignObjOp Nothing Word64Rep r a i

-- IndexXXXoffAddr

dscCOpStmt [r] IndexOffAddrOp_Char      [a,i] vols = doIndexOffAddrOp (Just MO_8U_to_32U) Word8Rep r a i
dscCOpStmt [r] IndexOffAddrOp_WideChar  [a,i] vols = doIndexOffAddrOp Nothing Word32Rep r a i
dscCOpStmt [r] IndexOffAddrOp_Int       [a,i] vols = doIndexOffAddrOp Nothing IntRep r a i
dscCOpStmt [r] IndexOffAddrOp_Word      [a,i] vols = doIndexOffAddrOp Nothing WordRep r a i
dscCOpStmt [r] IndexOffAddrOp_Addr      [a,i] vols = doIndexOffAddrOp Nothing AddrRep r a i
dscCOpStmt [r] IndexOffAddrOp_Float     [a,i] vols = doIndexOffAddrOp Nothing FloatRep r a i
dscCOpStmt [r] IndexOffAddrOp_Double    [a,i] vols = doIndexOffAddrOp Nothing DoubleRep r a i
dscCOpStmt [r] IndexOffAddrOp_StablePtr [a,i] vols = doIndexOffAddrOp Nothing StablePtrRep r a i

dscCOpStmt [r] IndexOffAddrOp_Int8      [a,i] vols = doIndexOffAddrOp Nothing Int8Rep  r a i
dscCOpStmt [r] IndexOffAddrOp_Int16     [a,i] vols = doIndexOffAddrOp Nothing Int16Rep r a i
dscCOpStmt [r] IndexOffAddrOp_Int32     [a,i] vols = doIndexOffAddrOp Nothing Int32Rep r a i
dscCOpStmt [r] IndexOffAddrOp_Int64     [a,i] vols = doIndexOffAddrOp Nothing Int64Rep r a i

dscCOpStmt [r] IndexOffAddrOp_Word8     [a,i] vols = doIndexOffAddrOp Nothing Word8Rep  r a i
dscCOpStmt [r] IndexOffAddrOp_Word16    [a,i] vols = doIndexOffAddrOp Nothing Word16Rep r a i
dscCOpStmt [r] IndexOffAddrOp_Word32    [a,i] vols = doIndexOffAddrOp Nothing Word32Rep r a i
dscCOpStmt [r] IndexOffAddrOp_Word64    [a,i] vols = doIndexOffAddrOp Nothing Word64Rep r a i

-- ReadXXXoffAddr, which are identical, for our purposes, to IndexXXXoffAddr.

dscCOpStmt [r] ReadOffAddrOp_Char      [a,i] vols = doIndexOffAddrOp (Just MO_8U_to_32U) Word8Rep r a i
dscCOpStmt [r] ReadOffAddrOp_WideChar  [a,i] vols = doIndexOffAddrOp Nothing Word32Rep r a i
dscCOpStmt [r] ReadOffAddrOp_Int       [a,i] vols = doIndexOffAddrOp Nothing IntRep r a i
dscCOpStmt [r] ReadOffAddrOp_Word      [a,i] vols = doIndexOffAddrOp Nothing WordRep r a i
dscCOpStmt [r] ReadOffAddrOp_Addr      [a,i] vols = doIndexOffAddrOp Nothing AddrRep r a i
dscCOpStmt [r] ReadOffAddrOp_Float     [a,i] vols = doIndexOffAddrOp Nothing FloatRep r a i
dscCOpStmt [r] ReadOffAddrOp_Double    [a,i] vols = doIndexOffAddrOp Nothing DoubleRep r a i
dscCOpStmt [r] ReadOffAddrOp_StablePtr [a,i] vols = doIndexOffAddrOp Nothing StablePtrRep r a i

dscCOpStmt [r] ReadOffAddrOp_Int8      [a,i] vols = doIndexOffAddrOp Nothing Int8Rep  r a i
dscCOpStmt [r] ReadOffAddrOp_Int16     [a,i] vols = doIndexOffAddrOp Nothing Int16Rep r a i
dscCOpStmt [r] ReadOffAddrOp_Int32     [a,i] vols = doIndexOffAddrOp Nothing Int32Rep r a i
dscCOpStmt [r] ReadOffAddrOp_Int64     [a,i] vols = doIndexOffAddrOp Nothing Int64Rep r a i

dscCOpStmt [r] ReadOffAddrOp_Word8     [a,i] vols = doIndexOffAddrOp Nothing Word8Rep  r a i
dscCOpStmt [r] ReadOffAddrOp_Word16    [a,i] vols = doIndexOffAddrOp Nothing Word16Rep r a i
dscCOpStmt [r] ReadOffAddrOp_Word32    [a,i] vols = doIndexOffAddrOp Nothing Word32Rep r a i
dscCOpStmt [r] ReadOffAddrOp_Word64    [a,i] vols = doIndexOffAddrOp Nothing Word64Rep r a i

-- IndexXXXArray

dscCOpStmt [r] IndexByteArrayOp_Char      [a,i] vols = doIndexByteArrayOp (Just MO_8U_to_32U) Word8Rep r a i
dscCOpStmt [r] IndexByteArrayOp_WideChar  [a,i] vols = doIndexByteArrayOp Nothing Word32Rep r a i
dscCOpStmt [r] IndexByteArrayOp_Int       [a,i] vols = doIndexByteArrayOp Nothing IntRep r a i
dscCOpStmt [r] IndexByteArrayOp_Word      [a,i] vols = doIndexByteArrayOp Nothing WordRep r a i
dscCOpStmt [r] IndexByteArrayOp_Addr      [a,i] vols = doIndexByteArrayOp Nothing AddrRep r a i
dscCOpStmt [r] IndexByteArrayOp_Float     [a,i] vols = doIndexByteArrayOp Nothing FloatRep r a i
dscCOpStmt [r] IndexByteArrayOp_Double    [a,i] vols = doIndexByteArrayOp Nothing DoubleRep r a i
dscCOpStmt [r] IndexByteArrayOp_StablePtr [a,i] vols = doIndexByteArrayOp Nothing StablePtrRep r a i

dscCOpStmt [r] IndexByteArrayOp_Int8      [a,i] vols = doIndexByteArrayOp Nothing Int8Rep  r a i
dscCOpStmt [r] IndexByteArrayOp_Int16     [a,i] vols = doIndexByteArrayOp Nothing Int16Rep  r a i
dscCOpStmt [r] IndexByteArrayOp_Int32     [a,i] vols = doIndexByteArrayOp Nothing Int32Rep  r a i
dscCOpStmt [r] IndexByteArrayOp_Int64     [a,i] vols = doIndexByteArrayOp Nothing Int64Rep  r a i

dscCOpStmt [r] IndexByteArrayOp_Word8     [a,i] vols = doIndexByteArrayOp Nothing Word8Rep  r a i
dscCOpStmt [r] IndexByteArrayOp_Word16    [a,i] vols = doIndexByteArrayOp Nothing Word16Rep  r a i
dscCOpStmt [r] IndexByteArrayOp_Word32    [a,i] vols = doIndexByteArrayOp Nothing Word32Rep  r a i
dscCOpStmt [r] IndexByteArrayOp_Word64    [a,i] vols = doIndexByteArrayOp Nothing Word64Rep  r a i

-- ReadXXXArray, identical to IndexXXXArray.

dscCOpStmt [r] ReadByteArrayOp_Char       [a,i] vols = doIndexByteArrayOp (Just MO_8U_to_32U) Word8Rep r a i
dscCOpStmt [r] ReadByteArrayOp_WideChar   [a,i] vols = doIndexByteArrayOp Nothing Word32Rep r a i
dscCOpStmt [r] ReadByteArrayOp_Int        [a,i] vols = doIndexByteArrayOp Nothing IntRep r a i
dscCOpStmt [r] ReadByteArrayOp_Word       [a,i] vols = doIndexByteArrayOp Nothing WordRep r a i
dscCOpStmt [r] ReadByteArrayOp_Addr       [a,i] vols = doIndexByteArrayOp Nothing AddrRep r a i
dscCOpStmt [r] ReadByteArrayOp_Float      [a,i] vols = doIndexByteArrayOp Nothing FloatRep r a i
dscCOpStmt [r] ReadByteArrayOp_Double     [a,i] vols = doIndexByteArrayOp Nothing DoubleRep r a i
dscCOpStmt [r] ReadByteArrayOp_StablePtr  [a,i] vols = doIndexByteArrayOp Nothing StablePtrRep r a i

dscCOpStmt [r] ReadByteArrayOp_Int8       [a,i] vols = doIndexByteArrayOp Nothing Int8Rep  r a i
dscCOpStmt [r] ReadByteArrayOp_Int16      [a,i] vols = doIndexByteArrayOp Nothing Int16Rep  r a i
dscCOpStmt [r] ReadByteArrayOp_Int32      [a,i] vols = doIndexByteArrayOp Nothing Int32Rep  r a i
dscCOpStmt [r] ReadByteArrayOp_Int64      [a,i] vols = doIndexByteArrayOp Nothing Int64Rep  r a i

dscCOpStmt [r] ReadByteArrayOp_Word8      [a,i] vols = doIndexByteArrayOp Nothing Word8Rep  r a i
dscCOpStmt [r] ReadByteArrayOp_Word16     [a,i] vols = doIndexByteArrayOp Nothing Word16Rep  r a i
dscCOpStmt [r] ReadByteArrayOp_Word32     [a,i] vols = doIndexByteArrayOp Nothing Word32Rep  r a i
dscCOpStmt [r] ReadByteArrayOp_Word64     [a,i] vols = doIndexByteArrayOp Nothing Word64Rep  r a i

-- WriteXXXoffAddr

dscCOpStmt [] WriteOffAddrOp_Char       [a,i,x] vols = doWriteOffAddrOp (Just MO_32U_to_8U) Word8Rep a i x
dscCOpStmt [] WriteOffAddrOp_WideChar   [a,i,x] vols = doWriteOffAddrOp Nothing Word32Rep a i x
dscCOpStmt [] WriteOffAddrOp_Int        [a,i,x] vols = doWriteOffAddrOp Nothing IntRep a i x
dscCOpStmt [] WriteOffAddrOp_Word       [a,i,x] vols = doWriteOffAddrOp Nothing WordRep a i x
dscCOpStmt [] WriteOffAddrOp_Addr       [a,i,x] vols = doWriteOffAddrOp Nothing AddrRep a i x
dscCOpStmt [] WriteOffAddrOp_Float      [a,i,x] vols = doWriteOffAddrOp Nothing FloatRep a i x
dscCOpStmt [] WriteOffAddrOp_ForeignObj [a,i,x] vols = doWriteOffAddrOp Nothing PtrRep a i x
dscCOpStmt [] WriteOffAddrOp_Double     [a,i,x] vols = doWriteOffAddrOp Nothing DoubleRep a i x
dscCOpStmt [] WriteOffAddrOp_StablePtr  [a,i,x] vols = doWriteOffAddrOp Nothing StablePtrRep a i x

dscCOpStmt [] WriteOffAddrOp_Int8       [a,i,x] vols = doWriteOffAddrOp Nothing Int8Rep  a i x
dscCOpStmt [] WriteOffAddrOp_Int16      [a,i,x] vols = doWriteOffAddrOp Nothing Int16Rep a i x
dscCOpStmt [] WriteOffAddrOp_Int32      [a,i,x] vols = doWriteOffAddrOp Nothing Int32Rep a i x
dscCOpStmt [] WriteOffAddrOp_Int64      [a,i,x] vols = doWriteOffAddrOp Nothing Int64Rep a i x

dscCOpStmt [] WriteOffAddrOp_Word8      [a,i,x] vols = doWriteOffAddrOp Nothing Word8Rep  a i x
dscCOpStmt [] WriteOffAddrOp_Word16     [a,i,x] vols = doWriteOffAddrOp Nothing Word16Rep a i x
dscCOpStmt [] WriteOffAddrOp_Word32     [a,i,x] vols = doWriteOffAddrOp Nothing Word32Rep a i x
dscCOpStmt [] WriteOffAddrOp_Word64     [a,i,x] vols = doWriteOffAddrOp Nothing Word64Rep a i x

-- WriteXXXArray

dscCOpStmt [] WriteByteArrayOp_Char      [a,i,x] vols = doWriteByteArrayOp (Just MO_32U_to_8U) Word8Rep a i x
dscCOpStmt [] WriteByteArrayOp_WideChar  [a,i,x] vols = doWriteByteArrayOp Nothing Word32Rep a i x
dscCOpStmt [] WriteByteArrayOp_Int       [a,i,x] vols = doWriteByteArrayOp Nothing IntRep a i x
dscCOpStmt [] WriteByteArrayOp_Word      [a,i,x] vols = doWriteByteArrayOp Nothing WordRep a i x
dscCOpStmt [] WriteByteArrayOp_Addr      [a,i,x] vols = doWriteByteArrayOp Nothing AddrRep a i x
dscCOpStmt [] WriteByteArrayOp_Float     [a,i,x] vols = doWriteByteArrayOp Nothing FloatRep a i x
dscCOpStmt [] WriteByteArrayOp_Double    [a,i,x] vols = doWriteByteArrayOp Nothing DoubleRep a i x
dscCOpStmt [] WriteByteArrayOp_StablePtr [a,i,x] vols = doWriteByteArrayOp Nothing StablePtrRep a i x

dscCOpStmt [] WriteByteArrayOp_Int8      [a,i,x] vols = doWriteByteArrayOp Nothing Int8Rep  a i x
dscCOpStmt [] WriteByteArrayOp_Int16     [a,i,x] vols = doWriteByteArrayOp Nothing Int16Rep  a i x
dscCOpStmt [] WriteByteArrayOp_Int32     [a,i,x] vols = doWriteByteArrayOp Nothing Int32Rep  a i x
dscCOpStmt [] WriteByteArrayOp_Int64     [a,i,x] vols = doWriteByteArrayOp Nothing Int64Rep  a i x

dscCOpStmt [] WriteByteArrayOp_Word8     [a,i,x] vols = doWriteByteArrayOp Nothing Word8Rep  a i x
dscCOpStmt [] WriteByteArrayOp_Word16    [a,i,x] vols = doWriteByteArrayOp Nothing Word16Rep  a i x
dscCOpStmt [] WriteByteArrayOp_Word32    [a,i,x] vols = doWriteByteArrayOp Nothing Word32Rep  a i x
dscCOpStmt [] WriteByteArrayOp_Word64    [a,i,x] vols = doWriteByteArrayOp Nothing Word64Rep  a i x


-- Handle all others as simply as possible.
dscCOpStmt ress op args vols
   = case translateOp ress op args of
        Nothing 
           -> pprPanic "dscCOpStmt: can't translate PrimOp" (ppr op)
        Just (maybe_res, mop, args)
           -> returnFlt (
                 CMachOpStmt maybe_res mop args 
                    (if isDefinitelyInlineMachOp mop then Nothing else Just vols)
              )

-- Native word signless ops

translateOp [r] IntAddOp       [a1,a2] = Just (r, MO_Nat_Add,        [a1,a2])
translateOp [r] IntSubOp       [a1,a2] = Just (r, MO_Nat_Sub,        [a1,a2])
translateOp [r] WordAddOp      [a1,a2] = Just (r, MO_Nat_Add,        [a1,a2])
translateOp [r] WordSubOp      [a1,a2] = Just (r, MO_Nat_Sub,        [a1,a2])
translateOp [r] AddrAddOp      [a1,a2] = Just (r, MO_Nat_Add,        [a1,a2])
translateOp [r] AddrSubOp      [a1,a2] = Just (r, MO_Nat_Sub,        [a1,a2])

translateOp [r] IntEqOp        [a1,a2] = Just (r, MO_Nat_Eq,         [a1,a2])
translateOp [r] IntNeOp        [a1,a2] = Just (r, MO_Nat_Ne,         [a1,a2])
translateOp [r] WordEqOp       [a1,a2] = Just (r, MO_Nat_Eq,         [a1,a2])
translateOp [r] WordNeOp       [a1,a2] = Just (r, MO_Nat_Ne,         [a1,a2])
translateOp [r] AddrEqOp       [a1,a2] = Just (r, MO_Nat_Eq,         [a1,a2])
translateOp [r] AddrNeOp       [a1,a2] = Just (r, MO_Nat_Ne,         [a1,a2])

translateOp [r] AndOp          [a1,a2] = Just (r, MO_Nat_And,        [a1,a2])
translateOp [r] OrOp           [a1,a2] = Just (r, MO_Nat_Or,         [a1,a2])
translateOp [r] XorOp          [a1,a2] = Just (r, MO_Nat_Xor,        [a1,a2])
translateOp [r] NotOp          [a1]    = Just (r, MO_Nat_Not,        [a1])

-- Native word signed ops

translateOp [r] IntMulOp       [a1,a2] = Just (r, MO_NatS_Mul,       [a1,a2])
translateOp [r] IntMulMayOfloOp [a1,a2] = Just (r, MO_NatS_MulMayOflo, [a1,a2])
translateOp [r] IntQuotOp      [a1,a2] = Just (r, MO_NatS_Quot,      [a1,a2])
translateOp [r] IntRemOp       [a1,a2] = Just (r, MO_NatS_Rem,       [a1,a2])
translateOp [r] IntNegOp       [a1]    = Just (r, MO_NatS_Neg,       [a1])

translateOp [r] IntGeOp        [a1,a2] = Just (r, MO_NatS_Ge,        [a1,a2])
translateOp [r] IntLeOp        [a1,a2] = Just (r, MO_NatS_Le,        [a1,a2])
translateOp [r] IntGtOp        [a1,a2] = Just (r, MO_NatS_Gt,        [a1,a2])
translateOp [r] IntLtOp        [a1,a2] = Just (r, MO_NatS_Lt,        [a1,a2])


-- Native word unsigned ops

translateOp [r] WordGeOp       [a1,a2] = Just (r, MO_NatU_Ge,        [a1,a2])
translateOp [r] WordLeOp       [a1,a2] = Just (r, MO_NatU_Le,        [a1,a2])
translateOp [r] WordGtOp       [a1,a2] = Just (r, MO_NatU_Gt,        [a1,a2])
translateOp [r] WordLtOp       [a1,a2] = Just (r, MO_NatU_Lt,        [a1,a2])

translateOp [r] WordMulOp      [a1,a2] = Just (r, MO_NatU_Mul,       [a1,a2])
translateOp [r] WordQuotOp     [a1,a2] = Just (r, MO_NatU_Quot,      [a1,a2])
translateOp [r] WordRemOp      [a1,a2] = Just (r, MO_NatU_Rem,       [a1,a2])

translateOp [r] AddrGeOp       [a1,a2] = Just (r, MO_NatU_Ge,        [a1,a2])
translateOp [r] AddrLeOp       [a1,a2] = Just (r, MO_NatU_Le,        [a1,a2])
translateOp [r] AddrGtOp       [a1,a2] = Just (r, MO_NatU_Gt,        [a1,a2])
translateOp [r] AddrLtOp       [a1,a2] = Just (r, MO_NatU_Lt,        [a1,a2])

-- 32-bit unsigned ops

translateOp [r] CharEqOp       [a1,a2] = Just (r, MO_32U_Eq,        [a1,a2])
translateOp [r] CharNeOp       [a1,a2] = Just (r, MO_32U_Ne,        [a1,a2])
translateOp [r] CharGeOp       [a1,a2] = Just (r, MO_32U_Ge,        [a1,a2])
translateOp [r] CharLeOp       [a1,a2] = Just (r, MO_32U_Le,        [a1,a2])
translateOp [r] CharGtOp       [a1,a2] = Just (r, MO_32U_Gt,        [a1,a2])
translateOp [r] CharLtOp       [a1,a2] = Just (r, MO_32U_Lt,        [a1,a2])

-- Double ops

translateOp [r] DoubleEqOp     [a1,a2] = Just (r, MO_Dbl_Eq,      [a1,a2])
translateOp [r] DoubleNeOp     [a1,a2] = Just (r, MO_Dbl_Ne,      [a1,a2])
translateOp [r] DoubleGeOp     [a1,a2] = Just (r, MO_Dbl_Ge,      [a1,a2])
translateOp [r] DoubleLeOp     [a1,a2] = Just (r, MO_Dbl_Le,      [a1,a2])
translateOp [r] DoubleGtOp     [a1,a2] = Just (r, MO_Dbl_Gt,      [a1,a2])
translateOp [r] DoubleLtOp     [a1,a2] = Just (r, MO_Dbl_Lt,      [a1,a2])

translateOp [r] DoubleAddOp    [a1,a2] = Just (r, MO_Dbl_Add,    [a1,a2])
translateOp [r] DoubleSubOp    [a1,a2] = Just (r, MO_Dbl_Sub,    [a1,a2])
translateOp [r] DoubleMulOp    [a1,a2] = Just (r, MO_Dbl_Mul,    [a1,a2])
translateOp [r] DoubleDivOp    [a1,a2] = Just (r, MO_Dbl_Div,    [a1,a2])
translateOp [r] DoublePowerOp  [a1,a2] = Just (r, MO_Dbl_Pwr,    [a1,a2])

translateOp [r] DoubleSinOp    [a1]    = Just (r, MO_Dbl_Sin,    [a1])
translateOp [r] DoubleCosOp    [a1]    = Just (r, MO_Dbl_Cos,    [a1])
translateOp [r] DoubleTanOp    [a1]    = Just (r, MO_Dbl_Tan,    [a1])
translateOp [r] DoubleSinhOp   [a1]    = Just (r, MO_Dbl_Sinh,   [a1])
translateOp [r] DoubleCoshOp   [a1]    = Just (r, MO_Dbl_Cosh,   [a1])
translateOp [r] DoubleTanhOp   [a1]    = Just (r, MO_Dbl_Tanh,   [a1])
translateOp [r] DoubleAsinOp   [a1]    = Just (r, MO_Dbl_Asin,    [a1])
translateOp [r] DoubleAcosOp   [a1]    = Just (r, MO_Dbl_Acos,    [a1])
translateOp [r] DoubleAtanOp   [a1]    = Just (r, MO_Dbl_Atan,    [a1])
translateOp [r] DoubleLogOp    [a1]    = Just (r, MO_Dbl_Log,    [a1])
translateOp [r] DoubleExpOp    [a1]    = Just (r, MO_Dbl_Exp,    [a1])
translateOp [r] DoubleSqrtOp   [a1]    = Just (r, MO_Dbl_Sqrt,    [a1])
translateOp [r] DoubleNegOp    [a1]    = Just (r, MO_Dbl_Neg,    [a1])

-- Float ops

translateOp [r] FloatEqOp     [a1,a2] = Just (r, MO_Flt_Eq,      [a1,a2])
translateOp [r] FloatNeOp     [a1,a2] = Just (r, MO_Flt_Ne,      [a1,a2])
translateOp [r] FloatGeOp     [a1,a2] = Just (r, MO_Flt_Ge,      [a1,a2])
translateOp [r] FloatLeOp     [a1,a2] = Just (r, MO_Flt_Le,      [a1,a2])
translateOp [r] FloatGtOp     [a1,a2] = Just (r, MO_Flt_Gt,      [a1,a2])
translateOp [r] FloatLtOp     [a1,a2] = Just (r, MO_Flt_Lt,      [a1,a2])

translateOp [r] FloatAddOp    [a1,a2] = Just (r, MO_Flt_Add,    [a1,a2])
translateOp [r] FloatSubOp    [a1,a2] = Just (r, MO_Flt_Sub,    [a1,a2])
translateOp [r] FloatMulOp    [a1,a2] = Just (r, MO_Flt_Mul,    [a1,a2])
translateOp [r] FloatDivOp    [a1,a2] = Just (r, MO_Flt_Div,    [a1,a2])
translateOp [r] FloatPowerOp  [a1,a2] = Just (r, MO_Flt_Pwr,    [a1,a2])

translateOp [r] FloatSinOp    [a1]    = Just (r, MO_Flt_Sin,    [a1])
translateOp [r] FloatCosOp    [a1]    = Just (r, MO_Flt_Cos,    [a1])
translateOp [r] FloatTanOp    [a1]    = Just (r, MO_Flt_Tan,    [a1])
translateOp [r] FloatSinhOp   [a1]    = Just (r, MO_Flt_Sinh,   [a1])
translateOp [r] FloatCoshOp   [a1]    = Just (r, MO_Flt_Cosh,   [a1])
translateOp [r] FloatTanhOp   [a1]    = Just (r, MO_Flt_Tanh,   [a1])
translateOp [r] FloatAsinOp   [a1]    = Just (r, MO_Flt_Asin,    [a1])
translateOp [r] FloatAcosOp   [a1]    = Just (r, MO_Flt_Acos,    [a1])
translateOp [r] FloatAtanOp   [a1]    = Just (r, MO_Flt_Atan,    [a1])
translateOp [r] FloatLogOp    [a1]    = Just (r, MO_Flt_Log,    [a1])
translateOp [r] FloatExpOp    [a1]    = Just (r, MO_Flt_Exp,    [a1])
translateOp [r] FloatSqrtOp   [a1]    = Just (r, MO_Flt_Sqrt,    [a1])
translateOp [r] FloatNegOp    [a1]    = Just (r, MO_Flt_Neg,    [a1])

-- Conversions

translateOp [r] Int2DoubleOp   [a1]   = Just (r, MO_NatS_to_Dbl,   [a1])
translateOp [r] Double2IntOp   [a1]   = Just (r, MO_Dbl_to_NatS,   [a1])

translateOp [r] Int2FloatOp    [a1]   = Just (r, MO_NatS_to_Flt,   [a1])
translateOp [r] Float2IntOp    [a1]   = Just (r, MO_Flt_to_NatS,   [a1])

translateOp [r] Float2DoubleOp [a1]   = Just (r, MO_Flt_to_Dbl,    [a1])
translateOp [r] Double2FloatOp [a1]   = Just (r, MO_Dbl_to_Flt,    [a1])

translateOp [r] Int2WordOp     [a1]   = Just (r, MO_NatS_to_NatU,  [a1])
translateOp [r] Word2IntOp     [a1]   = Just (r, MO_NatU_to_NatS,  [a1])

translateOp [r] Int2AddrOp     [a1]   = Just (r, MO_NatS_to_NatP,  [a1])
translateOp [r] Addr2IntOp     [a1]   = Just (r, MO_NatP_to_NatS,  [a1])

translateOp [r] OrdOp          [a1]   = Just (r, MO_32U_to_NatS,   [a1])
translateOp [r] ChrOp          [a1]   = Just (r, MO_NatS_to_32U,   [a1])

translateOp [r] Narrow8IntOp   [a1]   = Just (r, MO_8S_to_NatS,    [a1])
translateOp [r] Narrow16IntOp  [a1]   = Just (r, MO_16S_to_NatS,   [a1])
translateOp [r] Narrow32IntOp  [a1]   = Just (r, MO_32S_to_NatS,   [a1])

translateOp [r] Narrow8WordOp   [a1]  = Just (r, MO_8U_to_NatU,    [a1])
translateOp [r] Narrow16WordOp  [a1]  = Just (r, MO_16U_to_NatU,   [a1])
translateOp [r] Narrow32WordOp  [a1]  = Just (r, MO_32U_to_NatU,   [a1])

-- Word comparisons masquerading as more exotic things.

translateOp [r] SameMutVarOp   [a1,a2]  = Just (r, MO_Nat_Eq,    [a1,a2])
translateOp [r] SameMVarOp     [a1,a2]  = Just (r, MO_Nat_Eq,    [a1,a2])
translateOp [r] SameMutableArrayOp  [a1,a2]  = Just (r, MO_Nat_Eq,    [a1,a2])
translateOp [r] SameMutableByteArrayOp [a1,a2]  = Just (r, MO_Nat_Eq,    [a1,a2])
translateOp [r] EqForeignObj [a1,a2]  = Just (r, MO_Nat_Eq,    [a1,a2])
translateOp [r] EqStablePtrOp [a1,a2]  = Just (r, MO_Nat_Eq,    [a1,a2])

translateOp _ _ _ = Nothing

\end{code}
