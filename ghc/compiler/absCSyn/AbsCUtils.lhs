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

import AbsCSyn
import CLabel		( mkMAP_FROZEN_infoLabel )
import Digraph		( stronglyConnComp, SCC(..) )
import DataCon		( fIRST_TAG, ConTag )
import Literal		( literalPrimRep, mkMachWord, mkMachInt )
import PrimRep		( getPrimRepSize, PrimRep(..) )
import PrimOp		( PrimOp(..) )
import MachOp		( MachOp(..), isDefinitelyInlineMachOp )
import Unique		( Unique{-instance Eq-} )
import UniqSupply	( uniqFromSupply, uniqsFromSupply, splitUniqSupply, 
			  UniqSupply )
import CmdLineOpts      ( opt_EmitCExternDecls )
import ForeignCall	( ForeignCall(..), CCallSpec(..), CCallTarget(..), Safety(..),
			  isDynamicTarget, isCasmTarget, defaultCCallConv )
import StgSyn		( StgOp(..) )
import SMRep		( arrPtrsHdrSize, arrWordsHdrSize, fixedHdrSize )
import Constants	( wORD_SIZE )
import Maybes		( Maybe012(..) )
import Outputable
import Panic		( panic )
import FastTypes

import Maybe		( isJust, maybeToList )

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
mkAlgAltsCSwitch :: CAddrMode -> [(ConTag, AbstractC)] -> AbstractC -> AbstractC

mkAlgAltsCSwitch scrutinee tagged_alts deflt_absc
 | isJust (nonemptyAbsC deflt_absc) 
	= CSwitch scrutinee (adjust tagged_alts) deflt_absc
 | otherwise 
	= CSwitch scrutinee (adjust rest) first_alt
 where
   -- it's ok to convert one of the alts into a default if we don't already have
   -- one, because this is an algebraic case and we're guaranteed that the tag 
   -- will match one of the branches.
   ((_,first_alt):rest) = tagged_alts

   -- Adjust the tags in the switch to start at zero.
   -- This is the convention used by primitive ops which return algebraic
   -- data types.  Why?	 Because for two-constructor types, zero is faster
   -- to create and distinguish from 1 than are 1 and 2.

   -- We also need to convert to Literals to keep the CSwitch happy
   adjust tagged_alts
     = [ (mkMachWord (toInteger (tag - fIRST_TAG)), abs_c)
       | (tag, abs_c) <- tagged_alts ]
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
magicIdPrimRep Su		    = PtrRep
magicIdPrimRep SpLim		    = PtrRep
magicIdPrimRep Hp		    = PtrRep
magicIdPrimRep HpLim		    = PtrRep
magicIdPrimRep CurCostCentre	    = CostCentreRep
magicIdPrimRep VoidReg		    = VoidRep
magicIdPrimRep CurrentTSO	    = ThreadIdRep
magicIdPrimRep CurrentNursery	    = PtrRep
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
getAmodeRep (CMem rep addr)                 = rep
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

flatAbsC (CClosureInfoAndCode cl_info slow maybe_fast descr)
  = flatAbsC slow		`thenFlt` \ (slow_heres, slow_tops) ->
    flat_maybe maybe_fast	`thenFlt` \ (fast_heres, fast_tops) ->
    returnFlt (AbsCNop, mkAbstractCs [slow_tops, fast_tops,
       CClosureInfoAndCode cl_info slow_heres fast_heres descr]
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
  |  is_dynamic	 			 -- Emit a typedef if its a dynamic call
     || (opt_EmitCExternDecls && not (isCasmTarget target)) -- or we want extern decls
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
  | str == SLIT("TICK_CTR") 	= returnFlt (AbsCNop, stmt)
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
                    (CCall (CCallSpec (CasmTarget (_PK_ (mktxt op_str))) 
                                      defaultCCallConv PlaySafe))
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
flatAbsC stmt@(CStaticClosure _ _ _ _)		= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CClosureTbl _)			= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CSRT _ _)	  		= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CBitmap _ _)	  		= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CCostCentreDecl _ _) 		= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CCostCentreStackDecl _)		= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CSplitMarker) 			= returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CRetVector _ _ _ _)              = returnFlt (AbsCNop, stmt)
flatAbsC stmt@(CModuleInitBlock _ _)            = returnFlt (AbsCNop, stmt)
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


------------------------------------------------------------------------------

-- Assumes no volatiles
mkHalfWord_HIADDR res arg
#  if WORDS_BIGENDIAN
   = CMachOpStmt (Just1 res) MO_Nat_And [arg, CLit (mkMachWord halfword_mask)] Nothing
#  else
   = CMachOpStmt (Just1 res) MO_Nat_Shr [arg, CLit (mkMachWord halfword_shift)] Nothing
#  endif
   where
      (halfword_mask, halfword_shift)
         | wORD_SIZE == 4  = (65535,               16)
         | wORD_SIZE == 8  = (4294967295::Integer, 32)


mkTemp :: PrimRep -> FlatM CAddrMode
mkTemp rep 
   = getUniqFlt `thenFlt` \ uniq -> returnFlt (CTemp uniq rep)

mkTemps = mapFlt mkTemp

mkDerefOff :: PrimRep -> CAddrMode -> Int -> CAddrMode
mkDerefOff rep base off
   | off == 0	-- optimisation
   = CMem rep base
   | otherwise
   = CMem rep (CAddr (CIndex base (CLit (mkMachInt (toInteger off))) rep))

mkNoDerefOff :: PrimRep -> CAddrMode -> Int -> CAddrMode
mkNoDerefOff rep base off
   = CAddr (CIndex base (CLit (mkMachInt (toInteger off))) rep)

-- Sigh.  This is done in 3 seperate places.  Should be
-- commoned up (here, in pprAbsC of COpStmt, and presumably
-- somewhere in the NCG).
non_void_amode amode 
   = case getAmodeRep amode of
        VoidRep -> False
        k       -> True

doIndexOffForeignObjOp rep res addr idx
   = Just (Just1 res, MO_ReadOSBI fixedHdrSize rep, [addr,idx])

doIndexOffAddrOp rep res addr idx
   = Just (Just1 res, MO_ReadOSBI 0 rep, [addr,idx])

doIndexByteArrayOp rep res addr idx
   = Just (Just1 res, MO_ReadOSBI arrWordsHdrSize rep, [addr,idx])

doWriteOffAddrOp rep addr idx val
   = Just (Just0, MO_WriteOSBI 0 rep, [addr,idx,val])

doWriteByteArrayOp rep addr idx val
   = Just (Just0, MO_WriteOSBI arrWordsHdrSize rep, [addr,idx,val])

-- Simple dyadic op but one for which we need to cast first arg to
-- be sure of correctness
translateOp_dyadic_cast1 mop res cast_arg1_to arg1 arg2 vols
   = mkTemp cast_arg1_to		`thenFlt` \ arg1casted ->
     (returnFlt . CSequential) [
        CAssign arg1casted arg1,
        CMachOpStmt (Just1 res) mop [arg1casted,arg2]
           (if isDefinitelyInlineMachOp mop then Nothing else Just vols)
     ]

------------------------------------------------------------------------------

dscCOpStmt :: [CAddrMode]	-- Results
           -> PrimOp
           -> [CAddrMode]	-- Arguments
           -> [MagicId]		-- Potentially volatile/live registers
                         	-- (to save/restore around the op)
           -> FlatM AbstractC

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
        CMachOpStmt (Just1 w) 
           MO_NatU_Mul [w, CLit (mkMachInt (toInteger wORD_SIZE))] (Just vols),
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
         CMachOpStmt (Just1 ptr) MO_NatU_to_NatP [arg] Nothing,
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
        CMachOpStmt (Just1 res) MO_Nat_Eq [sn1,sn2] Nothing
     ]

-- #define addrToHValuezh(r,a) r=(P_)a
dscCOpStmt [res] AddrToHValueOp [arg] vols
   = returnFlt 
        (CAssign res arg)

-- #define dataToTagzh(r,a)  r=(GET_TAG(((StgClosure *)a)->header.info))
dscCOpStmt [res] DataToTagOp [arg] vols
   = mkTemps [PtrRep, WordRep]		`thenFlt` \ [t_infoptr, t_theword] ->
     (returnFlt . CSequential) [
        CAssign t_infoptr (mkDerefOff PtrRep arg 0),
        CAssign t_theword (mkDerefOff WordRep t_infoptr (-1)),
        mkHalfWord_HIADDR res t_theword
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
        CMachOpStmt (Just1 a1casted) MO_NatP_to_NatU [a1] Nothing,
        CMachOpStmt (Just1 r) MO_NatU_Rem [a1casted,a2] Nothing
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



translateOp [r] ReadArrayOp [obj,ix] 
   = Just (Just1 r, MO_ReadOSBI arrPtrsHdrSize PtrRep, [obj,ix])
translateOp [r] IndexArrayOp [obj,ix] 
   = Just (Just1 r, MO_ReadOSBI arrPtrsHdrSize PtrRep, [obj,ix])
translateOp [] WriteArrayOp [obj,ix,v] 
   = Just (Just0, MO_WriteOSBI arrPtrsHdrSize PtrRep, [obj,ix,v])

-- IndexXXXoffForeignObj

translateOp [r] IndexOffForeignObjOp_Char [a,i]  = doIndexOffForeignObjOp Word8Rep r a i
translateOp [r] IndexOffForeignObjOp_WideChar [a,i]  = doIndexOffForeignObjOp Word32Rep r a i
translateOp [r] IndexOffForeignObjOp_Int [a,i]  = doIndexOffForeignObjOp IntRep r a i
translateOp [r] IndexOffForeignObjOp_Word [a,i]  = doIndexOffForeignObjOp WordRep r a i
translateOp [r] IndexOffForeignObjOp_Addr [a,i]  = doIndexOffForeignObjOp AddrRep r a i
translateOp [r] IndexOffForeignObjOp_Float [a,i]  = doIndexOffForeignObjOp FloatRep r a i
translateOp [r] IndexOffForeignObjOp_Double [a,i]  = doIndexOffForeignObjOp DoubleRep r a i
translateOp [r] IndexOffForeignObjOp_StablePtr [a,i]  = doIndexOffForeignObjOp StablePtrRep r a i

translateOp [r] IndexOffForeignObjOp_Int8  [a,i] = doIndexOffForeignObjOp Int8Rep  r a i
translateOp [r] IndexOffForeignObjOp_Int16 [a,i] = doIndexOffForeignObjOp Int16Rep r a i
translateOp [r] IndexOffForeignObjOp_Int32 [a,i] = doIndexOffForeignObjOp Int32Rep r a i
translateOp [r] IndexOffForeignObjOp_Int64 [a,i] = doIndexOffForeignObjOp Int64Rep r a i

translateOp [r] IndexOffForeignObjOp_Word8  [a,i] = doIndexOffForeignObjOp Word8Rep  r a i
translateOp [r] IndexOffForeignObjOp_Word16 [a,i] = doIndexOffForeignObjOp Word16Rep r a i
translateOp [r] IndexOffForeignObjOp_Word32 [a,i] = doIndexOffForeignObjOp Word32Rep r a i
translateOp [r] IndexOffForeignObjOp_Word64 [a,i] = doIndexOffForeignObjOp Word64Rep r a i

-- IndexXXXoffAddr

translateOp [r] IndexOffAddrOp_Char [a,i]  = doIndexOffAddrOp Word8Rep r a i
translateOp [r] IndexOffAddrOp_WideChar [a,i]  = doIndexOffAddrOp Word32Rep r a i
translateOp [r] IndexOffAddrOp_Int [a,i]  = doIndexOffAddrOp IntRep r a i
translateOp [r] IndexOffAddrOp_Word [a,i]  = doIndexOffAddrOp WordRep r a i
translateOp [r] IndexOffAddrOp_Addr [a,i]  = doIndexOffAddrOp AddrRep r a i
translateOp [r] IndexOffAddrOp_Float [a,i]  = doIndexOffAddrOp FloatRep r a i
translateOp [r] IndexOffAddrOp_Double [a,i]  = doIndexOffAddrOp DoubleRep r a i
translateOp [r] IndexOffAddrOp_StablePtr [a,i]  = doIndexOffAddrOp StablePtrRep r a i

translateOp [r] IndexOffAddrOp_Int8  [a,i] = doIndexOffAddrOp Int8Rep  r a i
translateOp [r] IndexOffAddrOp_Int16 [a,i] = doIndexOffAddrOp Int16Rep r a i
translateOp [r] IndexOffAddrOp_Int32 [a,i] = doIndexOffAddrOp Int32Rep r a i
translateOp [r] IndexOffAddrOp_Int64 [a,i] = doIndexOffAddrOp Int64Rep r a i

translateOp [r] IndexOffAddrOp_Word8  [a,i] = doIndexOffAddrOp Word8Rep  r a i
translateOp [r] IndexOffAddrOp_Word16 [a,i] = doIndexOffAddrOp Word16Rep r a i
translateOp [r] IndexOffAddrOp_Word32 [a,i] = doIndexOffAddrOp Word32Rep r a i
translateOp [r] IndexOffAddrOp_Word64 [a,i] = doIndexOffAddrOp Word64Rep r a i

-- ReadXXXoffAddr, which are identical, for our purposes, to IndexXXXoffAddr.

translateOp [r] ReadOffAddrOp_Char [a,i]  = doIndexOffAddrOp Word8Rep r a i
translateOp [r] ReadOffAddrOp_WideChar [a,i]  = doIndexOffAddrOp Word32Rep r a i
translateOp [r] ReadOffAddrOp_Int [a,i]  = doIndexOffAddrOp IntRep r a i
translateOp [r] ReadOffAddrOp_Word [a,i]  = doIndexOffAddrOp WordRep r a i
translateOp [r] ReadOffAddrOp_Addr [a,i]  = doIndexOffAddrOp AddrRep r a i
translateOp [r] ReadOffAddrOp_Float [a,i]  = doIndexOffAddrOp FloatRep r a i
translateOp [r] ReadOffAddrOp_Double [a,i]  = doIndexOffAddrOp DoubleRep r a i
translateOp [r] ReadOffAddrOp_StablePtr [a,i]  = doIndexOffAddrOp StablePtrRep r a i

translateOp [r] ReadOffAddrOp_Int8  [a,i] = doIndexOffAddrOp Int8Rep  r a i
translateOp [r] ReadOffAddrOp_Int16 [a,i] = doIndexOffAddrOp Int16Rep r a i
translateOp [r] ReadOffAddrOp_Int32 [a,i] = doIndexOffAddrOp Int32Rep r a i
translateOp [r] ReadOffAddrOp_Int64 [a,i] = doIndexOffAddrOp Int64Rep r a i

translateOp [r] ReadOffAddrOp_Word8  [a,i] = doIndexOffAddrOp Word8Rep  r a i
translateOp [r] ReadOffAddrOp_Word16 [a,i] = doIndexOffAddrOp Word16Rep r a i
translateOp [r] ReadOffAddrOp_Word32 [a,i] = doIndexOffAddrOp Word32Rep r a i
translateOp [r] ReadOffAddrOp_Word64 [a,i] = doIndexOffAddrOp Word64Rep r a i

-- WriteXXXoffAddr

translateOp [] WriteOffAddrOp_Char [a,i,x]  = doWriteOffAddrOp Word8Rep a i x
translateOp [] WriteOffAddrOp_WideChar [a,i,x]  = doWriteOffAddrOp Word32Rep a i x
translateOp [] WriteOffAddrOp_Int [a,i,x]  = doWriteOffAddrOp IntRep a i x
translateOp [] WriteOffAddrOp_Word [a,i,x]  = doWriteOffAddrOp WordRep a i x
translateOp [] WriteOffAddrOp_Addr [a,i,x]  = doWriteOffAddrOp AddrRep a i x
translateOp [] WriteOffAddrOp_Float [a,i,x]  = doWriteOffAddrOp FloatRep a i x
translateOp [] WriteOffAddrOp_ForeignObj [a,i,x]  = doWriteOffAddrOp ForeignObjRep a i x
translateOp [] WriteOffAddrOp_Double [a,i,x]  = doWriteOffAddrOp DoubleRep a i x
translateOp [] WriteOffAddrOp_StablePtr [a,i,x]  = doWriteOffAddrOp StablePtrRep a i x

translateOp [] WriteOffAddrOp_Int8  [a,i,x] = doWriteOffAddrOp Int8Rep  a i x
translateOp [] WriteOffAddrOp_Int16 [a,i,x] = doWriteOffAddrOp Int16Rep a i x
translateOp [] WriteOffAddrOp_Int32 [a,i,x] = doWriteOffAddrOp Int32Rep a i x
translateOp [] WriteOffAddrOp_Int64 [a,i,x] = doWriteOffAddrOp Int64Rep a i x

translateOp [] WriteOffAddrOp_Word8  [a,i,x] = doWriteOffAddrOp Word8Rep  a i x
translateOp [] WriteOffAddrOp_Word16 [a,i,x] = doWriteOffAddrOp Word16Rep a i x
translateOp [] WriteOffAddrOp_Word32 [a,i,x] = doWriteOffAddrOp Word32Rep a i x
translateOp [] WriteOffAddrOp_Word64 [a,i,x] = doWriteOffAddrOp Word64Rep a i x

-- IndexXXXArray

translateOp [r] IndexByteArrayOp_Char [a,i]  = doIndexByteArrayOp Word8Rep r a i
translateOp [r] IndexByteArrayOp_WideChar [a,i]  = doIndexByteArrayOp Word32Rep r a i
translateOp [r] IndexByteArrayOp_Int [a,i]  = doIndexByteArrayOp IntRep r a i
translateOp [r] IndexByteArrayOp_Word [a,i]  = doIndexByteArrayOp WordRep r a i
translateOp [r] IndexByteArrayOp_Addr [a,i]  = doIndexByteArrayOp AddrRep r a i
translateOp [r] IndexByteArrayOp_Float [a,i]  = doIndexByteArrayOp FloatRep r a i
translateOp [r] IndexByteArrayOp_Double [a,i]  = doIndexByteArrayOp DoubleRep r a i
translateOp [r] IndexByteArrayOp_StablePtr [a,i]  = doIndexByteArrayOp StablePtrRep r a i

translateOp [r] IndexByteArrayOp_Int8  [a,i] = doIndexByteArrayOp Int8Rep  r a i
translateOp [r] IndexByteArrayOp_Int16 [a,i] = doIndexByteArrayOp Int16Rep  r a i
translateOp [r] IndexByteArrayOp_Int32 [a,i] = doIndexByteArrayOp Int32Rep  r a i
translateOp [r] IndexByteArrayOp_Int64 [a,i] = doIndexByteArrayOp Int64Rep  r a i

translateOp [r] IndexByteArrayOp_Word8  [a,i] = doIndexByteArrayOp Word8Rep  r a i
translateOp [r] IndexByteArrayOp_Word16 [a,i] = doIndexByteArrayOp Word16Rep  r a i
translateOp [r] IndexByteArrayOp_Word32 [a,i] = doIndexByteArrayOp Word32Rep  r a i
translateOp [r] IndexByteArrayOp_Word64 [a,i] = doIndexByteArrayOp Word64Rep  r a i

-- ReadXXXArray, identical to IndexXXXArray.

translateOp [r] ReadByteArrayOp_Char [a,i]  = doIndexByteArrayOp Word8Rep r a i
translateOp [r] ReadByteArrayOp_WideChar [a,i]  = doIndexByteArrayOp Word32Rep r a i
translateOp [r] ReadByteArrayOp_Int [a,i]  = doIndexByteArrayOp IntRep r a i
translateOp [r] ReadByteArrayOp_Word [a,i]  = doIndexByteArrayOp WordRep r a i
translateOp [r] ReadByteArrayOp_Addr [a,i]  = doIndexByteArrayOp AddrRep r a i
translateOp [r] ReadByteArrayOp_Float [a,i]  = doIndexByteArrayOp FloatRep r a i
translateOp [r] ReadByteArrayOp_Double [a,i]  = doIndexByteArrayOp DoubleRep r a i
translateOp [r] ReadByteArrayOp_StablePtr [a,i]  = doIndexByteArrayOp StablePtrRep r a i

translateOp [r] ReadByteArrayOp_Int8  [a,i] = doIndexByteArrayOp Int8Rep  r a i
translateOp [r] ReadByteArrayOp_Int16 [a,i] = doIndexByteArrayOp Int16Rep  r a i
translateOp [r] ReadByteArrayOp_Int32 [a,i] = doIndexByteArrayOp Int32Rep  r a i
translateOp [r] ReadByteArrayOp_Int64 [a,i] = doIndexByteArrayOp Int64Rep  r a i

translateOp [r] ReadByteArrayOp_Word8  [a,i] = doIndexByteArrayOp Word8Rep  r a i
translateOp [r] ReadByteArrayOp_Word16 [a,i] = doIndexByteArrayOp Word16Rep  r a i
translateOp [r] ReadByteArrayOp_Word32 [a,i] = doIndexByteArrayOp Word32Rep  r a i
translateOp [r] ReadByteArrayOp_Word64 [a,i] = doIndexByteArrayOp Word64Rep  r a i

-- WriteXXXArray

translateOp [] WriteByteArrayOp_Char [a,i,x]  = doWriteByteArrayOp Word8Rep a i x
translateOp [] WriteByteArrayOp_WideChar [a,i,x]  = doWriteByteArrayOp Word32Rep a i x
translateOp [] WriteByteArrayOp_Int [a,i,x]  = doWriteByteArrayOp IntRep a i x
translateOp [] WriteByteArrayOp_Word [a,i,x]  = doWriteByteArrayOp WordRep a i x
translateOp [] WriteByteArrayOp_Addr [a,i,x]  = doWriteByteArrayOp AddrRep a i x
translateOp [] WriteByteArrayOp_Float [a,i,x]  = doWriteByteArrayOp FloatRep a i x
translateOp [] WriteByteArrayOp_Double [a,i,x]  = doWriteByteArrayOp DoubleRep a i x
translateOp [] WriteByteArrayOp_StablePtr [a,i,x]  = doWriteByteArrayOp StablePtrRep a i x

translateOp [] WriteByteArrayOp_Int8  [a,i,x] = doWriteByteArrayOp Int8Rep  a i x
translateOp [] WriteByteArrayOp_Int16 [a,i,x] = doWriteByteArrayOp Int16Rep  a i x
translateOp [] WriteByteArrayOp_Int32 [a,i,x] = doWriteByteArrayOp Int32Rep  a i x
translateOp [] WriteByteArrayOp_Int64 [a,i,x] = doWriteByteArrayOp Int64Rep  a i x

translateOp [] WriteByteArrayOp_Word8  [a,i,x] = doWriteByteArrayOp Word8Rep  a i x
translateOp [] WriteByteArrayOp_Word16 [a,i,x] = doWriteByteArrayOp Word16Rep  a i x
translateOp [] WriteByteArrayOp_Word32 [a,i,x] = doWriteByteArrayOp Word32Rep  a i x
translateOp [] WriteByteArrayOp_Word64 [a,i,x] = doWriteByteArrayOp Word64Rep  a i x

-- Native word signless ops

translateOp [r] IntAddOp       [a1,a2] = Just (Just1 r, MO_Nat_Add,        [a1,a2])
translateOp [r] IntSubOp       [a1,a2] = Just (Just1 r, MO_Nat_Sub,        [a1,a2])
translateOp [r] WordAddOp      [a1,a2] = Just (Just1 r, MO_Nat_Add,        [a1,a2])
translateOp [r] WordSubOp      [a1,a2] = Just (Just1 r, MO_Nat_Sub,        [a1,a2])
translateOp [r] AddrAddOp      [a1,a2] = Just (Just1 r, MO_Nat_Add,        [a1,a2])
translateOp [r] AddrSubOp      [a1,a2] = Just (Just1 r, MO_Nat_Sub,        [a1,a2])

translateOp [r] IntEqOp        [a1,a2] = Just (Just1 r, MO_Nat_Eq,         [a1,a2])
translateOp [r] IntNeOp        [a1,a2] = Just (Just1 r, MO_Nat_Ne,         [a1,a2])
translateOp [r] WordEqOp       [a1,a2] = Just (Just1 r, MO_Nat_Eq,         [a1,a2])
translateOp [r] WordNeOp       [a1,a2] = Just (Just1 r, MO_Nat_Ne,         [a1,a2])
translateOp [r] AddrEqOp       [a1,a2] = Just (Just1 r, MO_Nat_Eq,         [a1,a2])
translateOp [r] AddrNeOp       [a1,a2] = Just (Just1 r, MO_Nat_Ne,         [a1,a2])

translateOp [r] AndOp          [a1,a2] = Just (Just1 r, MO_Nat_And,        [a1,a2])
translateOp [r] OrOp           [a1,a2] = Just (Just1 r, MO_Nat_Or,         [a1,a2])
translateOp [r] XorOp          [a1,a2] = Just (Just1 r, MO_Nat_Xor,        [a1,a2])
translateOp [r] NotOp          [a1]    = Just (Just1 r, MO_Nat_Not,        [a1])

-- Native word signed ops

translateOp [r] IntMulOp       [a1,a2] = Just (Just1 r, MO_NatS_Mul,       [a1,a2])
translateOp [r] IntQuotOp      [a1,a2] = Just (Just1 r, MO_NatS_Quot,      [a1,a2])
translateOp [r] IntRemOp       [a1,a2] = Just (Just1 r, MO_NatS_Rem,       [a1,a2])
translateOp [r] IntNegOp       [a1]    = Just (Just1 r, MO_NatS_Neg,       [a1])

translateOp [r,c] IntAddCOp    [a1,a2] = Just (Just2 r c, MO_NatS_AddC,    [a1,a2])
translateOp [r,c] IntSubCOp    [a1,a2] = Just (Just2 r c, MO_NatS_SubC,    [a1,a2])
translateOp [r,c] IntMulCOp    [a1,a2] = Just (Just2 r c, MO_NatS_MulC,    [a1,a2])

translateOp [r] IntGeOp        [a1,a2] = Just (Just1 r, MO_NatS_Ge,        [a1,a2])
translateOp [r] IntLeOp        [a1,a2] = Just (Just1 r, MO_NatS_Le,        [a1,a2])
translateOp [r] IntGtOp        [a1,a2] = Just (Just1 r, MO_NatS_Gt,        [a1,a2])
translateOp [r] IntLtOp        [a1,a2] = Just (Just1 r, MO_NatS_Lt,        [a1,a2])

-- Native word unsigned ops

translateOp [r] WordGeOp       [a1,a2] = Just (Just1 r, MO_NatU_Ge,        [a1,a2])
translateOp [r] WordLeOp       [a1,a2] = Just (Just1 r, MO_NatU_Le,        [a1,a2])
translateOp [r] WordGtOp       [a1,a2] = Just (Just1 r, MO_NatU_Gt,        [a1,a2])
translateOp [r] WordLtOp       [a1,a2] = Just (Just1 r, MO_NatU_Lt,        [a1,a2])

translateOp [r] WordMulOp      [a1,a2] = Just (Just1 r, MO_NatU_Mul,       [a1,a2])
translateOp [r] WordQuotOp     [a1,a2] = Just (Just1 r, MO_NatU_Quot,      [a1,a2])
translateOp [r] WordRemOp      [a1,a2] = Just (Just1 r, MO_NatU_Rem,       [a1,a2])

translateOp [r] AddrGeOp       [a1,a2] = Just (Just1 r, MO_NatU_Ge,        [a1,a2])
translateOp [r] AddrLeOp       [a1,a2] = Just (Just1 r, MO_NatU_Le,        [a1,a2])
translateOp [r] AddrGtOp       [a1,a2] = Just (Just1 r, MO_NatU_Gt,        [a1,a2])
translateOp [r] AddrLtOp       [a1,a2] = Just (Just1 r, MO_NatU_Lt,        [a1,a2])

-- 32-bit unsigned ops

translateOp [r] CharEqOp       [a1,a2] = Just (Just1 r, MO_32U_Eq,        [a1,a2])
translateOp [r] CharNeOp       [a1,a2] = Just (Just1 r, MO_32U_Ne,        [a1,a2])
translateOp [r] CharGeOp       [a1,a2] = Just (Just1 r, MO_32U_Ge,        [a1,a2])
translateOp [r] CharLeOp       [a1,a2] = Just (Just1 r, MO_32U_Le,        [a1,a2])
translateOp [r] CharGtOp       [a1,a2] = Just (Just1 r, MO_32U_Gt,        [a1,a2])
translateOp [r] CharLtOp       [a1,a2] = Just (Just1 r, MO_32U_Lt,        [a1,a2])

-- Double ops

translateOp [r] DoubleEqOp     [a1,a2] = Just (Just1 r, MO_Dbl_Eq,      [a1,a2])
translateOp [r] DoubleNeOp     [a1,a2] = Just (Just1 r, MO_Dbl_Ne,      [a1,a2])
translateOp [r] DoubleGeOp     [a1,a2] = Just (Just1 r, MO_Dbl_Ge,      [a1,a2])
translateOp [r] DoubleLeOp     [a1,a2] = Just (Just1 r, MO_Dbl_Le,      [a1,a2])
translateOp [r] DoubleGtOp     [a1,a2] = Just (Just1 r, MO_Dbl_Gt,      [a1,a2])
translateOp [r] DoubleLtOp     [a1,a2] = Just (Just1 r, MO_Dbl_Lt,      [a1,a2])

translateOp [r] DoubleAddOp    [a1,a2] = Just (Just1 r, MO_Dbl_Add,    [a1,a2])
translateOp [r] DoubleSubOp    [a1,a2] = Just (Just1 r, MO_Dbl_Sub,    [a1,a2])
translateOp [r] DoubleMulOp    [a1,a2] = Just (Just1 r, MO_Dbl_Mul,    [a1,a2])
translateOp [r] DoubleDivOp    [a1,a2] = Just (Just1 r, MO_Dbl_Div,    [a1,a2])
translateOp [r] DoublePowerOp  [a1,a2] = Just (Just1 r, MO_Dbl_Pwr,    [a1,a2])

translateOp [r] DoubleSinOp    [a1]    = Just (Just1 r, MO_Dbl_Sin,    [a1])
translateOp [r] DoubleCosOp    [a1]    = Just (Just1 r, MO_Dbl_Cos,    [a1])
translateOp [r] DoubleTanOp    [a1]    = Just (Just1 r, MO_Dbl_Tan,    [a1])
translateOp [r] DoubleSinhOp   [a1]    = Just (Just1 r, MO_Dbl_Sinh,   [a1])
translateOp [r] DoubleCoshOp   [a1]    = Just (Just1 r, MO_Dbl_Cosh,   [a1])
translateOp [r] DoubleTanhOp   [a1]    = Just (Just1 r, MO_Dbl_Tanh,   [a1])
translateOp [r] DoubleAsinOp   [a1]    = Just (Just1 r, MO_Dbl_Asin,    [a1])
translateOp [r] DoubleAcosOp   [a1]    = Just (Just1 r, MO_Dbl_Acos,    [a1])
translateOp [r] DoubleAtanOp   [a1]    = Just (Just1 r, MO_Dbl_Atan,    [a1])
translateOp [r] DoubleLogOp    [a1]    = Just (Just1 r, MO_Dbl_Log,    [a1])
translateOp [r] DoubleExpOp    [a1]    = Just (Just1 r, MO_Dbl_Exp,    [a1])
translateOp [r] DoubleSqrtOp   [a1]    = Just (Just1 r, MO_Dbl_Sqrt,    [a1])
translateOp [r] DoubleNegOp    [a1]    = Just (Just1 r, MO_Dbl_Neg,    [a1])

-- Float ops

translateOp [r] FloatEqOp     [a1,a2] = Just (Just1 r, MO_Flt_Eq,      [a1,a2])
translateOp [r] FloatNeOp     [a1,a2] = Just (Just1 r, MO_Flt_Ne,      [a1,a2])
translateOp [r] FloatGeOp     [a1,a2] = Just (Just1 r, MO_Flt_Ge,      [a1,a2])
translateOp [r] FloatLeOp     [a1,a2] = Just (Just1 r, MO_Flt_Le,      [a1,a2])
translateOp [r] FloatGtOp     [a1,a2] = Just (Just1 r, MO_Flt_Gt,      [a1,a2])
translateOp [r] FloatLtOp     [a1,a2] = Just (Just1 r, MO_Flt_Lt,      [a1,a2])

translateOp [r] FloatAddOp    [a1,a2] = Just (Just1 r, MO_Flt_Add,    [a1,a2])
translateOp [r] FloatSubOp    [a1,a2] = Just (Just1 r, MO_Flt_Sub,    [a1,a2])
translateOp [r] FloatMulOp    [a1,a2] = Just (Just1 r, MO_Flt_Mul,    [a1,a2])
translateOp [r] FloatDivOp    [a1,a2] = Just (Just1 r, MO_Flt_Div,    [a1,a2])
translateOp [r] FloatPowerOp  [a1,a2] = Just (Just1 r, MO_Flt_Pwr,    [a1,a2])

translateOp [r] FloatSinOp    [a1]    = Just (Just1 r, MO_Flt_Sin,    [a1])
translateOp [r] FloatCosOp    [a1]    = Just (Just1 r, MO_Flt_Cos,    [a1])
translateOp [r] FloatTanOp    [a1]    = Just (Just1 r, MO_Flt_Tan,    [a1])
translateOp [r] FloatSinhOp   [a1]    = Just (Just1 r, MO_Flt_Sinh,   [a1])
translateOp [r] FloatCoshOp   [a1]    = Just (Just1 r, MO_Flt_Cosh,   [a1])
translateOp [r] FloatTanhOp   [a1]    = Just (Just1 r, MO_Flt_Tanh,   [a1])
translateOp [r] FloatAsinOp   [a1]    = Just (Just1 r, MO_Flt_Asin,    [a1])
translateOp [r] FloatAcosOp   [a1]    = Just (Just1 r, MO_Flt_Acos,    [a1])
translateOp [r] FloatAtanOp   [a1]    = Just (Just1 r, MO_Flt_Atan,    [a1])
translateOp [r] FloatLogOp    [a1]    = Just (Just1 r, MO_Flt_Log,    [a1])
translateOp [r] FloatExpOp    [a1]    = Just (Just1 r, MO_Flt_Exp,    [a1])
translateOp [r] FloatSqrtOp   [a1]    = Just (Just1 r, MO_Flt_Sqrt,    [a1])
translateOp [r] FloatNegOp    [a1]    = Just (Just1 r, MO_Flt_Neg,    [a1])

-- Conversions

translateOp [r] Int2DoubleOp [a1]    = Just (Just1 r, MO_NatS_to_Dbl,    [a1])
translateOp [r] Double2IntOp [a1]    = Just (Just1 r, MO_Dbl_to_NatS,    [a1])

translateOp [r] Int2FloatOp  [a1]    = Just (Just1 r, MO_NatS_to_Flt,    [a1])
translateOp [r] Float2IntOp  [a1]    = Just (Just1 r, MO_Flt_to_NatS,    [a1])

translateOp [r] Float2DoubleOp [a1]    = Just (Just1 r, MO_Flt_to_Dbl,    [a1])
translateOp [r] Double2FloatOp [a1]    = Just (Just1 r, MO_Dbl_to_Flt,    [a1])

translateOp [r] Int2WordOp   [a1]    = Just (Just1 r, MO_NatS_to_NatU,   [a1])
translateOp [r] Word2IntOp   [a1]    = Just (Just1 r, MO_NatU_to_NatS,   [a1])

translateOp [r] Int2AddrOp   [a1]    = Just (Just1 r, MO_NatS_to_NatP,   [a1])
translateOp [r] Addr2IntOp   [a1]    = Just (Just1 r, MO_NatP_to_NatS,   [a1])

translateOp [r] OrdOp    [a1]    = Just (Just1 r, MO_32U_to_NatS,    [a1])
translateOp [r] ChrOp    [a1]    = Just (Just1 r, MO_NatS_to_32U,    [a1])

translateOp [r] Narrow8IntOp   [a1]    = Just (Just1 r, MO_8S_to_NatS,    [a1])
translateOp [r] Narrow16IntOp  [a1]    = Just (Just1 r, MO_16S_to_NatS,    [a1])
translateOp [r] Narrow32IntOp  [a1]    = Just (Just1 r, MO_32S_to_NatS,    [a1])

translateOp [r] Narrow8WordOp   [a1]    = Just (Just1 r, MO_8U_to_NatU,    [a1])
translateOp [r] Narrow16WordOp  [a1]    = Just (Just1 r, MO_16U_to_NatU,    [a1])
translateOp [r] Narrow32WordOp  [a1]    = Just (Just1 r, MO_32U_to_NatU,    [a1])

translateOp [r] SameMutVarOp   [a1,a2]  = Just (Just1 r, MO_Nat_Eq,    [a1,a2])
translateOp [r] SameMVarOp     [a1,a2]  = Just (Just1 r, MO_Nat_Eq,    [a1,a2])
translateOp [r] SameMutableArrayOp  [a1,a2]  = Just (Just1 r, MO_Nat_Eq,    [a1,a2])
translateOp [r] SameMutableByteArrayOp [a1,a2]  = Just (Just1 r, MO_Nat_Eq,    [a1,a2])
translateOp [r] EqForeignObj [a1,a2]  = Just (Just1 r, MO_Nat_Eq,    [a1,a2])
translateOp [r] EqStablePtrOp [a1,a2]  = Just (Just1 r, MO_Nat_Eq,    [a1,a2])

translateOp _ _ _ = Nothing

\end{code}
