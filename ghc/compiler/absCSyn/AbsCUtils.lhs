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
import Digraph		( stronglyConnComp, SCC(..) )
import DataCon		( fIRST_TAG, ConTag )
import Const		( literalPrimRep, mkMachWord )
import PrimRep		( getPrimRepSize, PrimRep(..) )
import Unique		( Unique{-instance Eq-} )
import UniqSupply	( uniqFromSupply, uniqsFromSupply, splitUniqSupply, 
			  UniqSupply )
import CmdLineOpts      ( opt_ProduceC, opt_EmitCExternDecls )
import Maybes		( maybeToBool )
import PrimOp		( PrimOp(..) )
import Panic		( panic )

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
 = CSwitch scrutinee (adjust tagged_alts) deflt_absc
 where
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
getAmodeRep (CLitLit _ kind)	    	    = kind
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

getUniqsFlt :: Int -> FlatM [Unique]
getUniqsFlt i us = uniqsFromSupply i us
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

flatAbsC stmt@(COpStmt results td@(CCallOp _ _ _ _) args vol_regs)
  | isCandidate && maybeToBool opt_ProduceC
  = returnFlt (stmt, tdef)
  where
    (isCandidate, isDyn) =
      case td of 
        CCallOp (Right _) _ _ _      -> (True, True)
	CCallOp (Left _) is_asm _ _  -> (opt_EmitCExternDecls && not is_asm, False)
        _			     -> (False, False)

    tdef = CCallTypedef isDyn td results args

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
flatAbsC stmt@(CMacroStmt macro amodes) 	= returnFlt (stmt, AbsCNop)
flatAbsC stmt@(CCallProfCCMacro str amodes) 	= returnFlt (stmt, AbsCNop)
flatAbsC stmt@(CAssign dest source) 		= returnFlt (stmt, AbsCNop)
flatAbsC stmt@(CJump target) 			= returnFlt (stmt, AbsCNop)
flatAbsC stmt@(CFallThrough target) 		= returnFlt (stmt, AbsCNop)
flatAbsC stmt@(CReturn target return_info) 	= returnFlt (stmt, AbsCNop)
flatAbsC stmt@(CInitHdr a b cc) 		= returnFlt (stmt, AbsCNop)
flatAbsC stmt@(COpStmt results op args vol_regs)= returnFlt (stmt, AbsCNop)

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

type CEdge = (CVertex, CVertex)

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
sameAmode (CVal (SpRel r1) _) (CVal (SpRel r2) _) 	     = r1 _EQ_ r2
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
	  = getUniqsFlt (length dests)	`thenFlt` \ uniqs ->
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

--    (COpStmt _ _ _ _ _) `should_follow` (CCallProfCtrMacro _ _) = False
--    (CCallProfCtrMacro _ _) `should_follow` (COpStmt _ _ _ _ _) = False


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

regConflictsWithRR (VanillaReg k ILIT(1)) (NodeRel _)	= True

regConflictsWithRR Sp	(SpRel _)	= True
regConflictsWithRR Hp	(HpRel _)	= True
regConflictsWithRR _	_		= False

rrConflictsWithRR :: Int -> Int			-- Sizes of two things
		  -> RegRelative -> RegRelative -- The two amodes
		  -> Bool

rrConflictsWithRR (I# s1) (I# s2) rr1 rr2 = rr rr1 rr2
  where
    rr (SpRel o1)    (SpRel o2)
	| s1 _EQ_ ILIT(0) || s2 _EQ_ ILIT(0) = False -- No conflict if either is size zero
	| s1 _EQ_ ILIT(1)  && s2 _EQ_ ILIT(1) = o1 _EQ_ o2
	| otherwise	     = (o1 _ADD_ s1) _GE_ o2  &&
			       (o2 _ADD_ s2) _GE_ o1

    rr (NodeRel o1)	 (NodeRel o2)
	| s1 _EQ_ ILIT(0) || s2 _EQ_ ILIT(0) = False -- No conflict if either is size zero
	| s1 _EQ_ ILIT(1) && s2 _EQ_ ILIT(1) = o1 _EQ_ o2
	| otherwise	     = True		-- Give up

    rr (HpRel _)	 (HpRel _)    = True	-- Give up (ToDo)

    rr other1	         other2	      = False
\end{code}
