%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[AbsCFuns]{Help functions for Abstract~C datatype}

\begin{code}
#include "HsVersions.h"

module AbsCFuns (
	nonemptyAbsC,
	mkAbstractCs, mkAbsCStmts,
	mkAlgAltsCSwitch,
	kindFromMagicId,
	getAmodeKind, amodeCanSurviveGC,
	mixedTypeLocn, mixedPtrLocn,
	flattenAbsC,
--UNUSED:	getDestinationRegs,
        mkAbsCStmtList,

	-- printing/forcing stuff comes from PprAbsC

	-- and for interface self-sufficiency...
	AbstractC, CAddrMode, PrimKind, SplitUniqSupply
    ) where

import AbsCSyn

import AbsPrel		( PrimOp(..)
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType	( kindFromType, splitTyArgs, TauType(..),
			  TyVar, TyCon, Arity(..), Class, UniType
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon COMMA cmpClass)
			  IF_ATTACK_PRAGMAS(COMMA cmpTyVar)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)

#ifndef DPH
import CLabelInfo	( CLabel, mkReturnPtLabel, mkVecTblLabel )
#else
import CLabelInfo	( CLabel, mkReturnPtLabel, 
		          isNestableBlockLabel, isSlowFastLabelPair  )
#endif {- Data Parallel Haskell -}

import BasicLit		( kindOfBasicLit )
import Digraph		( stronglyConnComp )
import Id		( fIRST_TAG, ConTag(..), DataCon(..), Id )
import Maybes		( Maybe(..) )
import PrimKind		( getKindSize, retKindSize, PrimKind(..) )
import SplitUniq
import StgSyn		( StgAtom )
import Unique		-- UniqueSupply primitives used in flattening monad
import Util

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
--UNUSED:nonemptyAbsC (CComment _)	= Nothing
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
mkAbsCStmts = AbsCStmts

{- Discarded SLPJ June 95; it calls nonemptyAbsC too much!
  = BIND (case (nonemptyAbsC abc2) of
	    Nothing -> AbsCNop
	    Just d2 -> d2)	_TO_ abc2b ->

    case (nonemptyAbsC abc1) of {
      Nothing -> abc2b;
      Just d1 -> AbsCStmts d1 abc2b
    } BEND
-}
{-
  = case (nonemptyAbsC abc1) of
      Nothing -> abc2
      Just d1 -> AbsCStmts d1 abc2
-}
{- old2:
  = case (nonemptyAbsC abc1) of
      Nothing -> case (nonemptyAbsC abc2) of
		   Nothing -> AbsCNop
		   Just d2 -> d2
      Just d1 -> AbsCStmts d1 abc2
-}
{- old:
    if abc1_empty then
	if abc2_empty
	then AbsCNop
	else abc2
    else if {- abc1 not empty but -} abc2_empty then
	abc1
    else {- neither empty -}
	AbsCStmts abc1 abc2
  where
    abc1_empty = noAbsCcode abc1
    abc2_empty = noAbsCcode abc2
-}
\end{code}

Get the sho' 'nuff statements out of an @AbstractC@.
\begin{code}
{-
mkAbsCStmtList :: AbstractC -> [AbstractC]

mkAbsCStmtList  AbsCNop		 = []
--UNUSED:mkAbsCStmtList (CComment _)	 = []
mkAbsCStmtList (AbsCStmts s1 s2) = mkAbsCStmtList s1 ++ mkAbsCStmtList s2
mkAbsCStmtList s@(CSimultaneous c) = if null (mkAbsCStmtList c)
				     then []
				     else [s]
mkAbsCStmtList other		 = [other]
-}

mkAbsCStmtList :: AbstractC -> [AbstractC]
mkAbsCStmtList  absC = mkAbsCStmtList' absC []
  
-- Optimised a la foldr/build!

mkAbsCStmtList'  AbsCNop          r = r
--UNUSED:mkAbsCStmtList' (CComment _)      r = r
mkAbsCStmtList' (AbsCStmts s1 s2) r = 
      mkAbsCStmtList' s1 (mkAbsCStmtList' s2 r)
mkAbsCStmtList' s@(CSimultaneous c) r = 
      if null (mkAbsCStmtList c) then r else s : r
mkAbsCStmtList' other             r = other : r

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

   -- We also need to convert to BasicLits to keep the CSwitch happy
   adjust tagged_alts
     = [ (MachInt (toInteger (tag - fIRST_TAG)) False{-unsigned-}, abs_c)
       | (tag, abs_c) <- tagged_alts ]
\end{code}

%************************************************************************
%*									*
\subsubsection[AbsCFuns-kinds-from-MagicIds]{Kinds from MagicIds}
%*									*
%************************************************************************

\begin{code}
kindFromMagicId BaseReg		    = PtrKind
kindFromMagicId StkOReg		    = PtrKind
kindFromMagicId (VanillaReg kind _) = kind
kindFromMagicId (FloatReg _)	    = FloatKind
kindFromMagicId (DoubleReg _)	    = DoubleKind
kindFromMagicId TagReg		    = IntKind
kindFromMagicId RetReg		    = RetKind
kindFromMagicId SpA		    = PtrKind
kindFromMagicId SuA		    = PtrKind
kindFromMagicId SpB		    = PtrKind
kindFromMagicId SuB		    = PtrKind
kindFromMagicId Hp		    = PtrKind
kindFromMagicId HpLim		    = PtrKind
kindFromMagicId LivenessReg	    = IntKind
--kindFromMagicId ActivityReg	    = IntKind -- UNUSED
kindFromMagicId StdUpdRetVecReg	    = PtrKind
kindFromMagicId StkStubReg	    = PtrKind
kindFromMagicId CurCostCentre	    = CostCentreKind
kindFromMagicId VoidReg		    = VoidKind
#ifdef DPH
kindFromMagicId (DataReg _ n)	    = kind
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[AbsCFuns-amode-kinds]{Finding @PrimitiveKinds@ of amodes}
%*									*
%************************************************************************

See also the return conventions for unboxed things; currently living
in @CgCon@ (next to the constructor return conventions).

ToDo: tiny tweaking may be in order
\begin{code}
getAmodeKind :: CAddrMode -> PrimKind

getAmodeKind (CVal _ kind)	    	    = kind
getAmodeKind (CAddr _)		    	    = PtrKind
getAmodeKind (CReg magic_id)	    	    = kindFromMagicId magic_id
getAmodeKind (CTemp uniq kind)	    	    = kind
getAmodeKind (CLbl label kind)	    	    = kind
getAmodeKind (CUnVecLbl _ _)	    	    = PtrKind
getAmodeKind (CCharLike _)	    	    = PtrKind
getAmodeKind (CIntLike _)	    	    = PtrKind
getAmodeKind (CString _)	    	    = PtrKind
getAmodeKind (CLit lit)		    	    = kindOfBasicLit lit
getAmodeKind (CLitLit _ kind)	    	    = kind
getAmodeKind (COffset _)	    	    = IntKind
getAmodeKind (CCode abs_C)		    = CodePtrKind
getAmodeKind (CLabelledCode label abs_C)    = CodePtrKind
getAmodeKind (CJoinPoint _ _)	    	    = panic "getAmodeKind:CJoinPoint"
getAmodeKind (CTableEntry _ _ kind)    	    = kind
getAmodeKind (CMacroExpr kind _ _)    	    = kind
getAmodeKind (CCostCentre _ _)		    = panic "getAmodeKind:CCostCentre"
\end{code}

@amodeCanSurviveGC@ tells, well, whether or not the amode is invariant
across a garbage collection.  Used only for PrimOp arguments (not that
it matters).

\begin{code}
amodeCanSurviveGC :: CAddrMode -> Bool

amodeCanSurviveGC (CTableEntry base offset _)
  = amodeCanSurviveGC base && amodeCanSurviveGC offset
    -- "Fixed table, so it's OK" (JSM); code is slightly paranoid

amodeCanSurviveGC (CLbl _ _)		= True
amodeCanSurviveGC (CUnVecLbl _ _)	= True
amodeCanSurviveGC (CCharLike arg)	= amodeCanSurviveGC arg
amodeCanSurviveGC (CIntLike arg)	= amodeCanSurviveGC arg
amodeCanSurviveGC (CString _)		= True
amodeCanSurviveGC (CLit _)		= True
amodeCanSurviveGC (CLitLit _ _)		= True
amodeCanSurviveGC (COffset _)		= True
amodeCanSurviveGC (CMacroExpr _ _ args)	= all amodeCanSurviveGC args

amodeCanSurviveGC _ = False
    -- there are some amodes that "cannot occur" as args
    -- to a PrimOp, but it is safe to return False (rather than panic)
\end{code}

@mixedTypeLocn@ tells whether an amode identifies an ``StgWord''
location; that is, one which can contain values of various types.

\begin{code}
mixedTypeLocn :: CAddrMode -> Bool

mixedTypeLocn (CVal (NodeRel _)   _)	= True
mixedTypeLocn (CVal (SpBRel _ _)  _)	= True
mixedTypeLocn (CVal (HpRel _ _)	  _)	= True
mixedTypeLocn other			= False	-- All the rest
\end{code}

@mixedPtrLocn@ tells whether an amode identifies a
location which can contain values of various pointer types.

\begin{code}
mixedPtrLocn :: CAddrMode -> Bool

mixedPtrLocn (CVal (SpARel _ _)  _)	= True
mixedPtrLocn other			= False	-- All the rest
\end{code}

%************************************************************************
%*									*
\subsection[AbsCFuns-flattening]{Flatten Abstract~C}
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
@CStaticClosure@, @CClosureUpdInfo@, @CRetUnVector@, @CFlatRetVector@, or
@CCodeBlock@.  The latter turns into a C function, and is never
actually produced by the code generator.  Rather it always starts life
as a @CLabelledCode@ addressing mode; when such an addr mode is
flattened, the ``tops'' stuff is a @CCodeBlock@.

\begin{code}
flattenAbsC :: SplitUniqSupply -> AbstractC -> AbstractC

flattenAbsC us abs_C
  = case (initFlt us (flatAbsC abs_C)) of { (here, tops) ->
    here `mkAbsCStmts` tops }
\end{code}

%************************************************************************
%*									*
\subsubsection{Flattening monadery}
%*									*
%************************************************************************

The flattener is monadised.  It's just a @UniqueSupply@, along with a
``come-back-to-here'' label to pin on heap and stack checks.

\begin{code}
type FlatM result
     = CLabel
    -> SplitUniqSupply
    -> result

initFlt :: SplitUniqSupply -> FlatM a -> a

initFlt init_us m = m (panic "initFlt:CLabel") init_us

#ifdef __GLASGOW_HASKELL__
{-# INLINE thenFlt #-}
{-# INLINE returnFlt #-}
#endif

thenFlt :: FlatM a -> (a -> FlatM b) -> FlatM b

thenFlt expr cont label us
  = case (splitUniqSupply us)   of { (s1, s2) ->
    case (expr label s1)    	of { result ->
    cont result label s2 }}

returnFlt :: a -> FlatM a
returnFlt result label us = result

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
getUniqFlt label us = getSUnique us

getUniqsFlt :: Int -> FlatM [Unique]
getUniqsFlt i label us = getSUniques i us

setLabelFlt :: CLabel -> FlatM a -> FlatM a
setLabelFlt new_label cont label us = cont new_label us

getLabelFlt :: FlatM CLabel
getLabelFlt label us = label
\end{code}

%************************************************************************
%*									*
\subsubsection{Flattening the top level}
%*									*
%************************************************************************

\begin{code}
flatAbsC :: AbstractC
	 -> FlatM (AbstractC,		-- Stuff to put inline		[Both are fully
		   AbstractC)		-- Stuff to put at top level	 flattened]

flatAbsC AbsCNop = returnFlt (AbsCNop, AbsCNop)

flatAbsC (AbsCStmts s1 s2)
  = flatAbsC s1	`thenFlt` \ (inline_s1, top_s1) ->
    flatAbsC s2	`thenFlt` \ (inline_s2, top_s2) ->
    returnFlt (mkAbsCStmts inline_s1 inline_s2,
	       mkAbsCStmts top_s1    top_s2)

flatAbsC (CClosureInfoAndCode cl_info slow maybe_fast upd descr liveness)
  = flatAbsC slow		`thenFlt` \ (slow_heres, slow_tops) ->
    flat_maybe maybe_fast	`thenFlt` \ (fast_heres, fast_tops) ->
    flatAmode upd               `thenFlt` \ (upd_lbl,    upd_tops) ->
    returnFlt (AbsCNop, mkAbstractCs [slow_tops, fast_tops, upd_tops,
       CClosureInfoAndCode cl_info slow_heres fast_heres upd_lbl descr liveness]
    )
  where
    flat_maybe :: Maybe AbstractC -> FlatM (Maybe AbstractC, AbstractC)
    flat_maybe Nothing      = returnFlt (Nothing, AbsCNop)
    flat_maybe (Just abs_c) = flatAbsC abs_c `thenFlt` \ (heres, tops) ->
			      returnFlt (Just heres, tops)

flatAbsC (CCodeBlock label abs_C)
  = flatAbsC abs_C	    `thenFlt` \ (absC_heres, absC_tops) ->
    returnFlt (AbsCNop, absC_tops `mkAbsCStmts` CCodeBlock label absC_heres)

flatAbsC (CClosureUpdInfo info) = flatAbsC info

flatAbsC (CStaticClosure closure_lbl closure_info cost_centre amodes)
  = flatAmodes (cost_centre:amodes)	`thenFlt` \ (new_cc:new_amodes, tops) ->
    returnFlt (AbsCNop, tops `mkAbsCStmts` 
		        CStaticClosure closure_lbl closure_info new_cc new_amodes)

flatAbsC (CRetVector tbl_label stuff deflt)
  = do_deflt deflt				`thenFlt` \ (deflt_amode, deflt_tops) ->
    mapAndUnzipFlt (do_alt deflt_amode) stuff	`thenFlt` \ (alt_amodes, alt_tops) ->
    returnFlt (AbsCNop, mkAbstractCs [deflt_tops, 
				      mkAbstractCs alt_tops, 
				      CFlatRetVector tbl_label alt_amodes])

  where
    do_deflt deflt = case nonemptyAbsC deflt of
			Nothing     -> returnFlt (bogus_default_label, AbsCNop)
			Just deflt' -> flatAmode (CCode deflt)	-- Deals correctly with the 
								-- CJump (CLabelledCode ...) case

    do_alt deflt_amode Nothing    = returnFlt (deflt_amode, AbsCNop)
    do_alt deflt_amode (Just alt) = flatAmode alt

    bogus_default_label = panic "flatAbsC: CRetVector: default needed and not available"


flatAbsC (CRetUnVector label amode)
  = flatAmode amode	`thenFlt` \ (new_amode, tops) ->
    returnFlt (AbsCNop, tops `mkAbsCStmts` CRetUnVector label new_amode)

flatAbsC (CFlatRetVector label amodes)
  = flatAmodes amodes	`thenFlt` \ (new_amodes, tops) ->
    returnFlt (AbsCNop, tops `mkAbsCStmts` CFlatRetVector label new_amodes)

flatAbsC cc@(CCostCentreDecl _ _)  -- at top, already flat
  = returnFlt (AbsCNop, cc)

-- now the real stmts:

flatAbsC (CAssign dest source)
  = flatAmode dest    `thenFlt` \ (dest_amode, dest_tops) ->
    flatAmode source  `thenFlt` \ (src_amode,  src_tops)  ->
    returnFlt ( CAssign dest_amode src_amode, mkAbsCStmts dest_tops src_tops )

-- special case: jump to some anonymous code
flatAbsC (CJump (CCode abs_C)) = flatAbsC abs_C

flatAbsC (CJump target)
  = flatAmode target `thenFlt` \ (targ_amode, targ_tops) ->
    returnFlt ( CJump targ_amode, targ_tops )

flatAbsC (CFallThrough target)
  = flatAmode target `thenFlt` \ (targ_amode, targ_tops) ->
    returnFlt ( CFallThrough targ_amode, targ_tops )

flatAbsC (CReturn target return_info)
  = flatAmode target `thenFlt` \ (targ_amode, targ_tops) ->
    returnFlt ( CReturn targ_amode return_info, targ_tops )

flatAbsC (CSwitch discrim alts deflt)
  = flatAmode discrim		 `thenFlt` \ (discrim_amode, discrim_tops) ->
    mapAndUnzipFlt flat_alt alts `thenFlt` \ (flat_alts, flat_alts_tops) ->
    flatAbsC deflt		 `thenFlt` \ (flat_def_alt, def_tops) ->
    returnFlt (
      CSwitch discrim_amode flat_alts flat_def_alt,
      mkAbstractCs (discrim_tops : def_tops : flat_alts_tops)
    )
  where
    flat_alt (tag, absC)
      = flatAbsC absC	`thenFlt` \ (alt_heres, alt_tops) ->
	returnFlt ( (tag, alt_heres), alt_tops )

flatAbsC stmt@(CInitHdr a b cc u)
  = flatAmode cc	`thenFlt` \ (new_cc, tops) ->
    returnFlt (CInitHdr a b new_cc u, tops)

flatAbsC stmt@(COpStmt results op args liveness_mask vol_regs)
  = flatAmodes results		`thenFlt` \ (results_here, tops1) ->
    flatAmodes args		`thenFlt` \ (args_here,    tops2) ->
    returnFlt (COpStmt results_here op args_here liveness_mask vol_regs,
		mkAbsCStmts tops1 tops2)

flatAbsC stmt@(CSimultaneous abs_c)
  = flatAbsC abs_c		`thenFlt` \ (stmts_here, tops) ->
    doSimultaneously stmts_here	`thenFlt` \ new_stmts_here ->
    returnFlt (new_stmts_here, tops)

flatAbsC stmt@(CMacroStmt macro amodes)
  = flatAmodes amodes		`thenFlt` \ (amodes_here, tops) ->
    returnFlt (CMacroStmt macro amodes_here, tops)

flatAbsC stmt@(CCallProfCtrMacro str amodes)
  = flatAmodes amodes		`thenFlt` \ (amodes_here, tops) ->
    returnFlt (CCallProfCtrMacro str amodes_here, tops)

flatAbsC stmt@(CCallProfCCMacro str amodes)
  = flatAmodes amodes		`thenFlt` \ (amodes_here, tops) ->
    returnFlt (CCallProfCCMacro str amodes_here, tops)

--UNUSED:flatAbsC comment_stmt@(CComment comment) = returnFlt (AbsCNop, AbsCNop)

flatAbsC stmt@(CSplitMarker) = returnFlt (AbsCNop, stmt)

#ifdef DPH
    -- Hack since 0.16 because Direct entry code blocks can be nested
    -- within other Direct entry blocks...
    flatAbsC (CNativeInfoTableAndCode cinfo descr 
                     (CCodeBlock slow_label 
		        (AbsCStmts slow_abs_c
		           (CCodeBlock fast_label fast_abs_c))))
      | isSlowFastLabelPair slow_label fast_label
      = flatAbsC slow_abs_c	    `thenFlt` \ (slow_here, slow_top) ->
        flatAbsC fast_abs_c	    `thenFlt` \ (fast_here, fast_top) ->
        returnFlt (CNativeInfoTableAndCode cinfo descr 	
		     (CCodeBlock slow_label
		       (AbsCStmts slow_here
		          (CCodeBlock fast_label fast_here))), 
		     mkAbsCStmts slow_top fast_top)

    flatAbsC (CNativeInfoTableAndCode cinfo descr abs_C)
      = flatAbsC abs_C	    `thenFlt` \ (heres, tops) ->
        returnFlt (CNativeInfoTableAndCode cinfo descr heres, tops)
#endif {- Data Parallel Haskell -}

--flatAbsC stmt = panic ("flatAbsC: funny statement " ++ printRealC (\x->False) stmt)
\end{code}

%************************************************************************
%*									*
\subsection[flat-amodes]{Flattening addressing modes}
%*									*
%************************************************************************

\begin{code}
flatAmode :: CAddrMode -> FlatM (CAddrMode, AbstractC)

-- easy ones first
flatAmode amode@(CVal _ _)	= returnFlt (amode, AbsCNop)

flatAmode amode@(CAddr _)	= returnFlt (amode, AbsCNop)
flatAmode amode@(CReg _)	= returnFlt (amode, AbsCNop)
flatAmode amode@(CTemp _ _)	= returnFlt (amode, AbsCNop)
flatAmode amode@(CLbl _ _)	= returnFlt (amode, AbsCNop)
flatAmode amode@(CUnVecLbl _ _)	= returnFlt (amode, AbsCNop)
flatAmode amode@(CString _)	= returnFlt (amode, AbsCNop)
flatAmode amode@(CLit _)	= returnFlt (amode, AbsCNop)
flatAmode amode@(CLitLit _ _)	= returnFlt (amode, AbsCNop)
flatAmode amode@(COffset _)	= returnFlt (amode, AbsCNop)

-- CIntLike must be a literal -- no flattening
flatAmode amode@(CIntLike int)  = returnFlt(amode, AbsCNop)

-- CCharLike may be arbitrary value -- have to flatten
flatAmode amode@(CCharLike char)
  = flatAmode char	`thenFlt` \ (flat_char, tops) ->
    returnFlt(CCharLike flat_char, tops)

flatAmode (CJoinPoint _ _) = panic "flatAmode:CJoinPoint"

flatAmode (CLabelledCode label abs_C)
  -- Push the code (with this label) to the top level
  = flatAbsC abs_C	`thenFlt` \ (body_code, tops) ->
    returnFlt (CLbl label CodePtrKind,	
	       tops `mkAbsCStmts` CCodeBlock label body_code)

flatAmode (CCode abs_C)
  = case mkAbsCStmtList abs_C of
      [CJump amode] -> flatAmode amode	-- Elide redundant labels
      _ ->
	-- de-anonymous-ise the code and push it (labelled) to the top level
        getUniqFlt 		`thenFlt` \ new_uniq ->
	BIND (mkReturnPtLabel new_uniq)    _TO_ return_pt_label ->
	flatAbsC abs_C	`thenFlt` \ (body_code, tops) ->
	returnFlt (
	    CLbl return_pt_label CodePtrKind,
	    tops `mkAbsCStmts` CCodeBlock return_pt_label body_code
	    -- DO NOT TOUCH the stuff sent to the top...
	)
	BEND
  
flatAmode (CTableEntry base index kind)
  = flatAmode base    	`thenFlt` \ (base_amode, base_tops) ->
    flatAmode index 	`thenFlt` \ (ix_amode,  ix_tops)  ->
    returnFlt ( CTableEntry base_amode ix_amode kind, mkAbsCStmts base_tops ix_tops )

flatAmode (CMacroExpr pk macro amodes)
  = flatAmodes amodes    	`thenFlt` \ (amodes_here, tops) ->
    returnFlt ( CMacroExpr pk macro amodes_here, tops )

flatAmode amode@(CCostCentre _ _) = returnFlt (amode, AbsCNop)
\end{code}

And a convenient way to do a whole bunch of 'em.
\begin{code}
flatAmodes :: [CAddrMode] -> FlatM ([CAddrMode], AbstractC)

flatAmodes [] = returnFlt ([], AbsCNop)

flatAmodes amodes
  = mapAndUnzipFlt flatAmode amodes `thenFlt` \ (amodes_here, tops) ->
    returnFlt (amodes_here, mkAbstractCs tops)
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

ADR Comment

Wow - fancy stuff.  But are we ever going to do anything other than
assignments in parallel?  If not, wouldn't it be simpler to generate
the following:

 x1, x2, x3 = e1, e2, e3

    |
    |
    V
 { int t1 = e1;
   int t2 = e2;
   int t3 = e3;
   x1 = t1;
   x2 = t2;
   x3 = t3;
 }

and leave it to the C compiler to figure out whether it needs al
those variables.

(Likewise, why not let the C compiler delete silly code like

    x = x

for us?)

tnemmoC RDA

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
sameAmode (CVal (SpARel r1 v1) _) (CVal (SpARel r2 v2) _) = r1 == r2 && v1 == v2
sameAmode (CVal (SpBRel r1 v1) _) (CVal (SpBRel r2 v2) _) = r1 == r2 && v1 == v2
sameAmode other1		     other2		     = False

doSimultaneously1 :: [CVertex] -> FlatM AbstractC
doSimultaneously1 vertices
  = let
	edges :: [CEdge]
	edges = concat (map edges_from vertices)

	edges_from :: CVertex -> [CEdge]
	edges_from v1 = [(v1,v2) | v2 <- vertices, v1 `should_follow` v2]

	should_follow :: CVertex -> CVertex -> Bool
	(n1, CAssign dest1 _) `should_follow` (n2, CAssign _ src2)
	  = dest1 `conflictsWith` src2
	(n1, COpStmt dests1 _ _ _ _) `should_follow` (n2, CAssign _ src2)
	  = or [dest1 `conflictsWith` src2 | dest1 <- dests1]
	(n1, CAssign dest1 _)`should_follow` (n2, COpStmt _ _ srcs2 _ _)
	  = or [dest1 `conflictsWith` src2 | src2 <- srcs2]
	(n1, COpStmt dests1 _ _ _ _) `should_follow` (n2, COpStmt _ _ srcs2 _ _)
	  = or [dest1 `conflictsWith` src2 | dest1 <- dests1, src2 <- srcs2]

--	(_, COpStmt _ _ _ _ _) `should_follow` (_, CCallProfCtrMacro _ _) = False
--	(_, CCallProfCtrMacro _ _) `should_follow` (_, COpStmt _ _ _ _ _) = False

	eq_vertex :: CVertex -> CVertex -> Bool
	(n1, _) `eq_vertex` (n2, _) = n1 == n2

	components = stronglyConnComp eq_vertex edges vertices

	-- do_components deal with one strongly-connected component
	do_component :: [CVertex] -> FlatM AbstractC

	-- A singleton?  Then just do it.
	do_component [(n,abs_c)] = returnFlt abs_c

	-- Two or more?  Then go via temporaries.
	do_component ((n,first_stmt):rest)
	  = doSimultaneously1 rest	`thenFlt` \ abs_cs ->
	    go_via_temps first_stmt	`thenFlt` \ (to_temps, from_temps) ->
	    returnFlt (mkAbstractCs [to_temps, abs_cs, from_temps])

	go_via_temps (CAssign dest src)
	  = getUniqFlt 			`thenFlt` \ uniq ->
	    let  the_temp = CTemp uniq (getAmodeKind dest)  in
	    returnFlt (CAssign the_temp src, CAssign dest the_temp)

	go_via_temps (COpStmt dests op srcs liveness_mask vol_regs)
	  = getUniqsFlt (length dests)	`thenFlt` \ uniqs ->
	    let the_temps = zipWith (\ u d -> CTemp u (getAmodeKind d)) uniqs dests
	    in
	    returnFlt (COpStmt the_temps op srcs liveness_mask vol_regs,
		       mkAbstractCs (zipWith CAssign dests the_temps))
    in
    mapFlt do_component components `thenFlt` \ abs_cs ->
    returnFlt (mkAbstractCs abs_cs)
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
  = rrConflictsWithRR (getKindSize k1) (getKindSize k2) reg_rel1 reg_rel2

other1		  `conflictsWith` other2		= False
-- CAddr and literals are impossible on the LHS of an assignment

regConflictsWithRR :: MagicId -> RegRelative -> Bool

regConflictsWithRR (VanillaReg k ILIT(1)) (NodeRel _)	= True

regConflictsWithRR SpA	(SpARel _ _)	= True
regConflictsWithRR SpB	(SpBRel _ _)	= True
regConflictsWithRR Hp	(HpRel _ _)	= True
regConflictsWithRR _	_		= False

rrConflictsWithRR :: Int -> Int			-- Sizes of two things
		  -> RegRelative -> RegRelative -- The two amodes
		  -> Bool

rrConflictsWithRR s1 s2 rr1 rr2 = rr rr1 rr2
  where
    rr (SpARel p1 o1)    (SpARel p2 o2) 
	| s1 == 0 || s2 == 0 = False	-- No conflict if either is size zero
	| s1 == 1 && s2 == 1 = b1 == b2
	| otherwise	     = (b1+s1) >= b2  &&
			       (b2+s2) >= b1
	where
	  b1 = p1-o1
	  b2 = p2-o2

    rr (SpBRel p1 o1)    (SpBRel p2 o2)
	| s1 == 0 || s2 == 0 = False	-- No conflict if either is size zero
	| s1 == 1 && s2 == 1 = b1 == b2
	| otherwise	     = (b1+s1) >= b2  &&
			       (b2+s2) >= b1
	where
	  b1 = p1-o1
	  b2 = p2-o2

    rr (NodeRel o1)	 (NodeRel o2)
	| s1 == 0 || s2 == 0 = False	-- No conflict if either is size zero
	| s1 == 1 && s2 == 1 = o1 `possiblyEqualHeapOffset` o2
	| otherwise	     = True		-- Give up

    rr (HpRel _ _)	 (HpRel _ _)    = True	-- Give up

    rr other1	         other2	        = False
\end{code}

%************************************************************************
%*									*
\subsection[gaze-into-simultaneous]{Registers live in a @CSimultaneous@?}
%*									*
%************************************************************************

Hidden in a blob of ``simultaneous assignments'' is the info of how
many pointer (``followable'') registers are live (i.e., assigned
into).  What we do here is merely fish out the destination registers.

\begin{code}
{- UNUSED:
getDestinationRegs :: AbstractC -> [MagicId]

getDestinationRegs abs_c
  = foldr gather [{-acc-}] (en_list abs_c)
  where
    gather :: AbstractC -> [MagicId] -> [MagicId]

	-- only CAssigns and COpStmts now possible...

    gather (CAssign (CReg magic_id) _) acc | magic_id `not_elem` acc
      = magic_id : acc
      where
	not_elem = isn'tIn "getDestinationRegs"

    gather (COpStmt dests _ _ _ _) acc
      = foldr gather2 acc dests
      where
    	gather2 (CReg magic_id) acc | magic_id `not_elem` acc = magic_id : acc
	gather2 _   	    	acc 			      = acc

	not_elem = isn'tIn "getDestinationRegs2"

    gather _ acc = acc
-}
\end{code}
