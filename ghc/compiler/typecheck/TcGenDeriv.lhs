%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcGenDeriv]{Generating derived instance declarations}

This module is nominally ``subordinate'' to @TcDeriv@, which is the
``official'' interface to deriving-related things.

This is where we do all the grimy bindings' generation.

\begin{code}
module TcGenDeriv (
	gen_Bounded_binds,
	gen_Enum_binds,
	gen_Eq_binds,
	gen_Ix_binds,
	gen_Ord_binds,
	gen_Read_binds,
	gen_Show_binds,
	gen_Data_binds,
	gen_Typeable_binds,
	gen_tag_n_con_monobind,

	con2tag_RDR, tag2con_RDR, maxtag_RDR,

	TagThingWanted(..)
    ) where

#include "HsVersions.h"

import HsSyn		( Pat(..), HsConDetails(..), HsExpr(..), MonoBinds(..),
			  Match(..), GRHSs(..), Stmt(..), HsLit(..),
			  HsBinds(..), HsType(..), HsStmtContext(..),
			  unguardedRHS, mkSimpleMatch, mkMonoBind, andMonoBindList, placeHolderType
			)
import RdrName		( RdrName, mkUnqual, mkRdrUnqual, nameRdrName, getRdrName )
import RdrHsSyn		( mkHsOpApp, RdrNameMonoBinds, RdrNameHsExpr, RdrNamePat, mkHsDo )
import BasicTypes	( RecFlag(..), Fixity(..), FixityDirection(..)
			, maxPrecedence
			, Boxity(..)
			)
import FieldLabel       ( fieldLabelName )
import DataCon		( isNullaryDataCon, dataConTag,
			  dataConOrigArgTys, dataConSourceArity, fIRST_TAG,
			  DataCon, 
			  dataConFieldLabels )
import Name		( getOccString, getOccName, getSrcLoc, occNameString, 
			  occNameUserString, varName,
			  Name, NamedThing(..), 
			  isDataSymOcc, isSymOcc
			)

import HscTypes		( FixityEnv, lookupFixity )
import PrelNames	-- Lots of Names
import PrimOp		-- Lots of Names
import SrcLoc		( generatedSrcLoc, SrcLoc )
import TyCon		( TyCon, isNewTyCon, tyConDataCons, isEnumerationTyCon,
			  maybeTyConSingleCon, tyConFamilySize, tyConTyVars
			)
import TcType		( isUnLiftedType, tcEqType, Type )
import TysPrim		( charPrimTy, intPrimTy, wordPrimTy, addrPrimTy, floatPrimTy, doublePrimTy )
import TysWiredIn	( charDataCon, intDataCon, floatDataCon, doubleDataCon, wordDataCon )
import Util		( zipWithEqual, isSingleton,
			  zipWith3Equal, nOfThem, zipEqual )
import Panic		( panic, assertPanic )
import Char		( ord, isAlpha )
import Constants
import List		( partition, intersperse )
import Outputable
import FastString
import OccName
\end{code}

%************************************************************************
%*									*
\subsection{Generating code, by derivable class}
%*									*
%************************************************************************

%************************************************************************
%*									*
\subsubsection{Generating @Eq@ instance declarations}
%*									*
%************************************************************************

Here are the heuristics for the code we generate for @Eq@:
\begin{itemize}
\item
  Let's assume we have a data type with some (possibly zero) nullary
  data constructors and some ordinary, non-nullary ones (the rest,
  also possibly zero of them).  Here's an example, with both \tr{N}ullary
  and \tr{O}rdinary data cons.
\begin{verbatim}
data Foo ... = N1 | N2 ... | Nn | O1 a b | O2 Int | O3 Double b b | ...
\end{verbatim}

\item
  For the ordinary constructors (if any), we emit clauses to do The
  Usual Thing, e.g.,:

\begin{verbatim}
(==) (O1 a1 b1)	   (O1 a2 b2)    = a1 == a2 && b1 == b2
(==) (O2 a1)	   (O2 a2)	 = a1 == a2
(==) (O3 a1 b1 c1) (O3 a2 b2 c2) = a1 == a2 && b1 == b2 && c1 == c2
\end{verbatim}

  Note: if we're comparing unlifted things, e.g., if \tr{a1} and
  \tr{a2} are \tr{Float#}s, then we have to generate
\begin{verbatim}
case (a1 `eqFloat#` a2) of
  r -> r
\end{verbatim}
  for that particular test.

\item
  If there are any nullary constructors, we emit a catch-all clause of
  the form:

\begin{verbatim}
(==) a b  = case (con2tag_Foo a) of { a# ->
	    case (con2tag_Foo b) of { b# ->
	    case (a# ==# b#)	 of {
	      r -> r
	    }}}
\end{verbatim}

  If there aren't any nullary constructors, we emit a simpler
  catch-all:
\begin{verbatim}
(==) a b  = False
\end{verbatim}

\item
  For the @(/=)@ method, we normally just use the default method.

  If the type is an enumeration type, we could/may/should? generate
  special code that calls @con2tag_Foo@, much like for @(==)@ shown
  above.

\item
  We thought about doing this: If we're also deriving @Ord@ for this
  tycon, we generate:
\begin{verbatim}
instance ... Eq (Foo ...) where
  (==) a b  = case (compare a b) of { _LT -> False; _EQ -> True ; _GT -> False}
  (/=) a b  = case (compare a b) of { _LT -> True ; _EQ -> False; _GT -> True }
\begin{verbatim}
  However, that requires that \tr{Ord <whatever>} was put in the context
  for the instance decl, which it probably wasn't, so the decls
  produced don't get through the typechecker.
\end{itemize}


\begin{code}
gen_Eq_binds :: TyCon -> RdrNameMonoBinds

gen_Eq_binds tycon
  = let
	tycon_loc = getSrcLoc tycon
        (nullary_cons, nonnullary_cons)
           | isNewTyCon tycon = ([], tyConDataCons tycon)
           | otherwise	      = partition isNullaryDataCon (tyConDataCons tycon)

	rest
	  = if (null nullary_cons) then
		case maybeTyConSingleCon tycon of
		  Just _ -> []
		  Nothing -> -- if cons don't match, then False
		     [([wildPat, wildPat], false_Expr)]
	    else -- calc. and compare the tags
		 [([a_Pat, b_Pat],
		    untag_Expr tycon [(a_RDR,ah_RDR), (b_RDR,bh_RDR)]
		               (genOpApp (HsVar ah_RDR) eqInt_RDR (HsVar bh_RDR)))]
    in
    mk_FunMonoBind tycon_loc eq_RDR ((map pats_etc nonnullary_cons) ++ rest)
	    `AndMonoBinds`
    mk_easy_FunMonoBind tycon_loc ne_RDR [a_Pat, b_Pat] [] (
	HsApp (HsVar not_RDR) (HsPar (mkHsVarApps eq_RDR [a_RDR, b_RDR])))
  where
    ------------------------------------------------------------------
    pats_etc data_con
      = let
	    con1_pat = mkConPat data_con_RDR as_needed
	    con2_pat = mkConPat data_con_RDR bs_needed

	    data_con_RDR = getRdrName data_con
	    con_arity   = length tys_needed
	    as_needed   = take con_arity as_RDRs
	    bs_needed   = take con_arity bs_RDRs
	    tys_needed  = dataConOrigArgTys data_con
	in
	([con1_pat, con2_pat], nested_eq_expr tys_needed as_needed bs_needed)
      where
	nested_eq_expr []  [] [] = true_Expr
	nested_eq_expr tys as bs
	  = foldl1 and_Expr (zipWith3Equal "nested_eq" nested_eq tys as bs)
	  where
	    nested_eq ty a b = HsPar (eq_Expr tycon ty (HsVar a) (HsVar b))
\end{code}

%************************************************************************
%*									*
\subsubsection{Generating @Ord@ instance declarations}
%*									*
%************************************************************************

For a derived @Ord@, we concentrate our attentions on @compare@
\begin{verbatim}
compare :: a -> a -> Ordering
data Ordering = LT | EQ | GT deriving ()
\end{verbatim}

We will use the same example data type as above:
\begin{verbatim}
data Foo ... = N1 | N2 ... | Nn | O1 a b | O2 Int | O3 Double b b | ...
\end{verbatim}

\begin{itemize}
\item
  We do all the other @Ord@ methods with calls to @compare@:
\begin{verbatim}
instance ... (Ord <wurble> <wurble>) where
    a <  b  = case (compare a b) of { LT -> True;  EQ -> False; GT -> False }
    a <= b  = case (compare a b) of { LT -> True;  EQ -> True;  GT -> False }
    a >= b  = case (compare a b) of { LT -> False; EQ -> True;  GT -> True  }
    a >  b  = case (compare a b) of { LT -> False; EQ -> False; GT -> True  }

    max a b = case (compare a b) of { LT -> b; EQ -> a;  GT -> a }
    min a b = case (compare a b) of { LT -> a; EQ -> b;  GT -> b }

    -- compare to come...
\end{verbatim}

\item
  @compare@ always has two parts.  First, we use the compared
  data-constructors' tags to deal with the case of different
  constructors:
\begin{verbatim}
compare a b = case (con2tag_Foo a) of { a# ->
	      case (con2tag_Foo b) of { b# ->
	      case (a# ==# b#)     of {
	       True  -> cmp_eq a b
	       False -> case (a# <# b#) of
			 True  -> _LT
			 False -> _GT
	      }}}
  where
    cmp_eq = ... to come ...
\end{verbatim}

\item
  We are only left with the ``help'' function @cmp_eq@, to deal with
  comparing data constructors with the same tag.

  For the ordinary constructors (if any), we emit the sorta-obvious
  compare-style stuff; for our example:
\begin{verbatim}
cmp_eq (O1 a1 b1) (O1 a2 b2)
  = case (compare a1 a2) of { LT -> LT; EQ -> compare b1 b2; GT -> GT }

cmp_eq (O2 a1) (O2 a2)
  = compare a1 a2

cmp_eq (O3 a1 b1 c1) (O3 a2 b2 c2)
  = case (compare a1 a2) of {
      LT -> LT;
      GT -> GT;
      EQ -> case compare b1 b2 of {
	      LT -> LT;
	      GT -> GT;
	      EQ -> compare c1 c2
	    }
    }
\end{verbatim}

  Again, we must be careful about unlifted comparisons.  For example,
  if \tr{a1} and \tr{a2} were \tr{Int#}s in the 2nd example above, we'd need to
  generate:

\begin{verbatim}
cmp_eq lt eq gt (O2 a1) (O2 a2)
  = compareInt# a1 a2
  -- or maybe the unfolded equivalent
\end{verbatim}

\item
  For the remaining nullary constructors, we already know that the
  tags are equal so:
\begin{verbatim}
cmp_eq _ _ = EQ
\end{verbatim}
\end{itemize}

If there is only one constructor in the Data Type we don't need the WildCard Pattern. 
JJQC-30-Nov-1997

\begin{code}
gen_Ord_binds :: TyCon -> RdrNameMonoBinds

gen_Ord_binds tycon
  = compare 	-- `AndMonoBinds` compare	
		-- The default declaration in PrelBase handles this
  where
    tycon_loc = getSrcLoc tycon
    --------------------------------------------------------------------
    compare = mk_easy_FunMonoBind tycon_loc compare_RDR
				  [a_Pat, b_Pat] [cmp_eq] compare_rhs
    compare_rhs
	| single_con_type = cmp_eq_Expr a_Expr b_Expr
 	| otherwise
	= untag_Expr tycon [(a_RDR, ah_RDR), (b_RDR, bh_RDR)]
		  (cmp_tags_Expr eqInt_RDR ah_RDR bh_RDR
			(cmp_eq_Expr a_Expr b_Expr)	-- True case
			-- False case; they aren't equal
			-- So we need to do a less-than comparison on the tags
		  	(cmp_tags_Expr ltInt_RDR ah_RDR bh_RDR ltTag_Expr gtTag_Expr))

    tycon_data_cons = tyConDataCons tycon
    single_con_type = isSingleton tycon_data_cons
    (nullary_cons, nonnullary_cons)
       | isNewTyCon tycon = ([], tyConDataCons tycon)
       | otherwise	  = partition isNullaryDataCon tycon_data_cons

    cmp_eq = mk_FunMonoBind tycon_loc cmp_eq_RDR cmp_eq_match
    cmp_eq_match
      | isEnumerationTyCon tycon
			   -- We know the tags are equal, so if it's an enumeration TyCon,
			   -- then there is nothing left to do
			   -- Catch this specially to avoid warnings
			   -- about overlapping patterns from the desugarer,
			   -- and to avoid unnecessary pattern-matching
      = [([wildPat,wildPat], eqTag_Expr)]
      | otherwise
      = map pats_etc nonnullary_cons ++
	(if single_con_type then	-- Omit wildcards when there's just one 
	      []			-- constructor, to silence desugarer
	else
              [([wildPat, wildPat], default_rhs)])

      where
	pats_etc data_con
	  = ([con1_pat, con2_pat],
	     nested_compare_expr tys_needed as_needed bs_needed)
	  where
	    con1_pat = mkConPat data_con_RDR as_needed
	    con2_pat = mkConPat data_con_RDR bs_needed

	    data_con_RDR = getRdrName data_con
	    con_arity   = length tys_needed
	    as_needed   = take con_arity as_RDRs
	    bs_needed   = take con_arity bs_RDRs
	    tys_needed  = dataConOrigArgTys data_con

	    nested_compare_expr [ty] [a] [b]
	      = careful_compare_Case tycon ty eqTag_Expr (HsVar a) (HsVar b)

	    nested_compare_expr (ty:tys) (a:as) (b:bs)
	      = let eq_expr = nested_compare_expr tys as bs
		in  careful_compare_Case tycon ty eq_expr (HsVar a) (HsVar b)

	default_rhs | null nullary_cons = impossible_Expr	-- Keep desugarer from complaining about
								-- inexhaustive patterns
		    | otherwise		= eqTag_Expr		-- Some nullary constructors;
								-- Tags are equal, no args => return EQ
\end{code}

%************************************************************************
%*									*
\subsubsection{Generating @Enum@ instance declarations}
%*									*
%************************************************************************

@Enum@ can only be derived for enumeration types.  For a type
\begin{verbatim}
data Foo ... = N1 | N2 | ... | Nn
\end{verbatim}

we use both @con2tag_Foo@ and @tag2con_Foo@ functions, as well as a
@maxtag_Foo@ variable (all generated by @gen_tag_n_con_binds@).

\begin{verbatim}
instance ... Enum (Foo ...) where
    succ x   = toEnum (1 + fromEnum x)
    pred x   = toEnum (fromEnum x - 1)

    toEnum i = tag2con_Foo i

    enumFrom a = map tag2con_Foo [con2tag_Foo a .. maxtag_Foo]

    -- or, really...
    enumFrom a
      = case con2tag_Foo a of
	  a# -> map tag2con_Foo (enumFromTo (I# a#) maxtag_Foo)

   enumFromThen a b
     = map tag2con_Foo [con2tag_Foo a, con2tag_Foo b .. maxtag_Foo]

    -- or, really...
    enumFromThen a b
      = case con2tag_Foo a of { a# ->
	case con2tag_Foo b of { b# ->
	map tag2con_Foo (enumFromThenTo (I# a#) (I# b#) maxtag_Foo)
	}}
\end{verbatim}

For @enumFromTo@ and @enumFromThenTo@, we use the default methods.

\begin{code}
gen_Enum_binds :: TyCon -> RdrNameMonoBinds

gen_Enum_binds tycon
  = succ_enum		`AndMonoBinds`
    pred_enum		`AndMonoBinds`
    to_enum             `AndMonoBinds`
    enum_from		`AndMonoBinds`
    enum_from_then	`AndMonoBinds`
    from_enum
  where
    tycon_loc = getSrcLoc tycon
    occ_nm    = getOccString tycon

    succ_enum
      = mk_easy_FunMonoBind tycon_loc succ_RDR [a_Pat] [] $
	untag_Expr tycon [(a_RDR, ah_RDR)] $
	HsIf (mkHsApps eq_RDR [HsVar (maxtag_RDR tycon),
			       mkHsVarApps mkInt_RDR [ah_RDR]])
	     (illegal_Expr "succ" occ_nm "tried to take `succ' of last tag in enumeration")
	     (HsApp (HsVar (tag2con_RDR tycon))
		    (mkHsApps plus_RDR [mkHsVarApps mkInt_RDR [ah_RDR],
		 			mkHsIntLit 1]))
	     tycon_loc
		    
    pred_enum
      = mk_easy_FunMonoBind tycon_loc pred_RDR [a_Pat] [] $
	untag_Expr tycon [(a_RDR, ah_RDR)] $
	HsIf (mkHsApps eq_RDR [mkHsIntLit 0,
			       mkHsVarApps mkInt_RDR [ah_RDR]])
	     (illegal_Expr "pred" occ_nm "tried to take `pred' of first tag in enumeration")
	     (HsApp (HsVar (tag2con_RDR tycon))
			   (mkHsApps plus_RDR [mkHsVarApps mkInt_RDR [ah_RDR],
					       HsLit (HsInt (-1))]))
	     tycon_loc

    to_enum
      = mk_easy_FunMonoBind tycon_loc toEnum_RDR [a_Pat] [] $
	HsIf (mkHsApps and_RDR
		[mkHsApps ge_RDR [HsVar a_RDR, mkHsIntLit 0],
                 mkHsApps le_RDR [HsVar a_RDR, HsVar (maxtag_RDR tycon)]])
             (mkHsVarApps (tag2con_RDR tycon) [a_RDR])
	     (illegal_toEnum_tag occ_nm (maxtag_RDR tycon))
	     tycon_loc

    enum_from
      = mk_easy_FunMonoBind tycon_loc enumFrom_RDR [a_Pat] [] $
	  untag_Expr tycon [(a_RDR, ah_RDR)] $
	  mkHsApps map_RDR 
		[HsVar (tag2con_RDR tycon),
		 HsPar (enum_from_to_Expr
			    (mkHsVarApps mkInt_RDR [ah_RDR])
			    (HsVar (maxtag_RDR tycon)))]

    enum_from_then
      = mk_easy_FunMonoBind tycon_loc enumFromThen_RDR [a_Pat, b_Pat] [] $
	  untag_Expr tycon [(a_RDR, ah_RDR), (b_RDR, bh_RDR)] $
	  HsApp (mkHsVarApps map_RDR [tag2con_RDR tycon]) $
	    HsPar (enum_from_then_to_Expr
		    (mkHsVarApps mkInt_RDR [ah_RDR])
		    (mkHsVarApps mkInt_RDR [bh_RDR])
		    (HsIf  (mkHsApps gt_RDR [mkHsVarApps mkInt_RDR [ah_RDR],
					     mkHsVarApps mkInt_RDR [bh_RDR]])
			   (mkHsIntLit 0)
			   (HsVar (maxtag_RDR tycon))
			   tycon_loc))

    from_enum
      = mk_easy_FunMonoBind tycon_loc fromEnum_RDR [a_Pat] [] $
	  untag_Expr tycon [(a_RDR, ah_RDR)] $
	  (mkHsVarApps mkInt_RDR [ah_RDR])
\end{code}

%************************************************************************
%*									*
\subsubsection{Generating @Bounded@ instance declarations}
%*									*
%************************************************************************

\begin{code}
gen_Bounded_binds tycon
  = if isEnumerationTyCon tycon then
	min_bound_enum `AndMonoBinds` max_bound_enum
    else
	ASSERT(isSingleton data_cons)
	min_bound_1con `AndMonoBinds` max_bound_1con
  where
    data_cons = tyConDataCons tycon
    tycon_loc = getSrcLoc tycon

    ----- enum-flavored: ---------------------------
    min_bound_enum = mkVarMonoBind tycon_loc minBound_RDR (HsVar data_con_1_RDR)
    max_bound_enum = mkVarMonoBind tycon_loc maxBound_RDR (HsVar data_con_N_RDR)

    data_con_1	  = head data_cons
    data_con_N	  = last data_cons
    data_con_1_RDR = getRdrName data_con_1
    data_con_N_RDR = getRdrName data_con_N

    ----- single-constructor-flavored: -------------
    arity	   = dataConSourceArity data_con_1

    min_bound_1con = mkVarMonoBind tycon_loc minBound_RDR $
		     mkHsVarApps data_con_1_RDR (nOfThem arity minBound_RDR)
    max_bound_1con = mkVarMonoBind tycon_loc maxBound_RDR $
		     mkHsVarApps data_con_1_RDR (nOfThem arity maxBound_RDR)
\end{code}

%************************************************************************
%*									*
\subsubsection{Generating @Ix@ instance declarations}
%*									*
%************************************************************************

Deriving @Ix@ is only possible for enumeration types and
single-constructor types.  We deal with them in turn.

For an enumeration type, e.g.,
\begin{verbatim}
    data Foo ... = N1 | N2 | ... | Nn
\end{verbatim}
things go not too differently from @Enum@:
\begin{verbatim}
instance ... Ix (Foo ...) where
    range (a, b)
      = map tag2con_Foo [con2tag_Foo a .. con2tag_Foo b]

    -- or, really...
    range (a, b)
      = case (con2tag_Foo a) of { a# ->
	case (con2tag_Foo b) of { b# ->
	map tag2con_Foo (enumFromTo (I# a#) (I# b#))
	}}

    index c@(a, b) d
      = if inRange c d
	then case (con2tag_Foo d -# con2tag_Foo a) of
	       r# -> I# r#
	else error "Ix.Foo.index: out of range"

    inRange (a, b) c
      = let
	    p_tag = con2tag_Foo c
	in
	p_tag >= con2tag_Foo a && p_tag <= con2tag_Foo b

    -- or, really...
    inRange (a, b) c
      = case (con2tag_Foo a)   of { a_tag ->
	case (con2tag_Foo b)   of { b_tag ->
	case (con2tag_Foo c)   of { c_tag ->
	if (c_tag >=# a_tag) then
	  c_tag <=# b_tag
	else
	  False
	}}}
\end{verbatim}
(modulo suitable case-ification to handle the unlifted tags)

For a single-constructor type (NB: this includes all tuples), e.g.,
\begin{verbatim}
    data Foo ... = MkFoo a b Int Double c c
\end{verbatim}
we follow the scheme given in Figure~19 of the Haskell~1.2 report
(p.~147).

\begin{code}
gen_Ix_binds :: TyCon -> RdrNameMonoBinds

gen_Ix_binds tycon
  = if isEnumerationTyCon tycon
    then enum_ixes
    else single_con_ixes
  where
    tycon_str = getOccString tycon
    tycon_loc = getSrcLoc tycon

    --------------------------------------------------------------
    enum_ixes = enum_range `AndMonoBinds`
    	    	enum_index `AndMonoBinds` enum_inRange

    enum_range
      = mk_easy_FunMonoBind tycon_loc range_RDR 
		[TuplePat [a_Pat, b_Pat] Boxed] [] $
	  untag_Expr tycon [(a_RDR, ah_RDR)] $
	  untag_Expr tycon [(b_RDR, bh_RDR)] $
	  HsApp (mkHsVarApps map_RDR [tag2con_RDR tycon]) $
	      HsPar (enum_from_to_Expr
			(mkHsVarApps mkInt_RDR [ah_RDR])
			(mkHsVarApps mkInt_RDR [bh_RDR]))

    enum_index
      = mk_easy_FunMonoBind tycon_loc index_RDR 
		[AsPat c_RDR (TuplePat [a_Pat, wildPat] Boxed), 
				d_Pat] [] (
	HsIf (HsPar (mkHsVarApps inRange_RDR [c_RDR, d_RDR])) (
	   untag_Expr tycon [(a_RDR, ah_RDR)] (
	   untag_Expr tycon [(d_RDR, dh_RDR)] (
	   let
		rhs = mkHsVarApps mkInt_RDR [c_RDR]
	   in
	   HsCase
	     (genOpApp (HsVar dh_RDR) minusInt_RDR (HsVar ah_RDR))
	     [mk_triv_Match (VarPat c_RDR) rhs]
	     tycon_loc
	   ))
	) {-else-} (
	   HsApp (HsVar error_RDR) (HsLit (HsString (mkFastString ("Ix."++tycon_str++".index: out of range\n"))))
	)
	tycon_loc)

    enum_inRange
      = mk_easy_FunMonoBind tycon_loc inRange_RDR 
	  [TuplePat [a_Pat, b_Pat] Boxed, c_Pat] [] (
	  untag_Expr tycon [(a_RDR, ah_RDR)] (
	  untag_Expr tycon [(b_RDR, bh_RDR)] (
	  untag_Expr tycon [(c_RDR, ch_RDR)] (
	  HsIf (genOpApp (HsVar ch_RDR) geInt_RDR (HsVar ah_RDR)) (
	     (genOpApp (HsVar ch_RDR) leInt_RDR (HsVar bh_RDR))
	  ) {-else-} (
	     false_Expr
	  ) tycon_loc))))

    --------------------------------------------------------------
    single_con_ixes 
      = single_con_range `AndMonoBinds`
    	single_con_index `AndMonoBinds`
	single_con_inRange

    data_con
      =	case maybeTyConSingleCon tycon of -- just checking...
	  Nothing -> panic "get_Ix_binds"
	  Just dc | any isUnLiftedType (dataConOrigArgTys dc)
		  -> pprPanic "Can't derive Ix for a single-constructor type with primitive argument types:" (ppr tycon)
		  | otherwise -> dc

    con_arity    = dataConSourceArity data_con
    data_con_RDR = getRdrName data_con

    as_needed = take con_arity as_RDRs
    bs_needed = take con_arity bs_RDRs
    cs_needed = take con_arity cs_RDRs

    con_pat  xs  = mkConPat data_con_RDR xs
    con_expr     = mkHsVarApps data_con_RDR cs_needed

    --------------------------------------------------------------
    single_con_range
      = mk_easy_FunMonoBind tycon_loc range_RDR 
	  [TuplePat [con_pat as_needed, con_pat bs_needed] Boxed] [] $
	mkHsDo ListComp stmts tycon_loc
      where
	stmts = zipWith3Equal "single_con_range" mk_qual as_needed bs_needed cs_needed
		++
		[ResultStmt con_expr tycon_loc]

	mk_qual a b c = BindStmt (VarPat c)
				 (HsApp (HsVar range_RDR) 
					(ExplicitTuple [HsVar a, HsVar b] Boxed))
				 tycon_loc

    ----------------
    single_con_index
      = mk_easy_FunMonoBind tycon_loc index_RDR 
		[TuplePat [con_pat as_needed, con_pat bs_needed] Boxed, 
		 con_pat cs_needed] [range_size] (
	foldl mk_index (mkHsIntLit 0) (zip3 as_needed bs_needed cs_needed))
      where
	mk_index multiply_by (l, u, i)
	  = genOpApp (
	       (mkHsApps index_RDR [ExplicitTuple [HsVar l, HsVar u] Boxed,  
				    HsVar i])
	   ) plus_RDR (
		genOpApp (
		    (HsApp (HsVar rangeSize_RDR) 
			   (ExplicitTuple [HsVar l, HsVar u] Boxed))
		) times_RDR multiply_by
	   )

	range_size
	  = mk_easy_FunMonoBind tycon_loc rangeSize_RDR 
			[TuplePat [a_Pat, b_Pat] Boxed] [] (
		genOpApp (
		    (mkHsApps index_RDR [ExplicitTuple [a_Expr, b_Expr] Boxed,
					 b_Expr])
		) plus_RDR (mkHsIntLit 1))

    ------------------
    single_con_inRange
      = mk_easy_FunMonoBind tycon_loc inRange_RDR 
		[TuplePat [con_pat as_needed, con_pat bs_needed] Boxed, 
		 con_pat cs_needed]
			   [] (
	  foldl1 and_Expr (zipWith3Equal "single_con_inRange" in_range as_needed bs_needed cs_needed))
      where
    	in_range a b c = mkHsApps inRange_RDR [ExplicitTuple [HsVar a, HsVar b] Boxed,
					       HsVar c]
\end{code}

%************************************************************************
%*									*
\subsubsection{Generating @Read@ instance declarations}
%*									*
%************************************************************************

Example

  infix 4 %%
  data T = Int %% Int
 	 | T1 { f1 :: Int }
	 | T2 Int


instance Read T where
  readPrec =
    parens
    ( prec 4 (
        do x           <- ReadP.step Read.readPrec
           Symbol "%%" <- Lex.lex
           y           <- ReadP.step Read.readPrec
           return (x %% y))
      +++
      prec appPrec (
	do Ident "T1" <- Lex.lex
	   Punc '{' <- Lex.lex
	   Ident "f1" <- Lex.lex
	   Punc '=' <- Lex.lex
	   x	      <- ReadP.reset Read.readPrec
	   Punc '}' <- Lex.lex
	   return (T1 { f1 = x }))
      +++
      prec appPrec (
        do Ident "T2" <- Lex.lexP
           x          <- ReadP.step Read.readPrec
           return (T2 x))
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault


\begin{code}
gen_Read_binds :: FixityEnv -> TyCon -> RdrNameMonoBinds

gen_Read_binds get_fixity tycon
  = read_prec `AndMonoBinds` default_binds
  where
    -----------------------------------------------------------------------
    default_binds 
	= mkVarMonoBind loc readList_RDR     (HsVar readListDefault_RDR)
		`AndMonoBinds`
	  mkVarMonoBind loc readListPrec_RDR (HsVar readListPrecDefault_RDR)
    -----------------------------------------------------------------------

    loc       = getSrcLoc tycon
    data_cons = tyConDataCons tycon
    (nullary_cons, non_nullary_cons) = partition isNullaryDataCon data_cons
    
    read_prec = mkVarMonoBind loc readPrec_RDR
	 		      (HsApp (HsVar parens_RDR) read_cons)

    read_cons 	          = foldr1 mk_alt (read_nullary_cons ++ read_non_nullary_cons)
    read_non_nullary_cons = map read_non_nullary_con non_nullary_cons
    
    read_nullary_cons 
      = case nullary_cons of
    	    []    -> []
    	    [con] -> [mkHsDo DoExpr [bindLex (ident_pat (data_con_str con)),
		    		     result_stmt con []] loc]
            _     -> [HsApp (HsVar choose_RDR) 
    		   	    (ExplicitList placeHolderType (map mk_pair nullary_cons))]
    
    mk_pair con = ExplicitTuple [HsLit (data_con_str con),
    		   		 HsApp (HsVar returnM_RDR) (HsVar (getRdrName con))]
				Boxed
    
    read_non_nullary_con data_con
      = mkHsApps prec_RDR [mkHsIntLit prec, mkHsDo DoExpr stmts loc]
      where
       	stmts | is_infix 	  = infix_stmts
     	      | length labels > 0 = lbl_stmts
     	      | otherwise	  = prefix_stmts
     
       	prefix_stmts		-- T a b c
       	  = [bindLex (ident_pat (data_con_str data_con))]
       	    ++ read_args
       	    ++ [result_stmt data_con as_needed]
     	 
       	infix_stmts 		-- a %% b
       	  = [read_a1, 
   	     bindLex (symbol_pat (data_con_str data_con)),
     	     read_a2,
     	     result_stmt data_con [a1,a2]]
     
       	lbl_stmts		-- T { f1 = a, f2 = b }
       	  = [bindLex (ident_pat (data_con_str data_con)),
       	     read_punc "{"]
       	    ++ concat (intersperse [read_punc ","] field_stmts)
       	    ++ [read_punc "}", result_stmt data_con as_needed]
     
       	field_stmts  = zipWithEqual "lbl_stmts" read_field labels as_needed
     
       	con_arity    = dataConSourceArity data_con
       	nullary_con  = con_arity == 0
       	labels       = dataConFieldLabels data_con
       	lab_fields   = length labels
       	dc_nm	     = getName data_con
       	is_infix     = isDataSymOcc (getOccName dc_nm)
       	as_needed    = take con_arity as_RDRs
	read_args    = zipWithEqual "gen_Read_binds" read_arg as_needed (dataConOrigArgTys data_con)
       	(read_a1:read_a2:_) = read_args
	(a1:a2:_)           = as_needed
       	prec 	     = getPrec is_infix get_fixity dc_nm

    ------------------------------------------------------------------------
    --		Helpers
    ------------------------------------------------------------------------
    mk_alt e1 e2     = genOpApp e1 alt_RDR e2
    bindLex pat	     = BindStmt pat (HsVar lexP_RDR) loc
    result_stmt c as = ResultStmt (HsApp (HsVar returnM_RDR) (con_app c as)) loc
    con_app c as     = mkHsVarApps (getRdrName c) as
    
    punc_pat s   = ConPatIn punc_RDR  (PrefixCon [LitPat (mkHsString s)])	  -- Punc 'c'
    ident_pat s  = ConPatIn ident_RDR (PrefixCon [LitPat s])			  -- Ident "foo"
    symbol_pat s = ConPatIn symbol_RDR (PrefixCon [LitPat s])			  -- Symbol ">>"
    
    data_con_str con = mkHsString (occNameUserString (getOccName con))
    
    read_punc c = bindLex (punc_pat c)
    read_arg a ty 
	| isUnLiftedType ty = pprPanic "Error in deriving:" (text "Can't read unlifted types yet:" <+> ppr ty)
	| otherwise = BindStmt (VarPat a) (mkHsVarApps step_RDR [readPrec_RDR]) loc
    
    read_field lbl a = read_lbl lbl ++
    		       [read_punc "=",
    		        BindStmt (VarPat a) (mkHsVarApps reset_RDR [readPrec_RDR]) loc]

	-- When reading field labels we might encounter
	-- 	a  = 3
	-- 	_a = 3
	-- or	(#) = 4
	-- Note the parens!
    read_lbl lbl | is_id_start (head lbl_str) 
		 = [bindLex (ident_pat lbl_lit)]
		 | otherwise
		 = [read_punc "(", 
		    bindLex (symbol_pat lbl_lit),
		    read_punc ")"]
		 where	
		   lbl_str = occNameUserString (getOccName (fieldLabelName lbl)) 
		   lbl_lit = mkHsString lbl_str
		   is_id_start c = isAlpha c || c == '_'
\end{code}


%************************************************************************
%*									*
\subsubsection{Generating @Show@ instance declarations}
%*									*
%************************************************************************

Example

    infixr 5 :^:

    data Tree a =  Leaf a  |  Tree a :^: Tree a

    instance (Show a) => Show (Tree a) where

        showsPrec d (Leaf m) = showParen (d > app_prec) showStr
          where
             showStr = showString "Leaf " . showsPrec (app_prec+1) m

        showsPrec d (u :^: v) = showParen (d > up_prec) showStr
          where
             showStr = showsPrec (up_prec+1) u . 
                       showString " :^: "      .
                       showsPrec (up_prec+1) v
                -- Note: right-associativity of :^: ignored

    up_prec  = 5    -- Precedence of :^:
    app_prec = 10   -- Application has precedence one more than
		    -- the most tightly-binding operator

\begin{code}
gen_Show_binds :: FixityEnv -> TyCon -> RdrNameMonoBinds

gen_Show_binds get_fixity tycon
  = shows_prec `AndMonoBinds` show_list
  where
    tycon_loc = getSrcLoc tycon
    -----------------------------------------------------------------------
    show_list = mkVarMonoBind tycon_loc showList_RDR
		  (HsApp (HsVar showList___RDR) (HsPar (HsApp (HsVar showsPrec_RDR) (mkHsIntLit 0))))
    -----------------------------------------------------------------------
    shows_prec = mk_FunMonoBind tycon_loc showsPrec_RDR (map pats_etc (tyConDataCons tycon))
      where
	pats_etc data_con
	  | nullary_con =  -- skip the showParen junk...
	     ASSERT(null bs_needed)
	     ([wildPat, con_pat], mk_showString_app con_str)
	  | otherwise   =
	     ([a_Pat, con_pat],
		  showParen_Expr (HsPar (genOpApp a_Expr ge_RDR (HsLit (HsInt con_prec_plus_one))))
				 (HsPar (nested_compose_Expr show_thingies)))
	    where
	     data_con_RDR  = getRdrName data_con
	     con_arity     = dataConSourceArity data_con
	     bs_needed     = take con_arity bs_RDRs
	     arg_tys       = dataConOrigArgTys data_con		-- Correspond 1-1 with bs_needed
	     con_pat       = mkConPat data_con_RDR bs_needed
	     nullary_con   = con_arity == 0
             labels        = dataConFieldLabels data_con
	     lab_fields    = length labels
	     record_syntax = lab_fields > 0

	     dc_nm	    = getName data_con
	     dc_occ_nm	    = getOccName data_con
             con_str        = occNameUserString dc_occ_nm

	     show_thingies 
		| is_infix     	= [show_arg1, mk_showString_app (" " ++ con_str ++ " "), show_arg2]
		| record_syntax = mk_showString_app (con_str ++ " {") : 
				  show_record_args ++ [mk_showString_app "}"]
		| otherwise	= mk_showString_app (con_str ++ " ") : show_prefix_args
                
	     show_label l = mk_showString_app (the_name ++ " = ")
			-- Note the spaces around the "=" sign.  If we don't have them
			-- then we get Foo { x=-1 } and the "=-" parses as a single
			-- lexeme.  Only the space after the '=' is necessary, but
			-- it seems tidier to have them both sides.
		 where
		   occ_nm   = getOccName (fieldLabelName l)
		   nm       = occNameUserString occ_nm
		   is_op    = isSymOcc occ_nm	    -- Legal, but rare.
		   the_name | is_op     = '(':nm ++ ")"
			    | otherwise = nm

             show_args 		     = zipWith show_arg bs_needed arg_tys
	     (show_arg1:show_arg2:_) = show_args
	     show_prefix_args	     = intersperse (HsVar showSpace_RDR) show_args

		--  Assumption for record syntax: no of fields == no of labelled fields 
		--            (and in same order)
	     show_record_args = concat $
				intersperse [mk_showString_app ", "] $
				[ [show_label lbl, arg] 
				| (lbl,arg) <- zipEqual "gen_Show_binds" 
							labels show_args ]
			       
		-- Generates (showsPrec p x) for argument x, but it also boxes
		-- the argument first if necessary.  Note that this prints unboxed
		-- things without any '#' decorations; could change that if need be
	     show_arg b arg_ty = mkHsApps showsPrec_RDR [HsLit (HsInt arg_prec), 
							 box_if_necy "Show" tycon (HsVar b) arg_ty]

		-- Fixity stuff
	     is_infix = isDataSymOcc dc_occ_nm
             con_prec_plus_one = 1 + getPrec is_infix get_fixity dc_nm
	     arg_prec | record_syntax = 0	-- Record fields don't need parens
		      | otherwise     = con_prec_plus_one

mk_showString_app str = HsApp (HsVar showString_RDR) (HsLit (mkHsString str))
\end{code}

\begin{code}
getPrec :: Bool -> FixityEnv -> Name -> Integer
getPrec is_infix get_fixity nm 
  | not is_infix   = appPrecedence
  | otherwise      = getPrecedence get_fixity nm
		  
appPrecedence :: Integer
appPrecedence = fromIntegral maxPrecedence + 1
  -- One more than the precedence of the most 
  -- tightly-binding operator

getPrecedence :: FixityEnv -> Name -> Integer
getPrecedence get_fixity nm 
   = case lookupFixity get_fixity nm of
        Fixity x _ -> fromIntegral x

isLRAssoc :: FixityEnv -> Name -> (Bool, Bool)
isLRAssoc get_fixity nm =
     case lookupFixity get_fixity nm of
       Fixity _ InfixN -> (False, False)
       Fixity _ InfixR -> (False, True)
       Fixity _ InfixL -> (True,  False)
\end{code}


%************************************************************************
%*									*
\subsection{Typeable}
%*									*
%************************************************************************

From the data type

	data T a b = ....

we generate

	instance (Typeable a, Typeable b) => Typeable (T a b) where
		typeOf _ = mkTypeRep (mkTyConRep "T")
				     [typeOf (undefined::a),
				      typeOf (undefined::b)]

Notice the use of lexically scoped type variables.

\begin{code}
gen_Typeable_binds :: TyCon -> RdrNameMonoBinds
gen_Typeable_binds tycon
  = mk_easy_FunMonoBind tycon_loc typeOf_RDR [wildPat] []
	(mkHsApps mkTypeRep_RDR [tycon_rep, arg_reps])
  where
    tycon_loc = getSrcLoc tycon
    tyvars    = tyConTyVars tycon
    tycon_rep = HsVar mkTyConRep_RDR `HsApp` HsLit (mkHsString (showSDoc (ppr tycon)))
    arg_reps  = ExplicitList placeHolderType (map mk tyvars)
    mk tyvar  = HsApp (HsVar typeOf_RDR) 
		      (ExprWithTySig (HsVar undefined_RDR)
				     (HsTyVar (getRdrName tyvar)))
\end{code}



%************************************************************************
%*									*
\subsection{Data}
%*									*
%************************************************************************

From the data type

  data T a b = T1 a b | T2

we generate

  $cT1 = mkConstr 1 "T1" Prefix
  $cT2 = mkConstr 2 "T2" Prefix
  $dT  = mkDataType [$con_T1, $con_T2]

  instance (Data a, Data b) => Data (T a b) where
    gfoldl k z (T1 a b) = z T `k` a `k` b
    gfoldl k z T2	    = z T2
    -- ToDo: add gmapT,Q,M, gfoldr
    
    fromConstr c = case conIndex c of
	    		I# 1# -> T1 undefined undefined
    			I# 2# -> T2
    
    toConstr (T1 _ _) = $cT1
    toConstr T2	      = $cT2
    
    dataTypeOf _ = $dT

\begin{code}
gen_Data_binds :: FixityEnv
	       -> TyCon 
	       -> (RdrNameMonoBinds,	-- The method bindings
		   RdrNameMonoBinds)	-- Auxiliary bindings
gen_Data_binds fix_env tycon
  = (andMonoBindList [gfoldl_bind, fromCon_bind, toCon_bind, dataTypeOf_bind],
		-- Auxiliary definitions: the data type and constructors
     datatype_bind `AndMonoBinds` andMonoBindList (map mk_con_bind data_cons))
  where
    tycon_loc = getSrcLoc tycon
    data_cons = tyConDataCons tycon

	------------ gfoldl
    gfoldl_bind = mk_FunMonoBind tycon_loc gfoldl_RDR (map gfoldl_eqn data_cons)
    gfoldl_eqn con = ([VarPat k_RDR, VarPat z_RDR, mkConPat con_name as_needed], 
		       foldl mk_k_app (HsVar z_RDR `HsApp` HsVar con_name) as_needed)
		   where
		     con_name :: RdrName
		     con_name = getRdrName con
		     as_needed = take (dataConSourceArity con) as_RDRs
		     mk_k_app e v = HsPar (mkHsOpApp e k_RDR (HsVar v))

	------------ fromConstr
    fromCon_bind = mk_FunMonoBind tycon_loc fromConstr_RDR [([c_Pat], from_con_rhs)]
    from_con_rhs = HsCase (HsVar conIndex_RDR `HsApp` c_Expr) 
			  (map from_con_alt data_cons) tycon_loc
    from_con_alt dc = mk_triv_Match (ConPatIn mkInt_RDR (PrefixCon [LitPat (HsIntPrim (toInteger (dataConTag dc)))]))
				    (mkHsVarApps (getRdrName dc)
					         (replicate (dataConSourceArity dc) undefined_RDR))
			  
	------------ toConstr
    toCon_bind = mk_FunMonoBind tycon_loc toConstr_RDR (map to_con_eqn data_cons)
    to_con_eqn dc = ([mkWildConPat dc], HsVar (mkConstrName dc))
    
	------------ dataTypeOf
    dataTypeOf_bind = mk_easy_FunMonoBind tycon_loc dataTypeOf_RDR [wildPat] 
					  [] (HsVar data_type_name)

	------------ $dT
    data_type_name = mkDataTypeName tycon
    datatype_bind  = mkVarMonoBind tycon_loc data_type_name
				   (HsVar mkDataType_RDR `HsApp` 
				    ExplicitList placeHolderType constrs)
    constrs = [HsVar (mkConstrName con) | con <- data_cons]

	------------ $cT1 etc
    mk_con_bind dc = mkVarMonoBind tycon_loc (mkConstrName dc) 
					     (mkHsApps mkConstr_RDR (constr_args dc))
    constr_args dc = [mkHsIntLit (toInteger (dataConTag dc)),		-- Tag
		      HsLit (mkHsString (occNameUserString dc_occ)),	-- String name
		      HsVar fixity]					-- Fixity
	where
	  dc_occ   = getOccName dc
	  is_infix = isDataSymOcc dc_occ
	  fixity | is_infix  = infix_RDR
		 | otherwise = prefix_RDR

gfoldl_RDR     = varQual_RDR gENERICS_Name FSLIT("gfoldl")
fromConstr_RDR = varQual_RDR gENERICS_Name FSLIT("fromConstr")
toConstr_RDR   = varQual_RDR gENERICS_Name FSLIT("toConstr")
dataTypeOf_RDR = varQual_RDR gENERICS_Name FSLIT("dataTypeOf")
mkConstr_RDR   = varQual_RDR gENERICS_Name FSLIT("mkConstr")
mkDataType_RDR = varQual_RDR gENERICS_Name FSLIT("mkDataType")
conIndex_RDR   = varQual_RDR gENERICS_Name FSLIT("conIndex")
prefix_RDR     = dataQual_RDR gENERICS_Name FSLIT("Prefix")
infix_RDR      = dataQual_RDR gENERICS_Name FSLIT("Infix")

mkDataTypeName :: TyCon -> RdrName	-- $tT
mkDataTypeName tc = mkRdrUnqual (mkDataTOcc (getOccName tc))

mkConstrName :: DataCon -> RdrName	-- $cT1
mkConstrName con = mkRdrUnqual (mkDataCOcc (getOccName con))


apN :: Int -> (a -> a) -> a -> a
apN 0 k z = z
apN n k z = apN (n-1) k (k z)
\end{code}

%************************************************************************
%*									*
\subsection{Generating extra binds (@con2tag@ and @tag2con@)}
%*									*
%************************************************************************

\begin{verbatim}
data Foo ... = ...

con2tag_Foo :: Foo ... -> Int#
tag2con_Foo :: Int -> Foo ...	-- easier if Int, not Int#
maxtag_Foo  :: Int		-- ditto (NB: not unlifted)
\end{verbatim}

The `tags' here start at zero, hence the @fIRST_TAG@ (currently one)
fiddling around.

\begin{code}
data TagThingWanted
  = GenCon2Tag | GenTag2Con | GenMaxTag

gen_tag_n_con_monobind
    :: (RdrName,	    -- (proto)Name for the thing in question
	TyCon,		    -- tycon in question
	TagThingWanted)
    -> RdrNameMonoBinds

gen_tag_n_con_monobind (rdr_name, tycon, GenCon2Tag)
  | lots_of_constructors
  = mk_FunMonoBind loc rdr_name [([], get_tag_rhs)]

  | otherwise
  = mk_FunMonoBind loc rdr_name (map mk_stuff (tyConDataCons tycon))

  where
    loc = getSrcLoc tycon

	-- Give a signature to the bound variable, so 
	-- that the case expression generated by getTag is
	-- monomorphic.  In the push-enter model we get better code.
    get_tag_rhs = ExprWithTySig 
			(HsLam (mk_match loc [VarPat a_RDR] 
					     (HsApp getTag_Expr a_Expr) 
					     EmptyBinds))
			(HsForAllTy Nothing [] con2tag_ty)
				-- Nothing => implicit quantification

    con2tag_ty = foldl HsAppTy (HsTyVar (getRdrName tycon)) 
		     [HsTyVar (getRdrName tv) | tv <- tyConTyVars tycon]
		`HsFunTy` 
		HsTyVar (getRdrName intPrimTyConName)

    lots_of_constructors = tyConFamilySize tycon > mAX_FAMILY_SIZE_FOR_VEC_RETURNS

    mk_stuff :: DataCon -> ([RdrNamePat], RdrNameHsExpr)
    mk_stuff con = ([mkWildConPat con], 
		    HsLit (HsIntPrim (toInteger ((dataConTag con) - fIRST_TAG))))

gen_tag_n_con_monobind (rdr_name, tycon, GenTag2Con)
  = mk_FunMonoBind (getSrcLoc tycon) rdr_name 
	[([mkConPat mkInt_RDR [a_RDR]], 
	   ExprWithTySig (HsApp tagToEnum_Expr a_Expr) 
			 (HsTyVar (getRdrName tycon)))]

gen_tag_n_con_monobind (rdr_name, tycon, GenMaxTag)
  = mkVarMonoBind (getSrcLoc tycon) rdr_name 
		  (HsApp (HsVar mkInt_RDR) (HsLit (HsIntPrim max_tag)))
  where
    max_tag =  case (tyConDataCons tycon) of
		 data_cons -> toInteger ((length data_cons) - fIRST_TAG)

\end{code}

%************************************************************************
%*									*
\subsection{Utility bits for generating bindings}
%*									*
%************************************************************************

@mk_easy_FunMonoBind fun pats binds expr@ generates:
\begin{verbatim}
    fun pat1 pat2 ... patN = expr where binds
\end{verbatim}

@mk_FunMonoBind fun [([p1a, p1b, ...], e1), ...]@ is for
multi-clause definitions; it generates:
\begin{verbatim}
    fun p1a p1b ... p1N = e1
    fun p2a p2b ... p2N = e2
    ...
    fun pMa pMb ... pMN = eM
\end{verbatim}

\begin{code}
mkVarMonoBind :: SrcLoc -> RdrName -> RdrNameHsExpr -> RdrNameMonoBinds
mkVarMonoBind loc var rhs = mk_easy_FunMonoBind loc var [] [] rhs

mk_easy_FunMonoBind :: SrcLoc -> RdrName -> [RdrNamePat]
		    -> [RdrNameMonoBinds] -> RdrNameHsExpr
		    -> RdrNameMonoBinds

mk_easy_FunMonoBind loc fun pats binds expr
  = FunMonoBind fun False{-not infix-} [mk_easy_Match loc pats binds expr] loc

mk_easy_Match loc pats binds expr
  = mk_match loc pats expr (mkMonoBind Recursive (andMonoBindList binds))
	-- The renamer expects everything in its input to be a
	-- "recursive" MonoBinds, and it is its job to sort things out
	-- from there.

mk_triv_Match pat expr = mkSimpleMatch [pat] expr placeHolderType generatedSrcLoc

mk_FunMonoBind	:: SrcLoc -> RdrName
		-> [([RdrNamePat], RdrNameHsExpr)]
		-> RdrNameMonoBinds

mk_FunMonoBind loc fun [] = panic "TcGenDeriv:mk_FunMonoBind"
mk_FunMonoBind loc fun pats_and_exprs
  = FunMonoBind fun False{-not infix-}
		[ mk_match loc p e EmptyBinds | (p,e) <-pats_and_exprs ]
		loc

mk_match loc pats expr binds
  = Match (map paren pats) Nothing 
	  (GRHSs (unguardedRHS expr loc) binds placeHolderType)
  where
    paren p@(VarPat _) = p
    paren other_p      = ParPat other_p
\end{code}

\begin{code}
mkHsApps    f xs = foldl HsApp (HsVar f) xs
mkHsVarApps f xs = foldl HsApp (HsVar f) (map HsVar xs)

mkHsIntLit n = HsLit (HsInt n)
mkHsString s = HsString (mkFastString s)
mkHsChar c   = HsChar   (ord c)

mkConPat con vars   = ConPatIn con (PrefixCon (map VarPat vars))
mkNullaryConPat con = ConPatIn con (PrefixCon [])
mkWildConPat con    = ConPatIn (getRdrName con) (PrefixCon (nOfThem (dataConSourceArity con) wildPat))
\end{code}

ToDo: Better SrcLocs.

\begin{code}
compare_gen_Case ::
	  RdrNameHsExpr	-- What to do for equality
	  -> RdrNameHsExpr -> RdrNameHsExpr
	  -> RdrNameHsExpr
careful_compare_Case :: -- checks for primitive types...
	  TyCon			-- The tycon we are deriving for
	  -> Type
	  -> RdrNameHsExpr	-- What to do for equality
	  -> RdrNameHsExpr -> RdrNameHsExpr
	  -> RdrNameHsExpr

cmp_eq_Expr a b = HsApp (HsApp (HsVar cmp_eq_RDR) a) b
	-- Was: compare_gen_Case cmp_eq_RDR

compare_gen_Case (HsVar eq_tag) a b | eq_tag == eqTag_RDR
  = HsApp (HsApp (HsVar compare_RDR) a) b	-- Simple case 
compare_gen_Case eq a b				-- General case
  = HsCase (HsPar (HsApp (HsApp (HsVar compare_RDR) a) b)) {-of-}
      [mk_triv_Match (mkNullaryConPat ltTag_RDR) ltTag_Expr,
       mk_triv_Match (mkNullaryConPat eqTag_RDR) eq,
       mk_triv_Match (mkNullaryConPat gtTag_RDR) gtTag_Expr]
      generatedSrcLoc

careful_compare_Case tycon ty eq a b
  | not (isUnLiftedType ty)
  = compare_gen_Case eq a b
  | otherwise      -- We have to do something special for primitive things...
  = HsIf (genOpApp a relevant_eq_op b)
	 eq
	 (HsIf (genOpApp a relevant_lt_op b) ltTag_Expr gtTag_Expr generatedSrcLoc)
	 generatedSrcLoc
  where
    relevant_eq_op = assoc_ty_id "Ord" tycon eq_op_tbl ty
    relevant_lt_op = assoc_ty_id "Ord" tycon lt_op_tbl ty


box_if_necy :: String		-- The class involved
	    -> TyCon		-- The tycon involved
	    -> RdrNameHsExpr	-- The argument
	    -> Type		-- The argument type
	    -> RdrNameHsExpr	-- Boxed version of the arg
box_if_necy cls_str tycon arg arg_ty
  | isUnLiftedType arg_ty = HsApp (HsVar box_con) arg
  | otherwise		  = arg
  where
    box_con = assoc_ty_id cls_str tycon box_con_tbl arg_ty

assoc_ty_id :: String		-- The class involved
	    -> TyCon		-- The tycon involved
	    -> [(Type,a)]	-- The table
	    -> Type		-- The type
	    -> a		-- The result of the lookup
assoc_ty_id cls_str tycon tbl ty 
  | null res = pprPanic "Error in deriving:" (text "Can't derive" <+> text cls_str <+> 
					      text "for primitive type" <+> ppr ty)
  | otherwise = head res
  where
    res = [id | (ty',id) <- tbl, ty `tcEqType` ty']

eq_op_tbl =
    [(charPrimTy,	eqChar_RDR)
    ,(intPrimTy,	eqInt_RDR)
    ,(wordPrimTy,	eqWord_RDR)
    ,(addrPrimTy,	eqAddr_RDR)
    ,(floatPrimTy,	eqFloat_RDR)
    ,(doublePrimTy,	eqDouble_RDR)
    ]

lt_op_tbl =
    [(charPrimTy,	ltChar_RDR)
    ,(intPrimTy,	ltInt_RDR)
    ,(wordPrimTy,	ltWord_RDR)
    ,(addrPrimTy,	ltAddr_RDR)
    ,(floatPrimTy,	ltFloat_RDR)
    ,(doublePrimTy,	ltDouble_RDR)
    ]

box_con_tbl =
    [(charPrimTy,	getRdrName charDataCon)
    ,(intPrimTy,	getRdrName intDataCon)
    ,(wordPrimTy,	getRdrName wordDataCon)
    ,(addrPrimTy,	addrDataCon_RDR)
    ,(floatPrimTy,	getRdrName floatDataCon)
    ,(doublePrimTy,	getRdrName doubleDataCon)
    ]

-----------------------------------------------------------------------

and_Expr, append_Expr :: RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr

and_Expr    a b = genOpApp a and_RDR    b
append_Expr a b = genOpApp a append_RDR b

-----------------------------------------------------------------------

eq_Expr :: TyCon -> Type -> RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr
eq_Expr tycon ty a b = genOpApp a eq_op b
 where
   eq_op
    | not (isUnLiftedType ty) = eq_RDR
    | otherwise               =
         -- we have to do something special for primitive things...
	assoc_ty_id "Eq" tycon eq_op_tbl ty

\end{code}

\begin{code}
untag_Expr :: TyCon -> [(RdrName, RdrName)] -> RdrNameHsExpr -> RdrNameHsExpr
untag_Expr tycon [] expr = expr
untag_Expr tycon ((untag_this, put_tag_here) : more) expr
  = HsCase (HsPar (HsApp (con2tag_Expr tycon) (HsVar untag_this))) {-of-}
      [mk_triv_Match (VarPat put_tag_here) (untag_Expr tycon more expr)]
      generatedSrcLoc

cmp_tags_Expr :: RdrName 		-- Comparison op
	     -> RdrName -> RdrName	-- Things to compare
	     -> RdrNameHsExpr 		-- What to return if true
	     -> RdrNameHsExpr		-- What to return if false
	     -> RdrNameHsExpr

cmp_tags_Expr op a b true_case false_case
  = HsIf (genOpApp (HsVar a) op (HsVar b)) true_case false_case generatedSrcLoc

enum_from_to_Expr
	:: RdrNameHsExpr -> RdrNameHsExpr
	-> RdrNameHsExpr
enum_from_then_to_Expr
	:: RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr
	-> RdrNameHsExpr

enum_from_to_Expr      f   t2 = HsApp (HsApp (HsVar enumFromTo_RDR) f) t2
enum_from_then_to_Expr f t t2 = HsApp (HsApp (HsApp (HsVar enumFromThenTo_RDR) f) t) t2

showParen_Expr
	:: RdrNameHsExpr -> RdrNameHsExpr
	-> RdrNameHsExpr

showParen_Expr e1 e2 = HsApp (HsApp (HsVar showParen_RDR) e1) e2

nested_compose_Expr :: [RdrNameHsExpr] -> RdrNameHsExpr

nested_compose_Expr [e] = parenify e
nested_compose_Expr (e:es)
  = HsApp (HsApp (HsVar compose_RDR) (parenify e)) (nested_compose_Expr es)

-- impossible_Expr is used in case RHSs that should never happen.
-- We generate these to keep the desugarer from complaining that they *might* happen!
impossible_Expr = HsApp (HsVar error_RDR) (HsLit (HsString (mkFastString "Urk! in TcGenDeriv")))

-- illegal_Expr is used when signalling error conditions in the RHS of a derived
-- method. It is currently only used by Enum.{succ,pred}
illegal_Expr meth tp msg = 
   HsApp (HsVar error_RDR) (HsLit (HsString (mkFastString (meth ++ '{':tp ++ "}: " ++ msg))))

-- illegal_toEnum_tag is an extended version of illegal_Expr, which also allows you
-- to include the value of a_RDR in the error string.
illegal_toEnum_tag tp maxtag =
   HsApp (HsVar error_RDR) 
         (HsApp (HsApp (HsVar append_RDR)
	               (HsLit (HsString (mkFastString ("toEnum{" ++ tp ++ "}: tag (")))))
	               (HsApp (HsApp (HsApp 
		           (HsVar showsPrec_RDR)
			   (mkHsIntLit 0))
   		           (HsVar a_RDR))
			   (HsApp (HsApp 
			       (HsVar append_RDR)
			       (HsLit (HsString (mkFastString ") is outside of enumeration's range (0,"))))
			       (HsApp (HsApp (HsApp 
					(HsVar showsPrec_RDR)
				        (mkHsIntLit 0))
					(HsVar maxtag))
					(HsLit (HsString (mkFastString ")")))))))

parenify e@(HsVar _) = e
parenify e	     = HsPar e

-- genOpApp wraps brackets round the operator application, so that the
-- renamer won't subsequently try to re-associate it. 
-- For some reason the renamer doesn't reassociate it right, and I can't
-- be bothered to find out why just now.

genOpApp e1 op e2 = mkHsOpApp e1 op e2
\end{code}

\begin{code}
varUnqual n     = mkUnqual OccName.varName n

zz_a_RDR	= varUnqual FSLIT("_a")
a_RDR		= varUnqual FSLIT("a")
b_RDR		= varUnqual FSLIT("b")
c_RDR		= varUnqual FSLIT("c")
d_RDR		= varUnqual FSLIT("d")
e_RDR		= varUnqual FSLIT("e")
k_RDR		= varUnqual FSLIT("k")
z_RDR		= varUnqual FSLIT("z") :: RdrName
ah_RDR		= varUnqual FSLIT("a#")
bh_RDR		= varUnqual FSLIT("b#")
ch_RDR		= varUnqual FSLIT("c#")
dh_RDR		= varUnqual FSLIT("d#")
cmp_eq_RDR	= varUnqual FSLIT("cmp_eq")
rangeSize_RDR	= varUnqual FSLIT("rangeSize")

as_RDRs		= [ varUnqual (mkFastString ("a"++show i)) | i <- [(1::Int) .. ] ]
bs_RDRs		= [ varUnqual (mkFastString ("b"++show i)) | i <- [(1::Int) .. ] ]
cs_RDRs		= [ varUnqual (mkFastString ("c"++show i)) | i <- [(1::Int) .. ] ]

zz_a_Expr	= HsVar zz_a_RDR
a_Expr		= HsVar a_RDR
b_Expr		= HsVar b_RDR
c_Expr		= HsVar c_RDR
d_Expr		= HsVar d_RDR
z_Expr		= HsVar z_RDR
ltTag_Expr	= HsVar ltTag_RDR
eqTag_Expr	= HsVar eqTag_RDR
gtTag_Expr	= HsVar gtTag_RDR
false_Expr	= HsVar false_RDR
true_Expr	= HsVar true_RDR

getTag_Expr  	= HsVar getTag_RDR
tagToEnum_Expr 	= HsVar tagToEnum_RDR
con2tag_Expr tycon = HsVar (con2tag_RDR tycon)

wildPat		= WildPat placeHolderType
zz_a_Pat	= VarPat zz_a_RDR
a_Pat		= VarPat a_RDR
b_Pat		= VarPat b_RDR
c_Pat		= VarPat c_RDR
d_Pat		= VarPat d_RDR

con2tag_RDR, tag2con_RDR, maxtag_RDR :: TyCon -> RdrName

con2tag_RDR tycon = varUnqual (mkFastString ("con2tag_" ++ occNameString (getOccName tycon) ++ "#"))
tag2con_RDR tycon = varUnqual (mkFastString ("tag2con_" ++ occNameString (getOccName tycon) ++ "#"))
maxtag_RDR tycon  = varUnqual (mkFastString ("maxtag_"  ++ occNameString (getOccName tycon) ++ "#"))
\end{code}

RdrNames for PrimOps.  Can't be done in PrelNames, because PrimOp imports
PrelNames, so PrelNames can't import PrimOp.

\begin{code}
minusInt_RDR  = nameRdrName minusIntName
eqInt_RDR     = nameRdrName eqIntName
ltInt_RDR     = nameRdrName ltIntName
geInt_RDR     = nameRdrName geIntName
leInt_RDR     = nameRdrName leIntName
eqChar_RDR    = nameRdrName eqCharName
eqWord_RDR    = nameRdrName eqWordName
eqAddr_RDR    = nameRdrName eqAddrName
eqFloat_RDR   = nameRdrName eqFloatName
eqDouble_RDR  = nameRdrName eqDoubleName
ltChar_RDR    = nameRdrName ltCharName
ltWord_RDR    = nameRdrName ltWordName
ltAddr_RDR    = nameRdrName ltAddrName
ltFloat_RDR   = nameRdrName ltFloatName
ltDouble_RDR  = nameRdrName ltDoubleName
tagToEnum_RDR = nameRdrName tagToEnumName                   
\end{code}
