%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcGenDeriv]{Generating derived instance declarations}

This module is nominally ``subordinate'' to @TcDeriv@, which is the
``official'' interface to deriving-related things.

This is where we do all the grimy bindings' generation.

\begin{code}
#include "HsVersions.h"

module TcGenDeriv (
	a_Expr,
	a_PN,
	a_Pat,
	ah_PN,
	b_Expr,
	b_PN,
	b_Pat,
	bh_PN,
	c_Expr,
	c_PN,
	c_Pat,
	ch_PN,
	cmp_eq_PN,
	d_Expr,
	d_PN,
	d_Pat,
	dh_PN,
	eqH_Int_PN,
	eqTag_Expr,
	eq_PN,
	error_PN,
	false_Expr,
	false_PN,
	geH_PN,
	gen_Bounded_binds,
	gen_Enum_binds,
	gen_Eval_binds,
	gen_Eq_binds,
	gen_Ix_binds,
	gen_Ord_binds,
	gen_Read_binds,
	gen_Show_binds,
	gen_tag_n_con_monobind,
	gtTag_Expr,
	gt_PN,
	leH_PN,
	ltH_Int_PN,
	ltTag_Expr,
	lt_PN,
	minusH_PN,
	mkInt_PN,
	rangeSize_PN,
	true_Expr,
	true_PN,

	con2tag_PN, tag2con_PN, maxtag_PN,

	TagThingWanted(..)
    ) where

IMP_Ubiq()

import HsSyn		( HsBinds(..), Bind(..), MonoBinds(..), Match(..), GRHSsAndBinds(..),
			  GRHS(..), HsExpr(..), HsLit(..), InPat(..), Qual(..), Stmt,
			  ArithSeqInfo, Sig, PolyType, FixityDecl, Fake )
import RdrHsSyn		( RdrNameMonoBinds(..), RdrNameHsExpr(..), RdrNamePat(..) )
import RnHsSyn		( RenamedFixityDecl(..) )
--import RnUtils

import Id		( GenId, dataConArity, isNullaryDataCon, dataConTag,
			  dataConRawArgTys, fIRST_TAG,
			  isDataCon, DataCon(..), ConTag(..) )
import IdUtils		( primOpId )
import Maybes		( maybeToBool )
import Name		( moduleNamePair, origName, RdrName(..) )
import PrelMods		( fromPrelude, pRELUDE, pRELUDE_BUILTIN, pRELUDE_LIST, pRELUDE_TEXT )
import PrelVals		( eRROR_ID )

import PrimOp		( PrimOp(..) )
import SrcLoc		( mkGeneratedSrcLoc )
import TyCon		( TyCon, tyConDataCons, isEnumerationTyCon, maybeTyConSingleCon )
import Type		( eqTy, isPrimType )
import TysPrim		( charPrimTy, intPrimTy, wordPrimTy, addrPrimTy,
			  floatPrimTy, doublePrimTy
			)
import TysWiredIn	( falseDataCon, trueDataCon, intDataCon )
--import Unique
import Util		( mapAccumL, zipEqual, zipWith3Equal, nOfThem, panic, assertPanic )
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

  Note: if we're comparing unboxed things, e.g., if \tr{a1} and
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
	(nullary_cons, nonnullary_cons)
	  = partition isNullaryDataCon (tyConDataCons tycon)

	rest
	  = if (null nullary_cons) then
		case maybeTyConSingleCon tycon of
		  Just _ -> []
		  Nothing -> -- if cons don't match, then False
		     [([a_Pat, b_Pat], false_Expr)]
	    else -- calc. and compare the tags
		 [([a_Pat, b_Pat],
		    untag_Expr tycon [(a_PN,ah_PN), (b_PN,bh_PN)]
		      (cmp_tags_Expr eqH_Int_PN ah_PN bh_PN true_Expr false_Expr))]
    in
    mk_FunMonoBind eq_PN ((map pats_etc nonnullary_cons) ++ rest)
    `AndMonoBinds` boring_ne_method
  where
    ------------------------------------------------------------------
    pats_etc data_con
      = let
	    con1_pat = ConPatIn data_con_PN (map VarPatIn as_needed)
	    con2_pat = ConPatIn data_con_PN (map VarPatIn bs_needed)

	    data_con_PN = origName data_con
	    con_arity   = dataConArity data_con
	    as_needed   = take con_arity as_PNs
	    bs_needed   = take con_arity bs_PNs
	    tys_needed  = dataConRawArgTys data_con
	in
	([con1_pat, con2_pat], nested_eq_expr tys_needed as_needed bs_needed)
      where
	nested_eq_expr []  [] [] = true_Expr
	nested_eq_expr tys as bs
	  = foldr1 and_Expr (zipWith3Equal "nested_eq" nested_eq tys as bs)
	  where
	    nested_eq ty a b = HsPar (eq_Expr ty (HsVar a) (HsVar b))
{-OLD:
	nested_eq_expr []     []     []  = true_Expr
	nested_eq_expr [ty]   [a]    [b] = 
	nested_eq_expr (t:ts) (a:as) (b:bs)
	  = let
		rest_expr = nested_eq_expr ts as bs
	    in
	    and_Expr (eq_Expr t (HsVar a) (HsVar b)) rest_expr
-}

boring_ne_method
  = mk_easy_FunMonoBind ne_PN [a_Pat, b_Pat] [] $
	HsApp (HsVar not_PN) (HsPar (mk_easy_App eq_PN [a_PN, b_PN]))
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

  Again, we must be careful about unboxed comparisons.  For example,
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

\begin{code}
gen_Ord_binds :: TyCon -> RdrNameMonoBinds

gen_Ord_binds tycon
  = defaulted `AndMonoBinds` compare
  where
    --------------------------------------------------------------------
    compare = mk_easy_FunMonoBind compare_PN
		[a_Pat, b_Pat]
		[cmp_eq]
	    (if maybeToBool (maybeTyConSingleCon tycon) then
		cmp_eq_Expr ltTag_Expr eqTag_Expr gtTag_Expr a_Expr b_Expr
	     else
		untag_Expr tycon [(a_PN, ah_PN), (b_PN, bh_PN)]
		  (cmp_tags_Expr eqH_Int_PN ah_PN bh_PN
			-- True case; they are equal
			-- If an enumeration type we are done; else
			-- recursively compare their components
		    (if isEnumerationTyCon tycon then
			eqTag_Expr
		     else
			cmp_eq_Expr ltTag_Expr eqTag_Expr gtTag_Expr a_Expr b_Expr
		    )
			-- False case; they aren't equal
			-- So we need to do a less-than comparison on the tags
		    (cmp_tags_Expr ltH_Int_PN ah_PN bh_PN ltTag_Expr gtTag_Expr)))

    (nullary_cons, nonnullary_cons)
      = partition (\ con -> dataConArity con == 0) (tyConDataCons tycon)

    cmp_eq
      = mk_FunMonoBind cmp_eq_PN (map pats_etc nonnullary_cons ++ deflt_pats_etc)
      where
	pats_etc data_con
	  = ([con1_pat, con2_pat],
	     nested_compare_expr tys_needed as_needed bs_needed)
	  where
	    con1_pat = ConPatIn data_con_PN (map VarPatIn as_needed)
	    con2_pat = ConPatIn data_con_PN (map VarPatIn bs_needed)

	    data_con_PN = origName data_con
	    con_arity   = dataConArity data_con
	    as_needed   = take con_arity as_PNs
	    bs_needed   = take con_arity bs_PNs
	    tys_needed  = dataConRawArgTys data_con

	    nested_compare_expr [ty] [a] [b]
	      = careful_compare_Case ty ltTag_Expr eqTag_Expr gtTag_Expr (HsVar a) (HsVar b)

	    nested_compare_expr (ty:tys) (a:as) (b:bs)
	      = let eq_expr = nested_compare_expr tys as bs
		in  careful_compare_Case ty ltTag_Expr eq_expr gtTag_Expr (HsVar a) (HsVar b)

	deflt_pats_etc
	  = if null nullary_cons
	    then []
	    else [([a_Pat, b_Pat], eqTag_Expr)]
    --------------------------------------------------------------------

defaulted = foldr1 AndMonoBinds [lt, le, ge, gt, max_, min_]

lt = mk_easy_FunMonoBind lt_PN [a_Pat, b_Pat] [] (
	    compare_Case true_Expr  false_Expr false_Expr a_Expr b_Expr)
le = mk_easy_FunMonoBind le_PN [a_Pat, b_Pat] [] (
	    compare_Case true_Expr  true_Expr  false_Expr a_Expr b_Expr)
ge = mk_easy_FunMonoBind ge_PN [a_Pat, b_Pat] [] (
	    compare_Case false_Expr true_Expr  true_Expr  a_Expr b_Expr)
gt = mk_easy_FunMonoBind gt_PN [a_Pat, b_Pat] [] (
	    compare_Case false_Expr false_Expr true_Expr  a_Expr b_Expr)

max_ = mk_easy_FunMonoBind max_PN [a_Pat, b_Pat] [] (
	    compare_Case b_Expr a_Expr a_Expr a_Expr b_Expr)
min_ = mk_easy_FunMonoBind min_PN [a_Pat, b_Pat] [] (
	    compare_Case a_Expr b_Expr b_Expr a_Expr b_Expr)
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
  = enum_from `AndMonoBinds` enum_from_then
  where
    enum_from
      = mk_easy_FunMonoBind enumFrom_PN [a_Pat] [] $
	  untag_Expr tycon [(a_PN, ah_PN)] $
	  HsApp (mk_easy_App map_PN [tag2con_PN tycon]) $
	    HsPar (enum_from_to_Expr
		    (mk_easy_App mkInt_PN [ah_PN])
		    (HsVar (maxtag_PN tycon)))

    enum_from_then
      = mk_easy_FunMonoBind enumFromThen_PN [a_Pat, b_Pat] [] $
	  untag_Expr tycon [(a_PN, ah_PN), (b_PN, bh_PN)] $
	  HsApp (mk_easy_App map_PN [tag2con_PN tycon]) $
	    HsPar (enum_from_then_to_Expr
		    (mk_easy_App mkInt_PN [ah_PN])
		    (mk_easy_App mkInt_PN [bh_PN])
		    (HsVar (maxtag_PN tycon)))
\end{code}

%************************************************************************
%*									*
\subsubsection{Generating @Eval@ instance declarations}
%*									*
%************************************************************************

\begin{code}
gen_Eval_binds tycon = EmptyMonoBinds
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
	ASSERT(length data_cons == 1)
	min_bound_1con `AndMonoBinds` max_bound_1con
  where
    data_cons     = tyConDataCons tycon

    ----- enum-flavored: ---------------------------
    min_bound_enum = mk_easy_FunMonoBind minBound_PN [] [] (HsVar data_con_1_PN)
    max_bound_enum = mk_easy_FunMonoBind maxBound_PN [] [] (HsVar data_con_N_PN)

    data_con_1	  = head data_cons
    data_con_N	  = last data_cons
    data_con_1_PN = origName data_con_1
    data_con_N_PN = origName data_con_N

    ----- single-constructor-flavored: -------------
    arity	   = dataConArity data_con_1

    min_bound_1con = mk_easy_FunMonoBind minBound_PN [] [] $
		     mk_easy_App data_con_1_PN (nOfThem arity minBound_PN)
    max_bound_1con = mk_easy_FunMonoBind maxBound_PN [] [] $
		     mk_easy_App data_con_1_PN (nOfThem arity maxBound_PN)
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
(modulo suitable case-ification to handle the unboxed tags)

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
    tycon_str = _UNPK_ (snd (moduleNamePair tycon))

    --------------------------------------------------------------
    enum_ixes = enum_range `AndMonoBinds`
    	    	enum_index `AndMonoBinds` enum_inRange

    enum_range
      = mk_easy_FunMonoBind range_PN [TuplePatIn [a_Pat, b_Pat]] [] $
	  untag_Expr tycon [(a_PN, ah_PN)] $
	  untag_Expr tycon [(b_PN, bh_PN)] $
	  HsApp (mk_easy_App map_PN [tag2con_PN tycon]) $
	      HsPar (enum_from_to_Expr
			(mk_easy_App mkInt_PN [ah_PN])
			(mk_easy_App mkInt_PN [bh_PN]))

    enum_index
      = mk_easy_FunMonoBind index_PN [AsPatIn c_PN (TuplePatIn [a_Pat, b_Pat]), d_Pat] [] (
	HsIf (HsPar (mk_easy_App inRange_PN [c_PN, d_PN])) (
	   untag_Expr tycon [(a_PN, ah_PN)] (
	   untag_Expr tycon [(d_PN, dh_PN)] (
	   let
		grhs = [OtherwiseGRHS (mk_easy_App mkInt_PN [c_PN]) mkGeneratedSrcLoc]
	   in
	   HsCase
	     (HsPar (OpApp (HsVar dh_PN) (HsVar minusH_PN) (HsVar ah_PN)))
	     [PatMatch (VarPatIn c_PN)
				(GRHSMatch (GRHSsAndBindsIn grhs EmptyBinds))]
	     mkGeneratedSrcLoc
	   ))
	) {-else-} (
	   HsApp (HsVar error_PN) (HsLit (HsString (_PK_ ("Ix."++tycon_str++".index: out of range\n"))))
	)
	mkGeneratedSrcLoc)

    enum_inRange
      = mk_easy_FunMonoBind inRange_PN [TuplePatIn [a_Pat, b_Pat], c_Pat] [] (
	  untag_Expr tycon [(a_PN, ah_PN)] (
	  untag_Expr tycon [(b_PN, bh_PN)] (
	  untag_Expr tycon [(c_PN, ch_PN)] (
	  HsIf (HsPar (OpApp (HsVar ch_PN) (HsVar geH_PN) (HsVar ah_PN))) (
	     (OpApp (HsVar ch_PN) (HsVar leH_PN) (HsVar bh_PN))
	  ) {-else-} (
	     false_Expr
	  ) mkGeneratedSrcLoc))))

    --------------------------------------------------------------
    single_con_ixes = single_con_range `AndMonoBinds`
    	    	single_con_index `AndMonoBinds` single_con_inRange

    data_con
      =	case maybeTyConSingleCon tycon of -- just checking...
	  Nothing -> panic "get_Ix_binds"
	  Just dc -> if (any isPrimType (dataConRawArgTys dc)) then
			 error ("ERROR: Can't derive Ix for a single-constructor type with primitive argument types: "++tycon_str)
		     else
			 dc

    con_arity   = dataConArity data_con
    data_con_PN = origName data_con
    con_pat  xs = ConPatIn data_con_PN (map VarPatIn xs)
    con_expr xs = mk_easy_App data_con_PN xs

    as_needed = take con_arity as_PNs
    bs_needed = take con_arity bs_PNs
    cs_needed = take con_arity cs_PNs

    --------------------------------------------------------------
    single_con_range
      = mk_easy_FunMonoBind range_PN [TuplePatIn [con_pat as_needed, con_pat bs_needed]] [] (
	  ListComp (con_expr cs_needed) (zipWith3Equal "single_con_range" mk_qual as_needed bs_needed cs_needed)
	)
      where
	mk_qual a b c = GeneratorQual (VarPatIn c)
			    (HsApp (HsVar range_PN) (ExplicitTuple [HsVar a, HsVar b]))

    ----------------
    single_con_index
      = mk_easy_FunMonoBind index_PN [TuplePatIn [con_pat as_needed, con_pat bs_needed], con_pat cs_needed] [range_size] (
	foldl mk_index (HsLit (HsInt 0)) (zip3 as_needed bs_needed cs_needed))
      where
	mk_index multiply_by (l, u, i)
	  =OpApp (
		(HsApp (HsApp (HsVar index_PN) (ExplicitTuple [HsVar l, HsVar u])) (HsVar i))
	   ) (HsVar plus_PN) (
		OpApp (
		    (HsApp (HsVar rangeSize_PN) (ExplicitTuple [HsVar l, HsVar u]))
		) (HsVar times_PN) multiply_by
	   )

	range_size
	  = mk_easy_FunMonoBind rangeSize_PN [TuplePatIn [a_Pat, b_Pat]] [] (
		OpApp (
		    (HsApp (HsApp (HsVar index_PN) (ExplicitTuple [a_Expr, b_Expr])) b_Expr)
		) (HsVar plus_PN) (HsLit (HsInt 1)))

    ------------------
    single_con_inRange
      = mk_easy_FunMonoBind inRange_PN [TuplePatIn [con_pat as_needed, con_pat bs_needed], con_pat cs_needed] [] (
	  foldl1 and_Expr (zipWith3Equal "single_con_inRange" in_range as_needed bs_needed cs_needed))
      where
    	in_range a b c = HsApp (HsApp (HsVar inRange_PN) (ExplicitTuple [HsVar a, HsVar b])) (HsVar c)
\end{code}

%************************************************************************
%*									*
\subsubsection{Generating @Read@ instance declarations}
%*									*
%************************************************************************

Ignoring all the infix-ery mumbo jumbo (ToDo)

\begin{code}
gen_Read_binds :: [RenamedFixityDecl] -> TyCon -> RdrNameMonoBinds

gen_Read_binds fixities tycon
  = reads_prec `AndMonoBinds` read_list
  where
    -----------------------------------------------------------------------
    read_list = mk_easy_FunMonoBind readList_PN [] []
		  (HsApp (HsVar _readList_PN) (HsPar (HsApp (HsVar readsPrec_PN) (HsLit (HsInt 0)))))
    -----------------------------------------------------------------------
    reads_prec
      = let
	    read_con_comprehensions
	      = map read_con (tyConDataCons tycon)
	in
	mk_easy_FunMonoBind readsPrec_PN [a_Pat, b_Pat] [] (
	      foldl1 append_Expr read_con_comprehensions
	)
      where
	read_con data_con   -- note: "b" is the string being "read"
	  = let
		data_con_PN = origName data_con
		data_con_str= snd  (moduleNamePair data_con)
		con_arity   = dataConArity data_con
		as_needed   = take con_arity as_PNs
		bs_needed   = take con_arity bs_PNs
		con_expr    = mk_easy_App data_con_PN as_needed
		nullary_con = isNullaryDataCon data_con

		con_qual
		  = GeneratorQual
		      (TuplePatIn [LitPatIn (HsString data_con_str), d_Pat])
		      (HsApp (HsVar lex_PN) c_Expr)

		field_quals = snd (mapAccumL mk_qual d_Expr (zipEqual "as_needed" as_needed bs_needed))

		read_paren_arg
		  = if nullary_con then -- must be False (parens are surely optional)
		       false_Expr
		    else -- parens depend on precedence...
		       HsPar (OpApp a_Expr (HsVar gt_PN) (HsLit (HsInt 9)))
	    in
	    HsApp (
	      readParen_Expr read_paren_arg $ HsPar $
		 HsLam (mk_easy_Match [c_Pat] []  (
		   ListComp (ExplicitTuple [con_expr,
			    if null bs_needed then d_Expr else HsVar (last bs_needed)])
		    (con_qual : field_quals)))
	      ) (HsVar b_PN)
	  where
	    mk_qual draw_from (con_field, str_left)
	      = (HsVar str_left,	-- what to draw from down the line...
		 GeneratorQual
		  (TuplePatIn [VarPatIn con_field, VarPatIn str_left])
		  (HsApp (HsApp (HsVar readsPrec_PN) (HsLit (HsInt 10))) draw_from))
\end{code}

%************************************************************************
%*									*
\subsubsection{Generating @Show@ instance declarations}
%*									*
%************************************************************************

Ignoring all the infix-ery mumbo jumbo (ToDo)

\begin{code}
gen_Show_binds :: [RenamedFixityDecl] -> TyCon -> RdrNameMonoBinds

gen_Show_binds fixities tycon
  = shows_prec `AndMonoBinds` show_list
  where
    -----------------------------------------------------------------------
    show_list = mk_easy_FunMonoBind showList_PN [] []
		  (HsApp (HsVar _showList_PN) (HsPar (HsApp (HsVar showsPrec_PN) (HsLit (HsInt 0)))))
    -----------------------------------------------------------------------
    shows_prec
      = mk_FunMonoBind showsPrec_PN (map pats_etc (tyConDataCons tycon))
      where
	pats_etc data_con
	  = let
		data_con_PN = origName data_con
		con_arity   = dataConArity data_con
		bs_needed   = take con_arity bs_PNs
		con_pat     = ConPatIn data_con_PN (map VarPatIn bs_needed)
		nullary_con = isNullaryDataCon data_con

		show_con
		  = let (mod, nm)   = moduleNamePair data_con
			space_maybe = if nullary_con then _NIL_ else SLIT(" ")
		    in
			HsApp (HsVar showString_PN) (HsLit (HsString (nm _APPEND_ space_maybe)))

		show_thingies = show_con : (spacified real_show_thingies)

		real_show_thingies
		  = [ HsApp (HsApp (HsVar showsPrec_PN) (HsLit (HsInt 10))) (HsVar b)
		  | b <- bs_needed ]
	    in
	    if nullary_con then  -- skip the showParen junk...
		ASSERT(null bs_needed)
		([a_Pat, con_pat], show_con)
	    else
		([a_Pat, con_pat],
		    showParen_Expr (HsPar (OpApp a_Expr (HsVar ge_PN) (HsLit (HsInt 10))))
				   (HsPar (nested_compose_Expr show_thingies)))
	  where
	    spacified []     = []
	    spacified [x]    = [x]
	    spacified (x:xs) = (x : (HsVar showSpace_PN) : spacified xs)
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
maxtag_Foo  :: Int		-- ditto (NB: not unboxed)
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

gen_tag_n_con_monobind (pn, tycon, GenCon2Tag)
  = mk_FunMonoBind pn (map mk_stuff (tyConDataCons tycon))
  where
    mk_stuff :: DataCon -> ([RdrNamePat], RdrNameHsExpr)

    mk_stuff var
      = ASSERT(isDataCon var)
	([pat], HsLit (HsIntPrim (toInteger ((dataConTag var) - fIRST_TAG))))
      where
	pat    = ConPatIn var_PN (nOfThem (dataConArity var) WildPatIn)
	var_PN = origName var

gen_tag_n_con_monobind (pn, tycon, GenTag2Con)
  = mk_FunMonoBind pn (map mk_stuff (tyConDataCons tycon))
  where
    mk_stuff :: DataCon -> ([RdrNamePat], RdrNameHsExpr)

    mk_stuff var
      = ASSERT(isDataCon var)
	([lit_pat], HsVar var_PN)
      where
	lit_pat = ConPatIn mkInt_PN [LitPatIn (HsIntPrim (toInteger ((dataConTag var) - fIRST_TAG)))]
	var_PN  = origName var

gen_tag_n_con_monobind (pn, tycon, GenMaxTag)
  = mk_easy_FunMonoBind pn [] [] (HsApp (HsVar mkInt_PN) (HsLit (HsIntPrim max_tag)))
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
mk_easy_FunMonoBind :: RdrName -> [RdrNamePat]
		    -> [RdrNameMonoBinds] -> RdrNameHsExpr
		    -> RdrNameMonoBinds

mk_easy_FunMonoBind fun pats binds expr
  = FunMonoBind fun False{-not infix-} [mk_easy_Match pats binds expr] mkGeneratedSrcLoc

mk_easy_Match pats binds expr
  = mk_match pats expr (mkbind binds)
  where
    mkbind [] = EmptyBinds
    mkbind bs = SingleBind (RecBind (foldr1 AndMonoBinds bs))
	-- The renamer expects everything in its input to be a
	-- "recursive" MonoBinds, and it is its job to sort things out
	-- from there.

mk_FunMonoBind	:: RdrName
		-> [([RdrNamePat], RdrNameHsExpr)]
		-> RdrNameMonoBinds

mk_FunMonoBind fun [] = panic "TcGenDeriv:mk_FunMonoBind"
mk_FunMonoBind fun pats_and_exprs
  = FunMonoBind fun False{-not infix-}
		[ mk_match p e EmptyBinds | (p,e) <-pats_and_exprs ]
		mkGeneratedSrcLoc

mk_match pats expr binds
  = foldr PatMatch
	  (GRHSMatch (GRHSsAndBindsIn [OtherwiseGRHS expr mkGeneratedSrcLoc] binds))
	  (map paren pats)
  where
    paren p@(VarPatIn _) = p
    paren other_p	 = ParPatIn other_p
\end{code}

\begin{code}
mk_easy_App f xs = foldl HsApp (HsVar f) (map HsVar xs)
\end{code}

\begin{code}
compare_Case, cmp_eq_Expr ::
	  RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr
	  -> RdrNameHsExpr -> RdrNameHsExpr
	  -> RdrNameHsExpr
compare_gen_Case ::
	  RdrName
	  -> RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr
	  -> RdrNameHsExpr -> RdrNameHsExpr
	  -> RdrNameHsExpr
careful_compare_Case :: -- checks for primitive types...
	  Type
	  -> RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr
	  -> RdrNameHsExpr -> RdrNameHsExpr
	  -> RdrNameHsExpr

compare_Case = compare_gen_Case compare_PN
cmp_eq_Expr = compare_gen_Case cmp_eq_PN

compare_gen_Case fun lt eq gt a b
  = HsCase (HsPar (HsApp (HsApp (HsVar fun) a) b)) {-of-}
      [PatMatch (ConPatIn ltTag_PN [])
	  (GRHSMatch (GRHSsAndBindsIn [OtherwiseGRHS lt mkGeneratedSrcLoc] EmptyBinds)),

       PatMatch (ConPatIn eqTag_PN [])
	  (GRHSMatch (GRHSsAndBindsIn [OtherwiseGRHS eq mkGeneratedSrcLoc] EmptyBinds)),

       PatMatch (ConPatIn gtTag_PN [])
	  (GRHSMatch (GRHSsAndBindsIn [OtherwiseGRHS gt mkGeneratedSrcLoc] EmptyBinds))]
       mkGeneratedSrcLoc

careful_compare_Case ty lt eq gt a b
  = if not (isPrimType ty) then
       compare_gen_Case compare_PN lt eq gt a b

    else -- we have to do something special for primitive things...
       HsIf (HsPar (OpApp a (HsVar relevant_eq_op) b))
	    eq
	    (HsIf (HsPar (OpApp a (HsVar relevant_lt_op) b)) lt gt mkGeneratedSrcLoc)
	    mkGeneratedSrcLoc
  where
    relevant_eq_op = assoc_ty_id eq_op_tbl ty
    relevant_lt_op = assoc_ty_id lt_op_tbl ty

assoc_ty_id tyids ty 
  = if null res then panic "assoc_ty"
    else head res
  where
    res = [id | (ty',id) <- tyids, eqTy ty ty']

eq_op_tbl =
    [(charPrimTy,	eqH_Char_PN)
    ,(intPrimTy,	eqH_Int_PN)
    ,(wordPrimTy,	eqH_Word_PN)
    ,(addrPrimTy,	eqH_Addr_PN)
    ,(floatPrimTy,	eqH_Float_PN)
    ,(doublePrimTy,	eqH_Double_PN)
    ]

lt_op_tbl =
    [(charPrimTy,	ltH_Char_PN)
    ,(intPrimTy,	ltH_Int_PN)
    ,(wordPrimTy,	ltH_Word_PN)
    ,(addrPrimTy,	ltH_Addr_PN)
    ,(floatPrimTy,	ltH_Float_PN)
    ,(doublePrimTy,	ltH_Double_PN)
    ]

-----------------------------------------------------------------------

and_Expr, append_Expr :: RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr

and_Expr    a b = OpApp a (HsVar and_PN)    b
append_Expr a b = OpApp a (HsVar append_PN) b

-----------------------------------------------------------------------

eq_Expr :: Type -> RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr
eq_Expr ty a b
  = if not (isPrimType ty) then
       OpApp a (HsVar eq_PN)  b
    else -- we have to do something special for primitive things...
       OpApp a (HsVar relevant_eq_op) b
  where
    relevant_eq_op = assoc_ty_id eq_op_tbl ty
\end{code}

\begin{code}
untag_Expr :: TyCon -> [(RdrName, RdrName)] -> RdrNameHsExpr -> RdrNameHsExpr
untag_Expr tycon [] expr = expr
untag_Expr tycon ((untag_this, put_tag_here) : more) expr
  = HsCase (HsPar (HsApp (con2tag_Expr tycon) (HsVar untag_this))) {-of-}
      [PatMatch (VarPatIn put_tag_here)
			(GRHSMatch (GRHSsAndBindsIn grhs EmptyBinds))]
      mkGeneratedSrcLoc
  where
    grhs = [OtherwiseGRHS (untag_Expr tycon more expr) mkGeneratedSrcLoc]

cmp_tags_Expr :: RdrName 		-- Comparison op
	     -> RdrName -> RdrName	-- Things to compare
	     -> RdrNameHsExpr 		-- What to return if true
	     -> RdrNameHsExpr		-- What to return if false
	     -> RdrNameHsExpr

cmp_tags_Expr op a b true_case false_case
  = HsIf (HsPar (OpApp (HsVar a) (HsVar op) (HsVar b))) true_case false_case mkGeneratedSrcLoc

enum_from_to_Expr
	:: RdrNameHsExpr -> RdrNameHsExpr
	-> RdrNameHsExpr
enum_from_then_to_Expr
	:: RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr
	-> RdrNameHsExpr

enum_from_to_Expr      f   t2 = HsApp (HsApp (HsVar enumFromTo_PN) f) t2
enum_from_then_to_Expr f t t2 = HsApp (HsApp (HsApp (HsVar enumFromThenTo_PN) f) t) t2

showParen_Expr, readParen_Expr
	:: RdrNameHsExpr -> RdrNameHsExpr
	-> RdrNameHsExpr

showParen_Expr e1 e2 = HsApp (HsApp (HsVar showParen_PN) e1) e2
readParen_Expr e1 e2 = HsApp (HsApp (HsVar readParen_PN) e1) e2

nested_compose_Expr :: [RdrNameHsExpr] -> RdrNameHsExpr

nested_compose_Expr [e] = parenify e
nested_compose_Expr (e:es)
  = HsApp (HsApp (HsVar compose_PN) (parenify e)) (nested_compose_Expr es)

parenify e@(HsVar _) = e
parenify e	     = HsPar e
\end{code}

\begin{code}
a_PN		= Unqual SLIT("a")
b_PN		= Unqual SLIT("b")
c_PN		= Unqual SLIT("c")
d_PN		= Unqual SLIT("d")
ah_PN		= Unqual SLIT("a#")
bh_PN		= Unqual SLIT("b#")
ch_PN		= Unqual SLIT("c#")
dh_PN		= Unqual SLIT("d#")
cmp_eq_PN	= Unqual SLIT("cmp_eq")
rangeSize_PN	= Unqual SLIT("rangeSize")

as_PNs		= [ Unqual (_PK_ ("a"++show i)) | i <- [(1::Int) .. ] ]
bs_PNs		= [ Unqual (_PK_ ("b"++show i)) | i <- [(1::Int) .. ] ]
cs_PNs		= [ Unqual (_PK_ ("c"++show i)) | i <- [(1::Int) .. ] ]

eq_PN		= prelude_method SLIT("Eq")  SLIT("==")
ne_PN		= prelude_method SLIT("Eq")  SLIT("/=")
le_PN		= prelude_method SLIT("Ord") SLIT("<=")
lt_PN		= prelude_method SLIT("Ord") SLIT("<")
ge_PN		= prelude_method SLIT("Ord") SLIT(">=")
gt_PN		= prelude_method SLIT("Ord") SLIT(">")
max_PN		= prelude_method SLIT("Ord") SLIT("max")
min_PN		= prelude_method SLIT("Ord") SLIT("min")
compare_PN	= prelude_method SLIT("Ord") SLIT("compare")
minBound_PN	= prelude_method SLIT("Bounded") SLIT("minBound")
maxBound_PN	= prelude_method SLIT("Bounded") SLIT("maxBound")
ltTag_PN	= Unqual SLIT("LT")
eqTag_PN	= Unqual SLIT("EQ")
gtTag_PN	= Unqual SLIT("GT")
enumFrom_PN	 = prelude_method SLIT("Enum") SLIT("enumFrom")
enumFromTo_PN	 = prelude_method SLIT("Enum") SLIT("enumFromTo")
enumFromThen_PN	 = prelude_method SLIT("Enum") SLIT("enumFromThen")
enumFromThenTo_PN= prelude_method SLIT("Enum") SLIT("enumFromThenTo")
range_PN	 = prelude_method SLIT("Ix")   SLIT("range")
index_PN	 = prelude_method SLIT("Ix")   SLIT("index")
inRange_PN	 = prelude_method SLIT("Ix")   SLIT("inRange")
readsPrec_PN	 = prelude_method SLIT("Read") SLIT("readsPrec")
readList_PN	 = prelude_method SLIT("Read") SLIT("readList")
showsPrec_PN	 = prelude_method SLIT("Show") SLIT("showsPrec")
showList_PN	 = prelude_method SLIT("Show") SLIT("showList")
plus_PN		 = prelude_method SLIT("Num")  SLIT("+")
times_PN	 = prelude_method SLIT("Num")  SLIT("*")

false_PN	= prelude_val pRELUDE SLIT("False")
true_PN		= prelude_val pRELUDE SLIT("True")
eqH_Char_PN	= prelude_primop CharEqOp
ltH_Char_PN	= prelude_primop CharLtOp
eqH_Word_PN	= prelude_primop WordEqOp
ltH_Word_PN	= prelude_primop WordLtOp
eqH_Addr_PN	= prelude_primop AddrEqOp
ltH_Addr_PN	= prelude_primop AddrLtOp
eqH_Float_PN	= prelude_primop FloatEqOp
ltH_Float_PN	= prelude_primop FloatLtOp
eqH_Double_PN	= prelude_primop DoubleEqOp
ltH_Double_PN	= prelude_primop DoubleLtOp
eqH_Int_PN	= prelude_primop IntEqOp
ltH_Int_PN	= prelude_primop IntLtOp
geH_PN		= prelude_primop IntGeOp
leH_PN		= prelude_primop IntLeOp
minusH_PN	= prelude_primop IntSubOp
and_PN		= prelude_val pRELUDE     SLIT("&&")
not_PN		= prelude_val pRELUDE     SLIT("not")
append_PN	= prelude_val pRELUDE_LIST SLIT("++")
map_PN		= prelude_val pRELUDE_LIST SLIT("map")
compose_PN	= prelude_val pRELUDE     SLIT(".")
mkInt_PN	= prelude_val pRELUDE_BUILTIN SLIT("I#")
error_PN	= prelude_val pRELUDE SLIT("error")
showString_PN	= prelude_val pRELUDE_TEXT SLIT("showString")
showParen_PN	= prelude_val pRELUDE_TEXT SLIT("showParen")
readParen_PN	= prelude_val pRELUDE_TEXT SLIT("readParen")
lex_PN		= prelude_val pRELUDE_TEXT SLIT("lex")
showSpace_PN	= prelude_val pRELUDE_TEXT SLIT("__showSpace")
_showList_PN    = prelude_val pRELUDE SLIT("__showList")
_readList_PN    = prelude_val pRELUDE SLIT("__readList")

prelude_val    m s = Unqual s
prelude_method c o = Unqual o
prelude_primop   o = origName (primOpId o)

a_Expr		= HsVar a_PN
b_Expr		= HsVar b_PN
c_Expr		= HsVar c_PN
d_Expr		= HsVar d_PN
ltTag_Expr	= HsVar ltTag_PN
eqTag_Expr	= HsVar eqTag_PN
gtTag_Expr	= HsVar gtTag_PN
false_Expr	= HsVar false_PN
true_Expr	= HsVar true_PN

con2tag_Expr tycon = HsVar (con2tag_PN tycon)

a_Pat		= VarPatIn a_PN
b_Pat		= VarPatIn b_PN
c_Pat		= VarPatIn c_PN
d_Pat		= VarPatIn d_PN

con2tag_PN, tag2con_PN, maxtag_PN :: TyCon -> RdrName

con2tag_PN tycon
  = let	(mod, nm) = moduleNamePair tycon
	con2tag	  = SLIT("con2tag_") _APPEND_ nm _APPEND_ SLIT("#")
    in
    (if fromPrelude mod then Unqual else Qual mod) con2tag

tag2con_PN tycon
  = let	(mod, nm) = moduleNamePair tycon
	tag2con	  = SLIT("tag2con_") _APPEND_ nm _APPEND_ SLIT("#")
    in
    (if fromPrelude mod then Unqual else Qual mod) tag2con

maxtag_PN tycon
  = let	(mod, nm) = moduleNamePair tycon
	maxtag	  = SLIT("maxtag_") _APPEND_ nm _APPEND_ SLIT("#")
    in
    (if fromPrelude mod then Unqual else Qual mod) maxtag
\end{code}
