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
	gen_Bounded_binds,
	gen_Enum_binds,
	gen_Eval_binds,
	gen_Eq_binds,
	gen_Ix_binds,
	gen_Ord_binds,
	gen_Read_binds,
	gen_Show_binds,
	gen_tag_n_con_monobind,

	con2tag_RDR, tag2con_RDR, maxtag_RDR,

	TagThingWanted(..)
    ) where

IMP_Ubiq()
IMPORT_1_3(List(partition))

import HsSyn		( HsBinds(..), MonoBinds(..), Match(..), GRHSsAndBinds(..),
			  GRHS(..), HsExpr(..), HsLit(..), InPat(..), Stmt(..), DoOrListComp(..),
			  SYN_IE(RecFlag), recursive,
			  ArithSeqInfo, Sig, HsType, FixityDecl, Fixity, Fake )
import RdrHsSyn		( RdrName(..), varQual, varUnqual, mkOpApp,
			  SYN_IE(RdrNameMonoBinds), SYN_IE(RdrNameHsExpr), SYN_IE(RdrNamePat)
			)
import BasicTypes	( IfaceFlavour(..) )
import Id		( GenId, isNullaryDataCon, dataConTag,
			  dataConRawArgTys, fIRST_TAG,
			  isDataCon, SYN_IE(DataCon), SYN_IE(ConTag),
			  SYN_IE(Id) )
import Maybes		( maybeToBool )
import Name		( getOccString, getOccName, getSrcLoc, occNameString, modAndOcc, OccName, Name )

import PrimOp		( PrimOp(..) )
import PrelInfo		-- Lots of RdrNames
import SrcLoc		( mkGeneratedSrcLoc, SrcLoc )
import TyCon		( TyCon, isNewTyCon, tyConDataCons, isEnumerationTyCon, maybeTyConSingleCon )
import Type		( eqTy, isPrimType, SYN_IE(Type) )
import TysPrim		( charPrimTy, intPrimTy, wordPrimTy, addrPrimTy,
			  floatPrimTy, doublePrimTy
			)
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
	tycon_loc = getSrcLoc tycon
        (nullary_cons, nonnullary_cons)
           | isNewTyCon tycon = ([], tyConDataCons tycon)
           | otherwise	      = partition isNullaryDataCon (tyConDataCons tycon)

	rest
	  = if (null nullary_cons) then
		case maybeTyConSingleCon tycon of
		  Just _ -> []
		  Nothing -> -- if cons don't match, then False
		     [([a_Pat, b_Pat], false_Expr)]
	    else -- calc. and compare the tags
		 [([a_Pat, b_Pat],
		    untag_Expr tycon [(a_RDR,ah_RDR), (b_RDR,bh_RDR)]
		      (cmp_tags_Expr eqH_Int_RDR ah_RDR bh_RDR true_Expr false_Expr))]
    in
    mk_FunMonoBind tycon_loc eq_RDR ((map pats_etc nonnullary_cons) ++ rest)
	    `AndMonoBinds`
    mk_easy_FunMonoBind tycon_loc ne_RDR [a_Pat, b_Pat] [] (
	HsApp (HsVar not_RDR) (HsPar (mk_easy_App eq_RDR [a_RDR, b_RDR])))
  where
    ------------------------------------------------------------------
    pats_etc data_con
      = let
	    con1_pat = ConPatIn data_con_RDR (map VarPatIn as_needed)
	    con2_pat = ConPatIn data_con_RDR (map VarPatIn bs_needed)

	    data_con_RDR = qual_orig_name data_con
	    con_arity   = length tys_needed
	    as_needed   = take con_arity as_RDRs
	    bs_needed   = take con_arity bs_RDRs
	    tys_needed  = dataConRawArgTys data_con
	in
	([con1_pat, con2_pat], nested_eq_expr tys_needed as_needed bs_needed)
      where
	nested_eq_expr []  [] [] = true_Expr
	nested_eq_expr tys as bs
	  = foldl1 and_Expr (zipWith3Equal "nested_eq" nested_eq tys as bs)
	  where
	    nested_eq ty a b = HsPar (eq_Expr ty (HsVar a) (HsVar b))
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
    tycon_loc = getSrcLoc tycon
    --------------------------------------------------------------------
    compare = mk_easy_FunMonoBind tycon_loc compare_RDR
		[a_Pat, b_Pat]
		[cmp_eq]
	    (if maybeToBool (maybeTyConSingleCon tycon) then
		cmp_eq_Expr ltTag_Expr eqTag_Expr gtTag_Expr a_Expr b_Expr
	     else
		untag_Expr tycon [(a_RDR, ah_RDR), (b_RDR, bh_RDR)]
		  (cmp_tags_Expr eqH_Int_RDR ah_RDR bh_RDR
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
		    (cmp_tags_Expr ltH_Int_RDR ah_RDR bh_RDR ltTag_Expr gtTag_Expr)))

    (nullary_cons, nonnullary_cons)
       | isNewTyCon tycon = ([], tyConDataCons tycon)
       | otherwise	  = partition isNullaryDataCon (tyConDataCons tycon)

    cmp_eq
      = mk_FunMonoBind tycon_loc cmp_eq_RDR (map pats_etc nonnullary_cons ++
					     [([WildPatIn, WildPatIn], default_rhs)])
      where
	pats_etc data_con
	  = ([con1_pat, con2_pat],
	     nested_compare_expr tys_needed as_needed bs_needed)
	  where
	    con1_pat = ConPatIn data_con_RDR (map VarPatIn as_needed)
	    con2_pat = ConPatIn data_con_RDR (map VarPatIn bs_needed)

	    data_con_RDR = qual_orig_name data_con
	    con_arity   = length tys_needed
	    as_needed   = take con_arity as_RDRs
	    bs_needed   = take con_arity bs_RDRs
	    tys_needed  = dataConRawArgTys data_con

	    nested_compare_expr [ty] [a] [b]
	      = careful_compare_Case ty ltTag_Expr eqTag_Expr gtTag_Expr (HsVar a) (HsVar b)

	    nested_compare_expr (ty:tys) (a:as) (b:bs)
	      = let eq_expr = nested_compare_expr tys as bs
		in  careful_compare_Case ty ltTag_Expr eq_expr gtTag_Expr (HsVar a) (HsVar b)

	default_rhs | null nullary_cons = impossible_Expr	-- Keep desugarer from complaining about
								-- inexhaustive patterns
		    | otherwise		= eqTag_Expr		-- Some nullary constructors;
								-- Tags are equal, no args => return EQ
    --------------------------------------------------------------------

defaulted = foldr1 AndMonoBinds [lt, le, ge, gt, max_, min_]

lt = mk_easy_FunMonoBind mkGeneratedSrcLoc lt_RDR [a_Pat, b_Pat] [] (
	    compare_Case true_Expr  false_Expr false_Expr a_Expr b_Expr)
le = mk_easy_FunMonoBind mkGeneratedSrcLoc le_RDR [a_Pat, b_Pat] [] (
	    compare_Case true_Expr  true_Expr  false_Expr a_Expr b_Expr)
ge = mk_easy_FunMonoBind mkGeneratedSrcLoc ge_RDR [a_Pat, b_Pat] [] (
	    compare_Case false_Expr true_Expr  true_Expr  a_Expr b_Expr)
gt = mk_easy_FunMonoBind mkGeneratedSrcLoc gt_RDR [a_Pat, b_Pat] [] (
	    compare_Case false_Expr false_Expr true_Expr  a_Expr b_Expr)

max_ = mk_easy_FunMonoBind mkGeneratedSrcLoc max_RDR [a_Pat, b_Pat] [] (
	    compare_Case b_Expr a_Expr a_Expr a_Expr b_Expr)
min_ = mk_easy_FunMonoBind mkGeneratedSrcLoc min_RDR [a_Pat, b_Pat] [] (
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
  = to_enum             `AndMonoBinds`
    enum_from		`AndMonoBinds`
    enum_from_then	`AndMonoBinds`
    from_enum
  where
    tycon_loc = getSrcLoc tycon

    to_enum
      = mk_easy_FunMonoBind tycon_loc toEnum_RDR [a_Pat] [] $
        mk_easy_App (tag2con_RDR tycon) [a_RDR]

    enum_from
      = mk_easy_FunMonoBind tycon_loc enumFrom_RDR [a_Pat] [] $
	  untag_Expr tycon [(a_RDR, ah_RDR)] $
	  HsApp (mk_easy_App map_RDR [tag2con_RDR tycon]) $
	    HsPar (enum_from_to_Expr
		    (mk_easy_App mkInt_RDR [ah_RDR])
		    (HsVar (maxtag_RDR tycon)))

    enum_from_then
      = mk_easy_FunMonoBind tycon_loc enumFromThen_RDR [a_Pat, b_Pat] [] $
	  untag_Expr tycon [(a_RDR, ah_RDR), (b_RDR, bh_RDR)] $
	  HsApp (mk_easy_App map_RDR [tag2con_RDR tycon]) $
	    HsPar (enum_from_then_to_Expr
		    (mk_easy_App mkInt_RDR [ah_RDR])
		    (mk_easy_App mkInt_RDR [bh_RDR])
		    (HsVar (maxtag_RDR tycon)))

    from_enum
      = mk_easy_FunMonoBind tycon_loc fromEnum_RDR [a_Pat] [] $
	  untag_Expr tycon [(a_RDR, ah_RDR)] $
	  (mk_easy_App mkInt_RDR [ah_RDR])
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
    data_cons = tyConDataCons tycon
    tycon_loc = getSrcLoc tycon

    ----- enum-flavored: ---------------------------
    min_bound_enum = mk_easy_FunMonoBind tycon_loc minBound_RDR [] [] (HsVar data_con_1_RDR)
    max_bound_enum = mk_easy_FunMonoBind tycon_loc maxBound_RDR [] [] (HsVar data_con_N_RDR)

    data_con_1	  = head data_cons
    data_con_N	  = last data_cons
    data_con_1_RDR = qual_orig_name data_con_1
    data_con_N_RDR = qual_orig_name data_con_N

    ----- single-constructor-flavored: -------------
    arity	   = argFieldCount data_con_1

    min_bound_1con = mk_easy_FunMonoBind tycon_loc minBound_RDR [] [] $
		     mk_easy_App data_con_1_RDR (nOfThem arity minBound_RDR)
    max_bound_1con = mk_easy_FunMonoBind tycon_loc maxBound_RDR [] [] $
		     mk_easy_App data_con_1_RDR (nOfThem arity maxBound_RDR)
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
    tycon_str = getOccString tycon
    tycon_loc = getSrcLoc tycon

    --------------------------------------------------------------
    enum_ixes = enum_range `AndMonoBinds`
    	    	enum_index `AndMonoBinds` enum_inRange

    enum_range
      = mk_easy_FunMonoBind tycon_loc range_RDR [TuplePatIn [a_Pat, b_Pat]] [] $
	  untag_Expr tycon [(a_RDR, ah_RDR)] $
	  untag_Expr tycon [(b_RDR, bh_RDR)] $
	  HsApp (mk_easy_App map_RDR [tag2con_RDR tycon]) $
	      HsPar (enum_from_to_Expr
			(mk_easy_App mkInt_RDR [ah_RDR])
			(mk_easy_App mkInt_RDR [bh_RDR]))

    enum_index
      = mk_easy_FunMonoBind tycon_loc index_RDR [AsPatIn c_RDR (TuplePatIn [a_Pat, b_Pat]), d_Pat] [] (
	HsIf (HsPar (mk_easy_App inRange_RDR [c_RDR, d_RDR])) (
	   untag_Expr tycon [(a_RDR, ah_RDR)] (
	   untag_Expr tycon [(d_RDR, dh_RDR)] (
	   let
		grhs = [OtherwiseGRHS (mk_easy_App mkInt_RDR [c_RDR]) tycon_loc]
	   in
	   HsCase
	     (genOpApp (HsVar dh_RDR) minusH_RDR (HsVar ah_RDR))
	     [PatMatch (VarPatIn c_RDR)
				(GRHSMatch (GRHSsAndBindsIn grhs EmptyBinds))]
	     tycon_loc
	   ))
	) {-else-} (
	   HsApp (HsVar error_RDR) (HsLit (HsString (_PK_ ("Ix."++tycon_str++".index: out of range\n"))))
	)
	tycon_loc)

    enum_inRange
      = mk_easy_FunMonoBind tycon_loc inRange_RDR [TuplePatIn [a_Pat, b_Pat], c_Pat] [] (
	  untag_Expr tycon [(a_RDR, ah_RDR)] (
	  untag_Expr tycon [(b_RDR, bh_RDR)] (
	  untag_Expr tycon [(c_RDR, ch_RDR)] (
	  HsIf (genOpApp (HsVar ch_RDR) geH_RDR (HsVar ah_RDR)) (
	     (genOpApp (HsVar ch_RDR) leH_RDR (HsVar bh_RDR))
	  ) {-else-} (
	     false_Expr
	  ) tycon_loc))))

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

    con_arity   = argFieldCount data_con
    data_con_RDR = qual_orig_name data_con
    con_pat  xs = ConPatIn data_con_RDR (map VarPatIn xs)
    con_expr xs = mk_easy_App data_con_RDR xs

    as_needed = take con_arity as_RDRs
    bs_needed = take con_arity bs_RDRs
    cs_needed = take con_arity cs_RDRs

    --------------------------------------------------------------
    single_con_range
      = mk_easy_FunMonoBind tycon_loc range_RDR [TuplePatIn [con_pat as_needed, con_pat bs_needed]] [] $
	HsDo ListComp stmts tycon_loc
      where
	stmts = zipWith3Equal "single_con_range" mk_qual as_needed bs_needed cs_needed
		++
		[ReturnStmt (con_expr cs_needed)]

	mk_qual a b c = BindStmt (VarPatIn c)
				 (HsApp (HsVar range_RDR) (ExplicitTuple [HsVar a, HsVar b]))
				 tycon_loc

    ----------------
    single_con_index
      = mk_easy_FunMonoBind tycon_loc index_RDR [TuplePatIn [con_pat as_needed, con_pat bs_needed], con_pat cs_needed] [range_size] (
	foldl mk_index (HsLit (HsInt 0)) (zip3 as_needed bs_needed cs_needed))
      where
	mk_index multiply_by (l, u, i)
	  = genOpApp (
		(HsApp (HsApp (HsVar index_RDR) (ExplicitTuple [HsVar l, HsVar u])) (HsVar i))
	   ) plus_RDR (
		genOpApp (
		    (HsApp (HsVar rangeSize_RDR) (ExplicitTuple [HsVar l, HsVar u]))
		) times_RDR multiply_by
	   )

	range_size
	  = mk_easy_FunMonoBind tycon_loc rangeSize_RDR [TuplePatIn [a_Pat, b_Pat]] [] (
		genOpApp (
		    (HsApp (HsApp (HsVar index_RDR) (ExplicitTuple [a_Expr, b_Expr])) b_Expr)
		) plus_RDR (HsLit (HsInt 1)))

    ------------------
    single_con_inRange
      = mk_easy_FunMonoBind tycon_loc inRange_RDR 
			   [TuplePatIn [con_pat as_needed, con_pat bs_needed], con_pat cs_needed]
			   [] (
	  foldl1 and_Expr (zipWith3Equal "single_con_inRange" in_range as_needed bs_needed cs_needed))
      where
    	in_range a b c = HsApp (HsApp (HsVar inRange_RDR) (ExplicitTuple [HsVar a, HsVar b])) (HsVar c)
\end{code}

%************************************************************************
%*									*
\subsubsection{Generating @Read@ instance declarations}
%*									*
%************************************************************************

Ignoring all the infix-ery mumbo jumbo (ToDo)

\begin{code}
gen_Read_binds :: TyCon -> RdrNameMonoBinds

gen_Read_binds tycon
  = reads_prec `AndMonoBinds` read_list
  where
    tycon_loc = getSrcLoc tycon
    -----------------------------------------------------------------------
    read_list = mk_easy_FunMonoBind tycon_loc readList_RDR [] []
		  (HsApp (HsVar readList___RDR) (HsPar (HsApp (HsVar readsPrec_RDR) (HsLit (HsInt 0)))))
    -----------------------------------------------------------------------
    reads_prec
      = let
	    read_con_comprehensions
	      = map read_con (tyConDataCons tycon)
	in
	mk_easy_FunMonoBind tycon_loc readsPrec_RDR [a_Pat, b_Pat] [] (
	      foldr1 append_Expr read_con_comprehensions
	)
      where
	read_con data_con   -- note: "b" is the string being "read"
	  = let
		data_con_RDR = qual_orig_name data_con
		data_con_str= occNameString (getOccName data_con)
		con_arity   = argFieldCount data_con
		as_needed   = take con_arity as_RDRs
		bs_needed   = take con_arity bs_RDRs
		con_expr    = mk_easy_App data_con_RDR as_needed
		nullary_con = con_arity == 0

		con_qual
		  = BindStmt
		      (TuplePatIn [LitPatIn (HsString data_con_str), d_Pat])
		      (HsApp (HsVar lex_RDR) c_Expr)
		      tycon_loc

		field_quals = snd (mapAccumL mk_qual d_Expr (zipEqual "as_needed" as_needed bs_needed))
		mk_qual draw_from (con_field, str_left)
		  = (HsVar str_left,	-- what to draw from down the line...
			 BindStmt
			  (TuplePatIn [VarPatIn con_field, VarPatIn str_left])
			  (HsApp (HsApp (HsVar readsPrec_RDR) (HsLit (HsInt 10))) draw_from)
			  tycon_loc
		    )

		result_expr = ExplicitTuple [con_expr, if null bs_needed 
						       then d_Expr 
						       else HsVar (last bs_needed)]

		stmts = (con_qual : field_quals) ++ [ReturnStmt result_expr]
		

		read_paren_arg
		  = if nullary_con then -- must be False (parens are surely optional)
		       false_Expr
		    else -- parens depend on precedence...
		       HsPar (genOpApp a_Expr gt_RDR (HsLit (HsInt 9)))
	    in
	    HsApp (
	      readParen_Expr read_paren_arg $ HsPar $
		 HsLam (mk_easy_Match tycon_loc [c_Pat] [] $
		        HsDo ListComp stmts tycon_loc)
	      ) (HsVar b_RDR)
\end{code}

%************************************************************************
%*									*
\subsubsection{Generating @Show@ instance declarations}
%*									*
%************************************************************************

Ignoring all the infix-ery mumbo jumbo (ToDo)

\begin{code}
gen_Show_binds :: TyCon -> RdrNameMonoBinds

gen_Show_binds tycon
  = shows_prec `AndMonoBinds` show_list
  where
    tycon_loc = getSrcLoc tycon
    -----------------------------------------------------------------------
    show_list = mk_easy_FunMonoBind tycon_loc showList_RDR [] []
		  (HsApp (HsVar showList___RDR) (HsPar (HsApp (HsVar showsPrec_RDR) (HsLit (HsInt 0)))))
    -----------------------------------------------------------------------
    shows_prec
      = mk_FunMonoBind tycon_loc showsPrec_RDR (map pats_etc (tyConDataCons tycon))
      where
	pats_etc data_con
	  = let
		data_con_RDR = qual_orig_name data_con
		con_arity   = argFieldCount data_con
		bs_needed   = take con_arity bs_RDRs
		con_pat     = ConPatIn data_con_RDR (map VarPatIn bs_needed)
		nullary_con = con_arity == 0

		show_con
		  = let nm = occNameString (getOccName data_con)
			space_maybe = if nullary_con then _NIL_ else SLIT(" ")
		    in
			HsApp (HsVar showString_RDR) (HsLit (HsString (nm _APPEND_ space_maybe)))

		show_thingies = show_con : (spacified real_show_thingies)

		real_show_thingies
		  = [ HsApp (HsApp (HsVar showsPrec_RDR) (HsLit (HsInt 10))) (HsVar b)
		  | b <- bs_needed ]
	    in
	    if nullary_con then  -- skip the showParen junk...
		ASSERT(null bs_needed)
		([a_Pat, con_pat], show_con)
	    else
		([a_Pat, con_pat],
		    showParen_Expr (HsPar (genOpApp a_Expr ge_RDR (HsLit (HsInt 10))))
				   (HsPar (nested_compose_Expr show_thingies)))
	  where
	    spacified []     = []
	    spacified [x]    = [x]
	    spacified (x:xs) = (x : (HsVar showSpace_RDR) : spacified xs)
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

gen_tag_n_con_monobind (rdr_name, tycon, GenCon2Tag)
  = mk_FunMonoBind (getSrcLoc tycon) rdr_name (map mk_stuff (tyConDataCons tycon))
  where
    mk_stuff :: DataCon -> ([RdrNamePat], RdrNameHsExpr)

    mk_stuff var
      = ASSERT(isDataCon var)
	([pat], HsLit (HsIntPrim (toInteger ((dataConTag var) - fIRST_TAG))))
      where
	pat    = ConPatIn var_RDR (nOfThem (argFieldCount var) WildPatIn)
	var_RDR = qual_orig_name var

gen_tag_n_con_monobind (rdr_name, tycon, GenTag2Con)
  = mk_FunMonoBind (getSrcLoc tycon) rdr_name (map mk_stuff (tyConDataCons tycon) ++ 
							     [([WildPatIn], impossible_Expr)])
  where
    mk_stuff :: DataCon -> ([RdrNamePat], RdrNameHsExpr)

    mk_stuff var
      = ASSERT(isDataCon var)
	([lit_pat], HsVar var_RDR)
      where
	lit_pat = ConPatIn mkInt_RDR [LitPatIn (HsIntPrim (toInteger ((dataConTag var) - fIRST_TAG)))]
	var_RDR  = qual_orig_name var

gen_tag_n_con_monobind (rdr_name, tycon, GenMaxTag)
  = mk_easy_FunMonoBind (getSrcLoc tycon) 
		rdr_name [] [] (HsApp (HsVar mkInt_RDR) (HsLit (HsIntPrim max_tag)))
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
mk_easy_FunMonoBind :: SrcLoc -> RdrName -> [RdrNamePat]
		    -> [RdrNameMonoBinds] -> RdrNameHsExpr
		    -> RdrNameMonoBinds

mk_easy_FunMonoBind loc fun pats binds expr
  = FunMonoBind fun False{-not infix-} [mk_easy_Match loc pats binds expr] loc

mk_easy_Match loc pats binds expr
  = mk_match loc pats expr (mkbind binds)
  where
    mkbind [] = EmptyBinds
    mkbind bs = MonoBind (foldr1 AndMonoBinds bs) [] recursive
	-- The renamer expects everything in its input to be a
	-- "recursive" MonoBinds, and it is its job to sort things out
	-- from there.

mk_FunMonoBind	:: SrcLoc -> RdrName
		-> [([RdrNamePat], RdrNameHsExpr)]
		-> RdrNameMonoBinds

mk_FunMonoBind loc fun [] = panic "TcGenDeriv:mk_FunMonoBind"
mk_FunMonoBind loc fun pats_and_exprs
  = FunMonoBind fun False{-not infix-}
		[ mk_match loc p e EmptyBinds | (p,e) <-pats_and_exprs ]
		loc

mk_match loc pats expr binds
  = foldr PatMatch
	  (GRHSMatch (GRHSsAndBindsIn [OtherwiseGRHS expr loc] binds))
	  (map paren pats)
  where
    paren p@(VarPatIn _) = p
    paren other_p	 = ParPatIn other_p
\end{code}

\begin{code}
mk_easy_App f xs = foldl HsApp (HsVar f) (map HsVar xs)
\end{code}

ToDo: Better SrcLocs.

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

compare_Case = compare_gen_Case compare_RDR
cmp_eq_Expr = compare_gen_Case cmp_eq_RDR

compare_gen_Case fun lt eq gt a b
  = HsCase (HsPar (HsApp (HsApp (HsVar fun) a) b)) {-of-}
      [PatMatch (ConPatIn ltTag_RDR [])
	  (GRHSMatch (GRHSsAndBindsIn [OtherwiseGRHS lt mkGeneratedSrcLoc] EmptyBinds)),

       PatMatch (ConPatIn eqTag_RDR [])
	  (GRHSMatch (GRHSsAndBindsIn [OtherwiseGRHS eq mkGeneratedSrcLoc] EmptyBinds)),

       PatMatch (ConPatIn gtTag_RDR [])
	  (GRHSMatch (GRHSsAndBindsIn [OtherwiseGRHS gt mkGeneratedSrcLoc] EmptyBinds))]
       mkGeneratedSrcLoc

careful_compare_Case ty lt eq gt a b
  = if not (isPrimType ty) then
       compare_gen_Case compare_RDR lt eq gt a b

    else -- we have to do something special for primitive things...
       HsIf (genOpApp a relevant_eq_op b)
	    eq
	    (HsIf (genOpApp a relevant_lt_op b) lt gt mkGeneratedSrcLoc)
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
    [(charPrimTy,	eqH_Char_RDR)
    ,(intPrimTy,	eqH_Int_RDR)
    ,(wordPrimTy,	eqH_Word_RDR)
    ,(addrPrimTy,	eqH_Addr_RDR)
    ,(floatPrimTy,	eqH_Float_RDR)
    ,(doublePrimTy,	eqH_Double_RDR)
    ]

lt_op_tbl =
    [(charPrimTy,	ltH_Char_RDR)
    ,(intPrimTy,	ltH_Int_RDR)
    ,(wordPrimTy,	ltH_Word_RDR)
    ,(addrPrimTy,	ltH_Addr_RDR)
    ,(floatPrimTy,	ltH_Float_RDR)
    ,(doublePrimTy,	ltH_Double_RDR)
    ]

-----------------------------------------------------------------------

and_Expr, append_Expr :: RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr

and_Expr    a b = genOpApp a and_RDR    b
append_Expr a b = genOpApp a append_RDR b

-----------------------------------------------------------------------

eq_Expr :: Type -> RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr
eq_Expr ty a b
  = if not (isPrimType ty) then
       genOpApp a eq_RDR  b
    else -- we have to do something special for primitive things...
       genOpApp a relevant_eq_op b
  where
    relevant_eq_op = assoc_ty_id eq_op_tbl ty
\end{code}

\begin{code}
argFieldCount :: Id -> Int	-- Works on data and newtype constructors
argFieldCount con = length (dataConRawArgTys con)
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
  = HsIf (genOpApp (HsVar a) op (HsVar b)) true_case false_case mkGeneratedSrcLoc

enum_from_to_Expr
	:: RdrNameHsExpr -> RdrNameHsExpr
	-> RdrNameHsExpr
enum_from_then_to_Expr
	:: RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr
	-> RdrNameHsExpr

enum_from_to_Expr      f   t2 = HsApp (HsApp (HsVar enumFromTo_RDR) f) t2
enum_from_then_to_Expr f t t2 = HsApp (HsApp (HsApp (HsVar enumFromThenTo_RDR) f) t) t2

showParen_Expr, readParen_Expr
	:: RdrNameHsExpr -> RdrNameHsExpr
	-> RdrNameHsExpr

showParen_Expr e1 e2 = HsApp (HsApp (HsVar showParen_RDR) e1) e2
readParen_Expr e1 e2 = HsApp (HsApp (HsVar readParen_RDR) e1) e2

nested_compose_Expr :: [RdrNameHsExpr] -> RdrNameHsExpr

nested_compose_Expr [e] = parenify e
nested_compose_Expr (e:es)
  = HsApp (HsApp (HsVar compose_RDR) (parenify e)) (nested_compose_Expr es)

-- impossible_Expr is used in case RHSs that should never happen.
-- We generate these to keep the desugarer from complaining that they *might* happen!
impossible_Expr = HsApp (HsVar error_RDR) (HsLit (HsString (_PK_ "Urk! in TcGenDeriv")))

parenify e@(HsVar _) = e
parenify e	     = HsPar e

-- genOpApp wraps brackets round the operator application, so that the
-- renamer won't subsequently try to re-associate it. 
-- For some reason the renamer doesn't reassociate it right, and I can't
-- be bothered to find out why just now.

genOpApp e1 op e2 = mkOpApp e1 op e2
\end{code}

\begin{code}
qual_orig_name n = case modAndOcc n of { (m,n) -> Qual m n HiFile }

a_RDR		= varUnqual SLIT("a")
b_RDR		= varUnqual SLIT("b")
c_RDR		= varUnqual SLIT("c")
d_RDR		= varUnqual SLIT("d")
ah_RDR		= varUnqual SLIT("a#")
bh_RDR		= varUnqual SLIT("b#")
ch_RDR		= varUnqual SLIT("c#")
dh_RDR		= varUnqual SLIT("d#")
cmp_eq_RDR	= varUnqual SLIT("cmp_eq")
rangeSize_RDR	= varUnqual SLIT("rangeSize")

as_RDRs		= [ varUnqual (_PK_ ("a"++show i)) | i <- [(1::Int) .. ] ]
bs_RDRs		= [ varUnqual (_PK_ ("b"++show i)) | i <- [(1::Int) .. ] ]
cs_RDRs		= [ varUnqual (_PK_ ("c"++show i)) | i <- [(1::Int) .. ] ]

a_Expr		= HsVar a_RDR
b_Expr		= HsVar b_RDR
c_Expr		= HsVar c_RDR
d_Expr		= HsVar d_RDR
ltTag_Expr	= HsVar ltTag_RDR
eqTag_Expr	= HsVar eqTag_RDR
gtTag_Expr	= HsVar gtTag_RDR
false_Expr	= HsVar false_RDR
true_Expr	= HsVar true_RDR

con2tag_Expr tycon = HsVar (con2tag_RDR tycon)

a_Pat		= VarPatIn a_RDR
b_Pat		= VarPatIn b_RDR
c_Pat		= VarPatIn c_RDR
d_Pat		= VarPatIn d_RDR

con2tag_RDR, tag2con_RDR, maxtag_RDR :: TyCon -> RdrName

con2tag_RDR tycon = varUnqual (SLIT("con2tag_") _APPEND_ occNameString (getOccName tycon) _APPEND_ SLIT("#"))
tag2con_RDR tycon = varUnqual (SLIT("tag2con_") _APPEND_ occNameString (getOccName tycon) _APPEND_ SLIT("#"))
maxtag_RDR tycon  = varUnqual (SLIT("maxtag_")  _APPEND_ occNameString (getOccName tycon) _APPEND_ SLIT("#"))
\end{code}
