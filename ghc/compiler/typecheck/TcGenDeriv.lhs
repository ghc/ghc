%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcGenDeriv]{Generating derived instance declarations}

This module is nominally ``subordinate'' to @TcDeriv@, which is the
``official'' interface to deriving-related things.

This is where we do all the grimy bindings' generation.

\begin{code}
#include "HsVersions.h"

module TcGenDeriv {- (
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
	eqH_PN,
	eqTag_Expr,
	eq_PN,
	error_PN,
	false_Expr,
	false_PN,
	geH_PN,
	gen_Binary_binds,
	gen_Enum_binds,
	gen_Eq_binds,
	gen_Ix_binds,
	gen_Ord_binds,
	gen_Read_binds,
	gen_Show_binds,
	gen_tag_n_con_monobind,
	gtTag_Expr,
	gt_PN,
	leH_PN,
	ltH_PN,
	ltTag_Expr,
	lt_PN,
	minusH_PN,
	mkInt_PN,
	rangeSize_PN,
	true_Expr,
	true_PN,

	con2tag_FN, tag2con_FN, maxtag_FN,
	con2tag_PN, tag2con_PN, maxtag_PN,

	TagThingWanted(..)
    ) -} where

import Ubiq

import HsSyn		( HsBinds(..), Bind(..), MonoBinds(..), Match(..), GRHSsAndBinds(..),
			  GRHS(..), HsExpr(..), HsLit(..), InPat(..), Qual(..), Stmt,
			  ArithSeqInfo, Sig, PolyType, FixityDecl, Fake )
import RdrHsSyn		( RdrNameMonoBinds(..), RdrNameHsExpr(..), RdrNamePat(..) )
import RnHsSyn		( RnName(..), RenamedFixityDecl(..) )

--import RnMonad4		-- initRn4, etc.
import RnUtils

import Id		( GenId, dataConArity, dataConTag,
			  dataConSig, fIRST_TAG,
			  isDataCon, DataCon(..), ConTag(..) )
import IdUtils		( primOpId )
import Maybes		( maybeToBool )
--import Name		( Name(..) )
import Outputable
import PrimOp
import PrelInfo
import Pretty
import SrcLoc		( mkGeneratedSrcLoc )
import TyCon		( TyCon, tyConDataCons, isEnumerationTyCon, maybeTyConSingleCon )
import Type		( eqTy, isPrimType )
import Unique
import Util
\end{code}

%************************************************************************
%*									*
\subsection[TcGenDeriv-classes]{Generating code, by derivable class}
%*									*
%************************************************************************

%************************************************************************
%*									*
\subsubsection[TcGenDeriv-Eq]{Generating @Eq@ instance declarations}
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
foo_TcGenDeriv = panic "Nothing in TcGenDeriv LATER ToDo"

{- LATER:
gen_Eq_binds :: TyCon -> RdrNameMonoBinds

gen_Eq_binds tycon
  = case (partition (\ con -> dataConArity con == 0)
		    (tyConDataCons tycon))
    of { (nullary_cons, nonnullary_cons) ->
    let
	rest
	  = if null nullary_cons then
		case maybeTyConSingleCon tycon of
		  Just _ -> []
		  Nothing -> -- if cons don't match, then False
		     [([a_Pat, b_Pat], false_Expr)]
	    else -- calc. and compare the tags
		 [([a_Pat, b_Pat],
		    untag_Expr tycon [(a_PN,ah_PN), (b_PN,bh_PN)]
		      (cmp_tags_Expr eqH_PN ah_PN bh_PN true_Expr false_Expr))]
    in
    mk_FunMonoBind eq_PN ((map pats_etc nonnullary_cons) ++ rest)
    `AndMonoBinds` boring_ne_method
    }
  where
    ------------------------------------------------------------------
    pats_etc data_con
      = let
	    con1_pat = ConPatIn data_con_PN (map VarPatIn as_needed)
	    con2_pat = ConPatIn data_con_PN (map VarPatIn bs_needed)

	    data_con_PN = Prel (WiredInId data_con)
	    as_needed   = take (dataConArity data_con) as_PNs
	    bs_needed   = take (dataConArity data_con) bs_PNs
	    tys_needed  = case (dataConSig data_con) of
			    (_,_, arg_tys, _) -> arg_tys
	in
	([con1_pat, con2_pat], nested_eq_expr tys_needed as_needed bs_needed)
      where
	nested_eq_expr []     []     []  = true_Expr
	nested_eq_expr [ty]   [a]    [b] = eq_Expr ty (HsVar a) (HsVar b)
	nested_eq_expr (t:ts) (a:as) (b:bs)
	  = let
		rest_expr = nested_eq_expr ts as bs
	    in
	    and_Expr (eq_Expr t (HsVar a) (HsVar b)) rest_expr

boring_ne_method
  = mk_easy_FunMonoBind ne_PN [a_Pat, b_Pat] [] (
	HsApp (HsVar not_PN) (HsApp (HsApp (HsVar eq_PN) a_Expr) b_Expr)
	)
\end{code}

%************************************************************************
%*									*
\subsubsection[TcGenDeriv-Ord]{Generating @Ord@ instance declarations}
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
    a <  b  = case compare a b of { LT -> True;  EQ -> False; GT -> False }
    a <= b  = case compare a b of { LT -> True;  EQ -> True;  GT -> False }
    a >= b  = case compare a b of { LT -> False; EQ -> True;  GT -> True  }
    a >  b  = case compare a b of { LT -> False; EQ -> False; GT -> True  }

    max a b = case compare a b of { LT -> b; EQ -> a;  GT -> a }
    min a b = case compare a b of { LT -> a; EQ -> b;  GT -> b }

    -- compare to come...
\end{verbatim}

\item
  @compare@ always has two parts.  First, we use the compared
  data-constructors' tags to deal with the case of different
  constructors:
\begin{verbatim}
compare a b = case (con2tag_Foo a) of { a# ->
	      case (con2tag_Foo b) of { b# ->
	      case (a# ==# b#)  	 of {
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
		  (cmp_tags_Expr eqH_PN ah_PN bh_PN
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
		    (cmp_tags_Expr ltH_PN ah_PN bh_PN ltTag_Expr gtTag_Expr)))

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

	    data_con_PN = Prel (WiredInId data_con)
	    as_needed   = take (dataConArity data_con) as_PNs
	    bs_needed   = take (dataConArity data_con) bs_PNs
	    tys_needed  = case (dataConSig data_con) of
			    (_,_, arg_tys, _) -> arg_tys

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
\subsubsection[TcGenDeriv-Enum]{Generating @Enum@ instance declarations}
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
      = mk_easy_FunMonoBind enumFrom_PN [a_Pat] [] (
	  untag_Expr tycon [(a_PN, ah_PN)] (
	  HsApp (HsApp (HsVar map_PN) (HsVar (tag2con_PN tycon))) (
	      enum_from_to_Expr
		(HsApp (HsVar mkInt_PN) (HsVar ah_PN))
		(HsVar (maxtag_PN tycon)))))

    enum_from_then
      = mk_easy_FunMonoBind enumFromThen_PN [a_Pat, b_Pat] [] (
	  untag_Expr tycon [(a_PN, ah_PN), (b_PN, bh_PN)] (
	  HsApp (HsApp (HsVar map_PN) (HsVar (tag2con_PN tycon))) (
	      enum_from_then_to_Expr
		(HsApp (HsVar mkInt_PN) (HsVar ah_PN))
		(HsApp (HsVar mkInt_PN) (HsVar bh_PN))
		(HsVar (maxtag_PN tycon)))))
\end{code}

%************************************************************************
%*									*
\subsubsection[TcGenDeriv-Ix]{Generating @Ix@ instance declarations}
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
      = mk_easy_FunMonoBind range_PN [TuplePatIn [a_Pat, b_Pat]] [] (
	  untag_Expr tycon [(a_PN, ah_PN)] (
	  untag_Expr tycon [(b_PN, bh_PN)] (
	  HsApp (HsApp (HsVar map_PN) (HsVar (tag2con_PN tycon))) (
	      enum_from_to_Expr
		(HsApp (HsVar mkInt_PN) (HsVar ah_PN))
		(HsApp (HsVar mkInt_PN) (HsVar bh_PN))
	))))

    enum_index
      = mk_easy_FunMonoBind index_PN [AsPatIn c_PN (TuplePatIn [a_Pat, b_Pat]), d_Pat] [] (
	HsIf (HsApp (HsApp (HsVar inRange_PN) c_Expr) d_Expr) (
	   untag_Expr tycon [(a_PN, ah_PN)] (
	   untag_Expr tycon [(d_PN, dh_PN)] (
	   let
		grhs = [OtherwiseGRHS (HsApp (HsVar mkInt_PN) (HsVar c_PN)) mkGeneratedSrcLoc]
	   in
	   HsCase
	     (OpApp (HsVar dh_PN) (HsVar minusH_PN) (HsVar ah_PN))
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
	  HsIf (OpApp (HsVar ch_PN) (HsVar geH_PN) (HsVar ah_PN)) (
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
	  Just dc -> let
			 (_, _, arg_tys, _) = dataConSig dc
		     in
		     if any isPrimType arg_tys then
			 error ("ERROR: Can't derive Ix for a single-constructor type with primitive argument types: "++tycon_str)
		     else
			 dc

    con_arity   = dataConArity data_con
    data_con_PN = Prel (WiredInId data_con)
    con_pat  xs = ConPatIn data_con_PN (map VarPatIn xs)
    con_expr xs = foldl HsApp (HsVar data_con_PN) (map HsVar xs)

    as_needed = take (dataConArity data_con) as_PNs
    bs_needed = take (dataConArity data_con) bs_PNs
    cs_needed = take (dataConArity data_con) cs_PNs

    --------------------------------------------------------------
    single_con_range
      = mk_easy_FunMonoBind range_PN [TuplePatIn [con_pat as_needed, con_pat bs_needed]] [] (
	  ListComp (con_expr cs_needed) (zipWith3Equal mk_qual as_needed bs_needed cs_needed)
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
	  foldl1 and_Expr (zipWith3Equal in_range as_needed bs_needed cs_needed))
      where
    	in_range a b c = HsApp (HsApp (HsVar inRange_PN) (ExplicitTuple [HsVar a, HsVar b])) (HsVar c)
\end{code}

%************************************************************************
%*									*
\subsubsection[TcGenDeriv-Text]{Generating @Show@ and @Read@ instance declarations}
%*									*
%************************************************************************

Ignoring all the infix-ery mumbo jumbo (ToDo)

\begin{code}
gen_Read_binds :: [RenamedFixityDecl] -> TyCon -> RdrNameMonoBinds
gen_Show_binds :: [RenamedFixityDecl] -> TyCon -> RdrNameMonoBinds

gen_Read_binds fixities tycon
  = reads_prec `AndMonoBinds` read_list
  where
    -----------------------------------------------------------------------
    read_list = mk_easy_FunMonoBind readList_PN [] []
		  (HsApp (HsVar _readList_PN) (HsApp (HsVar readsPrec_PN) (HsLit (HsInt 0))))
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
		data_con_PN = Prel (WiredInId data_con)
		data_con_str= snd  (moduleNamePair data_con)
		as_needed   = take (dataConArity data_con) as_PNs
		bs_needed   = take (dataConArity data_con) bs_PNs
		con_expr    = foldl HsApp (HsVar data_con_PN) (map HsVar as_needed)
		nullary_con = dataConArity data_con == 0

		con_qual
		  = GeneratorQual
		      (TuplePatIn [LitPatIn (HsString data_con_str), d_Pat])
		      (HsApp (HsVar lex_PN) c_Expr)

		field_quals = snd (mapAccumL mk_qual d_Expr (as_needed `zip` bs_needed))

		read_paren_arg
		  = if nullary_con then -- must be False (parens are surely optional)
		       false_Expr
		    else -- parens depend on precedence...
		       OpApp a_Expr (HsVar gt_PN) (HsLit (HsInt 9))
	    in
	    HsApp (
	      readParen_Expr read_paren_arg (
		 HsLam (mk_easy_Match [c_Pat] []  (
		   ListComp (ExplicitTuple [con_expr,
			    if null bs_needed then d_Expr else HsVar (last bs_needed)])
		    (con_qual : field_quals)))
	    )) (HsVar b_PN)
	  where
	    mk_qual draw_from (con_field, str_left)
	      = (HsVar str_left,	-- what to draw from down the line...
		 GeneratorQual
		  (TuplePatIn [VarPatIn con_field, VarPatIn str_left])
		  (HsApp (HsApp (HsVar readsPrec_PN) (HsLit (HsInt 10))) draw_from))


gen_Show_binds fixities tycon
  = shows_prec `AndMonoBinds` show_list
  where
    -----------------------------------------------------------------------
    show_list = mk_easy_FunMonoBind showList_PN [] []
		  (HsApp (HsVar _showList_PN) (HsApp (HsVar showsPrec_PN) (HsLit (HsInt 0))))
    -----------------------------------------------------------------------
    shows_prec
      = mk_FunMonoBind showsPrec_PN (map pats_etc (tyConDataCons tycon))
      where
	pats_etc data_con
	  = let
		data_con_PN = Prel (WiredInId data_con)
		bs_needed   = take (dataConArity data_con) bs_PNs
		con_pat     = ConPatIn data_con_PN (map VarPatIn bs_needed)
		nullary_con = dataConArity data_con == 0

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
		    showParen_Expr (OpApp a_Expr (HsVar ge_PN) (HsLit (HsInt 10)))
				   (nested_compose_Expr show_thingies))
	  where
	    spacified []     = []
	    spacified [x]    = [x]
	    spacified (x:xs) = (x : (HsVar showSpace_PN) : spacified xs)
\end{code}

%************************************************************************
%*									*
\subsubsection[TcGenDeriv-Binary]{Generating @Binary@ instance declarations}
%*									*
%************************************************************************

ToDo: NOT DONE YET.

\begin{code}
gen_Binary_binds :: TyCon -> RdrNameMonoBinds

gen_Binary_binds tycon
  = panic "gen_Binary_binds"
\end{code}

%************************************************************************
%*									*
\subsection[TcGenDeriv-con2tag-tag2con]{Generating extra binds (@con2tag@ and @tag2con@)}
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
    :: (RdrName, RnName,    -- (proto)Name for the thing in question
	TyCon,		    -- tycon in question
	TagThingWanted)
    -> RdrNameMonoBinds

gen_tag_n_con_monobind (pn, _, tycon, GenCon2Tag)
  = mk_FunMonoBind pn (map mk_stuff (tyConDataCons tycon))
  where
    mk_stuff :: DataCon -> ([RdrNamePat], RdrNameHsExpr)

    mk_stuff var
      = ASSERT(isDataCon var)
	([pat], HsLit (HsIntPrim (toInteger ((dataConTag var) - fIRST_TAG))))
      where
	pat    = ConPatIn var_PN (nOfThem (dataConArity var) WildPatIn)
	var_PN = Prel (WiredInId var)

gen_tag_n_con_monobind (pn, _, tycon, GenTag2Con)
  = mk_FunMonoBind pn (map mk_stuff (tyConDataCons tycon))
  where
    mk_stuff :: DataCon -> ([RdrNamePat], RdrNameHsExpr)

    mk_stuff var
      = ASSERT(isDataCon var)
	([lit_pat], HsVar var_PN)
      where
	lit_pat = ConPatIn mkInt_PN [LitPatIn (HsIntPrim (toInteger ((dataConTag var) - fIRST_TAG)))]
	var_PN  = Prel (WiredInId var)

gen_tag_n_con_monobind (pn, _, tycon, GenMaxTag)
  = mk_easy_FunMonoBind pn [] [] (HsApp (HsVar mkInt_PN) (HsLit (HsIntPrim max_tag)))
  where
    max_tag =  case (tyConDataCons tycon) of
		 data_cons -> toInteger ((length data_cons) - fIRST_TAG)
\end{code}

%************************************************************************
%*									*
\subsection[TcGenDeriv-bind-utils]{Utility bits for generating bindings}
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
  = foldr PatMatch
	  (GRHSMatch (GRHSsAndBindsIn [OtherwiseGRHS expr mkGeneratedSrcLoc] (mkbind binds)))
	  pats
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
  = FunMonoBind fun False{-not infix-} (map mk_match pats_and_exprs) mkGeneratedSrcLoc
  where
    mk_match (pats, expr)
      = foldr PatMatch
		(GRHSMatch (GRHSsAndBindsIn [OtherwiseGRHS expr mkGeneratedSrcLoc] EmptyBinds))
		pats
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
  = HsCase (HsApp (HsApp (HsVar fun) a) b) {-of-}
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
       HsIf (OpApp a (HsVar relevant_eq_op) b)
	    eq
	    (HsIf (OpApp a (HsVar relevant_lt_op) b) lt gt mkGeneratedSrcLoc)
	    mkGeneratedSrcLoc
  where
    relevant_eq_op = assoc_ty_id eq_op_tbl ty
    relevant_lt_op = assoc_ty_id lt_op_tbl ty

assoc_ty_id tyids ty 
  = if null res then panic "assoc_ty"
    else head res
  where
    res = [id | (ty',id) <- tyids, eqTy ty ty']

eq_op_tbl = [
    (charPrimTy,	Prel (WiredInId (primOpId CharEqOp))),
    (intPrimTy,		Prel (WiredInId (primOpId IntEqOp))),
    (wordPrimTy,	Prel (WiredInId (primOpId WordEqOp))),
    (addrPrimTy,	Prel (WiredInId (primOpId AddrEqOp))),
    (floatPrimTy,	Prel (WiredInId (primOpId FloatEqOp))),
    (doublePrimTy,	Prel (WiredInId (primOpId DoubleEqOp))) ]

lt_op_tbl = [
    (charPrimTy,	Prel (WiredInId (primOpId CharLtOp))),
    (intPrimTy,		Prel (WiredInId (primOpId IntLtOp))),
    (wordPrimTy,	Prel (WiredInId (primOpId WordLtOp))),
    (addrPrimTy,	Prel (WiredInId (primOpId AddrLtOp))),
    (floatPrimTy,	Prel (WiredInId (primOpId FloatLtOp))),
    (doublePrimTy,	Prel (WiredInId (primOpId DoubleLtOp))) ]

-----------------------------------------------------------------------

and_Expr, append_Expr :: RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr

and_Expr    a b = OpApp a (HsVar and_PN)    b
append_Expr a b = OpApp a (HsVar append_PN) b

-----------------------------------------------------------------------

eq_Expr  :: Type -> RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr
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
  = HsCase (HsApp (con2tag_Expr tycon) (HsVar untag_this)) {-of-}
      [PatMatch (VarPatIn put_tag_here)
			(GRHSMatch (GRHSsAndBindsIn grhs EmptyBinds))]
      mkGeneratedSrcLoc
  where
    grhs = [OtherwiseGRHS (untag_Expr tycon more expr) mkGeneratedSrcLoc]

cmp_tags_Expr :: RdrName 			-- Comparison op
	     -> RdrName -> RdrName		-- Things to compare
	     -> RdrNameHsExpr 		-- What to return if true
	     -> RdrNameHsExpr			-- What to return if false
	     -> RdrNameHsExpr

cmp_tags_Expr op a b true_case false_case
  = HsIf (OpApp (HsVar a) (HsVar op) (HsVar b)) true_case false_case mkGeneratedSrcLoc

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

nested_compose_Expr [e] = e
nested_compose_Expr (e:es)
  = HsApp (HsApp (HsVar compose_PN) e) (nested_compose_Expr es)
\end{code}

\begin{code}
a_PN		= Unk SLIT("a")
b_PN		= Unk SLIT("b")
c_PN		= Unk SLIT("c")
d_PN		= Unk SLIT("d")
ah_PN		= Unk SLIT("a#")
bh_PN		= Unk SLIT("b#")
ch_PN		= Unk SLIT("c#")
dh_PN		= Unk SLIT("d#")
cmp_eq_PN	= Unk SLIT("cmp_eq")
rangeSize_PN	= Unk SLIT("rangeSize")

as_PNs		= [ Unk (_PK_ ("a"++show i)) | i <- [(1::Int) .. ] ]
bs_PNs		= [ Unk (_PK_ ("b"++show i)) | i <- [(1::Int) .. ] ]
cs_PNs		= [ Unk (_PK_ ("c"++show i)) | i <- [(1::Int) .. ] ]

eq_PN		= prelude_method SLIT("Eq")  SLIT("==")
ne_PN		= prelude_method SLIT("Eq")  SLIT("/=")
le_PN		= prelude_method SLIT("Ord") SLIT("<=")
lt_PN		= prelude_method SLIT("Ord") SLIT("<")
ge_PN		= prelude_method SLIT("Ord") SLIT(">=")
gt_PN		= prelude_method SLIT("Ord") SLIT(">")
max_PN		= prelude_method SLIT("Ord") SLIT("max")
min_PN		= prelude_method SLIT("Ord") SLIT("min")
compare_PN	= prelude_method SLIT("Ord") SLIT("compare")
ltTag_PN	= Prel (WiredInId ltDataCon)
eqTag_PN	= Prel (WiredInId eqDataCon)
gtTag_PN	= Prel (WiredInId gtDataCon)
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

false_PN	= Prel (WiredInId falseDataCon)
true_PN		= Prel (WiredInId trueDataCon)
eqH_PN		= Prel (WiredInId (primOpId IntEqOp))
geH_PN		= Prel (WiredInId (primOpId IntGeOp))
leH_PN		= Prel (WiredInId (primOpId IntLeOp))
ltH_PN		= Prel (WiredInId (primOpId IntLtOp))
minusH_PN	= Prel (WiredInId (primOpId IntSubOp))
and_PN		= prelude_val pRELUDE     SLIT("&&")
not_PN		= prelude_val pRELUDE     SLIT("not")
append_PN	= prelude_val pRELUDE_LIST SLIT("++")
map_PN		= prelude_val pRELUDE_LIST SLIT("map")
compose_PN	= prelude_val pRELUDE     SLIT(".")
mkInt_PN	= Prel (WiredInId intDataCon)
error_PN	= Prel (WiredInId eRROR_ID)
showSpace_PN	= prelude_val pRELUDE_TEXT SLIT("showSpace__") -- not quite std
showString_PN	= prelude_val pRELUDE_TEXT SLIT("showString")
showParen_PN	= prelude_val pRELUDE_TEXT SLIT("showParen")
readParen_PN	= prelude_val pRELUDE_TEXT SLIT("readParen")
lex_PN		= prelude_val pRELUDE_TEXT SLIT("lex")
_showList_PN    = prelude_val pRELUDE_CORE SLIT("_showList")
_readList_PN    = prelude_val pRELUDE_CORE SLIT("_readList")

prelude_val    m s = Imp m s [m] s
prelude_method c o = Imp pRELUDE_CORE o [pRELUDE_CORE] o -- class not used...

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
    Imp mod con2tag [mod] con2tag

tag2con_PN tycon
  = let	(mod, nm) = moduleNamePair tycon
	tag2con	  = SLIT("tag2con_") _APPEND_ nm _APPEND_ SLIT("#")
    in
    Imp mod tag2con [mod] tag2con

maxtag_PN tycon
  = let	(mod, nm) = moduleNamePair tycon
	maxtag	  = SLIT("maxtag_") _APPEND_ nm _APPEND_ SLIT("#")
    in
    Imp mod maxtag [mod] maxtag


con2tag_FN, tag2con_FN, maxtag_FN :: TyCon -> RnName

tag2con_FN tycon
  = let	(mod, nm) = moduleNamePair tycon
	tag2con	  = SLIT("tag2con_") _APPEND_ nm _APPEND_ SLIT("#")
    in
    mkFullName mod tag2con InventedInThisModule NotExported mkGeneratedSrcLoc

maxtag_FN tycon
  = let	(mod, nm) = moduleNamePair tycon
	maxtag	  = SLIT("maxtag_") _APPEND_ nm _APPEND_ SLIT("#")
    in
    mkFullName mod maxtag InventedInThisModule NotExported mkGeneratedSrcLoc

con2tag_FN tycon
  = let	(mod, nm) = moduleNamePair tycon
	con2tag	  = SLIT("con2tag_") _APPEND_ nm _APPEND_ SLIT("#")
    in
    mkFullName mod con2tag InventedInThisModule NotExported mkGeneratedSrcLoc
-}
\end{code}

