%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
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
	eqH_PN,
	eq_TAG_Expr,
	eq_TAG_PN,
	error_PN,
	false_Expr,
	false_PN,
	geH_PN,
	gen_Binary_binds,
	gen_Enum_binds,
	gen_Eq_binds,
	gen_Ix_binds,
	gen_Ord_binds,
	gen_Text_binds,
	gen_tag_n_con_monobind,
	gt_TAG_Expr,
	gt_TAG_PN,
	leH_PN,
	ltH_PN,
	lt_TAG_Expr,
	lt_TAG_PN,
	minusH_PN,
	mkInt_PN,
	rangeSize_PN,
	true_Expr,
	true_PN
    ) where

IMPORT_Trace		-- ToDo:rm debugging
import Outputable
import Pretty

import AbsSyn		-- the stuff being typechecked

import AbsPrel
import PrimOps

import AbsUniType	( getTyConDataCons, isEnumerationTyCon,
			  maybeSingleConstructorTyCon, --UNUSED: preludeClassDerivedFor,
			  -- UNUSED: isEnumerationTyConMostly,
			  isPrimType, UniType,
			  TauType(..), TyVarTemplate, ThetaType(..)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import Id		( getDataConArity, getDataConTag,
			  getDataConSig, isNullaryDataCon, fIRST_TAG,
			  isDataCon, DataCon(..), ConTag(..), Id
			)
import Maybes		( maybeToBool, Maybe(..) )
import Name		( Name(..) )
import ProtoName	( ProtoName(..) )
import RenameAuxFuns	-- why not? take all of it...
import RenameMonad4	-- initRn4, etc.
import SrcLoc		( mkGeneratedSrcLoc )
import TcDeriv		( con2tag_PN, tag2con_PN, maxtag_PN,
			  TagThingWanted(..), DerivEqn(..)
			)
import Unique		-- some ClassKey stuff
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
  (==) a b  = case (tagCmp a b) of { _LT -> False; _EQ -> True ; _GT -> False}
  (/=) a b  = case (tagCmp a b) of { _LT -> True ; _EQ -> False; _GT -> True }
\begin{verbatim}
  However, that requires that \tr{Ord <whatever>} was put in the context
  for the instance decl, which it probably wasn't, so the decls
  produced don't get through the typechecker.
\end{itemize}

\begin{code}
gen_Eq_binds :: TyCon -> ProtoNameMonoBinds

gen_Eq_binds tycon
  = case (partition isNullaryDataCon (getTyConDataCons tycon))
				of { (nullary_cons, nonnullary_cons) ->
    let
	rest
	  = if null nullary_cons then
		case maybeSingleConstructorTyCon tycon of
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

	    data_con_PN = Prel (WiredInVal data_con)
	    as_needed   = take (getDataConArity data_con) as_PNs
	    bs_needed   = take (getDataConArity data_con) bs_PNs
	    tys_needed  = case (getDataConSig data_con) of
			    (_,_, arg_tys, _) -> arg_tys
        in
	([con1_pat, con2_pat], nested_eq_expr tys_needed as_needed bs_needed)
      where
	nested_eq_expr []     []     []  = true_Expr
	nested_eq_expr [ty]   [a]    [b] = eq_Expr ty (Var a) (Var b)
	nested_eq_expr (t:ts) (a:as) (b:bs)
	  = let
		rest_expr = nested_eq_expr ts as bs
	    in
	    and_Expr (eq_Expr t (Var a) (Var b)) rest_expr

boring_ne_method
  = mk_easy_FunMonoBind ne_PN [a_Pat, b_Pat] [] (
	App (Var not_PN) (App (App (Var eq_PN) a_Expr) b_Expr)
	)
\end{code}

%************************************************************************
%*									*
\subsubsection[TcGenDeriv-Ord]{Generating @Ord@ instance declarations}
%*									*
%************************************************************************

For a derived @Ord@, we concentrate our attentions on the non-standard
@_tagCmp@ method, which type:
\begin{verbatim}
_tagCmp :: a -> a -> _CMP_TAG

-- and the builtin tag type is:

data _CMP_TAG = _LT | _EQ | _GT deriving ()
\end{verbatim}

(All this @_tagCmp@ stuff is due to the sterling analysis by Julian
Seward.)

We will use the same example data type as above:
\begin{verbatim}
data Foo ... = N1 | N2 ... | Nn | O1 a b | O2 Int | O3 Double b b | ...
\end{verbatim}

\begin{itemize}
\item
  We do all the other @Ord@ methods with calls to @_tagCmp@:
\begin{verbatim}
instance ... (Ord <wurble> <wurble>) where
    a <  b  = case _tagCmp a b of { _LT -> True;  _EQ -> False; _GT -> False }
    a <= b  = case _tagCmp a b of { _LT -> True;  _EQ -> True;  _GT -> False }
    a >= b  = case _tagCmp a b of { _LT -> False; _EQ -> True;  _GT -> True  }
    a >  b  = case _tagCmp a b of { _LT -> False; _EQ -> False; _GT -> True  }

    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }

    -- _tagCmp to come...
\end{verbatim}

\item
  @_tagCmp@ always has two parts.  First, we use the compared
  data-constructors' tags to deal with the case of different
  constructors:
\begin{verbatim}
_tagCmp a b = case (con2tag_Foo a) of { a# ->
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
  tagCmp-style stuff; for our example:
\begin{verbatim}
cmp_eq (O1 a1 b1) (O1 a2 b2)
  = case (_tagCmp a1 a2) of { _LT -> _LT; _EQ -> _tagCmp b1 b2; _GT -> _GT }

cmp_eq (O2 a1) (O2 a2)
  = _tagCmp a1 a2

cmp_eq (O3 a1 b1 c1) (O3 a2 b2 c2)
  = case (_tagCmp a1 a2) of {
      _LT -> _LT;
      _GT -> _GT;
      _EQ -> case _tagCmp b1 b2 of {
		  _LT -> _LT;
		  _GT -> _GT;
		  _EQ -> _tagCmp c1 c2
		}
    }
\end{verbatim}

  Again, we must be careful about unboxed comparisons.  For example,
  if \tr{a1} and \tr{a2} were \tr{Int#}s in the 2nd example above, we'd need to
  generate:
\begin{verbatim}
cmp_eq lt eq gt (O2 a1) (O2 a2)
  = tagCmpInt# a1 a2
  -- or maybe the unfolded equivalent
\end{verbatim}

\item
  For the remaining nullary constructors, we already know that the
  tags are equal so:
\begin{verbatim}
cmp_eq _ _ = _EQ
\end{verbatim}
\end{itemize}

\begin{code}
gen_Ord_binds :: TyCon -> ProtoNameMonoBinds

gen_Ord_binds tycon
  = defaulted `AndMonoBinds` tagCmp
  where
    --------------------------------------------------------------------
    tagCmp = mk_easy_FunMonoBind tagCmp_PN
		[a_Pat, b_Pat]
		[cmp_eq]
	    (if maybeToBool (maybeSingleConstructorTyCon tycon) then
		cmp_eq_Expr lt_TAG_Expr eq_TAG_Expr gt_TAG_Expr a_Expr b_Expr
	     else
		untag_Expr tycon [(a_PN, ah_PN), (b_PN, bh_PN)]
		  (cmp_tags_Expr eqH_PN ah_PN bh_PN
			-- True case; they are equal
			-- If an enumeration type we are done; else
			-- recursively compare their components
		    (if isEnumerationTyCon tycon then
			eq_TAG_Expr
		     else
			cmp_eq_Expr lt_TAG_Expr eq_TAG_Expr gt_TAG_Expr a_Expr b_Expr
		    )
			-- False case; they aren't equal
			-- So we need to do a less-than comparison on the tags
		    (cmp_tags_Expr ltH_PN ah_PN bh_PN lt_TAG_Expr gt_TAG_Expr)))

    (nullary_cons, nonnullary_cons)
      = partition isNullaryDataCon (getTyConDataCons tycon)

    cmp_eq
      = mk_FunMonoBind cmp_eq_PN (map pats_etc nonnullary_cons ++ deflt_pats_etc)
      where
	pats_etc data_con
	  = ([con1_pat, con2_pat],
	     nested_tagCmp_expr tys_needed as_needed bs_needed)
	  where
	    con1_pat = ConPatIn data_con_PN (map VarPatIn as_needed)
	    con2_pat = ConPatIn data_con_PN (map VarPatIn bs_needed)

	    data_con_PN = Prel (WiredInVal data_con)
	    as_needed   = take (getDataConArity data_con) as_PNs
	    bs_needed   = take (getDataConArity data_con) bs_PNs
	    tys_needed  = case (getDataConSig data_con) of
			    (_,_, arg_tys, _) -> arg_tys

	    nested_tagCmp_expr [ty] [a] [b]
	      = careful_tagCmp_Case ty lt_TAG_Expr eq_TAG_Expr gt_TAG_Expr (Var a) (Var b)

	    nested_tagCmp_expr (ty:tys) (a:as) (b:bs)
	      = let eq_expr = nested_tagCmp_expr tys as bs
	        in  careful_tagCmp_Case ty lt_TAG_Expr eq_expr gt_TAG_Expr (Var a) (Var b)

	deflt_pats_etc
	  = if null nullary_cons
	    then []
	    else [([a_Pat, b_Pat], eq_TAG_Expr)]
    --------------------------------------------------------------------

defaulted = foldr1 AndMonoBinds [lt, le, ge, gt, max_, min_]

lt = mk_easy_FunMonoBind lt_PN [a_Pat, b_Pat] [] (
	    tagCmp_Case true_Expr  false_Expr false_Expr a_Expr b_Expr)
le = mk_easy_FunMonoBind le_PN [a_Pat, b_Pat] [] (
	    tagCmp_Case true_Expr  true_Expr  false_Expr a_Expr b_Expr)
ge = mk_easy_FunMonoBind ge_PN [a_Pat, b_Pat] [] (
	    tagCmp_Case false_Expr true_Expr  true_Expr  a_Expr b_Expr)
gt = mk_easy_FunMonoBind gt_PN [a_Pat, b_Pat] [] (
	    tagCmp_Case false_Expr false_Expr true_Expr  a_Expr b_Expr)

max_ = mk_easy_FunMonoBind max_PN [a_Pat, b_Pat] [] (
	    tagCmp_Case b_Expr a_Expr a_Expr a_Expr b_Expr)
min_ = mk_easy_FunMonoBind min_PN [a_Pat, b_Pat] [] (
	    tagCmp_Case a_Expr a_Expr b_Expr a_Expr b_Expr)
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
gen_Enum_binds :: TyCon -> ProtoNameMonoBinds

gen_Enum_binds tycon
  = enum_from `AndMonoBinds` enum_from_then
  where
    enum_from
      = mk_easy_FunMonoBind enumFrom_PN [a_Pat] [] (
	  untag_Expr tycon [(a_PN, ah_PN)] (
	  App (App (Var map_PN) (Var (tag2con_PN tycon))) (
	      enum_from_to_Expr
		(App (Var mkInt_PN) (Var ah_PN))
		(Var (maxtag_PN tycon)))))

    enum_from_then
      = mk_easy_FunMonoBind enumFromThen_PN [a_Pat, b_Pat] [] (
	  untag_Expr tycon [(a_PN, ah_PN), (b_PN, bh_PN)] (
	  App (App (Var map_PN) (Var (tag2con_PN tycon))) (
	      enum_from_then_to_Expr
		(App (Var mkInt_PN) (Var ah_PN))
		(App (Var mkInt_PN) (Var bh_PN))
		(Var (maxtag_PN tycon)))))
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
gen_Ix_binds :: TyCon -> ProtoNameMonoBinds

gen_Ix_binds tycon
  = if isEnumerationTyCon tycon
    then enum_ixes
    else single_con_ixes
  where
    tycon_str = _UNPK_ (snd (getOrigName tycon))

    --------------------------------------------------------------
    enum_ixes = enum_range `AndMonoBinds`
    	    	enum_index `AndMonoBinds` enum_inRange

    enum_range
      = mk_easy_FunMonoBind range_PN [TuplePatIn [a_Pat, b_Pat]] [] (
	  untag_Expr tycon [(a_PN, ah_PN)] (
	  untag_Expr tycon [(b_PN, bh_PN)] (
	  App (App (Var map_PN) (Var (tag2con_PN tycon))) (
	      enum_from_to_Expr
		(App (Var mkInt_PN) (Var ah_PN))
		(App (Var mkInt_PN) (Var bh_PN))
	))))

    enum_index
      = mk_easy_FunMonoBind index_PN [AsPatIn c_PN (TuplePatIn [a_Pat, b_Pat]), d_Pat] [] (
	If (App (App (Var inRange_PN) c_Expr) d_Expr) (
	   untag_Expr tycon [(a_PN, ah_PN)] (
	   untag_Expr tycon [(d_PN, dh_PN)] (
	   let
		grhs = [OtherwiseGRHS (App (Var mkInt_PN) (Var c_PN)) mkGeneratedSrcLoc]
	   in
	   Case (OpApp (Var dh_PN) (Var minusH_PN) (Var ah_PN)) {-of-}
	     [PatMatch (VarPatIn c_PN)
				(GRHSMatch (GRHSsAndBindsIn grhs EmptyBinds))]
	   ))
	) {-else-} (
	   App (Var error_PN) (Lit (StringLit (_PK_ ("Ix."++tycon_str++".index: out of range\n"))))
	)
	)

    enum_inRange
      = mk_easy_FunMonoBind inRange_PN [TuplePatIn [a_Pat, b_Pat], c_Pat] [] (
	  untag_Expr tycon [(a_PN, ah_PN)] (
	  untag_Expr tycon [(b_PN, bh_PN)] (
	  untag_Expr tycon [(c_PN, ch_PN)] (
	  If (OpApp (Var ch_PN) (Var geH_PN) (Var ah_PN)) (
	     (OpApp (Var ch_PN) (Var leH_PN) (Var bh_PN))
	  ) {-else-} (
	     false_Expr
	  )))))

    --------------------------------------------------------------
    single_con_ixes = single_con_range `AndMonoBinds`
    	    	single_con_index `AndMonoBinds` single_con_inRange

    data_con
      =	case maybeSingleConstructorTyCon tycon of -- just checking...
	  Nothing -> panic "get_Ix_binds"
	  Just dc -> let
			 (_, _, arg_tys, _) = getDataConSig dc
		     in
		     if any isPrimType arg_tys then
			 error ("ERROR: Can't derive Ix for a single-constructor type with primitive argument types: "++tycon_str)
		     else
			 dc

    con_arity   = getDataConArity data_con
    data_con_PN = Prel (WiredInVal data_con)
    con_pat  xs = ConPatIn data_con_PN (map VarPatIn xs)
    con_expr xs = foldl App (Var data_con_PN) (map Var xs)

    as_needed = take (getDataConArity data_con) as_PNs
    bs_needed = take (getDataConArity data_con) bs_PNs
    cs_needed = take (getDataConArity data_con) cs_PNs

    --------------------------------------------------------------
    single_con_range
      = mk_easy_FunMonoBind range_PN [TuplePatIn [con_pat as_needed, con_pat bs_needed]] [] (
	  ListComp (con_expr cs_needed) (zipWith3 mk_qual as_needed bs_needed cs_needed)
	)
      where
	mk_qual a b c = GeneratorQual (VarPatIn c)
			    (App (Var range_PN) (ExplicitTuple [Var a, Var b]))

    ----------------
    single_con_index
      = mk_easy_FunMonoBind index_PN [TuplePatIn [con_pat as_needed, con_pat bs_needed], con_pat cs_needed] [range_size] (
	foldl mk_index (Lit (IntLit 0)) (zip3 as_needed bs_needed cs_needed))
      where
	mk_index multiply_by (l, u, i)
	  =OpApp (
		(App (App (Var index_PN) (ExplicitTuple [Var l, Var u])) (Var i))
	   ) (Var plus_PN) (
		OpApp (
		    (App (Var rangeSize_PN) (ExplicitTuple [Var l, Var u]))
		) (Var times_PN) multiply_by
	   )

	range_size
	  = mk_easy_FunMonoBind rangeSize_PN [TuplePatIn [a_Pat, b_Pat]] [] (
		OpApp (
		    (App (App (Var index_PN) (ExplicitTuple [a_Expr, b_Expr])) b_Expr) 
		) (Var plus_PN) (Lit (IntLit 1)))

    ------------------
    single_con_inRange
      = mk_easy_FunMonoBind inRange_PN [TuplePatIn [con_pat as_needed, con_pat bs_needed], con_pat cs_needed] [] (
	  foldl1 and_Expr (zipWith3 in_range as_needed bs_needed cs_needed))
      where
    	in_range a b c = App (App (Var inRange_PN) (ExplicitTuple [Var a, Var b])) (Var c)
\end{code}

%************************************************************************
%*									*
\subsubsection[TcGenDeriv-Text]{Generating @Text@ instance declarations}
%*									*
%************************************************************************

Deriving @Text@ is a pain.  @show@ is commonly used; @read@ is rarely
used---but we're supposed to generate massive amounts of code for it
anyway.  We provide a command-line flag to say ``Don't bother''
(@OmitDerivedRead@).

Also: ignoring all the infix-ery mumbo jumbo (ToDo)

The part of the Haskell report that deals with this (pages~147--151,
1.2~version) is an adequate guide to what needs to be done.  Note that
this is where we may (eventually) use the fixity info that's been
passed around.

\begin{code}
gen_Text_binds :: [RenamedFixityDecl] -> Bool -> TyCon -> ProtoNameMonoBinds

gen_Text_binds fixities omit_derived_read tycon
  = if omit_derived_read
    then shows_prec `AndMonoBinds` show_list
    else shows_prec `AndMonoBinds` show_list
	   `AndMonoBinds`
	 reads_prec `AndMonoBinds` read_list
  where
    -----------------------------------------------------------------------
    show_list = mk_easy_FunMonoBind showList_PN [] []
		  (App (Var _showList_PN) (App (Var showsPrec_PN) (Lit (IntLit 0))))

    read_list = mk_easy_FunMonoBind readList_PN [] []
		  (App (Var _readList_PN) (App (Var readsPrec_PN) (Lit (IntLit 0))))

    -----------------------------------------------------------------------
    shows_prec
      = mk_FunMonoBind showsPrec_PN (map pats_etc (getTyConDataCons tycon))
      where
	pats_etc data_con
	  = let
		data_con_PN = Prel (WiredInVal data_con)
		bs_needed   = take (getDataConArity data_con) bs_PNs
		con_pat     = ConPatIn data_con_PN (map VarPatIn bs_needed)
		is_nullary_con = isNullaryDataCon data_con

		show_con
		  = let (mod, nm)   = getOrigName data_con
			space_maybe = if is_nullary_con then _NIL_ else SLIT(" ")
		    in
			App (Var showString_PN) (Lit (StringLit (nm _APPEND_ space_maybe)))

		show_thingies = show_con : (spacified real_show_thingies)

		real_show_thingies
		  = [ App (App (Var showsPrec_PN) (Lit (IntLit 10))) (Var b)
		  | b <- bs_needed ]
	    in
	    if is_nullary_con then  -- skip the showParen junk...
		ASSERT(null bs_needed)
		([a_Pat, con_pat], show_con)
	    else
		([a_Pat, con_pat],
		    showParen_Expr (OpApp a_Expr (Var ge_PN) (Lit (IntLit 10)))
				   (nested_compose_Expr show_thingies))
	  where
	    spacified []     = []
	    spacified [x]    = [x]
	    spacified (x:xs) = (x : (Var showSpace_PN) : spacified xs)

    -----------------------------------------------------------------------
    reads_prec	-- ignore the infix game altogether
      = let
	    read_con_comprehensions
	      = map read_con (getTyConDataCons tycon)
	in
	mk_easy_FunMonoBind readsPrec_PN [a_Pat, b_Pat] [] (
	      foldl1 append_Expr read_con_comprehensions
	)
      where
	read_con data_con   -- note: "b" is the string being "read"
	  = let
		data_con_PN = Prel (WiredInVal data_con)
		data_con_str= snd  (getOrigName data_con)
		as_needed   = take (getDataConArity data_con) as_PNs
		bs_needed   = take (getDataConArity data_con) bs_PNs
		con_expr    = foldl App (Var data_con_PN) (map Var as_needed)
		is_nullary_con = isNullaryDataCon data_con

		con_qual
		  = GeneratorQual
		      (TuplePatIn [LitPatIn (StringLit data_con_str), d_Pat])
		      (App (Var lex_PN) c_Expr)

		field_quals = snd (mapAccumL mk_qual d_Expr (as_needed `zip` bs_needed))

		read_paren_arg
		  = if is_nullary_con then -- must be False (parens are surely optional)
		       false_Expr
		    else -- parens depend on precedence...
		       OpApp a_Expr (Var gt_PN) (Lit (IntLit 9))
	    in
	    App (
	      readParen_Expr read_paren_arg (
		 Lam (mk_easy_Match [c_Pat] []  (
		   ListComp (ExplicitTuple [con_expr,
			    if null bs_needed then d_Expr else Var (last bs_needed)])
		    (con_qual : field_quals)))
	    )) (Var b_PN)
	  where
	    mk_qual draw_from (con_field, str_left)
	      = (Var str_left,	-- what to draw from down the line...
		 GeneratorQual
		  (TuplePatIn [VarPatIn con_field, VarPatIn str_left])
		  (App (App (Var readsPrec_PN) (Lit (IntLit 10))) draw_from))
\end{code}

%************************************************************************
%*									*
\subsubsection[TcGenDeriv-Binary]{Generating @Binary@ instance declarations}
%*									*
%************************************************************************

ToDo: NOT DONE YET.

\begin{code}
gen_Binary_binds :: TyCon -> ProtoNameMonoBinds

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
gen_tag_n_con_monobind
    :: (ProtoName, Name,    -- (proto)Name for the thing in question
	TyCon,		    -- tycon in question
	TagThingWanted)
    -> ProtoNameMonoBinds

gen_tag_n_con_monobind (pn, _, tycon, GenCon2Tag)
  = mk_FunMonoBind pn (map mk_stuff (getTyConDataCons tycon))
  where
    mk_stuff :: DataCon -> ([ProtoNamePat], ProtoNameExpr)

    mk_stuff var
      = ASSERT(isDataCon var)
	([pat], Lit (IntPrimLit (toInteger ((getDataConTag var) - fIRST_TAG))))
      where
	pat    = ConPatIn var_PN (nOfThem (getDataConArity var) WildPatIn)
	var_PN = Prel (WiredInVal var)

gen_tag_n_con_monobind (pn, _, tycon, GenTag2Con)
  = mk_FunMonoBind pn (map mk_stuff (getTyConDataCons tycon))
  where
    mk_stuff :: DataCon -> ([ProtoNamePat], ProtoNameExpr)

    mk_stuff var
      = ASSERT(isDataCon var)
	([lit_pat], Var var_PN)
      where
	lit_pat = ConPatIn mkInt_PN [LitPatIn (IntPrimLit (toInteger ((getDataConTag var) - fIRST_TAG)))]
	var_PN  = Prel (WiredInVal var)

gen_tag_n_con_monobind (pn, _, tycon, GenMaxTag)
  = mk_easy_FunMonoBind pn [] [] (App (Var mkInt_PN) (Lit (IntPrimLit max_tag)))
  where
    max_tag =  case (getTyConDataCons tycon) of
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
mk_easy_FunMonoBind :: ProtoName -> [ProtoNamePat]
		    -> [ProtoNameMonoBinds] -> ProtoNameExpr
		    -> ProtoNameMonoBinds

mk_easy_FunMonoBind fun pats binds expr
  = FunMonoBind fun [mk_easy_Match pats binds expr] mkGeneratedSrcLoc

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

mk_FunMonoBind	:: ProtoName
		-> [([ProtoNamePat], ProtoNameExpr)]
		-> ProtoNameMonoBinds

mk_FunMonoBind fun [] = panic "TcGenDeriv:mk_FunMonoBind"
mk_FunMonoBind fun pats_and_exprs
  = FunMonoBind fun (map mk_match pats_and_exprs) mkGeneratedSrcLoc
  where
    mk_match (pats, expr)
      = foldr PatMatch
		(GRHSMatch (GRHSsAndBindsIn [OtherwiseGRHS expr mkGeneratedSrcLoc] EmptyBinds))
		pats
\end{code}

\begin{code}
tagCmp_Case, cmp_eq_Expr ::
	  ProtoNameExpr -> ProtoNameExpr -> ProtoNameExpr
	  -> ProtoNameExpr -> ProtoNameExpr
	  -> ProtoNameExpr
tagCmp_gen_Case :: 
	  ProtoName
	  -> ProtoNameExpr -> ProtoNameExpr -> ProtoNameExpr
	  -> ProtoNameExpr -> ProtoNameExpr
	  -> ProtoNameExpr
careful_tagCmp_Case :: -- checks for primitive types...
	  UniType
	  -> ProtoNameExpr -> ProtoNameExpr -> ProtoNameExpr
	  -> ProtoNameExpr -> ProtoNameExpr
	  -> ProtoNameExpr

tagCmp_Case = tagCmp_gen_Case tagCmp_PN
cmp_eq_Expr = tagCmp_gen_Case cmp_eq_PN

tagCmp_gen_Case fun lt eq gt a b
  = Case (App (App (Var fun) a) b) {-of-}
      [PatMatch (ConPatIn lt_TAG_PN [])
	  (GRHSMatch (GRHSsAndBindsIn [OtherwiseGRHS lt mkGeneratedSrcLoc] EmptyBinds)),

       PatMatch (ConPatIn eq_TAG_PN [])
	  (GRHSMatch (GRHSsAndBindsIn [OtherwiseGRHS eq mkGeneratedSrcLoc] EmptyBinds)),

       PatMatch (ConPatIn gt_TAG_PN [])
	  (GRHSMatch (GRHSsAndBindsIn [OtherwiseGRHS gt mkGeneratedSrcLoc] EmptyBinds))]

careful_tagCmp_Case ty lt eq gt a b
  = if not (isPrimType ty) then
       tagCmp_gen_Case tagCmp_PN lt eq gt a b

    else -- we have to do something special for primitive things...
       If (OpApp a (Var relevant_eq_op) b)
	  eq
	  (If (OpApp a (Var relevant_lt_op) b) lt gt)
  where
    relevant_eq_op = assoc "careful_tagCmp_Case" eq_op_tbl ty
    relevant_lt_op = assoc "careful_tagCmp_Case" lt_op_tbl ty

eq_op_tbl = [
    (charPrimTy,	Prel (WiredInVal (primOpId CharEqOp))),
    (intPrimTy,		Prel (WiredInVal (primOpId IntEqOp))),
    (wordPrimTy,	Prel (WiredInVal (primOpId WordEqOp))),
    (addrPrimTy,	Prel (WiredInVal (primOpId AddrEqOp))),
    (floatPrimTy,	Prel (WiredInVal (primOpId FloatEqOp))),
    (doublePrimTy,	Prel (WiredInVal (primOpId DoubleEqOp))) ]

lt_op_tbl = [
    (charPrimTy,	Prel (WiredInVal (primOpId CharLtOp))),
    (intPrimTy,		Prel (WiredInVal (primOpId IntLtOp))),
    (wordPrimTy,	Prel (WiredInVal (primOpId WordLtOp))),
    (addrPrimTy,	Prel (WiredInVal (primOpId AddrLtOp))),
    (floatPrimTy,	Prel (WiredInVal (primOpId FloatLtOp))),
    (doublePrimTy,	Prel (WiredInVal (primOpId DoubleLtOp))) ]

-----------------------------------------------------------------------

and_Expr, append_Expr :: ProtoNameExpr -> ProtoNameExpr -> ProtoNameExpr

and_Expr    a b = OpApp a (Var and_PN)    b
append_Expr a b = OpApp a (Var append_PN) b

-----------------------------------------------------------------------

eq_Expr  :: UniType -> ProtoNameExpr -> ProtoNameExpr -> ProtoNameExpr
eq_Expr ty a b
  = if not (isPrimType ty) then
       OpApp a (Var eq_PN)  b
    else -- we have to do something special for primitive things...
       OpApp a (Var relevant_eq_op) b
  where
    relevant_eq_op = assoc "eq_Expr" eq_op_tbl ty
\end{code}

\begin{code}
untag_Expr :: TyCon -> [(ProtoName, ProtoName)] -> ProtoNameExpr -> ProtoNameExpr
untag_Expr tycon [] expr = expr
untag_Expr tycon ((untag_this, put_tag_here) : more) expr
  = Case (App (con2tag_Expr tycon) (Var untag_this)) {-of-}
      [PatMatch (VarPatIn put_tag_here)
			(GRHSMatch (GRHSsAndBindsIn grhs EmptyBinds))]
  where
    grhs = [OtherwiseGRHS (untag_Expr tycon more expr) mkGeneratedSrcLoc]

cmp_tags_Expr :: ProtoName 			-- Comparison op
	     -> ProtoName -> ProtoName		-- Things to compare
	     -> ProtoNameExpr 			-- What to return if true
	     -> ProtoNameExpr			-- What to return if false
	     -> ProtoNameExpr

cmp_tags_Expr op a b true_case false_case 
  = If (OpApp (Var a) (Var op) (Var b)) true_case false_case

enum_from_to_Expr
	:: ProtoNameExpr -> ProtoNameExpr
	-> ProtoNameExpr
enum_from_then_to_Expr
	:: ProtoNameExpr -> ProtoNameExpr -> ProtoNameExpr
	-> ProtoNameExpr

enum_from_to_Expr      f   t2 = App (App (Var enumFromTo_PN) f) t2
enum_from_then_to_Expr f t t2 = App (App (App (Var enumFromThenTo_PN) f) t) t2

showParen_Expr, readParen_Expr
	:: ProtoNameExpr -> ProtoNameExpr
	-> ProtoNameExpr

showParen_Expr e1 e2 = App (App (Var showParen_PN) e1) e2
readParen_Expr e1 e2 = App (App (Var readParen_PN) e1) e2

nested_compose_Expr :: [ProtoNameExpr] -> ProtoNameExpr

nested_compose_Expr [e] = e
nested_compose_Expr (e:es)
  = App (App (Var compose_PN) e) (nested_compose_Expr es)
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
tagCmp_PN	= prelude_method SLIT("Ord") SLIT("_tagCmp")
lt_TAG_PN	= Prel (WiredInVal ltPrimDataCon)
eq_TAG_PN	= Prel (WiredInVal eqPrimDataCon)
gt_TAG_PN	= Prel (WiredInVal gtPrimDataCon)
enumFrom_PN	 = prelude_method SLIT("Enum") SLIT("enumFrom")
enumFromTo_PN	 = prelude_method SLIT("Enum") SLIT("enumFromTo")
enumFromThen_PN	 = prelude_method SLIT("Enum") SLIT("enumFromThen")
enumFromThenTo_PN= prelude_method SLIT("Enum") SLIT("enumFromThenTo")
range_PN	 = prelude_method SLIT("Ix")   SLIT("range")
index_PN	 = prelude_method SLIT("Ix")   SLIT("index")
inRange_PN	 = prelude_method SLIT("Ix")   SLIT("inRange")
readsPrec_PN	 = prelude_method SLIT("Text") SLIT("readsPrec")
showsPrec_PN	 = prelude_method SLIT("Text") SLIT("showsPrec")
readList_PN	 = prelude_method SLIT("Text") SLIT("readList")
showList_PN	 = prelude_method SLIT("Text") SLIT("showList")
plus_PN		 = prelude_method SLIT("Num")  SLIT("+")
times_PN	 = prelude_method SLIT("Num")  SLIT("*")

false_PN	= Prel (WiredInVal falseDataCon)
true_PN		= Prel (WiredInVal trueDataCon)
eqH_PN		= Prel (WiredInVal (primOpId IntEqOp))
geH_PN		= Prel (WiredInVal (primOpId IntGeOp))
leH_PN		= Prel (WiredInVal (primOpId IntLeOp))
ltH_PN		= Prel (WiredInVal (primOpId IntLtOp))
minusH_PN	= Prel (WiredInVal (primOpId IntSubOp))
and_PN		= prelude_val pRELUDE     SLIT("&&")
not_PN		= prelude_val pRELUDE     SLIT("not")
append_PN	= prelude_val pRELUDE_LIST SLIT("++")
map_PN		= prelude_val pRELUDE_LIST SLIT("map")
compose_PN	= prelude_val pRELUDE     SLIT(".")
mkInt_PN	= Prel (WiredInVal intDataCon)
error_PN	= Prel (WiredInVal eRROR_ID)
showSpace_PN	= prelude_val pRELUDE_TEXT SLIT("showSpace__") -- not quite std
showString_PN	= prelude_val pRELUDE_TEXT SLIT("showString")
showParen_PN	= prelude_val pRELUDE_TEXT SLIT("showParen")
readParen_PN	= prelude_val pRELUDE_TEXT SLIT("readParen")
lex_PN		= prelude_val pRELUDE_TEXT SLIT("lex")
_showList_PN    = prelude_val pRELUDE_CORE SLIT("_showList")
_readList_PN    = prelude_val pRELUDE_CORE SLIT("_readList")

prelude_val    m s = Imp m s [m] s
prelude_method c o = Imp pRELUDE_CORE o [pRELUDE_CORE] o -- class not used...

a_Expr		= Var a_PN
b_Expr		= Var b_PN
c_Expr		= Var c_PN
d_Expr		= Var d_PN
lt_TAG_Expr	= Var lt_TAG_PN
eq_TAG_Expr	= Var eq_TAG_PN
gt_TAG_Expr	= Var gt_TAG_PN
false_Expr	= Var false_PN
true_Expr	= Var true_PN

con2tag_Expr tycon = Var (con2tag_PN tycon)

a_Pat		= VarPatIn a_PN
b_Pat		= VarPatIn b_PN
c_Pat		= VarPatIn c_PN
d_Pat		= VarPatIn d_PN
\end{code}

%************************************************************************
%*									*
\subsection[TcGenDeriv-misc-utils]{Miscellaneous utility bits for deriving}
%*									*
%************************************************************************

\begin{code}
{- UNUSED:
hasCon2TagFun :: TyCon -> Bool
hasCon2TagFun tycon
  =  preludeClassDerivedFor ordClassKey tycon
  || isEnumerationTyConMostly tycon

hasTag2ConFun :: TyCon -> Bool
hasTag2ConFun tycon
  =  isEnumerationTyCon tycon
  && (preludeClassDerivedFor ixClassKey   tycon
   || preludeClassDerivedFor enumClassKey tycon)
-}
\end{code}
