%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

TcGenDeriv: Generating derived instance declarations

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

import HsSyn
import RdrName
import BasicTypes
import DataCon
import Name

import HscTypes
import PrelInfo
import PrelNames
import MkId
import PrimOp
import SrcLoc
import TyCon
import TcType
import TysPrim
import TysWiredIn
import Util
import Constants
import Outputable
import FastString
import OccName
import Bag

import Data.List	( partition, intersperse )
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
gen_Eq_binds :: TyCon -> LHsBinds RdrName

gen_Eq_binds tycon
  = let
	tycon_loc = getSrcSpan tycon

        (nullary_cons, nonnullary_cons)
           | isNewTyCon tycon = ([], tyConDataCons tycon)
           | otherwise	      = partition isNullarySrcDataCon (tyConDataCons tycon)

	rest
	  = if (null nullary_cons) then
		case maybeTyConSingleCon tycon of
		  Just _ -> []
		  Nothing -> -- if cons don't match, then False
		     [([nlWildPat, nlWildPat], false_Expr)]
	    else -- calc. and compare the tags
		 [([a_Pat, b_Pat],
		    untag_Expr tycon [(a_RDR,ah_RDR), (b_RDR,bh_RDR)]
		               (genOpApp (nlHsVar ah_RDR) eqInt_RDR (nlHsVar bh_RDR)))]
    in
    listToBag [
      mk_FunBind tycon_loc eq_RDR ((map pats_etc nonnullary_cons) ++ rest),
      mk_easy_FunBind tycon_loc ne_RDR [a_Pat, b_Pat] (
	nlHsApp (nlHsVar not_RDR) (nlHsPar (nlHsVarApps eq_RDR [a_RDR, b_RDR])))
    ]
  where
    ------------------------------------------------------------------
    pats_etc data_con
      = let
	    con1_pat = nlConVarPat data_con_RDR as_needed
	    con2_pat = nlConVarPat data_con_RDR bs_needed

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
	    nested_eq ty a b = nlHsPar (eq_Expr tycon ty (nlHsVar a) (nlHsVar b))
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
gen_Ord_binds :: TyCon -> LHsBinds RdrName

gen_Ord_binds tycon
  = unitBag compare 	-- `AndMonoBinds` compare	
		-- The default declaration in PrelBase handles this
  where
    tycon_loc = getSrcSpan tycon
    --------------------------------------------------------------------

    compare = L tycon_loc (mkFunBind (L tycon_loc compare_RDR) compare_matches)
    compare_matches = [mkMatch [a_Pat, b_Pat] compare_rhs cmp_eq_binds]
    cmp_eq_binds    = HsValBinds (ValBindsIn (unitBag cmp_eq) [])

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
       | otherwise	  = partition isNullarySrcDataCon tycon_data_cons

    cmp_eq = mk_FunBind tycon_loc cmp_eq_RDR cmp_eq_match
    cmp_eq_match
      | isEnumerationTyCon tycon
			   -- We know the tags are equal, so if it's an enumeration TyCon,
			   -- then there is nothing left to do
			   -- Catch this specially to avoid warnings
			   -- about overlapping patterns from the desugarer,
			   -- and to avoid unnecessary pattern-matching
      = [([nlWildPat,nlWildPat], eqTag_Expr)]
      | otherwise
      = map pats_etc nonnullary_cons ++
	(if single_con_type then	-- Omit wildcards when there's just one 
	      []			-- constructor, to silence desugarer
	else
              [([nlWildPat, nlWildPat], default_rhs)])

      where
	pats_etc data_con
	  = ([con1_pat, con2_pat],
	     nested_compare_expr tys_needed as_needed bs_needed)
	  where
	    con1_pat = nlConVarPat data_con_RDR as_needed
	    con2_pat = nlConVarPat data_con_RDR bs_needed

	    data_con_RDR = getRdrName data_con
	    con_arity   = length tys_needed
	    as_needed   = take con_arity as_RDRs
	    bs_needed   = take con_arity bs_RDRs
	    tys_needed  = dataConOrigArgTys data_con

	    nested_compare_expr [ty] [a] [b]
	      = careful_compare_Case tycon ty eqTag_Expr (nlHsVar a) (nlHsVar b)

	    nested_compare_expr (ty:tys) (a:as) (b:bs)
	      = let eq_expr = nested_compare_expr tys as bs
		in  careful_compare_Case tycon ty eq_expr (nlHsVar a) (nlHsVar b)

	    nested_compare_expr _ _ _ = panic "nested_compare_expr"	-- Args always equal length

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
gen_Enum_binds :: TyCon -> LHsBinds RdrName

gen_Enum_binds tycon
  = listToBag [
	succ_enum,
	pred_enum,
	to_enum,
	enum_from,
	enum_from_then,
	from_enum
    ]
  where
    tycon_loc = getSrcSpan tycon
    occ_nm    = getOccString tycon

    succ_enum
      = mk_easy_FunBind tycon_loc succ_RDR [a_Pat] $
	untag_Expr tycon [(a_RDR, ah_RDR)] $
	nlHsIf (nlHsApps eq_RDR [nlHsVar (maxtag_RDR tycon),
			       nlHsVarApps intDataCon_RDR [ah_RDR]])
	     (illegal_Expr "succ" occ_nm "tried to take `succ' of last tag in enumeration")
	     (nlHsApp (nlHsVar (tag2con_RDR tycon))
		    (nlHsApps plus_RDR [nlHsVarApps intDataCon_RDR [ah_RDR],
		 			nlHsIntLit 1]))
		    
    pred_enum
      = mk_easy_FunBind tycon_loc pred_RDR [a_Pat] $
	untag_Expr tycon [(a_RDR, ah_RDR)] $
	nlHsIf (nlHsApps eq_RDR [nlHsIntLit 0,
			       nlHsVarApps intDataCon_RDR [ah_RDR]])
	     (illegal_Expr "pred" occ_nm "tried to take `pred' of first tag in enumeration")
	     (nlHsApp (nlHsVar (tag2con_RDR tycon))
			   (nlHsApps plus_RDR [nlHsVarApps intDataCon_RDR [ah_RDR],
					       nlHsLit (HsInt (-1))]))

    to_enum
      = mk_easy_FunBind tycon_loc toEnum_RDR [a_Pat] $
	nlHsIf (nlHsApps and_RDR
		[nlHsApps ge_RDR [nlHsVar a_RDR, nlHsIntLit 0],
                 nlHsApps le_RDR [nlHsVar a_RDR, nlHsVar (maxtag_RDR tycon)]])
             (nlHsVarApps (tag2con_RDR tycon) [a_RDR])
	     (illegal_toEnum_tag occ_nm (maxtag_RDR tycon))

    enum_from
      = mk_easy_FunBind tycon_loc enumFrom_RDR [a_Pat] $
	  untag_Expr tycon [(a_RDR, ah_RDR)] $
	  nlHsApps map_RDR 
		[nlHsVar (tag2con_RDR tycon),
		 nlHsPar (enum_from_to_Expr
			    (nlHsVarApps intDataCon_RDR [ah_RDR])
			    (nlHsVar (maxtag_RDR tycon)))]

    enum_from_then
      = mk_easy_FunBind tycon_loc enumFromThen_RDR [a_Pat, b_Pat] $
	  untag_Expr tycon [(a_RDR, ah_RDR), (b_RDR, bh_RDR)] $
	  nlHsApp (nlHsVarApps map_RDR [tag2con_RDR tycon]) $
	    nlHsPar (enum_from_then_to_Expr
		    (nlHsVarApps intDataCon_RDR [ah_RDR])
		    (nlHsVarApps intDataCon_RDR [bh_RDR])
		    (nlHsIf  (nlHsApps gt_RDR [nlHsVarApps intDataCon_RDR [ah_RDR],
					     nlHsVarApps intDataCon_RDR [bh_RDR]])
			   (nlHsIntLit 0)
			   (nlHsVar (maxtag_RDR tycon))
			   ))

    from_enum
      = mk_easy_FunBind tycon_loc fromEnum_RDR [a_Pat] $
	  untag_Expr tycon [(a_RDR, ah_RDR)] $
	  (nlHsVarApps intDataCon_RDR [ah_RDR])
\end{code}

%************************************************************************
%*									*
\subsubsection{Generating @Bounded@ instance declarations}
%*									*
%************************************************************************

\begin{code}
gen_Bounded_binds tycon
  = if isEnumerationTyCon tycon then
	listToBag [ min_bound_enum, max_bound_enum ]
    else
	ASSERT(isSingleton data_cons)
	listToBag [ min_bound_1con, max_bound_1con ]
  where
    data_cons = tyConDataCons tycon
    tycon_loc = getSrcSpan tycon

    ----- enum-flavored: ---------------------------
    min_bound_enum = mkVarBind tycon_loc minBound_RDR (nlHsVar data_con_1_RDR)
    max_bound_enum = mkVarBind tycon_loc maxBound_RDR (nlHsVar data_con_N_RDR)

    data_con_1	  = head data_cons
    data_con_N	  = last data_cons
    data_con_1_RDR = getRdrName data_con_1
    data_con_N_RDR = getRdrName data_con_N

    ----- single-constructor-flavored: -------------
    arity	   = dataConSourceArity data_con_1

    min_bound_1con = mkVarBind tycon_loc minBound_RDR $
		     nlHsVarApps data_con_1_RDR (nOfThem arity minBound_RDR)
    max_bound_1con = mkVarBind tycon_loc maxBound_RDR $
		     nlHsVarApps data_con_1_RDR (nOfThem arity maxBound_RDR)
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

    -- Generate code for unsafeIndex, becuase using index leads
    -- to lots of redundant range tests
    unsafeIndex c@(a, b) d
      = case (con2tag_Foo d -# con2tag_Foo a) of
	       r# -> I# r#

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
gen_Ix_binds :: TyCon -> LHsBinds RdrName

gen_Ix_binds tycon
  = if isEnumerationTyCon tycon
    then enum_ixes
    else single_con_ixes
  where
    tycon_loc = getSrcSpan tycon

    --------------------------------------------------------------
    enum_ixes = listToBag [ enum_range, enum_index, enum_inRange ]

    enum_range
      = mk_easy_FunBind tycon_loc range_RDR [nlTuplePat [a_Pat, b_Pat] Boxed] $
	  untag_Expr tycon [(a_RDR, ah_RDR)] $
	  untag_Expr tycon [(b_RDR, bh_RDR)] $
	  nlHsApp (nlHsVarApps map_RDR [tag2con_RDR tycon]) $
	      nlHsPar (enum_from_to_Expr
			(nlHsVarApps intDataCon_RDR [ah_RDR])
			(nlHsVarApps intDataCon_RDR [bh_RDR]))

    enum_index
      = mk_easy_FunBind tycon_loc unsafeIndex_RDR 
		[noLoc (AsPat (noLoc c_RDR) 
			   (nlTuplePat [a_Pat, nlWildPat] Boxed)), 
				d_Pat] (
	   untag_Expr tycon [(a_RDR, ah_RDR)] (
	   untag_Expr tycon [(d_RDR, dh_RDR)] (
	   let
		rhs = nlHsVarApps intDataCon_RDR [c_RDR]
	   in
	   nlHsCase
	     (genOpApp (nlHsVar dh_RDR) minusInt_RDR (nlHsVar ah_RDR))
	     [mkSimpleHsAlt (nlVarPat c_RDR) rhs]
	   ))
	)

    enum_inRange
      = mk_easy_FunBind tycon_loc inRange_RDR [nlTuplePat [a_Pat, b_Pat] Boxed, c_Pat] $
	  untag_Expr tycon [(a_RDR, ah_RDR)] (
	  untag_Expr tycon [(b_RDR, bh_RDR)] (
	  untag_Expr tycon [(c_RDR, ch_RDR)] (
	  nlHsIf (genOpApp (nlHsVar ch_RDR) geInt_RDR (nlHsVar ah_RDR)) (
	     (genOpApp (nlHsVar ch_RDR) leInt_RDR (nlHsVar bh_RDR))
	  ) {-else-} (
	     false_Expr
	  ))))

    --------------------------------------------------------------
    single_con_ixes 
      = listToBag [single_con_range, single_con_index, single_con_inRange]

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

    con_pat  xs  = nlConVarPat data_con_RDR xs
    con_expr     = nlHsVarApps data_con_RDR cs_needed

    --------------------------------------------------------------
    single_con_range
      = mk_easy_FunBind tycon_loc range_RDR 
	  [nlTuplePat [con_pat as_needed, con_pat bs_needed] Boxed] $
	nlHsDo ListComp stmts con_expr
      where
	stmts = zipWith3Equal "single_con_range" mk_qual as_needed bs_needed cs_needed

	mk_qual a b c = noLoc $ mkBindStmt (nlVarPat c)
				 (nlHsApp (nlHsVar range_RDR) 
					(nlTuple [nlHsVar a, nlHsVar b] Boxed))

    ----------------
    single_con_index
      = mk_easy_FunBind tycon_loc unsafeIndex_RDR 
		[nlTuplePat [con_pat as_needed, con_pat bs_needed] Boxed, 
		 con_pat cs_needed] 
		(mk_index (zip3 as_needed bs_needed cs_needed))
      where
	-- index (l1,u1) i1 + rangeSize (l1,u1) * (index (l2,u2) i2 + ...)
	mk_index [] 	   = nlHsIntLit 0
	mk_index [(l,u,i)] = mk_one l u i
	mk_index ((l,u,i) : rest)
	  = genOpApp (
		mk_one l u i
	    ) plus_RDR (
		genOpApp (
		    (nlHsApp (nlHsVar unsafeRangeSize_RDR) 
			   (nlTuple [nlHsVar l, nlHsVar u] Boxed))
		) times_RDR (mk_index rest)
	   )
	mk_one l u i
	  = nlHsApps unsafeIndex_RDR [nlTuple [nlHsVar l, nlHsVar u] Boxed, nlHsVar i]

    ------------------
    single_con_inRange
      = mk_easy_FunBind tycon_loc inRange_RDR 
		[nlTuplePat [con_pat as_needed, con_pat bs_needed] Boxed, 
		 con_pat cs_needed] $
	  foldl1 and_Expr (zipWith3Equal "single_con_inRange" in_range as_needed bs_needed cs_needed)
      where
    	in_range a b c = nlHsApps inRange_RDR [nlTuple [nlHsVar a, nlHsVar b] Boxed,
					       nlHsVar c]
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
	 | T2 T


instance Read T where
  readPrec =
    parens
    ( prec 4 (
        do x           <- ReadP.step Read.readPrec
           Symbol "%%" <- Lex.lex
           y           <- ReadP.step Read.readPrec
           return (x %% y))
      +++
      prec (appPrec+1) (
	-- Note the "+1" part; "T2 T1 {f1=3}" should parse ok
	-- Record construction binds even more tightly than application
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
gen_Read_binds :: FixityEnv -> TyCon -> LHsBinds RdrName

gen_Read_binds get_fixity tycon
  = listToBag [read_prec, default_readlist, default_readlistprec]
  where
    -----------------------------------------------------------------------
    default_readlist 
	= mkVarBind loc readList_RDR     (nlHsVar readListDefault_RDR)

    default_readlistprec
	= mkVarBind loc readListPrec_RDR (nlHsVar readListPrecDefault_RDR)
    -----------------------------------------------------------------------

    loc       = getSrcSpan tycon
    data_cons = tyConDataCons tycon
    (nullary_cons, non_nullary_cons) = partition isNullarySrcDataCon data_cons
    
    read_prec = mkVarBind loc readPrec_RDR
	 		      (nlHsApp (nlHsVar parens_RDR) read_cons)

    read_cons 	          = foldr1 mk_alt (read_nullary_cons ++ read_non_nullary_cons)
    read_non_nullary_cons = map read_non_nullary_con non_nullary_cons
    
    read_nullary_cons 
      = case nullary_cons of
    	    []    -> []
    	    [con] -> [nlHsDo DoExpr [bindLex (ident_pat (data_con_str con))]
				    (result_expr con [])]
            _     -> [nlHsApp (nlHsVar choose_RDR) 
    		   	      (nlList (map mk_pair nullary_cons))]
    
    mk_pair con = nlTuple [nlHsLit (mkHsString (data_con_str con)), 
			   result_expr con []]
			  Boxed
    
    read_non_nullary_con data_con
      | is_infix  = mk_parser infix_prec  infix_stmts  body
      | is_record = mk_parser record_prec record_stmts body
--		Using these two lines instead allows the derived
--		read for infix and record bindings to read the prefix form
--      | is_infix  = mk_alt prefix_parser (mk_parser infix_prec  infix_stmts  body)
--      | is_record = mk_alt prefix_parser (mk_parser record_prec record_stmts body)
      | otherwise = prefix_parser
      where
	body = result_expr data_con as_needed
	con_str = data_con_str data_con
	
	prefix_parser = mk_parser prefix_prec prefix_stmts body
       	prefix_stmts		-- T a b c
       	  = (if not (isSym con_str) then
		  [bindLex (ident_pat con_str)]
	     else [read_punc "(", bindLex (symbol_pat con_str), read_punc ")"])
       	    ++ read_args
     	 
       	infix_stmts 		-- a %% b, or  a `T` b 
       	  = [read_a1]
   	    ++	(if isSym con_str
		 then [bindLex (symbol_pat con_str)]
		 else [read_punc "`", bindLex (ident_pat con_str), read_punc "`"])
	    ++ [read_a2]
     
       	record_stmts		-- T { f1 = a, f2 = b }
       	  = [bindLex (ident_pat (wrapOpParens con_str)),
       	     read_punc "{"]
       	    ++ concat (intersperse [read_punc ","] field_stmts)
       	    ++ [read_punc "}"]
     
       	field_stmts  = zipWithEqual "lbl_stmts" read_field labels as_needed
     
       	con_arity    = dataConSourceArity data_con
       	labels       = dataConFieldLabels data_con
       	dc_nm	     = getName data_con
       	is_infix     = dataConIsInfix data_con
	is_record    = length labels > 0
       	as_needed    = take con_arity as_RDRs
	read_args    = zipWithEqual "gen_Read_binds" read_arg as_needed (dataConOrigArgTys data_con)
       	(read_a1:read_a2:_) = read_args
	
	prefix_prec = appPrecedence
       	infix_prec  = getPrecedence get_fixity dc_nm
	record_prec = appPrecedence + 1	-- Record construction binds even more tightly
					-- than application; e.g. T2 T1 {x=2} means T2 (T1 {x=2})

    ------------------------------------------------------------------------
    --		Helpers
    ------------------------------------------------------------------------
    mk_alt e1 e2       = genOpApp e1 alt_RDR e2					-- e1 +++ e2
    mk_parser p ss b   = nlHsApps prec_RDR [nlHsIntLit p, nlHsDo DoExpr ss b]	-- prec p (do { ss ; b })
    bindLex pat	       = noLoc (mkBindStmt pat (nlHsVar lexP_RDR))		-- pat <- lexP
    con_app con as     = nlHsVarApps (getRdrName con) as			-- con as
    result_expr con as = nlHsApp (nlHsVar returnM_RDR) (con_app con as)		-- return (con as)
    
    punc_pat s   = nlConPat punc_RDR   [nlLitPat (mkHsString s)]  -- Punc 'c'
    ident_pat s  = nlConPat ident_RDR  [nlLitPat (mkHsString s)]  -- Ident "foo"
    symbol_pat s = nlConPat symbol_RDR [nlLitPat (mkHsString s)]  -- Symbol ">>"
    
    data_con_str con = occNameString (getOccName con)
    
    read_punc c = bindLex (punc_pat c)
    read_arg a ty 
	| isUnLiftedType ty = pprPanic "Error in deriving:" (text "Can't read unlifted types yet:" <+> ppr ty)
	| otherwise = noLoc (mkBindStmt (nlVarPat a) (nlHsVarApps step_RDR [readPrec_RDR]))
    
    read_field lbl a = read_lbl lbl ++
    		       [read_punc "=",
    		        noLoc (mkBindStmt (nlVarPat a) (nlHsVarApps reset_RDR [readPrec_RDR]))]

	-- When reading field labels we might encounter
	-- 	a  = 3
	-- 	_a = 3
	-- or	(#) = 4
	-- Note the parens!
    read_lbl lbl | isSym lbl_str 
		 = [read_punc "(", 
		    bindLex (symbol_pat lbl_str),
		    read_punc ")"]
		 | otherwise
		 = [bindLex (ident_pat lbl_str)]
		 where	
		   lbl_str = occNameString (getOccName lbl) 
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
gen_Show_binds :: FixityEnv -> TyCon -> LHsBinds RdrName

gen_Show_binds get_fixity tycon
  = listToBag [shows_prec, show_list]
  where
    tycon_loc = getSrcSpan tycon
    -----------------------------------------------------------------------
    show_list = mkVarBind tycon_loc showList_RDR
		  (nlHsApp (nlHsVar showList___RDR) (nlHsPar (nlHsApp (nlHsVar showsPrec_RDR) (nlHsIntLit 0))))
    -----------------------------------------------------------------------
    shows_prec = mk_FunBind tycon_loc showsPrec_RDR (map pats_etc (tyConDataCons tycon))
      where
	pats_etc data_con
	  | nullary_con =  -- skip the showParen junk...
	     ASSERT(null bs_needed)
	     ([nlWildPat, con_pat], mk_showString_app con_str)
	  | otherwise   =
	     ([a_Pat, con_pat],
		  showParen_Expr (nlHsPar (genOpApp a_Expr ge_RDR (nlHsLit (HsInt con_prec_plus_one))))
				 (nlHsPar (nested_compose_Expr show_thingies)))
	    where
	     data_con_RDR  = getRdrName data_con
	     con_arity     = dataConSourceArity data_con
	     bs_needed     = take con_arity bs_RDRs
	     arg_tys       = dataConOrigArgTys data_con		-- Correspond 1-1 with bs_needed
	     con_pat       = nlConVarPat data_con_RDR bs_needed
	     nullary_con   = con_arity == 0
             labels        = dataConFieldLabels data_con
	     lab_fields    = length labels
	     record_syntax = lab_fields > 0

	     dc_nm	    = getName data_con
	     dc_occ_nm	    = getOccName data_con
             con_str        = occNameString dc_occ_nm
	     op_con_str     = wrapOpParens con_str
	     backquote_str  = wrapOpBackquotes con_str

	     show_thingies 
		| is_infix     	= [show_arg1, mk_showString_app (" " ++ backquote_str ++ " "), show_arg2]
		| record_syntax = mk_showString_app (op_con_str ++ " {") : 
				  show_record_args ++ [mk_showString_app "}"]
		| otherwise	= mk_showString_app (op_con_str ++ " ") : show_prefix_args
                
	     show_label l = mk_showString_app (nm ++ " = ")
			-- Note the spaces around the "=" sign.  If we don't have them
			-- then we get Foo { x=-1 } and the "=-" parses as a single
			-- lexeme.  Only the space after the '=' is necessary, but
			-- it seems tidier to have them both sides.
		 where
		   occ_nm   = getOccName l
		   nm       = wrapOpParens (occNameString occ_nm)

             show_args 		     = zipWith show_arg bs_needed arg_tys
	     (show_arg1:show_arg2:_) = show_args
	     show_prefix_args	     = intersperse (nlHsVar showSpace_RDR) show_args

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
	     show_arg b arg_ty = nlHsApps showsPrec_RDR [nlHsLit (HsInt arg_prec), 
							 box_if_necy "Show" tycon (nlHsVar b) arg_ty]

		-- Fixity stuff
	     is_infix = dataConIsInfix data_con
             con_prec_plus_one = 1 + getPrec is_infix get_fixity dc_nm
	     arg_prec | record_syntax = 0	-- Record fields don't need parens
		      | otherwise     = con_prec_plus_one

wrapOpParens :: String -> String
wrapOpParens s | isSym s   = '(' : s ++ ")"
	       | otherwise = s

wrapOpBackquotes :: String -> String
wrapOpBackquotes s | isSym s   = s
		   | otherwise = '`' : s ++ "`"

isSym :: String -> Bool
isSym ""     = False
isSym (c:cs) = startsVarSym c || startsConSym c

mk_showString_app str = nlHsApp (nlHsVar showString_RDR) (nlHsLit (mkHsString str))
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
\end{code}


%************************************************************************
%*									*
\subsection{Typeable}
%*									*
%************************************************************************

From the data type

	data T a b = ....

we generate

	instance Typeable2 T where
		typeOf2 _ = mkTyConApp (mkTyConRep "T") []

We are passed the Typeable2 class as well as T

\begin{code}
gen_Typeable_binds :: TyCon -> LHsBinds RdrName
gen_Typeable_binds tycon
  = unitBag $
	mk_easy_FunBind tycon_loc 
		(mk_typeOf_RDR tycon) 	-- Name of appropriate type0f function
		[nlWildPat] 
		(nlHsApps mkTypeRep_RDR [tycon_rep, nlList []])
  where
    tycon_loc = getSrcSpan tycon
    tycon_rep = nlHsVar mkTyConRep_RDR `nlHsApp` nlHsLit (mkHsString (showSDoc (ppr tycon)))

mk_typeOf_RDR :: TyCon -> RdrName
-- Use the arity of the TyCon to make the right typeOfn function
mk_typeOf_RDR tycon = varQual_RDR tYPEABLE (mkFastString ("typeOf" ++ suffix))
		where
		  arity = tyConArity tycon
		  suffix | arity == 0 = ""
			 | otherwise  = show arity
\end{code}



%************************************************************************
%*									*
\subsection{Data}
%*									*
%************************************************************************

From the data type

  data T a b = T1 a b | T2

we generate

  $cT1 = mkDataCon $dT "T1" Prefix
  $cT2 = mkDataCon $dT "T2" Prefix
  $dT  = mkDataType "Module.T" [] [$con_T1, $con_T2]
  -- the [] is for field labels.

  instance (Data a, Data b) => Data (T a b) where
    gfoldl k z (T1 a b) = z T `k` a `k` b
    gfoldl k z T2	    = z T2
    -- ToDo: add gmapT,Q,M, gfoldr
 
    gunfold k z c = case conIndex c of
	    		I# 1# -> k (k (z T1))
    			I# 2# -> z T2

    toConstr (T1 _ _) = $cT1
    toConstr T2	      = $cT2
    
    dataTypeOf _ = $dT

\begin{code}
gen_Data_binds :: FixityEnv
	       -> TyCon 
	       -> (LHsBinds RdrName,	-- The method bindings
		   LHsBinds RdrName)	-- Auxiliary bindings
gen_Data_binds fix_env tycon
  = (listToBag [gfoldl_bind, gunfold_bind, toCon_bind, dataTypeOf_bind],
		-- Auxiliary definitions: the data type and constructors
     datatype_bind `consBag` listToBag (map mk_con_bind data_cons))
  where
    tycon_loc  = getSrcSpan tycon
    tycon_name = tyConName tycon
    data_cons  = tyConDataCons tycon
    n_cons     = length data_cons
    one_constr = n_cons == 1

	------------ gfoldl
    gfoldl_bind = mk_FunBind tycon_loc gfoldl_RDR (map gfoldl_eqn data_cons)
    gfoldl_eqn con = ([nlVarPat k_RDR, nlVarPat z_RDR, nlConVarPat con_name as_needed], 
		       foldl mk_k_app (nlHsVar z_RDR `nlHsApp` nlHsVar con_name) as_needed)
		   where
		     con_name ::  RdrName
		     con_name = getRdrName con
		     as_needed = take (dataConSourceArity con) as_RDRs
		     mk_k_app e v = nlHsPar (nlHsOpApp e k_RDR (nlHsVar v))

	------------ gunfold
    gunfold_bind = mk_FunBind tycon_loc
                              gunfold_RDR
                              [([k_Pat, z_Pat, if one_constr then nlWildPat else c_Pat], 
				gunfold_rhs)]

    gunfold_rhs 
	| one_constr = mk_unfold_rhs (head data_cons)	-- No need for case
	| otherwise  = nlHsCase (nlHsVar conIndex_RDR `nlHsApp` c_Expr) 
			        (map gunfold_alt data_cons)

    gunfold_alt dc = mkSimpleHsAlt (mk_unfold_pat dc) (mk_unfold_rhs dc)
    mk_unfold_rhs dc = foldr nlHsApp
                           (nlHsVar z_RDR `nlHsApp` nlHsVar (getRdrName dc))
                           (replicate (dataConSourceArity dc) (nlHsVar k_RDR))

    mk_unfold_pat dc	-- Last one is a wild-pat, to avoid 
			-- redundant test, and annoying warning
      | tag-fIRST_TAG == n_cons-1 = nlWildPat	-- Last constructor
      | otherwise = nlConPat intDataCon_RDR [nlLitPat (HsIntPrim (toInteger tag))]
      where 
	tag = dataConTag dc
			  
	------------ toConstr
    toCon_bind = mk_FunBind tycon_loc toConstr_RDR (map to_con_eqn data_cons)
    to_con_eqn dc = ([nlWildConPat dc], nlHsVar (mk_constr_name dc))
    
	------------ dataTypeOf
    dataTypeOf_bind = mk_easy_FunBind
                        tycon_loc
                        dataTypeOf_RDR
			[nlWildPat]
                        (nlHsVar data_type_name)

	------------  $dT

    data_type_name = mkDerivedRdrName tycon_name mkDataTOcc
    datatype_bind  = mkVarBind
                       tycon_loc
                       data_type_name
		       (           nlHsVar mkDataType_RDR 
                         `nlHsApp` nlHsLit (mkHsString (showSDoc (ppr tycon)))
                         `nlHsApp` nlList constrs
                       )
    constrs = [nlHsVar (mk_constr_name con) | con <- data_cons]


	------------  $cT1 etc
    mk_constr_name con = mkDerivedRdrName (dataConName con) mkDataCOcc
    mk_con_bind dc = mkVarBind
                       tycon_loc
                       (mk_constr_name dc) 
		       (nlHsApps mkConstr_RDR (constr_args dc))
    constr_args dc =
	 [ -- nlHsIntLit (toInteger (dataConTag dc)),		-- Tag
	   nlHsVar data_type_name,				-- DataType
	   nlHsLit (mkHsString (occNameString dc_occ)),	-- String name
           nlList  labels,					-- Field labels
	   nlHsVar fixity]					-- Fixity
	where
          labels   = map (nlHsLit . mkHsString . getOccString)
                         (dataConFieldLabels dc)
	  dc_occ   = getOccName dc
	  is_infix = isDataSymOcc dc_occ
	  fixity | is_infix  = infix_RDR
		 | otherwise = prefix_RDR

gfoldl_RDR     = varQual_RDR gENERICS FSLIT("gfoldl")
gunfold_RDR    = varQual_RDR gENERICS FSLIT("gunfold")
toConstr_RDR   = varQual_RDR gENERICS FSLIT("toConstr")
dataTypeOf_RDR = varQual_RDR gENERICS FSLIT("dataTypeOf")
mkConstr_RDR   = varQual_RDR gENERICS FSLIT("mkConstr")
mkDataType_RDR = varQual_RDR gENERICS FSLIT("mkDataType")
conIndex_RDR   = varQual_RDR gENERICS FSLIT("constrIndex")
prefix_RDR     = dataQual_RDR gENERICS FSLIT("Prefix")
infix_RDR      = dataQual_RDR gENERICS FSLIT("Infix")
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
    :: ( RdrName,	    -- (proto)Name for the thing in question
	TyCon,		    -- tycon in question
	TagThingWanted)
    -> LHsBind RdrName

gen_tag_n_con_monobind (rdr_name, tycon, GenCon2Tag)
  | lots_of_constructors
  = mk_FunBind tycon_loc rdr_name [([], get_tag_rhs)]

  | otherwise
  = mk_FunBind tycon_loc rdr_name (map mk_stuff (tyConDataCons tycon))

  where
    tycon_loc = getSrcSpan tycon

    tvs = map (mkRdrUnqual . getOccName) (tyConTyVars tycon)
	-- We can't use gerRdrName because that makes an Exact  RdrName
	-- and we can't put them in the LocalRdrEnv

	-- Give a signature to the bound variable, so 
	-- that the case expression generated by getTag is
	-- monomorphic.  In the push-enter model we get better code.
    get_tag_rhs = noLoc $ ExprWithTySig 
			(nlHsLam (mkSimpleHsAlt (nlVarPat a_RDR) 
					      (nlHsApp (nlHsVar getTag_RDR) a_Expr)))
			(noLoc (mkExplicitHsForAllTy (map (noLoc.UserTyVar) tvs) (noLoc []) con2tag_ty))

    con2tag_ty = foldl nlHsAppTy (nlHsTyVar (getRdrName tycon)) 
		       (map nlHsTyVar tvs)
		`nlHsFunTy` 
		nlHsTyVar (getRdrName intPrimTyCon)

    lots_of_constructors = tyConFamilySize tycon > mAX_FAMILY_SIZE_FOR_VEC_RETURNS

    mk_stuff :: DataCon -> ([LPat RdrName], LHsExpr RdrName)
    mk_stuff con = ([nlWildConPat con], 
		    nlHsLit (HsIntPrim (toInteger ((dataConTag con) - fIRST_TAG))))

gen_tag_n_con_monobind (rdr_name, tycon, GenTag2Con)
  = mk_FunBind (getSrcSpan tycon) rdr_name 
	[([nlConVarPat intDataCon_RDR [a_RDR]], 
	   noLoc (ExprWithTySig (nlHsApp (nlHsVar tagToEnum_RDR) a_Expr) 
			 (nlHsTyVar (getRdrName tycon))))]

gen_tag_n_con_monobind (rdr_name, tycon, GenMaxTag)
  = mkVarBind (getSrcSpan tycon) rdr_name 
		  (nlHsApp (nlHsVar intDataCon_RDR) (nlHsLit (HsIntPrim max_tag)))
  where
    max_tag =  case (tyConDataCons tycon) of
		 data_cons -> toInteger ((length data_cons) - fIRST_TAG)

\end{code}

%************************************************************************
%*									*
\subsection{Utility bits for generating bindings}
%*									*
%************************************************************************


ToDo: Better SrcLocs.

\begin{code}
compare_gen_Case ::
	  LHsExpr RdrName	-- What to do for equality
	  -> LHsExpr RdrName -> LHsExpr RdrName
	  -> LHsExpr RdrName
careful_compare_Case :: -- checks for primitive types...
	  TyCon			-- The tycon we are deriving for
	  -> Type
	  -> LHsExpr RdrName	-- What to do for equality
	  -> LHsExpr RdrName -> LHsExpr RdrName
	  -> LHsExpr RdrName

cmp_eq_Expr a b = nlHsApp (nlHsApp (nlHsVar cmp_eq_RDR) a) b
	-- Was: compare_gen_Case cmp_eq_RDR

compare_gen_Case (L _ (HsVar eq_tag)) a b | eq_tag == eqTag_RDR
  = nlHsApp (nlHsApp (nlHsVar compare_RDR) a) b	-- Simple case 
compare_gen_Case eq a b				-- General case
  = nlHsCase (nlHsPar (nlHsApp (nlHsApp (nlHsVar compare_RDR) a) b)) {-of-}
      [mkSimpleHsAlt (nlNullaryConPat ltTag_RDR) ltTag_Expr,
       mkSimpleHsAlt (nlNullaryConPat eqTag_RDR) eq,
       mkSimpleHsAlt (nlNullaryConPat gtTag_RDR) gtTag_Expr]

careful_compare_Case tycon ty eq a b
  | not (isUnLiftedType ty)
  = compare_gen_Case eq a b
  | otherwise      -- We have to do something special for primitive things...
  = nlHsIf (genOpApp a relevant_eq_op b)
	 eq
	 (nlHsIf (genOpApp a relevant_lt_op b) ltTag_Expr gtTag_Expr)
  where
    relevant_eq_op = primOpRdrName (assoc_ty_id "Ord" tycon eq_op_tbl ty)
    relevant_lt_op = primOpRdrName (assoc_ty_id "Ord" tycon lt_op_tbl ty)


box_if_necy :: String		-- The class involved
	    -> TyCon		-- The tycon involved
	    -> LHsExpr RdrName	-- The argument
	    -> Type		-- The argument type
	    -> LHsExpr RdrName	-- Boxed version of the arg
box_if_necy cls_str tycon arg arg_ty
  | isUnLiftedType arg_ty = nlHsApp (nlHsVar box_con) arg
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

eq_op_tbl :: [(Type, PrimOp)]
eq_op_tbl =
    [(charPrimTy,	CharEqOp)
    ,(intPrimTy,	IntEqOp)
    ,(wordPrimTy,	WordEqOp)
    ,(addrPrimTy,	AddrEqOp)
    ,(floatPrimTy,	FloatEqOp)
    ,(doublePrimTy,	DoubleEqOp)
    ]

lt_op_tbl :: [(Type, PrimOp)]
lt_op_tbl =
    [(charPrimTy,	CharLtOp)
    ,(intPrimTy,	IntLtOp)
    ,(wordPrimTy,	WordLtOp)
    ,(addrPrimTy,	AddrLtOp)
    ,(floatPrimTy,	FloatLtOp)
    ,(doublePrimTy,	DoubleLtOp)
    ]

box_con_tbl =
    [(charPrimTy,	getRdrName charDataCon)
    ,(intPrimTy,	getRdrName intDataCon)
    ,(wordPrimTy,	wordDataCon_RDR)
    ,(floatPrimTy,	getRdrName floatDataCon)
    ,(doublePrimTy,	getRdrName doubleDataCon)
    ]

-----------------------------------------------------------------------

and_Expr :: LHsExpr RdrName -> LHsExpr RdrName -> LHsExpr RdrName
and_Expr a b = genOpApp a and_RDR    b

-----------------------------------------------------------------------

eq_Expr :: TyCon -> Type -> LHsExpr RdrName -> LHsExpr RdrName -> LHsExpr RdrName
eq_Expr tycon ty a b = genOpApp a eq_op b
 where
   eq_op
    | not (isUnLiftedType ty) = eq_RDR
    | otherwise               = primOpRdrName (assoc_ty_id "Eq" tycon eq_op_tbl ty)
         -- we have to do something special for primitive things...
\end{code}

\begin{code}
untag_Expr :: TyCon -> [( RdrName,  RdrName)] -> LHsExpr RdrName -> LHsExpr RdrName
untag_Expr tycon [] expr = expr
untag_Expr tycon ((untag_this, put_tag_here) : more) expr
  = nlHsCase (nlHsPar (nlHsVarApps (con2tag_RDR tycon) [untag_this])) {-of-}
      [mkSimpleHsAlt (nlVarPat put_tag_here) (untag_Expr tycon more expr)]

cmp_tags_Expr ::  RdrName 		-- Comparison op
	     ->  RdrName ->  RdrName	-- Things to compare
	     -> LHsExpr RdrName 		-- What to return if true
	     -> LHsExpr RdrName		-- What to return if false
	     -> LHsExpr RdrName

cmp_tags_Expr op a b true_case false_case
  = nlHsIf (genOpApp (nlHsVar a) op (nlHsVar b)) true_case false_case

enum_from_to_Expr
	:: LHsExpr RdrName -> LHsExpr RdrName
	-> LHsExpr RdrName
enum_from_then_to_Expr
	:: LHsExpr RdrName -> LHsExpr RdrName -> LHsExpr RdrName
	-> LHsExpr RdrName

enum_from_to_Expr      f   t2 = nlHsApp (nlHsApp (nlHsVar enumFromTo_RDR) f) t2
enum_from_then_to_Expr f t t2 = nlHsApp (nlHsApp (nlHsApp (nlHsVar enumFromThenTo_RDR) f) t) t2

showParen_Expr
	:: LHsExpr RdrName -> LHsExpr RdrName
	-> LHsExpr RdrName

showParen_Expr e1 e2 = nlHsApp (nlHsApp (nlHsVar showParen_RDR) e1) e2

nested_compose_Expr :: [LHsExpr RdrName] -> LHsExpr RdrName

nested_compose_Expr []  = panic "nested_compose_expr"	-- Arg is always non-empty
nested_compose_Expr [e] = parenify e
nested_compose_Expr (e:es)
  = nlHsApp (nlHsApp (nlHsVar compose_RDR) (parenify e)) (nested_compose_Expr es)

-- impossible_Expr is used in case RHSs that should never happen.
-- We generate these to keep the desugarer from complaining that they *might* happen!
impossible_Expr = nlHsApp (nlHsVar error_RDR) (nlHsLit (mkHsString "Urk! in TcGenDeriv"))

-- illegal_Expr is used when signalling error conditions in the RHS of a derived
-- method. It is currently only used by Enum.{succ,pred}
illegal_Expr meth tp msg = 
   nlHsApp (nlHsVar error_RDR) (nlHsLit (mkHsString (meth ++ '{':tp ++ "}: " ++ msg)))

-- illegal_toEnum_tag is an extended version of illegal_Expr, which also allows you
-- to include the value of a_RDR in the error string.
illegal_toEnum_tag tp maxtag =
   nlHsApp (nlHsVar error_RDR) 
           (nlHsApp (nlHsApp (nlHsVar append_RDR)
	               (nlHsLit (mkHsString ("toEnum{" ++ tp ++ "}: tag ("))))
	            (nlHsApp (nlHsApp (nlHsApp 
		           (nlHsVar showsPrec_RDR)
			   (nlHsIntLit 0))
   		           (nlHsVar a_RDR))
			   (nlHsApp (nlHsApp 
			       (nlHsVar append_RDR)
			       (nlHsLit (mkHsString ") is outside of enumeration's range (0,")))
			       (nlHsApp (nlHsApp (nlHsApp 
					(nlHsVar showsPrec_RDR)
				        (nlHsIntLit 0))
					(nlHsVar maxtag))
					(nlHsLit (mkHsString ")"))))))

parenify e@(L _ (HsVar _)) = e
parenify e	           = mkHsPar e

-- genOpApp wraps brackets round the operator application, so that the
-- renamer won't subsequently try to re-associate it. 
genOpApp e1 op e2 = nlHsPar (nlHsOpApp e1 op e2)
\end{code}

\begin{code}
getSrcSpan = srcLocSpan . getSrcLoc
\end{code}

\begin{code}
a_RDR		= mkVarUnqual FSLIT("a")
b_RDR		= mkVarUnqual FSLIT("b")
c_RDR		= mkVarUnqual FSLIT("c")
d_RDR		= mkVarUnqual FSLIT("d")
k_RDR		= mkVarUnqual FSLIT("k")
z_RDR		= mkVarUnqual FSLIT("z")
ah_RDR		= mkVarUnqual FSLIT("a#")
bh_RDR		= mkVarUnqual FSLIT("b#")
ch_RDR		= mkVarUnqual FSLIT("c#")
dh_RDR		= mkVarUnqual FSLIT("d#")
cmp_eq_RDR	= mkVarUnqual FSLIT("cmp_eq")

as_RDRs		= [ mkVarUnqual (mkFastString ("a"++show i)) | i <- [(1::Int) .. ] ]
bs_RDRs		= [ mkVarUnqual (mkFastString ("b"++show i)) | i <- [(1::Int) .. ] ]
cs_RDRs		= [ mkVarUnqual (mkFastString ("c"++show i)) | i <- [(1::Int) .. ] ]

a_Expr		= nlHsVar a_RDR
b_Expr		= nlHsVar b_RDR
c_Expr		= nlHsVar c_RDR
ltTag_Expr	= nlHsVar ltTag_RDR
eqTag_Expr	= nlHsVar eqTag_RDR
gtTag_Expr	= nlHsVar gtTag_RDR
false_Expr	= nlHsVar false_RDR
true_Expr	= nlHsVar true_RDR

a_Pat		= nlVarPat a_RDR
b_Pat		= nlVarPat b_RDR
c_Pat		= nlVarPat c_RDR
d_Pat		= nlVarPat d_RDR
k_Pat		= nlVarPat k_RDR
z_Pat		= nlVarPat z_RDR

con2tag_RDR, tag2con_RDR, maxtag_RDR :: TyCon ->  RdrName
-- Generates Orig s RdrName, for the binding positions
con2tag_RDR tycon = mk_tc_deriv_name tycon "con2tag_"
tag2con_RDR tycon = mk_tc_deriv_name tycon "tag2con_"
maxtag_RDR  tycon = mk_tc_deriv_name tycon "maxtag_"

mk_tc_deriv_name tycon str 
  = mkDerivedRdrName tc_name mk_occ
  where
    tc_name = tyConName tycon
    mk_occ tc_occ = mkVarOccFS (mkFastString new_str)
		  where
		    new_str = str ++ occNameString tc_occ ++ "#"
\end{code}

s RdrName for PrimOps.  Can't be done in PrelNames, because PrimOp imports
PrelNames, so PrelNames can't import PrimOp.

\begin{code}
primOpRdrName op = getRdrName (primOpId op)

minusInt_RDR  = primOpRdrName IntSubOp
eqInt_RDR     = primOpRdrName IntEqOp
ltInt_RDR     = primOpRdrName IntLtOp
geInt_RDR     = primOpRdrName IntGeOp
leInt_RDR     = primOpRdrName IntLeOp
tagToEnum_RDR = primOpRdrName TagToEnumOp

error_RDR = getRdrName eRROR_ID
\end{code}
