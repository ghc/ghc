%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[CoreSyn]{A data type for the Haskell compiler midsection}

\begin{code}
#include "HsVersions.h"

module CoreSyn (
	GenCoreBinding(..), GenCoreExpr(..), GenCoreAtom(..),
	GenCoreCaseAlternatives(..), GenCoreCaseDefault(..),
	pprCoreBinding, pprCoreExpr,

	GenCoreArg(..), applyToArgs, decomposeArgs, collectArgs,

	-- and to make the interface self-sufficient ...
    ) where

import PrelInfo		( PrimOp, PrimRep
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import Type		( isPrimType, pprParendUniType, TyVar, TyCon, Type
			)
import Literal		( Literal )
import Id		( getIdUniType, isBottomingId, Id
			  IF_ATTACK_PRAGMAS(COMMA bottomIsGuaranteed)
			)
import Outputable
import Pretty
import CostCentre	( showCostCentre, CostCentre )
import Util
\end{code}

%************************************************************************
%*									*
\subsection[CoreTopBinding_and_CoreBinding]{@CoreTopBinding@ and @GenCoreBinding@}
%*									*
%************************************************************************

Core programs, bindings, expressions, etc., are parameterised with
respect to the information kept about binding and bound occurrences of
variables, called {\em binders} and {\em val_occ tyvar uvars}, respectively.  [I
don't really like the pair of names; I prefer {\em binder} and {\em
bounder}.  Or {\em binder} and {\em var}.]

A @GenCoreBinding@ is either a single non-recursive binding of a
``binder'' to an expression, or a mutually-recursive blob of same.
\begin{code}
data GenCoreBinding val_bdr val_occ tyvar uvar
  = NonRec	val_bdr (GenCoreExpr val_bdr val_occ tyvar uvar)
  | Rec		[(val_bdr, GenCoreExpr val_bdr val_occ tyvar uvar)]
\end{code}


%************************************************************************
%*									*
\subsection[GenCoreExpr]{Core expressions: @GenCoreExpr@}
%*									*
%************************************************************************

@GenCoreExpr@ is the heart of the ``core'' data types; it is
(more-or-less) boiled-down second-order polymorphic lambda calculus.
For types in the core world, we just keep using @Types@.
\begin{code}
data GenCoreExpr val_bdr val_occ tyvar uvar
     = Var    val_occ
     | Lit    Literal	-- literal constants
\end{code}

@Cons@ and @Prims@ are saturated constructor and primitive-op
applications (see the comment).  Note: @Con@s are only set up by the
simplifier (and by the desugarer when it knows what it's doing).  The
desugarer sets up constructors as applications of global @Vars@s.

\begin{code}
     | Con	Id (GenType tyvar) [GenCoreArg val_occ tyvar uvar]
		-- Saturated constructor application:
		-- The constructor is a function of the form:
		--	/\ a1 -> ... /\ am -> \ b1 -> ... \ bn ->
		-- <expr> where "/\" is a type lambda and "\" the
		-- regular kind; there will be "m" Types and
		-- "n" bindees in the Con args.
		--
		-- The type given is the result type of the application;
		-- you can figure out the argument types from it if you want.

     | Prim	PrimOp Type [GenCoreArg val_occ tyvar uvar]
		-- saturated primitive operation;
		-- comment on Cons applies here, too.
		-- The types work the same way
		-- (PrimitiveOps may be polymorphic).
\end{code}

Ye olde abstraction and application operators.
\begin{code}
     | Lam	(GenCoreBinder val_bdr tyvar uvar)
		(GenCoreExpr val_bdr val_occ tyvar uvar)

     | App	(GenCoreExpr val_bdr val_occ tyvar uvar)
		(GenCoreArg val_occ tyvar uvar)
\end{code}

Case expressions (\tr{case <expr> of <List of alternatives>}): there
are really two flavours masquerading here---those for scrutinising
{\em algebraic} types and those for {\em primitive} types.  Please see
under @GenCoreCaseAlternatives@.
\begin{code}
     | Case	(GenCoreExpr val_bdr val_occ tyvar uvar)
		(GenCoreCaseAlternatives val_bdr val_occ tyvar uvar)
\end{code}

A Core case expression \tr{case e of v -> ...} implies evaluation of
\tr{e}; it is not equivalent to \tr{let v = in ...} (as with a Haskell
\tr{case}).

Non-recursive @Lets@ only have one binding; having more than one
doesn't buy you much, and it is an easy way to mess up variable
scoping.
\begin{code}
     | Let	(GenCoreBinding val_bdr val_occ tyvar uvar)
		(GenCoreExpr binder val_occ tyvar uvar)
		-- both recursive and non-.
		-- The "GenCoreBinding" records that information
\end{code}

For cost centre scc expressions we introduce a new core construct
@SCC@ so transforming passes have to deal with it explicitly. The
alternative of using a new PrimativeOp may result in a bad
transformations of which we are unaware.
\begin{code}
     | SCC	CostCentre				    -- label of scc
		(GenCoreExpr val_bdr val_occ tyvar uvar)    -- scc expression
\end{code}


%************************************************************************
%*									*
\subsection{Case alternatives in @GenCoreExpr@}
%*									*
%************************************************************************

We have different kinds of @case@s, the differences being reflected in
the kinds of alternatives a case has.  We maintain a distinction
between cases for scrutinising algebraic datatypes, as opposed to
primitive types.  In both cases, we carry around a @TyCon@, as a
handle with which we can get info about the case (e.g., total number
of data constructors for this type).

For example:
\begin{verbatim}
let# x=e in b
\end{verbatim}
becomes
\begin{verbatim}
Case e [ BindDefaultAlt x -> b ]
\end{verbatim}

\begin{code}
data GenCoreCaseAlternatives val_bdr val_occ tyvar uvar

  = AlgAlts	[(Id,				-- alts: data constructor,
		  [val_bdr],			-- constructor's parameters,
		  GenCoreExpr val_bdr val_occ tyvar uvar)]	-- rhs.
		(GenCoreCaseDefault val_bdr val_occ tyvar uvar)

  | PrimAlts	[(Literal,			-- alts: unboxed literal,
		  GenCoreExpr val_bdr val_occ tyvar uvar)]	-- rhs.
		(GenCoreCaseDefault val_bdr val_occ tyvar uvar)

-- obvious things: if there are no alts in the list, then the default
-- can't be NoDefault.

data GenCoreCaseDefault val_bdr val_occ tyvar uvar
  = NoDefault					-- small con family: all
						-- constructor accounted for
  | BindDefault val_bdr				-- form: var -> expr;
		(GenCoreExpr val_bdr val_occ tyvar uvar)	-- "val_bdr" may or may not
						-- be used in RHS.
\end{code}

%************************************************************************
%*									*
\subsection[CoreSyn-arguments]{Core ``argument'' wrapper type}
%*									*
%************************************************************************

\begin{code}
data GenCoreAtom val_occ tyvar uvar
  = LitAtom	Literal
  | VarAtom	val_occ
  | TyAtom	(GenType tyvar)
  | UsageAtom	(Usage uvar)


===+*** fix from here down ****===
=================================

instance Outputable bindee => Outputable (GenCoreArg bindee) where
  ppr sty (ValArg atom) = ppr sty atom
  ppr sty (TypeArg ty)  = ppr sty ty
\end{code}

\begin{code}
applyToArgs :: GenCoreExpr val_bdr bindee
	    -> [GenCoreArg bindee]
	    -> GenCoreExpr val_bdr bindee

applyToArgs fun []		    = fun
applyToArgs fun (ValArg val : args) = applyToArgs (App  fun val) args
applyToArgs fun (TypeArg ty : args) = applyToArgs (CoTyApp fun ty) args
\end{code}

@decomposeArgs@ just pulls of the contiguous TypeArg-then-ValArg block
on the front of the args.  Pretty common.

\begin{code}
decomposeArgs :: [GenCoreArg bindee]
	      -> ([Type], [GenCoreAtom bindee], [GenCoreArg bindee])

decomposeArgs [] = ([],[],[])

decomposeArgs (TypeArg ty : args)
  = case (decomposeArgs args) of { (tys, vals, rest) ->
    (ty:tys, vals, rest) }

decomposeArgs (ValArg val : args)
  = case (do_vals args) of { (vals, rest) ->
    ([], val:vals, rest) }
  where
    do_vals (ValArg val : args)
      = case (do_vals args) of { (vals, rest) ->
    	(val:vals, rest) }

    do_vals args = ([], args)
\end{code}

@collectArgs@ takes an application expression, returning the function
and the arguments to which it is applied.

\begin{code}
collectArgs :: GenCoreExpr val_bdr bindee
	    -> (GenCoreExpr val_bdr bindee, [GenCoreArg bindee])

collectArgs expr
  = collect expr []
  where
    collect (App fun arg)  args = collect fun (ValArg arg : args)
    collect (CoTyApp fun ty) args = collect fun (TypeArg ty : args)
    collect other_expr args 	  = (other_expr, args)
\end{code}

%************************************************************************
%*									*
\subsection[CoreSyn-output]{Instance declarations for output}
%*									*
%************************************************************************

@pprCoreBinding@ and @pprCoreExpr@ let you give special printing
function for ``major'' val_bdrs (those next to equal signs :-),
``minor'' ones (lambda-bound, case-bound), and bindees.  They would
usually be called through some intermediary.

\begin{code}
pprCoreBinding
	:: PprStyle
	-> (PprStyle -> bndr -> Pretty)	-- to print "major" val_bdrs
	-> (PprStyle -> bndr -> Pretty) -- to print "minor" val_bdrs
	-> (PprStyle -> bdee -> Pretty) -- to print bindees
	-> GenCoreBinding bndr bdee
	-> Pretty

pprCoreBinding sty pbdr1 pbdr2 pbdee (NonRec val_bdr expr)
  = ppHang (ppCat [pbdr1 sty val_bdr, ppEquals])
	 4 (pprCoreExpr sty pbdr1 pbdr2 pbdee expr)

pprCoreBinding sty pbdr1 pbdr2 pbdee (Rec binds)
  = ppAboves [ifPprDebug sty (ppStr "{- Rec -}"),
	      ppAboves (map ppr_bind binds),
	      ifPprDebug sty (ppStr "{- end Rec -}")]
  where
    ppr_bind (val_bdr, expr)
      = ppHang (ppCat [pbdr1 sty val_bdr, ppEquals])
	     4 (pprCoreExpr sty pbdr1 pbdr2 pbdee expr)
\end{code}

\begin{code}
instance (Outputable bndr, Outputable bdee)
		=> Outputable (GenCoreBinding bndr bdee) where
    ppr sty bind = pprCoreBinding sty ppr ppr ppr bind

instance (Outputable bndr, Outputable bdee)
		=> Outputable (GenCoreExpr bndr bdee) where
    ppr sty expr = pprCoreExpr sty ppr ppr ppr expr

instance Outputable bdee => Outputable (GenCoreAtom bdee) where
    ppr sty atom = pprCoreAtom sty ppr atom
\end{code}

\begin{code}
pprCoreAtom
	:: PprStyle
	-> (PprStyle -> bdee -> Pretty) -- to print bindees
	-> GenCoreAtom bdee
	-> Pretty

pprCoreAtom sty pbdee (LitAtom lit) = ppr sty lit
pprCoreAtom sty pbdee (VarAtom v)   = pbdee sty v
\end{code}

\begin{code}
pprCoreExpr, pprParendCoreExpr
	:: PprStyle
	-> (PprStyle -> bndr -> Pretty)	-- to print "major" val_bdrs
	-> (PprStyle -> bndr -> Pretty) -- to print "minor" val_bdrs
	-> (PprStyle -> bdee -> Pretty) -- to print bindees
	-> GenCoreExpr bndr bdee
	-> Pretty

pprCoreExpr sty pbdr1 pbdr2 pbdee (Var name) = pbdee sty name

pprCoreExpr sty pbdr1 pbdr2 pbdee (Lit literal) = ppr sty literal

pprCoreExpr sty pbdr1 pbdr2 pbdee (Con con [] []) = ppr sty con

pprCoreExpr sty pbdr1 pbdr2 pbdee (Con con types args)
  = ppHang (ppBesides [ppr sty con, ppChar '!'])
	 4 (ppSep (  (map (pprParendUniType sty) types)
		  ++ (map (pprCoreAtom sty pbdee) args)))

pprCoreExpr sty pbdr1 pbdr2 pbdee (Prim prim tys args)
  = ppHang (ppBesides [ppr sty prim, ppChar '!'])
	 4 (ppSep (  (map (pprParendUniType sty) tys)
		  ++ (map (pprCoreAtom sty pbdee) args) ))

pprCoreExpr sty pbdr1 pbdr2 pbdee (Lam val_bdr expr)
  = ppHang (ppCat [ppStr "\\", pbdr2 sty val_bdr, ppStr "->"])
	 4 (pprCoreExpr sty pbdr1 pbdr2 pbdee expr)

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoTyLam tyvar expr)
  = ppHang (ppCat [ppStr "/\\", interppSP sty (tyvar:tyvars),
		   ppStr "->", pp_varss var_lists])
	   4 (pprCoreExpr sty pbdr1 pbdr2 pbdee expr_after)
  where
    (tyvars, var_lists, expr_after) = collect_tyvars expr

    collect_tyvars (CoTyLam tyv e) = ( tyv:tyvs, vs, e_after )
      where (tyvs, vs, e_after) = collect_tyvars e
    collect_tyvars e@(Lam _ _)   = ( [], vss, e_after )
      where (vss, e_after) = collect_vars e
    collect_tyvars other_e	   = ( [], [], other_e )

    collect_vars (Lam var e) = ([var]:varss, e_after)
      where (varss, e_after) = collect_vars e
    collect_vars other_e	   = ( [], other_e )

    pp_varss [] = ppNil
    pp_varss (vars:varss)
      = ppCat [ppStr "\\", ppInterleave ppSP (map (pbdr2 sty) vars),
	       ppStr "->", pp_varss varss]

pprCoreExpr sty pbdr1 pbdr2 pbdee expr@(App fun_expr atom)
  = let
	(fun, args) = collect_args expr []
    in
    ppHang (pprParendCoreExpr sty pbdr1 pbdr2 pbdee fun)
	 4 (ppSep (map (pprCoreAtom sty pbdee) args))
  where
    collect_args (App fun arg) args = collect_args fun (arg:args)
    collect_args fun		 args = (fun, args)

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoTyApp expr ty)
  = ppHang (ppBeside pp_note (pprParendCoreExpr sty pbdr1 pbdr2 pbdee expr))
	 4 (pprParendUniType sty ty)
  where
    pp_note = ifPprShowAll sty (ppStr "{-CoTyApp-} ")

pprCoreExpr sty pbdr1 pbdr2 pbdee (Case expr alts)
  = ppSep [ppSep [ppStr "case", ppNest 4 (pprParendCoreExpr sty pbdr1 pbdr2 pbdee expr),
		     ppStr "of {"],
	   ppNest 2 (pprCoreCaseAlts sty pbdr1 pbdr2 pbdee alts),
	   ppStr "}"]

-- special cases: let ... in let ...
-- ("disgusting" SLPJ)

pprCoreExpr sty pbdr1 pbdr2 pbdee (Let bind@(NonRec val_bdr rhs@(Let _ _)) body)
  = ppAboves [
      ppCat [ppStr "let {", pbdr1 sty val_bdr, ppEquals],
      ppNest 2 (pprCoreExpr sty pbdr1 pbdr2 pbdee rhs),
      ppStr "} in",
      pprCoreExpr sty pbdr1 pbdr2 pbdee body ]

pprCoreExpr sty pbdr1 pbdr2 pbdee (Let bind@(NonRec val_bdr rhs) expr@(Let _ _))
  = ppAbove
      (ppHang (ppStr "let {")
	    2 (ppCat [ppHang (ppCat [pbdr1 sty val_bdr, ppEquals])
			   4 (pprCoreExpr sty pbdr1 pbdr2 pbdee rhs),
       ppStr "} in"]))
      (pprCoreExpr sty pbdr1 pbdr2 pbdee expr)

-- general case (recursive case, too)
pprCoreExpr sty pbdr1 pbdr2 pbdee (Let bind expr)
  = ppSep [ppHang (ppStr "let {") 2 (pprCoreBinding sty pbdr1 pbdr2 pbdee bind),
	   ppHang (ppStr "} in ") 2 (pprCoreExpr    sty pbdr1 pbdr2 pbdee expr)]

pprCoreExpr sty pbdr1 pbdr2 pbdee (SCC cc expr)
  = ppSep [ ppCat [ppStr "_scc_", ppStr (showCostCentre sty True{-as string-} cc)],
	    pprParendCoreExpr sty pbdr1 pbdr2 pbdee expr ]
\end{code}

\begin{code}
pprParendCoreExpr sty pbdr1 pbdr2 pbdee e@(Var _) = pprCoreExpr sty pbdr1 pbdr2 pbdee e
pprParendCoreExpr sty pbdr1 pbdr2 pbdee e@(Lit _) = pprCoreExpr sty pbdr1 pbdr2 pbdee e
pprParendCoreExpr sty pbdr1 pbdr2 pbdee other_e
  = ppBesides [ppLparen, pprCoreExpr sty pbdr1 pbdr2 pbdee other_e, ppRparen]
\end{code}

\begin{code}
instance (Outputable bndr, Outputable bdee)
		=> Outputable (GenCoreCaseAlternatives bndr bdee) where
    ppr sty alts = pprCoreCaseAlts sty ppr ppr ppr alts
\end{code}

\begin{code}
pprCoreCaseAlts
	:: PprStyle
	-> (PprStyle -> bndr -> Pretty)	-- to print "major" val_bdrs
	-> (PprStyle -> bndr -> Pretty) -- to print "minor" val_bdrs
	-> (PprStyle -> bdee -> Pretty) -- to print bindees
	-> GenCoreCaseAlternatives bndr bdee
	-> Pretty

pprCoreCaseAlts sty pbdr1 pbdr2 pbdee (AlgAlts alts deflt)
  = ppAboves [ ppAboves (map ppr_alt alts),
	       pprCoreCaseDefault sty pbdr1 pbdr2 pbdee deflt ]
  where
    ppr_alt (con, params, expr)
      = ppHang (ppCat [ppr_con con,
		       ppInterleave ppSP (map (pbdr2 sty) params),
		       ppStr "->"])
		4 (pprCoreExpr sty pbdr1 pbdr2 pbdee expr)
      where
    	ppr_con con
    	  = if isOpLexeme con
	    then ppBesides [ppLparen, ppr sty con, ppRparen]
	    else ppr sty con

pprCoreCaseAlts sty pbdr1 pbdr2 pbdee (PrimAlts alts deflt)
  = ppAboves [ ppAboves (map ppr_alt alts),
	       pprCoreCaseDefault sty pbdr1 pbdr2 pbdee deflt ]
  where
    ppr_alt (lit, expr)
      = ppHang (ppCat [ppr sty lit, ppStr "->"])
	     4 (pprCoreExpr sty pbdr1 pbdr2 pbdee expr)
\end{code}

\begin{code}
instance (Outputable bndr, Outputable bdee)
		=> Outputable (GenCoreCaseDefault bndr bdee) where
    ppr sty deflt  = pprCoreCaseDefault sty ppr ppr ppr deflt
\end{code}

\begin{code}
pprCoreCaseDefault
	:: PprStyle
	-> (PprStyle -> bndr -> Pretty)	-- to print "major" val_bdrs
	-> (PprStyle -> bndr -> Pretty) -- to print "minor" val_bdrs
	-> (PprStyle -> bdee -> Pretty) -- to print bindees
	-> GenCoreCaseDefault bndr bdee
	-> Pretty

pprCoreCaseDefault sty pbdr1 pbdr2 pbdee NoDefault = ppNil

pprCoreCaseDefault sty pbdr1 pbdr2 pbdee (BindDefault val_bdr expr)
  = ppHang (ppCat [pbdr2 sty val_bdr, ppStr "->"])
	 4 (pprCoreExpr sty pbdr1 pbdr2 pbdee expr)
\end{code}
