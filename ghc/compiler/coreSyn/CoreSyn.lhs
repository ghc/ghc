%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[CoreSyn]{A data type for the Haskell compiler midsection}

\begin{code}
#include "HsVersions.h"

module CoreSyn (
	CoreBinding(..), CoreExpr(..), CoreAtom(..),
	CoreCaseAlternatives(..), CoreCaseDefault(..),
#ifdef DPH
	CoreParQuals(..),
	CoreParCommunicate(..),
#endif {- Data Parallel Haskell -}
	mkCoTyApp,
	pprCoreBinding, pprCoreExpr,

	CoreArg(..), applyToArgs, decomposeArgs, collectArgs,

	-- and to make the interface self-sufficient ...
	Id, UniType, TyVar, TyCon, PrimOp, BasicLit,
	PprStyle, PrettyRep, CostCentre, Maybe
    ) where

import AbsPrel		( PrimOp, PrimKind
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType	( isPrimType, pprParendUniType, TyVar, TyCon, UniType
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon COMMA cmpTyVar)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import BasicLit		( BasicLit )
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
\subsection[CoreTopBinding_and_CoreBinding]{@CoreTopBinding@ and @CoreBinding@}
%*									*
%************************************************************************

Core programs, bindings, expressions, etc., are parameterised with
respect to the information kept about binding and bound occurrences of
variables, called {\em binders} and {\em bindees}, respectively.  [I
don't really like the pair of names; I prefer {\em binder} and {\em
bounder}.  Or {\em binder} and {\em var}.]

A @CoreBinding@ is either a single non-recursive binding of a
``binder'' to an expression, or a mutually-recursive blob of same.
\begin{code}
data CoreBinding binder bindee
  = CoNonRec	binder (CoreExpr binder bindee)
  | CoRec	[(binder, CoreExpr binder bindee)]
\end{code}

%************************************************************************
%*									*
\subsection[CoreAtom]{Core atoms: @CoreAtom@}
%*									*
%************************************************************************

Same deal as @StgAtoms@, except that, for @Core@, the atomic object
may need to be applied to some types.

\begin{code}
data CoreAtom bindee
  = CoVarAtom	bindee
  | CoLitAtom   BasicLit
\end{code}

%************************************************************************
%*									*
\subsection[CoreExpr]{Core expressions: @CoreExpr@}
%*									*
%************************************************************************

@CoreExpr@ is the heart of the ``core'' data types; it is
(more-or-less) boiled-down second-order polymorphic lambda calculus.
For types in the core world, we just keep using @UniTypes@.
\begin{code}
data CoreExpr binder bindee
     = CoVar    bindee
     | CoLit    BasicLit	-- literal constants
\end{code}

@CoCons@ and @CoPrims@ are saturated constructor and primitive-op
applications (see the comment).  Note: @CoCon@s are only set up by the
simplifier (and by the desugarer when it knows what it's doing).  The
desugarer sets up constructors as applications of global @CoVars@s.
\begin{code}
     | CoCon	    Id [UniType] [CoreAtom bindee]
		    -- saturated constructor application:
		    -- the constructor is a function of the form:
		    --	/\ a1 -> ... /\ am -> \ b1 -> ... \ bn ->
		    -- <expr> where "/\" is a type lambda and "\" the
		    -- regular kind; there will be "m" UniTypes and
		    -- "n" bindees in the CoCon args.

     | CoPrim	    PrimOp [UniType] [CoreAtom bindee]
		    -- saturated primitive operation;
		    -- comment on CoCons applies here, too.
		    -- The types work the same way
		    -- (PrimitiveOps may be polymorphic).
\end{code}

Lambdas have multiple binders; this is good for the lambda lifter.
Single binders may be simulated easily with multiple binders; vice
versa is a pain.
\begin{code}
     | CoLam	    [binder]	-- lambda var_1 ... var_n -> CoreExpr
		    (CoreExpr binder bindee)
     | CoTyLam	    TyVar	-- Lambda TyVar -> CoreExpr
		    (CoreExpr binder bindee)

     | CoApp	    (CoreExpr binder bindee)
		    (CoreAtom bindee)
     | CoTyApp      (CoreExpr binder bindee)
		    UniType	-- type application
\end{code}

Case expressions (\tr{case CoreExpr of <List of alternatives>}): there
are really two flavours masquerading here---those for scrutinising
{\em algebraic} types and those for {\em primitive} types.  Please see
under @CoreCaseAlternatives@.
\begin{code}
     | CoCase	    (CoreExpr binder bindee)
		    (CoreCaseAlternatives binder bindee)
\end{code}

A Core case expression \tr{case e of v -> ...} implies evaluation of
\tr{e}; it is not equivalent to \tr{let v = in ...} (as with a Haskell
\tr{case}).

Non-recursive @CoLets@ only have one binding; having more than one
doesn't buy you much, and it is an easy way to mess up variable
scoping.
\begin{code}
     | CoLet	    (CoreBinding binder bindee)
		    (CoreExpr binder bindee)
		    -- both recursive and non-.
		    -- The "CoreBinding" records that information
\end{code}

@build@ as a function is a *PAIN*. See Andy's thesis for
futher details. This is equivalent to:
@
	build unitype (/\ tyvar \ c n -> expr)
@
\begin{code}
--ANDY:
--   | CoBuild UniType TyVar binder binder (CoreExpr binder bindee)
\end{code}

@CoZfExpr@ exist in the core language, along with their qualifiers. After
succesive optimisations to the sequential bindings, we desugar the 
@CoZfExpr@ into a subset of the core language without them - ``podization''.
\begin{code}
#ifdef DPH
     | CoZfExpr     (CoreExpr binder bindee) 
	            (CoreParQuals binder bindee)
#endif {- Data Parallel Haskell -}
\end{code}

@CoParCon@ is the parallel equivalent to the sequential @CoCon@ expression. 
They are introduced into the core syntax by a pass of the compiler that
removes the parallel ZF expressions, and {\em vectorises} ordinary sequential
functions.
\begin{code}
#ifdef DPH
      | CoParCon  Id Int [UniType] [CoreExpr binder bindee] --ToDo:DPH: CoreAtom
#endif {- Data Parallel Haskell -}
\end{code}

@CoParCommunicate@ constructs are introduced by the desugaring of parallel
ZF expressions. 
\begin{code}
#ifdef DPH
     | CoParComm
 		     Int
		    (CoreExpr binder bindee)
		    (CoreParCommunicate binder bindee)
#endif {- Data Parallel Haskell -}
\end{code}

@CoParZipWith@ constructs are introduced whenever podization fails during the
desuagring of ZF expressions. These constructs represent zipping the function
represented by the first @CoreExpr@ with the list of @CoreExpr@'s (hopefully
we wont see this that often in the resultant program :-).

\begin{code}
#ifdef DPH
     | CoParZipWith
		   Int
		   (CoreExpr binder bindee)
		   [CoreExpr binder bindee]
#endif {- Data Parallel Haskell -}
\end{code}

For cost centre scc expressions we introduce a new core construct
@CoSCC@ so transforming passes have to deal with it explicitly. The
alternative of using a new PrimativeOp may result in a bad
transformations of which we are unaware.
\begin{code}
     | CoSCC	    CostCentre			-- label of scc
		    (CoreExpr binder bindee)	-- scc expression

-- end of CoreExpr
\end{code}


%************************************************************************
%*									*
\subsection[CoreParQualifiers]{Parallel qualifiers in @CoreExpr@}
%*									*
%************************************************************************

\begin{code}
#ifdef DPH
data CoreParQuals binder bindee
   = CoAndQuals  (CoreParQuals binder bindee)
		 (CoreParQuals binder bindee)
   | CoParFilter (CoreExpr binder bindee)
   | CoDrawnGen  [binder]
		 (binder)
		 (CoreExpr binder bindee)	
   | CoIndexGen  [CoreExpr binder bindee]
   		 (binder)
		 (CoreExpr binder bindee)	
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[ParCommunicate]{Parallel Communication primitives}
%*									*
%************************************************************************
\begin{code}
#ifdef DPH
data CoreParCommunicate binder bindee
  = CoParSend	[CoreExpr binder bindee]     -- fns of form Integer -> Integer
  | CoParFetch  [CoreExpr binder bindee]     -- to determine where moved
  | CoToPodized
  | CoFromPodized
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[CoreCaseAlternatives]{Case alternatives in @CoreExpr@}
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
CoCase e [ CoBindDefaultAlt x -> b ]
\end{verbatim}

\begin{code}
data CoreCaseAlternatives binder bindee

  = CoAlgAlts	[(Id,				-- alts: data constructor,
		  [binder],			-- constructor's parameters,
		  CoreExpr binder bindee)]	-- rhs.
		(CoreCaseDefault binder bindee)

  | CoPrimAlts	[(BasicLit,			-- alts: unboxed literal,
		  CoreExpr binder bindee)]	-- rhs.
		(CoreCaseDefault binder bindee)
#ifdef DPH
  | CoParAlgAlts 
		TyCon
		Int
		[binder]
		[(Id,
		  CoreExpr binder bindee)]
		(CoreCaseDefault binder bindee)

  | CoParPrimAlts
  		TyCon
		Int
		[(BasicLit,
		  CoreExpr binder bindee)]
		(CoreCaseDefault binder bindee)
#endif {- Data Parallel Haskell -}

-- obvious things: if there are no alts in the list, then the default
-- can't be CoNoDefault.

data CoreCaseDefault binder bindee
  = CoNoDefault					-- small con family: all
						-- constructor accounted for
  | CoBindDefault   binder			-- form: var -> expr;
		    (CoreExpr binder bindee)	-- "binder" may or may not
						-- be used in RHS.
\end{code}

%************************************************************************
%*									*
\subsection[CoreSyn-arguments]{Core ``argument'' wrapper type}
%*									*
%************************************************************************

\begin{code}
data CoreArg bindee
  = TypeArg UniType
  | ValArg  (CoreAtom bindee)

instance Outputable bindee => Outputable (CoreArg bindee) where
  ppr sty (ValArg atom) = ppr sty atom
  ppr sty (TypeArg ty)  = ppr sty ty
\end{code}

\begin{code}
mkCoTyApp expr ty = CoTyApp expr ty

{- OLD: unboxed tyapps now allowed!
mkCoTyApp expr ty
#ifdef DEBUG
  | isPrimType ty && not (error_app expr)
  = pprPanic "mkCoTyApp:" (ppr PprDebug ty)
#endif
  | otherwise = ty_app
  where
    ty_app = CoTyApp expr ty

    error_app (CoVar id) {-| isBottomingId id-} = True -- debugging
	-- OOPS! can't do this because it forces
	-- the bindee type to be Id (ToDo: what?) WDP 95/02
    error_app _ = False
-}
\end{code}

\begin{code}
applyToArgs :: CoreExpr binder bindee
	    -> [CoreArg bindee]
	    -> CoreExpr binder bindee

applyToArgs fun []		    = fun
applyToArgs fun (ValArg val : args) = applyToArgs (CoApp     fun val) args
applyToArgs fun (TypeArg ty : args) = applyToArgs (mkCoTyApp fun ty)  args
\end{code}

@decomposeArgs@ just pulls of the contiguous TypeArg-then-ValArg block
on the front of the args.  Pretty common.

\begin{code}
decomposeArgs :: [CoreArg bindee]
	      -> ([UniType], [CoreAtom bindee], [CoreArg bindee])

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
collectArgs :: CoreExpr binder bindee
	    -> (CoreExpr binder bindee, [CoreArg bindee])

collectArgs expr
  = collect expr []
  where
    collect (CoApp fun arg)  args = collect fun (ValArg arg : args)
    collect (CoTyApp fun ty) args = collect fun (TypeArg ty : args)
    collect other_expr args 	  = (other_expr, args)
\end{code}

%************************************************************************
%*									*
\subsection[CoreSyn-output]{Instance declarations for output}
%*									*
%************************************************************************

@pprCoreBinding@ and @pprCoreExpr@ let you give special printing
function for ``major'' binders (those next to equal signs :-),
``minor'' ones (lambda-bound, case-bound), and bindees.  They would
usually be called through some intermediary.

\begin{code}
pprCoreBinding
	:: PprStyle
	-> (PprStyle -> bndr -> Pretty)	-- to print "major" binders
	-> (PprStyle -> bndr -> Pretty) -- to print "minor" binders
	-> (PprStyle -> bdee -> Pretty) -- to print bindees
	-> CoreBinding bndr bdee
	-> Pretty

pprCoreBinding sty pbdr1 pbdr2 pbdee (CoNonRec binder expr)
  = ppHang (ppCat [pbdr1 sty binder, ppEquals])
	 4 (pprCoreExpr sty pbdr1 pbdr2 pbdee expr)

pprCoreBinding sty pbdr1 pbdr2 pbdee (CoRec binds)
  = ppAboves [ifPprDebug sty (ppStr "{- CoRec -}"),
	      ppAboves (map ppr_bind binds),
	      ifPprDebug sty (ppStr "{- end CoRec -}")]
  where
    ppr_bind (binder, expr)
      = ppHang (ppCat [pbdr1 sty binder, ppEquals])
	     4 (pprCoreExpr sty pbdr1 pbdr2 pbdee expr)
\end{code}

\begin{code}
instance (Outputable bndr, Outputable bdee)
		=> Outputable (CoreBinding bndr bdee) where
    ppr sty bind = pprCoreBinding sty ppr ppr ppr bind

instance (Outputable bndr, Outputable bdee)
		=> Outputable (CoreExpr bndr bdee) where
    ppr sty expr = pprCoreExpr sty ppr ppr ppr expr

instance Outputable bdee => Outputable (CoreAtom bdee) where
    ppr sty atom = pprCoreAtom sty ppr atom
\end{code}

\begin{code}
pprCoreAtom
	:: PprStyle
	-> (PprStyle -> bdee -> Pretty) -- to print bindees
	-> CoreAtom bdee
	-> Pretty

pprCoreAtom sty pbdee (CoLitAtom lit) = ppr sty lit
pprCoreAtom sty pbdee (CoVarAtom v)   = pbdee sty v
\end{code}

\begin{code}
pprCoreExpr, pprParendCoreExpr
	:: PprStyle
	-> (PprStyle -> bndr -> Pretty)	-- to print "major" binders
	-> (PprStyle -> bndr -> Pretty) -- to print "minor" binders
	-> (PprStyle -> bdee -> Pretty) -- to print bindees
	-> CoreExpr bndr bdee
	-> Pretty

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoVar name) = pbdee sty name

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoLit literal) = ppr sty literal

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoCon con [] []) = ppr sty con

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoCon con types args)
  = ppHang (ppBesides [ppr sty con, ppChar '!'])
	 4 (ppSep (  (map (pprParendUniType sty) types)
		  ++ (map (pprCoreAtom sty pbdee) args)))

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoPrim prim tys args)
  = ppHang (ppBesides [ppr sty prim, ppChar '!'])
	 4 (ppSep (  (map (pprParendUniType sty) tys)
		  ++ (map (pprCoreAtom sty pbdee) args) ))

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoLam binders expr)
  = ppHang (ppCat [ppStr "\\", ppInterleave ppSP (map (pbdr2 sty) binders), ppStr "->"])
	 4 (pprCoreExpr sty pbdr1 pbdr2 pbdee expr)

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoTyLam tyvar expr)
  = ppHang (ppCat [ppStr "/\\", interppSP sty (tyvar:tyvars),
		   ppStr "->", pp_varss var_lists])
	   4 (pprCoreExpr sty pbdr1 pbdr2 pbdee expr_after)
  where
    (tyvars, var_lists, expr_after) = collect_tyvars expr

    collect_tyvars (CoTyLam tyv e) = ( tyv:tyvs, vs, e_after )
      where (tyvs, vs, e_after) = collect_tyvars e
    collect_tyvars e@(CoLam _ _)   = ( [], vss, e_after )
      where (vss, e_after) = collect_vars e
    collect_tyvars other_e	   = ( [], [], other_e )

    collect_vars (CoLam vars e) = (vars:varss, e_after)
      where (varss, e_after) = collect_vars e
    collect_vars other_e	   = ( [], other_e )

    pp_varss [] = ppNil
    pp_varss (vars:varss)
      = ppCat [ppStr "\\", ppInterleave ppSP (map (pbdr2 sty) vars),
	       ppStr "->", pp_varss varss]

pprCoreExpr sty pbdr1 pbdr2 pbdee expr@(CoApp fun_expr atom)
  = let
	(fun, args) = collect_args expr []
    in
    ppHang (pprParendCoreExpr sty pbdr1 pbdr2 pbdee fun)
	 4 (ppSep (map (pprCoreAtom sty pbdee) args))
  where
    collect_args (CoApp fun arg) args = collect_args fun (arg:args)
    collect_args fun		 args = (fun, args)

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoTyApp expr ty)
  = ppHang (ppBeside pp_note (pprParendCoreExpr sty pbdr1 pbdr2 pbdee expr))
	 4 (pprParendUniType sty ty)
  where
    pp_note = ifPprShowAll sty (ppStr "{-CoTyApp-} ")

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoCase expr alts)
  = ppSep [ppSep [ppStr "case", ppNest 4 (pprParendCoreExpr sty pbdr1 pbdr2 pbdee expr),
		     ppStr "of {"],
	   ppNest 2 (pprCoreCaseAlts sty pbdr1 pbdr2 pbdee alts),
	   ppStr "}"]

-- special cases: let ... in let ...
-- ("disgusting" SLPJ)

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoLet bind@(CoNonRec binder rhs@(CoLet _ _)) body)
  = ppAboves [
      ppCat [ppStr "let {", pbdr1 sty binder, ppEquals],
      ppNest 2 (pprCoreExpr sty pbdr1 pbdr2 pbdee rhs),
      ppStr "} in",
      pprCoreExpr sty pbdr1 pbdr2 pbdee body ]

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoLet bind@(CoNonRec binder rhs) expr@(CoLet _ _))
  = ppAbove
      (ppHang (ppStr "let {")
	    2 (ppCat [ppHang (ppCat [pbdr1 sty binder, ppEquals])
			   4 (pprCoreExpr sty pbdr1 pbdr2 pbdee rhs),
       ppStr "} in"]))
      (pprCoreExpr sty pbdr1 pbdr2 pbdee expr)

-- general case (recursive case, too)
pprCoreExpr sty pbdr1 pbdr2 pbdee (CoLet bind expr)
  = ppSep [ppHang (ppStr "let {") 2 (pprCoreBinding sty pbdr1 pbdr2 pbdee bind),
	   ppHang (ppStr "} in ") 2 (pprCoreExpr    sty pbdr1 pbdr2 pbdee expr)]

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoSCC cc expr)
  = ppSep [ ppCat [ppStr "_scc_", ppStr (showCostCentre sty True{-as string-} cc)],
	    pprParendCoreExpr sty pbdr1 pbdr2 pbdee expr ]
#ifdef DPH
pprCoreExpr sty pbdr1 pbdr2 pbdee (CoZfExpr expr quals)
    = ppHang (ppCat [ppStr "<<" , pprCoreExpr sty pbdr1 pbdr2 pbdee expr , ppStr "|"])
         4 (ppSep [pprParQuals sty pbdr1 pbdr2 pbdee quals, ppStr ">>"])

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoParCon con dim types args)
  = ppHang (ppBesides [ppr sty con, ppStr "!<<" , ppr sty dim , ppStr ">>"])
	   4 (ppSep (  (map (pprParendUniType sty) types)
		    ++ (map (pprParendCoreExpr sty pbdr1 pbdr2 pbdee) args) ))

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoParComm dim expr comType)
  = ppSep [ppSep [ppStr "COMM",
		  ppNest 2 (pprParendCoreExpr sty pbdr1 pbdr2 pbdee expr),ppStr "{"],
	   ppNest 2 (ppr sty comType),
	   ppStr "}"]

pprCoreExpr sty pbdr1 pbdr2 pbdee (CoParZipWith dim expr exprs)
  = ppHang (ppBesides [ ppStr "CoParZipWith {" , ppr sty dim , ppStr "}",
		        pprParendCoreExpr sty pbdr1 pbdr2 pbdee expr])
	   4 (ppr sty exprs)
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
pprParendCoreExpr sty pbdr1 pbdr2 pbdee e@(CoVar _) = pprCoreExpr sty pbdr1 pbdr2 pbdee e
pprParendCoreExpr sty pbdr1 pbdr2 pbdee e@(CoLit _) = pprCoreExpr sty pbdr1 pbdr2 pbdee e
pprParendCoreExpr sty pbdr1 pbdr2 pbdee other_e
  = ppBesides [ppLparen, pprCoreExpr sty pbdr1 pbdr2 pbdee other_e, ppRparen]
\end{code}

\begin{code}
instance (Outputable bndr, Outputable bdee)
		=> Outputable (CoreCaseAlternatives bndr bdee) where
    ppr sty alts = pprCoreCaseAlts sty ppr ppr ppr alts
\end{code}

\begin{code}
pprCoreCaseAlts
	:: PprStyle
	-> (PprStyle -> bndr -> Pretty)	-- to print "major" binders
	-> (PprStyle -> bndr -> Pretty) -- to print "minor" binders
	-> (PprStyle -> bdee -> Pretty) -- to print bindees
	-> CoreCaseAlternatives bndr bdee
	-> Pretty

pprCoreCaseAlts sty pbdr1 pbdr2 pbdee (CoAlgAlts alts deflt)
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

pprCoreCaseAlts sty pbdr1 pbdr2 pbdee (CoPrimAlts alts deflt)
  = ppAboves [ ppAboves (map ppr_alt alts),
	       pprCoreCaseDefault sty pbdr1 pbdr2 pbdee deflt ]
  where
    ppr_alt (lit, expr)
      = ppHang (ppCat [ppr sty lit, ppStr "->"])
	     4 (pprCoreExpr sty pbdr1 pbdr2 pbdee expr)

#ifdef DPH
-- ToDo: niceties of printing
-- using special binder/bindee printing funs, rather than just "ppr"

pprCoreCaseAlts sty pbdr1 pbdr2 pbdee (CoParAlgAlts tycon dim params alts deflt)
  = ppAboves [ ifPprShowAll sty (ppr sty tycon),
	       ppBeside (ppCat (map (ppr sty) params))
	       		(ppCat [ppStr "|" , ppr sty dim , ppStr "|"]),
	       ppAboves (map (ppr_alt sty) alts),
	       ppr sty deflt ]
  where
    ppr_alt sty (con, expr)
      = ppHang (ppCat [ppStr "\\/", ppr_con sty con, ppStr "->"])
		4 (ppr sty expr)
      where
    	ppr_con sty con
    	  = if isOpLexeme con
	    then ppBesides [ppLparen, ppr sty con, ppRparen]
	    else ppr sty con

pprCoreCaseAlts sty pbdr1 pbdr2 pbdee (CoParPrimAlts tycon dim alts deflt)
  = ppAboves [ ifPprShowAll sty (ppr sty tycon),
	       ppCat [ppStr "|" , ppr sty dim , ppStr "|"],
	       ppAboves (map (ppr_alt sty) alts),
	       ppr sty deflt ]
  where
    ppr_alt sty (lit, expr)
      = ppHang (ppCat [ppStr "\\/", ppr sty lit, ppStr "->"]) 4 (ppr sty expr)

#endif /* Data Parallel Haskell */
\end{code}

\begin{code}
instance (Outputable bndr, Outputable bdee)
		=> Outputable (CoreCaseDefault bndr bdee) where
    ppr sty deflt  = pprCoreCaseDefault sty ppr ppr ppr deflt
\end{code}

\begin{code}
pprCoreCaseDefault
	:: PprStyle
	-> (PprStyle -> bndr -> Pretty)	-- to print "major" binders
	-> (PprStyle -> bndr -> Pretty) -- to print "minor" binders
	-> (PprStyle -> bdee -> Pretty) -- to print bindees
	-> CoreCaseDefault bndr bdee
	-> Pretty

pprCoreCaseDefault sty pbdr1 pbdr2 pbdee CoNoDefault = ppNil

pprCoreCaseDefault sty pbdr1 pbdr2 pbdee (CoBindDefault binder expr)
  = ppHang (ppCat [pbdr2 sty binder, ppStr "->"])
	 4 (pprCoreExpr sty pbdr1 pbdr2 pbdee expr)
\end{code}

\begin{code}
#ifdef DPH
instance (Outputable bndr, Outputable bdee)
		=> Outputable (CoreParQuals bndr bdee) where
    ppr sty qual = pprParQuals sty ppr ppr ppr qual

pprParQuals sty pbdr1 pbdr2 pbdee (CoAndQuals x y) 
     = ppAboves [(ppBesides [pprParQuals sty pbdr1 pbdr2 pbdee x , ppComma]) , pprParQuals sty pbdr1 pbdr2 pbdee y]

pprParQuals sty pbdr1 pbdr2 pbdee (CoDrawnGen pats pat expr)
     = ppCat [ppStr "(|",
              ppInterleave ppComma (map (ppr sty) pats),
              ppSemi, ppr sty pat,ppStr "|)",
              ppStr "<<-", pprCoreExpr sty pbdr1 pbdr2 pbdee expr]

pprParQuals sty pbdr1 pbdr2 pbdee (CoIndexGen exprs pat expr)
     = ppCat [ppStr "(|",
              ppInterleave ppComma (map (pprCoreExpr sty pbdr1 pbdr2 pbdee) exprs),
              ppSemi, ppr sty pat,ppStr "|)",
              ppStr "<<=", pprCoreExpr sty pbdr1 pbdr2 pbdee expr]

pprParQuals sty pbdr1 pbdr2 pbdee (CoParFilter expr)
     = pprParendCoreExpr sty pbdr1 pbdr2 pbdee expr
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
#ifdef DPH
instance (Outputable bndr, Outputable bdee)
		=> Outputable (CoreParCommunicate bndr bdee) where
    ppr sty c = pprCoreParCommunicate sty ppr ppr ppr c

pprCoreParCommunicate sty pbdr1 pbdr2 pbdee (CoParSend fns)
  = ppHang 
       (ppStr "SEND") 
	4 
	(ppAboves (zipWith ppSendFns fns ([1..]::[Int])))
  where
     ppSendFns expr dim 
        = ppCat [ppStr "Dim" , ppr sty dim , ppStr "=" , 
		 pprParendCoreExpr sty pbdr1 pbdr2 pbdee expr ]

pprCoreParCommunicate sty pbdr1 pbdr2 pbdee (CoParFetch fns)
  = ppHang 
	(ppStr "FETCH") 
	4 
	(ppAboves (zipWith ppSendFns fns ([1..]::[Int])))
  where
     ppSendFns expr dim 
        = ppCat [ppStr "Dim" , ppr sty dim , ppStr "=" , 
		 pprParendCoreExpr sty pbdr1 pbdr2 pbdee expr ]

pprCoreParCommunicate sty pbdr1 pbdr2 pbdee (CoToPodized)
  = ppStr "ConvertToPodized"

pprCoreParCommunicate sty pbdr1 pbdr2 pbdee (CoFromPodized)
  = ppStr "ConvertFromPodized"
#endif {- Data Parallel Haskell -}
\end{code}
