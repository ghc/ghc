%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[CoreSyn]{A data type for the Haskell compiler midsection}

\begin{code}
#include "HsVersions.h"

module CoreSyn (
	GenCoreBinding(..), GenCoreExpr(..),
	GenCoreArg(..), GenCoreBinder(..), GenCoreCaseAlts(..),
	GenCoreCaseDefault(..),
	Coercion(..),

	bindersOf, pairsFromCoreBinds, rhssOfBind,

	mkGenApp, mkValApp, mkTyApp, mkUseApp,
	mkApp, mkCon, mkPrim,
	mkValLam, mkTyLam, mkUseLam,
	mkLam,
	collectBinders, collectUsageAndTyBinders, collectValBinders, 
	isValBinder, notValBinder,
	
	collectArgs, initialTyArgs, initialValArgs, isValArg, notValArg, numValArgs,

	mkCoLetAny, mkCoLetNoUnboxed, mkCoLetUnboxedToCase,
	mkCoLetsAny, mkCoLetsNoUnboxed, mkCoLetsUnboxedToCase,
	mkCoLetrecAny, mkCoLetrecNoUnboxed,

	rhssOfAlts,

	-- Common type instantiation...
	SYN_IE(CoreBinding),
	SYN_IE(CoreExpr),
	SYN_IE(CoreBinder),
	SYN_IE(CoreArg),
	SYN_IE(CoreCaseAlts),
	SYN_IE(CoreCaseDefault),

	-- And not-so-common type instantiations...
	SYN_IE(TaggedCoreBinding),
	SYN_IE(TaggedCoreExpr),
	SYN_IE(TaggedCoreBinder),
	SYN_IE(TaggedCoreArg),
	SYN_IE(TaggedCoreCaseAlts),
	SYN_IE(TaggedCoreCaseDefault),

	SYN_IE(SimplifiableCoreBinding),
	SYN_IE(SimplifiableCoreExpr),
	SYN_IE(SimplifiableCoreBinder),
	SYN_IE(SimplifiableCoreArg),
	SYN_IE(SimplifiableCoreCaseAlts),
	SYN_IE(SimplifiableCoreCaseDefault)
    ) where

IMP_Ubiq(){-uitous-}

import CostCentre	( showCostCentre, CostCentre )
import Id		( idType, GenId{-instance Eq-}, SYN_IE(Id) )
import Type		( isUnboxedType,GenType, SYN_IE(Type) )
import TyVar		( GenTyVar, SYN_IE(TyVar) )
import Usage		( SYN_IE(UVar),GenUsage,SYN_IE(Usage) )
import Util		( panic, assertPanic {-pprTrace:ToDo:rm-} )
#if __GLASGOW_HASKELL__ >= 202
import Literal          ( Literal )
import BinderInfo       ( BinderInfo )
import PrimOp           ( PrimOp )
#endif
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

\begin{code}
bindersOf :: GenCoreBinding val_bdr val_occ tyvar uvar -> [val_bdr]

pairsFromCoreBinds ::
  [GenCoreBinding val_bdr val_occ tyvar uvar] ->
  [(val_bdr, GenCoreExpr val_bdr val_occ tyvar uvar)]

rhssOfBind :: GenCoreBinding val_bdr val_occ tyvar uvar -> [GenCoreExpr val_bdr val_occ tyvar uvar]

bindersOf (NonRec binder _) = [binder]
bindersOf (Rec pairs)       = [binder | (binder, _) <- pairs]

pairsFromCoreBinds []		       = []
pairsFromCoreBinds ((NonRec b e) : bs) = (b,e) :  pairsFromCoreBinds bs
pairsFromCoreBinds ((Rec  pairs) : bs) = pairs ++ pairsFromCoreBinds bs

rhssOfBind (NonRec _ rhs) = [rhs]
rhssOfBind (Rec pairs)    = [rhs | (_,rhs) <- pairs]
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
     | Con	Id [GenCoreArg val_occ tyvar uvar]
		-- Saturated constructor application:
		-- The constructor is a function of the form:
		--	/\ a1 -> ... /\ am -> \ b1 -> ... \ bn ->
		-- <expr> where "/\" is a type lambda and "\" the
		-- regular kind; there will be "m" Types and
		-- "n" bindees in the Con args.

     | Prim	PrimOp [GenCoreArg val_occ tyvar uvar]
		-- saturated primitive operation;

		-- comment on Cons applies here, too.
\end{code}

Ye olde abstraction and application operators.
\begin{code}
     | Lam	(GenCoreBinder val_bdr tyvar uvar)
		(GenCoreExpr   val_bdr val_occ tyvar uvar)

     | App	(GenCoreExpr val_bdr val_occ tyvar uvar)
		(GenCoreArg  val_occ tyvar uvar)
\end{code}

Case expressions (\tr{case <expr> of <List of alternatives>}): there
are really two flavours masquerading here---those for scrutinising
{\em algebraic} types and those for {\em primitive} types.  Please see
under @GenCoreCaseAlts@.
\begin{code}
     | Case	(GenCoreExpr val_bdr val_occ tyvar uvar)
		(GenCoreCaseAlts val_bdr val_occ tyvar uvar)
\end{code}

A Core case expression \tr{case e of v -> ...} implies evaluation of
\tr{e}; it is not equivalent to \tr{let v = in ...} (as with a Haskell
\tr{case}).

Non-recursive @Lets@ only have one binding; having more than one
doesn't buy you much, and it is an easy way to mess up variable
scoping.
\begin{code}
     | Let	(GenCoreBinding val_bdr val_occ tyvar uvar)
		(GenCoreExpr val_bdr val_occ tyvar uvar)
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

Coercions arise from uses of the constructor of a @newtype@
declaration, either in construction (resulting in a @CoreceIn@) or
pattern matching (resulting in a @CoerceOut@).

\begin{code}
    | Coerce	Coercion
		(GenType tyvar uvar)		-- Type of the whole expression
		(GenCoreExpr val_bdr val_occ tyvar uvar)
\end{code}

\begin{code}
data Coercion	= CoerceIn Id		-- Apply this constructor
		| CoerceOut Id		-- Strip this constructor
\end{code}


%************************************************************************
%*									*
\subsection{Core-constructing functions with checking}
%*									*
%************************************************************************

When making @Lets@, we may want to take evasive action if the thing
being bound has unboxed type. We have different variants ...

@mkCoLet(s|rec)Any@ 		let-binds any binding, regardless of type
@mkCoLet(s|rec)NoUnboxed@ 	prohibits unboxed bindings
@mkCoLet(s)UnboxedToCase@ 	converts an unboxed binding to a case
				(unboxed bindings in a letrec are still prohibited)

\begin{code}
mkCoLetAny :: GenCoreBinding Id Id tyvar uvar
	   -> GenCoreExpr    Id Id tyvar uvar
	   -> GenCoreExpr    Id Id tyvar uvar
mkCoLetsAny :: [GenCoreBinding Id Id tyvar uvar] ->
		GenCoreExpr Id Id tyvar uvar ->
		GenCoreExpr Id Id tyvar uvar

mkCoLetrecAny :: [(val_bdr, GenCoreExpr val_bdr val_occ tyvar uvar)]
	      -> GenCoreExpr val_bdr val_occ tyvar uvar
	      -> GenCoreExpr val_bdr val_occ tyvar uvar

mkCoLetrecAny []    body = body
mkCoLetrecAny binds body = Let (Rec binds) body

mkCoLetsAny []    expr = expr
mkCoLetsAny binds expr = foldr mkCoLetAny expr binds

mkCoLetAny bind@(Rec binds)         body = mkCoLetrecAny binds body
mkCoLetAny bind@(NonRec binder rhs) body = Let bind body
\end{code}

\begin{code}
mkCoLetNoUnboxed bind@(Rec binds) body
  = mkCoLetrecNoUnboxed binds body

mkCoLetNoUnboxed bind@(NonRec binder rhs) body
  = --ASSERT (not (isUnboxedType (idType binder)))
    case body of
      Var binder2 | binder == binder2
	 -> rhs   -- hey, I have the rhs
      other
	 -> Let bind body

mkCoLetsNoUnboxed []    expr = expr
mkCoLetsNoUnboxed binds expr = foldr mkCoLetNoUnboxed expr binds

mkCoLetrecNoUnboxed []    body = body
mkCoLetrecNoUnboxed binds body
  = ASSERT (all is_boxed_bind binds)
    Let (Rec binds) body
  where
    is_boxed_bind (binder, rhs)
      = (not . isUnboxedType . idType) binder
\end{code}

\begin{code}
mkCoLetUnboxedToCase bind@(Rec binds) body
  = mkCoLetrecNoUnboxed binds body

mkCoLetUnboxedToCase bind@(NonRec binder rhs) body
  = case body of
      Var binder2 | binder == binder2
	 -> rhs   -- hey, I have the rhs
      other
	 -> if (not (isUnboxedType (idType binder))) then
		Let bind body		 -- boxed...
	    else
		Case rhs		  -- unboxed...
	    	  (PrimAlts []
		    (BindDefault binder body))

mkCoLetsUnboxedToCase []    expr = expr
mkCoLetsUnboxedToCase binds expr = foldr mkCoLetUnboxedToCase expr binds
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
data GenCoreCaseAlts val_bdr val_occ tyvar uvar
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

\begin{code}
rhssOfAlts (AlgAlts alts deflt)  = rhssOfDeflt deflt ++ [rhs | (_,_,rhs) <- alts]
rhssOfAlts (PrimAlts alts deflt) = rhssOfDeflt deflt ++ [rhs | (_,rhs)   <- alts]

rhssOfDeflt NoDefault		= []
rhssOfDeflt (BindDefault _ rhs) = [rhs]
\end{code}

%************************************************************************
%*									*
\subsection{Core binders}
%*									*
%************************************************************************

\begin{code}
data GenCoreBinder val_bdr tyvar uvar
  = ValBinder	val_bdr
  | TyBinder	tyvar
  | UsageBinder	uvar

isValBinder (ValBinder _) = True
isValBinder _		  = False

notValBinder = not . isValBinder
\end{code}

Clump Lams together if possible.

\begin{code}
mkValLam :: [val_bdr]
	 -> GenCoreExpr val_bdr val_occ tyvar uvar
	 -> GenCoreExpr val_bdr val_occ tyvar uvar
mkTyLam  :: [tyvar]
	 -> GenCoreExpr val_bdr val_occ tyvar uvar
	 -> GenCoreExpr val_bdr val_occ tyvar uvar
mkUseLam :: [uvar]
	 -> GenCoreExpr val_bdr val_occ tyvar uvar
	 -> GenCoreExpr val_bdr val_occ tyvar uvar

mkValLam binders body = foldr (Lam . ValBinder)   body binders
mkTyLam  binders body = foldr (Lam . TyBinder)    body binders
mkUseLam binders body = foldr (Lam . UsageBinder) body binders

mkLam :: [tyvar] -> [val_bdr] -- ToDo: could add a [uvar] arg...
	 -> GenCoreExpr val_bdr val_occ tyvar uvar
	 -> GenCoreExpr val_bdr val_occ tyvar uvar

mkLam tyvars valvars body
  = mkTyLam tyvars (mkValLam valvars body)
\end{code}

We often want to strip off leading lambdas before getting down to
business.  @collectBinders@ is your friend.

We expect (by convention) usage-, type-, and value- lambdas in that
order.

\begin{code}
collectBinders ::
  GenCoreExpr val_bdr val_occ tyvar uvar ->
  ([uvar], [tyvar], [val_bdr], GenCoreExpr val_bdr val_occ tyvar uvar)

collectBinders expr
  = case collectValBinders body1 of { (vals,body) -> (usages, tyvars, vals, body) }
  where
    (usages, tyvars, body1) = collectUsageAndTyBinders expr
--    (vals, body) 	    = collectValBinders body1


collectUsageAndTyBinders expr
  = case usages expr [] of
      ([],tyvars,body) -> ([],tyvars,body)
      v                -> v
  where
    usages (Lam (UsageBinder u) body) uacc = usages body (u:uacc)
    usages other uacc
      = case (tyvars other []) of { (tacc, expr) ->
	(reverse uacc, tacc, expr) }

    tyvars (Lam (TyBinder t) body) tacc = tyvars body (t:tacc)
    tyvars other tacc
      = ASSERT(not (usage_lambda other))
	(reverse tacc, other)

    ---------------------------------------
    usage_lambda (Lam (UsageBinder _) _) = True
    usage_lambda _			 = False

    tyvar_lambda (Lam (TyBinder _) _)    = True
    tyvar_lambda _			 = False


collectValBinders :: GenCoreExpr val_bdr val_occ tyvar uvar ->
		     ([val_bdr], GenCoreExpr val_bdr val_occ tyvar uvar)
collectValBinders expr
  = case go [] expr of
      ([],body) -> ([],body)
      v         -> v
  where
    go acc (Lam (ValBinder v) b) = go (v:acc) b
    go acc body 		 = (reverse acc, body)

\end{code}

%************************************************************************
%*									*
\subsection{Core arguments (atoms)}
%*									*
%************************************************************************

\begin{code}
data GenCoreArg val_occ tyvar uvar
  = LitArg	Literal
  | VarArg	val_occ
  | TyArg	(GenType tyvar uvar)
  | UsageArg	(GenUsage uvar)
\end{code}

General and specific forms:
\begin{code}
mkGenApp :: GenCoreExpr val_bdr val_occ tyvar uvar
	 -> [GenCoreArg val_occ tyvar uvar]
	 -> GenCoreExpr val_bdr val_occ tyvar uvar
mkTyApp  :: GenCoreExpr val_bdr val_occ tyvar uvar
	 -> [GenType tyvar uvar]
	 -> GenCoreExpr val_bdr val_occ tyvar uvar
mkUseApp :: GenCoreExpr val_bdr val_occ tyvar uvar
	 -> [GenUsage uvar]
	 -> GenCoreExpr val_bdr val_occ tyvar uvar
mkValApp :: GenCoreExpr val_bdr val_occ tyvar uvar
	 -> [GenCoreArg val_occ tyvar uvar] -- but we ASSERT they are LitArg or VarArg
	 -> GenCoreExpr val_bdr val_occ tyvar uvar

mkGenApp f args = foldl App		  		   f args
mkTyApp  f args = foldl (\ e a -> App e (TyArg a))	   f args
mkUseApp f args = foldl (\ e a -> App e (UsageArg a))	   f args
mkValApp f args = foldl (\ e a -> App e (is_Lit_or_Var a)) f args

#ifndef DEBUG
is_Lit_or_Var a = a
#else
is_Lit_or_Var a
  = if isValArg a then a else panic "CoreSyn.mkValApps:not LitArg or VarArg"
#endif

isValArg (LitArg _) = True  -- often used for sanity-checking
isValArg (VarArg _) = True
isValArg _	    = False

notValArg = not . isValArg -- exists only because it's a common use of isValArg

numValArgs as = length [ a | a <- as, isValArg a ] -- again, convenience
\end{code}

\begin{code}
mkApp  fun = mk_thing (mkGenApp fun)
mkCon  con = mk_thing (Con      con)
mkPrim op  = mk_thing (Prim     op)

mk_thing thing uses tys vals
  = thing (map UsageArg uses ++ map TyArg tys ++ map is_Lit_or_Var vals)
\end{code}

@collectArgs@ takes an application expression, returning the function
and the arguments to which it is applied.

\begin{code}
collectArgs :: GenCoreExpr val_bdr val_occ tyvar uvar
	    -> (GenCoreExpr val_bdr val_occ tyvar uvar,
		[GenUsage uvar],
		[GenType tyvar uvar],
	        [GenCoreArg val_occ tyvar uvar]{-ValArgs-})

collectArgs expr
  = valvars expr []
  where
    valvars (App fun v) vacc | isValArg v = valvars fun (v:vacc)
    valvars fun vacc
      = case (tyvars fun []) of { (expr, uacc, tacc) ->
	(expr, uacc, tacc, vacc) }

    tyvars (App fun (TyArg t))    tacc = tyvars fun (t:tacc)
    tyvars fun tacc
      = case (usages fun []) of { (expr, uacc) ->
	(expr, uacc, tacc) }

    usages (App fun (UsageArg u)) uacc = usages fun (u:uacc)
    usages fun uacc
      = (fun,uacc)
\end{code}


\begin{code}
initialTyArgs :: [GenCoreArg val_occ tyvar uvar]
	      -> ([GenType tyvar uvar], [GenCoreArg val_occ tyvar uvar])
initialTyArgs (TyArg ty : args) = (ty:tys, args') 
				where
				  (tys, args') = initialTyArgs args
initialTyArgs other 		= ([],other)

initialValArgs :: [GenCoreArg val_occ tyvar uvar]
	      -> ([GenCoreArg val_occ tyvar uvar], [GenCoreArg val_occ tyvar uvar])
initialValArgs args = span isValArg args
\end{code}


%************************************************************************
%*									*
\subsection{The main @Core*@ instantiation of the @GenCore*@ types}
%*									*
%************************************************************************

\begin{code}
type CoreBinding = GenCoreBinding  Id Id TyVar UVar
type CoreExpr    = GenCoreExpr     Id Id TyVar UVar
type CoreBinder	 = GenCoreBinder   Id    TyVar UVar
type CoreArg     = GenCoreArg         Id TyVar UVar

type CoreCaseAlts    = GenCoreCaseAlts    Id Id TyVar UVar
type CoreCaseDefault = GenCoreCaseDefault Id Id TyVar UVar
\end{code}

%************************************************************************
%*									*
\subsection{The @TaggedCore*@ instantiation of the @GenCore*@ types}
%*									*
%************************************************************************

Binders are ``tagged'' with a \tr{t}:
\begin{code}
type Tagged t = (Id, t)

type TaggedCoreBinding t = GenCoreBinding (Tagged t) Id TyVar UVar
type TaggedCoreExpr    t = GenCoreExpr    (Tagged t) Id TyVar UVar
type TaggedCoreBinder  t = GenCoreBinder  (Tagged t)    TyVar UVar
type TaggedCoreArg     t = GenCoreArg                Id TyVar UVar

type TaggedCoreCaseAlts    t = GenCoreCaseAlts    (Tagged t) Id TyVar UVar
type TaggedCoreCaseDefault t = GenCoreCaseDefault (Tagged t) Id TyVar UVar
\end{code}

%************************************************************************
%*									*
\subsection{The @SimplifiableCore*@ instantiation of the @GenCore*@ types}
%*									*
%************************************************************************

Binders are tagged with @BinderInfo@:
\begin{code}
type Simplifiable = (Id, BinderInfo)

type SimplifiableCoreBinding = GenCoreBinding Simplifiable Id TyVar UVar
type SimplifiableCoreExpr    = GenCoreExpr    Simplifiable Id TyVar UVar
type SimplifiableCoreBinder  = GenCoreBinder  Simplifiable    TyVar UVar
type SimplifiableCoreArg     = GenCoreArg                  Id TyVar UVar

type SimplifiableCoreCaseAlts    = GenCoreCaseAlts    Simplifiable Id TyVar UVar
type SimplifiableCoreCaseDefault = GenCoreCaseDefault Simplifiable Id TyVar UVar
\end{code}
