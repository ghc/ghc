%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[CoreSyn]{A data type for the Haskell compiler midsection}

\begin{code}
module CoreSyn (
	GenCoreBinding(..), GenCoreExpr(..),
	GenCoreArg(..), GenCoreBinder(..), GenCoreCaseAlts(..),
	GenCoreCaseDefault(..), CoreNote(..),

	bindersOf, pairsFromCoreBinds, rhssOfBind,

	mkGenApp, mkValApp, mkTyApp, 
	mkApp, mkCon, mkPrim,
	mkValLam, mkTyLam, 
	mkLam,
	collectBinders, collectValBinders, collectTyBinders,
	isValBinder, notValBinder,
	
	collectArgs, initialTyArgs, initialValArgs, isValArg, notValArg, numValArgs,

	mkCoLetAny, mkCoLetNoUnboxed, mkCoLetUnboxedToCase,
	mkCoLetsAny, mkCoLetsNoUnboxed, mkCoLetsUnboxedToCase,
	mkCoLetrecAny, mkCoLetrecNoUnboxed,

	rhssOfAlts,

	-- Common type instantiation...
	CoreBinding,
	CoreExpr,
	CoreBinder,
	CoreArg,
	CoreCaseAlts,
	CoreCaseDefault,

	-- And not-so-common type instantiations...
	TaggedCoreBinding,
	TaggedCoreExpr,
	TaggedCoreBinder,
	TaggedCoreArg,
	TaggedCoreCaseAlts,
	TaggedCoreCaseDefault,

	SimplifiableCoreBinding,
	SimplifiableCoreExpr,
	SimplifiableCoreBinder,
	SimplifiableCoreArg,
	SimplifiableCoreCaseAlts,
	SimplifiableCoreCaseDefault
    ) where

#include "HsVersions.h"

import CostCentre	( CostCentre )
import Id		( idType, Id )
import Type		( isUnboxedType,GenType, Type )
import TyVar		( GenTyVar, TyVar )
import Util		( panic, assertPanic )
import BinderInfo       ( BinderInfo )
import BasicTypes	( Unused )
import Literal          ( Literal )
import PrimOp           ( PrimOp )
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
data GenCoreBinding val_bdr val_occ flexi
  = NonRec	val_bdr (GenCoreExpr val_bdr val_occ flexi)
  | Rec		[(val_bdr, GenCoreExpr val_bdr val_occ flexi)]
\end{code}

\begin{code}
bindersOf :: GenCoreBinding val_bdr val_occ flexi -> [val_bdr]

pairsFromCoreBinds ::
  [GenCoreBinding val_bdr val_occ flexi] ->
  [(val_bdr, GenCoreExpr val_bdr val_occ flexi)]

rhssOfBind :: GenCoreBinding val_bdr val_occ flexi -> [GenCoreExpr val_bdr val_occ flexi]

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
data GenCoreExpr val_bdr val_occ flexi
     = Var    val_occ
     | Lit    Literal	-- literal constants
\end{code}

@Cons@ and @Prims@ are saturated constructor and primitive-op
applications (see the comment).  Note: @Con@s are only set up by the
simplifier (and by the desugarer when it knows what it's doing).  The
desugarer sets up constructors as applications of global @Vars@s.

\begin{code}
     | Con	Id [GenCoreArg val_occ flexi]
		-- Saturated constructor application:
		-- The constructor is a function of the form:
		--	/\ a1 -> ... /\ am -> \ b1 -> ... \ bn ->
		-- <expr> where "/\" is a type lambda and "\" the
		-- regular kind; there will be "m" Types and
		-- "n" bindees in the Con args.

     | Prim	PrimOp [GenCoreArg val_occ flexi]
		-- saturated primitive operation;

		-- comment on Cons applies here, too.
\end{code}

Ye olde abstraction and application operators.
\begin{code}
     | Lam	(GenCoreBinder val_bdr flexi)
		(GenCoreExpr   val_bdr val_occ flexi)

     | App	(GenCoreExpr val_bdr val_occ flexi)
		(GenCoreArg  val_occ flexi)
\end{code}

Case expressions (\tr{case <expr> of <List of alternatives>}): there
are really two flavours masquerading here---those for scrutinising
{\em algebraic} types and those for {\em primitive} types.  Please see
under @GenCoreCaseAlts@.
\begin{code}
     | Case	(GenCoreExpr val_bdr val_occ flexi)
		(GenCoreCaseAlts val_bdr val_occ flexi)
\end{code}

A Core case expression \tr{case e of v -> ...} implies evaluation of
\tr{e}; it is not equivalent to \tr{let v = in ...} (as with a Haskell
\tr{case}).

Non-recursive @Lets@ only have one binding; having more than one
doesn't buy you much, and it is an easy way to mess up variable
scoping.
\begin{code}
     | Let	(GenCoreBinding val_bdr val_occ flexi)
		(GenCoreExpr val_bdr val_occ flexi)
		-- both recursive and non-.
		-- The "GenCoreBinding" records that information
\end{code}

A @Note@ annotates a @CoreExpr@ with useful information
of some kind.
\begin{code}
     | Note	(CoreNote flexi)
		(GenCoreExpr val_bdr val_occ flexi)
\end{code}


%************************************************************************
%*									*
\subsection{Core-notes}
%*									*
%************************************************************************

\begin{code}
data CoreNote flexi
  = SCC 
	CostCentre

  | Coerce	
	(GenType flexi)		-- The to-type:   type of whole coerce expression
	(GenType flexi)		-- The from-type: type of enclosed expression

  | InlineCall			-- Instructs simplifier to inline
				-- the enclosed call
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
mkCoLetAny :: GenCoreBinding Id Id flexi
	   -> GenCoreExpr    Id Id flexi
	   -> GenCoreExpr    Id Id flexi
mkCoLetsAny :: [GenCoreBinding Id Id flexi] ->
		GenCoreExpr Id Id flexi ->
		GenCoreExpr Id Id flexi

mkCoLetrecAny :: [(val_bdr, GenCoreExpr val_bdr val_occ flexi)]
	      -> GenCoreExpr val_bdr val_occ flexi
	      -> GenCoreExpr val_bdr val_occ flexi

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
data GenCoreCaseAlts val_bdr val_occ flexi
  = AlgAlts	[(Id,				-- alts: data constructor,
		  [val_bdr],			-- constructor's parameters,
		  GenCoreExpr val_bdr val_occ flexi)]	-- rhs.
		(GenCoreCaseDefault val_bdr val_occ flexi)

  | PrimAlts	[(Literal,			-- alts: unboxed literal,
		  GenCoreExpr val_bdr val_occ flexi)]	-- rhs.
		(GenCoreCaseDefault val_bdr val_occ flexi)

-- obvious things: if there are no alts in the list, then the default
-- can't be NoDefault.

data GenCoreCaseDefault val_bdr val_occ flexi
  = NoDefault					-- small con family: all
						-- constructor accounted for
  | BindDefault val_bdr				-- form: var -> expr;
		(GenCoreExpr val_bdr val_occ flexi)	-- "val_bdr" may or may not
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
data GenCoreBinder val_bdr flexi
  = ValBinder	val_bdr
  | TyBinder	(GenTyVar flexi)

isValBinder (ValBinder _) = True
isValBinder _		  = False

notValBinder = not . isValBinder
\end{code}

Clump Lams together if possible.

\begin{code}
mkValLam :: [val_bdr]
	 -> GenCoreExpr val_bdr val_occ flexi
	 -> GenCoreExpr val_bdr val_occ flexi
mkTyLam  :: [GenTyVar flexi]
	 -> GenCoreExpr val_bdr val_occ flexi
	 -> GenCoreExpr val_bdr val_occ flexi

mkValLam binders body = foldr (Lam . ValBinder)   body binders
mkTyLam  binders body = foldr (Lam . TyBinder)    body binders

mkLam :: [GenTyVar flexi] -> [val_bdr] -- ToDo: could add a [uvar] arg...
	 -> GenCoreExpr val_bdr val_occ flexi
	 -> GenCoreExpr val_bdr val_occ flexi

mkLam tyvars valvars body
  = mkTyLam tyvars (mkValLam valvars body)
\end{code}

We often want to strip off leading lambdas before getting down to
business.  @collectBinders@ is your friend.

We expect (by convention) usage-, type-, and value- lambdas in that
order.

\begin{code}
collectBinders ::
  GenCoreExpr val_bdr val_occ flexi ->
  ([GenTyVar flexi], [val_bdr], GenCoreExpr val_bdr val_occ flexi)

collectBinders expr
  = case collectValBinders body1 of { (vals,body) -> (tyvars, vals, body) }
  where
    (tyvars, body1) = collectTyBinders expr

collectTyBinders expr
  = tyvars expr []
  where
    tyvars (Lam (TyBinder t) body) tacc = tyvars body (t:tacc)
    tyvars other tacc = (reverse tacc, other)

collectValBinders :: GenCoreExpr val_bdr val_occ flexi ->
		     ([val_bdr], GenCoreExpr val_bdr val_occ flexi)
collectValBinders expr
  = go [] expr
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
data GenCoreArg val_occ flexi
  = LitArg	Literal
  | VarArg	val_occ
  | TyArg	(GenType flexi)
\end{code}

General and specific forms:
\begin{code}
mkGenApp :: GenCoreExpr val_bdr val_occ flexi
	 -> [GenCoreArg val_occ flexi]
	 -> GenCoreExpr val_bdr val_occ flexi
mkTyApp  :: GenCoreExpr val_bdr val_occ flexi
	 -> [GenType flexi]
	 -> GenCoreExpr val_bdr val_occ flexi
mkValApp :: GenCoreExpr val_bdr val_occ flexi
	 -> [GenCoreArg val_occ flexi] -- but we ASSERT they are LitArg or VarArg
	 -> GenCoreExpr val_bdr val_occ flexi

mkGenApp f args = foldl App		  		   f args
mkTyApp  f args = foldl (\ e a -> App e (TyArg a))	   f args
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

mk_thing thing tys vals
  = ASSERT( all isValArg vals )
    thing (map TyArg tys ++ vals)
\end{code}

@collectArgs@ takes an application expression, returning the function
and the arguments to which it is applied.

\begin{code}
collectArgs :: GenCoreExpr val_bdr val_occ flexi
	    -> (GenCoreExpr val_bdr val_occ flexi,
		[GenType flexi],
	        [GenCoreArg val_occ flexi]{-ValArgs-})

collectArgs expr
  = valvars expr []
  where
    valvars (App fun v) vacc | isValArg v = valvars fun (v:vacc)
    valvars fun vacc
      = case (tyvars fun []) of { (expr, tacc) ->
	(expr, tacc, vacc) }

    tyvars (App fun (TyArg t)) tacc = tyvars fun (t:tacc)
    tyvars fun tacc		    = (expr, tacc)
\end{code}


\begin{code}
initialTyArgs :: [GenCoreArg val_occ flexi]
	      -> ([GenType flexi], [GenCoreArg val_occ flexi])
initialTyArgs (TyArg ty : args) = (ty:tys, args') 
				where
				  (tys, args') = initialTyArgs args
initialTyArgs other 		= ([],other)

initialValArgs :: [GenCoreArg val_occ flexi]
	      -> ([GenCoreArg val_occ flexi], [GenCoreArg val_occ flexi])
initialValArgs args = span isValArg args
\end{code}


%************************************************************************
%*									*
\subsection{The main @Core*@ instantiation of the @GenCore*@ types}
%*									*
%************************************************************************

\begin{code}
type CoreBinding = GenCoreBinding  Id Id Unused
type CoreExpr    = GenCoreExpr     Id Id Unused
type CoreBinder	 = GenCoreBinder   Id    Unused
type CoreArg     = GenCoreArg         Id Unused

type CoreCaseAlts    = GenCoreCaseAlts    Id Id Unused
type CoreCaseDefault = GenCoreCaseDefault Id Id Unused
\end{code}

%************************************************************************
%*									*
\subsection{The @TaggedCore*@ instantiation of the @GenCore*@ types}
%*									*
%************************************************************************

Binders are ``tagged'' with a \tr{t}:
\begin{code}
type Tagged t = (Id, t)

type TaggedCoreBinding t = GenCoreBinding (Tagged t) Id Unused
type TaggedCoreExpr    t = GenCoreExpr    (Tagged t) Id Unused
type TaggedCoreBinder  t = GenCoreBinder  (Tagged t)    Unused
type TaggedCoreArg     t = GenCoreArg                Id Unused

type TaggedCoreCaseAlts    t = GenCoreCaseAlts    (Tagged t) Id Unused
type TaggedCoreCaseDefault t = GenCoreCaseDefault (Tagged t) Id Unused
\end{code}

%************************************************************************
%*									*
\subsection{The @SimplifiableCore*@ instantiation of the @GenCore*@ types}
%*									*
%************************************************************************

Binders are tagged with @BinderInfo@:
\begin{code}
type Simplifiable = (Id, BinderInfo)

type SimplifiableCoreBinding = GenCoreBinding Simplifiable Id Unused
type SimplifiableCoreExpr    = GenCoreExpr    Simplifiable Id Unused
type SimplifiableCoreBinder  = GenCoreBinder  Simplifiable    Unused
type SimplifiableCoreArg     = GenCoreArg                  Id Unused

type SimplifiableCoreCaseAlts    = GenCoreCaseAlts    Simplifiable Id Unused
type SimplifiableCoreCaseDefault = GenCoreCaseDefault Simplifiable Id Unused
\end{code}
