%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreSyn]{A data type for the Haskell compiler midsection}

\begin{code}
module CoreSyn (
	Expr(..), Alt, Bind(..), Arg(..), Note(..),
	CoreExpr, CoreAlt, CoreBind, CoreArg, CoreBndr,
	TaggedExpr, TaggedAlt, TaggedBind, TaggedArg,

	mkLets, mkLams,
	mkApps, mkTyApps, mkValApps, mkVarApps,
	mkLit, mkStringLit, mkConApp, mkPrimApp, mkNote,
	bindNonRec, mkIfThenElse, varToCoreExpr,

	bindersOf, bindersOfBinds, rhssOfBind, rhssOfAlts, isDeadBinder, isTyVar, isId,
	collectBinders, collectTyBinders, collectValBinders, collectTyAndValBinders,
	collectArgs, collectBindersIgnoringNotes,
	coreExprCc,
	flattenBinds, 

	isValArg, isTypeArg, valArgCount, valBndrCount,

	-- Annotated expressions
	AnnExpr, AnnExpr'(..), AnnBind(..), AnnAlt, deAnnotate,

	-- Core rules
	CoreRules(..), 	-- Representation needed by friends
	CoreRule(..),	-- CoreSubst, CoreTidy, CoreFVs, PprCore only
	emptyCoreRules, isEmptyCoreRules, rulesRhsFreeVars, rulesRules
    ) where

#include "HsVersions.h"

import TysWiredIn	( boolTy, stringTy, nilDataCon )
import CostCentre	( CostCentre, isDupdCC, noCostCentre )
import Var		( Var, Id, TyVar, IdOrTyVar, isTyVar, isId, idType )
import VarEnv
import Id		( mkWildId, getInlinePragma )
import Type		( Type, UsageAnn, mkTyVarTy, isUnLiftedType )
import IdInfo		( InlinePragInfo(..) )
import Const	        ( Con(..), DataCon, Literal(NoRepStr), PrimOp )
import TysWiredIn	( trueDataCon, falseDataCon )
import VarSet
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{The main data types}
%*									*
%************************************************************************

These data types are the heart of the compiler

\begin{code}
infixl 8 `App`	-- App brackets to the left

data Expr b	-- "b" for the type of binders, 
  = Var	  Id
  | Con   Con [Arg b]		-- Guaranteed saturated
				-- The Con can be a DataCon, Literal, PrimOP
				-- but cannot be DEFAULT
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b [Alt b]  	-- Binder gets bound to value of scrutinee
				-- DEFAULT case must be last, if it occurs at all
  | Note  Note (Expr b)
  | Type  Type			-- This should only show up at the top
				-- level of an Arg

type Arg b = Expr b		-- Can be a Type

type Alt b = (Con, [b], Expr b)
	-- (DEFAULT, [], rhs) is the default alternative
	-- The Con can be a Literal, DataCon, or DEFAULT, but cannot be PrimOp

data Bind b = NonRec b (Expr b)
	      | Rec [(b, (Expr b))]

data Note
  = SCC CostCentre

  | Coerce	
	Type		-- The to-type:   type of whole coerce expression
	Type		-- The from-type: type of enclosed expression

  | InlineCall		-- Instructs simplifier to inline
			-- the enclosed call

  | InlineMe		-- Instructs simplifer to treat the enclosed expression
			-- as very small, and inline it at its call sites

  | TermUsg             -- A term-level usage annotation
        UsageAnn        -- (should not be a variable except during UsageSP inference)
\end{code}


%************************************************************************
%*									*
\subsection{Transformation rules}
%*									*
%************************************************************************

The CoreRule type and its friends are dealt with mainly in CoreRules,
but CoreFVs, Subst, PprCore, CoreTidy also inspect the representation.

\begin{code}
data CoreRules 
  = Rules [CoreRule]
	  IdOrTyVarSet		-- Locally-defined free vars of RHSs

data CoreRule
  = Rule FAST_STRING	-- Rule name
	 [CoreBndr]	-- Forall'd variables
	 [CoreExpr]	-- LHS args
	 CoreExpr	-- RHS

emptyCoreRules :: CoreRules
emptyCoreRules = Rules [] emptyVarSet

isEmptyCoreRules :: CoreRules -> Bool
isEmptyCoreRules (Rules rs _) = null rs

rulesRhsFreeVars :: CoreRules -> IdOrTyVarSet
rulesRhsFreeVars (Rules _ fvs) = fvs

rulesRules :: CoreRules -> [CoreRule]
rulesRules (Rules rules _) = rules
\end{code}


%************************************************************************
%*									*
\subsection{Useful synonyms}
%*									*
%************************************************************************

The common case

\begin{code}
type CoreBndr = IdOrTyVar
type CoreExpr = Expr CoreBndr
type CoreArg  = Arg  CoreBndr
type CoreBind = Bind CoreBndr
type CoreAlt  = Alt  CoreBndr
type CoreNote = Note
\end{code}

Binders are ``tagged'' with a \tr{t}:

\begin{code}
type Tagged t = (CoreBndr, t)

type TaggedBind t = Bind (Tagged t)
type TaggedExpr t = Expr (Tagged t)
type TaggedArg  t = Arg  (Tagged t)
type TaggedAlt  t = Alt  (Tagged t)
\end{code}


%************************************************************************
%*									*
\subsection{Core-constructing functions with checking}
%*									*
%************************************************************************

\begin{code}
mkApps    :: Expr b -> [Arg b]  -> Expr b
mkTyApps  :: Expr b -> [Type]   -> Expr b
mkValApps :: Expr b -> [Expr b] -> Expr b
mkVarApps :: CoreExpr -> [IdOrTyVar] -> CoreExpr

mkApps    f args = foldl App		  	   f args
mkTyApps  f args = foldl (\ e a -> App e (Type a)) f args
mkValApps f args = foldl (\ e a -> App e a)	   f args
mkVarApps f vars = foldl (\ e a -> App e (varToCoreExpr a)) f vars

mkLit       :: Literal -> Expr b
mkStringLit :: String  -> Expr b
mkConApp    :: DataCon -> [Arg b] -> Expr b
mkPrimApp   :: PrimOp  -> [Arg b] -> Expr b

mkLit lit	  = Con (Literal lit) []
mkStringLit str	  = Con (Literal (NoRepStr (_PK_ str) stringTy)) []
mkConApp con args = Con (DataCon con) args
mkPrimApp op args = Con (PrimOp op)   args

varToCoreExpr :: CoreBndr -> CoreExpr
varToCoreExpr v | isId v    = Var v
                | otherwise = Type (mkTyVarTy v)
\end{code}

\begin{code}
mkLams :: [b] -> Expr b -> Expr b
mkLams binders body = foldr Lam body binders
\end{code}

\begin{code}
mkLets :: [Bind b] -> Expr b -> Expr b
mkLets binds body = foldr Let body binds

bindNonRec :: Id -> CoreExpr -> CoreExpr -> CoreExpr
-- (bindNonRec x r b) produces either
--	let x = r in b
-- or
--	case r of x { _DEFAULT_ -> b }
--
-- depending on whether x is unlifted or not
-- It's used by the desugarer to avoid building bindings
-- that give Core Lint a heart attack.  Actually the simplifier
-- deals with them perfectly well.
bindNonRec bndr rhs body 
  | isUnLiftedType (idType bndr) = Case rhs bndr [(DEFAULT,[],body)]
  | otherwise			 = Let (NonRec bndr rhs) body

mkIfThenElse :: CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr
mkIfThenElse guard then_expr else_expr
  = Case guard (mkWildId boolTy) 
	 [ (DataCon trueDataCon,  [], then_expr),
	   (DataCon falseDataCon, [], else_expr) ]
\end{code}

mkNote removes redundant coercions, and SCCs where possible

\begin{code}
mkNote :: Note -> Expr b -> Expr b
mkNote (Coerce to_ty1 from_ty1) (Note (Coerce to_ty2 from_ty2) expr)
 = ASSERT( from_ty1 == to_ty2 )
   mkNote (Coerce to_ty1 from_ty2) expr

mkNote (SCC cc1) expr@(Note (SCC cc2) _)
  | isDupdCC cc1	-- Discard the outer SCC provided we don't need
  = expr		-- to track its entry count

mkNote note@(SCC cc1) expr@(Lam x e)	-- Move _scc_ inside lambda
  = Lam x (mkNote note e)

-- Drop trivial InlineMe's
mkNote InlineMe expr@(Con _ _) = expr
mkNote InlineMe expr@(Var v)   = expr

-- Slide InlineCall in around the function
--	No longer necessary I think (SLPJ Apr 99)
-- mkNote InlineCall (App f a) = App (mkNote InlineCall f) a
-- mkNote InlineCall (Var v)   = Note InlineCall (Var v)
-- mkNote InlineCall expr      = expr

mkNote note expr = Note note expr
\end{code}

%************************************************************************
%*									*
\subsection{Simple access functions}
%*									*
%************************************************************************

\begin{code}
bindersOf  :: Bind b -> [b]
bindersOf (NonRec binder _) = [binder]
bindersOf (Rec pairs)       = [binder | (binder, _) <- pairs]

bindersOfBinds :: [Bind b] -> [b]
bindersOfBinds binds = foldr ((++) . bindersOf) [] binds

rhssOfBind :: Bind b -> [Expr b]
rhssOfBind (NonRec _ rhs) = [rhs]
rhssOfBind (Rec pairs)    = [rhs | (_,rhs) <- pairs]

rhssOfAlts :: [Alt b] -> [Expr b]
rhssOfAlts alts = [e | (_,_,e) <- alts]

isDeadBinder :: CoreBndr -> Bool
isDeadBinder bndr | isId bndr = case getInlinePragma bndr of
					IAmDead -> True
					other	-> False
		  | otherwise = False	-- TyVars count as not dead

flattenBinds :: [Bind b] -> [(b, Expr b)]	-- Get all the lhs/rhs pairs
flattenBinds (NonRec b r : binds) = (b,r) : flattenBinds binds
flattenBinds (Rec prs1   : binds) = prs1 ++ flattenBinds binds
flattenBinds []			  = []
\end{code}

We often want to strip off leading lambdas before getting down to
business.  @collectBinders@ is your friend.

We expect (by convention) type-, and value- lambdas in that
order.

\begin{code}
collectBinders	             :: Expr b -> ([b],         Expr b)
collectBindersIgnoringNotes  :: Expr b -> ([b],         Expr b)
collectTyBinders       	     :: CoreExpr -> ([TyVar],     CoreExpr)
collectValBinders      	     :: CoreExpr -> ([Id],        CoreExpr)
collectTyAndValBinders 	     :: CoreExpr -> ([TyVar], [Id], CoreExpr)

collectBinders expr
  = go [] expr
  where
    go bs (Lam b e) = go (b:bs) e
    go bs e	     = (reverse bs, e)

-- This one ignores notes.  It's used in CoreUnfold and StrAnal
-- when we aren't going to put the expression back together from
-- the pieces, so we don't mind losing the Notes
collectBindersIgnoringNotes expr
  = go [] expr
  where
    go bs (Lam b e)  = go (b:bs) e
    go bs (Note _ e) = go    bs  e
    go bs e	     = (reverse bs, e)

collectTyAndValBinders expr
  = (tvs, ids, body)
  where
    (tvs, body1) = collectTyBinders expr
    (ids, body)  = collectValBinders body1

collectTyBinders expr
  = go [] expr
  where
    go tvs (Lam b e) | isTyVar b = go (b:tvs) e
    go tvs e			 = (reverse tvs, e)

collectValBinders expr
  = go [] expr
  where
    go ids (Lam b e) | isId b = go (b:ids) e
    go ids body		      = (reverse ids, body)
\end{code}


@collectArgs@ takes an application expression, returning the function
and the arguments to which it is applied.

\begin{code}
collectArgs :: Expr b -> (Expr b, [Arg b])
collectArgs expr
  = go expr []
  where
    go (App f a) as = go f (a:as)
    go e 	 as = (e, as)
\end{code}

coreExprCc gets the cost centre enclosing an expression, if any.
It looks inside lambdas because (scc "foo" \x.e) = \x.scc "foo" e

\begin{code}
coreExprCc :: Expr b -> CostCentre
coreExprCc (Note (SCC cc) e)   = cc
coreExprCc (Note other_note e) = coreExprCc e
coreExprCc (Lam _ e)           = coreExprCc e
coreExprCc other               = noCostCentre
\end{code}


%************************************************************************
%*									*
\subsection{Predicates}
%*									*
%************************************************************************

\begin{code}
isValArg (Type _) = False
isValArg other    = True

isTypeArg (Type _) = True
isTypeArg other    = False

valBndrCount :: [CoreBndr] -> Int
valBndrCount []		    	  = 0
valBndrCount (b : bs) | isId b    = 1 + valBndrCount bs
		      | otherwise = valBndrCount bs

valArgCount :: [Arg b] -> Int
valArgCount []		    = 0
valArgCount (Type _ : args) = valArgCount args
valArgCount (other  : args) = 1 + valArgCount args
\end{code}


%************************************************************************
%*									*
\subsection{Annotated core; annotation at every node in the tree}
%*									*
%************************************************************************

\begin{code}
type AnnExpr bndr annot = (annot, AnnExpr' bndr annot)

data AnnExpr' bndr annot
  = AnnVar	Id
  | AnnCon	Con [AnnExpr bndr annot]
  | AnnLam	bndr (AnnExpr bndr annot)
  | AnnApp	(AnnExpr bndr annot) (AnnExpr bndr annot)
  | AnnCase	(AnnExpr bndr annot) bndr [AnnAlt bndr annot]
  | AnnLet	(AnnBind bndr annot) (AnnExpr bndr annot)
  | AnnNote	Note (AnnExpr bndr annot)
  | AnnType	Type

type AnnAlt bndr annot = (Con, [bndr], AnnExpr bndr annot)

data AnnBind bndr annot
  = AnnNonRec bndr (AnnExpr bndr annot)
  | AnnRec    [(bndr, AnnExpr bndr annot)]
\end{code}

\begin{code}
deAnnotate :: AnnExpr bndr annot -> Expr bndr

deAnnotate (_, AnnType	t)          = Type t
deAnnotate (_, AnnVar	v)          = Var v
deAnnotate (_, AnnCon	con args)   = Con con (map deAnnotate args)
deAnnotate (_, AnnLam	binder body)= Lam binder (deAnnotate body)
deAnnotate (_, AnnApp	fun arg)    = App (deAnnotate fun) (deAnnotate arg)
deAnnotate (_, AnnNote	note body)  = Note note (deAnnotate body)

deAnnotate (_, AnnLet bind body)
  = Let (deAnnBind bind) (deAnnotate body)
  where
    deAnnBind (AnnNonRec var rhs) = NonRec var (deAnnotate rhs)
    deAnnBind (AnnRec pairs) = Rec [(v,deAnnotate rhs) | (v,rhs) <- pairs]

deAnnotate (_, AnnCase scrut v alts)
  = Case (deAnnotate scrut) v (map deAnnAlt alts)
  where
    deAnnAlt (con,args,rhs) = (con,args,deAnnotate rhs)
\end{code}

