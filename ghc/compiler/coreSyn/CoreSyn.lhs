%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreSyn]{A data type for the Haskell compiler midsection}

\begin{code}
module CoreSyn (
	Expr(..), Alt, Bind(..), AltCon(..), Arg, Note(..),
	CoreExpr, CoreAlt, CoreBind, CoreArg, CoreBndr,
	TaggedExpr, TaggedAlt, TaggedBind, TaggedArg,

	mkLets, mkLams, 
	mkApps, mkTyApps, mkValApps, mkVarApps,
	mkLit, mkIntLitInt, mkIntLit, 
	mkStringLit, mkStringLitFS, mkConApp, 
 	mkAltExpr,
	bindNonRec, mkIfThenElse, varToCoreExpr,

	bindersOf, bindersOfBinds, rhssOfBind, rhssOfAlts, isTyVar, isId,
	collectBinders, collectTyBinders, collectValBinders, collectTyAndValBinders,
	collectArgs, collectBindersIgnoringNotes,
	coreExprCc,
	flattenBinds, 

	isValArg, isTypeArg, valArgCount, valBndrCount,

	-- Seq stuff
	seqRules, seqExpr, seqExprs, 

	-- Size
	coreBindsSize,

	-- Annotated expressions
	AnnExpr, AnnExpr'(..), AnnBind(..), AnnAlt, deAnnotate, deAnnotate',

	-- Core rules
	CoreRules(..), 	-- Representation needed by friends
	CoreRule(..),	-- CoreSubst, CoreTidy, CoreFVs, PprCore only
	RuleName,
	emptyCoreRules, isEmptyCoreRules, rulesRhsFreeVars, rulesRules
    ) where

#include "HsVersions.h"

import TysWiredIn	( boolTy, stringTy, nilDataCon )
import CostCentre	( CostCentre, noCostCentre )
import Var		( Var, Id, TyVar, isTyVar, isId, idType )
import VarEnv
import Id		( mkWildId, idOccInfo, idInfo )
import Type		( Type, UsageAnn, mkTyVarTy, isUnLiftedType, seqType )
import IdInfo		( OccInfo(..), megaSeqIdInfo )
import Literal	        ( Literal(MachStr), mkMachInt )
import PrimOp		( PrimOp )
import DataCon		( DataCon, dataConId )
import TysWiredIn	( trueDataCon, falseDataCon )
import ThinAir		( unpackCStringId, unpackCString2Id, addr2IntegerId )
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
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b [Alt b]  	-- Binder gets bound to value of scrutinee
				-- DEFAULT case must be last, if it occurs at all
  | Note  Note (Expr b)
  | Type  Type			-- This should only show up at the top
				-- level of an Arg

type Arg b = Expr b		-- Can be a Type

type Alt b = (AltCon, [b], Expr b)	-- (DEFAULT, [], rhs) is the default alternative

data AltCon = DataAlt DataCon
	    | LitAlt  Literal
	    | DEFAULT
	 deriving (Eq, Ord)

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
	  VarSet		-- Locally-defined free vars of RHSs

type RuleName = FAST_STRING

data CoreRule
  = Rule RuleName
	 [CoreBndr]	-- Forall'd variables
	 [CoreExpr]	-- LHS args
	 CoreExpr	-- RHS

  | BuiltinRule		-- Built-in rules are used for constant folding
			-- and suchlike.  It has no free variables.
	([CoreExpr] -> Maybe (RuleName, CoreExpr))

emptyCoreRules :: CoreRules
emptyCoreRules = Rules [] emptyVarSet

isEmptyCoreRules :: CoreRules -> Bool
isEmptyCoreRules (Rules rs _) = null rs

rulesRhsFreeVars :: CoreRules -> VarSet
rulesRhsFreeVars (Rules _ fvs) = fvs

rulesRules :: CoreRules -> [CoreRule]
rulesRules (Rules rules _) = rules
\end{code}


%************************************************************************
%*									*
\subsection{The main data type}
%*									*
%************************************************************************

\begin{code}
-- The Ord is needed for the FiniteMap used in the lookForConstructor
-- in SimplEnv.  If you declared that lookForConstructor *ignores*
-- constructor-applications with LitArg args, then you could get
-- rid of this Ord.

instance Outputable AltCon where
  ppr (DataAlt dc) = ppr dc
  ppr (LitAlt lit) = ppr lit
  ppr DEFAULT      = ptext SLIT("__DEFAULT")

instance Show AltCon where
  showsPrec p con = showsPrecSDoc p (ppr con)
\end{code}


%************************************************************************
%*									*
\subsection{Useful synonyms}
%*									*
%************************************************************************

The common case

\begin{code}
type CoreBndr = Var
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
mkVarApps :: Expr b -> [Var] -> Expr b

mkApps    f args = foldl App		  	   f args
mkTyApps  f args = foldl (\ e a -> App e (Type a)) f args
mkValApps f args = foldl (\ e a -> App e a)	   f args
mkVarApps f vars = foldl (\ e a -> App e (varToCoreExpr a)) f vars

mkLit         :: Literal -> Expr b
mkIntLit      :: Integer -> Expr b
mkIntLitInt   :: Int     -> Expr b
mkStringLit   :: String  -> Expr b	-- Makes a [Char] literal
mkStringLitFS :: FAST_STRING  -> Expr b -- Makes a [Char] literal
mkConApp      :: DataCon -> [Arg b] -> Expr b

mkLit lit	  = Lit lit
mkConApp con args = mkApps (Var (dataConId con)) args

mkIntLit    n = Lit (mkMachInt n)
mkIntLitInt n = Lit (mkMachInt (toInteger n))

mkStringLit str	= mkStringLitFS (_PK_ str)

mkStringLitFS str
  | any is_NUL (_UNPK_ str)
  = 	 -- Must cater for NULs in literal string
    mkApps (Var unpackCString2Id)
		[Lit (MachStr str),
		 mkIntLitInt (_LENGTH_ str)]

  | otherwise
  =	-- No NULs in the string
    App (Var unpackCStringId) (Lit (MachStr str))

  where
    is_NUL c = c == '\0'

varToCoreExpr :: CoreBndr -> Expr b
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
	 [ (DataAlt trueDataCon,  [], then_expr),
	   (DataAlt falseDataCon, [], else_expr) ]
\end{code}


\begin{code}
mkAltExpr :: AltCon -> [CoreBndr] -> [Type] -> CoreExpr
	-- This guy constructs the value that the scrutinee must have
	-- when you are in one particular branch of a case
mkAltExpr (DataAlt con) args inst_tys
  = mkConApp con (map Type inst_tys ++ map varToCoreExpr args)
mkAltExpr (LitAlt lit) [] []
  = Lit lit
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
\subsection{Seq stuff}
%*									*
%************************************************************************

\begin{code}
seqExpr :: CoreExpr -> ()
seqExpr (Var v)       = v `seq` ()
seqExpr (Lit lit)     = lit `seq` ()
seqExpr (App f a)     = seqExpr f `seq` seqExpr a
seqExpr (Lam b e)     = seqBndr b `seq` seqExpr e
seqExpr (Let b e)     = seqBind b `seq` seqExpr e
seqExpr (Case e b as) = seqExpr e `seq` seqBndr b `seq` seqAlts as
seqExpr (Note n e)    = seqNote n `seq` seqExpr e
seqExpr (Type t)      = seqType t

seqExprs [] = ()
seqExprs (e:es) = seqExpr e `seq` seqExprs es

seqNote (Coerce t1 t2) = seqType t1 `seq` seqType t2
seqNote other	       = ()

seqBndr b = b `seq` ()

seqBndrs [] = ()
seqBndrs (b:bs) = seqBndr b `seq` seqBndrs bs

seqBind (NonRec b e) = seqBndr b `seq` seqExpr e
seqBind (Rec prs)    = seqPairs prs

seqPairs [] = ()
seqPairs ((b,e):prs) = seqBndr b `seq` seqExpr e `seq` seqPairs prs

seqAlts [] = ()
seqAlts ((c,bs,e):alts) = seqBndrs bs `seq` seqExpr e `seq` seqAlts alts

seqRules :: CoreRules -> ()
seqRules (Rules rules fvs) = seq_rules rules `seq` seqVarSet fvs

seq_rules [] = ()
seq_rules (Rule fs bs es e : rules) = seqBndrs bs `seq` seqExprs (e:es) `seq` seq_rules rules
seq_rules (BuiltinRule _ : rules) = seq_rules rules
\end{code}

\begin{code}
coreBindsSize :: [CoreBind] -> Int
coreBindsSize bs = foldr ((+) . bindSize) 0 bs

exprSize :: CoreExpr -> Int
	-- A measure of the size of the expressions
	-- It also forces the expression pretty drastically as a side effect
exprSize (Var v)       = varSize v 
exprSize (Lit lit)     = 1
exprSize (App f a)     = exprSize f + exprSize a
exprSize (Lam b e)     = varSize b + exprSize e
exprSize (Let b e)     = bindSize b + exprSize e
exprSize (Case e b as) = exprSize e + varSize b + foldr ((+) . altSize) 0  as
exprSize (Note n e)    = exprSize e
exprSize (Type t)      = seqType t `seq`
			 1

exprsSize = foldr ((+) . exprSize) 0 

varSize :: Var -> Int
varSize b | isTyVar b = 1
	  | otherwise = seqType (idType b)		`seq`
			megaSeqIdInfo (idInfo b) 	`seq`
			1

varsSize = foldr ((+) . varSize) 0

bindSize (NonRec b e) = varSize b + exprSize e
bindSize (Rec prs)    = foldr ((+) . pairSize) 0 prs

pairSize (b,e) = varSize b + exprSize e

altSize (c,bs,e) = c `seq` varsSize bs + exprSize e
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
  | AnnLit	Literal
  | AnnLam	bndr (AnnExpr bndr annot)
  | AnnApp	(AnnExpr bndr annot) (AnnExpr bndr annot)
  | AnnCase	(AnnExpr bndr annot) bndr [AnnAlt bndr annot]
  | AnnLet	(AnnBind bndr annot) (AnnExpr bndr annot)
  | AnnNote	Note (AnnExpr bndr annot)
  | AnnType	Type

type AnnAlt bndr annot = (AltCon, [bndr], AnnExpr bndr annot)

data AnnBind bndr annot
  = AnnNonRec bndr (AnnExpr bndr annot)
  | AnnRec    [(bndr, AnnExpr bndr annot)]
\end{code}

\begin{code}
deAnnotate :: AnnExpr bndr annot -> Expr bndr
deAnnotate (_, e) = deAnnotate' e

deAnnotate' (AnnType t)           = Type t
deAnnotate' (AnnVar  v)           = Var v
deAnnotate' (AnnLit  lit)         = Lit lit
deAnnotate' (AnnLam  binder body) = Lam binder (deAnnotate body)
deAnnotate' (AnnApp  fun arg)     = App (deAnnotate fun) (deAnnotate arg)
deAnnotate' (AnnNote note body)   = Note note (deAnnotate body)

deAnnotate' (AnnLet bind body)
  = Let (deAnnBind bind) (deAnnotate body)
  where
    deAnnBind (AnnNonRec var rhs) = NonRec var (deAnnotate rhs)
    deAnnBind (AnnRec pairs) = Rec [(v,deAnnotate rhs) | (v,rhs) <- pairs]

deAnnotate' (AnnCase scrut v alts)
  = Case (deAnnotate scrut) v (map deAnnAlt alts)
  where
    deAnnAlt (con,args,rhs) = (con,args,deAnnotate rhs)
\end{code}

