%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreSyn]{A data type for the Haskell compiler midsection}

\begin{code}
module CoreSyn (
	Expr(..), Alt, Bind(..), AltCon(..), Arg, Note(..),
	CoreExpr, CoreAlt, CoreBind, CoreArg, CoreBndr,
	TaggedExpr, TaggedAlt, TaggedBind, TaggedArg, TaggedBndr(..),

	mkLets, mkLams, 
	mkApps, mkTyApps, mkValApps, mkVarApps,
	mkLit, mkIntLitInt, mkIntLit, 
	mkConApp, 
	varToCoreExpr,

	isTyVar, isId, cmpAltCon, cmpAlt, ltAlt,
	bindersOf, bindersOfBinds, rhssOfBind, rhssOfAlts, 
	collectBinders, collectTyBinders, collectValBinders, collectTyAndValBinders,
	collectArgs, 
	coreExprCc,
	flattenBinds, 

	isValArg, isTypeArg, valArgCount, valBndrCount, isRuntimeArg, isRuntimeVar,

	-- Unfoldings
	Unfolding(..),	UnfoldingGuidance(..), 	-- Both abstract everywhere but in CoreUnfold.lhs
	noUnfolding, evaldUnfolding, mkOtherCon,
	unfoldingTemplate, maybeUnfoldingTemplate, otherCons, 
	isValueUnfolding, isEvaldUnfolding, isCheapUnfolding, isCompulsoryUnfolding,
	hasUnfolding, hasSomeUnfolding, neverUnfold,

	-- Seq stuff
	seqExpr, seqExprs, seqUnfolding, 

	-- Annotated expressions
	AnnExpr, AnnExpr'(..), AnnBind(..), AnnAlt, 
	deAnnotate, deAnnotate', deAnnAlt, collectAnnBndrs,

	-- Core rules
	CoreRule(..),	-- CoreSubst, CoreTidy, CoreFVs, PprCore only
	RuleName, seqRules, 
	isBuiltinRule, ruleName, isLocalRule, ruleIdName
    ) where

#include "HsVersions.h"

import StaticFlags	( opt_RuntimeTypes )
import CostCentre	( CostCentre, noCostCentre )
import Var		( Var, Id, TyVar, isTyVar, isId )
import Type		( Type, mkTyVarTy, seqType )
import Name		( Name )
import OccName		( OccName )
import Literal	        ( Literal, mkMachInt )
import DataCon		( DataCon, dataConWorkId, dataConTag )
import BasicTypes	( Activation )
import FastString
import Outputable

infixl 4 `mkApps`, `mkValApps`, `mkTyApps`, `mkVarApps`
-- Left associative, so that we can say (f `mkTyApps` xs `mkVarApps` ys)
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
  | Case  (Expr b) b Type [Alt b]  	-- Binder gets bound to value of scrutinee
	-- Invariant: The list of alternatives is ALWAYS EXHAUSTIVE,
	--	      meaning that it covers all cases that can occur
	--	      See the example below
	--
	-- Invariant: The DEFAULT case must be *first*, if it occurs at all
	-- Invariant: The remaining cases are in order of increasing 
	--		tag	(for DataAlts)
	--		lit	(for LitAlts)
	--	      This makes finding the relevant constructor easy,
	--	      and makes comparison easier too
  | Note  Note (Expr b)
  | Type  Type			-- This should only show up at the top
				-- level of an Arg

-- An "exhausive" case does not necessarily mention all constructors:
--	data Foo = Red | Green | Blue
--
--	...case x of 
--		Red   -> True
--		other -> f (case x of 
--				Green -> ...
--				Blue  -> ... )
-- The inner case does not need a Red alternative, because x can't be Red at
-- that program point.


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

  | InlineMe		-- Instructs simplifer to treat the enclosed expression
			-- as very small, and inline it at its call sites

  | CoreNote String     -- A generic core annotation, propagated but not used by GHC

-- NOTE: we also treat expressions wrapped in InlineMe as
-- 'cheap' and 'dupable' (in the sense of exprIsCheap, exprIsDupable)
-- What this means is that we obediently inline even things that don't
-- look like valuse.  This is sometimes important:
--	{-# INLINE f #-}
--	f = g . h
-- Here, f looks like a redex, and we aren't going to inline (.) because it's
-- inside an INLINE, so it'll stay looking like a redex.  Nevertheless, we 
-- should inline f even inside lambdas.  In effect, we should trust the programmer.
\end{code}

INVARIANTS:

* The RHS of a letrec, and the RHSs of all top-level lets,
  must be of LIFTED type.

* The RHS of a let, may be of UNLIFTED type, but only if the expression 
  is ok-for-speculation.  This means that the let can be floated around 
  without difficulty.  e.g.
	y::Int# = x +# 1#	ok
	y::Int# = fac 4#	not ok [use case instead]

* The argument of an App can be of any type.

* The simplifier tries to ensure that if the RHS of a let is a constructor
  application, its arguments are trivial, so that the constructor can be
  inlined vigorously.


%************************************************************************
%*									*
\subsection{Transformation rules}
%*									*
%************************************************************************

The CoreRule type and its friends are dealt with mainly in CoreRules,
but CoreFVs, Subst, PprCore, CoreTidy also inspect the representation.

A Rule is 

  "local"  if the function it is a rule for is defined in the
	   same module as the rule itself.

  "orphan" if nothing on the LHS is defined in the same module
	   as the rule itself

\begin{code}
type RuleName = FastString

data CoreRule
  = Rule { 
	ru_name :: RuleName,
	ru_act  :: Activation,	-- When the rule is active
	
	-- Rough-matching stuff
	-- see comments with InstEnv.Instance( is_cls, is_rough )
	ru_fn    :: Name,	-- Name of the Id at the head of this rule
	ru_rough :: [Maybe Name],	-- Name at the head of each argument
	
	-- Proper-matching stuff
	-- see comments with InstEnv.Instance( is_tvs, is_tys )
	ru_bndrs :: [CoreBndr],	-- Forall'd variables
	ru_args  :: [CoreExpr],	-- LHS args
	
	-- And the right-hand side
	ru_rhs   :: CoreExpr,

	-- Locality
	ru_local :: Bool,	-- The fn at the head of the rule is
				-- defined in the same module as the rule

	-- Orphan-hood; see comments is InstEnv.Instance( is_orph )
	ru_orph  :: Maybe OccName }

  | BuiltinRule {		-- Built-in rules are used for constant folding
	ru_name :: RuleName,	-- and suchlike.  It has no free variables.
	ru_fn :: Name,		-- Name of the Id at 
				-- the head of this rule
	ru_try  :: [CoreExpr] -> Maybe CoreExpr }

isBuiltinRule (BuiltinRule {}) = True
isBuiltinRule _		       = False

ruleName :: CoreRule -> RuleName
ruleName = ru_name

ruleIdName :: CoreRule -> Name
ruleIdName = ru_fn

isLocalRule :: CoreRule -> Bool
isLocalRule = ru_local
\end{code}


%************************************************************************
%*									*
		Unfoldings
%*									*
%************************************************************************

The @Unfolding@ type is declared here to avoid numerous loops, but it
should be abstract everywhere except in CoreUnfold.lhs

\begin{code}
data Unfolding
  = NoUnfolding

  | OtherCon [AltCon]		-- It ain't one of these
				-- (OtherCon xs) also indicates that something has been evaluated
				-- and hence there's no point in re-evaluating it.
				-- OtherCon [] is used even for non-data-type values
				-- to indicated evaluated-ness.  Notably:
				--	data C = C !(Int -> Int)
				-- 	case x of { C f -> ... }
				-- Here, f gets an OtherCon [] unfolding.

  | CompulsoryUnfolding CoreExpr	-- There is no "original" definition,
					-- so you'd better unfold.

  | CoreUnfolding			-- An unfolding with redundant cached information
		CoreExpr		-- Template; binder-info is correct
		Bool			-- True <=> top level binding
		Bool			-- exprIsHNF template (cached); it is ok to discard a `seq` on
					--	this variable
		Bool			-- True <=> doesn't waste (much) work to expand inside an inlining
					-- 	Basically it's exprIsCheap
		UnfoldingGuidance	-- Tells about the *size* of the template.


data UnfoldingGuidance
  = UnfoldNever
  | UnfoldIfGoodArgs	Int	-- and "n" value args

			[Int]	-- Discount if the argument is evaluated.
				-- (i.e., a simplification will definitely
				-- be possible).  One elt of the list per *value* arg.

			Int	-- The "size" of the unfolding; to be elaborated
				-- later. ToDo

			Int	-- Scrutinee discount: the discount to substract if the thing is in
				-- a context (case (thing args) of ...),
				-- (where there are the right number of arguments.)

noUnfolding    = NoUnfolding
evaldUnfolding = OtherCon []

mkOtherCon = OtherCon

seqUnfolding :: Unfolding -> ()
seqUnfolding (CoreUnfolding e top b1 b2 g)
  = seqExpr e `seq` top `seq` b1 `seq` b2 `seq` seqGuidance g
seqUnfolding other = ()

seqGuidance (UnfoldIfGoodArgs n ns a b) = n `seq` sum ns `seq` a `seq` b `seq` ()
seqGuidance other			= ()
\end{code}

\begin{code}
unfoldingTemplate :: Unfolding -> CoreExpr
unfoldingTemplate (CoreUnfolding expr _ _ _ _) = expr
unfoldingTemplate (CompulsoryUnfolding expr)   = expr
unfoldingTemplate other = panic "getUnfoldingTemplate"

maybeUnfoldingTemplate :: Unfolding -> Maybe CoreExpr
maybeUnfoldingTemplate (CoreUnfolding expr _ _ _ _) = Just expr
maybeUnfoldingTemplate (CompulsoryUnfolding expr)   = Just expr
maybeUnfoldingTemplate other 			    = Nothing

otherCons :: Unfolding -> [AltCon]
otherCons (OtherCon cons) = cons
otherCons other		  = []

isValueUnfolding :: Unfolding -> Bool
	-- Returns False for OtherCon
isValueUnfolding (CoreUnfolding _ _ is_evald _ _) = is_evald
isValueUnfolding other			          = False

isEvaldUnfolding :: Unfolding -> Bool
	-- Returns True for OtherCon
isEvaldUnfolding (OtherCon _)		          = True
isEvaldUnfolding (CoreUnfolding _ _ is_evald _ _) = is_evald
isEvaldUnfolding other			          = False

isCheapUnfolding :: Unfolding -> Bool
isCheapUnfolding (CoreUnfolding _ _ _ is_cheap _) = is_cheap
isCheapUnfolding other			  	  = False

isCompulsoryUnfolding :: Unfolding -> Bool
isCompulsoryUnfolding (CompulsoryUnfolding _) = True
isCompulsoryUnfolding other		      = False

hasUnfolding :: Unfolding -> Bool
hasUnfolding (CoreUnfolding _ _ _ _ _) = True
hasUnfolding (CompulsoryUnfolding _)   = True
hasUnfolding other 	 	       = False

hasSomeUnfolding :: Unfolding -> Bool
hasSomeUnfolding NoUnfolding = False
hasSomeUnfolding other	     = True

neverUnfold :: Unfolding -> Bool
neverUnfold NoUnfolding				= True
neverUnfold (OtherCon _)			= True
neverUnfold (CoreUnfolding _ _ _ _ UnfoldNever) = True
neverUnfold other 				= False
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

cmpAlt :: Alt b -> Alt b -> Ordering
cmpAlt (con1, _, _) (con2, _, _) = con1 `cmpAltCon` con2

ltAlt :: Alt b -> Alt b -> Bool
ltAlt a1 a2 = case a1 `cmpAlt` a2 of { LT -> True; other -> False }

cmpAltCon :: AltCon -> AltCon -> Ordering
-- Compares AltCons within a single list of alternatives
cmpAltCon DEFAULT      DEFAULT	   = EQ
cmpAltCon DEFAULT      con	   = LT

cmpAltCon (DataAlt d1) (DataAlt d2) = dataConTag d1 `compare` dataConTag d2
cmpAltCon (DataAlt _)  DEFAULT      = GT
cmpAltCon (LitAlt  l1) (LitAlt  l2) = l1 `compare` l2
cmpAltCon (LitAlt _)   DEFAULT      = GT

cmpAltCon con1 con2 = WARN( True, text "Comparing incomparable AltCons" <+> 
			 	  ppr con1 <+> ppr con2 )
		      LT
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
\end{code}

Binders are ``tagged'' with a \tr{t}:

\begin{code}
data TaggedBndr t = TB CoreBndr t	-- TB for "tagged binder"

type TaggedBind t = Bind (TaggedBndr t)
type TaggedExpr t = Expr (TaggedBndr t)
type TaggedArg  t = Arg  (TaggedBndr t)
type TaggedAlt  t = Alt  (TaggedBndr t)

instance Outputable b => Outputable (TaggedBndr b) where
  ppr (TB b l) = char '<' <> ppr b <> comma <> ppr l <> char '>'

instance Outputable b => OutputableBndr (TaggedBndr b) where
  pprBndr _ b = ppr b	-- Simple
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
mkConApp      :: DataCon -> [Arg b] -> Expr b
mkLets	      :: [Bind b] -> Expr b -> Expr b
mkLams	      :: [b] -> Expr b -> Expr b

mkLit lit	  = Lit lit
mkConApp con args = mkApps (Var (dataConWorkId con)) args

mkLams binders body = foldr Lam body binders
mkLets binds body   = foldr Let body binds

mkIntLit    n = Lit (mkMachInt n)
mkIntLitInt n = Lit (mkMachInt (toInteger n))

varToCoreExpr :: CoreBndr -> Expr b
varToCoreExpr v | isId v    = Var v
                | otherwise = Type (mkTyVarTy v)
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
collectTyBinders       	     :: CoreExpr -> ([TyVar],     CoreExpr)
collectValBinders      	     :: CoreExpr -> ([Id],        CoreExpr)
collectTyAndValBinders 	     :: CoreExpr -> ([TyVar], [Id], CoreExpr)

collectBinders expr
  = go [] expr
  where
    go bs (Lam b e) = go (b:bs) e
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

@isRuntimeVar v@ returns if (Lam v _) really becomes a lambda at runtime,
i.e. if type applications are actual lambdas because types are kept around
at runtime.  

Similarly isRuntimeArg.  

\begin{code}
isRuntimeVar :: Var -> Bool
isRuntimeVar | opt_RuntimeTypes = \v -> True
	     | otherwise	= \v -> isId v

isRuntimeArg :: CoreExpr -> Bool
isRuntimeArg | opt_RuntimeTypes = \e -> True
	     | otherwise	= \e -> isValArg e
\end{code}

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
seqExpr (Var v)         = v `seq` ()
seqExpr (Lit lit)       = lit `seq` ()
seqExpr (App f a)       = seqExpr f `seq` seqExpr a
seqExpr (Lam b e)       = seqBndr b `seq` seqExpr e
seqExpr (Let b e)       = seqBind b `seq` seqExpr e
-- gaw 2004
seqExpr (Case e b t as) = seqExpr e `seq` seqBndr b `seq` seqType t `seq` seqAlts as
seqExpr (Note n e)      = seqNote n `seq` seqExpr e
seqExpr (Type t)        = seqType t

seqExprs [] = ()
seqExprs (e:es) = seqExpr e `seq` seqExprs es

seqNote (Coerce t1 t2) = seqType t1 `seq` seqType t2
seqNote (CoreNote s)   = s `seq` ()
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

seqRules [] = ()
seqRules (Rule { ru_bndrs = bndrs, ru_args = args, ru_rhs = rhs } : rules) 
  = seqBndrs bndrs `seq` seqExprs (rhs:args) `seq` seqRules rules
seqRules (BuiltinRule {} : rules) = seqRules rules
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
-- gaw 2004
  | AnnCase	(AnnExpr bndr annot) bndr Type [AnnAlt bndr annot]
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

-- gaw 2004
deAnnotate' (AnnCase scrut v t alts)
  = Case (deAnnotate scrut) v t (map deAnnAlt alts)

deAnnAlt :: AnnAlt bndr annot -> Alt bndr
deAnnAlt (con,args,rhs) = (con,args,deAnnotate rhs)
\end{code}

\begin{code}
collectAnnBndrs :: AnnExpr bndr annot -> ([bndr], AnnExpr bndr annot)
collectAnnBndrs e
  = collect [] e
  where
    collect bs (_, AnnLam b body) = collect (b:bs) body
    collect bs body		  = (reverse bs, body)
\end{code}
