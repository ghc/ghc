%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

	Arity and ete expansion

\begin{code}
-- | Arit and eta expansion
module CoreArity (
	manifestArity, exprArity, 
	exprEtaExpandArity, etaExpand 
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreFVs
import CoreUtils
import qualified CoreSubst
import CoreSubst ( Subst, substBndr, substBndrs, substExpr
       		 , mkEmptySubst, isEmptySubst )
import Var
import VarEnv
#if mingw32_TARGET_OS
import Packages
#endif
import Id
import Type
import Coercion
import BasicTypes
import Unique
import Outputable
import DynFlags
import FastString
import Maybes

import GHC.Exts		-- For `xori` 
\end{code}

%************************************************************************
%*									*
              manifestArity and exprArity
%*									*
%************************************************************************

exprArity is a cheap-and-cheerful version of exprEtaExpandArity.
It tells how many things the expression can be applied to before doing
any work.  It doesn't look inside cases, lets, etc.  The idea is that
exprEtaExpandArity will do the hard work, leaving something that's easy
for exprArity to grapple with.  In particular, Simplify uses exprArity to
compute the ArityInfo for the Id. 

Originally I thought that it was enough just to look for top-level lambdas, but
it isn't.  I've seen this

	foo = PrelBase.timesInt

We want foo to get arity 2 even though the eta-expander will leave it
unchanged, in the expectation that it'll be inlined.  But occasionally it
isn't, because foo is blacklisted (used in a rule).  

Similarly, see the ok_note check in exprEtaExpandArity.  So 
	f = __inline_me (\x -> e)
won't be eta-expanded.

And in any case it seems more robust to have exprArity be a bit more intelligent.
But note that 	(\x y z -> f x y z)
should have arity 3, regardless of f's arity.

Note [exprArity invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~
exprArity has the following invariant:
	(exprArity e) = n, then manifestArity (etaExpand e n) = n

That is, if exprArity says "the arity is n" then etaExpand really can get
"n" manifest lambdas to the top.

Why is this important?  Because 
  - In TidyPgm we use exprArity to fix the *final arity* of 
    each top-level Id, and in
  - In CorePrep we use etaExpand on each rhs, so that the visible lambdas
    actually match that arity, which in turn means
    that the StgRhs has the right number of lambdas

An alternative would be to do the eta-expansion in TidyPgm, at least
for top-level bindings, in which case we would not need the trim_arity
in exprArity.  That is a less local change, so I'm going to leave it for today!


\begin{code}
manifestArity :: CoreExpr -> Arity
-- ^ manifestArity sees how many leading value lambdas there are
manifestArity (Lam v e) | isId v    = 1 + manifestArity e
			| otherwise = manifestArity e
manifestArity (Note _ e)	    = manifestArity e
manifestArity (Cast e _)            = manifestArity e
manifestArity _                     = 0

exprArity :: CoreExpr -> Arity
-- ^ An approximate, fast, version of 'exprEtaExpandArity'
exprArity e = go e
  where
    go (Var v) 	       	         = idArity v
    go (Lam x e) | isId x    	 = go e + 1
    		 | otherwise 	 = go e
    go (Note _ e)                = go e
    go (Cast e co)               = trim_arity (go e) 0 (snd (coercionKind co))
    go (App e (Type _))          = go e
    go (App f a) | exprIsCheap a = (go f - 1) `max` 0
    	-- NB: exprIsCheap a!  
    	--	f (fac x) does not have arity 2, 
    	-- 	even if f has arity 3!
    	-- NB: `max 0`!  (\x y -> f x) has arity 2, even if f is
    	--		 unknown, hence arity 0
    go _		       	   = 0

	-- Note [exprArity invariant]
    trim_arity n a ty
	| n==a					      = a
	| Just (_, ty') <- splitForAllTy_maybe ty     = trim_arity n a     ty'
	| Just (_, ty') <- splitFunTy_maybe ty        = trim_arity n (a+1) ty'
	| Just (ty',_)  <- splitNewTypeRepCo_maybe ty = trim_arity n a     ty'
	| otherwise				      = a
\end{code}

%************************************************************************
%*									*
\subsection{Eta reduction and expansion}
%*									*
%************************************************************************

exprEtaExpandArity is used when eta expanding
	e  ==>  \xy -> e x y

It returns 1 (or more) to:
	case x of p -> \s -> ...
because for I/O ish things we really want to get that \s to the top.
We are prepared to evaluate x each time round the loop in order to get that

It's all a bit more subtle than it looks:

1.  One-shot lambdas

Consider one-shot lambdas
		let x = expensive in \y z -> E
We want this to have arity 2 if the \y-abstraction is a 1-shot lambda
Hence the ArityType returned by arityType

2.  The state-transformer hack

The one-shot lambda special cause is particularly important/useful for
IO state transformers, where we often get
	let x = E in \ s -> ...

and the \s is a real-world state token abstraction.  Such abstractions
are almost invariably 1-shot, so we want to pull the \s out, past the
let x=E, even if E is expensive.  So we treat state-token lambdas as 
one-shot even if they aren't really.  The hack is in Id.isOneShotBndr.

3.  Dealing with bottom

Consider also 
	f = \x -> error "foo"
Here, arity 1 is fine.  But if it is
	f = \x -> case x of 
			True  -> error "foo"
			False -> \y -> x+y
then we want to get arity 2.  Tecnically, this isn't quite right, because
	(f True) `seq` 1
should diverge, but it'll converge if we eta-expand f.  Nevertheless, we
do so; it improves some programs significantly, and increasing convergence
isn't a bad thing.  Hence the ABot/ATop in ArityType.

Actually, the situation is worse.  Consider
	f = \x -> case x of
			True  -> \y -> x+y
			False -> \y -> x-y
Can we eta-expand here?  At first the answer looks like "yes of course", but
consider
	(f bot) `seq` 1
This should diverge!  But if we eta-expand, it won't.   Again, we ignore this
"problem", because being scrupulous would lose an important transformation for
many programs.


4. Newtypes

Non-recursive newtypes are transparent, and should not get in the way.
We do (currently) eta-expand recursive newtypes too.  So if we have, say

	newtype T = MkT ([T] -> Int)

Suppose we have
	e = coerce T f
where f has arity 1.  Then: etaExpandArity e = 1; 
that is, etaExpandArity looks through the coerce.

When we eta-expand e to arity 1: eta_expand 1 e T
we want to get: 		 coerce T (\x::[T] -> (coerce ([T]->Int) e) x)

HOWEVER, note that if you use coerce bogusly you can ge
	coerce Int negate
And since negate has arity 2, you might try to eta expand.  But you can't
decopose Int to a function type.   Hence the final case in eta_expand.


\begin{code}
-- ^ The Arity returned is the number of value args the 
-- expression can be applied to without doing much work
exprEtaExpandArity :: DynFlags -> CoreExpr -> Arity
exprEtaExpandArity dflags e = arityDepth (arityType dflags e)

-- A limited sort of function type
data ArityType = AFun Bool ArityType	-- True <=> one-shot
	       | ATop			-- Know nothing
	       | ABot			-- Diverges

arityDepth :: ArityType -> Arity
arityDepth (AFun _ ty) = 1 + arityDepth ty
arityDepth _           = 0

andArityType :: ArityType -> ArityType -> ArityType
andArityType ABot           at2           = at2
andArityType ATop           _             = ATop
andArityType (AFun t1 at1)  (AFun t2 at2) = AFun (t1 && t2) (andArityType at1 at2)
andArityType at1            at2           = andArityType at2 at1

arityType :: DynFlags -> CoreExpr -> ArityType
	-- (go1 e) = [b1,..,bn]
	-- means expression can be rewritten \x_b1 -> ... \x_bn -> body
	-- where bi is True <=> the lambda is one-shot

arityType dflags (Note _ e) = arityType dflags e
--	Not needed any more: etaExpand is cleverer
-- removed: | ok_note n = arityType dflags e
-- removed: | otherwise = ATop

arityType dflags (Cast e _) = arityType dflags e

arityType _ (Var v)
  = mk (idArity v) (arg_tys (idType v))
  where
    mk :: Arity -> [Type] -> ArityType
	-- The argument types are only to steer the "state hack"
	-- Consider case x of
	--		True  -> foo
	--		False -> \(s:RealWorld) -> e
	-- where foo has arity 1.  Then we want the state hack to
	-- apply to foo too, so we can eta expand the case.
    mk 0 tys | isBottomingId v                   = ABot
             | (ty:_) <- tys, isStateHackType ty = AFun True ATop
             | otherwise                         = ATop
    mk n (ty:tys) = AFun (isStateHackType ty) (mk (n-1) tys)
    mk n []       = AFun False		      (mk (n-1) [])

    arg_tys :: Type -> [Type]	-- Ignore for-alls
    arg_tys ty 
	| Just (_, ty')  <- splitForAllTy_maybe ty = arg_tys ty'
	| Just (arg,res) <- splitFunTy_maybe ty    = arg : arg_tys res
	| otherwise				   = []

	-- Lambdas; increase arity
arityType dflags (Lam x e)
  | isId x    = AFun (isOneShotBndr x) (arityType dflags e)
  | otherwise = arityType dflags e

	-- Applications; decrease arity
arityType dflags (App f (Type _)) = arityType dflags f
arityType dflags (App f a)
   = case arityType dflags f of
	ABot -> ABot	-- If function diverges, ignore argument
	ATop -> ATop	-- No no info about function
	AFun _ xs
		| exprIsCheap a -> xs
		| otherwise	-> ATop
							   
	-- Case/Let; keep arity if either the expression is cheap
	-- or it's a 1-shot lambda
	-- The former is not really right for Haskell
	--	f x = case x of { (a,b) -> \y. e }
	--  ===>
	--	f x y = case x of { (a,b) -> e }
	-- The difference is observable using 'seq'
arityType dflags (Case scrut _ _ alts)
  = case foldr1 andArityType [arityType dflags rhs | (_,_,rhs) <- alts] of
        xs | exprIsCheap scrut     -> xs
        AFun one_shot _ | one_shot -> AFun True ATop
        _                          -> ATop

arityType dflags (Let b e) 
  = case arityType dflags e of
        xs              | cheap_bind b -> xs
        AFun one_shot _ | one_shot     -> AFun True ATop
        _                              -> ATop
  where
    cheap_bind (NonRec b e) = is_cheap (b,e)
    cheap_bind (Rec prs)    = all is_cheap prs
    is_cheap (b,e) = (dopt Opt_DictsCheap dflags && isDictId b)
		   || exprIsCheap e
	-- If the experimental -fdicts-cheap flag is on, we eta-expand through
	-- dictionary bindings.  This improves arities. Thereby, it also
	-- means that full laziness is less prone to floating out the
	-- application of a function to its dictionary arguments, which
	-- can thereby lose opportunities for fusion.  Example:
	-- 	foo :: Ord a => a -> ...
	--	foo = /\a \(d:Ord a). let d' = ...d... in \(x:a). ....
	--		-- So foo has arity 1
	--
	--	f = \x. foo dInt $ bar x
	--
	-- The (foo DInt) is floated out, and makes ineffective a RULE 
	--	foo (bar x) = ...
	--
	-- One could go further and make exprIsCheap reply True to any
	-- dictionary-typed expression, but that's more work.

arityType _ _ = ATop
\end{code}


%************************************************************************
%*									*
              The main eta-expander								
%*									*
%************************************************************************

IMPORTANT NOTE: The eta expander is careful not to introduce "crap".
In particular, given a CoreExpr satisfying the 'CpeRhs' invariant (in
CorePrep), it returns a CoreExpr satisfying the same invariant. See
Note [Eta expansion and the CorePrep invariants] in CorePrep.

This means the eta-expander has to do a bit of on-the-fly
simplification but it's not too hard.  The alernative, of relying on 
a subsequent clean-up phase of the Simplifier to de-crapify the result,
means you can't really use it in CorePrep, which is painful.

\begin{code}
-- | @etaExpand n us e ty@ returns an expression with
-- the same meaning as @e@, but with arity @n@.
--
-- Given:
--
-- > e' = etaExpand n us e ty
--
-- We should have that:
--
-- > ty = exprType e = exprType e'
etaExpand :: Arity	  	-- ^ Result should have this number of value args
	  -> CoreExpr	        -- ^ Expression to expand
	  -> CoreExpr
-- Note that SCCs are not treated specially.  If we have
--	etaExpand 2 (\x -> scc "foo" e)
--	= (\xy -> (scc "foo" e) y)
-- So the costs of evaluating 'e' (not 'e y') are attributed to "foo"

-- etaExpand deals with for-alls. For example:
--		etaExpand 1 E
-- where  E :: forall a. a -> a
-- would return
--	(/\b. \y::a -> E b y)
--
-- It deals with coerces too, though they are now rare
-- so perhaps the extra code isn't worth it

etaExpand n orig_expr
  | manifestArity orig_expr >= n = orig_expr	-- The no-op case
  | otherwise 		    
  = go n orig_expr
  where
      -- Strip off existing lambdas
    go 0 expr = expr
    go n (Lam v body) | isTyVar v = Lam v (go n     body)
       	              | otherwise = Lam v (go (n-1) body)
    go n (Note InlineMe expr) = Note InlineMe (go n expr)
        -- Note [Eta expansion and SCCs]
    go n (Cast expr co) = Cast (go n expr) co
    go n expr           = -- pprTrace "ee" (vcat [ppr orig_expr, ppr expr, ppr etas]) $
       	 		  etaInfoAbs etas (etaInfoApp subst' expr etas)
    	  		where
			    in_scope = mkInScopeSet (exprFreeVars expr)
			    (in_scope', etas) = mkEtaWW n in_scope (exprType expr)
			    subst' = mkEmptySubst in_scope'

      	      	       	        -- Wrapper    Unwrapper
--------------
data EtaInfo = EtaVar Var	-- /\a. [],   [] a
     	                 	-- \x.  [],   [] x
	     | EtaCo Coercion   -- [] |> co,  [] |> (sym co)

instance Outputable EtaInfo where
   ppr (EtaVar v) = ptext (sLit "EtaVar") <+> ppr v
   ppr (EtaCo co) = ptext (sLit "EtaCo")  <+> ppr co

pushCoercion :: Coercion -> [EtaInfo] -> [EtaInfo]
pushCoercion co1 (EtaCo co2 : eis)
  | isIdentityCoercion co = eis
  | otherwise	       	  = EtaCo co : eis
  where
    co = co1 `mkTransCoercion` co2

pushCoercion co eis = EtaCo co : eis

--------------
etaInfoAbs :: [EtaInfo] -> CoreExpr -> CoreExpr
etaInfoAbs []               expr = expr
etaInfoAbs (EtaVar v : eis) expr = Lam v (etaInfoAbs eis expr)
etaInfoAbs (EtaCo co : eis) expr = Cast (etaInfoAbs eis expr) (mkSymCoercion co)

--------------
etaInfoApp :: Subst -> CoreExpr -> [EtaInfo] -> CoreExpr
-- (etaInfoApp s e eis) returns something equivalent to 
-- 	       ((substExpr s e) `appliedto` eis)

etaInfoApp subst (Lam v1 e) (EtaVar v2 : eis) 
  = etaInfoApp subst' e eis
  where
    subst' | isTyVar v1 = CoreSubst.extendTvSubst subst v1 (mkTyVarTy v2) 
    	   | otherwise  = CoreSubst.extendIdSubst subst v1 (Var v2)

etaInfoApp subst (Cast e co1) eis
  = etaInfoApp subst e (pushCoercion co' eis)
  where
    co' = CoreSubst.substTy subst co1

etaInfoApp subst (Case e b _ alts) eis 
  = Case (subst_expr subst e) b1 (coreAltsType alts') alts'
  where
    (subst1, b1) = substBndr subst b
    alts' = map subst_alt alts
    subst_alt (con, bs, rhs) = (con, bs', etaInfoApp subst2 rhs eis) 
    	      where
	      	 (subst2,bs') = substBndrs subst1 bs
    
etaInfoApp subst (Let b e) eis 
  = Let b' (etaInfoApp subst' e eis)
  where
    (subst', b') = subst_bind subst b

etaInfoApp subst (Note note e) eis
  = Note note (etaInfoApp subst e eis)

etaInfoApp subst e eis
  = go (subst_expr subst e) eis
  where
    go e []                  = e
    go e (EtaVar v    : eis) = go (App e (varToCoreExpr v)) eis
    go e (EtaCo co    : eis) = go (Cast e co) eis

--------------
mkEtaWW :: Arity -> InScopeSet -> Type
	-> (InScopeSet, [EtaInfo])
	-- EtaInfo contains fresh variables,
	--   not free in the incoming CoreExpr
	-- Outgoing InScopeSet includes the EtaInfo vars
	--   and the original free vars

mkEtaWW n in_scope ty
  = go n empty_subst ty []
  where
    empty_subst = mkTvSubst in_scope emptyTvSubstEnv

    go n subst ty eis
       | n == 0
       = (getTvInScope subst, reverse eis)

       | Just (tv,ty') <- splitForAllTy_maybe ty
       , let (subst', tv') = substTyVarBndr subst tv
           -- Avoid free vars of the original expression
       = go n subst' ty' (EtaVar tv' : eis)

       | Just (arg_ty, res_ty) <- splitFunTy_maybe ty
       , let (subst', eta_id') = freshEtaId n subst arg_ty 
           -- Avoid free vars of the original expression
       = go (n-1) subst' res_ty (EtaVar eta_id' : eis)
       				   
       | Just(ty',co) <- splitNewTypeRepCo_maybe ty
       = 	-- Given this:
       		-- 	newtype T = MkT ([T] -> Int)
       		-- Consider eta-expanding this
       		--  	eta_expand 1 e T
       		-- We want to get
       		--	coerce T (\x::[T] -> (coerce ([T]->Int) e) x)
         go n subst ty' (EtaCo (substTy subst co) : eis)

       | otherwise	       	           -- We have an expression of arity > 0, 
       = (getTvInScope subst, reverse eis) -- but its type isn't a function. 
    	-- This *can* legitmately happen:
    	-- e.g.  coerce Int (\x. x) Essentially the programmer is
	-- playing fast and loose with types (Happy does this a lot).
	-- So we simply decline to eta-expand.  Otherwise we'd end up
	-- with an explicit lambda having a non-function type
   

--------------
-- Avoiding unnecessary substitution

subst_expr :: Subst -> CoreExpr -> CoreExpr
subst_expr s e | isEmptySubst s = e
	       | otherwise      = substExpr s e

subst_bind :: Subst -> CoreBind -> (Subst, CoreBind)
subst_bind subst (NonRec b r)
  = (subst', NonRec b' (subst_expr subst r))
  where
    (subst', b') = substBndr subst b
subst_bind subst (Rec prs)
  = (subst', Rec (bs1 `zip` map (subst_expr subst') rhss))
  where
    (bs, rhss) = unzip prs
    (subst', bs1) = substBndrs subst bs 


--------------
freshEtaId :: Int -> TvSubst -> Type -> (TvSubst, Id)
-- Make a fresh Id, with specified type (after applying substitution)
-- It should be "fresh" in the sense that it's not in the in-scope set
-- of the TvSubstEnv; and it should itself then be added to the in-scope
-- set of the TvSubstEnv
-- 
-- The Int is just a reasonable starting point for generating a unique;
-- it does not necessarily have to be unique itself.
freshEtaId n subst ty
      = (subst', eta_id')
      where
        ty'     = substTy subst ty
	eta_id' = uniqAway (getTvInScope subst) $
		  mkSysLocal (fsLit "eta") (mkBuiltinUnique n) ty'
	subst'  = extendTvInScope subst [eta_id']		  
\end{code}

