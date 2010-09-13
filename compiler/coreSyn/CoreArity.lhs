%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

	Arity and ete expansion

\begin{code}
-- | Arit and eta expansion
module CoreArity (
	manifestArity, exprArity, exprBotStrictness_maybe,
	exprEtaExpandArity, etaExpand
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreFVs
import CoreUtils
import CoreSubst
import Demand
import Var
import VarEnv
import Id
import Type
import TyCon	( isRecursiveTyCon, isClassTyCon )
import TcType	( isDictLikeTy )
import Coercion
import BasicTypes
import Unique
import Outputable
import DynFlags
import FastString
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

  * If typeArity (exprType e) = n,
    then manifestArity (etaExpand e n) = n
 
    That is, etaExpand can always expand as much as typeArity says
    So the case analysis in etaExpand and in typeArity must match
 
  * exprArity e <= typeArity (exprType e)      

  * Hence if (exprArity e) = n, then manifestArity (etaExpand e n) = n

    That is, if exprArity says "the arity is n" then etaExpand really 
    can get "n" manifest lambdas to the top.

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
    go (Var v) 	       	           = idArity v
    go (Lam x e) | isId x    	   = go e + 1
    		 | otherwise 	   = go e
    go (Note _ e)                  = go e
    go (Cast e co)                 = go e `min` length (typeArity (snd (coercionKind co)))
       	       			     	-- Note [exprArity invariant]
    go (App e (Type _))            = go e
    go (App f a) | exprIsTrivial a = (go f - 1) `max` 0
        -- See Note [exprArity for applications]
    go _		       	   = 0


typeArity :: Type -> [OneShot]
-- How many value arrows are visible in the type?
-- We look through foralls, and newtypes
-- See Note [exprArity invariant]
typeArity ty 
  | Just (_, ty')  <- splitForAllTy_maybe ty 
  = typeArity ty'

  | Just (arg,res) <- splitFunTy_maybe ty    
  = isStateHackType arg : typeArity res

  | Just (tc,tys) <- splitTyConApp_maybe ty 
  , Just (ty', _) <- instNewTyCon_maybe tc tys
  , not (isRecursiveTyCon tc)
  , not (isClassTyCon tc)	-- Do not eta-expand through newtype classes
    		      		-- See Note [Newtype classes and eta expansion]
  = typeArity ty'
  	-- Important to look through non-recursive newtypes, so that, eg 
  	--	(f x)   where f has arity 2, f :: Int -> IO ()
  	-- Here we want to get arity 1 for the result!

  | otherwise
  = []
\end{code}

Note [Newtype classes and eta expansion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have to be careful when eta-expanding through newtypes.  In general
it's a good idea, but annoyingly it interacts badly with the class-op 
rule mechanism.  Consider
 
   class C a where { op :: a -> a }
   instance C b => C [b] where
     op x = ...

These translate to

   co :: forall a. (a->a) ~ C a

   $copList :: C b -> [b] -> [b]
   $copList d x = ...

   $dfList :: C b -> C [b]
   {-# DFunUnfolding = [$copList] #-}
   $dfList d = $copList d |> co@[b]

Now suppose we have:

   dCInt :: C Int    

   blah :: [Int] -> [Int]
   blah = op ($dfList dCInt)

Now we want the built-in op/$dfList rule will fire to give
   blah = $copList dCInt

But with eta-expansion 'blah' might (and in Trac #3772, which is
slightly more complicated, does) turn into

   blah = op (\eta. ($dfList dCInt |> sym co) eta)

and now it is *much* harder for the op/$dfList rule to fire, becuase
exprIsConApp_maybe won't hold of the argument to op.  I considered
trying to *make* it hold, but it's tricky and I gave up.

The test simplCore/should_compile/T3722 is an excellent example.


Note [exprArity for applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we come to an application we check that the arg is trivial.
   eg  f (fac x) does not have arity 2, 
                 even if f has arity 3!

* We require that is trivial rather merely cheap.  Suppose f has arity 2.
  Then    f (Just y)
  has arity 0, because if we gave it arity 1 and then inlined f we'd get
          let v = Just y in \w. <f-body>
  which has arity 0.  And we try to maintain the invariant that we don't
  have arity decreases.

*  The `max 0` is important!  (\x y -> f x) has arity 2, even if f is
   unknown, hence arity 0


%************************************************************************
%*									*
	   Eta expansion
%*									*
%************************************************************************

\begin{code}
exprBotStrictness_maybe :: CoreExpr -> Maybe (Arity, StrictSig)
-- A cheap and cheerful function that identifies bottoming functions
-- and gives them a suitable strictness signatures.  It's used during
-- float-out
exprBotStrictness_maybe e
  = case getBotArity (arityType False e) of
	Nothing -> Nothing
	Just ar -> Just (ar, mkStrictSig (mkTopDmdType (replicate ar topDmd) BotRes))
\end{code}	

Note [Definition of arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The "arity" of an expression 'e' is n if
   applying 'e' to *fewer* than n *value* arguments
   converges rapidly

Or, to put it another way

   there is no work lost in duplicating the partial
   application (e x1 .. x(n-1))

In the divegent case, no work is lost by duplicating because if the thing
is evaluated once, that's the end of the program.

Or, to put it another way, in any context C

   C[ (\x1 .. xn. e x1 .. xn) ]
         is as efficient as
   C[ e ]

It's all a bit more subtle than it looks:

Note [Arity of case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We treat the arity of 
	case x of p -> \s -> ...
as 1 (or more) because for I/O ish things we really want to get that
\s to the top.  We are prepared to evaluate x each time round the loop
in order to get that.

This isn't really right in the presence of seq.  Consider
	f = \x -> case x of
			True  -> \y -> x+y
			False -> \y -> x-y
Can we eta-expand here?  At first the answer looks like "yes of course", but
consider
	(f bot) `seq` 1
This should diverge!  But if we eta-expand, it won't.   Again, we ignore this
"problem", because being scrupulous would lose an important transformation for
many programs.

1.  Note [One-shot lambdas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider one-shot lambdas
		let x = expensive in \y z -> E
We want this to have arity 1 if the \y-abstraction is a 1-shot lambda.

3.  Note [Dealing with bottom]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
	f = \x -> error "foo"
Here, arity 1 is fine.  But if it is
	f = \x -> case x of 
			True  -> error "foo"
			False -> \y -> x+y
then we want to get arity 2.  Technically, this isn't quite right, because
	(f True) `seq` 1
should diverge, but it'll converge if we eta-expand f.  Nevertheless, we
do so; it improves some programs significantly, and increasing convergence
isn't a bad thing.  Hence the ABot/ATop in ArityType.

4. Note [Newtype arity]
~~~~~~~~~~~~~~~~~~~~~~~~
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
  
Note [The state-transformer hack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have 
	f = e
where e has arity n.  Then, if we know from the context that f has
a usage type like
	t1 -> ... -> tn -1-> t(n+1) -1-> ... -1-> tm -> ...
then we can expand the arity to m.  This usage type says that
any application (x e1 .. en) will be applied to uniquely to (m-n) more args
Consider f = \x. let y = <expensive> 
		 in case x of
		      True  -> foo
		      False -> \(s:RealWorld) -> e
where foo has arity 1.  Then we want the state hack to
apply to foo too, so we can eta expand the case.

Then we expect that if f is applied to one arg, it'll be applied to two
(that's the hack -- we don't really know, and sometimes it's false)
See also Id.isOneShotBndr.

Note [State hack and bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's a terrible idea to use the state hack on a bottoming function.
Here's what happens (Trac #2861):

  f :: String -> IO T
  f = \p. error "..."

Eta-expand, using the state hack:

  f = \p. (\s. ((error "...") |> g1) s) |> g2
  g1 :: IO T ~ (S -> (S,T))
  g2 :: (S -> (S,T)) ~ IO T

Extrude the g2

  f' = \p. \s. ((error "...") |> g1) s
  f = f' |> (String -> g2)

Discard args for bottomming function

  f' = \p. \s. ((error "...") |> g1 |> g3
  g3 :: (S -> (S,T)) ~ (S,T)

Extrude g1.g3

  f'' = \p. \s. (error "...")
  f' = f'' |> (String -> S -> g1.g3)

And now we can repeat the whole loop.  Aargh!  The bug is in applying the
state hack to a function which then swallows the argument.

This arose in another guise in Trac #3959.  Here we had

     catch# (throw exn >> return ())

Note that (throw :: forall a e. Exn e => e -> a) is called with [a = IO ()].
After inlining (>>) we get 

     catch# (\_. throw {IO ()} exn)

We must *not* eta-expand to 

     catch# (\_ _. throw {...} exn)

because 'catch#' expects to get a (# _,_ #) after applying its argument to
a State#, not another function!  

In short, we use the state hack to allow us to push let inside a lambda,
but not to introduce a new lambda.


Note [ArityType]
~~~~~~~~~~~~~~~~
ArityType is the result of a compositional analysis on expressions,
from which we can decide the real arity of the expression (extracted
with function getArity).

Here is what the fields mean. If e has ArityType 
     (AT as r), where n = length as, 
then

 * If r is ABot then (e x1..xn) definitely diverges
   Partial applications may or may not diverge

 * If r is ACheap then (e x1..x(n-1)) is cheap,
   including any nested sub-expressions inside e
   (say e is (f e1 e2) then e1,e2 are cheap too)

 * e, (e x1), ... (e x1 ... x(n-1)) are definitely really 
   functions, or bottom, not casts from a data type
   So eta expansion is dynamically ok; 
    see Note [State hack and bottoming functions], 
    the part about catch#

We regard ABot as stronger than ACheap; ie if ABot holds
we don't bother about ACheap

Suppose f = \xy. x+y
Then  f             :: AT [False,False] ACheap
      f v           :: AT [False] 	ACheap
      f <expensive> :: AT [False] 	ATop
Note the ArityRes flag tells whether the whole expression is cheap.
Note also that having a non-empty 'as' doesn't mean it has that
arity; see (f <expensive>) which does not have arity 1!

The key function getArity extracts the arity (which in turn guides
eta-expansion) from ArityType. 
  * If the term is cheap or diverges we can certainly eta expand it
      e.g.   (f x)   where x has arity 2
  
  * If its a function whose first arg is one-shot (probably via the
    state hack) we can eta expand it
      e.g.   (getChar <expensive>)  

-------------------- Main arity code ----------------------------
\begin{code}
-- See Note [ArityType]
data ArityType = AT [OneShot] ArityRes
     -- There is always an explicit lambda
     -- to justify the [OneShot]

type OneShot = Bool    -- False <=> Know nothing
                       -- True  <=> Can definitely float inside this lambda
	               -- The 'True' case can arise either because a binder
		       -- is marked one-shot, or because it's a state lambda
		       -- and we have the state hack on

data ArityRes  = ATop | ACheap | ABot

vanillaArityType :: ArityType
vanillaArityType = AT [] ATop	-- Totally uninformative

-- ^ The Arity returned is the number of value args the [_$_]
-- expression can be applied to without doing much work
exprEtaExpandArity :: DynFlags -> CoreExpr -> Arity
-- exprEtaExpandArity is used when eta expanding
-- 	e  ==>  \xy -> e x y
exprEtaExpandArity dflags e
  = case (arityType dicts_cheap e) of
      AT (a:as) res | want_eta a res -> 1 + length as
      _                              -> 0
  where
    want_eta one_shot ATop   = one_shot
    want_eta _        _      = True

    dicts_cheap = dopt Opt_DictsCheap dflags

getBotArity :: ArityType -> Maybe Arity
-- Arity of a divergent function
getBotArity (AT as ABot) = Just (length as)
getBotArity _            = Nothing

arityLam :: Id -> ArityType -> ArityType
arityLam id (AT as r) = AT (isOneShotBndr id : as) r

floatIn :: Bool -> ArityType -> ArityType
-- We have something like (let x = E in b), 
-- where b has the given arity type.  
floatIn c (AT as r) = AT as (extendArityRes r c)

arityApp :: ArityType -> CoreExpr -> ArityType
-- Processing (fun arg) where at is the ArityType of fun,
arityApp (AT [] r)     arg = AT [] (extendArityRes r (exprIsCheap arg))
arityApp (AT (_:as) r) arg = AT as (extendArityRes r (exprIsCheap arg))

extendArityRes :: ArityRes -> Bool -> ArityRes
extendArityRes ABot   _    = ABot
extendArityRes ACheap True = ACheap
extendArityRes _      _    = ATop

andArityType :: ArityType -> ArityType -> ArityType   -- Used for branches of a 'case'
andArityType (AT as1 r1) (AT as2 r2) 
  = AT (go_as as1 as2) (go_r r1 r2)
  where
    go_r ABot ABot     = ABot
    go_r ABot ACheap   = ACheap
    go_r ACheap ABot   = ACheap
    go_r ACheap ACheap = ACheap
    go_r _    _        = ATop

    go_as (os1:as1) (os2:as2) = (os1 || os2) : go_as as1 as2
    go_as []        as2       = as2 
    go_as as1       []        = as1
\end{code}


\begin{code}
---------------------------
arityType :: Bool -> CoreExpr -> ArityType
arityType _ (Var v)
  | Just strict_sig <- idStrictness_maybe v
  , (ds, res) <- splitStrictSig strict_sig
  = mk_arity (length ds) res
  | otherwise
  = mk_arity (idArity v) TopRes

  where
    mk_arity id_arity res 
      | isBotRes res = AT (take id_arity one_shots) ABot
      | id_arity>0   = AT (take id_arity one_shots) ACheap
      | otherwise    = AT []                        ATop

    one_shots = typeArity (idType v)

	-- Lambdas; increase arity
arityType dicts_cheap (Lam x e)
  | isId x    = arityLam x (arityType dicts_cheap e)
  | otherwise = arityType dicts_cheap e

	-- Applications; decrease arity
arityType dicts_cheap (App fun (Type _))
   = arityType dicts_cheap fun
arityType dicts_cheap (App fun arg )
   = arityApp (arityType dicts_cheap fun) arg 

	-- Case/Let; keep arity if either the expression is cheap
	-- or it's a 1-shot lambda
	-- The former is not really right for Haskell
	--	f x = case x of { (a,b) -> \y. e }
	--  ===>
	--	f x y = case x of { (a,b) -> e }
	-- The difference is observable using 'seq'
arityType dicts_cheap (Case scrut _ _ alts)
  = floatIn (exprIsCheap scrut)
	      (foldr1 andArityType [arityType dicts_cheap rhs | (_,_,rhs) <- alts])

arityType dicts_cheap (Let b e) 
  = floatIn (cheap_bind b) (arityType dicts_cheap e)
  where
    cheap_bind (NonRec b e) = is_cheap (b,e)
    cheap_bind (Rec prs)    = all is_cheap prs
    is_cheap (b,e) = (dicts_cheap && isDictLikeTy (idType b))
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
	-- 
	-- See Note [Dictionary-like types] in TcType.lhs for why we use
	-- isDictLikeTy here rather than isDictTy

arityType dicts_cheap (Note _ e) = arityType dicts_cheap e
arityType dicts_cheap (Cast e _) = arityType dicts_cheap e
arityType _           _          = vanillaArityType
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

Note [Eta expansion and SCCs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that SCCs are not treated specially by etaExpand.  If we have
	etaExpand 2 (\x -> scc "foo" e)
	= (\xy -> (scc "foo" e) y)
So the costs of evaluating 'e' (not 'e y') are attributed to "foo"

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
-- etaExpand deals with for-alls. For example:
--		etaExpand 1 E
-- where  E :: forall a. a -> a
-- would return
--	(/\b. \y::a -> E b y)
--
-- It deals with coerces too, though they are now rare
-- so perhaps the extra code isn't worth it

etaExpand n orig_expr
  = go n orig_expr
  where
      -- Strip off existing lambdas and casts
      -- Note [Eta expansion and SCCs]
    go 0 expr = expr
    go n (Lam v body) | isTyCoVar v = Lam v (go n     body)
       	              | otherwise = Lam v (go (n-1) body)
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
    subst' | isTyCoVar v1 = CoreSubst.extendTvSubst subst v1 (mkTyVarTy v2) 
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

mkEtaWW orig_n in_scope orig_ty
  = go orig_n empty_subst orig_ty []
  where
    empty_subst = mkTvSubst in_scope emptyTvSubstEnv

    go n subst ty eis	    -- See Note [exprArity invariant]
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
         go n subst ty' (EtaCo (Type.substTy subst co) : eis)

       | otherwise	       	           -- We have an expression of arity > 0, 
       = WARN( True, ppr orig_n <+> ppr orig_ty )
         (getTvInScope subst, reverse eis) -- but its type isn't a function. 
    	-- This *can* legitmately happen:
    	-- e.g.  coerce Int (\x. x) Essentially the programmer is
	-- playing fast and loose with types (Happy does this a lot).
	-- So we simply decline to eta-expand.  Otherwise we'd end up
	-- with an explicit lambda having a non-function type
   

--------------
-- Avoiding unnecessary substitution; use short-cutting versions

subst_expr :: Subst -> CoreExpr -> CoreExpr
subst_expr = substExprSC (text "CoreArity:substExpr")

subst_bind :: Subst -> CoreBind -> (Subst, CoreBind)
subst_bind = substBindSC


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
        ty'     = Type.substTy subst ty
	eta_id' = uniqAway (getTvInScope subst) $
		  mkSysLocal (fsLit "eta") (mkBuiltinUnique n) ty'
	subst'  = extendTvInScope subst eta_id'		  
\end{code}

