%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

	Arity and eta expansion

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- | Arit and eta expansion
module CoreArity (
	manifestArity, exprArity, exprBotStrictness_maybe,
	exprEtaExpandArity, CheapFun, etaExpand
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
import TyCon	( initRecTc, checkRecTc )
import Coercion
import BasicTypes
import Unique
import DynFlags ( DynFlags, GeneralFlag(..), gopt )
import Outputable
import FastString
import Pair
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

\begin{code}
manifestArity :: CoreExpr -> Arity
-- ^ manifestArity sees how many leading value lambdas there are
manifestArity (Lam v e) | isId v    	= 1 + manifestArity e
			| otherwise 	= manifestArity e
manifestArity (Tick t e) | not (tickishIsCode t) =  manifestArity e
manifestArity (Cast e _)            	= manifestArity e
manifestArity _                     	= 0

---------------
exprArity :: CoreExpr -> Arity
-- ^ An approximate, fast, version of 'exprEtaExpandArity'
exprArity e = go e
  where
    go (Var v) 	       	           = idArity v
    go (Lam x e) | isId x    	   = go e + 1
    		 | otherwise 	   = go e
    go (Tick t e) | not (tickishIsCode t) = go e
    go (Cast e co)                 = trim_arity (go e) (pSnd (coercionKind co))
                                        -- Note [exprArity invariant]
    go (App e (Type _))            = go e
    go (App f a) | exprIsTrivial a = (go f - 1) `max` 0
        -- See Note [exprArity for applications]
	-- NB: coercions count as a value argument

    go _		       	   = 0

    trim_arity :: Arity -> Type -> Arity
    trim_arity arity ty = arity `min` length (typeArity ty)

---------------
typeArity :: Type -> [OneShot]
-- How many value arrows are visible in the type?
-- We look through foralls, and newtypes
-- See Note [exprArity invariant]
typeArity ty 
  = go initRecTc ty
  where
    go rec_nts ty 
      | Just (_, ty')  <- splitForAllTy_maybe ty 
      = go rec_nts ty'

      | Just (arg,res) <- splitFunTy_maybe ty    
      = isStateHackType arg : go rec_nts res

      | Just (tc,tys) <- splitTyConApp_maybe ty 
      , Just (ty', _) <- instNewTyCon_maybe tc tys
      , Just rec_nts' <- checkRecTc rec_nts tc  -- See Note [Expanding newtypes]
                                                -- in TyCon
--   , not (isClassTyCon tc)	-- Do not eta-expand through newtype classes
--    		      		-- See Note [Newtype classes and eta expansion]
--                              (no longer required)
      = go rec_nts' ty'
  	-- Important to look through non-recursive newtypes, so that, eg 
  	--	(f x)   where f has arity 2, f :: Int -> IO ()
  	-- Here we want to get arity 1 for the result!
        --
        -- AND through a layer of recursive newtypes
        -- e.g. newtype Stream m a b = Stream (m (Either b (a, Stream m a b)))

      | otherwise
      = []

---------------
exprBotStrictness_maybe :: CoreExpr -> Maybe (Arity, StrictSig)
-- A cheap and cheerful function that identifies bottoming functions
-- and gives them a suitable strictness signatures.  It's used during
-- float-out
exprBotStrictness_maybe e
  = case getBotArity (arityType env e) of
	Nothing -> Nothing
	Just ar -> Just (ar, sig ar)
  where
    env    = AE { ae_bndrs = [], ae_ped_bot = True, ae_cheap_fn = \ _ _ -> False }
    sig ar = mkStrictSig (mkTopDmdType (replicate ar topDmd) botRes)
                  -- For this purpose we can be very simple
\end{code}

Note [exprArity invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~
exprArity has the following invariant:

  (1) If typeArity (exprType e) = n,
      then manifestArity (etaExpand e n) = n
 
      That is, etaExpand can always expand as much as typeArity says
      So the case analysis in etaExpand and in typeArity must match
 
  (2) exprArity e <= typeArity (exprType e)      

  (3) Hence if (exprArity e) = n, then manifestArity (etaExpand e n) = n

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

Note [Newtype classes and eta expansion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NB: this nasty special case is no longer required, because
    for newtype classes we don't use the class-op rule mechanism
    at all.  See Note [Single-method classes] in TcInstDcls. SLPJ May 2013

-------- Old out of date comments, just for interest -----------
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

and now it is *much* harder for the op/$dfList rule to fire, because
exprIsConApp_maybe won't hold of the argument to op.  I considered
trying to *make* it hold, but it's tricky and I gave up.

The test simplCore/should_compile/T3722 is an excellent example.
-------- End of old out of date comments, just for interest -----------


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
	   Computing the "arity" of an expression
%*									*
%************************************************************************

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

Note [One-shot lambdas]
~~~~~~~~~~~~~~~~~~~~~~~
Consider one-shot lambdas
		let x = expensive in \y z -> E
We want this to have arity 1 if the \y-abstraction is a 1-shot lambda.

Note [Dealing with bottom]
~~~~~~~~~~~~~~~~~~~~~~~~~~
A Big Deal with computing arities is expressions like

   f = \x -> case x of
               True  -> \s -> e1
               False -> \s -> e2

This happens all the time when f :: Bool -> IO ()
In this case we do eta-expand, in order to get that \s to the
top, and give f arity 2.

This isn't really right in the presence of seq.  Consider
	(f bot) `seq` 1

This should diverge!  But if we eta-expand, it won't.  We ignore this
"problem" (unless -fpedantic-bottoms is on), because being scrupulous
would lose an important transformation for many programs. (See 
Trac #5587 for an example.)

Consider also
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

So these two transformations aren't always the Right Thing, and we
have several tickets reporting unexpected bahaviour resulting from
this transformation.  So we try to limit it as much as possible:

 (1) Do NOT move a lambda outside a known-bottom case expression
       case undefined of { (a,b) -> \y -> e }
     This showed up in Trac #5557

 (2) Do NOT move a lambda outside a case if all the branches of 
     the case are known to return bottom.
        case x of { (a,b) -> \y -> error "urk" }
     This case is less important, but the idea is that if the fn is 
     going to diverge eventually anyway then getting the best arity 
     isn't an issue, so we might as well play safe

 (3) Do NOT move a lambda outside a case unless 
     (a) The scrutinee is ok-for-speculation, or
     (b) There is an enclosing value \x, and the scrutinee is x
         E.g.  let x = case y of ( DEFAULT -> \v -> blah }
     We don't move the \y out.  This is pretty arbitrary; but it
     catches the common case of doing `seq` on y.
     This is the reason for the under_lam argument to arityType.
     See Trac #5625

Of course both (1) and (2) are readily defeated by disguising the bottoms.

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
with function exprEtaExpandArity).

Here is what the fields mean. If an arbitrary expression 'f' has 
ArityType 'at', then

 * If at = ABot n, then (f x1..xn) definitely diverges. Partial
   applications to fewer than n args may *or may not* diverge.

   We allow ourselves to eta-expand bottoming functions, even
   if doing so may lose some `seq` sharing, 
       let x = <expensive> in \y. error (g x y)
       ==> \y. let x = <expensive> in error (g x y)

 * If at = ATop as, and n=length as, 
   then expanding 'f' to (\x1..xn. f x1 .. xn) loses no sharing, 
   assuming the calls of f respect the one-shot-ness of of
   its definition.  

   NB 'f' is an arbitary expression, eg (f = g e1 e2).  This 'f'
   can have ArityType as ATop, with length as > 0, only if e1 e2 are 
   themselves.

 * In both cases, f, (f x1), ... (f x1 ... f(n-1)) are definitely
   really functions, or bottom, but *not* casts from a data type, in
   at least one case branch.  (If it's a function in one case branch but
   an unsafe cast from a data type in another, the program is bogus.)
   So eta expansion is dynamically ok; see Note [State hack and
   bottoming functions], the part about catch#

Example: 
      f = \x\y. let v = <expensive> in 
          \s(one-shot) \t(one-shot). blah
      'f' has ArityType [ManyShot,ManyShot,OneShot,OneShot]
      The one-shot-ness means we can, in effect, push that
      'let' inside the \st.


Suppose f = \xy. x+y
Then  f             :: AT [False,False] ATop
      f v           :: AT [False] 	ATop
      f <expensive> :: AT [] 	        ATop

-------------------- Main arity code ----------------------------
\begin{code}
-- See Note [ArityType]
data ArityType = ATop [OneShot] | ABot Arity
     -- There is always an explicit lambda
     -- to justify the [OneShot], or the Arity

type OneShot = Bool    -- False <=> Know nothing
                       -- True  <=> Can definitely float inside this lambda
	               -- The 'True' case can arise either because a binder
		       -- is marked one-shot, or because it's a state lambda
		       -- and we have the state hack on

vanillaArityType :: ArityType
vanillaArityType = ATop []	-- Totally uninformative

-- ^ The Arity returned is the number of value args the
-- expression can be applied to without doing much work
exprEtaExpandArity :: DynFlags -> CheapAppFun -> CoreExpr -> Arity
-- exprEtaExpandArity is used when eta expanding
-- 	e  ==>  \xy -> e x y
exprEtaExpandArity dflags cheap_app e
  = case (arityType env e) of
      ATop (os:oss) 
        | os || has_lam e -> 1 + length oss	-- Note [Eta expanding thunks]
        | otherwise       -> 0
      ATop []             -> 0
      ABot n              -> n
  where
    env = AE { ae_bndrs    = []
             , ae_cheap_fn = mk_cheap_fn dflags cheap_app
             , ae_ped_bot  = gopt Opt_PedanticBottoms dflags }

    has_lam (Tick _ e) = has_lam e
    has_lam (Lam b e)  = isId b || has_lam e
    has_lam _          = False

getBotArity :: ArityType -> Maybe Arity
-- Arity of a divergent function
getBotArity (ABot n) = Just n
getBotArity _        = Nothing

mk_cheap_fn :: DynFlags -> CheapAppFun -> CheapFun
mk_cheap_fn dflags cheap_app
  | not (gopt Opt_DictsCheap dflags)
  = \e _     -> exprIsCheap' cheap_app e
  | otherwise
  = \e mb_ty -> exprIsCheap' cheap_app e
             || case mb_ty of
                  Nothing -> False
                  Just ty -> isDictLikeTy ty
\end{code}

Note [Eta expanding through dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the experimental -fdicts-cheap flag is on, we eta-expand through
dictionary bindings.  This improves arities. Thereby, it also
means that full laziness is less prone to floating out the
application of a function to its dictionary arguments, which
can thereby lose opportunities for fusion.  Example:
	foo :: Ord a => a -> ...
     foo = /\a \(d:Ord a). let d' = ...d... in \(x:a). ....
     	-- So foo has arity 1

     f = \x. foo dInt $ bar x

The (foo DInt) is floated out, and makes ineffective a RULE 
     foo (bar x) = ...

One could go further and make exprIsCheap reply True to any
dictionary-typed expression, but that's more work.

See Note [Dictionary-like types] in TcType.lhs for why we use
isDictLikeTy here rather than isDictTy

Note [Eta expanding thunks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we see
     f = case y of p -> \x -> blah
should we eta-expand it? Well, if 'x' is a one-shot state token 
then 'yes' because 'f' will only be applied once.  But otherwise
we (conservatively) say no.  My main reason is to avoid expanding
PAPSs
	f = g d  ==>  f = \x. g d x
because that might in turn make g inline (if it has an inline pragma), 
which we might not want.  After all, INLINE pragmas say "inline only
when saturated" so we don't want to be too gung-ho about saturating!

\begin{code}
arityLam :: Id -> ArityType -> ArityType
arityLam id (ATop as) = ATop (isOneShotBndr id : as)
arityLam _  (ABot n)  = ABot (n+1)

floatIn :: Bool -> ArityType -> ArityType
-- We have something like (let x = E in b), 
-- where b has the given arity type.  
floatIn _     (ABot n)  = ABot n
floatIn True  (ATop as) = ATop as
floatIn False (ATop as) = ATop (takeWhile id as)
   -- If E is not cheap, keep arity only for one-shots

arityApp :: ArityType -> Bool -> ArityType
-- Processing (fun arg) where at is the ArityType of fun,
-- Knock off an argument and behave like 'let'
arityApp (ABot 0)      _     = ABot 0
arityApp (ABot n)      _     = ABot (n-1)
arityApp (ATop [])     _     = ATop []
arityApp (ATop (_:as)) cheap = floatIn cheap (ATop as)

andArityType :: ArityType -> ArityType -> ArityType   -- Used for branches of a 'case'
andArityType (ABot n1) (ABot n2) 
  = ABot (n1 `min` n2)
andArityType (ATop as)  (ABot _)  = ATop as
andArityType (ABot _)   (ATop bs) = ATop bs
andArityType (ATop as)  (ATop bs) = ATop (as `combine` bs)
  where	     -- See Note [Combining case branches]
    combine (a:as) (b:bs) = (a && b) : combine as bs
    combine []     bs     = take_one_shots bs
    combine as     []     = take_one_shots as

    take_one_shots [] = []
    take_one_shots (one_shot : as) 
      | one_shot  = True : take_one_shots as
      | otherwise = [] 
\end{code}

Note [Combining case branches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider    
  go = \x. let z = go e0
               go2 = \x. case x of
                           True  -> z
                           False -> \s(one-shot). e1
           in go2 x
We *really* want to eta-expand go and go2.  
When combining the barnches of the case we have
     ATop [] `andAT` ATop [True]
and we want to get ATop [True].  But if the inner
lambda wasn't one-shot we don't want to do this.
(We need a proper arity analysis to justify that.)


\begin{code}
---------------------------
type CheapFun = CoreExpr -> Maybe Type -> Bool
     	-- How to decide if an expression is cheap
	-- If the Maybe is Just, the type is the type
	-- of the expression; Nothing means "don't know"

data ArityEnv 
  = AE { ae_bndrs :: [Id]          -- Enclosing value-lambda Ids
                                   -- See Note [Dealing with bottom (3)]
       , ae_cheap_fn :: CheapFun
       , ae_ped_bot  :: Bool       -- True <=> be pedantic about bottoms
  }

arityType :: ArityEnv -> CoreExpr -> ArityType

arityType env (Cast e co)
  = case arityType env e of
      ATop os -> ATop (take co_arity os)
      ABot n  -> ABot (n `min` co_arity)
  where
    co_arity = length (typeArity (pSnd (coercionKind co)))
    -- See Note [exprArity invariant] (2); must be true of
    -- arityType too, since that is how we compute the arity
    -- of variables, and they in turn affect result of exprArity
    -- Trac #5441 is a nice demo
    -- However, do make sure that ATop -> ATop and ABot -> ABot!
    --   Casts don't affect that part. Getting this wrong provoked #5475

arityType _ (Var v)
  | strict_sig <- idStrictness v
  , not $ isTopSig strict_sig
  , (ds, res) <- splitStrictSig strict_sig
  , let arity = length ds
  = if isBotRes res then ABot arity
                    else ATop (take arity one_shots)
  | otherwise
  = ATop (take (idArity v) one_shots)
  where
    one_shots :: [Bool]	    -- One-shot-ness derived from the type
    one_shots = typeArity (idType v)

	-- Lambdas; increase arity
arityType env (Lam x e)
  | isId x    = arityLam x (arityType env' e)
  | otherwise = arityType env e
  where
    env' = env { ae_bndrs = x : ae_bndrs env }

	-- Applications; decrease arity, except for types
arityType env (App fun (Type _))
   = arityType env fun
arityType env (App fun arg )
   = arityApp (arityType env' fun) (ae_cheap_fn env arg Nothing) 
   where
     env' = env { ae_bndrs = case ae_bndrs env of
                                { [] -> []; (_:xs) -> xs } }

	-- Case/Let; keep arity if either the expression is cheap
	-- or it's a 1-shot lambda
	-- The former is not really right for Haskell
	--	f x = case x of { (a,b) -> \y. e }
	--  ===>
	--	f x y = case x of { (a,b) -> e }
	-- The difference is observable using 'seq'
	--
arityType env (Case scrut _ _ alts)
  | exprIsBottom scrut || null alts
  = ABot 0     -- Do not eta expand
               -- See Note [Dealing with bottom (1)]
  | otherwise
  = case alts_type of
     ABot n  | n>0       -> ATop []    -- Don't eta expand 
     	     | otherwise -> ABot 0     -- if RHS is bottomming
    			               -- See Note [Dealing with bottom (2)]

     ATop as | not (ae_ped_bot env)    -- Check -fpedantic-bottoms
             , is_under scrut             -> ATop as
             | exprOkForSpeculation scrut -> ATop as
             | otherwise                  -> ATop (takeWhile id as)	    
  where
    -- is_under implements Note [Dealing with bottom (3)]
    is_under (Var f)           = f `elem` ae_bndrs env
    is_under (App f (Type {})) = is_under f
    is_under (Cast f _)        = is_under f
    is_under _                 = False

    alts_type = foldr1 andArityType [arityType env rhs | (_,_,rhs) <- alts]

arityType env (Let b e) 
  = floatIn (cheap_bind b) (arityType env e)
  where
    cheap_bind (NonRec b e) = is_cheap (b,e)
    cheap_bind (Rec prs)    = all is_cheap prs
    is_cheap (b,e) = ae_cheap_fn env e (Just (idType b))

arityType env (Tick t e)
  | not (tickishIsCode t)     = arityType env e

arityType _ _ = vanillaArityType
\end{code}
  
  
%************************************************************************
%*									*
              The main eta-expander								
%*									*
%************************************************************************

We go for:
   f = \x1..xn -> N  ==>   f = \x1..xn y1..ym -> N y1..ym
				 (n >= 0)

where (in both cases) 

	* The xi can include type variables

	* The yi are all value variables

	* N is a NORMAL FORM (i.e. no redexes anywhere)
	  wanting a suitable number of extra args.

The biggest reason for doing this is for cases like

	f = \x -> case x of
		    True  -> \y -> e1
		    False -> \y -> e2

Here we want to get the lambdas together.  A good example is the nofib
program fibheaps, which gets 25% more allocation if you don't do this
eta-expansion.

We may have to sandwich some coerces between the lambdas
to make the types work.   exprEtaExpandArity looks through coerces
when computing arity; and etaExpand adds the coerces as necessary when
actually computing the expansion.

Note [No crap in eta-expanded code]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The eta expander is careful not to introduce "crap".  In particular,
given a CoreExpr satisfying the 'CpeRhs' invariant (in CorePrep), it
returns a CoreExpr satisfying the same invariant. See Note [Eta
expansion and the CorePrep invariants] in CorePrep.

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
    go n (Lam v body) | isTyVar v = Lam v (go n     body)
       	              | otherwise = Lam v (go (n-1) body)
    go n (Cast expr co) = Cast (go n expr) co
    go n expr           = -- pprTrace "ee" (vcat [ppr orig_expr, ppr expr, ppr etas]) $
       	 		  etaInfoAbs etas (etaInfoApp subst' expr etas)
    	  		where
			    in_scope = mkInScopeSet (exprFreeVars expr)
			    (in_scope', etas) = mkEtaWW n orig_expr in_scope (exprType expr)
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
  | isReflCo co = eis
  | otherwise	= EtaCo co : eis
  where
    co = co1 `mkTransCo` co2

pushCoercion co eis = EtaCo co : eis

--------------
etaInfoAbs :: [EtaInfo] -> CoreExpr -> CoreExpr
etaInfoAbs []               expr = expr
etaInfoAbs (EtaVar v : eis) expr = Lam v (etaInfoAbs eis expr)
etaInfoAbs (EtaCo co : eis) expr = Cast (etaInfoAbs eis expr) (mkSymCo co)

--------------
etaInfoApp :: Subst -> CoreExpr -> [EtaInfo] -> CoreExpr
-- (etaInfoApp s e eis) returns something equivalent to 
-- 	       ((substExpr s e) `appliedto` eis)

etaInfoApp subst (Lam v1 e) (EtaVar v2 : eis) 
  = etaInfoApp (CoreSubst.extendSubstWithVar subst v1 v2) e eis

etaInfoApp subst (Cast e co1) eis
  = etaInfoApp subst e (pushCoercion co' eis)
  where
    co' = CoreSubst.substCo subst co1

etaInfoApp subst (Case e b ty alts) eis 
  = Case (subst_expr subst e) b1 (mk_alts_ty (CoreSubst.substTy subst ty) eis) alts'
  where
    (subst1, b1) = substBndr subst b
    alts' = map subst_alt alts
    subst_alt (con, bs, rhs) = (con, bs', etaInfoApp subst2 rhs eis) 
    	      where
	      	 (subst2,bs') = substBndrs subst1 bs

    mk_alts_ty ty []               = ty
    mk_alts_ty ty (EtaVar v : eis) = mk_alts_ty (applyTypeToArg ty (varToCoreExpr v)) eis
    mk_alts_ty _  (EtaCo co : eis) = mk_alts_ty (pSnd (coercionKind co)) eis
    
etaInfoApp subst (Let b e) eis 
  = Let b' (etaInfoApp subst' e eis)
  where
    (subst', b') = subst_bind subst b

etaInfoApp subst (Tick t e) eis
  = Tick (substTickish subst t) (etaInfoApp subst e eis)

etaInfoApp subst e eis
  = go (subst_expr subst e) eis
  where
    go e []                  = e
    go e (EtaVar v    : eis) = go (App e (varToCoreExpr v)) eis
    go e (EtaCo co    : eis) = go (Cast e co) eis

--------------
mkEtaWW :: Arity -> CoreExpr -> InScopeSet -> Type
	-> (InScopeSet, [EtaInfo])
	-- EtaInfo contains fresh variables,
	--   not free in the incoming CoreExpr
	-- Outgoing InScopeSet includes the EtaInfo vars
	--   and the original free vars

mkEtaWW orig_n orig_expr in_scope orig_ty
  = go orig_n empty_subst orig_ty []
  where
    empty_subst = TvSubst in_scope emptyTvSubstEnv

    go n subst ty eis	    -- See Note [exprArity invariant]
       | n == 0
       = (getTvInScope subst, reverse eis)

       | Just (tv,ty') <- splitForAllTy_maybe ty
       , let (subst', tv') = Type.substTyVarBndr subst tv
           -- Avoid free vars of the original expression
       = go n subst' ty' (EtaVar tv' : eis)

       | Just (arg_ty, res_ty) <- splitFunTy_maybe ty
       , let (subst', eta_id') = freshEtaId n subst arg_ty 
           -- Avoid free vars of the original expression
       = go (n-1) subst' res_ty (EtaVar eta_id' : eis)
       				   
       | Just (co, ty') <- topNormaliseNewType_maybe ty
       = 	-- Given this:
       		-- 	newtype T = MkT ([T] -> Int)
       		-- Consider eta-expanding this
       		--  	eta_expand 1 e T
       		-- We want to get
       		--	coerce T (\x::[T] -> (coerce ([T]->Int) e) x)
         go n subst ty' (EtaCo co : eis)

       | otherwise	 -- We have an expression of arity > 0, 
       	 		 -- but its type isn't a function. 		   
       = WARN( True, (ppr orig_n <+> ppr orig_ty) $$ ppr orig_expr )
         (getTvInScope subst, reverse eis)
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

