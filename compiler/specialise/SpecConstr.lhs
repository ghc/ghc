%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SpecConstr]{Specialise over constructors}

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module SpecConstr(
	specConstrProgram	
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreSubst
import CoreUtils
import CoreUnfold	( couldBeSmallEnoughToInline )
import CoreLint		( showPass, endPass )
import CoreFVs 		( exprsFreeVars )
import CoreTidy		( tidyRules )
import PprCore		( pprRules )
import WwLib		( mkWorkerArgs )
import DataCon		( dataConRepArity, dataConUnivTyVars )
import Coercion	
import Type		hiding( substTy )
import Id		( Id, idName, idType, isDataConWorkId_maybe, idArity,
			  mkUserLocal, mkSysLocal, idUnfolding, isLocalId )
import Var
import VarEnv
import VarSet
import Name
import Rules		( addIdSpecialisations, mkLocalRule, rulesOfBinds )
import OccName		( mkSpecOcc )
import ErrUtils		( dumpIfSet_dyn )
import DynFlags		( DynFlags(..), DynFlag(..) )
import BasicTypes	( Activation(..) )
import Maybes		( orElse, catMaybes, isJust )
import Util
import List		( nubBy, partition )
import UniqSupply
import Outputable
import FastString
import UniqFM
\end{code}

-----------------------------------------------------
			Game plan
-----------------------------------------------------

Consider
	drop n []     = []
	drop 0 xs     = []
	drop n (x:xs) = drop (n-1) xs

After the first time round, we could pass n unboxed.  This happens in
numerical code too.  Here's what it looks like in Core:

	drop n xs = case xs of
		      []     -> []
		      (y:ys) -> case n of 
				  I# n# -> case n# of
					     0 -> []
					     _ -> drop (I# (n# -# 1#)) xs

Notice that the recursive call has an explicit constructor as argument.
Noticing this, we can make a specialised version of drop
	
	RULE: drop (I# n#) xs ==> drop' n# xs

	drop' n# xs = let n = I# n# in ...orig RHS...

Now the simplifier will apply the specialisation in the rhs of drop', giving

	drop' n# xs = case xs of
		      []     -> []
		      (y:ys) -> case n# of
				  0 -> []
				  _ -> drop (n# -# 1#) xs

Much better!  

We'd also like to catch cases where a parameter is carried along unchanged,
but evaluated each time round the loop:

	f i n = if i>0 || i>n then i else f (i*2) n

Here f isn't strict in n, but we'd like to avoid evaluating it each iteration.
In Core, by the time we've w/wd (f is strict in i) we get

	f i# n = case i# ># 0 of
		   False -> I# i#
		   True  -> case n of n' { I# n# ->
			    case i# ># n# of
				False -> I# i#
				True  -> f (i# *# 2#) n'

At the call to f, we see that the argument, n is know to be (I# n#),
and n is evaluated elsewhere in the body of f, so we can play the same
trick as above.  


Note [Reboxing]
~~~~~~~~~~~~~~~
We must be careful not to allocate the same constructor twice.  Consider
	f p = (...(case p of (a,b) -> e)...p...,
	       ...let t = (r,s) in ...t...(f t)...)
At the recursive call to f, we can see that t is a pair.  But we do NOT want
to make a specialised copy:
	f' a b = let p = (a,b) in (..., ...)
because now t is allocated by the caller, then r and s are passed to the
recursive call, which allocates the (r,s) pair again.

This happens if
  (a) the argument p is used in other than a case-scrutinsation way.
  (b) the argument to the call is not a 'fresh' tuple; you have to
	look into its unfolding to see that it's a tuple

Hence the "OR" part of Note [Good arguments] below.

ALTERNATIVE 2: pass both boxed and unboxed versions.  This no longer saves
allocation, but does perhaps save evals. In the RULE we'd have
something like

  f (I# x#) = f' (I# x#) x#

If at the call site the (I# x) was an unfolding, then we'd have to
rely on CSE to eliminate the duplicate allocation.... This alternative
doesn't look attractive enough to pursue.

ALTERNATIVE 3: ignore the reboxing problem.  The trouble is that 
the conservative reboxing story prevents many useful functions from being
specialised.  Example:
	foo :: Maybe Int -> Int -> Int
	foo   (Just m) 0 = 0
	foo x@(Just m) n = foo x (n-m)
Here the use of 'x' will clearly not require boxing in the specialised function.

The strictness analyser has the same problem, in fact.  Example:
	f p@(a,b) = ...
If we pass just 'a' and 'b' to the worker, it might need to rebox the
pair to create (a,b).  A more sophisticated analysis might figure out
precisely the cases in which this could happen, but the strictness
analyser does no such analysis; it just passes 'a' and 'b', and hopes
for the best.

So my current choice is to make SpecConstr similarly aggressive, and
ignore the bad potential of reboxing.


Note [Good arguments]
~~~~~~~~~~~~~~~~~~~~~
So we look for

* A self-recursive function.  Ignore mutual recursion for now, 
  because it's less common, and the code is simpler for self-recursion.

* EITHER

   a) At a recursive call, one or more parameters is an explicit 
      constructor application
	AND
      That same parameter is scrutinised by a case somewhere in 
      the RHS of the function

  OR

    b) At a recursive call, one or more parameters has an unfolding
       that is an explicit constructor application
	AND
      That same parameter is scrutinised by a case somewhere in 
      the RHS of the function
	AND
      Those are the only uses of the parameter (see Note [Reboxing])


What to abstract over
~~~~~~~~~~~~~~~~~~~~~
There's a bit of a complication with type arguments.  If the call
site looks like

	f p = ...f ((:) [a] x xs)...

then our specialised function look like

	f_spec x xs = let p = (:) [a] x xs in ....as before....

This only makes sense if either
  a) the type variable 'a' is in scope at the top of f, or
  b) the type variable 'a' is an argument to f (and hence fs)

Actually, (a) may hold for value arguments too, in which case
we may not want to pass them.  Supose 'x' is in scope at f's
defn, but xs is not.  Then we'd like

	f_spec xs = let p = (:) [a] x xs in ....as before....

Similarly (b) may hold too.  If x is already an argument at the
call, no need to pass it again.

Finally, if 'a' is not in scope at the call site, we could abstract
it as we do the term variables:

	f_spec a x xs = let p = (:) [a] x xs in ...as before...

So the grand plan is:

	* abstract the call site to a constructor-only pattern
	  e.g.  C x (D (f p) (g q))  ==>  C s1 (D s2 s3)

	* Find the free variables of the abstracted pattern

	* Pass these variables, less any that are in scope at
	  the fn defn.  But see Note [Shadowing] below.


NOTICE that we only abstract over variables that are not in scope,
so we're in no danger of shadowing variables used in "higher up"
in f_spec's RHS.


Note [Shadowing]
~~~~~~~~~~~~~~~~
In this pass we gather up usage information that may mention variables
that are bound between the usage site and the definition site; or (more
seriously) may be bound to something different at the definition site.
For example:

	f x = letrec g y v = let x = ... 
			     in ...(g (a,b) x)...

Since 'x' is in scope at the call site, we may make a rewrite rule that 
looks like
	RULE forall a,b. g (a,b) x = ...
But this rule will never match, because it's really a different 'x' at 
the call site -- and that difference will be manifest by the time the
simplifier gets to it.  [A worry: the simplifier doesn't *guarantee*
no-shadowing, so perhaps it may not be distinct?]

Anyway, the rule isn't actually wrong, it's just not useful.  One possibility
is to run deShadowBinds before running SpecConstr, but instead we run the
simplifier.  That gives the simplest possible program for SpecConstr to
chew on; and it virtually guarantees no shadowing.

Note [Specialising for constant parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This one is about specialising on a *constant* (but not necessarily
constructor) argument

    foo :: Int -> (Int -> Int) -> Int
    foo 0 f = 0
    foo m f = foo (f m) (+1)

It produces

    lvl_rmV :: GHC.Base.Int -> GHC.Base.Int
    lvl_rmV =
      \ (ds_dlk :: GHC.Base.Int) ->
        case ds_dlk of wild_alH { GHC.Base.I# x_alG ->
        GHC.Base.I# (GHC.Prim.+# x_alG 1)

    T.$wfoo :: GHC.Prim.Int# -> (GHC.Base.Int -> GHC.Base.Int) ->
    GHC.Prim.Int#
    T.$wfoo =
      \ (ww_sme :: GHC.Prim.Int#) (w_smg :: GHC.Base.Int -> GHC.Base.Int) ->
        case ww_sme of ds_Xlw {
          __DEFAULT ->
    	case w_smg (GHC.Base.I# ds_Xlw) of w1_Xmo { GHC.Base.I# ww1_Xmz ->
    	T.$wfoo ww1_Xmz lvl_rmV
    	};
          0 -> 0
        }

The recursive call has lvl_rmV as its argument, so we could create a specialised copy
with that argument baked in; that is, not passed at all.   Now it can perhaps be inlined.

When is this worth it?  Call the constant 'lvl'
- If 'lvl' has an unfolding that is a constructor, see if the corresponding
  parameter is scrutinised anywhere in the body.

- If 'lvl' has an unfolding that is a inlinable function, see if the corresponding
  parameter is applied (...to enough arguments...?)

  Also do this is if the function has RULES?

Also 	

Note [Specialising for lambda parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    foo :: Int -> (Int -> Int) -> Int
    foo 0 f = 0
    foo m f = foo (f m) (\n -> n-m)

This is subtly different from the previous one in that we get an
explicit lambda as the argument:

    T.$wfoo :: GHC.Prim.Int# -> (GHC.Base.Int -> GHC.Base.Int) ->
    GHC.Prim.Int#
    T.$wfoo =
      \ (ww_sm8 :: GHC.Prim.Int#) (w_sma :: GHC.Base.Int -> GHC.Base.Int) ->
        case ww_sm8 of ds_Xlr {
          __DEFAULT ->
    	case w_sma (GHC.Base.I# ds_Xlr) of w1_Xmf { GHC.Base.I# ww1_Xmq ->
    	T.$wfoo
    	  ww1_Xmq
    	  (\ (n_ad3 :: GHC.Base.Int) ->
    	     case n_ad3 of wild_alB { GHC.Base.I# x_alA ->
    	     GHC.Base.I# (GHC.Prim.-# x_alA ds_Xlr)
    	     })
    	};
          0 -> 0
        }

I wonder if SpecConstr couldn't be extended to handle this? After all,
lambda is a sort of constructor for functions and perhaps it already
has most of the necessary machinery?

Furthermore, there's an immediate win, because you don't need to allocate the lamda
at the call site; and if perchance it's called in the recursive call, then you
may avoid allocating it altogether.  Just like for constructors.

Looks cool, but probably rare...but it might be easy to implement.


Note [SpecConstr for casts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider 
    data family T a :: *
    data instance T Int = T Int

    foo n = ...
       where
         go (T 0) = 0
         go (T n) = go (T (n-1))

The recursive call ends up looking like 
	go (T (I# ...) `cast` g)
So we want to spot the construtor application inside the cast.
That's why we have the Cast case in argToPat


-----------------------------------------------------
		Stuff not yet handled
-----------------------------------------------------

Here are notes arising from Roman's work that I don't want to lose.

Example 1
~~~~~~~~~
    data T a = T !a

    foo :: Int -> T Int -> Int
    foo 0 t = 0
    foo x t | even x    = case t of { T n -> foo (x-n) t }
            | otherwise = foo (x-1) t

SpecConstr does no specialisation, because the second recursive call
looks like a boxed use of the argument.  A pity.

    $wfoo_sFw :: GHC.Prim.Int# -> T.T GHC.Base.Int -> GHC.Prim.Int#
    $wfoo_sFw =
      \ (ww_sFo [Just L] :: GHC.Prim.Int#) (w_sFq [Just L] :: T.T GHC.Base.Int) ->
    	 case ww_sFo of ds_Xw6 [Just L] {
    	   __DEFAULT ->
    		case GHC.Prim.remInt# ds_Xw6 2 of wild1_aEF [Dead Just A] {
    		  __DEFAULT -> $wfoo_sFw (GHC.Prim.-# ds_Xw6 1) w_sFq;
    		  0 ->
    		    case w_sFq of wild_Xy [Just L] { T.T n_ad5 [Just U(L)] ->
    		    case n_ad5 of wild1_aET [Just A] { GHC.Base.I# y_aES [Just L] ->
    		    $wfoo_sFw (GHC.Prim.-# ds_Xw6 y_aES) wild_Xy
    		    } } };
    	   0 -> 0

Example 2
~~~~~~~~~
    data a :*: b = !a :*: !b
    data T a = T !a

    foo :: (Int :*: T Int) -> Int
    foo (0 :*: t) = 0
    foo (x :*: t) | even x    = case t of { T n -> foo ((x-n) :*: t) }
                  | otherwise = foo ((x-1) :*: t)

Very similar to the previous one, except that the parameters are now in
a strict tuple. Before SpecConstr, we have

    $wfoo_sG3 :: GHC.Prim.Int# -> T.T GHC.Base.Int -> GHC.Prim.Int#
    $wfoo_sG3 =
      \ (ww_sFU [Just L] :: GHC.Prim.Int#) (ww_sFW [Just L] :: T.T
    GHC.Base.Int) ->
        case ww_sFU of ds_Xws [Just L] {
          __DEFAULT ->
    	case GHC.Prim.remInt# ds_Xws 2 of wild1_aEZ [Dead Just A] {
    	  __DEFAULT ->
    	    case ww_sFW of tpl_B2 [Just L] { T.T a_sFo [Just A] ->
    	    $wfoo_sG3 (GHC.Prim.-# ds_Xws 1) tpl_B2		-- $wfoo1
    	    };
    	  0 ->
    	    case ww_sFW of wild_XB [Just A] { T.T n_ad7 [Just S(L)] ->
    	    case n_ad7 of wild1_aFd [Just L] { GHC.Base.I# y_aFc [Just L] ->
    	    $wfoo_sG3 (GHC.Prim.-# ds_Xws y_aFc) wild_XB	-- $wfoo2
    	    } } };
          0 -> 0 }

We get two specialisations:
"SC:$wfoo1" [0] __forall {a_sFB :: GHC.Base.Int sc_sGC :: GHC.Prim.Int#}
		  Foo.$wfoo sc_sGC (Foo.T @ GHC.Base.Int a_sFB)
		  = Foo.$s$wfoo1 a_sFB sc_sGC ;
"SC:$wfoo2" [0] __forall {y_aFp :: GHC.Prim.Int# sc_sGC :: GHC.Prim.Int#}
		  Foo.$wfoo sc_sGC (Foo.T @ GHC.Base.Int (GHC.Base.I# y_aFp))
		  = Foo.$s$wfoo y_aFp sc_sGC ;

But perhaps the first one isn't good.  After all, we know that tpl_B2 is
a T (I# x) really, because T is strict and Int has one constructor.  (We can't
unbox the strict fields, becuase T is polymorphic!)



%************************************************************************
%*									*
\subsection{Top level wrapper stuff}
%*									*
%************************************************************************

\begin{code}
specConstrProgram :: DynFlags -> UniqSupply -> [CoreBind] -> IO [CoreBind]
specConstrProgram dflags us binds
  = do
	showPass dflags "SpecConstr"

	let (binds', _) = initUs us (go (initScEnv dflags) binds)

	endPass dflags "SpecConstr" Opt_D_dump_spec binds'

	dumpIfSet_dyn dflags Opt_D_dump_rules "Top-level specialisations"
		  (pprRules (tidyRules emptyTidyEnv (rulesOfBinds binds')))

	return binds'
  where
    go env []	        = returnUs []
    go env (bind:binds) = scBind env bind 	`thenUs` \ (env', _, bind') ->
			  go env' binds 	`thenUs` \ binds' ->
			  returnUs (bind' : binds')
\end{code}


%************************************************************************
%*									*
\subsection{Environment: goes downwards}
%*									*
%************************************************************************

\begin{code}
data ScEnv = SCE { sc_size :: Int,	-- Size threshold

		   sc_subst :: Subst,	-- Current substitution

		   sc_how_bound :: HowBoundEnv,
			-- Binds interesting non-top-level variables
			-- Domain is OutVars (*after* applying the substitution)

		   sc_vals  :: ValueEnv
			-- Domain is OutIds (*after* applying the substitution)
			-- Used even for top-level bindings (but not imported ones)
	     }

---------------------
-- As we go, we apply a substitution (sc_subst) to the current term
type InExpr = CoreExpr		-- *Before* applying the subst

type OutExpr = CoreExpr		-- *After* applying the subst
type OutId   = Id
type OutVar  = Var

---------------------
type HowBoundEnv = VarEnv HowBound	-- Domain is OutVars

---------------------
type ValueEnv = IdEnv Value		-- Domain is OutIds
data Value    = ConVal AltCon [CoreArg]	-- *Saturated* constructors
	      | LambdaVal		-- Inlinable lambdas or PAPs

instance Outputable Value where
   ppr (ConVal con args) = ppr con <+> interpp'SP args
   ppr LambdaVal	 = ptext SLIT("<Lambda>")

---------------------
initScEnv dflags
  = SCE { sc_size = specThreshold dflags,
	  sc_subst = emptySubst, 
	  sc_how_bound = emptyVarEnv, 
	  sc_vals = emptyVarEnv }

data HowBound = RecFun	-- These are the recursive functions for which 
			-- we seek interesting call patterns

	      | RecArg	-- These are those functions' arguments, or their sub-components; 
			-- we gather occurrence information for these

instance Outputable HowBound where
  ppr RecFun = text "RecFun"
  ppr RecArg = text "RecArg"

lookupHowBound :: ScEnv -> Id -> Maybe HowBound
lookupHowBound env id = lookupVarEnv (sc_how_bound env) id

scSubstId :: ScEnv -> Id -> CoreExpr
scSubstId env v = lookupIdSubst (sc_subst env) v

scSubstTy :: ScEnv -> Type -> Type
scSubstTy env ty = substTy (sc_subst env) ty

zapScSubst :: ScEnv -> ScEnv
zapScSubst env = env { sc_subst = zapSubstEnv (sc_subst env) }

extendScInScope :: ScEnv -> [Var] -> ScEnv
	-- Bring the quantified variables into scope
extendScInScope env qvars = env { sc_subst = extendInScopeList (sc_subst env) qvars }

extendScSubst :: ScEnv -> [(Var,CoreArg)] -> ScEnv
	-- Extend the substitution
extendScSubst env prs = env { sc_subst = extendSubstList (sc_subst env) prs }

extendHowBound :: ScEnv -> [Var] -> HowBound -> ScEnv
extendHowBound env bndrs how_bound
  = env { sc_how_bound = extendVarEnvList (sc_how_bound env)
			    [(bndr,how_bound) | bndr <- bndrs] }

extendBndrsWith :: HowBound -> ScEnv -> [Var] -> (ScEnv, [Var])
extendBndrsWith how_bound env bndrs 
  = (env { sc_subst = subst', sc_how_bound = hb_env' }, bndrs')
  where
    (subst', bndrs') = substBndrs (sc_subst env) bndrs
    hb_env' = sc_how_bound env `extendVarEnvList` 
		    [(bndr,how_bound) | bndr <- bndrs']

extendBndrWith :: HowBound -> ScEnv -> Var -> (ScEnv, Var)
extendBndrWith how_bound env bndr 
  = (env { sc_subst = subst', sc_how_bound = hb_env' }, bndr')
  where
    (subst', bndr') = substBndr (sc_subst env) bndr
    hb_env' = extendVarEnv (sc_how_bound env) bndr' how_bound

extendRecBndrs :: ScEnv -> [Var] -> (ScEnv, [Var])
extendRecBndrs env bndrs  = (env { sc_subst = subst' }, bndrs')
		      where
			(subst', bndrs') = substRecBndrs (sc_subst env) bndrs

extendBndr :: ScEnv -> Var -> (ScEnv, Var)
extendBndr  env bndr  = (env { sc_subst = subst' }, bndr')
		      where
			(subst', bndr') = substBndr (sc_subst env) bndr

extendValEnv :: ScEnv -> Id -> Maybe Value -> ScEnv
extendValEnv env id Nothing   = env
extendValEnv env id (Just cv) = env { sc_vals = extendVarEnv (sc_vals env) id cv }

extendCaseBndrs :: ScEnv -> CoreExpr -> Id -> AltCon -> [Var] -> ScEnv
-- When we encounter
--	case scrut of b
--	    C x y -> ...
-- we want to bind b, and perhaps scrut too, to (C x y)
-- NB: Extends only the sc_vals part of the envt
extendCaseBndrs env scrut case_bndr con alt_bndrs
  = case scrut of
	Var v -> extendValEnv env1 v cval
	other -> env1
 where
   env1 = extendValEnv env case_bndr cval
   cval = case con of
		DEFAULT    -> Nothing
		LitAlt lit -> Just (ConVal con [])
		DataAlt dc -> Just (ConVal con vanilla_args)
		      where
		       	vanilla_args = map Type (tyConAppArgs (idType case_bndr)) ++
				       varsToCoreExprs alt_bndrs
\end{code}


%************************************************************************
%*									*
\subsection{Usage information: flows upwards}
%*									*
%************************************************************************

\begin{code}
data ScUsage
   = SCU {
	calls :: CallEnv,		-- Calls
					-- The functions are a subset of the 
					-- 	RecFuns in the ScEnv

	occs :: !(IdEnv ArgOcc)		-- Information on argument occurrences
     }					-- The variables are a subset of the 
					--	RecArg in the ScEnv

type CallEnv = IdEnv [Call]
type Call = (ValueEnv, [CoreArg])
	-- The arguments of the call, together with the
	-- env giving the constructor bindings at the call site

nullUsage = SCU { calls = emptyVarEnv, occs = emptyVarEnv }

combineCalls :: CallEnv -> CallEnv -> CallEnv
combineCalls = plusVarEnv_C (++)

combineUsage u1 u2 = SCU { calls = combineCalls (calls u1) (calls u2),
			   occs  = plusVarEnv_C combineOcc (occs u1) (occs u2) }

combineUsages [] = nullUsage
combineUsages us = foldr1 combineUsage us

lookupOcc :: ScUsage -> Var -> (ScUsage, ArgOcc)
lookupOcc (SCU { calls = sc_calls, occs = sc_occs }) bndr
  = (SCU {calls = sc_calls, occs = delVarEnv sc_occs bndr},
     lookupVarEnv sc_occs bndr `orElse` NoOcc)

lookupOccs :: ScUsage -> [Var] -> (ScUsage, [ArgOcc])
lookupOccs (SCU { calls = sc_calls, occs = sc_occs }) bndrs
  = (SCU {calls = sc_calls, occs = delVarEnvList sc_occs bndrs},
     [lookupVarEnv sc_occs b `orElse` NoOcc | b <- bndrs])

data ArgOcc = NoOcc	-- Doesn't occur at all; or a type argument
	    | UnkOcc	-- Used in some unknown way

	    | ScrutOcc (UniqFM [ArgOcc])	-- See Note [ScrutOcc]

	    | BothOcc	-- Definitely taken apart, *and* perhaps used in some other way

{-	Note  [ScrutOcc]

An occurrence of ScrutOcc indicates that the thing, or a `cast` version of the thing,
is *only* taken apart or applied.

  Functions, literal: ScrutOcc emptyUFM
  Data constructors:  ScrutOcc subs,

where (subs :: UniqFM [ArgOcc]) gives usage of the *pattern-bound* components,
The domain of the UniqFM is the Unique of the data constructor

The [ArgOcc] is the occurrences of the *pattern-bound* components 
of the data structure.  E.g.
	data T a = forall b. MkT a b (b->a)
A pattern binds b, x::a, y::b, z::b->a, but not 'a'!

-}

instance Outputable ArgOcc where
  ppr (ScrutOcc xs) = ptext SLIT("scrut-occ") <> ppr xs
  ppr UnkOcc 	    = ptext SLIT("unk-occ")
  ppr BothOcc 	    = ptext SLIT("both-occ")
  ppr NoOcc    	    = ptext SLIT("no-occ")

-- Experimentally, this vesion of combineOcc makes ScrutOcc "win", so
-- that if the thing is scrutinised anywhere then we get to see that
-- in the overall result, even if it's also used in a boxed way
-- This might be too agressive; see Note [Reboxing] Alternative 3
combineOcc NoOcc	 occ 	       = occ
combineOcc occ 		 NoOcc	       = occ
combineOcc (ScrutOcc xs) (ScrutOcc ys) = ScrutOcc (plusUFM_C combineOccs xs ys)
combineOcc occ           (ScrutOcc ys) = ScrutOcc ys
combineOcc (ScrutOcc xs) occ	       = ScrutOcc xs
combineOcc UnkOcc        UnkOcc        = UnkOcc
combineOcc _	    _	     	       = BothOcc

combineOccs :: [ArgOcc] -> [ArgOcc] -> [ArgOcc]
combineOccs xs ys = zipWithEqual "combineOccs" combineOcc xs ys

setScrutOcc :: ScEnv -> ScUsage -> CoreExpr -> ArgOcc -> ScUsage
-- *Overwrite* the occurrence info for the scrutinee, if the scrutinee 
-- is a variable, and an interesting variable
setScrutOcc env usg (Cast e _) occ = setScrutOcc env usg e occ
setScrutOcc env usg (Note _ e) occ = setScrutOcc env usg e occ
setScrutOcc env usg (Var v)    occ
  | Just RecArg <- lookupHowBound env v = usg { occs = extendVarEnv (occs usg) v occ }
  | otherwise				= usg
setScrutOcc env usg other occ	-- Catch-all
  = usg	

conArgOccs :: ArgOcc -> AltCon -> [ArgOcc]
-- Find usage of components of data con; returns [UnkOcc...] if unknown
-- See Note [ScrutOcc] for the extra UnkOccs in the vanilla datacon case

conArgOccs (ScrutOcc fm) (DataAlt dc) 
  | Just pat_arg_occs <- lookupUFM fm dc
  = [UnkOcc | tv <- dataConUnivTyVars dc] ++ pat_arg_occs

conArgOccs other con = repeat UnkOcc
\end{code}

%************************************************************************
%*									*
\subsection{The main recursive function}
%*									*
%************************************************************************

The main recursive function gathers up usage information, and
creates specialised versions of functions.

\begin{code}
scExpr :: ScEnv -> CoreExpr -> UniqSM (ScUsage, CoreExpr)
	-- The unique supply is needed when we invent
	-- a new name for the specialised function and its args

scExpr env e = scExpr' env e


scExpr' env (Var v)     = case scSubstId env v of
		       	    Var v' -> returnUs (varUsage env v UnkOcc, Var v')
		            e'     -> scExpr (zapScSubst env) e'

scExpr' env e@(Type t)  = returnUs (nullUsage, Type (scSubstTy env t))
scExpr' env e@(Lit l)   = returnUs (nullUsage, e)
scExpr' env (Note n e)  = do { (usg,e') <- scExpr env e
			    ; return (usg, Note n e') }
scExpr' env (Cast e co) = do { (usg, e') <- scExpr env e
		            ; return (usg, Cast e' (scSubstTy env co)) }
scExpr' env (Lam b e)   = do { let (env', b') = extendBndr env b
			    ; (usg, e') <- scExpr env' e
			    ; return (usg, Lam b' e') }

scExpr' env (Case scrut b ty alts) 
  = do	{ (scrut_usg, scrut') <- scExpr env scrut
	; case isValue (sc_vals env) scrut' of
		Just (ConVal con args) -> sc_con_app con args scrut'
		other		       -> sc_vanilla scrut_usg scrut'
	}
  where
    sc_con_app con args scrut' 	-- Known constructor; simplify
	= do { let (_, bs, rhs) = findAlt con alts
		   alt_env' = extendScSubst env ((b,scrut') : bs `zip` trimConArgs con args)
	     ; scExpr alt_env' rhs }
				
    sc_vanilla scrut_usg scrut'	-- Normal case
     = do { let (alt_env,b') = extendBndrWith RecArg env b
			-- Record RecArg for the components

	  ; (alt_usgs, alt_occs, alts')
		<- mapAndUnzip3Us (sc_alt alt_env scrut' b') alts

	  ; let (alt_usg, b_occ) = lookupOcc (combineUsages alt_usgs) b
		scrut_occ        = foldr combineOcc b_occ alt_occs
		scrut_usg'       = setScrutOcc env scrut_usg scrut' scrut_occ
	  	-- The combined usage of the scrutinee is given
	  	-- by scrut_occ, which is passed to scScrut, which
	  	-- in turn treats a bare-variable scrutinee specially

	  ; return (alt_usg `combineUsage` scrut_usg',
	  	    Case scrut' b' (scSubstTy env ty) alts') }

    sc_alt env scrut' b' (con,bs,rhs)
      = do { let (env1, bs') = extendBndrsWith RecArg env bs
		 env2        = extendCaseBndrs env1 scrut' b' con bs'
	   ; (usg,rhs') <- scExpr env2 rhs
	   ; let (usg', arg_occs) = lookupOccs usg bs
		 scrut_occ = case con of
				DataAlt dc -> ScrutOcc (unitUFM dc arg_occs)
				other	   -> ScrutOcc emptyUFM
	   ; return (usg', scrut_occ, (con,bs',rhs')) }

scExpr' env (Let (NonRec bndr rhs) body)
  = do	{ let (body_env, bndr') = extendBndr env bndr
	; (rhs_usg, rhs_info@(_, args', rhs_body', _)) <- scRecRhs env (bndr',rhs)

	; if null args' || isEmptyVarEnv (calls rhs_usg) then do
	    do	{ 	-- Vanilla case
		  let rhs' = mkLams args' rhs_body'
		      body_env2 = extendValEnv body_env bndr' (isValue (sc_vals env) rhs')
			-- Record if the RHS is a value
		; (body_usg, body') <- scExpr body_env2 body
		; return (body_usg `combineUsage` rhs_usg, Let (NonRec bndr' rhs') body') }
	  else 
	    do	{ 	-- Join-point case
		  let body_env2 = extendHowBound body_env [bndr'] RecFun
			-- If the RHS of this 'let' contains calls
			-- to recursive functions that we're trying
			-- to specialise, then treat this let too
			-- as one to specialise
		; (body_usg, body') <- scExpr body_env2 body

		; (spec_usg, _, specs) <- specialise env (calls body_usg) ([], rhs_info)

		; return (body_usg { calls = calls body_usg `delVarEnv` bndr' } 
			  `combineUsage` rhs_usg `combineUsage` spec_usg,
			  mkLets [NonRec b r | (b,r) <- addRules rhs_info specs] body')
	}	}

scExpr' env (Let (Rec prs) body)
  = do	{ (env', bind_usg, bind') <- scBind env (Rec prs)
	; (body_usg, body') <- scExpr env' body
	; return (bind_usg `combineUsage` body_usg, Let bind' body') }

scExpr' env e@(App _ _) 
  = do	{ let (fn, args) = collectArgs e
	; (fn_usg, fn') <- scExpr env fn
	-- Process the function too.   It's almost always a variable,
	-- but not always.  In particular, if this pass follows float-in,
	-- which it may, we can get 
	--	(let f = ...f... in f) arg1 arg2
	-- Also the substitution may replace a variable by a non-variable

	; let fn_usg' = setScrutOcc env fn_usg fn' (ScrutOcc emptyUFM)
	-- We use setScrutOcc to record the fact that the function is called
	-- Perhaps we should check that it has at least one value arg, 
	-- but currently we don't bother

	; (arg_usgs, args') <- mapAndUnzipUs (scExpr env) args
	; let call_usg = case fn' of
		 	   Var f | Just RecFun <- lookupHowBound env f
				 , not (null args)	-- Not a proper call!
			         -> SCU { calls = unitVarEnv f [(sc_vals env, args')], 
				          occs  = emptyVarEnv }
			   other -> nullUsage
	; return (combineUsages arg_usgs `combineUsage` fn_usg' 
				         `combineUsage` call_usg,
	          mkApps fn' args') }


----------------------
scBind :: ScEnv -> CoreBind -> UniqSM (ScEnv, ScUsage, CoreBind)
scBind env (Rec prs)
  | not (all (couldBeSmallEnoughToInline (sc_size env)) rhss)
		-- No specialisation
  = do	{ let (rhs_env,bndrs') = extendRecBndrs env bndrs
	; (rhs_usgs, rhss') <- mapAndUnzipUs (scExpr rhs_env) rhss
	; return (rhs_env, combineUsages rhs_usgs, Rec (bndrs' `zip` rhss')) }
  | otherwise	-- Do specialisation
  = do	{ let (rhs_env1,bndrs') = extendRecBndrs env bndrs
	      rhs_env2 = extendHowBound rhs_env1 bndrs' RecFun

	; (rhs_usgs, rhs_infos) <- mapAndUnzipUs (scRecRhs rhs_env2) (bndrs' `zip` rhss)
	; let rhs_usg = combineUsages rhs_usgs

	; (spec_usg, specs) <- spec_loop rhs_env2 (calls rhs_usg)
					 (repeat [] `zip` rhs_infos)

	; let all_usg = rhs_usg `combineUsage` spec_usg

	; return (rhs_env1,  -- For the body of the letrec, delete the RecFun business
		  all_usg { calls = calls rhs_usg `delVarEnvList` bndrs' },
		  Rec (concat (zipWith addRules rhs_infos specs))) }
  where
    (bndrs,rhss) = unzip prs

    spec_loop :: ScEnv
	      -> CallEnv
	      -> [([CallPat], RhsInfo)]			-- One per binder
	      -> UniqSM (ScUsage, [[SpecInfo]])		-- One list per binder
    spec_loop env all_calls rhs_stuff
	= do { (spec_usg_s, new_pats_s, specs) <- mapAndUnzip3Us (specialise env all_calls) rhs_stuff
	     ; let spec_usg = combineUsages spec_usg_s
	     ; if all null new_pats_s then
		return (spec_usg, specs) else do
	     { (spec_usg1, specs1) <- spec_loop env (calls spec_usg) 
						(zipWith add_pats new_pats_s rhs_stuff)
	     ; return (spec_usg `combineUsage` spec_usg1, zipWith (++) specs specs1) } }

    add_pats :: [CallPat] -> ([CallPat], RhsInfo) -> ([CallPat], RhsInfo)
    add_pats new_pats (done_pats, rhs_info) = (done_pats ++ new_pats, rhs_info)

scBind env (NonRec bndr rhs)
  = do	{ (usg, rhs') <- scExpr env rhs
	; let (env1, bndr') = extendBndr env bndr
	      env2 = extendValEnv env1 bndr' (isValue (sc_vals env) rhs')
	; return (env2, usg, NonRec bndr' rhs') }

----------------------
scRecRhs :: ScEnv -> (OutId, InExpr) -> UniqSM (ScUsage, RhsInfo)
scRecRhs env (bndr,rhs)
  = do	{ let (arg_bndrs,body) = collectBinders rhs
	      (body_env, arg_bndrs') = extendBndrsWith RecArg env arg_bndrs
	; (body_usg, body') <- scExpr body_env body
	; let (rhs_usg, arg_occs) = lookupOccs body_usg arg_bndrs'
	; return (rhs_usg, (bndr, arg_bndrs', body', arg_occs)) }

		-- The arg_occs says how the visible,
		-- lambda-bound binders of the RHS are used
		-- (including the TyVar binders)
	 	-- Two pats are the same if they match both ways

----------------------
addRules :: RhsInfo -> [SpecInfo] -> [(Id,CoreExpr)]
addRules (fn, args, body, _) specs
  = [(id,rhs) | (_,id,rhs) <- specs] ++ 
    [(fn `addIdSpecialisations` rules, mkLams args body)]
  where
    rules = [r | (r,_,_) <- specs]

----------------------
varUsage env v use 
  | Just RecArg <- lookupHowBound env v = SCU { calls = emptyVarEnv, 
						occs = unitVarEnv v use }
  | otherwise		   	        = nullUsage
\end{code}


%************************************************************************
%*									*
		The specialiser itself
%*									*
%************************************************************************

\begin{code}
type RhsInfo = (OutId, [OutVar], OutExpr, [ArgOcc])
	-- Info about the *original* RHS of a binding we are specialising
 	-- Original binding f = \xs.body
	-- Plus info about usage of arguments

type SpecInfo = (CoreRule, OutId, OutExpr)
	-- One specialisation: Rule plus definition


specialise 
   :: ScEnv
   -> CallEnv				-- Info on calls
   -> ([CallPat], RhsInfo)		-- Original RHS plus patterns dealt with
   -> UniqSM (ScUsage, [CallPat], [SpecInfo])	-- Specialised calls

-- Note: the rhs here is the optimised version of the original rhs
-- So when we make a specialised copy of the RHS, we're starting
-- from an RHS whose nested functions have been optimised already.

specialise env bind_calls (done_pats, (fn, arg_bndrs, body, arg_occs))
  | notNull arg_bndrs,	-- Only specialise functions
    Just all_calls <- lookupVarEnv bind_calls fn
  = do	{ pats <- callsToPats env done_pats arg_occs all_calls
--	; pprTrace "specialise" (vcat [ppr fn <+> ppr arg_occs,
--	  				text "calls" <+> ppr all_calls,
--	  				text "good pats" <+> ppr pats])  $
--	  return ()

	; (spec_usgs, specs) <- mapAndUnzipUs (spec_one env fn arg_bndrs body)
					      (pats `zip` [length done_pats..])

	; return (combineUsages spec_usgs, pats, specs) }
  | otherwise
  = return (nullUsage, [], [])		-- The boring case


---------------------
spec_one :: ScEnv
	 -> OutId	-- Function
	 -> [Var]	-- Lambda-binders of RHS; should match patterns
	 -> CoreExpr	-- Body of the original function
	 -> (([Var], [CoreArg]), Int)
	 -> UniqSM (ScUsage, SpecInfo)	-- Rule and binding

-- spec_one creates a specialised copy of the function, together
-- with a rule for using it.  I'm very proud of how short this
-- function is, considering what it does :-).

{- 
  Example
  
     In-scope: a, x::a   
     f = /\b \y::[(a,b)] -> ....f (b,c) ((:) (a,(b,c)) (x,v) (h w))...
	  [c::*, v::(b,c) are presumably bound by the (...) part]
  ==>
     f_spec = /\ b c \ v::(b,c) hw::[(a,(b,c))] ->
		  (...entire body of f...) [b -> (b,c), 
  					    y -> ((:) (a,(b,c)) (x,v) hw)]
  
     RULE:  forall b::* c::*,		-- Note, *not* forall a, x
		   v::(b,c),
		   hw::[(a,(b,c))] .
  
	    f (b,c) ((:) (a,(b,c)) (x,v) hw) = f_spec b c v hw
-}

spec_one env fn arg_bndrs body ((qvars, pats), rule_number)
  = do	{ 	-- Specialise the body
	  let spec_env = extendScSubst (extendScInScope env qvars)
				       (arg_bndrs `zip` pats)
	; (spec_usg, spec_body) <- scExpr spec_env body

--	; pprTrace "spec_one" (ppr fn <+> vcat [text "pats" <+> ppr pats,
--			text "calls" <+> (ppr (calls spec_usg))])
--	  (return ())

		-- And build the results
	; spec_uniq <- getUniqueUs
	; let (spec_lam_args, spec_call_args) = mkWorkerArgs qvars body_ty
	      	-- Usual w/w hack to avoid generating 
	      	-- a spec_rhs of unlifted type and no args
	
	      fn_name   = idName fn
	      fn_loc    = nameSrcSpan fn_name
	      spec_occ  = mkSpecOcc (nameOccName fn_name)
	      rule_name = mkFastString ("SC:" ++ showSDoc (ppr fn <> int rule_number))
	      spec_rhs  = mkLams spec_lam_args spec_body
	      spec_id   = mkUserLocal spec_occ spec_uniq (mkPiTypes spec_lam_args body_ty) fn_loc
	      body_ty   = exprType spec_body
	      rule_rhs  = mkVarApps (Var spec_id) spec_call_args
	      rule      = mkLocalRule rule_name specConstrActivation fn_name qvars pats rule_rhs
	; return (spec_usg, (rule, spec_id, spec_rhs)) }

-- In which phase should the specialise-constructor rules be active?
-- Originally I made them always-active, but Manuel found that
-- this defeated some clever user-written rules.  So Plan B
-- is to make them active only in Phase 0; after all, currently,
-- the specConstr transformation is only run after the simplifier
-- has reached Phase 0.  In general one would want it to be 
-- flag-controllable, but for now I'm leaving it baked in
--					[SLPJ Oct 01]
specConstrActivation :: Activation
specConstrActivation = ActiveAfter 0	-- Baked in; see comments above
\end{code}

%************************************************************************
%*									*
\subsection{Argument analysis}
%*									*
%************************************************************************

This code deals with analysing call-site arguments to see whether
they are constructor applications.


\begin{code}
type CallPat = ([Var], [CoreExpr])	-- Quantified variables and arguments


callsToPats :: ScEnv -> [CallPat] -> [ArgOcc] -> [Call] -> UniqSM [CallPat]
	-- Result has no duplicate patterns, 
	-- nor ones mentioned in done_pats
callsToPats env done_pats bndr_occs calls
  = do	{ mb_pats <- mapM (callToPats env bndr_occs) calls

	; let good_pats :: [([Var], [CoreArg])]
	      good_pats = catMaybes mb_pats
	      is_done p = any (samePat p) done_pats

	; return (filterOut is_done (nubBy samePat good_pats)) }

callToPats :: ScEnv -> [ArgOcc] -> Call -> UniqSM (Maybe CallPat)
	-- The [Var] is the variables to quantify over in the rule
	--	Type variables come first, since they may scope 
	--	over the following term variables
	-- The [CoreExpr] are the argument patterns for the rule
callToPats env bndr_occs (con_env, args)
  | length args < length bndr_occs	-- Check saturated
  = return Nothing
  | otherwise
  = do	{ let in_scope = substInScope (sc_subst env)
	; prs <- argsToPats in_scope con_env (args `zip` bndr_occs)
	; let (good_pats, pats) = unzip prs
	      pat_fvs = varSetElems (exprsFreeVars pats)
	      qvars   = filterOut (`elemInScopeSet` in_scope) pat_fvs
		-- Quantify over variables that are not in sccpe
		-- at the call site
		-- See Note [Shadowing] at the top
		
	      (tvs, ids) = partition isTyVar qvars
	      qvars'     = tvs ++ ids
		-- Put the type variables first; the type of a term
		-- variable may mention a type variable

	; -- pprTrace "callToPats"  (ppr args $$ ppr prs $$ ppr bndr_occs) $
	  if or good_pats 
	  then return (Just (qvars', pats))
	  else return Nothing }

    -- argToPat takes an actual argument, and returns an abstracted
    -- version, consisting of just the "constructor skeleton" of the
    -- argument, with non-constructor sub-expression replaced by new
    -- placeholder variables.  For example:
    --    C a (D (f x) (g y))  ==>  C p1 (D p2 p3)

argToPat :: InScopeSet			-- What's in scope at the fn defn site
	 -> ValueEnv			-- ValueEnv at the call site
	 -> CoreArg			-- A call arg (or component thereof)
	 -> ArgOcc
	 -> UniqSM (Bool, CoreArg)
-- Returns (interesting, pat), 
-- where pat is the pattern derived from the argument
--	      intersting=True if the pattern is non-trivial (not a variable or type)
-- E.g.		x:xs	     --> (True, x:xs)
--		f xs         --> (False, w)	   where w is a fresh wildcard
--		(f xs, 'c')  --> (True, (w, 'c'))  where w is a fresh wildcard
--		\x. x+y      --> (True, \x. x+y)
--		lvl7	     --> (True, lvl7)	   if lvl7 is bound 
--						   somewhere further out

argToPat in_scope val_env arg@(Type ty) arg_occ
  = return (False, arg)

argToPat in_scope val_env (Note n arg) arg_occ
  = argToPat in_scope val_env arg arg_occ
	-- Note [Notes in call patterns]
	-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	-- Ignore Notes.  In particular, we want to ignore any InlineMe notes
	-- Perhaps we should not ignore profiling notes, but I'm going to
	-- ride roughshod over them all for now.
	--- See Note [Notes in RULE matching] in Rules

argToPat in_scope val_env (Let _ arg) arg_occ
  = argToPat in_scope val_env arg arg_occ
	-- Look through let expressions
	-- e.g.		f (let v = rhs in \y -> ...v...)
	-- Here we can specialise for f (\y -> ...)
	-- because the rule-matcher will look through the let.

argToPat in_scope val_env (Cast arg co) arg_occ
  = do	{ (interesting, arg') <- argToPat in_scope val_env arg arg_occ
	; let (ty1,ty2) = coercionKind co
	; if not interesting then 
		wildCardPat ty2
	  else do
	{ -- Make a wild-card pattern for the coercion
	  uniq <- getUniqueUs
	; let co_name = mkSysTvName uniq FSLIT("sg")
	      co_var = mkCoVar co_name (mkCoKind ty1 ty2)
	; return (interesting, Cast arg' (mkTyVarTy co_var)) } }

{-	Disabling lambda specialisation for now
	It's fragile, and the spec_loop can be infinite
argToPat in_scope val_env arg arg_occ
  | is_value_lam arg
  = return (True, arg)
  where
    is_value_lam (Lam v e) 	-- Spot a value lambda, even if 
	| isId v = True		-- it is inside a type lambda
	| otherwise = is_value_lam e
    is_value_lam other = False
-}

  -- Check for a constructor application
  -- NB: this *precedes* the Var case, so that we catch nullary constrs
argToPat in_scope val_env arg arg_occ
  | Just (ConVal dc args) <- isValue val_env arg
  , case arg_occ of
	ScrutOcc _ -> True		-- Used only by case scrutinee
	BothOcc    -> case arg of	-- Used elsewhere
			App {} -> True	--     see Note [Reboxing]
			other  -> False
	other	   -> False	-- No point; the arg is not decomposed
  = do	{ args' <- argsToPats in_scope val_env (args `zip` conArgOccs arg_occ dc)
	; return (True, mk_con_app dc (map snd args')) }

  -- Check if the argument is a variable that 
  -- is in scope at the function definition site
  -- It's worth specialising on this if
  --	(a) it's used in an interesting way in the body
  --	(b) we know what its value is
argToPat in_scope val_env (Var v) arg_occ
  | case arg_occ of { UnkOcc -> False; other -> True },	-- (a)
    is_value						-- (b)
  = return (True, Var v)
  where
    is_value 
	| isLocalId v = v `elemInScopeSet` in_scope 
			&& isJust (lookupVarEnv val_env v)
		-- Local variables have values in val_env
	| otherwise   = isValueUnfolding (idUnfolding v)
		-- Imports have unfoldings

--	I'm really not sure what this comment means
--	And by not wild-carding we tend to get forall'd 
--	variables that are in soope, which in turn can
--	expose the weakness in let-matching
--	See Note [Matching lets] in Rules
  -- Check for a variable bound inside the function. 
  -- Don't make a wild-card, because we may usefully share
  --	e.g.  f a = let x = ... in f (x,x)
  -- NB: this case follows the lambda and con-app cases!!
argToPat in_scope val_env (Var v) arg_occ
  = return (False, Var v)

  -- The default case: make a wild-card
argToPat in_scope val_env arg arg_occ
  = wildCardPat (exprType arg)

wildCardPat :: Type -> UniqSM (Bool, CoreArg)
wildCardPat ty = do { uniq <- getUniqueUs
		    ; let id = mkSysLocal FSLIT("sc") uniq ty
		    ; return (False, Var id) }

argsToPats :: InScopeSet -> ValueEnv
	   -> [(CoreArg, ArgOcc)]
	   -> UniqSM [(Bool, CoreArg)]
argsToPats in_scope val_env args
  = mapUs do_one args
  where
    do_one (arg,occ) = argToPat in_scope val_env arg occ
\end{code}


\begin{code}
isValue :: ValueEnv -> CoreExpr -> Maybe Value
isValue env (Lit lit)
  = Just (ConVal (LitAlt lit) [])

isValue env (Var v)
  | Just stuff <- lookupVarEnv env v
  = Just stuff	-- You might think we could look in the idUnfolding here
		-- but that doesn't take account of which branch of a 
		-- case we are in, which is the whole point

  | not (isLocalId v) && isCheapUnfolding unf
  = isValue env (unfoldingTemplate unf)
  where
    unf = idUnfolding v
	-- However we do want to consult the unfolding 
	-- as well, for let-bound constructors!

isValue env (Lam b e)
  | isTyVar b = isValue env e
  | otherwise = Just LambdaVal

isValue env expr	-- Maybe it's a constructor application
  | (Var fun, args) <- collectArgs expr
  = case isDataConWorkId_maybe fun of

	Just con | args `lengthAtLeast` dataConRepArity con 
		-- Check saturated; might be > because the 
		--		    arity excludes type args
		-> Just (ConVal (DataAlt con) args)

	other | valArgCount args < idArity fun
		-- Under-applied function
	      -> Just LambdaVal	-- Partial application

	other -> Nothing

isValue env expr = Nothing

mk_con_app :: AltCon -> [CoreArg] -> CoreExpr
mk_con_app (LitAlt lit)  []   = Lit lit
mk_con_app (DataAlt con) args = mkConApp con args
mk_con_app other args = panic "SpecConstr.mk_con_app"

samePat :: CallPat -> CallPat -> Bool
samePat (vs1, as1) (vs2, as2)
  = all2 same as1 as2
  where
    same (Var v1) (Var v2) 
	| v1 `elem` vs1 = v2 `elem` vs2
	| v2 `elem` vs2 = False
	| otherwise     = v1 == v2

    same (Lit l1)    (Lit l2)    = l1==l2
    same (App f1 a1) (App f2 a2) = same f1 f2 && same a1 a2

    same (Type t1) (Type t2) = True	-- Note [Ignore type differences]
    same (Note _ e1) e2	= same e1 e2	-- Ignore casts and notes
    same (Cast e1 _) e2	= same e1 e2
    same e1 (Note _ e2) = same e1 e2
    same e1 (Cast e2 _) = same e1 e2

    same e1 e2 = WARN( bad e1 || bad e2, ppr e1 $$ ppr e2) 
		 False 	-- Let, lambda, case should not occur
    bad (Case {}) = True
    bad (Let {})  = True
    bad (Lam {})  = True
    bad other	  = False
\end{code}

Note [Ignore type differences]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not want to generate specialisations where the call patterns
differ only in their type arguments!  Not only is it utterly useless,
but it also means that (with polymorphic recursion) we can generate
an infinite number of specialisations. Example is Data.Sequence.adjustTree, 
I think.

