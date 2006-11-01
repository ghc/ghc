%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SpecConstr]{Specialise over constructors}

\begin{code}
module SpecConstr(
	specConstrProgram	
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreLint		( showPass, endPass )
import CoreUtils	( exprType, mkPiTypes )
import CoreFVs 		( exprsFreeVars )
import CoreTidy		( tidyRules )
import PprCore		( pprRules )
import WwLib		( mkWorkerArgs )
import DataCon		( dataConRepArity, dataConUnivTyVars )
import Type		( Type, tyConAppArgs )
import Coercion		( coercionKind )
import Rules		( matchN )
import Id		( Id, idName, idType, isDataConWorkId_maybe, 
			  mkUserLocal, mkSysLocal, idUnfolding, isLocalId )
import Var		( Var )
import VarEnv
import VarSet
import Name		( nameOccName, nameSrcLoc )
import Rules		( addIdSpecialisations, mkLocalRule, rulesOfBinds )
import OccName		( mkSpecOcc )
import ErrUtils		( dumpIfSet_dyn )
import DynFlags		( DynFlags, DynFlag(..) )
import BasicTypes	( Activation(..) )
import Maybes		( orElse, catMaybes, isJust )
import Util		( zipWithEqual, lengthAtLeast, notNull )
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

ALTERNATIVE: pass both boxed and unboxed versions.  This no longer saves
allocation, but does perhaps save evals. In the RULE we'd have
something like

  f (I# x#) = f' (I# x#) x#

If at the call site the (I# x) was an unfolding, then we'd have to
rely on CSE to eliminate the duplicate allocation.... This alternative
doesn't look attractive enough to pursue.


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

	let (binds', _) = initUs us (go emptyScEnv binds)

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
data ScEnv = SCE { scope :: InScopeEnv,
			-- Binds all non-top-level variables in scope

		   cons  :: ConstrEnv
	     }

type InScopeEnv = VarEnv HowBound

type ConstrEnv = IdEnv ConValue
data ConValue  = CV AltCon [CoreArg]
	-- Variables known to be bound to a constructor
	-- in a particular case alternative


instance Outputable ConValue where
   ppr (CV con args) = ppr con <+> interpp'SP args

emptyScEnv = SCE { scope = emptyVarEnv, cons = emptyVarEnv }

data HowBound = RecFun	-- These are the recursive functions for which 
			-- we seek interesting call patterns

	      | RecArg	-- These are those functions' arguments, or their sub-components; 
			-- we gather occurrence information for these

	      | Other	-- We track all others so we know what's in scope
			-- This is used in spec_one to check what needs to be
			-- passed as a parameter and what is in scope at the 
			-- function definition site

instance Outputable HowBound where
  ppr RecFun = text "RecFun"
  ppr RecArg = text "RecArg"
  ppr Other = text "Other"

lookupScopeEnv env v = lookupVarEnv (scope env) v

extendBndrs env bndrs = env { scope = extendVarEnvList (scope env) [(b,Other) | b <- bndrs] }
extendBndr  env bndr  = env { scope = extendVarEnv (scope env) bndr Other }

    -- When we encounter
    --	case scrut of b
    --	    C x y -> ...
    -- we want to bind b, and perhaps scrut too, to (C x y)
extendCaseBndrs :: ScEnv -> Id -> CoreExpr -> AltCon -> [Var] -> ScEnv
extendCaseBndrs env case_bndr scrut con alt_bndrs
  = case con of
	DEFAULT    -> env1
	LitAlt lit -> extendCons env1 scrut case_bndr (CV con [])
	DataAlt dc -> extend_data_con dc
  where
    cur_scope = scope env
    env1 = env { scope = extendVarEnvList cur_scope 
				[(b,how_bound) | b <- case_bndr:alt_bndrs] }

	-- Record RecArg for the components iff the scrutinee is RecArg
	-- I think the only reason for this is to keep the usage envt small
	-- so is it worth it at all?
	--	[This comment looks plain wrong to me, so I'm ignoring it
	-- 	     "Also forget if the scrutinee is a RecArg, because we're
	-- 	     now in the branch of a case, and we don't want to
	-- 	     record a non-scrutinee use of v if we have
	-- 	     	case v of { (a,b) -> ...(f v)... }" ]
    how_bound = get_how scrut
	where
	    get_how (Var v)    = lookupVarEnv cur_scope v `orElse` Other
	    get_how (Cast e _) = get_how e
	    get_how (Note _ e) = get_how e
	    get_how other      = Other

    extend_data_con data_con = 
      extendCons env1 scrut case_bndr (CV con vanilla_args)
	where
	    vanilla_args = map Type (tyConAppArgs (idType case_bndr)) ++
			   varsToCoreExprs alt_bndrs

extendCons :: ScEnv -> CoreExpr -> Id -> ConValue -> ScEnv
extendCons env scrut case_bndr val
  = case scrut of
	Var v -> env { cons = extendVarEnv cons1 v val }
	other -> env { cons = cons1 }
  where
    cons1 = extendVarEnv (cons env) case_bndr val

    -- When we encounter a recursive function binding
    --	f = \x y -> ...
    -- we want to extend the scope env with bindings 
    -- that record that f is a RecFn and x,y are RecArgs
extendRecBndr env fn bndrs
  =  env { scope = scope env `extendVarEnvList` 
		   ((fn,RecFun): [(bndr,RecArg) | bndr <- bndrs]) }
\end{code}


%************************************************************************
%*									*
\subsection{Usage information: flows upwards}
%*									*
%************************************************************************

\begin{code}
data ScUsage
   = SCU {
	calls :: !(IdEnv ([Call])),	-- Calls
					-- The functions are a subset of the 
					-- 	RecFuns in the ScEnv

	occs :: !(IdEnv ArgOcc)		-- Information on argument occurrences
     }					-- The variables are a subset of the 
					--	RecArg in the ScEnv

type Call = (ConstrEnv, [CoreArg])
	-- The arguments of the call, together with the
	-- env giving the constructor bindings at the call site

nullUsage = SCU { calls = emptyVarEnv, occs = emptyVarEnv }

combineUsage u1 u2 = SCU { calls = plusVarEnv_C (++) (calls u1) (calls u2),
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

combineOcc NoOcc	 occ 	       = occ
combineOcc occ 		 NoOcc	       = occ
combineOcc (ScrutOcc xs) (ScrutOcc ys) = ScrutOcc (plusUFM_C combineOccs xs ys)
combineOcc UnkOcc        UnkOcc        = UnkOcc
combineOcc _	    _	     	       = BothOcc

combineOccs :: [ArgOcc] -> [ArgOcc] -> [ArgOcc]
combineOccs xs ys = zipWithEqual "combineOccs" combineOcc xs ys

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

scExpr env e@(Type t) = returnUs (nullUsage, e)
scExpr env e@(Lit l)  = returnUs (nullUsage, e)
scExpr env e@(Var v)  = returnUs (varUsage env v UnkOcc, e)
scExpr env (Note n e) = scExpr env e	`thenUs` \ (usg,e') ->
			returnUs (usg, Note n e')
scExpr env (Cast e co)= scExpr env e	`thenUs` \ (usg,e') ->
                        returnUs (usg, Cast e' co)
scExpr env (Lam b e)  = scExpr (extendBndr env b) e	`thenUs` \ (usg,e') ->
			returnUs (usg, Lam b e')

scExpr env (Case scrut b ty alts) 
  = do	{ (alt_usgs, alt_occs, alts') <- mapAndUnzip3Us sc_alt alts
	; let (alt_usg, b_occ) = lookupOcc (combineUsages alt_usgs) b
	      scrut_occ = foldr combineOcc b_occ alt_occs
		-- The combined usage of the scrutinee is given
		-- by scrut_occ, which is passed to scScrut, which
		-- in turn treats a bare-variable scrutinee specially
	; (scrut_usg, scrut') <- scScrut env scrut scrut_occ
	; return (alt_usg `combineUsage` scrut_usg,
		  Case scrut' b ty alts') }
  where
    sc_alt (con,bs,rhs)
      = do { let env1 = extendCaseBndrs env b scrut con bs
	   ; (usg,rhs') <- scExpr env1 rhs
	   ; let (usg', arg_occs) = lookupOccs usg bs
		 scrut_occ = case con of
				DataAlt dc -> ScrutOcc (unitUFM dc arg_occs)
				other	   -> ScrutOcc emptyUFM
	   ; return (usg', scrut_occ, (con,bs,rhs')) }

scExpr env (Let bind body)
  = scBind env bind	`thenUs` \ (env', bind_usg, bind') ->
    scExpr env' body	`thenUs` \ (body_usg, body') ->
    returnUs (bind_usg `combineUsage` body_usg, Let bind' body')

scExpr env e@(App _ _) 
  = do	{ let (fn, args) = collectArgs e
	; (fn_usg, fn') <- scScrut env fn (ScrutOcc emptyUFM)
	-- Process the function too.   It's almost always a variable,
	-- but not always.  In particular, if this pass follows float-in,
	-- which it may, we can get 
	--	(let f = ...f... in f) arg1 arg2
	-- We use scScrut to record the fact that the function is called
	-- Perhpas we should check that it has at least one value arg, 
	-- but currently we don't bother

	; (arg_usgs, args') <- mapAndUnzipUs (scExpr env) args
	; let call_usg = case fn of
		 	   Var f | Just RecFun <- lookupScopeEnv env f
			         -> SCU { calls = unitVarEnv f [(cons env, args)], 
				          occs  = emptyVarEnv }
			   other -> nullUsage
	; return (combineUsages arg_usgs `combineUsage` fn_usg 
				         `combineUsage` call_usg,
	          mkApps fn' args') }


----------------------
scScrut :: ScEnv -> CoreExpr -> ArgOcc -> UniqSM (ScUsage, CoreExpr)
-- Used for the scrutinee of a case, 
-- or the function of an application.
-- Remember to look through casts
scScrut env e@(Var v)   occ = returnUs (varUsage env v occ, e)
scScrut env (Cast e co) occ = do { (usg, e') <- scScrut env e occ
				 ; returnUs (usg, Cast e' co) }
scScrut env e	        occ = scExpr env e


----------------------
scBind :: ScEnv -> CoreBind -> UniqSM (ScEnv, ScUsage, CoreBind)
scBind env (Rec [(fn,rhs)])
  | notNull val_bndrs
  = scExpr env_fn_body body		`thenUs` \ (usg, body') ->
    specialise env fn bndrs body' usg	`thenUs` \ (rules, spec_prs) ->
	-- Note body': the specialised copies should be based on the 
	-- 	       optimised version of the body, in case there were
	--	       nested functions inside.
    let
	SCU { calls = calls, occs = occs } = usg
    in
    returnUs (extendBndr env fn,	-- For the body of the letrec, just
					-- extend the env with Other to record 
					-- that it's in scope; no funny RecFun business
	      SCU { calls = calls `delVarEnv` fn, occs = occs `delVarEnvList` val_bndrs},
	      Rec ((fn `addIdSpecialisations` rules, mkLams bndrs body') : spec_prs))
  where
    (bndrs,body) = collectBinders rhs
    val_bndrs    = filter isId bndrs
    env_fn_body	 = extendRecBndr env fn bndrs

scBind env (Rec prs)
  = mapAndUnzipUs do_one prs	`thenUs` \ (usgs, prs') ->
    returnUs (extendBndrs env (map fst prs), combineUsages usgs, Rec prs')
  where
    do_one (bndr,rhs) = scExpr env rhs	`thenUs` \ (usg, rhs') ->
		        returnUs (usg, (bndr,rhs'))

scBind env (NonRec bndr rhs)
  = scExpr env rhs	`thenUs` \ (usg, rhs') ->
    returnUs (extendBndr env bndr, usg, NonRec bndr rhs')

----------------------
varUsage env v use 
  | Just RecArg <- lookupScopeEnv env v = SCU { calls = emptyVarEnv, 
						occs = unitVarEnv v use }
  | otherwise		   	        = nullUsage
\end{code}


%************************************************************************
%*									*
\subsection{The specialiser}
%*									*
%************************************************************************

\begin{code}
specialise :: ScEnv
	   -> Id 			-- Functionn
	   -> [CoreBndr] -> CoreExpr	-- Its RHS
	   -> ScUsage			-- Info on usage
	   -> UniqSM ([CoreRule], 	-- Rules
		      [(Id,CoreExpr)])	-- Bindings

specialise env fn bndrs body body_usg
  = do	{ let (_, bndr_occs) = lookupOccs body_usg bndrs

	; mb_calls <- -- pprTrace "specialise" (ppr fn <+> ppr bndrs <+> ppr bndr_occs) $
		      mapM (callToPats (scope env) bndr_occs)
		   	   (lookupVarEnv (calls body_usg) fn `orElse` [])

	; let good_calls :: [([Var], [CoreArg])]
	      good_calls = catMaybes mb_calls
	      in_scope = mkInScopeSet $ unionVarSets $
			 [ exprsFreeVars pats `delVarSetList` vs 
			 | (vs,pats) <- good_calls ]
	      uniq_calls = nubBy (same_call in_scope) good_calls
	; mapAndUnzipUs (spec_one env fn (mkLams bndrs body)) 
			(uniq_calls `zip` [1..]) }
  where
	-- Two calls are the same if they match both ways
    same_call in_scope (vs1,as1)(vs2,as2)
	 =  isJust (matchN in_scope vs1 as1 as2)
 	 && isJust (matchN in_scope vs2 as2 as1)

callToPats :: InScopeEnv -> [ArgOcc] -> Call
	   -> UniqSM (Maybe ([Var], [CoreExpr]))
	-- The VarSet is the variables to quantify over in the rule
	-- The [CoreExpr] are the argument patterns for the rule
callToPats in_scope bndr_occs (con_env, args)
  | length args < length bndr_occs	-- Check saturated
  = return Nothing
  | otherwise
  = do	{ prs <- argsToPats in_scope con_env (args `zip` bndr_occs)
	; let (good_pats, pats) = unzip prs
	      pat_fvs = varSetElems (exprsFreeVars pats)
	      qvars   = filter (not . (`elemVarEnv` in_scope)) pat_fvs
		-- Quantify over variables that are not in sccpe
		-- See Note [Shadowing] at the top
		
	; -- pprTrace "callToPats"  (ppr args $$ ppr prs $$ ppr bndr_occs) $
	  if or good_pats 
	  then return (Just (qvars, pats))
	  else return Nothing }

---------------------
spec_one :: ScEnv
	 -> Id					-- Function
	 -> CoreExpr				-- Rhs of the original function
	 -> (([Var], [CoreArg]), Int)
	 -> UniqSM (CoreRule, (Id,CoreExpr))	-- Rule and binding

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
		  (...entire RHS of f...) (b,c) ((:) (a,(b,c)) (x,v) hw)
  
     RULE:  forall b::* c::*,		-- Note, *not* forall a, x
		   v::(b,c),
		   hw::[(a,(b,c))] .
  
	    f (b,c) ((:) (a,(b,c)) (x,v) hw) = f_spec b c v hw
-}

spec_one env fn rhs ((vars_to_bind, pats), rule_number)
  = getUniqueUs 		`thenUs` \ spec_uniq ->
    let 
	fn_name      = idName fn
	fn_loc       = nameSrcLoc fn_name
	spec_occ     = mkSpecOcc (nameOccName fn_name)

		-- Put the type variables first; the type of a term
		-- variable may mention a type variable
	(tvs, ids)   = partition isTyVar vars_to_bind
	bndrs  	     = tvs ++ ids
	spec_body    = mkApps rhs pats
	body_ty	     = exprType spec_body
	
	(spec_lam_args, spec_call_args) = mkWorkerArgs bndrs body_ty
		-- Usual w/w hack to avoid generating 
		-- a spec_rhs of unlifted type and no args
	
	rule_name = mkFastString ("SC:" ++ showSDoc (ppr fn <> int rule_number))
	spec_rhs  = mkLams spec_lam_args spec_body
	spec_id   = mkUserLocal spec_occ spec_uniq (mkPiTypes spec_lam_args body_ty) fn_loc
	rule_rhs  = mkVarApps (Var spec_id) spec_call_args
	rule      = mkLocalRule rule_name specConstrActivation fn_name bndrs pats rule_rhs
    in
    returnUs (rule, (spec_id, spec_rhs))

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
    -- argToPat takes an actual argument, and returns an abstracted
    -- version, consisting of just the "constructor skeleton" of the
    -- argument, with non-constructor sub-expression replaced by new
    -- placeholder variables.  For example:
    --    C a (D (f x) (g y))  ==>  C p1 (D p2 p3)

argToPat :: InScopeEnv			-- What's in scope at the fn defn site
	 -> ConstrEnv			-- ConstrEnv at the call site
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

argToPat in_scope con_env arg@(Type ty) arg_occ
  = return (False, arg)

argToPat in_scope con_env (Var v) arg_occ
  | not (isLocalId v) || v `elemVarEnv` in_scope
  = 	-- The recursive call passes a variable that 
	-- is in scope at the function definition site
	-- It's worth specialising on this if
	--	(a) it's used in an interesting way in the body
	--	(b) we know what its value is
    if    (case arg_occ of { UnkOcc -> False; other -> True })	-- (a)
       && isValueUnfolding (idUnfolding v)			-- (b)
    then return (True, Var v)
    else wildCardPat (idType v)

argToPat in_scope con_env (Let _ arg) arg_occ
  = argToPat in_scope con_env arg arg_occ
	-- Look through let expressions
	-- e.g.		f (let v = rhs in \y -> ...v...)
	-- Here we can specialise for f (\y -> ...)
	-- because the rule-matcher will look through the let.

argToPat in_scope con_env (Cast arg co) arg_occ
  = do	{ (interesting, arg') <- argToPat in_scope con_env arg arg_occ
	; if interesting then 
		return (interesting, Cast arg' co)
	  else 
		wildCardPat (snd (coercionKind co)) }

argToPat in_scope con_env arg arg_occ
  | is_value_lam arg
  = return (True, arg)
  where
    is_value_lam (Lam v e) 	-- Spot a value lambda, even if 
	| isId v = True		-- it is inside a type lambda
	| otherwise = is_value_lam e
    is_value_lam other = False

argToPat in_scope con_env arg arg_occ
  | Just (CV dc args) <- is_con_app_maybe con_env arg
  , case arg_occ of
	ScrutOcc _ -> True		-- Used only by case scrutinee
	BothOcc    -> case arg of	-- Used by case scrut
			App {} -> True	-- ...and elsewhere...
			other  -> False
	other	   -> False	-- No point; the arg is not decomposed
  = do	{ args' <- argsToPats in_scope con_env (args `zip` conArgOccs arg_occ dc)
	; return (True, mk_con_app dc (map snd args')) }

argToPat in_scope con_env (Var v) arg_occ
  = 	-- A variable bound inside the function. 
	-- Don't make a wild-card, because we may usefully share
	--	e.g.  f a = let x = ... in f (x,x)
	-- NB: this case follows the lambda and con-app cases!!
    return (False, Var v)

-- The default case: make a wild-card
argToPat in_scope con_env arg arg_occ = wildCardPat (exprType arg)

wildCardPat :: Type -> UniqSM (Bool, CoreArg)
wildCardPat ty = do { uniq <- getUniqueUs
		    ; let id = mkSysLocal FSLIT("sc") uniq ty
		    ; return (False, Var id) }

argsToPats :: InScopeEnv -> ConstrEnv
	   -> [(CoreArg, ArgOcc)]
	   -> UniqSM [(Bool, CoreArg)]
argsToPats in_scope con_env args
  = mapUs do_one args
  where
    do_one (arg,occ) = argToPat in_scope con_env arg occ
\end{code}


\begin{code}
is_con_app_maybe :: ConstrEnv -> CoreExpr -> Maybe ConValue
is_con_app_maybe env (Var v)
  = case lookupVarEnv env v of
	Just stuff -> Just stuff
		-- You might think we could look in the idUnfolding here
		-- but that doesn't take account of which branch of a 
		-- case we are in, which is the whole point

	Nothing | isCheapUnfolding unf
		-> is_con_app_maybe env (unfoldingTemplate unf)
		where
		  unf = idUnfolding v
		-- However we do want to consult the unfolding 
		-- as well, for let-bound constructors!

	other  -> Nothing

is_con_app_maybe env (Lit lit)
  = Just (CV (LitAlt lit) [])

is_con_app_maybe env expr
  = case collectArgs expr of
	(Var fun, args) | Just con <- isDataConWorkId_maybe fun,
			  args `lengthAtLeast` dataConRepArity con
		-- Might be > because the arity excludes type args
		        -> Just (CV (DataAlt con) args)

	other -> Nothing

mk_con_app :: AltCon -> [CoreArg] -> CoreExpr
mk_con_app (LitAlt lit)  []   = Lit lit
mk_con_app (DataAlt con) args = mkConApp con args
mk_con_app other args = panic "SpecConstr.mk_con_app"
\end{code}
