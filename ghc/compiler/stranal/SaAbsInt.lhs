%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[SaAbsInt]{Abstract interpreter for strictness analysis}

\begin{code}
module SaAbsInt (
	findStrictness,
	findDemand, findDemandAlts,
	absEval,
	widen,
	fixpoint,
	isBot
    ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_AllStrict, opt_NumbersStrict )
import CoreSyn
import CoreUnfold	( Unfolding(..) )
import PrimOp		( primOpStrictness )
import Id		( Id, idType, getIdStrictness, getIdUnfolding )
import Const		( Con(..) )
import DataCon		( dataConTyCon, dataConArgTys )
import IdInfo		( StrictnessInfo(..) )
import Demand		( Demand(..), wwPrim, wwStrict, wwEnum, wwUnpackData, 
			  wwUnpackNew )
import SaLib
import TyCon		( isProductTyCon, isEnumerationTyCon, isNewTyCon )
import BasicTypes	( NewOrData(..) )
import Type		( splitAlgTyConApp_maybe, 
		          isUnLiftedType, Type )
import TyCon		( tyConUnique )
import PrelInfo		( numericTyKeys )
import Util		( isIn, nOfThem, zipWithEqual )
import Outputable	

returnsRealWorld x = False -- ToDo: panic "SaAbsInt.returnsRealWorld (ToDo)"
\end{code}

%************************************************************************
%*									*
\subsection[AbsVal-ops]{Operations on @AbsVals@}
%*									*
%************************************************************************

Least upper bound, greatest lower bound.

\begin{code}
lub, glb :: AbsVal -> AbsVal -> AbsVal

lub val1 val2 | isBot val1    = val2	-- The isBot test includes the case where
lub val1 val2 | isBot val2    = val1	-- one of the val's is a function which
					-- always returns bottom, such as \y.x,
					-- when x is bound to bottom.

lub (AbsProd xs) (AbsProd ys) = AbsProd (zipWithEqual "lub" lub xs ys)

lub _		  _	      = AbsTop	-- Crude, but conservative
					-- The crudity only shows up if there
					-- are functions involved

-- Slightly funny glb; for absence analysis only;
-- AbsBot is the safe answer.
--
-- Using anyBot rather than just testing for AbsBot is important.
-- Consider:
--
--   f = \a b -> ...
--
--   g = \x y z -> case x of
--	  	     []     -> f x
--		     (p:ps) -> f p
--
-- Now, the abstract value of the branches of the case will be an
-- AbsFun, but when testing for z's absence we want to spot that it's
-- an AbsFun which can't possibly return AbsBot.  So when glb'ing we
-- mustn't be too keen to bale out and return AbsBot; the anyBot test
-- spots that (f x) can't possibly return AbsBot.

-- We have also tripped over the following interesting case:
--	case x of
--	  []     -> \y -> 1
--        (p:ps) -> f
--
-- Now, suppose f is bound to AbsTop.  Does this expression mention z?
-- Obviously not.  But the case will take the glb of AbsTop (for f) and
-- an AbsFun (for \y->1). We should not bale out and give AbsBot, because
-- that would say that it *does* mention z (or anything else for that matter).
-- Nor can we always return AbsTop, because the AbsFun might be something
-- like (\y->z), which obviously does mention z. The point is that we're
-- glbing two functions, and AbsTop is not actually the top of the function
-- lattice.  It is more like (\xyz -> x|y|z); that is, AbsTop returns
-- poison iff any of its arguments do.

-- Deal with functions specially, because AbsTop isn't the
-- top of their domain.

glb v1 v2
  | is_fun v1 || is_fun v2
  = if not (anyBot v1) && not (anyBot v2)
    then
	AbsTop
    else
	AbsBot
  where
    is_fun (AbsFun _ _ _)     = True
    is_fun (AbsApproxFun _ _) = True	-- Not used, but the glb works ok
    is_fun other              = False

-- The non-functional cases are quite straightforward

glb (AbsProd xs) (AbsProd ys) = AbsProd (zipWithEqual "glb" glb xs ys)

glb AbsTop	 v2	      = v2
glb v1           AbsTop	      = v1

glb _            _            = AbsBot 		-- Be pessimistic
\end{code}

@isBot@ returns True if its argument is (a representation of) bottom.  The
``representation'' part is because we need to detect the bottom {\em function}
too.  To detect the bottom function, bind its args to top, and see if it
returns bottom.

Used only in strictness analysis:
\begin{code}
isBot :: AbsVal -> Bool

isBot AbsBot = True
isBot other  = False	-- Functions aren't bottom any more

\end{code}

Used only in absence analysis:
\begin{code}
anyBot :: AbsVal -> Bool

anyBot AbsBot 		      = True	-- poisoned!
anyBot AbsTop 		      = False
anyBot (AbsProd vals) 	      = any anyBot vals
anyBot (AbsFun bndr body env) = anyBot (absEval AbsAnal body (addOneToAbsValEnv env bndr AbsTop))
anyBot (AbsApproxFun _ val)   = anyBot val
\end{code}

@widen@ takes an @AbsVal@, $val$, and returns and @AbsVal@ which is
approximated by $val$.  Furthermore, the result has no @AbsFun@s in
it, so it can be compared for equality by @sameVal@.

\begin{code}
widen :: AnalysisKind -> AbsVal -> AbsVal

-- Widening is complicated by the fact that funtions are lifted
widen StrAnal the_fn@(AbsFun bndr body env)
  = case widened_body of
	AbsApproxFun ds val -> AbsApproxFun (d : ds) val
			    where
			       d = findRecDemand str_fn abs_fn bndr_ty
			       str_fn val = foldl (absApply StrAnal) the_fn 
						  (val : [AbsTop | d <- ds])

	other		    -> AbsApproxFun [d] widened_body
			    where
			       d = findRecDemand str_fn abs_fn bndr_ty
			       str_fn val = absApply StrAnal the_fn val
  where
    bndr_ty      = idType bndr
    widened_body = widen StrAnal (absApply StrAnal the_fn AbsTop)
    abs_fn val   = AbsBot	-- Always says poison; so it looks as if
				-- nothing is absent; safe

{-	OLD comment... 
	This stuff is now instead handled neatly by the fact that AbsApproxFun 
	contains an AbsVal inside it.	SLPJ Jan 97

  | isBot abs_body = AbsBot
    -- It's worth checking for a function which is unconditionally
    -- bottom.  Consider
    --
    --	f x y = let g y = case x of ...
    --		in (g ..) + (g ..)
    --
    -- Here, when we are considering strictness of f in x, we'll
    -- evaluate the body of f with x bound to bottom.  The current
    -- strategy is to bind g to its *widened* value; without the isBot
    -- (...) test above, we'd bind g to an AbsApproxFun, and deliver
    -- Top, not Bot as the value of f's rhs.  The test spots the
    -- unconditional bottom-ness of g when x is bottom.  (Another
    -- alternative here would be to bind g to its exact abstract
    -- value, but that entails lots of potential re-computation, at
    -- every application of g.)
-}

widen StrAnal (AbsProd vals) = AbsProd (map (widen StrAnal) vals)
widen StrAnal other_val	     = other_val


widen AbsAnal the_fn@(AbsFun bndr body env)
  | anyBot widened_body = AbsBot
	-- In the absence-analysis case it's *essential* to check
	-- that the function has no poison in its body.  If it does,
	-- anywhere, then the whole function is poisonous.

  | otherwise
  = case widened_body of
	AbsApproxFun ds val -> AbsApproxFun (d : ds) val
			    where
			       d = findRecDemand str_fn abs_fn bndr_ty
			       abs_fn val = foldl (absApply AbsAnal) the_fn 
						  (val : [AbsTop | d <- ds])

	other		    -> AbsApproxFun [d] widened_body
			    where
			       d = findRecDemand str_fn abs_fn bndr_ty
			       abs_fn val = absApply AbsAnal the_fn val
  where
    bndr_ty      = idType bndr
    widened_body = widen AbsAnal (absApply AbsAnal the_fn AbsTop)
    str_fn val   = AbsBot	-- Always says non-termination;
				-- that'll make findRecDemand peer into the
				-- structure of the value.

widen AbsAnal (AbsProd vals) = AbsProd (map (widen AbsAnal) vals)

	-- It's desirable to do a good job of widening for product
	-- values.  Consider
	--
	--	let p = (x,y)
	--	in ...(case p of (x,y) -> x)...
	--
	-- Now, is y absent in this expression?  Currently the
	-- analyser widens p before looking at p's scope, to avoid
	-- lots of recomputation in the case where p is a function.
	-- So if widening doesn't have a case for products, we'll
	-- widen p to AbsBot (since when searching for absence in y we
	-- bind y to poison ie AbsBot), and now we are lost.

widen AbsAnal other_val = other_val

-- WAS:	  if anyBot val then AbsBot else AbsTop
-- Nowadays widen is doing a better job on functions for absence analysis.
\end{code}

@crudeAbsWiden@ is used just for absence analysis, and always
returns AbsTop or AbsBot, so it widens to a two-point domain

\begin{code}
crudeAbsWiden :: AbsVal -> AbsVal
crudeAbsWiden val = if anyBot val then AbsBot else AbsTop
\end{code}

@sameVal@ compares two abstract values for equality.  It can't deal with
@AbsFun@, but that should have been removed earlier in the day by @widen@.

\begin{code}
sameVal :: AbsVal -> AbsVal -> Bool	-- Can't handle AbsFun!

#ifdef DEBUG
sameVal (AbsFun _ _ _) _ = panic "sameVal: AbsFun: arg1"
sameVal _ (AbsFun _ _ _) = panic "sameVal: AbsFun: arg2"
#endif

sameVal AbsBot AbsBot = True
sameVal AbsBot other  = False	-- widen has reduced AbsFun bots to AbsBot

sameVal AbsTop AbsTop = True
sameVal AbsTop other  = False		-- Right?

sameVal (AbsProd vals1) (AbsProd vals2) = and (zipWithEqual "sameVal" sameVal vals1 vals2)
sameVal (AbsProd _)	AbsTop 		= False
sameVal (AbsProd _)	AbsBot 		= False

sameVal (AbsApproxFun str1 v1) (AbsApproxFun str2 v2) = str1 == str2 && sameVal v1 v2
sameVal (AbsApproxFun _ _)     AbsTop		      = False
sameVal (AbsApproxFun _ _)     AbsBot 		      = False

sameVal val1 val2 = panic "sameVal: type mismatch or AbsFun encountered"
\end{code}


@evalStrictness@ compares a @Demand@ with an abstract value, returning
@True@ iff the abstract value is {\em less defined} than the demand.
(@True@ is the exciting answer; @False@ is always safe.)

\begin{code}
evalStrictness :: Demand
	       -> AbsVal
	       -> Bool		-- True iff the value is sure
				-- to be less defined than the Demand

evalStrictness (WwLazy _) _   = False
evalStrictness WwStrict   val = isBot val
evalStrictness WwEnum	  val = isBot val

evalStrictness (WwUnpack NewType _ (demand:_)) val
  = evalStrictness demand val

evalStrictness (WwUnpack DataType _ demand_info) val
  = case val of
      AbsTop	   -> False
      AbsBot	   -> True
      AbsProd vals -> or (zipWithEqual "evalStrictness" evalStrictness demand_info vals)
      _	    	   -> pprTrace "evalStrictness?" empty False

evalStrictness WwPrim val
  = case val of
      AbsTop -> False
      AbsBot -> True	-- Can happen: consider f (g x), where g is a 
			-- recursive function returning an Int# that diverges

      other  -> pprPanic "evalStrictness: WwPrim:" (ppr other)
\end{code}

For absence analysis, we're interested in whether "poison" in the
argument (ie a bottom therein) can propagate to the result of the
function call; that is, whether the specified demand can {\em
possibly} hit poison.

\begin{code}
evalAbsence (WwLazy True) _ = False	-- Can't possibly hit poison
					-- with Absent demand

evalAbsence (WwUnpack NewType _ (demand:_)) val
  = evalAbsence demand val

evalAbsence (WwUnpack DataType _ demand_info) val
  = case val of
	AbsTop	     -> False		-- No poison in here
	AbsBot 	     -> True		-- Pure poison
	AbsProd vals -> or (zipWithEqual "evalAbsence" evalAbsence demand_info vals)
	_	     -> panic "evalAbsence: other"

evalAbsence other val = anyBot val
  -- The demand is conservative; even "Lazy" *might* evaluate the
  -- argument arbitrarily so we have to look everywhere for poison
\end{code}

%************************************************************************
%*									*
\subsection[absEval]{Evaluate an expression in the abstract domain}
%*									*
%************************************************************************

\begin{code}
-- The isBottomingId stuf is now dealt with via the Id's strictness info
-- absId anal var env | isBottomingId var
--   = case anal of
--	StrAnal -> AbsBot 	-- See discussion below
--	AbsAnal -> AbsTop	-- Just want to see if there's any poison in
				-- error's arg

absId anal var env
  = case (lookupAbsValEnv env var, getIdStrictness var, getIdUnfolding var) of

	(Just abs_val, _, _) ->
			abs_val	-- Bound in the environment

	(Nothing, NoStrictnessInfo, CoreUnfolding _ _ unfolding) ->
			-- We have an unfolding for the expr
			-- Assume the unfolding has no free variables since it
			-- came from inside the Id
			absEval anal unfolding env
		-- Notice here that we only look in the unfolding if we don't
		-- have strictness info (an unusual situation).
		-- We could have chosen to look in the unfolding if it exists,
		-- and only try the strictness info if it doesn't, and that would
		-- give more accurate results, at the cost of re-abstract-interpreting
		-- the unfolding every time.
		-- We found only one place where the look-at-unfolding-first
		-- method gave better results, which is in the definition of
		-- showInt in the Prelude.  In its defintion, fromIntegral is
		-- not inlined (it's big) but ab-interp-ing its unfolding gave
		-- a better result than looking at its strictness only.
		--  showInt :: Integral a => a -> [Char] -> [Char]
		-- !       {-# GHC_PRAGMA _A_ 1 _U_ 122 _S_
		--         "U(U(U(U(SA)AAAAAAAAL)AA)AAAAASAAASA)" {...} _N_ _N_ #-}
		-- --- 42,44 ----
		--   showInt :: Integral a => a -> [Char] -> [Char]
		-- !       {-# GHC_PRAGMA _A_ 1 _U_ 122 _S_
		--        "U(U(U(U(SL)LLLLLLLLL)LL)LLLLLSLLLLL)" _N_ _N_ #-}


	(Nothing, strictness_info, _) ->
			-- Includes MagicUnfolding, NoUnfolding
			-- Try the strictness info
			absValFromStrictness anal strictness_info
\end{code}

\begin{code}
absEval :: AnalysisKind -> CoreExpr -> AbsValEnv -> AbsVal

absEval anal (Type ty) env = AbsTop
absEval anal (Var var) env = absId anal var env
\end{code}

Discussion about error (following/quoting Lennart): Any expression
'error e' is regarded as bottom (with HBC, with the -ffail-strict
flag, on with -O).

Regarding it as bottom gives much better strictness properties for
some functions.	 E.g.

	f [x] y = x+y
	f (x:xs) y = f xs (x+y)
i.e.
	f [] _ = error "no match"
	f [x] y = x+y
	f (x:xs) y = f xs (x+y)

is strict in y, which you really want.  But, it may lead to
transformations that turn a call to \tr{error} into non-termination.
(The odds of this happening aren't good.)

Things are a little different for absence analysis, because we want
to make sure that any poison (?????)

\begin{code}
absEval anal (Con (Literal _) args) env
  = 	-- Literals terminate (strictness) and are not poison (absence)
    AbsTop

absEval anal (Con (PrimOp op) args) env
  = 	-- Not all PrimOps evaluate all their arguments
    if or (zipWith (check_arg anal) 
		   [absEval anal arg env | arg <- args, isValArg arg]
		   arg_demands)
    then AbsBot
    else case anal of
	    StrAnal | result_bot -> AbsBot
	    other		 -> AbsTop
  where
    (arg_demands, result_bot) = primOpStrictness op
    check_arg StrAnal arg dmd = evalStrictness dmd arg
    check_arg AbsAnal arg dmd = evalAbsence    dmd arg

absEval anal (Con (DataCon con) args) env
  | isProductTyCon (dataConTyCon con)
  = 	-- Products; filter out type arguments
    AbsProd [absEval anal a env | a <- args, isValArg a]

  | otherwise	-- Not single-constructor
  = case anal of
	StrAnal -> 	-- Strictness case: it's easy: it certainly terminates
		   AbsTop
	AbsAnal -> 	-- In the absence case we need to be more
			-- careful: look to see if there's any
			-- poison in the components
		   if any anyBot [absEval AbsAnal arg env | arg <- args]
		   then AbsBot
		   else AbsTop
\end{code}

\begin{code}
absEval anal (Lam bndr body) env
  | isTyVar bndr = absEval anal body env	-- Type lambda
  | otherwise    = AbsFun bndr body env		-- Value lambda

absEval anal (App expr (Type ty)) env
  = absEval anal expr env			-- Type appplication
absEval anal (App f val_arg) env
  = absApply anal (absEval anal f env) 		-- Value applicationn
		  (absEval anal val_arg env)
\end{code}

\begin{code}
absEval anal expr@(Case scrut case_bndr alts) env
  = let
	scrut_val  = absEval anal scrut env
	alts_env   = addOneToAbsValEnv env case_bndr scrut_val
    in
    case (scrut_val, alts) of
	(AbsBot, _) -> AbsBot

	(AbsProd arg_vals, [(con, bndrs, rhs)])
		| con /= DEFAULT ->
		-- The scrutinee is a product value, so it must be of a single-constr
		-- type; so the constructor in this alternative must be the right one
		-- so we can go ahead and bind the constructor args to the components
		-- of the product value.
	    ASSERT(length arg_vals == length val_bndrs)
	    absEval anal rhs rhs_env
	  where
	    val_bndrs = filter isId bndrs
	    rhs_env   = growAbsValEnvList alts_env (val_bndrs `zip` arg_vals)

	other -> absEvalAlts anal alts alts_env
\end{code}

For @Lets@ we widen the value we get.  This is nothing to
do with fixpointing.  The reason is so that we don't get an explosion
in the amount of computation.  For example, consider:
\begin{verbatim}
      let
	g a = case a of
		q1 -> ...
		q2 -> ...
	f x = case x of
		p1 -> ...g r...
		p2 -> ...g s...
      in
	f e
\end{verbatim}
If we bind @f@ and @g@ to their exact abstract value, then we'll
``execute'' one call to @f@ and {\em two} calls to @g@.  This can blow
up exponentially.  Widening cuts it off by making a fixed
approximation to @f@ and @g@, so that the bodies of @f@ and @g@ are
not evaluated again at all when they are called.

Of course, this can lose useful joint strictness, which is sad.  An
alternative approach would be to try with a certain amount of ``fuel''
and be prepared to bale out.

\begin{code}
absEval anal (Let (NonRec binder e1) e2) env
  = let
	new_env = addOneToAbsValEnv env binder (widen anal (absEval anal e1 env))
    in
	-- The binder of a NonRec should *not* be of unboxed type,
	-- hence no need to strictly evaluate the Rhs.
    absEval anal e2 new_env

absEval anal (Let (Rec pairs) body) env
  = let
	(binders,rhss) = unzip pairs
	rhs_vals = cheapFixpoint anal binders rhss env	-- Returns widened values
	new_env  = growAbsValEnvList env (binders `zip` rhs_vals)
    in
    absEval anal body new_env

absEval anal (Note note expr) env = absEval anal expr env
\end{code}

\begin{code}
absEvalAlts :: AnalysisKind -> [CoreAlt] -> AbsValEnv -> AbsVal
absEvalAlts anal alts env
  = combine anal (map go alts)
  where
    combine StrAnal = foldr1 lub	-- Diverge only if all diverge
    combine AbsAnal = foldr1 glb	-- Find any poison

    go (con, bndrs, rhs)
      = absEval anal rhs rhs_env
      where
	rhs_env = growAbsValEnvList env (filter isId bndrs `zip` repeat AbsTop)
\end{code}

%************************************************************************
%*									*
\subsection[absApply]{Apply an abstract function to an abstract argument}
%*									*
%************************************************************************

Easy ones first:

\begin{code}
absApply :: AnalysisKind -> AbsVal -> AbsVal -> AbsVal

absApply anal AbsBot arg = AbsBot
  -- AbsBot represents the abstract bottom *function* too

absApply StrAnal AbsTop	arg = AbsTop
absApply AbsAnal AbsTop	arg = if anyBot arg
			      then AbsBot
			      else AbsTop
	-- To be conservative, we have to assume that a function about
	-- which we know nothing (AbsTop) might look at some part of
	-- its argument
\end{code}

An @AbsFun@ with only one more argument needed---bind it and eval the
result.	 A @Lam@ with two or more args: return another @AbsFun@ with
an augmented environment.

\begin{code}
absApply anal (AbsFun binder body env) arg
  = absEval anal body (addOneToAbsValEnv env binder arg)
\end{code}

\begin{code}
absApply StrAnal (AbsApproxFun (d:ds) val) arg
  = case ds of 
	[]    -> val'
	other -> AbsApproxFun ds val'	-- Result is non-bot if there are still args
  where
    val' | evalStrictness d arg = AbsBot
	 | otherwise		= val

absApply AbsAnal (AbsApproxFun (d:ds) val) arg
  = if evalAbsence d arg
    then AbsBot		-- Poison in arg means poison in the application
    else case ds of
		[]    -> val
		other -> AbsApproxFun ds val

#ifdef DEBUG
absApply anal f@(AbsProd _)       arg = pprPanic ("absApply: Duff function: AbsProd." ++ show anal) ((ppr f) <+> (ppr arg))
#endif
\end{code}




%************************************************************************
%*									*
\subsection[findStrictness]{Determine some binders' strictness}
%*									*
%************************************************************************

@findStrictness@ applies the function \tr{\ ids -> expr} to
\tr{[bot,top,top,...]}, \tr{[top,bot,top,top,...]}, etc., (i.e., once
with @AbsBot@ in each argument position), and evaluates the resulting
abstract value; it returns a vector of @Demand@s saying whether the
result of doing this is guaranteed to be bottom.  This tells the
strictness of the function in each of the arguments.

If an argument is of unboxed type, then we declare that function to be
strict in that argument.

We don't really have to make up all those lists of mostly-@AbsTops@;
unbound variables in an @AbsValEnv@ are implicitly mapped to that.

See notes on @addStrictnessInfoToId@.

\begin{code}
findStrictness :: [Type]		-- Types of args in which strictness is wanted
	       -> AbsVal 		-- Abstract strictness value of function
	       -> AbsVal		-- Abstract absence value of function
	       -> ([Demand], Bool)	-- Resulting strictness annotation

findStrictness tys str_val abs_val
  = (map find_str tys_w_index, isBot (foldl (absApply StrAnal) str_val all_tops))
  where
    tys_w_index = tys `zip` [1..]

    find_str (ty,n) = findRecDemand str_fn abs_fn ty
		    where
		      str_fn val = foldl (absApply StrAnal) str_val 
					 (map (mk_arg val n) tys_w_index)

		      abs_fn val = foldl (absApply AbsAnal) abs_val 
					 (map (mk_arg val n) tys_w_index)

    mk_arg val n (_,m) | m==n      = val
		       | otherwise = AbsTop

    all_tops = [AbsTop | _ <- tys]
\end{code}


\begin{code}
findDemand str_env abs_env expr binder
  = findRecDemand str_fn abs_fn (idType binder)
  where
    str_fn val = absEval StrAnal expr (addOneToAbsValEnv str_env binder val)
    abs_fn val = absEval AbsAnal expr (addOneToAbsValEnv abs_env binder val)

findDemandAlts str_env abs_env alts binder
  = findRecDemand str_fn abs_fn (idType binder)
  where
    str_fn val = absEvalAlts StrAnal alts (addOneToAbsValEnv str_env binder val)
    abs_fn val = absEvalAlts AbsAnal alts (addOneToAbsValEnv abs_env binder val)
\end{code}

@findRecDemand@ is where we finally convert strictness/absence info
into ``Demands'' which we can pin on Ids (etc.).

NOTE: What do we do if something is {\em both} strict and absent?
Should \tr{f x y z = error "foo"} says that \tr{f}'s arguments are all
strict (because of bottoming effect of \tr{error}) or all absent
(because they're not used)?

Well, for practical reasons, we prefer absence over strictness.  In
particular, it makes the ``default defaults'' for class methods (the
ones that say \tr{defm.foo dict = error "I don't exist"}) come out
nicely [saying ``the dict isn't used''], rather than saying it is
strict in every component of the dictionary [massive gratuitious
casing to take the dict apart].

But you could have examples where going for strictness would be better
than absence.  Consider:
\begin{verbatim}
	let x = something big
	in
	f x y z + g x
\end{verbatim}

If \tr{x} is marked absent in \tr{f}, but not strict, and \tr{g} is
lazy, then the thunk for \tr{x} will be built.  If \tr{f} was strict,
then we'd let-to-case it:
\begin{verbatim}
	case something big of
	  x -> f x y z + g x
\end{verbatim}
Ho hum.

\begin{code}
findRecDemand :: (AbsVal -> AbsVal) -- The strictness function
	      -> (AbsVal -> AbsVal) -- The absence function
	      -> Type 	    -- The type of the argument
	      -> Demand

findRecDemand str_fn abs_fn ty
  = if isUnLiftedType ty then -- It's a primitive type!
       wwPrim

    else if not (anyBot (abs_fn AbsBot)) then -- It's absent
       -- We prefer absence over strictness: see NOTE above.
       WwLazy True

    else if not (opt_AllStrict ||
		(opt_NumbersStrict && is_numeric_type ty) ||
		(isBot (str_fn AbsBot))) then
	WwLazy False -- It's not strict and we're not pretending

    else -- It's strict (or we're pretending it is)!

       case (splitAlgTyConApp_maybe ty) of

	 Nothing    -> wwStrict

	 Just (tycon,tycon_arg_tys,[data_con]) | isProductTyCon tycon ->
	   -- Non-recursive, single constructor case
	   let
	      cmpnt_tys = dataConArgTys data_con tycon_arg_tys
	      prod_len = length cmpnt_tys
	   in

	   if isNewTyCon tycon then	-- A newtype!
		ASSERT( null (tail cmpnt_tys) )
		let
		    demand = findRecDemand str_fn abs_fn (head cmpnt_tys)
		in
		wwUnpackNew demand
	   else				-- A data type!
	   let
	      compt_strict_infos
		= [ findRecDemand
			 (\ cmpnt_val ->
			       str_fn (mkMainlyTopProd prod_len i cmpnt_val)
			 )
			 (\ cmpnt_val ->
			       abs_fn (mkMainlyTopProd prod_len i cmpnt_val)
			 )
		     cmpnt_ty
		  | (cmpnt_ty, i) <- cmpnt_tys `zip` [1..] ]
	   in
	   if null compt_strict_infos then
		 if isEnumerationTyCon tycon then wwEnum else wwStrict
	   else
		 wwUnpackData compt_strict_infos

	 Just (tycon,_,_) ->
		-- Multi-constr data types, *or* an abstract data
		-- types, *or* things we don't have a way of conveying
		-- the info over module boundaries (class ops,
		-- superdict sels, dfns).
	    if isEnumerationTyCon tycon then
		wwEnum
	    else
		wwStrict
  where
    is_numeric_type ty
      = case (splitAlgTyConApp_maybe ty) of -- NB: duplicates stuff done above
	  Nothing -> False
	  Just (tycon, _, _)
	    | tyConUnique tycon `is_elem` numericTyKeys
	    -> True
	  _{-something else-} -> False
      where
	is_elem = isIn "is_numeric_type"

    -- mkMainlyTopProd: make an AbsProd that is all AbsTops ("n"-1 of
    -- them) except for a given value in the "i"th position.

    mkMainlyTopProd :: Int -> Int -> AbsVal -> AbsVal

    mkMainlyTopProd n i val
      = let
	    befores = nOfThem (i-1) AbsTop
	    afters  = nOfThem (n-i) AbsTop
    	in
	AbsProd (befores ++ (val : afters))
\end{code}

%************************************************************************
%*									*
\subsection[fixpoint]{Fixpointer for the strictness analyser}
%*									*
%************************************************************************

The @fixpoint@ functions take a list of \tr{(binder, expr)} pairs, an
environment, and returns the abstract value of each binder.

The @cheapFixpoint@ function makes a conservative approximation,
by binding each of the variables to Top in their own right hand sides.
That allows us to make rapid progress, at the cost of a less-than-wonderful
approximation.

\begin{code}
cheapFixpoint :: AnalysisKind -> [Id] -> [CoreExpr] -> AbsValEnv -> [AbsVal]

cheapFixpoint AbsAnal [id] [rhs] env
  = [crudeAbsWiden (absEval AbsAnal rhs new_env)]
  where
    new_env = addOneToAbsValEnv env id AbsTop	-- Unsafe starting point!
		    -- In the just-one-binding case, we guarantee to
		    -- find a fixed point in just one iteration,
		    -- because we are using only a two-point domain.
		    -- This improves matters in cases like:
		    --
		    --	f x y = letrec g = ...g...
		    --		in g x
		    --
		    -- Here, y isn't used at all, but if g is bound to
		    -- AbsBot we simply get AbsBot as the next
		    -- iteration too.

cheapFixpoint anal ids rhss env
  = [widen anal (absEval anal rhs new_env) | rhs <- rhss]
		-- We do just one iteration, starting from a safe
		-- approximation.  This won't do a good job in situations
		-- like:
		--	\x -> letrec f = ...g...
		--		     g = ...f...x...
		--	      in
		--	      ...f...
		-- Here, f will end up bound to Top after one iteration,
		-- and hence we won't spot the strictness in x.
		-- (A second iteration would solve this.  ToDo: try the effect of
		--  really searching for a fixed point.)
  where
    new_env = growAbsValEnvList env [(id,safe_val) | id <- ids]

    safe_val
      = case anal of	-- The safe starting point
	  StrAnal -> AbsTop
	  AbsAnal -> AbsBot
\end{code}

\begin{verbatim}
mkLookupFun :: (key -> key -> Bool)	-- Equality predicate
	    -> (key -> key -> Bool)	-- Less-than predicate
	    -> [(key,val)] 		-- The assoc list
	    -> key 			-- The key
	    -> Maybe val		-- The corresponding value

mkLookupFun eq lt alist s
  = case [a | (s',a) <- alist, s' `eq` s] of
      []    -> Nothing
      (a:_) -> Just a
\end{verbatim}

\begin{code}
fixpoint :: AnalysisKind -> [Id] -> [CoreExpr] -> AbsValEnv -> [AbsVal]

fixpoint anal [] _ env = []

fixpoint anal ids rhss env
  = fix_loop initial_vals
  where
    initial_val id
      = case anal of	-- The (unsafe) starting point
	  StrAnal -> if (returnsRealWorld (idType id))
		     then AbsTop -- this is a massively horrible hack (SLPJ 95/05)
		     else AbsBot
	  AbsAnal -> AbsTop

    initial_vals = [ initial_val id | id <- ids ]

    fix_loop :: [AbsVal] -> [AbsVal]

    fix_loop current_widened_vals
      = let
	    new_env  = growAbsValEnvList env (ids `zip` current_widened_vals)
	    new_vals = [ absEval anal rhs new_env | rhs <- rhss ]
	    new_widened_vals = map (widen anal) new_vals
	in
	if (and (zipWith sameVal current_widened_vals new_widened_vals)) then
	    current_widened_vals

	    -- NB: I was too chicken to make that a zipWithEqual,
	    -- lest I jump into a black hole.  WDP 96/02

	    -- Return the widened values.  We might get a slightly
	    -- better value by returning new_vals (which we used to
	    -- do, see below), but alas that means that whenever the
	    -- function is called we have to re-execute it, which is
	    -- expensive.

	    -- OLD VERSION
	    -- new_vals
	    -- Return the un-widened values which may be a bit better
	    -- than the widened ones, and are guaranteed safe, since
	    -- they are one iteration beyond current_widened_vals,
	    -- which itself is a fixed point.
	else
	    fix_loop new_widened_vals
\end{code}

For absence analysis, we make do with a very very simple approach:
look for convergence in a two-point domain.

We used to use just one iteration, starting with the variables bound
to @AbsBot@, which is safe.

Prior to that, we used one iteration starting from @AbsTop@ (which
isn't safe).  Why isn't @AbsTop@ safe?  Consider:
\begin{verbatim}
	letrec
	  x = ...p..d...
	  d = (x,y)
	in
	...
\end{verbatim}
Here, if p is @AbsBot@, then we'd better {\em not} end up with a ``fixed
point'' of @d@ being @(AbsTop, AbsTop)@!  An @AbsBot@ initial value is
safe because it gives poison more often than really necessary, and
thus may miss some absence, but will never claim absence when it ain't
so.

Anyway, one iteration starting with everything bound to @AbsBot@ give
bad results for

	f = \ x -> ...f...

Here, f would always end up bound to @AbsBot@, which ain't very
clever, because then it would introduce poison whenever it was
applied.  Much better to start with f bound to @AbsTop@, and widen it
to @AbsBot@ if any poison shows up. In effect we look for convergence
in the two-point @AbsTop@/@AbsBot@ domain.

What we miss (compared with the cleverer strictness analysis) is
spotting that in this case

	f = \ x y -> ...y...(f x y')...

\tr{x} is actually absent, since it is only passed round the loop, never
used.  But who cares about missing that?

NB: despite only having a two-point domain, we may still have many
iterations, because there are several variables involved at once.
