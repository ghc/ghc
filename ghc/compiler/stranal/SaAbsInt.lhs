%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[SaAbsInt]{Abstract interpreter for strictness analysis}

\begin{code}
#include "HsVersions.h"

module SaAbsInt (
	findStrictness,
	findDemand,
	absEval,
	widen,
	fixpoint,
	isBot
    ) where

IMPORT_Trace		-- ToDo: rm
import Pretty
--import FiniteMap
import Outputable

import AbsPrel		( PrimOp(..),
			  intTyCon, integerTyCon, doubleTyCon,
			  floatTyCon, wordTyCon, addrTyCon,
			  PrimKind
			)
import AbsUniType	( isPrimType, getUniDataTyCon_maybe,
			  maybeSingleConstructorTyCon,
			  returnsRealWorld,
			  isEnumerationTyCon, TyVarTemplate, TyCon
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon)
			)
import Id		( getIdStrictness, getIdUniType, getIdUnfolding,
			  getDataConSig, getInstantiatedDataConSig,
			  DataCon(..), isBottomingId
			)

import IdInfo		-- various bits
import IdEnv
import CoreFuns		( unTagBinders )
import Maybes		( maybeToBool, Maybe(..) )
import PlainCore
import SaLib
import SimplEnv		( FormSummary(..) ) -- nice data abstraction, huh? (WDP 95/03)
import Util
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

lub (AbsProd xs) (AbsProd ys) = ASSERT (length xs == length ys)
				AbsProd (zipWith lub xs ys)

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
    is_fun (AbsFun _ _ _)   = True
    is_fun (AbsApproxFun _) = True	-- Not used, but the glb works ok
    is_fun other            = False

-- The non-functional cases are quite straightforward

glb (AbsProd xs) (AbsProd ys) = ASSERT (length xs == length ys)
				AbsProd (zipWith glb xs ys)

glb AbsTop	 v2	      = v2
glb v1           AbsTop	      = v1

glb _            _            = AbsBot 		-- Be pessimistic



combineCaseValues 
	:: AnalysisKind
	-> AbsVal	-- Value of scrutinee
	-> [AbsVal]	-- Value of branches (at least one)
	-> AbsVal	-- Result

-- For strictness analysis, see if the scrutinee is bottom; if so
-- return bottom; otherwise, the lub of the branches.

combineCaseValues StrAnal AbsBot	  branches = AbsBot
combineCaseValues StrAnal other_scrutinee branches
	-- Scrutinee can only be AbsBot, AbsProd or AbsTop
  = ASSERT(ok_scrutinee)
    foldr1 lub branches
  where
    ok_scrutinee
      = case other_scrutinee of {
	  AbsTop    -> True; 	-- i.e., cool
	  AbsProd _ -> True; 	-- ditto
	  _ 	    -> False 	-- party over
	}  

-- For absence analysis, check if the scrutinee is all poison (isBot)
-- If so, return poison (AbsBot); otherwise, any nested poison will come
-- out from looking at the branches, so just glb together the branches
-- to get the worst one.

combineCaseValues AbsAnal AbsBot          branches = AbsBot
combineCaseValues AbsAnal other_scrutinee branches
	-- Scrutinee can only be AbsBot, AbsProd or AbsTop
  = ASSERT(ok_scrutinee)
    let
	result = foldr1 glb branches

	tracer = if at_least_one_AbsFun && at_least_one_AbsTop
		    && no_AbsBots then
		    pprTrace "combineCase:" (ppr PprDebug branches)
		 else
		    id
    in
--    tracer (
    result
--    )
  where
    ok_scrutinee
      = case other_scrutinee of {
	  AbsTop    -> True; 	-- i.e., cool
	  AbsProd _ -> True; 	-- ditto
	  _ 	    -> False 	-- party over
	}

    at_least_one_AbsFun = foldr ((||) . is_AbsFun) False branches
    at_least_one_AbsTop = foldr ((||) . is_AbsTop) False branches
    no_AbsBots = foldr ((&&) . is_not_AbsBot) True branches

    is_AbsFun x = case x of { AbsFun _ _ _ -> True; _ -> False }
    is_AbsTop x = case x of { AbsTop -> True; _ -> False }
    is_not_AbsBot x = case x of { AbsBot -> False; _ -> True }
\end{code}

@isBot@ returns True if its argument is (a representation of) bottom.  The
``representation'' part is because we need to detect the bottom {\em function}
too.  To detect the bottom function, bind its args to top, and see if it
returns bottom.

Used only in strictness analysis:
\begin{code}
isBot :: AbsVal -> Bool

isBot AbsBot		     = True
isBot (AbsFun args body env) = isBot (absEval StrAnal body env)
			       -- Don't bother to extend the envt because 
			       -- unbound variables default to AbsTop anyway 
isBot other	 	     = False
\end{code}

Used only in absence analysis:
\begin{code}
anyBot :: AbsVal -> Bool

anyBot AbsBot 		      = True	-- poisoned!
anyBot AbsTop 		      = False
anyBot (AbsProd vals) 	      = any anyBot vals
anyBot (AbsFun args body env) = anyBot (absEval AbsAnal body env)
anyBot (AbsApproxFun demands) = False

    -- AbsApproxFun can only arise in absence analysis from the Demand
    -- info of an imported value; whatever it is we're looking for is
    -- certainly not present over in the imported value.
\end{code}

@widen@ takes an @AbsVal@, $val$, and returns and @AbsVal@ which is
approximated by $val$.  Furthermore, the result has no @AbsFun@s in
it, so it can be compared for equality by @sameVal@.

\begin{code}
widen :: AnalysisKind -> AbsVal -> AbsVal

widen StrAnal (AbsFun args body env) 
  | isBot (absEval StrAnal body env) = AbsBot
  | otherwise
  = ASSERT (not (null args))
    AbsApproxFun (map (findDemandStrOnly env body) args)

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
	
widen StrAnal (AbsProd vals) = AbsProd (map (widen StrAnal) vals)
widen StrAnal other_val	     = other_val


widen AbsAnal (AbsFun args body env) 
  | anyBot (absEval AbsAnal body env) = AbsBot
	-- In the absence-analysis case it's *essential* to check
	-- that the function has no poison in its body.  If it does,
	-- anywhere, then the whole function is poisonous.

  | otherwise
  = ASSERT (not (null args))
    AbsApproxFun (map (findDemandAbsOnly env body) args)
	
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

-- OLD			  if anyBot val then AbsBot else AbsTop
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

sameVal (AbsProd vals1) (AbsProd vals2) = ASSERT (length vals1 == length vals2)
					  and (zipWith sameVal vals1 vals2)
sameVal (AbsProd _)	AbsTop 		= False
sameVal (AbsProd _)	AbsBot 		= False

sameVal (AbsApproxFun str1) (AbsApproxFun str2) = str1 == str2
sameVal (AbsApproxFun _)    AbsTop		= False
sameVal (AbsApproxFun _)    AbsBot 		= False

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

evalStrictness (WwUnpack demand_info) val
  = case val of
      AbsTop	   -> False
      AbsBot	   -> True
      AbsProd vals -> ASSERT (length vals == length demand_info)
		      or (zipWith evalStrictness demand_info vals)
      _	    	   -> trace "evalStrictness?" False

evalStrictness WwPrim val
  = case val of
      AbsTop -> False	

      other  ->   -- A primitive value should be defined, never bottom; 
		  -- hence this paranoia check
		pprPanic "evalStrictness: WwPrim:" (ppr PprDebug other)
\end{code}

For absence analysis, we're interested in whether "poison" in the
argument (ie a bottom therein) can propagate to the result of the
function call; that is, whether the specified demand can {\em
possibly} hit poison.

\begin{code}
evalAbsence (WwLazy True) _ = False	-- Can't possibly hit poison 
					-- with Absent demand

evalAbsence (WwUnpack demand_info) val
  = case val of
	AbsTop	     -> False		-- No poison in here
	AbsBot 	     -> True		-- Pure poison
	AbsProd vals -> ASSERT (length demand_info == length vals)
			or (zipWith evalAbsence demand_info vals)
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
  = let
     result =
      case (lookupAbsValEnv env var, getIdStrictness var, getIdUnfolding var) of

        (Just abs_val, _, _) -> 
			abs_val	-- Bound in the environment

	(Nothing, NoStrictnessInfo, LiteralForm _) -> 
			AbsTop 	-- Literals all terminate, and have no poison

	(Nothing, NoStrictnessInfo, ConstructorForm _ _ _) -> 
			AbsTop -- An imported constructor won't have
			       -- bottom components, nor poison!

	(Nothing, NoStrictnessInfo, GeneralForm _ _ unfolding _) -> 
			-- We have an unfolding for the expr
			-- Assume the unfolding has no free variables since it
			-- came from inside the Id
			absEval anal (unTagBinders unfolding) env
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
			-- Includes MagicForm, IWantToBeINLINEd, NoUnfoldingDetails
			-- Try the strictness info
			absValFromStrictness anal strictness_info


	-- 	Done via strictness now
	--	  GeneralForm _ BottomForm _ _ -> AbsBot
    in
    -- pprTrace "absId:" (ppBesides [ppr PprDebug var, ppStr "=:", pp_anal anal, ppStr ":=",ppr PprDebug result]) (
    result
    -- )
  where
    pp_anal StrAnal = ppStr "STR"
    pp_anal AbsAnal = ppStr "ABS"

absEvalAtom anal (CoVarAtom v) env = absId anal v env
absEvalAtom anal (CoLitAtom _) env = AbsTop
\end{code}

\begin{code}
absEval :: AnalysisKind -> PlainCoreExpr -> AbsValEnv -> AbsVal

absEval anal (CoVar var) env = absId anal var env

absEval anal (CoLit _) env = AbsTop
    -- What if an unboxed literal?  That's OK: it terminates, so its
    -- abstract value is AbsTop.

    -- For absence analysis, a literal certainly isn't the "poison" variable
\end{code}

Discussion about \tr{error} (following/quoting Lennart): Any expression
\tr{error e} is regarded as bottom (with HBC, with the
\tr{-ffail-strict} flag, on with \tr{-O}).

Regarding it as bottom gives much better strictness properties for
some functions.	 E.g.
\begin{verbatim}
	f [x] y = x+y
	f (x:xs) y = f xs (x+y)
i.e.
	f [] _ = error "no match"
	f [x] y = x+y
	f (x:xs) y = f xs (x+y)
\end{verbatim}
is strict in \tr{y}, which you really want.  But, it may lead to
transformations that turn a call to \tr{error} into non-termination.
(The odds of this happening aren't good.)


Things are a little different for absence analysis, because we want
to make sure that any poison (?????)

\begin{code}
absEval StrAnal (CoPrim SeqOp [t] [e]) env
  = if isBot (absEvalAtom StrAnal e env) then AbsBot else AbsTop
	-- This is a special case to ensure that seq# is strict in its argument.
	-- The comments below (for most normal PrimOps) do not apply.

absEval StrAnal (CoPrim op ts es) env = AbsTop
	-- The arguments are all of unboxed type, so they will already
	-- have been eval'd.  If the boxed version was bottom, we'll
	-- already have returned bottom.

    	-- Actually, I believe we are saying that either (1) the
	-- primOp uses unboxed args and they've been eval'ed, so
	-- there's no need to force strictness here, _or_ the primOp
	-- uses boxed args and we don't know whether or not it's
    	-- strict, so we assume laziness. (JSM)

absEval AbsAnal (CoPrim op ts as) env 
  = if any anyBot [absEvalAtom AbsAnal a env | a <- as]
    then AbsBot
    else AbsTop
	-- For absence analysis, we want to see if the poison shows up...

absEval anal (CoCon con ts as) env
  | has_single_con
  = AbsProd [absEvalAtom anal a env | a <- as]

  | otherwise	-- Not single-constructor
  = case anal of
	StrAnal -> 	-- Strictness case: it's easy: it certainly terminates
		   AbsTop	
	AbsAnal -> 	-- In the absence case we need to be more 
			-- careful: look to see if there's any
			-- poison in the components
		   if any anyBot [absEvalAtom AbsAnal a env | a <- as]
		   then AbsBot
		   else AbsTop
  where
    (_,_,_, tycon) = getDataConSig con
    has_single_con = maybeToBool (maybeSingleConstructorTyCon tycon)
\end{code}

\begin{code}
absEval anal (CoLam []      body) env	= absEval anal body env	-- paranoia
absEval anal (CoLam binders body) env	= AbsFun binders body env
absEval anal (CoTyLam ty expr)	  env	= absEval  anal expr env
absEval anal (CoApp e1 e2)	  env	= absApply anal (absEval     anal e1 env) 
							(absEvalAtom anal e2 env)
absEval anal (CoTyApp expr ty)	  env	= absEval anal expr env
\end{code}

For primitive cases, just GLB the branches, then LUB with the expr part.

\begin{code}
absEval anal (CoCase expr (CoPrimAlts alts deflt)) env
  = let
	expr_val    = absEval anal expr env
	abs_alts    = [ absEval anal rhs env | (_, rhs) <- alts ]
			-- Don't bother to extend envt, because unbound vars
			-- default to the conservative AbsTop

	abs_deflt   = absEvalDefault anal expr_val deflt env
    in
	combineCaseValues anal expr_val
			       (abs_deflt ++ abs_alts)

absEval anal (CoCase expr (CoAlgAlts alts deflt)) env
  = let
	expr_val  = absEval anal expr env 
	abs_alts  = [ absEvalAlgAlt anal expr_val alt env | alt <- alts ]
	abs_deflt = absEvalDefault anal expr_val deflt env
    in
    let
	result =
	  combineCaseValues anal expr_val
				(abs_deflt ++ abs_alts)
    in
{-
    (case anal of
	StrAnal -> id
	_ -> pprTrace "absCase:ABS:" (ppAbove (ppCat [ppr PprDebug expr, ppr PprDebug result, ppr PprDebug expr_val, ppr PprDebug abs_deflt, ppr PprDebug abs_alts]) (ppr PprDebug (keysFM env `zip` eltsFM env)))
    )
-}
    result
\end{code}

For @CoLets@ we widen the value we get.  This is nothing to
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
absEval anal (CoLet (CoNonRec binder e1) e2) env
  = let
	new_env = addOneToAbsValEnv env binder (widen anal (absEval anal e1 env))
    in
	-- The binder of a CoNonRec should *not* be of unboxed type,
	-- hence no need to strictly evaluate the Rhs.
    absEval anal e2 new_env

absEval anal (CoLet (CoRec pairs) body) env
  = let
	(binders,rhss) = unzip pairs
	rhs_vals = cheapFixpoint anal binders rhss env	-- Returns widened values
	new_env  = growAbsValEnvList env (binders `zip` rhs_vals)
    in
    absEval anal body new_env
\end{code}

\begin{code}
absEval anal (CoSCC cc expr) env = absEval anal expr env

-- ToDo: add DPH stuff here
\end{code}

\begin{code}
absEvalAlgAlt :: AnalysisKind -> AbsVal -> (Id,[Id],PlainCoreExpr) -> AbsValEnv -> AbsVal

absEvalAlgAlt anal (AbsProd arg_vals) (con, args, rhs) env
  =	-- The scrutinee is a product value, so it must be of a single-constr
	-- type; so the constructor in this alternative must be the right one
	-- so we can go ahead and bind the constructor args to the components
	-- of the product value.
    ASSERT(length arg_vals == length args)
    let
	 new_env = growAbsValEnvList env (args `zip` arg_vals)
    in
    absEval anal rhs new_env

absEvalAlgAlt anal other_scrutinee (con, args, rhs) env
  = 	-- Scrutinised value is Top or Bot (it can't be a function!)
	-- So just evaluate the rhs with all constr args bound to Top.
	-- (If the scrutinee is Top we'll never evaluated this function
	-- call anyway!)
    ASSERT(ok_scrutinee)
    absEval anal rhs env
  where
    ok_scrutinee
      = case other_scrutinee of {
	  AbsTop -> True;   -- i.e., OK
	  AbsBot -> True;   -- ditto
	  _ 	 -> False   -- party over
	}

 
absEvalDefault :: AnalysisKind 
	       -> AbsVal		-- Value of scrutinee
	       -> PlainCoreCaseDefault 
	       -> AbsValEnv 
	       -> [AbsVal]		-- Empty or singleton

absEvalDefault anal scrut_val CoNoDefault env = []
absEvalDefault anal scrut_val (CoBindDefault binder expr) env	   
  = [absEval anal expr (addOneToAbsValEnv env binder scrut_val)]
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
result.	 A @CoLam@ with two or more args: return another @AbsFun@ with
an augmented environment.

\begin{code}
absApply anal (AbsFun [binder] body env) arg
  = absEval anal body (addOneToAbsValEnv env binder arg)

absApply anal (AbsFun (binder:bs) body env) arg
  = AbsFun bs body (addOneToAbsValEnv env binder arg)
\end{code}

\begin{code}
absApply StrAnal (AbsApproxFun (arg1_demand:ds)) arg
  = if evalStrictness arg1_demand arg
    then AbsBot
    else case ds of
	   []    -> AbsTop
	   other -> AbsApproxFun ds

absApply AbsAnal (AbsApproxFun (arg1_demand:ds)) arg
  = if evalAbsence arg1_demand arg
    then AbsBot
    else case ds of
	   []    -> AbsTop
	   other -> AbsApproxFun ds

#ifdef DEBUG
absApply anal (AbsApproxFun []) arg = panic ("absApply: Duff function: AbsApproxFun." ++ show anal)
absApply anal (AbsFun [] _ _)   arg = panic ("absApply: Duff function: AbsFun." ++ show anal)
absApply anal (AbsProd _)       arg = panic ("absApply: Duff function: AbsProd." ++ show anal)
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
findStrictness :: StrAnalFlags
	       -> [UniType]	-- Types of args in which strictness is wanted
	       -> AbsVal 	-- Abstract strictness value of function 
	       -> AbsVal	-- Abstract absence value of function
	       -> [Demand]	-- Resulting strictness annotation

findStrictness strflags [] str_val abs_val = []

findStrictness strflags (ty:tys) str_val abs_val
  = let
	demand 	     = findRecDemand strflags [] str_fn abs_fn ty
	str_fn val   = absApply StrAnal str_val val
	abs_fn val   = absApply AbsAnal abs_val val

	demands = findStrictness strflags tys
			(absApply StrAnal str_val AbsTop)
			(absApply AbsAnal abs_val AbsTop)
    in
    demand : demands
\end{code}


\begin{code}
findDemandStrOnly str_env expr binder 	-- Only strictness environment available
  = findRecDemand strflags [] str_fn abs_fn (getIdUniType binder)
  where
    str_fn val = absEval StrAnal expr (addOneToAbsValEnv str_env binder val)
    abs_fn val = AbsBot		-- Always says poison; so it looks as if
				-- nothing is absent; safe
    strflags   = getStrAnalFlags str_env

findDemandAbsOnly abs_env expr binder 	-- Only absence environment available
  = findRecDemand strflags [] str_fn abs_fn (getIdUniType binder)
  where
    str_fn val = AbsBot		-- Always says non-termination;
				-- that'll make findRecDemand peer into the
				-- structure of the value.
    abs_fn val = absEval AbsAnal expr (addOneToAbsValEnv abs_env binder val)
    strflags   = getStrAnalFlags abs_env
  

findDemand str_env abs_env expr binder
  = findRecDemand strflags [] str_fn abs_fn (getIdUniType binder)
  where
    str_fn val = absEval StrAnal expr (addOneToAbsValEnv str_env binder val)
    abs_fn val = absEval AbsAnal expr (addOneToAbsValEnv abs_env binder val)
    strflags   = getStrAnalFlags str_env
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
findRecDemand :: StrAnalFlags
	      -> [TyCon]	    -- TyCons already seen; used to avoid
				    -- zooming into recursive types
	      -> (AbsVal -> AbsVal) -- The strictness function
	      -> (AbsVal -> AbsVal) -- The absence function
	      -> UniType 	    -- The type of the argument
	      -> Demand

findRecDemand strflags seen str_fn abs_fn ty
  = if isPrimType ty then -- It's a primitive type!
       wwPrim

    else if not (anyBot (abs_fn AbsBot)) then -- It's absent
       -- We prefer absence over strictness: see NOTE above.
       WwLazy True

    else if not (all_strict ||
		 (num_strict && is_numeric_type ty) ||
		 (isBot (str_fn AbsBot))) then
	WwLazy False -- It's not strict and we're not pretending

    else -- It's strict (or we're pretending it is)!

       case getUniDataTyCon_maybe ty of

	 Nothing    -> wwStrict

	 Just (tycon,tycon_arg_tys,[data_con]) | tycon `not_elem` seen ->
	   -- Single constructor case, tycon not already seen higher up
	   let
	      (_,cmpnt_tys,_) = getInstantiatedDataConSig data_con tycon_arg_tys
	      prod_len = length cmpnt_tys

	      compt_strict_infos
		= [ findRecDemand strflags (tycon:seen)
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
		 wwUnpack compt_strict_infos
	  where
	   not_elem = isn'tIn "findRecDemand"

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
    (all_strict, num_strict) = strflags

    is_numeric_type ty
      = case (getUniDataTyCon_maybe ty) of -- NB: duplicates stuff done above
	  Nothing -> False
	  Just (tycon, _, _)
	    | tycon `is_elem`
	      [intTyCon, integerTyCon,
	       doubleTyCon, floatTyCon,
	       wordTyCon, addrTyCon]
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
cheapFixpoint :: AnalysisKind -> [Id] -> [PlainCoreExpr] -> AbsValEnv -> [AbsVal]

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
fixpoint :: AnalysisKind -> [Id] -> [PlainCoreExpr] -> AbsValEnv -> [AbsVal]

fixpoint anal [] _ env = []

fixpoint anal ids rhss env 
  = fix_loop initial_vals
  where
    initial_val id
      = case anal of	-- The (unsafe) starting point
	  StrAnal -> if (returnsRealWorld (getIdUniType id))
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
