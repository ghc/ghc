%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[StrictAnal]{``Simple'' Mycroft-style strictness analyser}

The original version(s) of all strictness-analyser code (except the
Semantique analyser) was written by Andy Gill.

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

#ifndef OLD_STRICTNESS
module StrictAnal ( ) where

#else

module StrictAnal ( saBinds ) where

#include "HsVersions.h"

import DynFlags	( DynFlags, DynFlag(..) )
import CoreSyn
import Id		( setIdStrictness, setInlinePragma, 
			  idDemandInfo, setIdDemandInfo, isBottomingId,
			  Id
			)
import CoreLint		( showPass, endPass )
import ErrUtils		( dumpIfSet_dyn )
import SaAbsInt
import SaLib
import Demand		( Demand, wwStrict, isStrict, isLazy )
import Util		( zipWith3Equal, stretchZipWith, compareLength )
import BasicTypes	( Activation( NeverActive ) )
import Outputable
import FastTypes
import State
\end{code}

%************************************************************************
%*									*
\subsection[Thoughts]{Random thoughts}
%*									*
%************************************************************************

A note about worker-wrappering.  If we have

	f :: Int -> Int
	f = let v = <expensive>
	    in \x -> <body>

and we deduce that f is strict, it is nevertheless NOT safe to worker-wapper to

	f = \x -> case x of Int x# -> fw x#
	fw = \x# -> let x = Int x#
		    in
		    let v = <expensive>
		    in <body>

because this obviously loses laziness, since now <expensive>
is done each time.  Alas.

WATCH OUT!  This can mean that something is unboxed only to be
boxed again. For example

	g x y = f x

Here g is strict, and *will* split into worker-wrapper.  A call to
g, with the wrapper inlined will then be

	case arg of Int a# -> gw a#

Now g calls f, which has no wrapper, so it has to box it.

	gw = \a# -> f (Int a#)

Alas and alack.


%************************************************************************
%*									*
\subsection[iface-StrictAnal]{Interface to the outside world}
%*									*
%************************************************************************

@saBinds@ decorates bindings with strictness info.  A later 
worker-wrapper pass can use this info to create wrappers and
strict workers.

\begin{code}
saBinds :: DynFlags -> [CoreBind] -> IO [CoreBind]
saBinds dflags binds
  = do {
	showPass dflags "Strictness analysis";

	-- Mark each binder with its strictness
#ifndef OMIT_STRANAL_STATS
	let { (binds_w_strictness, sa_stats) = runState $ (saTopBinds binds) nullSaStats };
	dumpIfSet_dyn dflags Opt_D_dump_simpl_stats "Strictness analysis statistics"
		  (pp_stats sa_stats);
#else
	let { binds_w_strictness = unSaM $ saTopBindsBinds binds };
#endif

	endPass dflags "Strictness analysis" Opt_D_dump_stranal
		binds_w_strictness
    }
\end{code}

%************************************************************************
%*									*
\subsection[saBinds]{Strictness analysis of bindings}
%*									*
%************************************************************************

[Some of the documentation about types, etc., in \tr{SaLib} may be
helpful for understanding this module.]

@saTopBinds@ tags each binder in the program with its @Demand@.
That tells how each binder is {\em used}; if @Strict@, then the binder
is sure to be evaluated to HNF; if @NonStrict@ it may or may not be;
if @Absent@, then it certainly is not used. [DATED; ToDo: update]

(The above info is actually recorded for posterity in each binder's
IdInfo, notably its @DemandInfo@.)

We proceed by analysing the bindings top-to-bottom, building up an
environment which maps @Id@s to their abstract values (i.e., an
@AbsValEnv@ maps an @Id@ to its @AbsVal@).

\begin{code}
saTopBinds :: [CoreBind] -> SaM [CoreBind] -- not exported

saTopBinds binds
  = let
	starting_abs_env = nullAbsValEnv
    in
    do_it starting_abs_env starting_abs_env binds
  where
    do_it _    _    [] = return []
    do_it senv aenv (b:bs) = do
        (senv2, aenv2, new_b) <- saTopBind senv  aenv  b
        new_bs                <- do_it     senv2 aenv2 bs
        return (new_b : new_bs)
\end{code}

@saTopBind@ is only used for the top level.  We don't add any demand
info to these ids because we can't work it out.  In any case, it
doesn't do us any good to know whether top-level binders are sure to
be used; we can't turn top-level @let@s into @case@s.

\begin{code}
saTopBind :: StrictEnv -> AbsenceEnv
	  -> CoreBind
	  -> SaM (StrictEnv, AbsenceEnv, CoreBind)

saTopBind str_env abs_env (NonRec binder rhs) = do
    new_rhs <- saExpr minDemand str_env abs_env rhs
    let
	str_rhs = absEval StrAnal rhs str_env
	abs_rhs = absEval AbsAnal rhs abs_env

	widened_str_rhs = widen StrAnal str_rhs
	widened_abs_rhs = widen AbsAnal abs_rhs
		-- The widening above is done for efficiency reasons.
		-- See notes on Let case in SaAbsInt.lhs

	new_binder
	  = addStrictnessInfoToTopId
		widened_str_rhs widened_abs_rhs
		binder

	  -- Augment environments with a mapping of the
	  -- binder to its abstract values, computed by absEval
      	new_str_env = addOneToAbsValEnv str_env binder widened_str_rhs
      	new_abs_env = addOneToAbsValEnv abs_env binder widened_abs_rhs
    
    return (new_str_env, new_abs_env, NonRec new_binder new_rhs)

saTopBind str_env abs_env (Rec pairs)
  = let
	(binders,rhss) = unzip pairs
	str_rhss    = fixpoint StrAnal binders rhss str_env
	abs_rhss    = fixpoint AbsAnal binders rhss abs_env
		      -- fixpoint returns widened values
      	new_str_env = growAbsValEnvList str_env (binders `zip` str_rhss)
      	new_abs_env = growAbsValEnvList abs_env (binders `zip` abs_rhss)
	new_binders = zipWith3Equal "saTopBind" addStrictnessInfoToTopId
				    str_rhss abs_rhss binders
    
    new_rhss <- mapM (saExpr minDemand new_str_env new_abs_env) rhss
    let
	new_pairs   = new_binders `zip` new_rhss
    
    return (new_str_env, new_abs_env, Rec new_pairs)

-- Hack alert!
-- Top level divergent bindings are marked NOINLINE
-- This avoids fruitless inlining of top level error functions
addStrictnessInfoToTopId str_val abs_val bndr
  = if isBottomingId new_id then
	new_id `setInlinePragma` NeverActive
    else
	new_id
  where
    new_id = addStrictnessInfoToId str_val abs_val bndr
\end{code}

%************************************************************************
%*									*
\subsection[saExpr]{Strictness analysis of an expression}
%*									*
%************************************************************************

@saExpr@ computes the strictness of an expression within a given
environment.

\begin{code}
saExpr :: Demand -> StrictEnv -> AbsenceEnv -> CoreExpr -> SaM CoreExpr
	-- The demand is the least demand we expect on the
	-- expression.  WwStrict is the least, because we're only
	-- interested in the expression at all if it's being evaluated,
	-- but the demand may be more.  E.g.
	--	f E
	-- where f has strictness u(LL), will evaluate E with demand u(LL)

minDemand = wwStrict 
minDemands = repeat minDemand

-- When we find an application, do the arguments
-- with demands gotten from the function
saApp str_env abs_env (fun, args) = do
    args' <- sequence sa_args
    fun' <- saExpr minDemand str_env abs_env fun
    return (mkApps fun' args')
  where
    arg_dmds = case fun of
		 Var var -> case lookupAbsValEnv str_env var of
				Just (AbsApproxFun ds _) 
				   | compareLength ds args /= LT 
				              -- 'ds' is at least as long as 'args'.
					-> ds ++ minDemands
				other   -> minDemands
		 other -> minDemands

    sa_args = stretchZipWith isTypeArg (error "saApp:dmd") 
			     sa_arg args arg_dmds 
	-- The arg_dmds are for value args only, we need to skip
	-- over the type args when pairing up with the demands
	-- Hence the stretchZipWith

    sa_arg arg dmd = saExpr dmd' str_env abs_env arg
		   where
			-- Bring arg demand up to minDemand
			dmd' | isLazy dmd = minDemand
			     | otherwise  = dmd

saExpr _ _ _ e@(Var _)	= return e
saExpr _ _ _ e@(Lit _)	= return e
saExpr _ _ _ e@(Type _)	= return e

saExpr dmd str_env abs_env (Lam bndr body)
  = do	-- Don't bother to set the demand-info on a lambda binder
	-- We do that only for let(rec)-bound functions
    new_body <- saExpr minDemand str_env abs_env body
    return (Lam bndr new_body)

saExpr dmd str_env abs_env e@(App fun arg)
  = saApp str_env abs_env (collectArgs e)

saExpr dmd str_env abs_env (Note note expr) = do
    new_expr <- saExpr dmd str_env abs_env expr
    return (Note note new_expr)

saExpr dmd str_env abs_env (Case expr case_bndr alts) = do
    new_expr <- saExpr minDemand str_env abs_env expr
    new_alts <- mapM sa_alt alts
    let
	new_case_bndr = addDemandInfoToCaseBndr dmd str_env abs_env alts case_bndr
    return (Case new_expr new_case_bndr new_alts)
  where
    sa_alt (con, binders, rhs) = do
        new_rhs <- saExpr dmd str_env abs_env rhs
	let
	    new_binders = map add_demand_info binders
	    add_demand_info bndr | isTyVar bndr = bndr
			  	 | otherwise	= addDemandInfoToId dmd str_env abs_env rhs bndr
	
	tickCases new_binders -- stats
	return (con, new_binders, new_rhs)

saExpr dmd str_env abs_env (Let (NonRec binder rhs) body) = do
	-- Analyse the RHS in the environment at hand
    let
	-- Find the demand on the RHS
	rhs_dmd = findDemand dmd str_env abs_env body binder

	-- Bind this binder to the abstract value of the RHS; analyse
	-- the body of the `let' in the extended environment.
      	str_rhs_val  	= absEval StrAnal rhs str_env
      	abs_rhs_val  	= absEval AbsAnal rhs abs_env

	widened_str_rhs = widen StrAnal str_rhs_val
	widened_abs_rhs = widen AbsAnal abs_rhs_val
		-- The widening above is done for efficiency reasons.
		-- See notes on Let case in SaAbsInt.lhs

      	new_str_env	= addOneToAbsValEnv str_env binder widened_str_rhs
      	new_abs_env	= addOneToAbsValEnv abs_env binder widened_abs_rhs

	-- Now determine the strictness of this binder; use that info
	-- to record DemandInfo/StrictnessInfo in the binder.
	new_binder = addStrictnessInfoToId
			widened_str_rhs widened_abs_rhs
			(binder `setIdDemandInfo` rhs_dmd)
    
    tickLet new_binder		 -- stats
    new_rhs <- saExpr rhs_dmd str_env abs_env rhs
    new_body <- saExpr dmd new_str_env new_abs_env body
    return (Let (NonRec new_binder new_rhs) new_body)

saExpr dmd str_env abs_env (Let (Rec pairs) body) = do
    let
	(binders,rhss) = unzip pairs
	str_vals       = fixpoint StrAnal binders rhss str_env
	abs_vals       = fixpoint AbsAnal binders rhss abs_env
			 -- fixpoint returns widened values
      	new_str_env    = growAbsValEnvList str_env (binders `zip` str_vals)
      	new_abs_env    = growAbsValEnvList abs_env (binders `zip` abs_vals)
    
    new_body <- saExpr dmd new_str_env new_abs_env body
    new_rhss <- mapM (saExpr minDemand new_str_env new_abs_env) rhss
    let
-- 		DON'T add demand info in a Rec!
--		a) it's useless: we can't do let-to-case
--		b) it's incorrect.  Consider
--			letrec x = ...y...
--			       y = ...x...
--			in ...x...
--		   When we ask whether y is demanded we'll bind y to bottom and
--		   evaluate the body of the letrec.  But that will result in our
--		   deciding that y is absent, which is plain wrong!
--		It's much easier simply not to do this.

	improved_binders = zipWith3Equal "saExpr" addStrictnessInfoToId
				         str_vals abs_vals binders

      	new_pairs   = improved_binders `zip` new_rhss
    
    return (Let (Rec new_pairs) new_body)
\end{code}


%************************************************************************
%*									*
\subsection[computeInfos]{Add computed info to binders}
%*									*
%************************************************************************

Important note (Sept 93).  @addStrictnessInfoToId@ is used only for
let(rec) bound variables, and is use to attach the strictness (not
demand) info to the binder.  We are careful to restrict this
strictness info to the lambda-bound arguments which are actually
visible, at the top level, lest we accidentally lose laziness by
eagerly looking for an "extra" argument.  So we "dig for lambdas" in a
rather syntactic way.

A better idea might be to have some kind of arity analysis to
tell how many args could safely be grabbed.

\begin{code}
addStrictnessInfoToId
	:: AbsVal 		-- Abstract strictness value
	-> AbsVal		-- Ditto absence
	-> Id 			-- The id
	-> Id			-- Augmented with strictness

addStrictnessInfoToId str_val abs_val binder
  = binder `setIdStrictness` findStrictness binder str_val abs_val
\end{code}

\begin{code}
addDemandInfoToId :: Demand -> StrictEnv -> AbsenceEnv
		  -> CoreExpr 	-- The scope of the id
		  -> Id
		  -> Id			-- Id augmented with Demand info

addDemandInfoToId dmd str_env abs_env expr binder
  = binder `setIdDemandInfo` (findDemand dmd str_env abs_env expr binder)

addDemandInfoToCaseBndr dmd str_env abs_env alts binder
  = binder `setIdDemandInfo` (findDemandAlts dmd str_env abs_env alts binder)
\end{code}

%************************************************************************
%*									*
\subsection{Monad used herein for stats}
%*									*
%************************************************************************

\begin{code}
data SaStats
  = SaStats FastInt FastInt	-- total/marked-demanded lambda-bound
	    FastInt FastInt	-- total/marked-demanded case-bound
	    FastInt FastInt	-- total/marked-demanded let-bound
				-- (excl. top-level; excl. letrecs)

nullSaStats = SaStats
   (_ILIT(0)) (_ILIT(0))
   (_ILIT(0)) (_ILIT(0))
   (_ILIT(0)) (_ILIT(0))

tickLambda :: Id   -> SaM ()
tickCases  :: [CoreBndr] -> SaM ()
tickLet    :: Id   -> SaM ()

#ifndef OMIT_STRANAL_STATS
type SaM a = State SaStats a

tickLambda var = modify $ \(SaStats tlam dlam tc dc tlet dlet)
  -> case (tick_demanded var (0,0)) of { (totB, demandedB) ->
     let tot = iUnbox totB ; demanded = iUnbox demandedB 
     in SaStats (tlam +# tot) (dlam +# demanded) tc dc tlet dlet)

tickCases vars = modify $ \(SaStats tlam dlam tc dc tlet dlet)
  = case (foldr tick_demanded (0,0) vars) of { (totB, demandedB) ->
    let tot = iUnbox totB ; demanded = iUnbox demandedB 
    in  SaStats tlam dlam (tc +# tot) (dc +# demanded) tlet dlet)

tickLet var = modify $ \(SaStats tlam dlam tc dc tlet dlet)
  = case (tick_demanded var (0,0))        of { (totB, demandedB) ->
    let tot = iUnbox totB ; demanded = iUnbox demandedB 
    in SaStats tlam dlam tc dc (tlet +# tot) (dlet +# demanded))

tick_demanded var (tot, demanded)
  | isTyVar var = (tot, demanded)
  | otherwise
  = (tot + 1,
     if (isStrict (idDemandInfo var))
     then demanded + 1
     else demanded)

pp_stats (SaStats tlam dlam tc dc tlet dlet)
      = hcat [ptext (sLit "Lambda vars: "), int (iBox dlam), char '/', int (iBox tlam),
	      ptext (sLit "; Case vars: "), int (iBox dc),   char '/', int (iBox tc),
	      ptext (sLit "; Let vars: "),  int (iBox dlet), char '/', int (iBox tlet)
	]

#else /* OMIT_STRANAL_STATS */
-- identity monad
newtype SaM a = SaM { unSaM :: a }

instance Monad SaM where
    return x    = SaM x
    SaM x >>= f = f x

tickLambda var  = panic "OMIT_STRANAL_STATS: tickLambda"
tickCases  vars = panic "OMIT_STRANAL_STATS: tickCases"
tickLet    var  = panic "OMIT_STRANAL_STATS: tickLet"

#endif /* OMIT_STRANAL_STATS */

#endif /* OLD_STRICTNESS */
\end{code}
