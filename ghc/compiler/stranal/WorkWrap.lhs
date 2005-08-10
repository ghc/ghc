%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[WorkWrap]{Worker/wrapper-generating back-end of strictness analyser}

\begin{code}
module WorkWrap ( wwTopBinds, mkWrapper ) where

#include "HsVersions.h"

import CoreSyn
import CoreUnfold	( certainlyWillInline )
import CoreLint		( showPass, endPass )
import CoreUtils	( exprType, exprIsHNF )
import Id		( Id, idType, isOneShotLambda, 
			  setIdNewStrictness, mkWorkerId,
			  setIdWorkerInfo, setInlinePragma,
			  idInfo )
import MkId		( lazyIdKey, lazyIdUnfolding )
import Type		( Type )
import IdInfo		( WorkerInfo(..), arityInfo,
			  newDemandInfo, newStrictnessInfo, unfoldingInfo, inlinePragInfo
			)
import NewDemand        ( Demand(..), StrictSig(..), DmdType(..), DmdResult(..), 
			  Demands(..), mkTopDmdType, isBotRes, returnsCPR, topSig, isAbsent
			)
import UniqSupply	( UniqSupply, initUs_, returnUs, thenUs, mapUs, getUniqueUs, UniqSM )
import Unique		( hasKey )
import BasicTypes	( RecFlag(..), isNonRec, Activation(..) )
import VarEnv		( isEmptyVarEnv )
import Maybes		( orElse )
import DynFlags
import WwLib
import Util		( lengthIs, notNull )
import Outputable
\end{code}

We take Core bindings whose binders have:

\begin{enumerate}

\item Strictness attached (by the front-end of the strictness
analyser), and / or

\item Constructed Product Result information attached by the CPR
analysis pass.

\end{enumerate}

and we return some ``plain'' bindings which have been
worker/wrapper-ified, meaning: 

\begin{enumerate} 

\item Functions have been split into workers and wrappers where
appropriate.  If a function has both strictness and CPR properties
then only one worker/wrapper doing both transformations is produced;

\item Binders' @IdInfos@ have been updated to reflect the existence of
these workers/wrappers (this is where we get STRICTNESS and CPR pragma
info for exported values).
\end{enumerate}

\begin{code}

wwTopBinds :: DynFlags 
	   -> UniqSupply
	   -> [CoreBind]
	   -> IO [CoreBind]

wwTopBinds dflags us binds
  = do {
	showPass dflags "Worker Wrapper binds";

	-- Create worker/wrappers, and mark binders with their
	-- "strictness info" [which encodes their worker/wrapper-ness]
	let { binds' = workersAndWrappers us binds };

	endPass dflags "Worker Wrapper binds" 
		Opt_D_dump_worker_wrapper binds'
    }
\end{code}


\begin{code}
workersAndWrappers :: UniqSupply -> [CoreBind] -> [CoreBind]

workersAndWrappers us top_binds
  = initUs_ us $
    mapUs wwBind top_binds `thenUs` \ top_binds' ->
    returnUs (concat top_binds')
\end{code}

%************************************************************************
%*									*
\subsection[wwBind-wwExpr]{@wwBind@ and @wwExpr@}
%*									*
%************************************************************************

@wwBind@ works on a binding, trying each \tr{(binder, expr)} pair in
turn.  Non-recursive case first, then recursive...

\begin{code}
wwBind	:: CoreBind
	-> UniqSM [CoreBind]	-- returns a WwBinding intermediate form;
				-- the caller will convert to Expr/Binding,
				-- as appropriate.

wwBind (NonRec binder rhs)
  = wwExpr rhs				`thenUs` \ new_rhs ->
    tryWW NonRecursive binder new_rhs 	`thenUs` \ new_pairs ->
    returnUs [NonRec b e | (b,e) <- new_pairs]
      -- Generated bindings must be non-recursive
      -- because the original binding was.

wwBind (Rec pairs)
  = mapUs do_one pairs		`thenUs` \ new_pairs ->
    returnUs [Rec (concat new_pairs)]
  where
    do_one (binder, rhs) = wwExpr rhs 	`thenUs` \ new_rhs ->
			   tryWW Recursive binder new_rhs
\end{code}

@wwExpr@ basically just walks the tree, looking for appropriate
annotations that can be used. Remember it is @wwBind@ that does the
matching by looking for strict arguments of the correct type.
@wwExpr@ is a version that just returns the ``Plain'' Tree.

\begin{code}
wwExpr :: CoreExpr -> UniqSM CoreExpr

wwExpr e@(Type _)   	      = returnUs e
wwExpr e@(Lit _)    	      = returnUs e
wwExpr e@(Note InlineMe expr) = returnUs e
	-- Don't w/w inside InlineMe's

wwExpr e@(Var v)
  | v `hasKey` lazyIdKey = returnUs lazyIdUnfolding
  | otherwise            = returnUs e
	-- Inline 'lazy' after strictness analysis
	-- (but not inside InlineMe's)

wwExpr (Lam binder expr)
  = wwExpr expr			`thenUs` \ new_expr ->
    returnUs (Lam binder new_expr)

wwExpr (App f a)
  = wwExpr f			`thenUs` \ new_f ->
    wwExpr a			`thenUs` \ new_a ->
    returnUs (App new_f new_a)

wwExpr (Note note expr)
  = wwExpr expr			`thenUs` \ new_expr ->
    returnUs (Note note new_expr)

wwExpr (Let bind expr)
  = wwBind bind			`thenUs` \ intermediate_bind ->
    wwExpr expr			`thenUs` \ new_expr ->
    returnUs (mkLets intermediate_bind new_expr)

wwExpr (Case expr binder ty alts)
  = wwExpr expr				`thenUs` \ new_expr ->
    mapUs ww_alt alts			`thenUs` \ new_alts ->
    returnUs (Case new_expr binder ty new_alts)
  where
    ww_alt (con, binders, rhs)
      =	wwExpr rhs			`thenUs` \ new_rhs ->
	returnUs (con, binders, new_rhs)
\end{code}

%************************************************************************
%*									*
\subsection[tryWW]{@tryWW@: attempt a worker/wrapper pair}
%*									*
%************************************************************************

@tryWW@ just accumulates arguments, converts strictness info from the
front-end into the proper form, then calls @mkWwBodies@ to do
the business.

We have to BE CAREFUL that we don't worker-wrapperize an Id that has
already been w-w'd!  (You can end up with several liked-named Ids
bouncing around at the same time---absolute mischief.)  So the
criterion we use is: if an Id already has an unfolding (for whatever
reason), then we don't w-w it.

The only reason this is monadised is for the unique supply.

\begin{code}
tryWW	:: RecFlag
	-> Id				-- The fn binder
	-> CoreExpr			-- The bound rhs; its innards
					--   are already ww'd
	-> UniqSM [(Id, CoreExpr)]	-- either *one* or *two* pairs;
					-- if one, then no worker (only
					-- the orig "wrapper" lives on);
					-- if two, then a worker and a
					-- wrapper.
tryWW is_rec fn_id rhs
  |  isNonRec is_rec && certainlyWillInline unfolding
	-- No point in worker/wrappering a function that is going to be
	-- INLINEd wholesale anyway.  If the strictness analyser is run
	-- twice, this test also prevents wrappers (which are INLINEd)
	-- from being re-done.
	--	
	-- It's very important to refrain from w/w-ing an INLINE function
	-- If we do so by mistake we transform
	--	f = __inline (\x -> E)
	-- into
	--	f = __inline (\x -> case x of (a,b) -> fw E)
	--	fw = \ab -> (__inline (\x -> E)) (a,b)
	-- and the original __inline now vanishes, so E is no longer
	-- inside its __inline wrapper.  Death!  Disaster!
  = returnUs [ (new_fn_id, rhs) ]

  | is_thunk && worthSplittingThunk maybe_fn_dmd res_info
  = ASSERT2( isNonRec is_rec, ppr new_fn_id )	-- The thunk must be non-recursive
    splitThunk new_fn_id rhs

  | is_fun && worthSplittingFun wrap_dmds res_info
  = splitFun new_fn_id fn_info wrap_dmds res_info inline_prag rhs

  | otherwise
  = returnUs [ (new_fn_id, rhs) ]

  where
    fn_info   	 = idInfo fn_id
    maybe_fn_dmd = newDemandInfo fn_info
    unfolding 	 = unfoldingInfo fn_info
    inline_prag  = inlinePragInfo fn_info

	-- In practice it always will have a strictness 
	-- signature, even if it's a uninformative one
    strict_sig  = newStrictnessInfo fn_info `orElse` topSig
    StrictSig (DmdType env wrap_dmds res_info) = strict_sig

	-- new_fn_id has the DmdEnv zapped.  
	--	(a) it is never used again
	--	(b) it wastes space
	--	(c) it becomes incorrect as things are cloned, because
	--	    we don't push the substitution into it
    new_fn_id | isEmptyVarEnv env = fn_id
	      | otherwise	  = fn_id `setIdNewStrictness` 
				     StrictSig (mkTopDmdType wrap_dmds res_info)

    is_fun    = notNull wrap_dmds
    is_thunk  = not is_fun && not (exprIsHNF rhs)

---------------------
splitFun fn_id fn_info wrap_dmds res_info inline_prag rhs
  = WARN( not (wrap_dmds `lengthIs` arity), ppr fn_id <+> (ppr arity $$ ppr wrap_dmds $$ ppr res_info) )
	-- The arity should match the signature
    mkWwBodies fun_ty wrap_dmds res_info one_shots 	`thenUs` \ (work_demands, wrap_fn, work_fn) ->
    getUniqueUs						`thenUs` \ work_uniq ->
    let
	work_rhs = work_fn rhs
	work_id  = mkWorkerId work_uniq fn_id (exprType work_rhs) 
			`setInlinePragma` inline_prag
			`setIdNewStrictness` StrictSig (mkTopDmdType work_demands work_res_info)
				-- Even though we may not be at top level, 
				-- it's ok to give it an empty DmdEnv

	wrap_rhs = wrap_fn work_id
	wrap_id  = fn_id `setIdWorkerInfo` HasWorker work_id arity
			 `setInlinePragma` AlwaysActive	-- Zap any inline pragma;
							-- Put it on the worker instead
    in
    returnUs ([(work_id, work_rhs), (wrap_id, wrap_rhs)])
	-- Worker first, because wrapper mentions it
	-- mkWwBodies has already built a wrap_rhs with an INLINE pragma wrapped around it
  where
    fun_ty = idType fn_id

    arity  = arityInfo fn_info	-- The arity is set by the simplifier using exprEtaExpandArity
				-- So it may be more than the number of top-level-visible lambdas

    work_res_info | isBotRes res_info = BotRes	-- Cpr stuff done by wrapper
		  | otherwise	      = TopRes

    one_shots = get_one_shots rhs

-- If the original function has one-shot arguments, it is important to
-- make the wrapper and worker have corresponding one-shot arguments too.
-- Otherwise we spuriously float stuff out of case-expression join points,
-- which is very annoying.
get_one_shots (Lam b e)
  | isId b    = isOneShotLambda b : get_one_shots e
  | otherwise = get_one_shots e
get_one_shots (Note _ e) = get_one_shots e
get_one_shots other	 = noOneShotInfo
\end{code}

Thunk splitting
~~~~~~~~~~~~~~~
Suppose x is used strictly (never mind whether it has the CPR
property).  

      let
	x* = x-rhs
      in body

splitThunk transforms like this:

      let
	x* = case x-rhs of { I# a -> I# a }
      in body

Now simplifier will transform to

      case x-rhs of 
	I# a ->	let x* = I# b 
	        in body

which is what we want. Now suppose x-rhs is itself a case:

	x-rhs = case e of { T -> I# a; F -> I# b }

The join point will abstract over a, rather than over (which is
what would have happened before) which is fine.

Notice that x certainly has the CPR property now!

In fact, splitThunk uses the function argument w/w splitting 
function, so that if x's demand is deeper (say U(U(L,L),L))
then the splitting will go deeper too.

\begin{code}
-- splitThunk converts the *non-recursive* binding
--	x = e
-- into
--	x = let x = e
--	    in case x of 
--		 I# y -> let x = I# y in x }
-- See comments above. Is it not beautifully short?

splitThunk fn_id rhs
  = mkWWstr [fn_id]		`thenUs` \ (_, wrap_fn, work_fn) ->
    returnUs [ (fn_id, Let (NonRec fn_id rhs) (wrap_fn (work_fn (Var fn_id)))) ]
\end{code}


%************************************************************************
%*									*
\subsection{Functions over Demands}
%*									*
%************************************************************************

\begin{code}
worthSplittingFun :: [Demand] -> DmdResult -> Bool
		-- True <=> the wrapper would not be an identity function
worthSplittingFun ds res
  = any worth_it ds || returnsCPR res
	-- worthSplitting returns False for an empty list of demands,
	-- and hence do_strict_ww is False if arity is zero and there is no CPR

	-- We used not to split if the result is bottom.
	-- [Justification:  there's no efficiency to be gained.]
	-- But it's sometimes bad not to make a wrapper.  Consider
	--	fw = \x# -> let x = I# x# in case e of
	--					p1 -> error_fn x
	--					p2 -> error_fn x
	--					p3 -> the real stuff
	-- The re-boxing code won't go away unless error_fn gets a wrapper too.
	-- [We don't do reboxing now, but in general it's better to pass 
	--  an unboxed thing to f, and have it reboxed in the error cases....]
  where
    worth_it Abs	      = True	-- Absent arg
    worth_it (Eval (Prod ds)) = True	-- Product arg to evaluate
    worth_it other	      = False

worthSplittingThunk :: Maybe Demand	-- Demand on the thunk
		    -> DmdResult	-- CPR info for the thunk
		    -> Bool
worthSplittingThunk maybe_dmd res
  = worth_it maybe_dmd || returnsCPR res
  where
	-- Split if the thing is unpacked
    worth_it (Just (Eval (Prod ds))) = not (all isAbsent ds)
    worth_it other	   	     = False
\end{code}



%************************************************************************
%*									*
\subsection{The worker wrapper core}
%*									*
%************************************************************************

@mkWrapper@ is called when importing a function.  We have the type of 
the function and the name of its worker, and we want to make its body (the wrapper).

\begin{code}
mkWrapper :: Type		-- Wrapper type
	  -> StrictSig		-- Wrapper strictness info
	  -> UniqSM (Id -> CoreExpr)	-- Wrapper body, missing worker Id

mkWrapper fun_ty (StrictSig (DmdType _ demands res_info))
  = mkWwBodies fun_ty demands res_info noOneShotInfo	`thenUs` \ (_, wrap_fn, _) ->
    returnUs wrap_fn

noOneShotInfo = repeat False
\end{code}
