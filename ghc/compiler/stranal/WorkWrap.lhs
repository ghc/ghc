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
import CoreUtils	( exprType )
import Id		( Id, idType, idNewStrictness, idArity, isOneShotLambda,
			  setIdNewStrictness, zapIdNewStrictness, idInlinePragma, mkWorkerId,
			  setIdWorkerInfo, setInlinePragma )
import Type		( Type )
import IdInfo		( WorkerInfo(..) )
import NewDemand        ( Demand(..), StrictSig(..), DmdType(..), DmdResult(..), 
			  mkTopDmdType, isBotRes, returnsCPR
			)
import UniqSupply	( UniqSupply, initUs_, returnUs, thenUs, mapUs, getUniqueUs, UniqSM )
import BasicTypes	( RecFlag(..), isNonRec, Activation(..), isNeverActive )
import CmdLineOpts
import WwLib
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

wwExpr e@(Type _)   = returnUs e
wwExpr e@(Var _)    = returnUs e
wwExpr e@(Lit _)    = returnUs e

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

wwExpr (Case expr binder alts)
  = wwExpr expr				`thenUs` \ new_expr ->
    mapUs ww_alt alts			`thenUs` \ new_alts ->
    returnUs (Case new_expr binder new_alts)
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
  | isNeverActive inline_prag
	-- Don't split NOINLINE things, because they will never be inlined
	-- Furthermore, zap the strictess info in the Id.  Why?  Because
	-- the NOINLINE says "don't expose any of the inner workings at the call 
	-- site" and the strictness is certainly an inner working.
	--
	-- More concretely, the demand analyser discovers the following strictness
	-- for unsafePerformIO:  C(U(AV))
	-- But then consider
	--	unsafePerformIO (\s -> let r = f x in 
	--			       case writeIORef v r s of (# s1, _ #) ->
	--			       (# s1, r #)
	-- The strictness analyser will find that the binding for r is strict,
	-- (becuase of uPIO's strictness sig), and so it'll evaluate it before 
	-- doing the writeIORef.  This actually makes tests/lib/should_run/memo002
	-- get a deadlock!  
	--
	-- Solution: don't expose the strictness of unsafePerformIO.
  = returnUs [ (zapIdNewStrictness fn_id, rhs) ]

  |  arity == 0
	-- Don't worker-wrapper thunks
  || isNonRec is_rec && certainlyWillInline fn_id
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
  || not (worthSplitting strict_sig)
	-- Strictness info suggests not to w/w
  = returnUs [ (fn_id, rhs) ]

  | otherwise		-- Do w/w split!
  = WARN( arity /= length wrap_dmds, ppr fn_id <+> (ppr arity $$ ppr strict_sig) )
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
	wrap_id  = fn_id `setIdWorkerInfo`	HasWorker work_id arity
			 `setInlinePragma`	AlwaysActive	-- Zap any inline pragma;
								-- Put it on the worker instead
    in
    returnUs ([(work_id, work_rhs), (wrap_id, wrap_rhs)])
	-- Worker first, because wrapper mentions it
	-- mkWwBodies has already built a wrap_rhs with an INLINE pragma wrapped around it
  where
    fun_ty = idType fn_id
    arity  = idArity fn_id	-- The arity is set by the simplifier using exprEtaExpandArity
				-- So it may be more than the number of top-level-visible lambdas

    inline_prag = idInlinePragma fn_id
    strict_sig  = idNewStrictness fn_id

    StrictSig (DmdType _ wrap_dmds res_info) = strict_sig
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


%************************************************************************
%*									*
\subsection{Functions over Demands}
%*									*
%************************************************************************

\begin{code}
worthSplitting :: StrictSig -> Bool
		-- True <=> the wrapper would not be an identity function
worthSplitting (StrictSig (DmdType _ ds res))
  = any worth_it ds || returnsCPR res
	-- worthSplitting returns False for an empty list of demands,
	-- and hence do_strict_ww is False if arity is zero

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
    worth_it Abs	= True	-- Absent arg
    worth_it (Seq _ ds) = True	-- Arg to evaluate
    worth_it other	= False
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
