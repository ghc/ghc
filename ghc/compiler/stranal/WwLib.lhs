%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[WwLib]{A library for the ``worker/wrapper'' back-end to the strictness analyser}

\begin{code}
#include "HsVersions.h"

module WwLib (
	WwBinding(..),

	mkWwBodies, mAX_WORKER_ARGS,

	-- our friendly worker/wrapper monad:
	WwM(..),
	returnWw, thenWw, mapWw,
	getUniqueWw, uniqSMtoWwM,

	-- and to make the interface self-sufficient...
	GlobalSwitch, CoreBinding, CoreExpr, PlainCoreBinding(..),
	PlainCoreExpr(..), Id, Demand, MaybeErr,
	TyVar, UniType, Unique, SplitUniqSupply, SUniqSM(..)

	IF_ATTACK_PRAGMAS(COMMA splitUniqSupply COMMA getSUnique)
	IF_ATTACK_PRAGMAS(COMMA mkUniqueGrimily)
    ) where

IMPORT_Trace
import Outputable	-- ToDo: rm (debugging)
import Pretty

import AbsPrel		( aBSENT_ERROR_ID, mkFunTy )
import AbsUniType	( mkTyVarTy, isPrimType, getUniDataTyCon_maybe,
			  quantifyTy, TyVarTemplate
			)
import CmdLineOpts	( GlobalSwitch(..) )
import Id		( mkWorkerId, mkSysLocal, getIdUniType,
			  getInstantiatedDataConSig, getIdInfo,
			  replaceIdInfo, addIdStrictness, DataCon(..)
			)
import IdInfo		-- lots of things
import Maybes		( maybeToBool, Maybe(..), MaybeErr )
import PlainCore
import SaLib
import SrcLoc		( mkUnknownSrcLoc )
import SplitUniq
import Unique
import Util

infixr 9 `thenWw`
\end{code}

%************************************************************************
%*									*
\subsection[datatype-WwLib]{@WwBinding@: a datatype for worker/wrapper-ing}
%*									*
%************************************************************************

In the worker/wrapper stuff, we want to carry around @CoreBindings@ in
an ``intermediate form'' that can later be turned into a \tr{let} or
\tr{case} (depending on strictness info).

\begin{code}
data WwBinding
  = WwLet  [PlainCoreBinding]
  | WwCase (PlainCoreExpr -> PlainCoreExpr)
		-- the "case" will be a "strict let" of the form:
		--
		--  case rhs of
		--    <blah> -> body
		--
		-- (instead of "let <blah> = rhs in body")
		--
		-- The expr you pass to the function is "body" (the
		-- expression that goes "in the corner").
\end{code}

%************************************************************************
%*									*
\subsection[mkWrapperAndWorker]{@mkWrapperAndWorker@}
%*									*
%************************************************************************

	************   WARNING  ******************
	these comments are rather out of date
	*****************************************

@mkWrapperAndWorker@ is given:
\begin{enumerate}
\item
The {\em original function} \tr{f}, of the form:
\begin{verbatim}
f = /\ tyvars -> \ args -> body
\end{verbatim}
The original-binder \tr{f}, the \tr{tyvars}, \tr{args}, and \tr{body}
are given separately.

We use the Id \tr{f} mostly to get its type.

\item
Strictness information about \tr{f}, in the form of a list of
@Demands@.

\item
A @UniqueSupply@.
\end{enumerate}

@mkWrapperAndWorker@ produces (A BIT OUT-OF-DATE...):
\begin{enumerate}
\item
Maybe @Nothing@: no worker/wrappering going on in this case. This can
happen (a)~if the strictness info says that there is nothing
interesting to do or (b)~if *any* of the argument types corresponding
to ``active'' arg postitions is abstract or will be to the outside
world (i.e., {\em this} module can see the constructors, but nobody
else will be able to).  An ``active'' arg position is one which the
wrapper has to unpack.  An importing module can't do this unpacking,
so it simply has to give up and call the wrapper only.

\item
Maybe \tr{Just (wrapper_Id, wrapper_body, worker_Id, worker_body)}.

The @wrapper_Id@ is just the one that was passed in, with its
strictness IdInfo updated.
\end{enumerate}

The \tr{body} of the original function may not be given (i.e., it's
BOTTOM), in which case you'd jolly well better not tug on the
worker-body output!

Here's an example.  The original function is:
\begin{verbatim}
g :: forall a . Int -> [a] -> a

g = /\ a -> \ x ys ->
	case x of
	  0 -> head ys
	  _ -> head (tail ys)
\end{verbatim}

From this, we want to produce:
\begin{verbatim}
-- wrapper (an unfolding)
g :: forall a . Int -> [a] -> a

g = /\ a -> \ x ys ->
	case x of
	  I# x# -> g.wrk a x# ys
	    -- call the worker; don't forget the type args!

-- worker
g.wrk :: forall a . Int# -> [a] -> a

g.wrk = /\ a -> \ x# ys ->
	let
	    x = I# x#
	in
	    case x of		    -- note: body of g moved intact
	      0 -> head ys
	      _ -> head (tail ys)
\end{verbatim}

Something we have to be careful about:  Here's an example:
\begin{verbatim}
-- "f" strictness: U(P)U(P)
f (I# a) (I# b) = a +# b

g = f	-- "g" strictness same as "f"
\end{verbatim}
\tr{f} will get a worker all nice and friendly-like; that's good.
{\em But we don't want a worker for \tr{g}}, even though it has the
same strictness as \tr{f}.  Doing so could break laziness, at best.

Consequently, we insist that the number of strictness-info items is
exactly the same as the number of lambda-bound arguments.  (This is
probably slightly paranoid, but OK in practice.)  If it isn't the
same, we ``revise'' the strictness info, so that we won't propagate
the unusable strictness-info into the interfaces.

==========================

Here's the real fun... The wrapper's ``deconstructing'' of arguments
and the worker's putting them back together again are ``duals'' in
some sense.

What we do is walk along the @Demand@ list, producing two
expressions (one for wrapper, one for worker...), each with a ``hole''
in it, where we will later plug in more information.  For our previous
example, the expressions-with-HOLES are:
\begin{verbatim}
\ x ys ->		-- wrapper
	case x of
	  I# x# -> <<HOLE>> x# ys

\ x# ys ->		-- worker
	let
	    x = I# x#
	in
	    <<HOLE>>
\end{verbatim}
(Actually, we add the lambda-bound arguments at the end...) (The big
Lambdas are added on the front later.)

\begin{code}
mkWwBodies
	:: UniType		-- Type of the *body* of the orig
				-- function; i.e. /\ tyvars -> \ vars -> body
	-> [TyVar] 		-- Type lambda vars of original function
	-> [Id]			-- Args of original function
	-> [Demand]		-- Strictness info for those args

	-> SUniqSM (Maybe 	-- Nothing iff (a) no interesting split possible
				-- 	       (b) any unpack on abstract type
		     (Id -> PlainCoreExpr,		-- Wrapper expr w/ 
							--   hole for worker id
		      PlainCoreExpr -> PlainCoreExpr,	-- Worker expr w/ hole 
	   						--   for original fn body
		      StrictnessInfo,			-- Worker strictness info
		      UniType -> UniType)		-- Worker type w/ hole
	   )						--   for type of original fn body
		  

mkWwBodies body_ty tyvars args arg_infos
  = ASSERT(length args == length arg_infos)
    -- or you can get disastrous user/definer-module mismatches
    if (all_absent_args_and_unboxed_value body_ty arg_infos)
    then returnSUs Nothing

    else -- the rest...
    mk_ww_arg_processing args arg_infos (mAX_WORKER_ARGS - nonAbsentArgs arg_infos)
		`thenUsMaybe` \ (wrap_frag, work_args_info, work_frag) ->
    let 
	(work_args, wrkr_demands) = unzip work_args_info

	wrkr_strictness = mkStrictnessInfo wrkr_demands Nothing -- no worker-of-worker...

	wrapper_w_hole = \ worker_id ->
				mkCoTyLam tyvars (
				mkCoLam args (
				wrap_frag (
				mkCoTyApps (CoVar worker_id) (map mkTyVarTy tyvars)
			 )))

	worker_w_hole = \ orig_body ->
				mkCoTyLam tyvars (
				mkCoLam work_args (
				work_frag orig_body
			))

	worker_ty_w_hole = \ body_ty ->
				snd (quantifyTy tyvars (
				foldr mkFunTy body_ty (map getIdUniType work_args)
			   ))
    in
    returnSUs (Just (wrapper_w_hole, worker_w_hole, wrkr_strictness, worker_ty_w_hole))
  where
    -- "all_absent_args_and_unboxed_value":
    -- check for the obscure case of "\ x y z ... -> body" where
    -- (a) *all* of the args x, y, z,... are absent, and
    -- (b) the type of body is unboxed
    -- If these conditions are true, we must *not* play worker/wrapper games!

    all_absent_args_and_unboxed_value body_ty arg_infos
      = not (null arg_infos)
	&& all is_absent_arg arg_infos
	&& isPrimType body_ty

    is_absent_arg (WwLazy True) = True
    is_absent_arg _		= False
\end{code}

Important: mk_ww_arg_processing doesn't check
for an "interesting" split.  It just races ahead and makes the
split, even if there's no unpacking at all.  This is important for
when it calls itself recursively.

It returns Nothing only if it encounters an abstract type in mid-flight.

\begin{code}
mAX_WORKER_ARGS :: Int		-- ToDo: set via flag
mAX_WORKER_ARGS = 6		-- Hmm... but this is an everything-must-
				-- be-compiled-with-the-same-val thing...

mk_ww_arg_processing
	:: [Id]			-- Args of original function
	-> [Demand]		-- Strictness info for those args
				--   must be at least as long as args

	-> Int			-- Number of extra args we are prepared to add.
				-- This prevents over-eager unpacking, leading
				-- to huge-arity functions.

	-> SUniqSM (Maybe 	-- Nothing iff any unpack on abstract type
		     (PlainCoreExpr -> PlainCoreExpr,	-- Wrapper expr w/ 
							--   hole for worker id
							--   applied to types
		      [(Id,Demand)],			-- Worker's args
							-- and their strictness info	
		      PlainCoreExpr -> PlainCoreExpr)	-- Worker body expr w/ hole 
	   )						--   for original fn body

mk_ww_arg_processing [] _ _ = returnSUs (Just (id, [], id))

mk_ww_arg_processing (arg : args) (WwLazy True : infos) max_extra_args
  =  	-- Absent argument
	-- So, finish args to the right...
    --pprTrace "Absent; num_wrkr_args=" (ppInt num_wrkr_args) (
    let
	arg_ty = getIdUniType arg
    in
    mk_ww_arg_processing args infos max_extra_args
				    -- we've already discounted for absent args,
				    -- so we don't change max_extra_args
		   `thenUsMaybe` \ (wrap_rest, work_args_info, work_rest) ->

       		-- wrapper doesn't pass this arg to worker:
    returnSUs (Just (
		 -- wrapper:
		 \ hole -> wrap_rest hole,

		 -- worker:
		 work_args_info, -- NB: no argument added
		 \ hole -> mk_absent_let arg arg_ty (work_rest hole)
    ))
    --)
  where
    mk_absent_let arg arg_ty body
      = if not (isPrimType arg_ty) then
	    CoLet (CoNonRec arg (mkCoTyApp (CoVar aBSENT_ERROR_ID) arg_ty))
		  body
	else -- quite horrible
	    panic "WwLib: haven't done mk_absent_let for primitives yet"


mk_ww_arg_processing (arg : args) (WwUnpack cmpnt_infos : infos) max_extra_args
  | new_max_extra_args > 0	-- Check that we are prepared to add arguments
  = 	-- this is the complicated one.
    --pprTrace "Unpack; num_wrkr_args=" (ppCat [ppInt num_wrkr_args, ppStr "; new_max=", ppInt new_num_wrkr_args, ppStr "; arg=", ppr PprDebug arg, ppr PprDebug (WwUnpack cmpnt_infos)]) (
    case getUniDataTyCon_maybe arg_ty of

	  Nothing 	  -> 	   -- Not a data type
				   panic "mk_ww_arg_processing: not datatype"

	  Just (_, _, []) ->	   -- An abstract type
				   -- We have to give up on the whole idea
				   returnSUs Nothing
	  Just (_, _, (_:_:_)) ->  -- Two or more constructors; that's odd
				   panic "mk_ww_arg_processing: multi-constr"

	  Just (arg_tycon, tycon_arg_tys, [data_con]) -> 
			-- The main event: a single-constructor data type

	    let
		(_,inst_con_arg_tys,_)
	          = getInstantiatedDataConSig data_con tycon_arg_tys
	    in
	    getSUniques (length inst_con_arg_tys)    `thenSUs` \ uniqs ->

	    let unpk_args = zipWith (\ u t -> mkSysLocal SLIT("upk") u t mkUnknownSrcLoc)
				    uniqs inst_con_arg_tys
	    in
		-- In processing the rest, push the sub-component args
		-- and infos on the front of the current bunch
	    mk_ww_arg_processing (unpk_args ++ args) (cmpnt_infos ++ infos) new_max_extra_args
			`thenUsMaybe` \ (wrap_rest, work_args_info, work_rest) ->

	    returnSUs (Just (
	      -- wrapper: unpack the value
	      \ hole -> mk_unpk_case arg unpk_args
			    data_con arg_tycon
			    (wrap_rest hole),

	      -- worker: expect the unpacked value;
	      -- reconstruct the orig value with a "let"
	      work_args_info,
	      \ hole -> work_rest (mk_pk_let arg data_con tycon_arg_tys unpk_args hole)
	    ))
    --)
  where
    arg_ty = getIdUniType arg

    new_max_extra_args
      = max_extra_args 
	+ 1			    -- We won't pass the original arg now
	- nonAbsentArgs cmpnt_infos -- But we will pass an arg for each cmpt

    mk_unpk_case arg unpk_args boxing_con boxing_tycon body
      = CoCase (CoVar arg) (
	  CoAlgAlts [(boxing_con, unpk_args, body)]
	  CoNoDefault
	)

    mk_pk_let arg boxing_con con_tys unpk_args body
      = CoLet (CoNonRec arg (CoCon boxing_con con_tys [CoVarAtom a | a <- unpk_args]))
	      body

mk_ww_arg_processing (arg : args) (arg_demand : infos) max_extra_args
  | otherwise
  = 	-- For all others at the moment, we just
	-- pass them to the worker unchanged.
    --pprTrace "Other; num_wrkr_args=" (ppCat [ppInt num_wrkr_args, ppStr ";arg=", ppr PprDebug arg, ppr PprDebug arg_demand]) (
    
	-- Finish args to the right...
    mk_ww_arg_processing args infos max_extra_args
			`thenUsMaybe` \ (wrap_rest, work_args_info, work_rest) ->
    
    returnSUs (Just (
	      -- wrapper:
	      \ hole -> wrap_rest (CoApp hole (CoVarAtom arg)),
    
	      -- worker:
	      (arg, arg_demand) : work_args_info,
	      \ hole -> work_rest hole
    )) 
    --)
\end{code}

%************************************************************************
%*									*
\subsection[monad-WwLib]{Simple monad for worker/wrapper}
%*									*
%************************************************************************

In this monad, we thread a @UniqueSupply@, and we carry a
@GlobalSwitch@-lookup function downwards.

\begin{code}
type WwM result
  =  SplitUniqSupply
  -> (GlobalSwitch -> Bool)
  -> result

#ifdef __GLASGOW_HASKELL__
{-# INLINE thenWw #-}
{-# INLINE returnWw #-}
#endif

returnWw :: a -> WwM a
thenWw	 :: WwM a -> (a -> WwM b) -> WwM b
mapWw	 :: (a -> WwM b) -> [a] -> WwM [b]

returnWw expr ns sw = expr

thenWw m k us sw_chk
  = case splitUniqSupply us	of { (s1, s2) ->
    case (m s1 sw_chk)	    	of { m_res ->
    k m_res s2 sw_chk }}

mapWw f []     = returnWw []
mapWw f (x:xs)
  = f x		`thenWw` \ x'  ->
    mapWw f xs	`thenWw` \ xs' ->
    returnWw (x':xs')
\end{code}

\begin{code}
getUniqueWw :: WwM Unique
uniqSMtoWwM :: SUniqSM a -> WwM a

getUniqueWw us sw_chk = getSUnique us

uniqSMtoWwM u_obj us sw_chk = u_obj us

thenUsMaybe :: SUniqSM (Maybe a) -> (a -> SUniqSM (Maybe b)) -> SUniqSM (Maybe b)
thenUsMaybe m k
  = m	`thenSUs` \ result ->
    case result of
      Nothing -> returnSUs Nothing
      Just x  -> k x
\end{code}
