%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
%************************************************************************
%*									*
\section[CoreToStg]{Converting core syntax to STG syntax}
%*									*
%************************************************************************

Convert a @CoreSyntax@ program to a @StgSyntax@ program.

\begin{code}
module CoreToStg ( topCoreBindsToStg ) where

#include "HsVersions.h"

import CoreSyn		-- input
import StgSyn		-- output

import CoreUtils	( coreExprType )
import SimplUtils	( findDefault )
import CostCentre	( noCCS )
import Id		( Id, mkSysLocal, idType,
			  externallyVisibleId, setIdUnique, idName, getIdDemandInfo
			)
import Var		( Var, varType, modifyIdInfo )
import IdInfo		( setDemandInfo )
import UsageSPUtils     ( primOpUsgTys )
import DataCon		( DataCon, dataConName, dataConId )
import Name	        ( Name, nameModule, isLocallyDefinedName )
import Module		( isDynamicModule )
import Const	        ( Con(..), Literal, isLitLitLit )
import VarEnv
import Const		( Con(..), isWHNFCon, Literal(..) )
import PrimOp		( PrimOp(..), primOpUsg )
import Type		( isUnLiftedType, isUnboxedTupleType, Type, splitFunTy_maybe,
                          UsageAnn(..), tyUsg, applyTy )
import TysPrim		( intPrimTy )
import Demand
import Unique		( Unique, Uniquable(..) )
import UniqSupply	-- all of it, really
import Util
import Maybes
import Outputable
\end{code}


	***************  OVERVIEW   *********************


The business of this pass is to convert Core to Stg.  On the way:

* We discard type lambdas and applications. In so doing we discard
  "trivial" bindings such as
	x = y t1 t2
  where t1, t2 are types

* We don't pin on correct arities any more, because they can be mucked up
  by the lambda lifter.  In particular, the lambda lifter can take a local
  letrec-bound variable and make it a lambda argument, which shouldn't have
  an arity.  So SetStgVarInfo sets arities now.

* We do *not* pin on the correct free/live var info; that's done later.
  Instead we use bOGUS_LVS and _FVS as a placeholder.

[Quite a bit of stuff that used to be here has moved 
 to tidyCorePgm (SimplCore.lhs) SLPJ Nov 96]


%************************************************************************
%*									*
\subsection[coreToStg-programs]{Converting a core program and core bindings}
%*									*
%************************************************************************

March 98: We keep a small environment to give all locally bound
Names new unique ids, since the code generator assumes that binders
are unique across a module. (Simplifier doesn't maintain this
invariant any longer.)

A binder to be floated out becomes an @StgFloatBind@.

\begin{code}
type StgEnv = IdEnv Id

data StgFloatBind = StgFloatBind Id StgExpr RhsDemand
\end{code}

A @RhsDemand@ gives the demand on an RHS: strict (@isStrictDem@) and
thus case-bound, or if let-bound, at most once (@isOnceDem@) or
otherwise.

\begin{code}
data RhsDemand  = RhsDemand { isStrictDem :: Bool,  -- True => used at least once
                              isOnceDem   :: Bool   -- True => used at most once
                            }

tyDem :: Type -> RhsDemand
-- derive RhsDemand (assuming let-binding)
tyDem ty = case tyUsg ty of
             UsOnce  -> RhsDemand False True
             UsMany  -> RhsDemand False False
             UsVar _ -> pprPanic "CoreToStg.tyDem: UsVar unexpected:" $ ppr ty

bdrDem :: Var -> RhsDemand
bdrDem = tyDem . varType

safeDem, onceDem :: RhsDemand
safeDem = RhsDemand False False  -- always safe to use this
onceDem = RhsDemand False True   -- used at most once
\end{code}

No free/live variable information is pinned on in this pass; it's added
later.  For this pass
we use @bOGUS_LVs@ and @bOGUS_FVs@ as placeholders.

\begin{code}
bOGUS_LVs :: StgLiveVars
bOGUS_LVs = panic "bOGUS_LVs" -- emptyUniqSet (used when pprTracing)

bOGUS_FVs :: [Id]
bOGUS_FVs = panic "bOGUS_FVs" -- [] (ditto)
\end{code}

\begin{code}
topCoreBindsToStg :: UniqSupply	-- name supply
		  -> [CoreBind]	-- input
		  -> [StgBinding]	-- output

topCoreBindsToStg us core_binds
  = initUs_ us (coreBindsToStg emptyVarEnv core_binds)
  where
    coreBindsToStg :: StgEnv -> [CoreBind] -> UniqSM [StgBinding]

    coreBindsToStg env [] = returnUs []
    coreBindsToStg env (b:bs)
      = coreBindToStg  env b		`thenUs` \ (new_b, new_env) ->
    	coreBindsToStg new_env bs 	`thenUs` \ new_bs ->
    	returnUs (new_b ++ new_bs)
\end{code}

%************************************************************************
%*									*
\subsection[coreToStg-binds]{Converting bindings}
%*									*
%************************************************************************

\begin{code}
coreBindToStg :: StgEnv
	      -> CoreBind
	      -> UniqSM ([StgBinding],	-- Empty or singleton
		    	 StgEnv)	-- Floats

coreBindToStg env (NonRec binder rhs)
  = coreRhsToStg env rhs (bdrDem binder) `thenUs` \ stg_rhs ->
    newLocalId env binder	         `thenUs` \ (new_env, new_binder) ->
    returnUs ([StgNonRec new_binder stg_rhs], new_env)

coreBindToStg env (Rec pairs)
  = newLocalIds env binders		 `thenUs` \ (env', binders') ->
    mapUs (\ (bdr,rhs) -> coreRhsToStg env' rhs (bdrDem bdr) )
          pairs                          `thenUs` \ stg_rhss ->
    returnUs ([StgRec (binders' `zip` stg_rhss)], env')
  where
    (binders, rhss) = unzip pairs
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-rhss]{Converting right hand sides}
%*									*
%************************************************************************

\begin{code}
coreRhsToStg :: StgEnv -> CoreExpr -> RhsDemand -> UniqSM StgRhs

coreRhsToStg env core_rhs dem
  = coreExprToStg env core_rhs dem  `thenUs` \ stg_expr ->
    returnUs (exprToRhs dem stg_expr)

exprToRhs dem (StgLet (StgNonRec var1 rhs) (StgApp var2 []))
  | var1 == var2 
  = rhs
	-- This curious stuff is to unravel what a lambda turns into
	-- We have to do it this way, rather than spot a lambda in the
	-- incoming rhs.  Why?  Because trivial bindings might conceal
	-- what the rhs is actually like.

{-
  We reject the following candidates for 'static constructor'dom:
  
    - any dcon that takes a lit-lit as an arg.
    - [Win32 DLLs only]: any dcon that is (or takes as arg)
      that's living in a DLL.

  These constraints are necessary to ensure that the code
  generated in the end for the static constructors, which
  live in the data segment, remain valid - i.e., it has to
  be constant. For obvious reasons, that's hard to guarantee
  with lit-lits. The second case of a constructor referring
  to static closures hiding out in some DLL is an artifact
  of the way Win32 DLLs handle global DLL variables. A (data)
  symbol exported from a DLL  has to be accessed through a
  level of indirection at the site of use, so whereas

     extern StgClosure y_closure;
     extern StgClosure z_closure;
     x = { ..., &y_closure, &z_closure };

  is legal when the symbols are in scope at link-time, it is
  not when y_closure is in a DLL. So, any potential static
  closures that refers to stuff that's residing in a DLL
  will be put in an (updateable) thunk instead.

  An alternative strategy is to support the generation of
  constructors (ala C++ static class constructors) which will
  then be run at load time to fix up static closures.
-}
exprToRhs dem (StgCon (DataCon con) args _)
  | not is_dynamic  &&
    all  (not.is_lit_lit) args  = StgRhsCon noCCS con args
 where
  is_dynamic = isDynCon con || any (isDynArg) args

  is_lit_lit (StgVarArg _) = False
  is_lit_lit (StgConArg x) =
     case x of
       Literal l -> isLitLitLit l
       _         -> False

exprToRhs dem expr
	= StgRhsClosure noCCS		-- No cost centre (ToDo?)
		        stgArgOcc	-- safe
			noSRT		-- figure out later
			bOGUS_FVs
			(if isOnceDem dem then SingleEntry else Updatable)
			[]
			expr

isDynCon :: DataCon -> Bool
isDynCon con = isDynName (dataConName con)

isDynArg :: StgArg -> Bool
isDynArg (StgVarArg v)   = isDynName (idName v)
isDynArg (StgConArg con) =
  case con of
    DataCon dc -> isDynCon dc
    Literal l  -> isLitLitLit l
    _          -> False

isDynName :: Name -> Bool
isDynName nm = 
      not (isLocallyDefinedName nm) && 
      isDynamicModule (nameModule nm)


\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-atoms{Converting atoms}
%*									*
%************************************************************************

\begin{code}
coreArgsToStg :: StgEnv -> [(CoreArg,RhsDemand)] -> UniqSM ([StgFloatBind], [StgArg])
-- arguments are all value arguments (tyargs already removed), paired with their demand

coreArgsToStg env []
  = returnUs ([], [])

coreArgsToStg env (ad:ads)
  = coreArgToStg env ad	        `thenUs` \ (bs1, a') ->
    coreArgsToStg env ads       `thenUs` \ (bs2, as') ->
    returnUs (bs1 ++ bs2, a' : as')

-- This is where we arrange that a non-trivial argument is let-bound

coreArgToStg :: StgEnv -> (CoreArg,RhsDemand) -> UniqSM ([StgFloatBind], StgArg)

coreArgToStg env (arg,dem)
  = let
        ty   = coreExprType arg
        dem' = if isUnLiftedType ty  -- if it's unlifted, it's definitely strict
               then dem { isStrictDem = True }
               else dem
    in
    coreExprToStgFloat env arg dem'  `thenUs` \ (binds, arg') ->
    case (binds, arg') of
	([], StgCon con [] _) | isWHNFCon con -> returnUs ([], StgConArg con)
	([], StgApp v [])		      -> returnUs ([], StgVarArg v)

	-- A non-trivial argument: we must let (or case-bind)
	-- We don't do the case part here... we leave that to mkStgBinds

	-- Further complication: if we're converting this binding into
	-- a case,  then try to avoid generating any case-of-case
	-- expressions by pulling out the floats.
	(_, other) ->
		 newStgVar ty	`thenUs` \ v ->
		 if isStrictDem dem'
		   then returnUs (binds ++ [StgFloatBind v arg' dem'], StgVarArg v)
		   else returnUs ([StgFloatBind v (mkStgBinds binds arg') dem'], StgVarArg v)
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-exprs]{Converting core expressions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg :: StgEnv -> CoreExpr -> RhsDemand -> UniqSM StgExpr

coreExprToStg env (Var var) dem
  = returnUs (StgApp (stgLookup env var) [])

\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-lambdas]{Lambda abstractions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg env expr@(Lam _ _) dem
  = let
	(binders, body) = collectBinders expr
	id_binders      = filter isId binders
        body_dem        = trace "coreExprToStg: approximating body_dem in Lam"
                          safeDem
    in
    newLocalIds env id_binders		`thenUs` \ (env', binders') ->
    coreExprToStg env' body body_dem    `thenUs` \ stg_body ->

    if null id_binders then -- it was all type/usage binders; tossed
	returnUs stg_body
    else
    case stg_body of

      -- if the body reduced to a lambda too...
      (StgLet (StgNonRec var (StgRhsClosure cc bi srt fvs uf args body))
	      (StgApp var' []))
       | var == var' ->
 	returnUs (StgLet (StgNonRec var 
			    (StgRhsClosure noCCS
				stgArgOcc
				noSRT
				bOGUS_FVs
				ReEntrant
				(binders' ++ args)
				body))
		(StgApp var []))
				    
      other ->

	-- We must let-bind the lambda
	newStgVar (coreExprType expr)	`thenUs` \ var ->
	returnUs
	  (StgLet (StgNonRec var (StgRhsClosure noCCS
				  stgArgOcc
				  noSRT
				  bOGUS_FVs
				  ReEntrant 	-- binders is non-empty
				  binders'
				  stg_body))
	   (StgApp var []))
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-let(rec)]{Let and letrec expressions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg env (Let bind body) dem
  = coreBindToStg env     bind      `thenUs` \ (stg_binds, new_env) ->
    coreExprToStg new_env body dem  `thenUs` \ stg_body ->
    returnUs (foldr StgLet stg_body stg_binds)
\end{code}


%************************************************************************
%*									*
\subsubsection[coreToStg-scc]{SCC expressions}
%*									*
%************************************************************************

Covert core @scc@ expression directly to STG @scc@ expression.
\begin{code}
coreExprToStg env (Note (SCC cc) expr) dem
  = coreExprToStg env expr dem  `thenUs` \ stg_expr ->
    returnUs (StgSCC cc stg_expr)
\end{code}

\begin{code}
coreExprToStg env (Note other_note expr) dem = coreExprToStg env expr dem
\end{code}

The rest are handled by coreExprStgFloat.

\begin{code}
coreExprToStg env expr dem
  = coreExprToStgFloat env expr dem  `thenUs` \ (binds,stg_expr) ->
    returnUs (mkStgBinds binds stg_expr)
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-applications]{Applications}
%*									*
%************************************************************************

\begin{code}
coreExprToStgFloat env expr@(App _ _) dem
  = let
        (fun,rads,_) = collect_args expr
        ads          = reverse rads
    in
    coreArgsToStg env ads		`thenUs` \ (binds, stg_args) ->

	-- Now deal with the function
    case (fun, stg_args) of
      (Var fun_id, _) -> 	-- A function Id, so do an StgApp; it's ok if
				-- there are no arguments.
			    returnUs (binds, 
				   StgApp (stgLookup env fun_id) stg_args)

      (non_var_fun, []) -> 	-- No value args, so recurse into the function
			    ASSERT( null binds )
			    coreExprToStg env non_var_fun dem  `thenUs` \e ->
			    returnUs ([], e)

      other ->	-- A non-variable applied to things; better let-bind it.
		newStgVar (coreExprType fun)	`thenUs` \ fun_id ->
                coreRhsToStg env fun onceDem    `thenUs` \ fun_rhs ->
		returnUs (binds,
			  StgLet (StgNonRec fun_id fun_rhs) $
			  StgApp fun_id stg_args)
  where
	-- Collect arguments and demands (*in reverse order*)
    collect_args :: CoreExpr -> (CoreExpr, [(CoreExpr,RhsDemand)], Type)
    collect_args (App fun (Type tyarg)) = let (the_fun,ads,fun_ty) = collect_args fun
                                          in  (the_fun,ads,applyTy fun_ty tyarg)
    collect_args (App fun arg         ) = let (the_fun,ads,fun_ty) = collect_args fun
                                              (arg_ty,res_ty)      = expectJust "coreExprToStgFloat:collect_args" $
                                                                     splitFunTy_maybe fun_ty
                                          in  (the_fun,(arg,tyDem arg_ty):ads,res_ty)
    collect_args (Note (Coerce ty _) e) = let (the_fun,ads,_     ) = collect_args e
                                          in  (the_fun,ads,ty)
    collect_args (Note InlineCall    e) = collect_args e
    collect_args (Note (TermUsg _)   e) = collect_args e
    collect_args fun                    = (fun,[],coreExprType fun)
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-con]{Constructors}
%*									*
%************************************************************************

For data constructors, the demand on an argument is the demand on the
constructor as a whole (see module UsageSPInf).  For primops, the
demand is derived from the type of the primop.

If usage inference is off, we simply make all bindings updatable for
speed.

\begin{code}
coreExprToStgFloat env expr@(Con con args) dem
  = let 
        args'       = filter isValArg args
        dems'       = case con of
                        Literal _ -> ASSERT( null args' {-'cpp-} )
                                     []
                        DEFAULT   -> panic "coreExprToStgFloat: DEFAULT"
                        DataCon c -> repeat (if isOnceDem dem then onceDem else safeDem)
                        PrimOp  p -> let tyargs      = map (\ (Type ty) -> ty) $
                                                           takeWhile isTypeArg args
                                         (arg_tys,_) = primOpUsgTys p tyargs
                                     in  ASSERT( length arg_tys == length args' {-'cpp-} )
                                         -- primops always fully applied, so == not >=
                                         map tyDem arg_tys
    in
    coreArgsToStg env (zip args' dems')                  `thenUs` \ (binds, stg_atoms) ->
    (case con of  -- must change unique if present
       PrimOp (CCallOp (Right _) a b c) -> getUniqueUs   `thenUs` \ u ->
                                           returnUs (PrimOp (CCallOp (Right u) a b c))
       _                                -> returnUs con)
                                                         `thenUs` \ con' ->
    returnUs (binds, StgCon con' stg_atoms (coreExprType expr))
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-cases]{Case expressions}
%*									*
%************************************************************************

\begin{code}
coreExprToStgFloat env expr@(Case scrut bndr alts) dem
  = coreExprToStgFloat env scrut (bdrDem bndr)	`thenUs` \ (binds, scrut') ->
    newEvaldLocalId env bndr			`thenUs` \ (env', bndr') ->
    alts_to_stg env' (findDefault alts)		`thenUs` \ alts' ->
    returnUs (binds, mkStgCase scrut' bndr' alts')
  where
    scrut_ty  = idType bndr
    prim_case = isUnLiftedType scrut_ty && not (isUnboxedTupleType scrut_ty)

    alts_to_stg env (alts, deflt)
      | prim_case
      = default_to_stg env deflt		`thenUs` \ deflt' ->
	mapUs (prim_alt_to_stg env) alts	`thenUs` \ alts' ->
	returnUs (StgPrimAlts scrut_ty alts' deflt')

      | otherwise
      = default_to_stg env deflt		`thenUs` \ deflt' ->
	mapUs (alg_alt_to_stg env) alts		`thenUs` \ alts' ->
	returnUs (StgAlgAlts scrut_ty alts' deflt')

    alg_alt_to_stg env (DataCon con, bs, rhs)
	  = coreExprToStg env rhs dem   `thenUs` \ stg_rhs ->
	    returnUs (con, filter isId bs, [ True | b <- bs ]{-bogus use mask-}, stg_rhs)
		-- NB the filter isId.  Some of the binders may be
		-- existential type variables, which STG doesn't care about

    prim_alt_to_stg env (Literal lit, args, rhs)
	  = ASSERT( null args )
	    coreExprToStg env rhs dem   `thenUs` \ stg_rhs ->
	    returnUs (lit, stg_rhs)

    default_to_stg env Nothing
      = returnUs StgNoDefault

    default_to_stg env (Just rhs)
      = coreExprToStg env rhs dem   `thenUs` \ stg_rhs ->
	returnUs (StgBindDefault stg_rhs)
		-- The binder is used for prim cases and not otherwise
		-- (hack for old code gen)
\end{code}

\begin{code}
coreExprToStgFloat env expr@(Type _) dem
  = pprPanic "coreExprToStgFloat: tyarg unexpected:" $ ppr expr
\end{code}

\begin{code}
coreExprToStgFloat env expr dem
  = coreExprToStg env expr dem  `thenUs` \stg_expr ->
    returnUs ([], stg_expr)
\end{code}

%************************************************************************
%*									*
\subsection[coreToStg-misc]{Miscellaneous helping functions}
%*									*
%************************************************************************

There's not anything interesting we can ASSERT about \tr{var} if it
isn't in the StgEnv. (WDP 94/06)

\begin{code}
stgLookup :: StgEnv -> Id -> Id
stgLookup env var = case (lookupVarEnv env var) of
		      Nothing  -> var
		      Just var -> var
\end{code}

Invent a fresh @Id@:
\begin{code}
newStgVar :: Type -> UniqSM Id
newStgVar ty
 = getUniqueUs	 		`thenUs` \ uniq ->
   returnUs (mkSysLocal SLIT("stg") uniq ty)
\end{code}

\begin{code}
newLocalId env id
  | externallyVisibleId id
  = returnUs (env, id)

  | otherwise
  =	-- Local binder, give it a new unique Id.
    getUniqueUs			`thenUs` \ uniq ->
    let
      id'     = setIdUnique id uniq
      new_env = extendVarEnv env id id'
    in
    returnUs (new_env, id')

-- we overload the demandInfo field of an Id to indicate whether the Id is definitely
-- evaluated or not (i.e. whether it is a case binder).  This can be used to eliminate
-- some redundant cases (c.f. dataToTag# above).

newEvaldLocalId env id
  = getUniqueUs			`thenUs` \ uniq ->
    let
      id'     = setIdUnique id uniq `modifyIdInfo` setDemandInfo wwStrict
      new_env = extendVarEnv env id id'
    in
    returnUs (new_env, id')

newLocalIds :: StgEnv -> [Id] -> UniqSM (StgEnv, [Id])
newLocalIds env []
  = returnUs (env, [])
newLocalIds env (b:bs)
  = newLocalId env b	`thenUs` \ (env', b') ->
    newLocalIds env' bs	`thenUs` \ (env'', bs') ->
    returnUs (env'', b':bs')
\end{code}


\begin{code}
mkStgBinds :: [StgFloatBind] -> StgExpr -> StgExpr
mkStgBinds binds body = foldr mkStgBind body binds

mkStgBind (StgFloatBind bndr rhs dem) body
  | isUnLiftedType bndr_ty
  = ASSERT( not ((isUnboxedTupleType bndr_ty) && (isStrictDem dem==False)) )
    mkStgCase rhs bndr (StgPrimAlts bndr_ty [] (StgBindDefault body))

  | isStrictDem dem == True    -- case
  = mkStgCase rhs bndr (StgAlgAlts bndr_ty [] (StgBindDefault body))

  | isStrictDem dem == False   -- let
  = StgLet (StgNonRec bndr (exprToRhs dem rhs)) body
  where
    bndr_ty = idType bndr

mkStgCase (StgLet bind expr) bndr alts
  = StgLet bind (mkStgCase expr bndr alts)
mkStgCase scrut bndr alts
  = StgCase scrut bOGUS_LVs bOGUS_LVs bndr noSRT alts
\end{code}
