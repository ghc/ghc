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
import Id		( Id, mkSysLocal, idType, getIdStrictness, idUnique, isExportedId, mkVanillaId,
			  externallyVisibleId, setIdUnique, idName, getIdDemandInfo, setIdType
			)
import Var		( Var, varType, modifyIdInfo )
import IdInfo		( setDemandInfo, StrictnessInfo(..) )
import UsageSPUtils     ( primOpUsgTys )
import DataCon		( DataCon, dataConName, dataConId )
import Demand		( Demand, isStrict, wwStrict, wwLazy )
import Name	        ( Name, nameModule, isLocallyDefinedName, setNameUnique )
import Module		( isDynamicModule )
import Const	        ( Con(..), Literal(..), isLitLitLit, conStrictness, isWHNFCon )
import VarEnv
import PrimOp		( PrimOp(..), primOpUsg, primOpSig )
import Type		( isUnLiftedType, isUnboxedTupleType, Type, splitFunTy_maybe,
                          UsageAnn(..), tyUsg, applyTy, mkUsgTy, repType, seqType )
import TysPrim		( intPrimTy )
import UniqSupply	-- all of it, really
import Util		( lengthExceeds )
import BasicTypes	( TopLevelFlag(..), isNotTopLevel )
import CmdLineOpts	( opt_D_verbose_stg2stg, opt_UsageSPOn )
import UniqSet		( emptyUniqSet )
import Maybes
import Outputable
\end{code}


	*************************************************
	***************  OVERVIEW   *********************
	*************************************************


The business of this pass is to convert Core to Stg.  On the way it
does some important transformations:

1.  We discard type lambdas and applications. In so doing we discard
    "trivial" bindings such as
	x = y t1 t2
    where t1, t2 are types

2.  We get the program into "A-normal form".  In particular:

	f E	   ==>  let x = E in f x
		OR ==>  case E of x -> f x

    where E is a non-trivial expression.
    Which transformation is used depends on whether f is strict or not.
    [Previously the transformation to case used to be done by the
     simplifier, but it's better done here.  It does mean that f needs
     to have its strictness info correct!.]

    Similarly, convert any unboxed let's into cases.
    [I'm experimenting with leaving 'ok-for-speculation' rhss in let-form
     right up to this point.]

3.  We clone all local binders.  The code generator uses the uniques to
    name chunks of code for thunks, so it's important that the names used
    are globally unique, not simply not-in-scope, which is all that 
    the simplifier ensures.


NOTE THAT:

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

data StgFloatBind = NoBindF
		  | RecF [(Id, StgRhs)]
		  | NonRecF 
			Id
			StgExpr		-- *Can* be a StgLam
			RhsDemand
			[StgFloatBind]

-- The interesting one is the NonRecF
-- 	NonRecF x rhs demand binds
-- means
--	x = let binds in rhs
-- (or possibly case etc if x demand is strict)
-- The binds are kept separate so they can be floated futher
-- if appropriate
\end{code}

A @RhsDemand@ gives the demand on an RHS: strict (@isStrictDem@) and
thus case-bound, or if let-bound, at most once (@isOnceDem@) or
otherwise.

\begin{code}
data RhsDemand  = RhsDemand { isStrictDem :: Bool,  -- True => used at least once
                              isOnceDem   :: Bool   -- True => used at most once
                            }

mkDem :: Demand -> Bool -> RhsDemand
mkDem strict once = RhsDemand (isStrict strict) once

mkDemTy :: Demand -> Type -> RhsDemand
mkDemTy strict ty = RhsDemand (isStrict strict) (isOnceTy ty)

isOnceTy :: Type -> Bool
isOnceTy ty
  =
#ifdef USMANY
    opt_UsageSPOn &&  -- can't expect annotations if -fusagesp is off
#endif
    case tyUsg ty of
      UsOnce   -> True
      UsMany   -> False
      UsVar uv -> pprPanic "CoreToStg: unexpected uvar annot:" (ppr uv)

bdrDem :: Id -> RhsDemand
bdrDem id = mkDem (getIdDemandInfo id) (isOnceTy (idType id))

safeDem, onceDem :: RhsDemand
safeDem = RhsDemand False False  -- always safe to use this
onceDem = RhsDemand False True   -- used at most once
\end{code}

No free/live variable information is pinned on in this pass; it's added
later.  For this pass
we use @bOGUS_LVs@ and @bOGUS_FVs@ as placeholders.

When printing out the Stg we need non-bottom values in these
locations.

\begin{code}
bOGUS_LVs :: StgLiveVars
bOGUS_LVs | opt_D_verbose_stg2stg = emptyUniqSet
	  | otherwise =panic "bOGUS_LVs"

bOGUS_FVs :: [Id]
bOGUS_FVs | opt_D_verbose_stg2stg = [] 
	  | otherwise = panic "bOGUS_FVs"
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
      = coreBindToStg  TopLevel env b	`thenUs` \ (bind_spec, new_env) ->
    	coreBindsToStg new_env bs 	`thenUs` \ new_bs ->
	case bind_spec of
	  NonRecF bndr rhs dem floats 
		-> ASSERT2( not (isStrictDem dem) && 
			    not (isUnLiftedType (idType bndr)),
			    ppr b )		-- No top-level cases!

		   mkStgBinds floats rhs	`thenUs` \ new_rhs ->
		   returnUs (StgNonRec bndr (exprToRhs dem TopLevel new_rhs)
			     : new_bs)
					-- Keep all the floats inside...
					-- Some might be cases etc
					-- We might want to revisit this decision

	  RecF prs -> returnUs (StgRec prs : new_bs)
	  NoBindF  -> pprTrace "topCoreBindsToStg" (ppr b) $
		      returnUs new_bs
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-binds]{Converting bindings}
%*									*
%************************************************************************

\begin{code}
coreBindToStg :: TopLevelFlag -> StgEnv -> CoreBind -> UniqSM (StgFloatBind, StgEnv)

coreBindToStg top_lev env (NonRec binder rhs)
  = coreExprToStgFloat env rhs dem			`thenUs` \ (floats, stg_rhs) ->
    case (floats, stg_rhs) of
	([], StgApp var []) | not (isExportedId binder)
		     -> returnUs (NoBindF, extendVarEnv env binder var)
		-- A trivial binding let x = y in ...
		-- can arise if postSimplExpr floats a NoRep literal out
		-- so it seems sensible to deal with it well.
		-- But we don't want to discard exported things.  They can
		-- occur; e.g. an exported user binding f = g

	other -> newLocalId top_lev env binder		`thenUs` \ (new_env, new_binder) ->
		 returnUs (NonRecF new_binder stg_rhs dem floats, new_env)
  where
    dem = bdrDem binder

coreBindToStg top_lev env (Rec pairs)
  = newLocalIds top_lev env binders	`thenUs` \ (env', binders') ->
    mapUs (do_rhs env') pairs		`thenUs` \ stg_rhss ->
    returnUs (RecF (binders' `zip` stg_rhss), env')
  where
    binders = map fst pairs
    do_rhs env (bndr,rhs) = coreExprToStgFloat env rhs dem	`thenUs` \ (floats, stg_expr) ->
			    mkStgBinds floats stg_expr		`thenUs` \ stg_expr' ->
				-- NB: stg_expr' might still be a StgLam (and we want that)
			    returnUs (exprToRhs dem top_lev stg_expr')
			  where
			    dem = bdrDem bndr
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-rhss]{Converting right hand sides}
%*									*
%************************************************************************

\begin{code}
exprToRhs :: RhsDemand -> TopLevelFlag -> StgExpr -> StgRhs
exprToRhs dem _ (StgLam _ bndrs body)
  = ASSERT( not (null bndrs) )
    StgRhsClosure noCCS
		  stgArgOcc
		  noSRT
		  bOGUS_FVs
	  	  ReEntrant 	-- binders is non-empty
		  bndrs
		  body

{-
  We reject the following candidates for 'static constructor'dom:
  
    - any dcon that takes a lit-lit as an arg.
    - [Win32 DLLs only]: any dcon that resides in a DLL
      (or takes as arg something that is.)

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
exprToRhs dem toplev (StgCon (DataCon con) args _)
  | isNotTopLevel toplev ||
    (not is_dynamic  &&
     all  (not.is_lit_lit) args)  = StgRhsCon noCCS con args
 where
  is_dynamic = isDynCon con || any (isDynArg) args

  is_lit_lit (StgVarArg _) = False
  is_lit_lit (StgConArg x) =
     case x of
       Literal l -> isLitLitLit l
       _         -> False

exprToRhs dem _ expr
  = upd `seq` 
    StgRhsClosure	noCCS		-- No cost centre (ToDo?)
		  	stgArgOcc	-- safe
			noSRT		-- figure out later
			bOGUS_FVs
			upd
			[]
			expr
  where
    upd = if isOnceDem dem then SingleEntry else Updatable
				-- HA!  Paydirt for "dem"

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
-- Arguments are all value arguments (tyargs already removed), paired with their demand

coreArgsToStg env []
  = returnUs ([], [])

coreArgsToStg env (ad:ads)
  = coreArgToStg env ad	        `thenUs` \ (bs1, a') ->
    coreArgsToStg env ads       `thenUs` \ (bs2, as') ->
    returnUs (bs1 ++ bs2, a' : as')


coreArgToStg :: StgEnv -> (CoreArg,RhsDemand) -> UniqSM ([StgFloatBind], StgArg)
-- This is where we arrange that a non-trivial argument is let-bound

coreArgToStg env (arg,dem)
  = coreExprToStgFloat env arg dem		`thenUs` \ (floats, arg') ->
    case arg' of
	StgCon con [] _ -> returnUs (floats, StgConArg con)
	StgApp v []	-> returnUs (floats, StgVarArg v)
	other		-> newStgVar arg_ty	`thenUs` \ v ->
			   returnUs ([NonRecF v arg' dem floats], StgVarArg v)
  where
    arg_ty = coreExprType arg
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-exprs]{Converting core expressions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg :: StgEnv -> CoreExpr -> RhsDemand -> UniqSM StgExpr
coreExprToStg env expr dem
  = coreExprToStgFloat env expr dem	`thenUs` \ (binds,stg_expr) ->
    mkStgBinds binds stg_expr		`thenUs` \ stg_expr' ->
    deStgLam stg_expr'
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-let(rec)]{Let and letrec expressions}
%*									*
%************************************************************************

\begin{code}
coreExprToStgFloat :: StgEnv -> CoreExpr 
		   -> RhsDemand
		   -> UniqSM ([StgFloatBind], StgExpr)
-- Transform an expression to STG. The demand on the expression is
-- given by RhsDemand, and is solely used ot figure out the usage
-- of constructor args: if the constructor is used once, then so are
-- its arguments.  The strictness info in RhsDemand isn't used.

-- The StgExpr returned *can* be an StgLam
\end{code}

Simple cases first

\begin{code}
coreExprToStgFloat env (Var var) dem
  = returnUs ([], mkStgApp (stgLookup env var) [])

coreExprToStgFloat env (Let bind body) dem
  = coreBindToStg NotTopLevel env bind	`thenUs` \ (new_bind, new_env) ->
    coreExprToStgFloat new_env body dem	`thenUs` \ (floats, stg_body) ->
    returnUs (new_bind:floats, stg_body)
\end{code}

Convert core @scc@ expression directly to STG @scc@ expression.

\begin{code}
coreExprToStgFloat env (Note (SCC cc) expr) dem
  = coreExprToStg env expr dem  `thenUs` \ stg_expr ->
    returnUs ([], StgSCC cc stg_expr)

coreExprToStgFloat env (Note other_note expr) dem
  = coreExprToStgFloat env expr dem
\end{code}

\begin{code}
coreExprToStgFloat env expr@(Type _) dem
  = pprPanic "coreExprToStgFloat: tyarg unexpected:" $ ppr expr
\end{code}


%************************************************************************
%*									*
\subsubsection[coreToStg-lambdas]{Lambda abstractions}
%*									*
%************************************************************************

\begin{code}
coreExprToStgFloat env expr@(Lam _ _) dem
  = let
	expr_ty		= coreExprType expr
	(binders, body) = collectBinders expr
	id_binders      = filter isId binders
        body_dem        = trace "coreExprToStg: approximating body_dem in Lam"
                          safeDem
    in
    if null id_binders then	-- It was all type/usage binders; tossed
	coreExprToStgFloat env body dem
    else
	-- At least some value binders
    newLocalIds NotTopLevel env id_binders	`thenUs` \ (env', binders') ->
    coreExprToStgFloat env' body body_dem	`thenUs` \ (floats, stg_body) ->
    mkStgBinds floats stg_body			`thenUs` \ stg_body' ->

    case stg_body' of
      StgLam ty lam_bndrs lam_body ->
		-- If the body reduced to a lambda too, join them up
	  returnUs ([], mkStgLam expr_ty (binders' ++ lam_bndrs) lam_body)

      other ->
		-- Body didn't reduce to a lambda, so return one
	  returnUs ([], mkStgLam expr_ty binders' stg_body')
\end{code}


%************************************************************************
%*									*
\subsubsection[coreToStg-applications]{Applications}
%*									*
%************************************************************************

\begin{code}
coreExprToStgFloat env expr@(App _ _) dem
  = let
        (fun,rads,_,ss)       = collect_args expr
        ads                   = reverse rads
	final_ads | null ss   = ads
		  | otherwise = zap ads	-- Too few args to satisfy strictness info
					-- so we have to ignore all the strictness info
					-- e.g. + (error "urk")
					-- Here, we can't evaluate the arg strictly,
					-- because this partial application might be seq'd
    in
    coreArgsToStg env final_ads		`thenUs` \ (arg_floats, stg_args) ->

	-- Now deal with the function
    case (fun, stg_args) of
      (Var fun_id, _) -> 	-- A function Id, so do an StgApp; it's ok if
				-- there are no arguments.
			    returnUs (arg_floats, 
				      mkStgApp (stgLookup env fun_id) stg_args)

      (non_var_fun, []) -> 	-- No value args, so recurse into the function
			    ASSERT( null arg_floats )
			    coreExprToStgFloat env non_var_fun dem

      other ->	-- A non-variable applied to things; better let-bind it.
		newStgVar (coreExprType fun)		`thenUs` \ fun_id ->
                coreExprToStgFloat env fun onceDem	`thenUs` \ (fun_floats, stg_fun) ->
		returnUs (NonRecF fun_id stg_fun onceDem fun_floats : arg_floats,
			  mkStgApp fun_id stg_args)

  where
	-- Collect arguments and demands (*in reverse order*)
	-- collect_args e = (f, args_w_demands, ty, stricts)
	--  => e = f tys args,	(i.e. args are just the value args)
	--     e :: ty
	--     stricts is the leftover demands of e on its further args
	-- If stricts runs out, we zap all the demands in args_w_demands
	-- because partial applications are lazy

    collect_args :: CoreExpr -> (CoreExpr, [(CoreExpr,RhsDemand)], Type, [Demand])

    collect_args (Note (Coerce ty _) e) = let (the_fun,ads,_,ss) = collect_args e
                                          in  (the_fun,ads,ty,ss)
    collect_args (Note InlineCall    e) = collect_args e
    collect_args (Note (TermUsg _)   e) = collect_args e

    collect_args (App fun (Type tyarg)) = let (the_fun,ads,fun_ty,ss) = collect_args fun
                                          in  (the_fun,ads,applyTy fun_ty tyarg,ss)
    collect_args (App fun arg) 
	= (the_fun, (arg, mkDemTy ss1 arg_ty) : ads, res_ty, ss_rest)
	where
	  (ss1, ss_rest) 	     = case ss of 
					 (ss1:ss_rest) -> (ss1, ss_rest)
					 []	       -> (wwLazy, [])
	  (the_fun, ads, fun_ty, ss) = collect_args fun
          (arg_ty, res_ty)           = expectJust "coreExprToStgFloat:collect_args" $
                                       splitFunTy_maybe fun_ty

    collect_args (Var v)
	= (Var v, [], idType v, stricts)
	where
	  stricts = case getIdStrictness v of
			StrictnessInfo demands _ -> demands
			other			 -> repeat wwLazy

    collect_args fun = (fun, [], coreExprType fun, repeat wwLazy)

    -- "zap" nukes the strictness info for a partial application 
    zap ads = [(arg, RhsDemand False once) | (arg, RhsDemand _ once) <- ads]
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-con]{Constructors and primops}
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
	expr_ty     = coreExprType expr
        (stricts,_) = conStrictness con
        onces = case con of
                    DEFAULT   -> panic "coreExprToStgFloat: DEFAULT"
		 
                    Literal _ -> ASSERT( null args' {-'cpp-} ) []
		 
                    DataCon c -> repeat (isOnceDem dem)
		 			-- HA!  This is the sole reason we propagate
		 			-- dem all the way down 
		 
                    PrimOp  p -> let tyargs      = map (\ (Type ty) -> ty) $
                                                       takeWhile isTypeArg args
                                     (arg_tys,_) = primOpUsgTys p tyargs
                                 in  ASSERT( length arg_tys == length args' {-'cpp-} )
                                     -- primops always fully applied, so == not >=
                                     map isOnceTy arg_tys

	dems' = zipWith mkDem stricts onces
        args' = filter isValArg args
    in
    coreArgsToStg env (zip args' dems')                  `thenUs` \ (arg_floats, stg_atoms) ->

	-- YUK YUK: must unique if present
    (case con of
       PrimOp (CCallOp (Right _) a b c) -> getUniqueUs   `thenUs` \ u ->
                                           returnUs (PrimOp (CCallOp (Right u) a b c))
       _                                -> returnUs con
    )                                                     `thenUs` \ con' ->

    returnUs (arg_floats, mkStgCon con' stg_atoms expr_ty)
\end{code}


%************************************************************************
%*									*
\subsubsection[coreToStg-cases]{Case expressions}
%*									*
%************************************************************************

First, two special cases.  We mangle cases involving 
		par# and seq#
inthe scrutinee.

Up to this point, seq# will appear like this:

	  case seq# e of
		0# -> seqError#
		_  -> <stuff>

This code comes from an unfolding for 'seq' in Prelude.hs.
The 0# branch is purely to bamboozle the strictness analyser.
For example, if <stuff> is strict in x, and there was no seqError#
branch, the strictness analyser would conclude that the whole expression
was strict in x, and perhaps evaluate x first -- but that would be a DISASTER.

Now that the evaluation order is safe, we translate this into

	  case e of
		_ -> ...

This used to be done in the post-simplification phase, but we need
unfoldings involving seq# to appear unmangled in the interface file,
hence we do this mangling here.

Similarly, par# has an unfolding in PrelConc.lhs that makes it show
up like this:

    	case par# e of
    	  0# -> rhs
    	  _  -> parError#


    ==>
    	case par# e of
    	  _ -> rhs

fork# isn't handled like this - it's an explicit IO operation now.
The reason is that fork# returns a ThreadId#, which gets in the
way of the above scheme.  And anyway, IO is the only guaranteed
way to enforce ordering  --SDM.


\begin{code}
coreExprToStgFloat env 
	(Case scrut@(Con (PrimOp SeqOp) [Type ty, e]) bndr alts) dem
  = coreExprToStgFloat env (Case e new_bndr [(DEFAULT,[],default_rhs)]) dem
  where 
    (other_alts, maybe_default) = findDefault alts
    Just default_rhs	        = maybe_default
    new_bndr 			= setIdType bndr ty
	-- NB:  SeqOp :: forall a. a -> Int#
	-- So bndr has type Int# 
	-- But now we are going to scrutinise the SeqOp's argument directly,
	-- so we must change the type of the case binder to match that
	-- of the argument expression e.  We can get this type from the argument
	-- type of the SeqOp.

coreExprToStgFloat env 
	(Case scrut@(Con (PrimOp ParOp) args) bndr alts) dem
  | maybeToBool maybe_default
  = coreExprToStgFloat env scrut (bdrDem bndr)	`thenUs` \ (binds, scrut') ->
    newEvaldLocalId env bndr			`thenUs` \ (env', bndr') ->
    coreExprToStg env' default_rhs dem 		`thenUs` \ default_rhs' ->
    returnUs (binds, mkStgCase scrut' bndr' (StgPrimAlts (idType bndr') [] (StgBindDefault default_rhs')))
  where
    (other_alts, maybe_default) = findDefault alts
    Just default_rhs		= maybe_default
\end{code}

Now for normal case expressions...

\begin{code}
coreExprToStgFloat env (Case scrut bndr alts) dem
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
	returnUs (mkStgPrimAlts scrut_ty alts' deflt')

      | otherwise
      = default_to_stg env deflt		`thenUs` \ deflt' ->
	mapUs (alg_alt_to_stg env) alts		`thenUs` \ alts' ->
	returnUs (mkStgAlgAlts scrut_ty alts' deflt')

    alg_alt_to_stg env (DataCon con, bs, rhs)
	  = newLocalIds NotTopLevel env (filter isId bs)	`thenUs` \ (env', stg_bs) -> 
	    coreExprToStg env' rhs dem  		 	`thenUs` \ stg_rhs ->
	    returnUs (con, stg_bs, [ True | b <- stg_bs ]{-bogus use mask-}, stg_rhs)
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
   seqType ty			`seq`
   returnUs (mkSysLocal SLIT("stg") uniq ty)
\end{code}

\begin{code}
{- 	Now redundant, I believe
-- we overload the demandInfo field of an Id to indicate whether the Id is definitely
-- evaluated or not (i.e. whether it is a case binder).  This can be used to eliminate
-- some redundant cases (c.f. dataToTag# above).

newEvaldLocalId env id
  = getUniqueUs			`thenUs` \ uniq ->
    let
      id'     = modifyIdInfo (`setDemandInfo` wwStrict) (setIdUnique id uniq)
      new_env = extendVarEnv env id id'
    in
    returnUs (new_env, id')
-}

newEvaldLocalId env id = newLocalId NotTopLevel env id

newLocalId TopLevel env id
  -- Don't clone top-level binders.  MkIface relies on their
  -- uniques staying the same, so it can snaffle IdInfo off the
  -- STG ids to put in interface files.	
  = let
      name = idName id
      ty   = idType id
    in
    name		`seq`
    seqType ty		`seq`
    returnUs (env, mkVanillaId name ty)


newLocalId NotTopLevel env id
  =	-- Local binder, give it a new unique Id.
    getUniqueUs			`thenUs` \ uniq ->
    let
      name    = idName id
      ty      = idType id
      new_id  = mkVanillaId (setNameUnique name uniq) ty
      new_env = extendVarEnv env id new_id
    in
    name		`seq`
    seqType ty		`seq`
    returnUs (new_env, new_id)

newLocalIds :: TopLevelFlag -> StgEnv -> [Id] -> UniqSM (StgEnv, [Id])
newLocalIds top_lev env []
  = returnUs (env, [])
newLocalIds top_lev env (b:bs)
  = newLocalId top_lev env b	`thenUs` \ (env', b') ->
    newLocalIds top_lev env' bs	`thenUs` \ (env'', bs') ->
    returnUs (env'', b':bs')
\end{code}


%************************************************************************
%*									*
\subsection{Building STG syn}
%*									*
%************************************************************************

\begin{code}
mkStgAlgAlts  ty alts deflt = seqType ty `seq` StgAlgAlts  ty alts deflt
mkStgPrimAlts ty alts deflt = seqType ty `seq` StgPrimAlts ty alts deflt
mkStgCon con args ty	    = seqType ty `seq` StgCon con args ty
mkStgLam ty bndrs body	    = seqType ty `seq` StgLam ty bndrs body

mkStgApp :: Id -> [StgArg] -> StgExpr
mkStgApp fn args = fn `seq` StgApp fn args
	-- Force the lookup
\end{code}

\begin{code}
-- Stg doesn't have a lambda *expression*, 
deStgLam (StgLam ty bndrs body) = mkStgLamExpr ty bndrs body
deStgLam expr		        = returnUs expr

mkStgLamExpr ty bndrs body
  = ASSERT( not (null bndrs) )
    newStgVar ty		`thenUs` \ fn ->
    returnUs (StgLet (StgNonRec fn lam_closure) (mkStgApp fn []))
  where
    lam_closure = StgRhsClosure noCCS
				stgArgOcc
				noSRT
				bOGUS_FVs
				ReEntrant 	-- binders is non-empty
				bndrs
				body

mkStgBinds :: [StgFloatBind] 
	   -> StgExpr		-- *Can* be a StgLam 
	   -> UniqSM StgExpr	-- *Can* be a StgLam 

mkStgBinds []     body = returnUs body
mkStgBinds (b:bs) body 
  = deStgLam body		`thenUs` \ body' ->
    go (b:bs) body'
  where
    go []     body = returnUs body
    go (b:bs) body = go bs body 	`thenUs` \ body' ->
		     mkStgBind  b body'

-- The 'body' arg of mkStgBind can't be a StgLam
mkStgBind NoBindF    body = returnUs body
mkStgBind (RecF prs) body = returnUs (StgLet (StgRec prs) body)

mkStgBind (NonRecF bndr rhs dem floats) body
#ifdef DEBUG
	-- We shouldn't get let or case of the form v=w
  = case rhs of
	StgApp v [] -> pprTrace "mkStgLet" (ppr bndr <+> ppr v)
		       (mk_stg_let bndr rhs dem floats body)
	other	    ->  mk_stg_let bndr rhs dem floats body

mk_stg_let bndr rhs dem floats body
#endif
  | isUnLiftedType bndr_rep_ty			-- Use a case/PrimAlts
  = ASSERT( not (isUnboxedTupleType bndr_rep_ty) )
    mkStgBinds floats $
    mkStgCase rhs bndr (StgPrimAlts bndr_rep_ty [] (StgBindDefault body))

  | is_whnf
  = if is_strict then
	-- Strict let with WHNF rhs
	mkStgBinds floats $
	StgLet (StgNonRec bndr (exprToRhs dem NotTopLevel rhs)) body
    else
	-- Lazy let with WHNF rhs; float until we find a strict binding
	let
	    (floats_out, floats_in) = splitFloats floats
	in
	mkStgBinds floats_in rhs	`thenUs` \ new_rhs ->
	mkStgBinds floats_out $
	StgLet (StgNonRec bndr (exprToRhs dem NotTopLevel new_rhs)) body

  | otherwise 	-- Not WHNF
  = if is_strict then
	-- Strict let with non-WHNF rhs
	mkStgBinds floats $
	mkStgCase rhs bndr (StgAlgAlts bndr_rep_ty [] (StgBindDefault body))
    else
	-- Lazy let with non-WHNF rhs, so keep the floats in the RHS
	mkStgBinds floats rhs		`thenUs` \ new_rhs ->
	returnUs (StgLet (StgNonRec bndr (exprToRhs dem NotTopLevel new_rhs)) body)
	
  where
    bndr_rep_ty = repType (idType bndr)
    is_strict   = isStrictDem dem
    is_whnf     = case rhs of
		    StgCon _ _ _ -> True
		    StgLam _ _ _ -> True
		    other	 -> False

-- Split at the first strict binding
splitFloats fs@(NonRecF _ _ dem _ : _) 
  | isStrictDem dem = ([], fs)

splitFloats (f : fs) = case splitFloats fs of
		  	     (fs_out, fs_in) -> (f : fs_out, fs_in)

splitFloats [] = ([], [])


mkStgCase scrut bndr alts
  = ASSERT( case scrut of { StgLam _ _ _ -> False; other -> True } )
	-- We should never find 
	--	case (\x->e) of { ... }
	-- The simplifier eliminates such things
    StgCase scrut bOGUS_LVs bOGUS_LVs bndr noSRT alts
\end{code}
