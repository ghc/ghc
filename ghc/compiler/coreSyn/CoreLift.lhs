%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[CoreLift]{Lifts unboxed bindings and any references to them}

\begin{code}
#include "HsVersions.h"

module CoreLift (
	liftCoreBindings,

	mkLiftedId,
	liftExpr,
	bindUnlift,
	applyBindUnlifts,
	isUnboxedButNotState

    ) where

import Ubiq{-uitous-}

import CoreSyn
import CoreUtils	( coreExprType )
import Id		( idType, mkSysLocal,
			  nullIdEnv, growIdEnvList, lookupIdEnv, IdEnv(..),
			  GenId{-instances-}
			)
import Name		( isLocallyDefined, getSrcLoc )
import TyCon		( isBoxedTyCon, TyCon{-instance-} )
import Type		( maybeAppDataTyConExpandingDicts, eqTy )
import TysPrim		( statePrimTyCon )
import TysWiredIn	( liftDataCon, mkLiftTy )
import UniqSupply	( getUnique, getUniques, splitUniqSupply, UniqSupply )
import Util		( zipEqual, zipWithEqual, assertPanic, panic )

infixr 9 `thenL`

updateIdType = panic "CoreLift.updateIdType"
\end{code}

%************************************************************************
%*									*
\subsection{``lift'' for various constructs}
%*									*
%************************************************************************

@liftCoreBindings@ is the top-level interface function.

\begin{code}
liftCoreBindings :: UniqSupply	-- unique supply
		 -> [CoreBinding]	-- unlifted bindings
		 -> [CoreBinding]	-- lifted bindings

liftCoreBindings us binds
  = initL (lift_top_binds binds) us
  where
    lift_top_binds [] = returnL []

    lift_top_binds (b:bs)
      = liftBindAndScope True b (
	  lift_top_binds bs `thenL` \ bs ->
	  returnL (ItsABinds bs)
	) 			`thenL` \ (b, ItsABinds bs) ->
	returnL (b:bs)


-----------------------
liftBindAndScope :: Bool		-- top level ?
		 -> CoreBinding		-- As yet unprocessed
		 -> LiftM BindsOrExpr	-- Do the scope of the bindings
		 -> LiftM (CoreBinding,	-- Processed
		 	   BindsOrExpr)

liftBindAndScope top_lev bind scopeM
  = liftBinders top_lev bind (
      liftCoreBind bind	`thenL` \ bind ->
      scopeM 		`thenL` \ bindsorexpr ->
      returnL (bind, bindsorexpr)
    )

-----------------------
liftCoreArg :: CoreArg -> LiftM (CoreArg, CoreExpr -> CoreExpr)

liftCoreArg arg@(TyArg     _) = returnL (arg, id)
liftCoreArg arg@(UsageArg  _) = returnL (arg, id)
liftCoreArg arg@(LitArg    _) = returnL (arg, id)
liftCoreArg arg@(VarArg v)
 = isLiftedId v			`thenL` \ lifted ->
    case lifted of
	Nothing -> returnL (arg, id)

	Just (lifted, unlifted) ->
	    returnL (VarArg unlifted, bindUnlift lifted unlifted)


-----------------------
liftCoreBind :: CoreBinding -> LiftM CoreBinding

liftCoreBind (NonRec b rhs)
  = liftOneBind (b,rhs)		`thenL` \ (b,rhs) ->
    returnL (NonRec b rhs)

liftCoreBind (Rec pairs)
  = mapL liftOneBind pairs	`thenL` \ pairs ->
    returnL (Rec pairs)

-----------------------
liftOneBind (binder,rhs)
  = liftCoreExpr rhs    	`thenL` \ rhs ->
    isLiftedId binder		`thenL` \ lifted ->
    case lifted of
	Just (lifted, unlifted) ->
	    returnL (lifted, liftExpr unlifted rhs)
	Nothing ->
	    returnL (binder, rhs)

-----------------------
liftCoreExpr :: CoreExpr -> LiftM CoreExpr

liftCoreExpr expr@(Var var)
  = isLiftedId var		`thenL` \ lifted ->
    case lifted of
	Nothing -> returnL expr
	Just (lifted, unlifted) ->
	    returnL (bindUnlift lifted unlifted (Var unlifted))

liftCoreExpr expr@(Lit lit) = returnL expr

liftCoreExpr (SCC label expr)
  = liftCoreExpr expr		`thenL` \ expr ->
    returnL (SCC label expr)

liftCoreExpr (Coerce coerce ty expr)
  = liftCoreExpr expr		`thenL` \ expr ->
    returnL (Coerce coerce ty expr) -- ToDo:right?:Coerce

liftCoreExpr (Let (NonRec binder rhs) body) -- special case: no lifting
  = liftCoreExpr rhs	`thenL` \ rhs ->
    liftCoreExpr body	`thenL` \ body ->
    returnL (mkCoLetUnboxedToCase (NonRec binder rhs) body)

liftCoreExpr (Let bind body)	-- general case
  = liftBindAndScope False bind (
      liftCoreExpr body	`thenL` \ body ->
      returnL (ItsAnExpr body)
    )				`thenL` \ (bind, ItsAnExpr body) ->
    returnL (Let bind body)

liftCoreExpr (Con con args)
  = mapAndUnzipL liftCoreArg args	`thenL` \ (args, unlifts) ->
    returnL (applyBindUnlifts unlifts (Con con args))

liftCoreExpr (Prim op args)
  = mapAndUnzipL liftCoreArg args	`thenL` \ (args, unlifts) ->
    returnL (applyBindUnlifts unlifts (Prim op args))

liftCoreExpr (App fun arg)
  = lift_app fun [arg]
  where
    lift_app (App fun arg) args
      = lift_app fun (arg:args)
    lift_app other_fun args
      = liftCoreExpr other_fun		`thenL` \ other_fun ->
	mapAndUnzipL liftCoreArg args	`thenL` \ (args, unlifts) ->
	returnL (applyBindUnlifts unlifts (mkGenApp other_fun args))

liftCoreExpr (Lam binder expr)
  = liftCoreExpr expr		`thenL` \ expr ->
    returnL (Lam binder expr)

liftCoreExpr (Case scrut alts)
 = liftCoreExpr scrut		`thenL` \ scrut ->
   liftCoreAlts alts		`thenL` \ alts ->
   returnL (Case scrut alts)

------------
liftCoreAlts :: CoreCaseAlts -> LiftM CoreCaseAlts

liftCoreAlts (AlgAlts alg_alts deflt)
 = mapL liftAlgAlt alg_alts	`thenL` \ alg_alts ->
   liftDeflt deflt		`thenL` \ deflt ->
   returnL (AlgAlts alg_alts deflt)

liftCoreAlts (PrimAlts prim_alts deflt)
 = mapL liftPrimAlt prim_alts	`thenL` \ prim_alts ->
   liftDeflt deflt		`thenL` \ deflt ->
   returnL (PrimAlts prim_alts deflt)

------------
liftAlgAlt (con,args,rhs)
  = liftCoreExpr rhs		`thenL` \ rhs ->
    returnL (con,args,rhs)

------------
liftPrimAlt (lit,rhs)
  = liftCoreExpr rhs		`thenL` \ rhs ->
    returnL (lit,rhs)

------------
liftDeflt NoDefault
  = returnL NoDefault
liftDeflt (BindDefault binder rhs)
  = liftCoreExpr rhs		`thenL` \ rhs ->
    returnL (BindDefault binder rhs)
\end{code}

%************************************************************************
%*									*
\subsection{Misc functions}
%*									*
%************************************************************************

\begin{code}
type LiftM a
  = IdEnv (Id, Id)	-- lifted Ids are mapped to:
			--   * lifted Id with the same Unique
			--     (top-level bindings must keep their
			--	unique (see TopLevId in Id.lhs))
			--   * unlifted version with a new Unique
    -> UniqSupply	-- unique supply
    -> a		-- result

data BindsOrExpr
  = ItsABinds [CoreBinding]
  | ItsAnExpr CoreExpr

initL m us = m nullIdEnv us

returnL :: a -> LiftM a
returnL r idenv us = r

thenL :: LiftM a -> (a -> LiftM b) -> LiftM b
thenL m k idenv s0
  = case (splitUniqSupply s0)	of { (s1, s2) ->
    case (m idenv s1)		of { r ->
    k r idenv s2 }}


mapL :: (a -> LiftM b) -> [a] -> LiftM [b]
mapL f [] = returnL []
mapL f (x:xs)
  = f x 		`thenL` \ r ->
    mapL f xs		`thenL` \ rs ->
    returnL (r:rs)

mapAndUnzipL  :: (a -> LiftM (b1, b2))	-> [a] -> LiftM ([b1],[b2])
mapAndUnzipL f [] = returnL ([],[])
mapAndUnzipL f (x:xs)
  = f x 		`thenL` \ (r1, r2) ->
    mapAndUnzipL f xs	`thenL` \ (rs1,rs2) ->
    returnL ((r1:rs1),(r2:rs2))

-- liftBinders is only called for top-level or recusive case
liftBinders :: Bool -> CoreBinding -> LiftM thing -> LiftM thing

liftBinders False (NonRec _ _) liftM idenv s0
  = panic "CoreLift:liftBinders"	-- should be caught by special case above

liftBinders top_lev bind liftM idenv s0
  = liftM (growIdEnvList idenv lift_map) s2
  where
    (s1, s2)   = splitUniqSupply s0
    lift_ids   = [ id | id <- bindersOf bind, isUnboxedButNotState (idType id) ]
    lift_uniqs = getUniques (length lift_ids) s1
    lift_map   = zipEqual "liftBinders" lift_ids (zipWithEqual "liftBinders" mkLiftedId lift_ids lift_uniqs)

    -- ToDo: Give warning for recursive bindings involving unboxed values ???

isLiftedId :: Id -> LiftM (Maybe (Id, Id))
isLiftedId id idenv us
  | isLocallyDefined id
     = lookupIdEnv idenv id
  | otherwise	-- ensure all imported ids are lifted
     = if isUnboxedButNotState (idType id)
       then Just (mkLiftedId id (getUnique us))
       else Nothing

mkLiftedId :: Id -> Unique -> (Id,Id)
mkLiftedId id u
  = ASSERT (isUnboxedButNotState unlifted_ty)
    (lifted_id, unlifted_id)
  where
    id_name     = panic "CoreLift.mkLiftedId:id_name" --LATER: getOccName id
    lifted_id   = updateIdType id lifted_ty
    unlifted_id = mkSysLocal id_name u unlifted_ty (getSrcLoc id)

    unlifted_ty = idType id
    lifted_ty   = mkLiftTy unlifted_ty

bindUnlift :: Id -> Id -> CoreExpr -> CoreExpr
bindUnlift vlift vunlift expr
  = ASSERT (isUnboxedButNotState unlift_ty)
    ASSERT (lift_ty `eqTy` mkLiftTy unlift_ty)
    Case (Var vlift)
	   (AlgAlts [(liftDataCon, [vunlift], expr)] NoDefault)
  where
    lift_ty   = idType vlift
    unlift_ty = idType vunlift

liftExpr :: Id -> CoreExpr -> CoreExpr
liftExpr vunlift rhs
  = ASSERT (isUnboxedButNotState unlift_ty)
    ASSERT (rhs_ty `eqTy` unlift_ty)
    Case rhs (PrimAlts []
	(BindDefault vunlift (mkCon liftDataCon [] [unlift_ty] [VarArg vunlift])))
  where
    rhs_ty    = coreExprType rhs
    unlift_ty = idType vunlift


applyBindUnlifts :: [CoreExpr -> CoreExpr] -> CoreExpr -> CoreExpr
applyBindUnlifts []     expr = expr
applyBindUnlifts (f:fs) expr = f (applyBindUnlifts fs expr)

isUnboxedButNotState ty
  = case (maybeAppDataTyConExpandingDicts ty) of
      Nothing -> False
      Just (tycon, _, _) ->
	not (isBoxedTyCon tycon) && not (tycon == statePrimTyCon)
\end{code}
