%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
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
	isUnboxedButNotState,
	
	CoreBinding, PlainCoreBinding(..),
	CoreExpr, PlainCoreExpr(..),
	Id, SplitUniqSupply, Unique
    ) where

IMPORT_Trace
import Pretty

import AbsPrel		( liftDataCon, mkLiftTy )
import TysPrim		( statePrimTyCon ) -- ToDo: get from AbsPrel
import AbsUniType
import Id		( getIdUniType, updateIdType, mkSysLocal, isLocallyDefined )
import IdEnv
import Outputable
import PlainCore
import SplitUniq
import Util

infixr 9 `thenL`

\end{code}

%************************************************************************
%*									*
\subsection{``lift'' for various constructs}
%*									*
%************************************************************************

@liftCoreBindings@ is the top-level interface function.

\begin{code}
liftCoreBindings :: SplitUniqSupply	-- unique supply
		 -> [PlainCoreBinding]	-- unlifted bindings
		 -> [PlainCoreBinding]	-- lifted bindings

liftCoreBindings us binds
  = initL (lift_top_binds binds) us
  where
    lift_top_binds (b:bs)
      = liftBindAndScope True b (
          lift_top_binds bs `thenL` \ bs ->
	  returnL (ItsABinds bs)
        ) 			`thenL` \ (b, ItsABinds bs) ->
	returnL (b:bs)

    lift_top_binds []
      = returnL []
    
liftBindAndScope :: Bool			-- top level ?
		 -> PlainCoreBinding		-- As yet unprocessed
		 -> LiftM BindsOrExpr		-- Do the scope of the bindings
		 -> LiftM (PlainCoreBinding,	-- Processed
		 	   BindsOrExpr)

liftBindAndScope top_lev bind scopeM
  = liftBinders top_lev bind (
      liftCoreBind bind	`thenL` \ bind ->
      scopeM 		`thenL` \ bindsorexpr ->
      returnL (bind, bindsorexpr)
    )


liftCoreAtom :: PlainCoreAtom -> LiftM (PlainCoreAtom, PlainCoreExpr -> PlainCoreExpr)

liftCoreAtom (CoLitAtom lit)
 = returnL (CoLitAtom lit, id)

liftCoreAtom (CoVarAtom v)
 = isLiftedId v			`thenL` \ lifted ->
    case lifted of
	Just (lifted, unlifted) ->
	    returnL (CoVarAtom unlifted, bindUnlift lifted unlifted)
	Nothing ->
            returnL (CoVarAtom v, id)


liftCoreBind :: PlainCoreBinding -> LiftM PlainCoreBinding

liftCoreBind (CoNonRec b rhs)
  = liftOneBind (b,rhs)		`thenL` \ (b,rhs) ->
    returnL (CoNonRec b rhs)

liftCoreBind (CoRec pairs) 
  = mapL liftOneBind pairs	`thenL` \ pairs -> 
    returnL (CoRec pairs)

liftOneBind (binder,rhs)
  = liftCoreExpr rhs    	`thenL` \ rhs ->
    isLiftedId binder		`thenL` \ lifted ->
    case lifted of
	Just (lifted, unlifted) ->
	    returnL (lifted, liftExpr unlifted rhs)
	Nothing ->
            returnL (binder, rhs)

liftCoreExpr :: PlainCoreExpr -> LiftM PlainCoreExpr

liftCoreExpr (CoVar var)
  = isLiftedId var		`thenL` \ lifted ->
    case lifted of
	Just (lifted, unlifted) ->
	    returnL (bindUnlift lifted unlifted (CoVar unlifted))
	Nothing ->
            returnL (CoVar var)

liftCoreExpr (CoLit lit)
  = returnL (CoLit lit)

liftCoreExpr (CoSCC label expr)
  = liftCoreExpr expr		`thenL` \ expr ->
    returnL (CoSCC label expr)

liftCoreExpr (CoLet (CoNonRec binder rhs) body)		-- special case: no lifting
  = liftCoreExpr rhs	`thenL` \ rhs ->
    liftCoreExpr body	`thenL` \ body ->
    returnL (mkCoLetUnboxedToCase (CoNonRec binder rhs) body)

liftCoreExpr (CoLet bind body)	-- general case
  = liftBindAndScope False bind (
      liftCoreExpr body	`thenL` \ body ->
      returnL (ItsAnExpr body)
    )				`thenL` \ (bind, ItsAnExpr body) ->
    returnL (CoLet bind body)

liftCoreExpr (CoCon con tys args)
  = mapAndUnzipL liftCoreAtom args	`thenL` \ (args, unlifts) ->
    returnL (applyBindUnlifts unlifts (CoCon con tys args))

liftCoreExpr (CoPrim op tys args)
  = mapAndUnzipL liftCoreAtom args	`thenL` \ (args, unlifts) ->
    returnL (applyBindUnlifts unlifts (CoPrim op tys args))

liftCoreExpr (CoApp fun arg)
  = lift_app fun [arg]
  where
    lift_app (CoApp fun arg) args
      = lift_app fun (arg:args)
    lift_app other_fun args
      = liftCoreExpr other_fun		`thenL` \ other_fun ->
        mapAndUnzipL liftCoreAtom args	`thenL` \ (args, unlifts) ->
        returnL (applyBindUnlifts unlifts (foldl CoApp other_fun args))

liftCoreExpr (CoTyApp fun ty_arg)
  = liftCoreExpr fun		`thenL` \ fun ->
    returnL (CoTyApp fun ty_arg)

liftCoreExpr (CoLam binders expr)
  = liftCoreExpr expr		`thenL` \ expr ->
    returnL (CoLam binders expr)

liftCoreExpr (CoTyLam tyvar expr)
  = liftCoreExpr expr		`thenL` \ expr ->
    returnL (CoTyLam tyvar expr)

liftCoreExpr (CoCase scrut alts)
 = liftCoreExpr scrut		`thenL` \ scrut ->
   liftCoreAlts alts		`thenL` \ alts ->
   returnL (CoCase scrut alts)


liftCoreAlts :: PlainCoreCaseAlternatives -> LiftM PlainCoreCaseAlternatives

liftCoreAlts (CoAlgAlts alg_alts deflt)
 = mapL liftAlgAlt alg_alts	`thenL` \ alg_alts ->
   liftDeflt deflt		`thenL` \ deflt ->
   returnL (CoAlgAlts alg_alts deflt)

liftCoreAlts (CoPrimAlts prim_alts deflt)
 = mapL liftPrimAlt prim_alts	`thenL` \ prim_alts ->
   liftDeflt deflt		`thenL` \ deflt ->
   returnL (CoPrimAlts prim_alts deflt)


liftAlgAlt (con,args,rhs)
  = liftCoreExpr rhs		`thenL` \ rhs ->
    returnL (con,args,rhs)

liftPrimAlt (lit,rhs)
  = liftCoreExpr rhs		`thenL` \ rhs ->
    returnL (lit,rhs)
   
liftDeflt CoNoDefault
  = returnL CoNoDefault
liftDeflt (CoBindDefault binder rhs)
  = liftCoreExpr rhs		`thenL` \ rhs ->
    returnL (CoBindDefault binder rhs)

\end{code}

%************************************************************************
%*									*
\subsection{Misc functions}
%*									*
%************************************************************************

\begin{code}
type LiftM a = IdEnv (Id, Id)	-- lifted Ids are mapped to:
				--   * lifted Id with the same Unique
				--     (top-level bindings must keep their
				--	unique (see TopLevId in Id.lhs))
				--   * unlifted version with a new Unique
            -> SplitUniqSupply	-- unique supply
	    -> a		-- result

data BindsOrExpr = ItsABinds [PlainCoreBinding]
		 | ItsAnExpr PlainCoreExpr

initL m us
  = m nullIdEnv us

returnL :: a -> LiftM a
returnL r idenv us
  = r

thenL :: LiftM a -> (a -> LiftM b) -> LiftM b
thenL m k idenv s0
  = case splitUniqSupply s0	   of { (s1, s2) ->
    case (m idenv s1) of { r ->
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
liftBinders :: Bool -> PlainCoreBinding -> LiftM thing -> LiftM thing

liftBinders False (CoNonRec _ _) liftM idenv s0
  = error "CoreLift:liftBinders"	-- should be caught by special case above

liftBinders top_lev bind liftM idenv s0
  = liftM (growIdEnvList idenv lift_map) s1
  where
    lift_ids = [ id | id <- bindersOf bind, isUnboxedButNotState (getIdUniType id) ]
    (lift_uniqs, s1) = getSUniquesAndDepleted (length lift_ids) s0
    lift_map = zip lift_ids (zipWith mkLiftedId lift_ids lift_uniqs)

    -- ToDo: Give warning for recursive bindings involving unboxed values ???


isLiftedId :: Id -> LiftM (Maybe (Id, Id))
isLiftedId id idenv us
  | isLocallyDefined id 
     = lookupIdEnv idenv id
  | otherwise	-- ensure all imported ids are lifted
     = if isUnboxedButNotState (getIdUniType id)
       then Just (mkLiftedId id (getSUnique us))
       else Nothing

mkLiftedId :: Id -> Unique -> (Id,Id)
mkLiftedId id u
  = ASSERT (isUnboxedButNotState unlifted_ty)
    (lifted_id, unlifted_id)
  where
    id_name     = getOccurrenceName id
    lifted_id   = updateIdType id lifted_ty
    unlifted_id = mkSysLocal id_name u unlifted_ty (getSrcLoc id)

    unlifted_ty = getIdUniType id
    lifted_ty   = mkLiftTy unlifted_ty

bindUnlift :: Id -> Id -> PlainCoreExpr -> PlainCoreExpr
bindUnlift vlift vunlift expr
  = ASSERT (isUnboxedButNotState unlift_ty)
    ASSERT (lift_ty == mkLiftTy unlift_ty)
    CoCase (CoVar vlift)
	   (CoAlgAlts [(liftDataCon, [vunlift], expr)] CoNoDefault)
  where
    lift_ty   = getIdUniType vlift
    unlift_ty = getIdUniType vunlift

liftExpr :: Id -> PlainCoreExpr -> PlainCoreExpr
liftExpr vunlift rhs
  = ASSERT (isUnboxedButNotState unlift_ty)
    ASSERT (rhs_ty == unlift_ty)
    CoCase rhs (CoPrimAlts [] (CoBindDefault vunlift 
			      (CoCon liftDataCon [unlift_ty] [CoVarAtom vunlift])))
  where
    rhs_ty    = typeOfCoreExpr rhs
    unlift_ty = getIdUniType vunlift


applyBindUnlifts :: [PlainCoreExpr -> PlainCoreExpr] -> PlainCoreExpr -> PlainCoreExpr
applyBindUnlifts []     expr = expr
applyBindUnlifts (f:fs) expr = f (applyBindUnlifts fs expr)

isUnboxedButNotState ty
  = case (getUniDataTyCon_maybe ty) of
      Nothing -> False
      Just (tycon, _, _) ->
	not (isBoxedTyCon tycon) && not (tycon == statePrimTyCon)
\end{code}
