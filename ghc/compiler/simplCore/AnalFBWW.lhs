%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[AnalFBWW]{Spoting good functions for splitting into workers/wrappers}

\begin{code}
#include "HsVersions.h"

module AnalFBWW ( analFBWW ) where

IMP_Ubiq(){-uitous-}

import CoreSyn		( SYN_IE(CoreBinding) )
import Util		( panic{-ToDo:rm-} )

--import Util
--import Id               	( addIdFBTypeInfo )
--import IdInfo
--import PrelInfo          ( foldrId, buildId,
--			  nilDataCon, consDataCon, mkListTy, mkFunTy,
--			  unpackCStringAppendId
--			)
--import BinderInfo
--import SimplEnv		-- everything
--import OccurAnal	-- OLD: was NewOccurAnal
--import Maybes
\end{code}

\begin{code}
analFBWW
	:: [CoreBinding]
	-> [CoreBinding]

analFBWW = panic "analFBWW (ToDo)"

{- LATER:
analFBWW top_binds = trace "ANALFBWW" (snd anno)
 where
	anals :: [InBinding]
	anals = newOccurAnalyseBinds top_binds (const False)
	anno = mapAccumL annotateBindingFBWW nullIdEnv anals
\end{code}

\begin{code}
data OurFBType
	= IsFB FBType
	| IsNotFB		-- unknown
	| IsCons		-- \ xy -> (:) ty xy
	| IsBottom		-- _|_
		deriving (Eq)
	-- We only handle *reasonable* types
	-- Later might add concept of bottom
	-- because foldr f z (<bottom>) = <bottom>
unknownFBType  = IsNotFB
goodProdFBType = IsFB (FBType [] FBGoodProd)

maybeFBtoFB (Just ty) = ty
maybeFBtoFB (Nothing) = IsNotFB

addArgs :: Int -> OurFBType -> OurFBType
addArgs n (IsFB (FBType args prod))
	= IsFB (FBType (nOfThem n FBBadConsum ++ args) prod)
addArgs n IsNotFB = IsNotFB
addArgs n IsCons = panic "adding argument to a cons"
addArgs n IsBottom = IsNotFB

rmArg :: OurFBType -> OurFBType
rmArg (IsFB (FBType [] prod)) = IsNotFB -- panic "removing argument from producer"
rmArg (IsFB (FBType args prod)) = IsFB (FBType (tail args) prod)
rmArg IsBottom = IsBottom
rmArg _ = IsNotFB

joinFBType :: OurFBType -> OurFBType -> OurFBType
joinFBType (IsBottom) a = a
joinFBType a (IsBottom) = a
joinFBType (IsFB (FBType args prod)) (IsFB (FBType args' prod'))
	| length args == length args' = (IsFB (FBType (zipWith{-Equal-} argJ args args')
						      (prodJ prod prod')))
   where
	argJ FBGoodConsum FBGoodConsum = FBGoodConsum
	argJ _ 		  _	       = FBBadConsum
	prodJ FBGoodProd FBGoodProd    = FBGoodProd
	prodJ _ 		  _    = FBBadProd

joinFBType _ _ = IsNotFB

--
-- Mutter :: IdEnv FBType need to be in an *inlinable* context.
--

analExprFBWW :: InExpr -> IdEnv OurFBType -> OurFBType

--
-- [ build g ]  	is a good context
--
analExprFBWW (App (CoTyApp (Var bld) _) _) env
	| bld == buildId         = goodProdFBType

--
-- [ foldr (:) ys xs ] ==> good
--			(but better if xs)
--
analExprFBWW (App (App (App
		(CoTyApp (CoTyApp (Var foldr_id) _) _) (VarArg c)) _) _)
		env
	| pprTrace ("FOLDR:" ++ show (foldr_id == foldrId,isCons c))
		(ppr PprDebug foldr_id)
		(foldr_id == foldrId && isCons c) = goodProdFBType
   where
	isCons c = case lookupIdEnv env c of
		    Just IsCons -> True
		    _ -> False
analExprFBWW (Var v) env       = maybeFBtoFB (lookupIdEnv env v)
analExprFBWW (Lit _) _         = unknownFBType

--
-- [ x : xs ]  ==> good iff [ xs ] is good
--

analExprFBWW (Con con _ [_,VarArg y]) env
	| con == consDataCon = maybeFBtoFB (lookupIdEnv env y)
--
-- [] is good
--
analExprFBWW (Con con _ []) _
	| con == nilDataCon = goodProdFBType
analExprFBWW (Con _ _ _) _     = unknownFBType
analExprFBWW (Prim _ _ _) _    = unknownFBType

-- \ xy -> (:) ty xy == a CONS

analExprFBWW (Lam (x,_) (Lam (y,_)
		(Con con _ [VarArg x',VarArg y']))) env
  | con == consDataCon && x == x' && y == y'
  = IsCons
analExprFBWW (Lam (id,_) e) env
  = addArgs 1 (analExprFBWW e (delOneFromIdEnv env id))

analExprFBWW (CoTyLam tyvar e) env = analExprFBWW e env
analExprFBWW (App f atom) env  = rmArg (analExprFBWW f env)
analExprFBWW (CoTyApp f ty) env  = analExprFBWW f env
analExprFBWW (SCC lab e) env   = analExprFBWW e env
analExprFBWW (Coerce _ _ _) env   = panic "AnalFBWW:analExprFBWW:Coerce"
analExprFBWW (Let binds e) env = analExprFBWW e (analBind binds env)
analExprFBWW (Case e alts) env = foldl1 joinFBType (analAltsFBWW alts env)

analAltsFBWW (AlgAlts alts deflt) env
  = case analDefFBWW deflt env of
	Just ty -> ty : tys
	Nothing -> tys
   where
     tys = map (\(con,binders,e) -> analExprFBWW e (delManyFromIdEnv env (map fst binders))) alts
analAltsFBWW (PrimAlts alts deflt) env
  = case analDefFBWW deflt env of
	Just ty -> ty : tys
	Nothing -> tys
   where
     tys = map (\(lit,e) -> analExprFBWW e env) alts


analDefFBWW NoDefault env = Nothing
analDefFBWW (BindDefault v e) env = Just (analExprFBWW e (delOneFromIdEnv env (fst v)))
\end{code}


Only add a type info if:

1. Is a functionn.
2. Is an inlineable object.

\begin{code}
analBindExpr :: BinderInfo -> InExpr -> IdEnv OurFBType -> OurFBType
analBindExpr bnd expr env
  =    case analExprFBWW expr env of
	      IsFB ty@(FBType [] _) ->
		   if oneSafeOcc False bnd
		   then IsFB ty
		   else IsNotFB
	      other -> other

analBind :: InBinding -> IdEnv OurFBType -> IdEnv OurFBType
analBind (NonRec (v,bnd) e) env =
	case analBindExpr bnd e env of
	 ty@(IsFB _) -> addOneToIdEnv env v ty
	 ty@(IsCons) -> addOneToIdEnv env v ty
	 _ -> delOneFromIdEnv env v	-- remember about shadowing!

analBind (Rec binds) env =
   let
	first_set = [ (v,IsFB (FBType [FBBadConsum | _ <- args ] FBGoodProd)) | ((v,_),e) <- binds,
				(_,_,args,_) <- [collectBinders e]]
	env' = delManyFromIdEnv env (map (fst.fst) binds)
   in
	growIdEnvList env' (fixpoint 0 binds env' first_set)

fixpoint :: Int -> [(InBinder,InExpr)] -> IdEnv OurFBType -> [(Id,OurFBType)] -> [(Id,OurFBType)]
fixpoint n binds env maps =
	if maps == maps'
	then maps
	else fixpoint (n+1) binds env maps'
   where
	env' = growIdEnvList env maps
	maps' = [ (v,ty) | ((v,bind),e) <- binds,
			(ty@(IsFB (FBType cons prod))) <- [analBindExpr bind e env']]

\end{code}


\begin{code}
annotateExprFBWW :: InExpr -> IdEnv OurFBType -> CoreExpr
annotateExprFBWW (Var v) env = Var v
annotateExprFBWW (Lit i) env = Lit i
annotateExprFBWW (Con c t a) env = Con c t a
annotateExprFBWW (Prim p t a) env = Prim p t a
annotateExprFBWW (Lam (id,_) e) env
  = Lam id (annotateExprFBWW e (delOneFromIdEnv env id))

annotateExprFBWW (CoTyLam tyvar e) env = CoTyLam tyvar (annotateExprFBWW e env)
annotateExprFBWW (App f atom) env = App (annotateExprFBWW f env) atom
annotateExprFBWW (CoTyApp f ty) env = CoTyApp (annotateExprFBWW f env) ty
annotateExprFBWW (SCC lab e) env = SCC lab (annotateExprFBWW e env)
annotateExprFBWW (Coerce c ty e) env = Coerce c ty (annotateExprFBWW e env)
annotateExprFBWW (Case e alts) env = Case (annotateExprFBWW e env)
					    (annotateAltsFBWW alts env)
annotateExprFBWW (Let bnds e) env = Let bnds' (annotateExprFBWW e env')
  where
	(env',bnds') = annotateBindingFBWW env bnds

annotateAltsFBWW (AlgAlts alts deflt) env = AlgAlts alts' deflt'
  where
	alts' = [ let
		   binders' = map fst binders
		  in (con,binders',annotateExprFBWW e (delManyFromIdEnv env binders'))
				| (con,binders,e) <- alts ]
	deflt' = annotateDefFBWW deflt env
annotateAltsFBWW (PrimAlts alts deflt) env = PrimAlts alts' deflt'
  where
	alts' = [ (lit,annotateExprFBWW e env) | (lit,e) <- alts ]
	deflt' = annotateDefFBWW deflt env

annotateDefFBWW NoDefault env = NoDefault
annotateDefFBWW (BindDefault v e) env
	= BindDefault (fst v) (annotateExprFBWW e (delOneFromIdEnv env (fst v)))

annotateBindingFBWW :: IdEnv OurFBType -> InBinding -> (IdEnv OurFBType,CoreBinding)
annotateBindingFBWW env bnds = (env',bnds')
  where
	env' = analBind bnds env
	bnds' = case bnds of
		  NonRec (v,_) e -> NonRec (fixId v) (annotateExprFBWW e env)
		  Rec bnds -> Rec [ (fixId v,annotateExprFBWW e env') | ((v,_),e) <- bnds ]
	fixId v =
		(case lookupIdEnv env' v of
		   Just (IsFB ty@(FBType xs p))
		    | not (null xs) -> pprTrace "ADDED to:" (ppr PprDebug v)
					(addIdFBTypeInfo v (mkFBTypeInfo ty))
		   _ -> v)
-}
\end{code}
