%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[AnalFBWW]{Spoting good functions for splitting into workers/wrappers}

\begin{code}
#include "HsVersions.h"

module AnalFBWW ( analFBWW ) where

IMPORT_Trace
import Outputable
import Pretty

import PlainCore
import TaggedCore
import Util
import Id               	( addIdFBTypeInfo )
import IdInfo           
import IdEnv
import AbsPrel          ( foldrId, buildId,
                          nilDataCon, consDataCon, mkListTy, mkFunTy,
                          unpackCStringAppendId
                        )
import BinderInfo
import SimplEnv		-- everything
import NewOccurAnal
import Maybes

\end{code}

\begin{code}
analFBWW 
        :: (GlobalSwitch -> Bool)
        -> PlainCoreProgram 
        -> PlainCoreProgram
analFBWW switch top_binds = trace "ANALFBWW" (snd anno)
 where
	anals :: [InBinding]
	anals = newOccurAnalyseBinds top_binds switch (const False)
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
	= IsFB (FBType (take n (repeat FBBadConsum) ++ args) prod)
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
	| length args == length args' = (IsFB (FBType (zipWith argJ args args')
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
analExprFBWW (CoApp (CoTyApp (CoVar bld) _) _) env  
	| bld == buildId         = goodProdFBType

--
-- [ foldr (:) ys xs ] ==> good
--			(but better if xs)
--
analExprFBWW (CoApp (CoApp (CoApp 
		(CoTyApp (CoTyApp (CoVar foldr_id) _) _) (CoVarAtom c)) _) _)
		env 
	| pprTrace ("FOLDR:" ++ show (foldr_id == foldrId,isCons c))
		(ppr PprDebug foldr_id)
		(foldr_id == foldrId && isCons c) = goodProdFBType
   where
	isCons c = case lookupIdEnv env c of
		    Just IsCons -> True
		    _ -> False
analExprFBWW (CoVar v) env       = maybeFBtoFB (lookupIdEnv env v)
analExprFBWW (CoLit _) _         = unknownFBType

--
-- [ x : xs ]  ==> good iff [ xs ] is good
--

analExprFBWW (CoCon con _ [_,CoVarAtom y]) env     
	| con == consDataCon = maybeFBtoFB (lookupIdEnv env y)
--
-- [] is good
--
analExprFBWW (CoCon con _ []) _     
	| con == nilDataCon = goodProdFBType
analExprFBWW (CoCon _ _ _) _     = unknownFBType
analExprFBWW (CoPrim _ _ _) _    = unknownFBType

-- \ xy -> (:) ty xy == a CONS
analExprFBWW (CoLam [(x,_),(y,_)]
		(CoCon con _ [CoVarAtom x',CoVarAtom y'])) env
	| con == consDataCon && x == x' && y == y' 
	= IsCons
analExprFBWW (CoLam ids e) env   
	= addArgs (length ids) (analExprFBWW e (delManyFromIdEnv env (map fst ids)))
analExprFBWW (CoTyLam tyvar e) env = analExprFBWW e env
analExprFBWW (CoApp f atom) env  = rmArg (analExprFBWW f env)
analExprFBWW (CoTyApp f ty) env  = analExprFBWW f env
analExprFBWW (CoSCC lab e) env   = analExprFBWW e env
analExprFBWW (CoLet binds e) env = analExprFBWW e (analBind binds env) 
analExprFBWW (CoCase e alts) env = foldl1 joinFBType (analAltsFBWW alts env)

analAltsFBWW (CoAlgAlts alts deflt) env = 
    case analDefFBWW deflt env of
	Just ty -> ty : tys
	Nothing -> tys
   where
     tys = map (\(con,binders,e) -> analExprFBWW e (delManyFromIdEnv env (map fst binders))) alts
analAltsFBWW (CoPrimAlts alts deflt) env = 
    case analDefFBWW deflt env of
	Just ty -> ty : tys
	Nothing -> tys
   where
     tys = map (\(lit,e) -> analExprFBWW e env) alts


analDefFBWW CoNoDefault env = Nothing
analDefFBWW (CoBindDefault v e) env = Just (analExprFBWW e (delOneFromIdEnv env (fst v)))
\end{code}


Only add a type info if:

1. Is a functionn.
2. Is an inlineable object.

\begin{code}
analBindExpr :: BinderInfo -> InExpr -> IdEnv OurFBType -> OurFBType
analBindExpr bnd expr env = 
       case analExprFBWW expr env of
	      IsFB ty@(FBType [] _) -> 
		   if oneSafeOcc False bnd
		   then IsFB ty
		   else IsNotFB
	      other -> other

analBind :: InBinding -> IdEnv OurFBType -> IdEnv OurFBType
analBind (CoNonRec (v,bnd) e) env = 
	case analBindExpr bnd e env of
	 ty@(IsFB _) -> addOneToIdEnv env v ty
	 ty@(IsCons) -> addOneToIdEnv env v ty
	 _ -> delOneFromIdEnv env v	-- remember about shadowing!

analBind (CoRec binds) env = 
   let
	first_set = [ (v,IsFB (FBType [FBBadConsum | _ <- args ] FBGoodProd)) | ((v,_),e) <- binds,
				(_,args,_) <- [digForLambdas e]]
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
annotateExprFBWW :: InExpr -> IdEnv OurFBType -> PlainCoreExpr
annotateExprFBWW (CoVar v) env = CoVar v
annotateExprFBWW (CoLit i) env = CoLit i
annotateExprFBWW (CoCon c t a) env = CoCon c t a
annotateExprFBWW (CoPrim p t a) env = CoPrim p t a 
annotateExprFBWW (CoLam ids e) env = CoLam ids' (annotateExprFBWW e (delManyFromIdEnv env ids'))
   where ids' = map fst ids
annotateExprFBWW (CoTyLam tyvar e) env = CoTyLam tyvar (annotateExprFBWW e env)
annotateExprFBWW (CoApp f atom) env = CoApp (annotateExprFBWW f env) atom 
annotateExprFBWW (CoTyApp f ty) env = CoTyApp (annotateExprFBWW f env) ty
annotateExprFBWW (CoSCC lab e) env = CoSCC lab (annotateExprFBWW e env)
annotateExprFBWW (CoCase e alts) env = CoCase (annotateExprFBWW e env)
					    (annotateAltsFBWW alts env)
annotateExprFBWW (CoLet bnds e) env = CoLet bnds' (annotateExprFBWW e env')
  where
	(env',bnds') = annotateBindingFBWW env bnds 

annotateAltsFBWW (CoAlgAlts alts deflt) env = CoAlgAlts alts' deflt'
  where
	alts' = [ let
		   binders' = map fst binders
		  in (con,binders',annotateExprFBWW e (delManyFromIdEnv env binders'))
				| (con,binders,e) <- alts ]
	deflt' = annotateDefFBWW deflt env
annotateAltsFBWW (CoPrimAlts alts deflt) env = CoPrimAlts alts' deflt'
  where
	alts' = [ (lit,annotateExprFBWW e env) | (lit,e) <- alts ]
	deflt' = annotateDefFBWW deflt env

annotateDefFBWW CoNoDefault env = CoNoDefault
annotateDefFBWW (CoBindDefault v e) env 
	= CoBindDefault (fst v) (annotateExprFBWW e (delOneFromIdEnv env (fst v)))

annotateBindingFBWW :: IdEnv OurFBType -> InBinding -> (IdEnv OurFBType,PlainCoreBinding)
annotateBindingFBWW env bnds = (env',bnds')
  where
	env' = analBind bnds env
	bnds' = case bnds of
		  CoNonRec (v,_) e -> CoNonRec (fixId v) (annotateExprFBWW e env)
		  CoRec bnds -> CoRec [ (fixId v,annotateExprFBWW e env') | ((v,_),e) <- bnds ]
	fixId v =
		(case lookupIdEnv env' v of
		   Just (IsFB ty@(FBType xs p))
		    | not (null xs) -> pprTrace "ADDED to:" (ppr PprDebug v)
				        (addIdFBTypeInfo v (mkFBTypeInfo ty))
		   _ -> v)
\end{code}
