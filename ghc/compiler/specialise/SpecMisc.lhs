%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[SpecMisc]{Miscellaneous stuff for the Specialiser}

\begin{code}
#include "HsVersions.h"

module SpecMisc where

import PlainCore
import SpecTyFuns
import SpecMonad

IMPORT_Trace
import Outputable	-- ToDo: these may be removable...
import Pretty

import AbsUniType
import Bag
import CmdLineOpts	( GlobalSwitch(..) )
import CoreLift		( mkLiftedId, liftExpr, bindUnlift, applyBindUnlifts )
import IdEnv
import Id
import IdInfo
import InstEnv		( lookupClassInstAtSimpleType )
import Maybes		( catMaybes, firstJust, maybeToBool, Maybe(..) )
import TyVarEnv		-- ( growTyVarEnvList, nullTyVarEnv, TyVarEnv, TypeEnv(..) )
import Util
import UniqSet
import SplitUniq

infixr 9 `thenSM`
\end{code}

%************************************************************************
%*									*
\subsubsection[CallInstances]{@CallInstances@ data type}
%*									*
%************************************************************************

\begin{code}
type FreeVarsSet   = UniqSet Id
type FreeTyVarsSet = UniqSet TyVar

data CallInstance 
  = CallInstance 
		Id 			-- This Id; *new* ie *cloned* id
		[Maybe UniType]		-- Specialised at these types (*new*, cloned)
					-- Nothing => no specialisation on this type arg
					--	      is required (flag dependent).
		[PlainCoreArg]		-- And these dictionaries; all ValArgs
		FreeVarsSet		-- Free vars of the dict-args in terms of *new* ids
		(Maybe SpecInfo)	-- For specialisation with explicit SpecId
\end{code}

\begin{code}
pprCI :: CallInstance -> Pretty
pprCI (CallInstance id spec_tys dicts _ maybe_specinfo)
  = ppHang (ppCat [ppStr "Call inst for", ppr PprDebug id])
	 4 (ppAboves [ppCat (ppStr "types" : [pprMaybeTy PprDebug ty | ty <- spec_tys]),
		      case maybe_specinfo of
			Nothing -> ppCat (ppStr "dicts" : [ppr PprDebug dict | dict <- dicts])
		        Just (SpecInfo _ _ spec_id)
				-> ppCat [ppStr "Explicit SpecId", ppr PprDebug spec_id]
		     ])

isUnboxedCI :: CallInstance -> Bool
isUnboxedCI (CallInstance _ spec_tys _ _ _)
  = any isUnboxedDataType (catMaybes spec_tys)

isExplicitCI :: CallInstance -> Bool
isExplicitCI (CallInstance _ _ _ _ (Just _))
  = True
isExplicitCI (CallInstance _ _ _ _ Nothing)
  = False
\end{code}

Comparisons are based on the {\em types}, ignoring the dictionary args:

\begin{code}

cmpCI :: CallInstance -> CallInstance -> TAG_
cmpCI (CallInstance id1 tys1 _ _ _) (CallInstance id2 tys2 _ _ _) 
  = case cmpId id1 id2 of { EQ_ -> cmpUniTypeMaybeList tys1 tys2; other -> other }

cmpCI_tys :: CallInstance -> CallInstance -> TAG_
cmpCI_tys (CallInstance _ tys1 _ _ _) (CallInstance _ tys2 _ _ _)
  = cmpUniTypeMaybeList tys1 tys2

eqCI_tys :: CallInstance -> CallInstance -> Bool
eqCI_tys c1 c2
  = case cmpCI_tys c1 c2 of { EQ_ -> True; other -> False }

isCIofTheseIds :: [Id] -> CallInstance -> Bool
isCIofTheseIds ids (CallInstance ci_id _ _ _ _)
  = any (eqId ci_id) ids

singleCI :: Id -> [Maybe UniType] -> [PlainCoreArg] -> UsageDetails
singleCI id tys dicts
  = UsageDetails (unitBag (CallInstance id tys dicts fv_set Nothing))
		 emptyBag [] emptyUniqSet 0 0
  where
    fv_set = mkUniqSet (id : [dict | ValArg (CoVarAtom dict) <- dicts])

explicitCI :: Id -> [Maybe UniType] -> SpecInfo -> UsageDetails
explicitCI id tys specinfo
  = UsageDetails (unitBag call_inst) emptyBag [] emptyUniqSet 0 0
  where
    call_inst = CallInstance id tys dicts fv_set (Just specinfo)
    dicts  = panic "Specialise:explicitCI:dicts"
    fv_set = singletonUniqSet id

-- We do not process the CIs for top-level dfuns or defms
-- Instead we require an explicit SPEC inst pragma for dfuns
-- and an explict method within any instances for the defms

getCIids :: Bool -> [Id] -> [Id]
getCIids True ids = filter not_dict_or_defm ids
getCIids _    ids = ids

not_dict_or_defm id
  = not (isDictTy (getIdUniType id) || maybeToBool (isDefaultMethodId_maybe id))

getCIs :: Bool -> [Id] -> UsageDetails -> ([CallInstance], UsageDetails)
getCIs top_lev ids (UsageDetails cis tycon_cis dbs fvs c i)
  = let
	(cis_here, cis_not_here) = partitionBag (isCIofTheseIds (getCIids top_lev ids)) cis
	cis_here_list = bagToList cis_here
    in
    -- pprTrace "getCIs:"
    -- (ppHang (ppBesides [ppStr "{", ppr PprDebug ids, ppStr "}"])
    --	     4 (ppAboves (map pprCI cis_here_list)))
    (cis_here_list, UsageDetails cis_not_here tycon_cis dbs fvs c i)

dumpCIs :: Bag CallInstance	-- The call instances
	-> Bool			-- True <=> top level bound Ids
	-> Bool			-- True <=> dict bindings to be floated (specBind only)
        -> [CallInstance]	-- Call insts for bound ids (instBind only)
	-> [Id]			-- Bound ids *new*
	-> [Id]			-- Full bound ids: includes dumped dicts
	-> Bag CallInstance	-- Kept call instances

   	-- CIs are dumped if: 
	--   1) they are a CI for one of the bound ids, or
	--   2) they mention any of the dicts in a local unfloated binding
	--
	-- For top-level bindings we allow the call instances to
	-- float past a dict bind and place all the top-level binds
	-- in a *global* CoRec.
	-- We leave it to the simplifier will sort it all out ...

dumpCIs cis top_lev floating inst_cis bound_ids full_ids
 = (if not (isEmptyBag cis_of_bound_id) &&
       not (isEmptyBag cis_of_bound_id_without_inst_cis)
    then
       pprTrace ("dumpCIs: dumping CI which was not instantiated ... \n" ++
	         "         (may be a non-HM recursive call)\n")
       (ppHang (ppBesides [ppStr "{", ppr PprDebug bound_ids, ppStr "}"])
             4 (ppAboves [ppStr "Dumping CIs:",
			  ppAboves (map pprCI (bagToList cis_of_bound_id)),
			  ppStr "Instantiating CIs:",
			  ppAboves (map pprCI inst_cis)]))
    else id) (
   if top_lev || floating then
       cis_not_bound_id
   else
       (if not (isEmptyBag cis_dump_unboxed)
	then pprTrace "dumpCIs: bound dictionary arg ... WITH UNBOXED TYPES!\n"
             (ppHang (ppBesides [ppStr "{", ppr PprDebug full_ids, ppStr "}"])
		   4 (ppAboves (map pprCI (bagToList cis_dump))))
	else id)
       cis_keep_not_bound_id
   )
 where
   (cis_of_bound_id, cis_not_bound_id)
      = partitionBag (isCIofTheseIds (getCIids top_lev bound_ids)) cis

   (cis_dump, cis_keep_not_bound_id)
      = partitionBag ok_to_dump_ci cis_not_bound_id

   ok_to_dump_ci (CallInstance _ _ _ fv_set _) 
	= or [i `elementOfUniqSet` fv_set | i <- full_ids]

   (_, cis_of_bound_id_without_inst_cis) = partitionBag have_inst_ci cis_of_bound_id
   have_inst_ci ci = any (eqCI_tys ci) inst_cis

   (cis_dump_unboxed, _) = partitionBag isUnboxedCI cis_dump

\end{code}

Any call instances of a bound_id can be safely dumped, because any
recursive calls should be at the same instance as the parent instance.

   letrec f = /\a -> \x::a -> ...(f t x')...

Here, the type, t, at which f is used in its own RHS should be
just "a"; that is, the recursive call is at the same type as
the original call. That means that when specialising f at some
type, say Int#, we shouldn't find any *new* instances of f 
arising from specialising f's RHS.  The only instance we'll find
is another call of (f Int#).

We check this in dumpCIs by passing in all the instantiated call
instances (inst_cis) and reporting any dumped cis (cis_of_bound_id)
for which there is no such instance.

We also report CIs dumped due to a bound dictionary arg if they
contain unboxed types.

%************************************************************************
%*									*
\subsubsection[TyConInstances]{@TyConInstances@ data type}
%*									*
%************************************************************************

\begin{code}
data TyConInstance
  = TyConInstance TyCon			-- Type Constructor
		  [Maybe UniType]	-- Applied to these specialising types

cmpTyConI :: TyConInstance -> TyConInstance -> TAG_
cmpTyConI (TyConInstance tc1 tys1) (TyConInstance tc2 tys2) 
  = case cmpTyCon tc1 tc2 of { EQ_ -> cmpUniTypeMaybeList tys1 tys2; other -> other }

cmpTyConI_tys :: TyConInstance -> TyConInstance -> TAG_
cmpTyConI_tys (TyConInstance _ tys1) (TyConInstance _ tys2) 
  = cmpUniTypeMaybeList tys1 tys2

singleTyConI :: TyCon -> [Maybe UniType] -> UsageDetails
singleTyConI ty_con spec_tys 
  = UsageDetails emptyBag (unitBag (TyConInstance ty_con spec_tys)) [] emptyUniqSet 0 0

isTyConIofThisTyCon :: TyCon -> TyConInstance -> Bool
isTyConIofThisTyCon ty_con (TyConInstance inst_ty_con _) = eqTyCon ty_con inst_ty_con

isLocalSpecTyConI :: Bool -> TyConInstance -> Bool
isLocalSpecTyConI comp_prel (TyConInstance inst_ty_con _) = isLocalSpecTyCon comp_prel inst_ty_con

getLocalSpecTyConIs :: Bool -> UsageDetails -> ([TyConInstance], UsageDetails)
getLocalSpecTyConIs comp_prel (UsageDetails cis tycon_cis dbs fvs c i)
  = let
	(tycon_cis_local, tycon_cis_global)
	  = partitionBag (isLocalSpecTyConI comp_prel) tycon_cis
	tycon_cis_local_list = bagToList tycon_cis_local
    in
    (tycon_cis_local_list, UsageDetails cis tycon_cis_global dbs fvs c i)
\end{code}


%************************************************************************
%*									*
\subsubsection[UsageDetails]{@UsageDetails@ data type}
%*									*
%************************************************************************

\begin{code}
data UsageDetails
  = UsageDetails 
	(Bag CallInstance) 	-- The collection of call-instances
	(Bag TyConInstance) 	-- Constructor call-instances
	[DictBindDetails]	-- Dictionary bindings in data-dependence order!
	FreeVarsSet		-- Free variables (excl imported ones, incl top level) (cloned)
	Int			-- no. of spec calls
	Int			-- no. of spec insts
\end{code}

The DictBindDetails are fully processed; their call-instance information is
incorporated in the call-instances of the
UsageDetails which includes the DictBindDetails.  The free vars in a usage details
will *include* the binders of the DictBind details.

A @DictBindDetails@ contains bindings for dictionaries *only*.

\begin{code}
data DictBindDetails 
  = DictBindDetails 
	[Id]			-- Main binders, originally visible in scope of binding (cloned)
	PlainCoreBinding	-- Fully processed
	FreeVarsSet		-- Free in binding group (cloned)
	FreeTyVarsSet		-- Free in binding group
\end{code}

\begin{code}
emptyUDs    :: UsageDetails
unionUDs    :: UsageDetails -> UsageDetails -> UsageDetails
unionUDList :: [UsageDetails] -> UsageDetails

tickSpecCall :: Bool -> UsageDetails -> UsageDetails
tickSpecInsts :: UsageDetails -> UsageDetails

tickSpecCall found (UsageDetails cis ty_cis dbs fvs c i)
 = UsageDetails cis ty_cis dbs fvs (c + (if found then 1 else 0)) i

tickSpecInsts (UsageDetails cis ty_cis dbs fvs c i)
 = UsageDetails cis ty_cis dbs fvs c (i+1)

emptyUDs = UsageDetails emptyBag emptyBag [] emptyUniqSet 0 0

unionUDs (UsageDetails cis1 tycon_cis1 dbs1 fvs1 c1 i1) (UsageDetails cis2 tycon_cis2 dbs2 fvs2 c2 i2) 
 = UsageDetails (unionBags cis1 cis2) (unionBags tycon_cis1 tycon_cis2)
	        (dbs1 ++ dbs2) (fvs1 `unionUniqSets` fvs2) (c1+c2) (i1+i2)
	-- The append here is really redundant, since the bindings don't
	-- scope over each other.  ToDo.

unionUDList = foldr unionUDs emptyUDs

singleFvUDs (CoVarAtom v) | not (isImportedId v)
 = UsageDetails emptyBag emptyBag [] (singletonUniqSet v) 0 0
singleFvUDs other
 = emptyUDs

singleConUDs con = UsageDetails emptyBag emptyBag [] (singletonUniqSet con) 0 0

dumpDBs :: [DictBindDetails] 
	-> Bool			-- True <=> top level bound Ids
	-> [TyVar]		-- TyVars being bound (cloned)
	-> [Id]			-- Ids being bound (cloned)
	-> FreeVarsSet		-- Fvs of body
	-> ([PlainCoreBinding],	-- These ones have to go here
	    [DictBindDetails],	-- These can float further
	    [Id],		-- Incoming list + names of dicts bound here
	    FreeVarsSet		-- Incoming fvs + fvs of dicts bound here
	   )

 	-- It is just to complex to try to float top-level
	-- dict bindings with constant methods, inst methods,
	-- auxillary derived instance defns and user instance
	-- defns all getting in the way.
	-- So we dump all dbinds as soon as we get to the top
	-- level and place them in a *global* CoRec.
	-- We leave it to the simplifier will sort it all out ...

dumpDBs [] top_lev bound_tyvars bound_ids fvs
  = ([], [], bound_ids, fvs)

dumpDBs ((db@(DictBindDetails dbinders dbind db_fvs db_ftv)):dbs) 
	top_lev bound_tyvars bound_ids fvs
  | top_lev
    || or [i `elementOfUniqSet` db_fvs  | i <- bound_ids]
    || or [tv `elementOfUniqSet` db_ftv | tv <- bound_tyvars]
  = let		-- Ha!  Dump it!
	(dbinds_here, dbs_outer, full_bound_ids, full_fvs)
	   = dumpDBs dbs top_lev bound_tyvars (dbinders ++ bound_ids) (db_fvs `unionUniqSets` fvs)
    in
    (dbind : dbinds_here, dbs_outer, full_bound_ids, full_fvs)

  | otherwise	-- This one can float out further
  = let
	(dbinds_here, dbs_outer, full_bound_ids, full_fvs)
	   = dumpDBs dbs top_lev bound_tyvars bound_ids fvs
    in
    (dbinds_here, db : dbs_outer, full_bound_ids, full_fvs)


     
dumpUDs :: UsageDetails
	-> Bool			-- True <=> top level bound Ids
	-> Bool			-- True <=> dict bindings to be floated (specBind only)
	-> [CallInstance]	-- Call insts for bound Ids (instBind only)
	-> [Id]			-- Ids which are just being bound; *new*
	-> [TyVar]		-- TyVars which are just being bound
	-> ([PlainCoreBinding],	-- Bindings from UsageDetails which mention the ids
	    UsageDetails)	-- The above bindings removed, and
				-- any call-instances which mention the ids dumped too

dumpUDs (UsageDetails cis tycon_cis dbs fvs c i) top_lev floating inst_cis bound_ids tvs
  = let
	(dict_binds_here, dbs_outer, full_bound_ids, full_fvs)
		  = dumpDBs dbs top_lev tvs bound_ids fvs
	cis_outer = dumpCIs cis top_lev floating inst_cis bound_ids full_bound_ids
	fvs_outer = full_fvs `minusUniqSet` (mkUniqSet full_bound_ids)
    in
    (dict_binds_here, UsageDetails cis_outer tycon_cis dbs_outer fvs_outer c i)
\end{code}

\begin{code}
addDictBinds :: [Id] -> PlainCoreBinding -> UsageDetails	-- Dict binding and RHS usage
	     -> UsageDetails	 				-- The usage to augment
	     -> UsageDetails
addDictBinds dbinders dbind (UsageDetails db_cis db_tycon_cis db_dbs db_fvs db_c db_i)
	 	            (UsageDetails cis    tycon_cis    dbs    fvs    c    i)
  = UsageDetails (db_cis `unionBags` cis)
		 (db_tycon_cis `unionBags` tycon_cis)
		 (db_dbs ++ [DictBindDetails dbinders dbind db_fvs db_ftvs] ++ dbs) 
		 fvs c i
		 -- NB: We ignore counts from dictbinds since it is not user code
  where
	-- The free tyvars of the dictionary bindings should really be
	-- gotten from the RHSs, but I'm pretty sure it's good enough just
	-- to look at the type of the dictionary itself.  
	-- Doing the proper job would entail keeping track of free tyvars as
	-- well as free vars, which would be a bore.
    db_ftvs = mkUniqSet (extractTyVarsFromTys (map getIdUniType dbinders))
\end{code}

%************************************************************************
%*									*
\subsection[Misc]{Miscellaneous junk}
%*									*
%************************************************************************

\begin{code}
mkCallInstance :: Id 
	       -> Id
	       -> [(PlainCoreArg, UsageDetails, PlainCoreExpr -> PlainCoreExpr)]
	       -> SpecM UsageDetails

mkCallInstance id new_id []
  = returnSM emptyUDs

mkCallInstance id new_id args

	-- No specialised versions for "error" and friends are req'd.
	-- This is a special case in core lint etc.

  | isBottomingId id
  = returnSM emptyUDs

	-- No call instances for SuperDictSelIds
	-- These are a special case in mkCall

  | maybeToBool (isSuperDictSelId_maybe id)
  = returnSM emptyUDs

	-- There are also no call instances for ClassOpIds
	-- However, we need to process it to get any second-level call
	-- instances for a ConstMethodId extracted from its SpecEnv

  | otherwise
  = getSwitchCheckerSM		`thenSM` \ sw_chkr ->
    let
        spec_overloading = sw_chkr SpecialiseOverloaded
        spec_unboxed     = sw_chkr SpecialiseUnboxed
        spec_all	 = sw_chkr SpecialiseAll

	(tyvars, class_tyvar_pairs) = getIdOverloading id

	arg_res = take_type_args tyvars class_tyvar_pairs args
	enough_args = maybeToBool arg_res

	(Just (tys, dicts, rest_args)) = arg_res

	record_spec id tys
	  = (record, lookup, spec_tys)
	  where
	    spec_tys = specialiseCallTys spec_all spec_unboxed spec_overloading
		                         (mkConstraintVector id) tys

	    record = any (not . isTyVarTy) (catMaybes spec_tys)

	    lookup = lookupSpecEnv (getIdSpecialisation id) tys
    in
    if (not enough_args) then
	pprPanic "Specialise:recordCallInst: Unsaturated Type & Dict Application:\n\t"
		 (ppCat [ppr PprDebug id, ppr PprDebug [arg | (arg,_,_) <- args] ]) 
    else
    case record_spec id tys of
	(False, _, _)
	     -> -- pprTrace "CallInst:NotReqd\n" 
	   	-- (ppCat [ppr PprDebug id, ppCat (map (ppr PprDebug) args)])
		(returnSM emptyUDs)

	(True, Nothing, spec_tys)
	     -> if isClassOpId id then	-- No CIs for class ops, dfun will give SPEC inst
		    returnSM emptyUDs
		else
		    -- pprTrace "CallInst:Reqd\n"
	            -- (ppAboves [ppCat [ppr PprDebug id, ppCat (map (ppr PprDebug) args)],
		    --	          ppCat [ppStr "CI", ppCat (map (pprMaybeTy PprDebug) spec_tys),
	   	    --		                     ppCat (map (ppr PprDebug) dicts)]])
		    (returnSM (singleCI new_id spec_tys dicts))

	(True, Just (spec_id, tys_left, toss), _)
	     -> if maybeToBool (isConstMethodId_maybe spec_id) then
			-- If we got a const method spec_id see if further spec required
			-- NB: const method is top-level so spec_id will not be cloned
		    case record_spec spec_id tys_left of
		      (False, _, _)
		    	-> -- pprTrace "CallInst:Exists\n" 
	            	   -- (ppAboves [ppCat [ppr PprDebug id, ppCat (map (ppr PprDebug) args)],
		    	   --	         ppCat [ppStr "->", ppr PprDebug spec_id,
		    	   --		        ppr PprDebug (tys_left ++ drop toss dicts)]])
			   (returnSM emptyUDs)

		      (True, Nothing, spec_tys)
			-> -- pprTrace "CallInst:Exists:Reqd\n"
	            	   -- (ppAboves [ppCat [ppr PprDebug id, ppCat (map (ppr PprDebug) args)],
		    	   --	         ppCat [ppStr "->", ppr PprDebug spec_id,
		    	   --		        ppr PprDebug (tys_left ++ drop toss dicts)],
			   --	         ppCat [ppStr "CI", ppCat (map (pprMaybeTy PprDebug) spec_tys),
	   		   --		                    ppCat (map (ppr PprDebug) (drop toss dicts))]])
			   (returnSM (singleCI spec_id spec_tys (drop toss dicts)))

		      (True, Just (spec_spec_id, tys_left_left, toss_toss), _)
			-> -- pprTrace "CallInst:Exists:Exists\n" 
	            	   -- (ppAboves [ppCat [ppr PprDebug id, ppCat (map (ppr PprDebug) args)],
		    	   --	         ppCat [ppStr "->", ppr PprDebug spec_id,
		    	   --		        ppr PprDebug (tys_left ++ drop toss dicts)],
		    	   --	         ppCat [ppStr "->", ppr PprDebug spec_spec_id,
		    	   --		        ppr PprDebug (tys_left_left ++ drop (toss + toss_toss) dicts)]])
			   (returnSM emptyUDs)

		else
		    -- pprTrace "CallInst:Exists\n" 
	            -- (ppAboves [ppCat [ppr PprDebug id, ppCat (map (ppr PprDebug) args)],
		    --	          ppCat [ppStr "->", ppr PprDebug spec_id,
		    --		         ppr PprDebug (tys_left ++ drop toss dicts)]])
		    (returnSM emptyUDs)


take_type_args (_:tyvars) class_tyvar_pairs ((TypeArg ty,_,_):args) 
	= case take_type_args tyvars class_tyvar_pairs args of
		Nothing 	          -> Nothing
		Just (tys, dicts, others) -> Just (ty:tys, dicts, others)
take_type_args (_:tyvars) class_tyvar_pairs []
	= Nothing
take_type_args [] class_tyvar_pairs args 
	= case take_dict_args class_tyvar_pairs args of
		Nothing              -> Nothing
		Just (dicts, others) -> Just ([], dicts, others)

take_dict_args (_:class_tyvar_pairs) ((dict@(ValArg _),_,_):args) 
	= case take_dict_args class_tyvar_pairs args of
		Nothing              -> Nothing
		Just (dicts, others) -> Just (dict:dicts, others)
take_dict_args (_:class_tyvar_pairs) []
	= Nothing
take_dict_args [] args
	= Just ([], args)
\end{code}

\begin{code}
mkCall :: Id
       -> [(PlainCoreArg, UsageDetails, PlainCoreExpr -> PlainCoreExpr)]
       -> SpecM (Bool, PlainCoreExpr)

mkCall new_id args
  | maybeToBool (isSuperDictSelId_maybe new_id)
    && any isUnboxedDataType ty_args
	-- No specialisations for super-dict selectors
	-- Specialise unboxed calls to SuperDictSelIds by extracting
	-- the super class dictionary directly form the super class
	-- NB: This should be dead code since all uses of this dictionary should
	--     have been specialised. We only do this to keep core-lint happy.
    = let
	 Just (_, super_class) = isSuperDictSelId_maybe new_id
         super_dict_id = case lookupClassInstAtSimpleType super_class (head ty_args) of
			 Nothing -> panic "Specialise:mkCall:SuperDictId"
			 Just id -> id
      in
      returnSM (False, CoVar super_dict_id)

  | otherwise
    = case lookupSpecEnv (getIdSpecialisation new_id) ty_args of
	Nothing -> checkUnspecOK new_id ty_args (
		   returnSM (False, unspec_call)
		   )

	Just spec_1_details@(spec_id_1, tys_left_1, dicts_to_toss_1) 
		-> let
			-- It may be necessary to specialsie a constant method spec_id again
		       (spec_id, tys_left, dicts_to_toss) =
			    case (maybeToBool (isConstMethodId_maybe spec_id_1),
				  lookupSpecEnv (getIdSpecialisation spec_id_1) tys_left_1) of
				 (False, _ )	 -> spec_1_details
				 (True, Nothing) -> spec_1_details
				 (True, Just (spec_id_2, tys_left_2, dicts_to_toss_2))
						 -> (spec_id_2, tys_left_2, dicts_to_toss_1 + dicts_to_toss_2)
				
		       args_left = toss_dicts dicts_to_toss val_args
		   in
		   checkSpecOK new_id ty_args spec_id tys_left (

			-- The resulting spec_id may be a top-level unboxed value
			-- This can arise for:
			-- 1) constant method values
			--    eq: class Num a where pi :: a
			--	  instance Num Double# where pi = 3.141#
			-- 2) specilised overloaded values
			--    eq: i1 :: Num a => a
			--	  i1 Int# d.Num.Int# ==> i1.Int#
			-- These top level defns should have been lifted.
			-- We must add code to unlift such a spec_id.

		   if isUnboxedDataType (getIdUniType spec_id) then
		       ASSERT (null tys_left && null args_left)
		       if toplevelishId spec_id then
		 	   liftId spec_id 	`thenSM` \ (lift_spec_id, unlift_spec_id) ->
			   returnSM (True, bindUnlift lift_spec_id unlift_spec_id
						      (CoVar unlift_spec_id))
		       else
			   pprPanic "Specialise:mkCall: unboxed spec_id not top-level ...\n"
				    (ppCat [ppr PprDebug new_id,
				            ppInterleave ppNil (map (pprParendUniType PprDebug) ty_args),
					    ppStr "==>",
					    ppr PprDebug spec_id])
		   else
		   let
		       (vals_left, _, unlifts_left) = unzip3 args_left
		       applied_tys  = mkCoTyApps (CoVar spec_id) tys_left
		       applied_vals = applyToArgs applied_tys vals_left
		   in
		   returnSM (True, applyBindUnlifts unlifts_left applied_vals)
		   )
  where
    (tys_and_vals, _, unlifts) = unzip3 args
    unspec_call = applyBindUnlifts unlifts (applyToArgs (CoVar new_id) tys_and_vals)


	-- ty_args is the types at the front of the arg list
	-- val_args is the rest of the arg-list

    (ty_args, val_args) = get args
      where
	get ((TypeArg ty,_,_) : args) = (ty : tys, rest) where (tys,rest) = get args
	get args		      = ([],       args)


	-- toss_dicts chucks away dict args, checking that they ain't types!
    toss_dicts 0 args 		     = args
    toss_dicts n ((ValArg _,_,_) : args) = toss_dicts (n-1) args

\end{code}

\begin{code}
checkUnspecOK :: Id -> [UniType] -> a -> a
checkUnspecOK check_id tys
  = if isLocallyDefined check_id && any isUnboxedDataType tys
    then pprPanic "Specialise:checkUnspecOK: unboxed instance for local id not found\n"
		  (ppCat [ppr PprDebug check_id,
			  ppInterleave ppNil (map (pprParendUniType PprDebug) tys)])
    else id

checkSpecOK :: Id -> [UniType] -> Id -> [UniType] -> a -> a
checkSpecOK check_id tys spec_id tys_left
  = if any isUnboxedDataType tys_left
    then pprPanic "Specialise:checkSpecOK: unboxed type args in specialised application\n"
		  (ppAboves [ppCat [ppr PprDebug check_id,
				    ppInterleave ppNil (map (pprParendUniType PprDebug) tys)],
			     ppCat [ppr PprDebug spec_id,
				    ppInterleave ppNil (map (pprParendUniType PprDebug) tys_left)]])
    else id
\end{code}

\begin{code}
mkTyConInstance :: Id
		-> [UniType]
   		-> SpecM UsageDetails
mkTyConInstance con tys
  = recordTyConInst con tys	`thenSM` \ record_inst ->
    case record_inst of
      Nothing				-- No TyCon instance
        -> -- pprTrace "NoTyConInst:" 
	   -- (ppCat [ppr PprDebug tycon, ppStr "at",
	   --	      ppr PprDebug con, ppCat (map (ppr PprDebug) tys)])
	   (returnSM (singleConUDs con))

      Just spec_tys			-- Record TyCon instance
	-> -- pprTrace "TyConInst:"
	   -- (ppCat [ppr PprDebug tycon, ppStr "at",
	   --	      ppr PprDebug con, ppCat (map (ppr PprDebug) tys),
	   --	      ppBesides [ppStr "(", 
	   --			 ppCat [pprMaybeTy PprDebug ty | ty <- spec_tys],
	   --			 ppStr ")"]])
	   (returnSM (singleTyConI tycon spec_tys `unionUDs` singleConUDs con))
  where
    tycon = getDataConTyCon con
\end{code}

\begin{code}
recordTyConInst :: Id
		-> [UniType]
		-> SpecM (Maybe [Maybe UniType])

recordTyConInst con tys
  = let
        spec_tys = specialiseConstrTys tys

	do_tycon_spec = maybeToBool (firstJust spec_tys)

        spec_exists = maybeToBool (lookupSpecEnv 
				      (getIdSpecialisation con) 
				      tys)
    in
    -- pprTrace "ConSpecExists?: "
    -- (ppAboves [ppStr (if spec_exists then "True" else "False"),
    --		  ppr PprShowAll con, ppCat (map (ppr PprDebug) tys)])
    (if (not spec_exists && do_tycon_spec)
     then returnSM (Just spec_tys)
     else returnSM Nothing)
\end{code}

