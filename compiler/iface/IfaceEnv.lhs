(c) The University of Glasgow 2002

\begin{code}
module IfaceEnv (
	newGlobalBinder, newIPName, newImplicitBinder, 
	lookupIfaceTop, lookupIfaceExt,
	lookupOrig, lookupIfaceTc,
	newIfaceName, newIfaceNames,
	extendIfaceIdEnv, extendIfaceTyVarEnv, refineIfaceIdEnv,
	tcIfaceLclId,     tcIfaceTyVar, 

	lookupAvail, ifaceExportNames,

	-- Name-cache stuff
	allocateGlobalBinder, initNameCache, 
   ) where

#include "HsVersions.h"

import TcRnMonad
import IfaceType	( IfaceExtName(..), IfaceTyCon(..), ifaceTyConName )
import TysWiredIn	( tupleTyCon, tupleCon )
import HscTypes		( NameCache(..), HscEnv(..), GenAvailInfo(..), 
			  IfaceExport, OrigNameCache )
import Type		( mkOpenTvSubst, substTy )
import TyCon		( TyCon, tyConName )
import Unify		( TypeRefinement )
import DataCon		( dataConWorkId, dataConName )
import Var		( TyVar, Id, varName, setIdType, idType )
import Name		( Name, nameUnique, nameModule, 
			  nameOccName, nameSrcLoc, 
			  getOccName, nameParent_maybe,
		  	  isWiredInName, mkIPName,
			  mkExternalName, mkInternalName )
import NameSet		( NameSet, emptyNameSet, addListToNameSet )
import OccName		( OccName, isTupleOcc_maybe, tcName, dataName, mapOccEnv, occNameFS,
			  lookupOccEnv, unitOccEnv, extendOccEnv, extendOccEnvList )
import PrelNames	( gHC_PRIM, pREL_TUP )
import Module		( Module, emptyModuleEnv, 
			  lookupModuleEnv, extendModuleEnv_C )
import UniqFM           ( lookupUFM, addListToUFM )
import FastString       ( FastString )
import UniqSupply	( UniqSupply, splitUniqSupply, uniqFromSupply, uniqsFromSupply )
import FiniteMap	( emptyFM, lookupFM, addToFM )
import BasicTypes	( IPName(..), mapIPName )
import SrcLoc		( SrcLoc, noSrcLoc )
import Maybes		( orElse )

import Outputable
\end{code}


%*********************************************************
%*							*
	Allocating new Names in the Name Cache
%*							*
%*********************************************************

\begin{code}
newGlobalBinder :: Module -> OccName -> Maybe Name -> SrcLoc -> TcRnIf a b Name
-- Used for source code and interface files, to make the
-- Name for a thing, given its Module and OccName
--
-- The cache may already already have a binding for this thing,
-- because we may have seen an occurrence before, but now is the
-- moment when we know its Module and SrcLoc in their full glory

newGlobalBinder mod occ mb_parent loc
  = do	{ mod `seq` occ `seq` return ()	-- See notes with lookupOrig_help
	-- ; traceIf (text "newGlobalBinder" <+> ppr mod <+> ppr occ <+> ppr loc)
    	; name_supply <- getNameCache
	; let (name_supply', name) = allocateGlobalBinder 
					name_supply mod occ
					mb_parent loc
	; setNameCache name_supply'
	; return name }

allocateGlobalBinder
  :: NameCache 
  -> Module -> OccName -> Maybe Name -> SrcLoc 
  -> (NameCache, Name)
allocateGlobalBinder name_supply mod occ mb_parent loc
  = case lookupOrigNameCache (nsNames name_supply) mod occ of
	-- A hit in the cache!  We are at the binding site of the name.
	-- This is the moment when we know the defining parent and SrcLoc
	-- of the Name, so we set these fields in the Name we return.
	--
	-- Then (bogus) multiple bindings of the same Name
	-- get different SrcLocs can can be reported as such.
	--
	-- Possible other reason: it might be in the cache because we
	-- 	encountered an occurrence before the binding site for an
	--	implicitly-imported Name.  Perhaps the current SrcLoc is
	--	better... but not really: it'll still just say 'imported'
	--
	-- IMPORTANT: Don't mess with wired-in names.  
	-- 	      Their wired-in-ness is in their NameSort
	--	      and their Module is correct.

	Just name | isWiredInName name -> (name_supply, name)
		  | otherwise -> (new_name_supply, name')
		  where
		    uniq      = nameUnique name
		    name'     = mkExternalName uniq mod occ mb_parent loc
		    new_cache = extend_name_cache (nsNames name_supply) mod occ name'
		    new_name_supply = name_supply {nsNames = new_cache}		     

	-- Miss in the cache!
	-- Build a completely new Name, and put it in the cache
	Nothing -> (new_name_supply, name)
		where
		  (us', us1)      = splitUniqSupply (nsUniqs name_supply)
		  uniq   	  = uniqFromSupply us1
		  name            = mkExternalName uniq mod occ mb_parent loc
		  new_cache       = extend_name_cache (nsNames name_supply) mod occ name
		  new_name_supply = name_supply {nsUniqs = us', nsNames = new_cache}


newImplicitBinder :: Name			-- Base name
	          -> (OccName -> OccName) 	-- Occurrence name modifier
	          -> TcRnIf m n Name		-- Implicit name
-- Called in BuildTyCl to allocate the implicit binders of type/class decls
-- For source type/class decls, this is the first occurrence
-- For iface ones, the LoadIface has alrady allocated a suitable name in the cache
--
-- An *implicit* name has the base-name as parent
newImplicitBinder base_name mk_sys_occ
  = newGlobalBinder (nameModule base_name)
		    (mk_sys_occ (nameOccName base_name))
		    (Just parent_name)
		    (nameSrcLoc base_name)    
  where
    parent_name = case nameParent_maybe base_name of
		    Just parent_name  -> parent_name
		    Nothing 	      -> base_name

ifaceExportNames :: [IfaceExport] -> TcRnIf gbl lcl NameSet
ifaceExportNames exports 
  = foldlM do_one emptyNameSet exports
  where
    do_one acc (mod, exports)  = foldlM (do_avail mod) acc exports
    do_avail mod acc avail = do { ns <- lookupAvail mod avail
				; return (addListToNameSet acc ns) }

lookupAvail :: Module -> GenAvailInfo OccName -> TcRnIf a b [Name]
-- Find all the names arising from an import
-- Make sure the parent info is correct, even though we may not
-- yet have read the interface for this module
lookupAvail mod (Avail n) = do { n' <- lookupOrig mod n; 
			       ; return [n'] }
lookupAvail mod (AvailTC p_occ occs) 
  = do { p_name <- lookupOrig mod p_occ
       ; let lookup_sub occ | occ == p_occ = return p_name
			    | otherwise	   = lookup_orig mod occ (Just p_name)
       ; mappM lookup_sub occs }
	-- Remember that 'occs' is all the exported things, including
	-- the parent.  It's possible to export just class ops without
	-- the class, via C( op ). If the class was exported too we'd
	-- have C( C, op )

	-- The use of lookupOrigSub here (rather than lookupOrig) 
	-- ensures that the subordinate names record their parent; 
	-- and that in turn ensures that the GlobalRdrEnv
	-- has the correct parent for all the names in its range.
	-- For imported things, we may only suck in the interface later, if ever.
	-- Reason for all this:
	--   Suppose module M exports type A.T, and constructor A.MkT
	--   Then, we know that A.MkT is a subordinate name of A.T,
	--   even though we aren't at the binding site of A.T
	--   And it's important, because we may simply re-export A.T
	--   without ever sucking in the declaration itself.


lookupOrig :: Module -> OccName -> TcRnIf a b Name
-- Even if we get a miss in the original-name cache, we 
-- make a new External Name. 
-- We fake up 
-- 	SrcLoc to noSrcLoc
--	Parent no Nothing
-- They'll be overwritten, in due course, by LoadIface.loadDecl.
lookupOrig mod occ = lookup_orig mod occ Nothing

lookup_orig :: Module -> OccName ->  Maybe Name -> TcRnIf a b Name
-- Used when we know the parent of the thing we are looking up
lookup_orig mod occ mb_parent
  = do 	{ 	-- First ensure that mod and occ are evaluated
		-- If not, chaos can ensue:
		-- 	we read the name-cache
		-- 	then pull on mod (say)
		--	which does some stuff that modifies the name cache
		-- This did happen, with tycon_mod in TcIface.tcIfaceAlt (DataAlt..)
	  mod `seq` occ `seq` return ()	
    
	; name_supply <- getNameCache
    	; case lookupOrigNameCache (nsNames name_supply) mod occ of {
	      Just name -> returnM name ;
	      Nothing   -> do 

	{ let { (us', us1)      = splitUniqSupply (nsUniqs name_supply)
	      ;	uniq   	  	= uniqFromSupply us1
	      ;	name            = mkExternalName uniq mod occ mb_parent noSrcLoc
	      ;	new_cache       = extend_name_cache (nsNames name_supply) mod occ name
	      ;	new_name_supply = name_supply {nsUniqs = us', nsNames = new_cache}
	  }
	; setNameCache new_name_supply
	; return name }
    }}

newIPName :: IPName OccName -> TcRnIf m n (IPName Name)
newIPName occ_name_ip
  = getNameCache		`thenM` \ name_supply ->
    let
	ipcache = nsIPs name_supply
    in
    case lookupFM ipcache key of
	Just name_ip -> returnM name_ip
	Nothing      -> setNameCache new_ns 	`thenM_`
		        returnM name_ip
		  where
		     (us', us1)  = splitUniqSupply (nsUniqs name_supply)
		     uniq   	 = uniqFromSupply us1
		     name_ip	 = mapIPName (mkIPName uniq) occ_name_ip
		     new_ipcache = addToFM ipcache key name_ip
		     new_ns	 = name_supply {nsUniqs = us', nsIPs = new_ipcache}
    where 
	key = occ_name_ip	-- Ensures that ?x and %x get distinct Names
\end{code}

	Local helper functions (not exported)

\begin{code}
lookupOrigNameCache :: OrigNameCache -> Module -> OccName -> Maybe Name
lookupOrigNameCache nc mod occ
  | mod == pREL_TUP || mod == gHC_PRIM,		-- Boxed tuples from one, 
    Just tup_info <- isTupleOcc_maybe occ	-- unboxed from the other
  = 	-- Special case for tuples; there are too many
	-- of them to pre-populate the original-name cache
    Just (mk_tup_name tup_info)
  where
    mk_tup_name (ns, boxity, arity)
	| ns == tcName   = tyConName (tupleTyCon boxity arity)
	| ns == dataName = dataConName (tupleCon boxity arity)
	| otherwise      = varName (dataConWorkId (tupleCon boxity arity))

lookupOrigNameCache nc mod occ	-- The normal case
  = case lookupModuleEnv nc mod of
	Nothing      -> Nothing
	Just occ_env -> lookupOccEnv occ_env occ

extendOrigNameCache :: OrigNameCache -> Name -> OrigNameCache
extendOrigNameCache nc name 
  = extend_name_cache nc (nameModule name) (nameOccName name) name

extend_name_cache :: OrigNameCache -> Module -> OccName -> Name -> OrigNameCache
extend_name_cache nc mod occ name
  = extendModuleEnv_C combine nc mod (unitOccEnv occ name)
  where
    combine occ_env _ = extendOccEnv occ_env occ name

getNameCache :: TcRnIf a b NameCache
getNameCache = do { HscEnv { hsc_NC = nc_var } <- getTopEnv; 
		    readMutVar nc_var }

setNameCache :: NameCache -> TcRnIf a b ()
setNameCache nc = do { HscEnv { hsc_NC = nc_var } <- getTopEnv; 
		       writeMutVar nc_var nc }
\end{code}


\begin{code}
initNameCache :: UniqSupply -> [Name] -> NameCache
initNameCache us names
  = NameCache { nsUniqs = us,
		nsNames = initOrigNames names,
		nsIPs   = emptyFM }

initOrigNames :: [Name] -> OrigNameCache
initOrigNames names = foldl extendOrigNameCache emptyModuleEnv names
\end{code}



%************************************************************************
%*									*
		Type variables and local Ids
%*									*
%************************************************************************

\begin{code}
tcIfaceLclId :: FastString -> IfL Id
tcIfaceLclId occ
  = do	{ lcl <- getLclEnv
	; case (lookupUFM (if_id_env lcl) occ) of
            Just ty_var -> return ty_var
            Nothing     -> failIfM (text "Iface id out of scope: " <+> ppr occ)
        }

refineIfaceIdEnv :: TypeRefinement -> IfL a -> IfL a
refineIfaceIdEnv (tv_subst, _) thing_inside
  = do	{ env <- getLclEnv
	; let { id_env' = mapOccEnv refine_id (if_id_env env)
	      ;	refine_id id = setIdType id (substTy subst (idType id))
	      ;	subst = mkOpenTvSubst tv_subst }
	; setLclEnv (env { if_id_env = id_env' }) thing_inside }
	
extendIfaceIdEnv :: [Id] -> IfL a -> IfL a
extendIfaceIdEnv ids thing_inside
  = do	{ env <- getLclEnv
	; let { id_env' = addListToUFM (if_id_env env) pairs
	      ;	pairs   = [(occNameFS (getOccName id), id) | id <- ids] }
	; setLclEnv (env { if_id_env = id_env' }) thing_inside }


tcIfaceTyVar :: FastString -> IfL TyVar
tcIfaceTyVar occ
  = do	{ lcl <- getLclEnv
	; case (lookupUFM (if_tv_env lcl) occ) of
            Just ty_var -> return ty_var
            Nothing     -> failIfM (text "Iface type variable out of scope: " <+> ppr occ)
        }

extendIfaceTyVarEnv :: [TyVar] -> IfL a -> IfL a
extendIfaceTyVarEnv tyvars thing_inside
  = do	{ env <- getLclEnv
	; let { tv_env' = addListToUFM (if_tv_env env) pairs
	      ;	pairs   = [(occNameFS (getOccName tv), tv) | tv <- tyvars] }
	; setLclEnv (env { if_tv_env = tv_env' }) thing_inside }
\end{code}


%************************************************************************
%*									*
		Getting from RdrNames to Names
%*									*
%************************************************************************

\begin{code}
lookupIfaceTc :: IfaceTyCon -> IfL Name
lookupIfaceTc (IfaceTc ext) = lookupIfaceExt ext
lookupIfaceTc other_tc	    = return (ifaceTyConName other_tc)

lookupIfaceExt :: IfaceExtName -> IfL Name
lookupIfaceExt (ExtPkg  mod occ)   = lookupOrig mod occ
lookupIfaceExt (HomePkg mod occ _) = lookupOrig mod occ
lookupIfaceExt (LocalTop occ)	   = lookupIfaceTop occ
lookupIfaceExt (LocalTopSub occ _) = lookupIfaceTop occ

lookupIfaceTop :: OccName -> IfL Name
-- Look up a top-level name from the current Iface module
lookupIfaceTop occ
  = do	{ env <- getLclEnv; lookupOrig (if_mod env) occ }

newIfaceName :: OccName -> IfL Name
newIfaceName occ
  = do	{ uniq <- newUnique
	; return (mkInternalName uniq occ noSrcLoc) }

newIfaceNames :: [OccName] -> IfL [Name]
newIfaceNames occs
  = do	{ uniqs <- newUniqueSupply
	; return [ mkInternalName uniq occ noSrcLoc
		 | (occ,uniq) <- occs `zip` uniqsFromSupply uniqs] }
\end{code}
