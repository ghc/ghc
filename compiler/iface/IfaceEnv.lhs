(c) The University of Glasgow 2002

\begin{code}
module IfaceEnv (
	newGlobalBinder, newIPName, newImplicitBinder, 
	lookupIfaceTop,
	lookupOrig, lookupOrigNameCache, extendNameCache,
	newIfaceName, newIfaceNames,
	extendIfaceIdEnv, extendIfaceTyVarEnv, 
	tcIfaceLclId,     tcIfaceTyVar, 

	ifaceExportNames,

	-- Name-cache stuff
	allocateGlobalBinder, initNameCache, 
        getNameCache, setNameCache
   ) where

#include "HsVersions.h"

import TcRnMonad
import TysWiredIn	( tupleTyCon, tupleCon )
import HscTypes		( NameCache(..), HscEnv(..), GenAvailInfo(..), 
			  IfaceExport, OrigNameCache, AvailInfo )
import Type		( mkOpenTvSubst, substTy )
import TyCon		( TyCon, tyConName )
import DataCon		( dataConWorkId, dataConName )
import Var		( TyVar, Id, varName )
import Name		( Name, nameUnique, nameModule,
			  nameOccName, nameSrcLoc, getOccName,
		  	  isWiredInName, mkIPName,
			  mkExternalName, mkInternalName )
import NameSet		( NameSet, emptyNameSet, addListToNameSet )
import OccName		( OccName, isTupleOcc_maybe, tcName, dataName, occNameFS,
			  lookupOccEnv, unitOccEnv, extendOccEnv )
import PrelNames	( gHC_PRIM, dATA_TUP )
import Module		( Module, emptyModuleEnv, ModuleName, modulePackageId,
			  lookupModuleEnv, extendModuleEnv_C, mkModule )
import UniqFM           ( lookupUFM, addListToUFM )
import FastString       ( FastString )
import UniqSupply	( UniqSupply, splitUniqSupply, uniqFromSupply, uniqsFromSupply )
import FiniteMap	( emptyFM, lookupFM, addToFM )
import BasicTypes	( IPName(..), mapIPName )
import SrcLoc		( SrcLoc, noSrcLoc )

import Outputable
\end{code}


%*********************************************************
%*							*
	Allocating new Names in the Name Cache
%*							*
%*********************************************************

\begin{code}
newGlobalBinder :: Module -> OccName -> SrcLoc -> TcRnIf a b Name
-- Used for source code and interface files, to make the
-- Name for a thing, given its Module and OccName
--
-- The cache may already already have a binding for this thing,
-- because we may have seen an occurrence before, but now is the
-- moment when we know its Module and SrcLoc in their full glory

newGlobalBinder mod occ loc
  = do	{ mod `seq` occ `seq` return ()	-- See notes with lookupOrig_help
	; traceIf (text "newGlobalBinder" <+> ppr mod <+> ppr occ <+> ppr loc)
    	; name_supply <- getNameCache
	; let (name_supply', name) = allocateGlobalBinder 
					name_supply mod occ
					loc
	; setNameCache name_supply'
	; return name }

allocateGlobalBinder
  :: NameCache 
  -> Module -> OccName -> SrcLoc 
  -> (NameCache, Name)
allocateGlobalBinder name_supply mod occ loc
  = case lookupOrigNameCache (nsNames name_supply) mod occ of
	-- A hit in the cache!  We are at the binding site of the name.
	-- This is the moment when we know the SrcLoc
	-- of the Name, so we set this field in the Name we return.
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
		    name'     = mkExternalName uniq mod occ loc
		    new_cache = extendNameCache (nsNames name_supply) mod occ name'
		    new_name_supply = name_supply {nsNames = new_cache}		     

	-- Miss in the cache!
	-- Build a completely new Name, and put it in the cache
	Nothing -> (new_name_supply, name)
		where
		  (us', us1)      = splitUniqSupply (nsUniqs name_supply)
		  uniq   	  = uniqFromSupply us1
		  name            = mkExternalName uniq mod occ loc
		  new_cache       = extendNameCache (nsNames name_supply) mod occ name
		  new_name_supply = name_supply {nsUniqs = us', nsNames = new_cache}


newImplicitBinder :: Name			-- Base name
	          -> (OccName -> OccName) 	-- Occurrence name modifier
	          -> TcRnIf m n Name		-- Implicit name
-- Called in BuildTyCl to allocate the implicit binders of type/class decls
-- For source type/class decls, this is the first occurrence
-- For iface ones, the LoadIface has alrady allocated a suitable name in the cache
newImplicitBinder base_name mk_sys_occ
  = newGlobalBinder (nameModule base_name)
		    (mk_sys_occ (nameOccName base_name))
		    (nameSrcLoc base_name)    

ifaceExportNames :: [IfaceExport] -> TcRnIf gbl lcl [AvailInfo]
ifaceExportNames exports = do
  mod_avails <- mapM (\(mod,avails) -> mapM (lookupAvail mod) avails) exports
  return (concat mod_avails)

-- Convert OccNames in GenAvailInfo to Names.
lookupAvail :: Module -> GenAvailInfo OccName -> TcRnIf a b AvailInfo
lookupAvail mod (Avail n) = do 
  n' <- lookupOrig mod n
  return (Avail n')
lookupAvail mod (AvailTC p_occ occs) = do
  p_name <- lookupOrig mod p_occ
  let lookup_sub occ | occ == p_occ = return p_name
                     | otherwise    = lookupOrig mod occ
  subs <- mappM lookup_sub occs
  return (AvailTC p_name subs)
	-- Remember that 'occs' is all the exported things, including
	-- the parent.  It's possible to export just class ops without
	-- the class, which shows up as C( op ) here. If the class was
	-- exported too we'd have C( C, op )

lookupOrig :: Module -> OccName ->  TcRnIf a b Name
lookupOrig mod occ
  = do 	{ 	-- First ensure that mod and occ are evaluated
		-- If not, chaos can ensue:
		-- 	we read the name-cache
		-- 	then pull on mod (say)
		--	which does some stuff that modifies the name cache
		-- This did happen, with tycon_mod in TcIface.tcIfaceAlt (DataAlt..)
	  mod `seq` occ `seq` return ()	
	; traceIf (text "lookup_orig" <+> ppr mod <+> ppr occ)
    
	; name_cache <- getNameCache
    	; case lookupOrigNameCache (nsNames name_cache) mod occ of {
	      Just name -> returnM name;
	      Nothing   ->
              let
                us        = nsUniqs name_cache
                uniq      = uniqFromSupply us
                name      = mkExternalName uniq mod occ noSrcLoc
                new_cache = extendNameCache (nsNames name_cache) mod occ name
              in
              case splitUniqSupply us of { (us',_) -> do
                setNameCache name_cache{ nsUniqs = us', nsNames = new_cache }
                return name
    }}}

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
  | mod == dATA_TUP || mod == gHC_PRIM,		-- Boxed tuples from one, 
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
  = extendNameCache nc (nameModule name) (nameOccName name) name

extendNameCache :: OrigNameCache -> Module -> OccName -> Name -> OrigNameCache
extendNameCache nc mod occ name
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
lookupIfaceTop :: OccName -> IfL Name
-- Look up a top-level name from the current Iface module
lookupIfaceTop occ
  = do	{ env <- getLclEnv; lookupOrig (if_mod env) occ }

lookupHomePackage :: ModuleName -> OccName -> IfL Name
lookupHomePackage mod_name occ
  = do	{ env <- getLclEnv; 
        ; let this_pkg = modulePackageId (if_mod env)
        ; lookupOrig (mkModule this_pkg mod_name) occ }

newIfaceName :: OccName -> IfL Name
newIfaceName occ
  = do	{ uniq <- newUnique
	; return $! mkInternalName uniq occ noSrcLoc }

newIfaceNames :: [OccName] -> IfL [Name]
newIfaceNames occs
  = do	{ uniqs <- newUniqueSupply
	; return [ mkInternalName uniq occ noSrcLoc
		 | (occ,uniq) <- occs `zip` uniqsFromSupply uniqs] }
\end{code}
