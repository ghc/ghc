(c) The University of Glasgow 2002

\begin{code}
module IfaceEnv (
	newGlobalBinder, newIPName, newImplicitBinder, 
	lookupIfaceTop, lookupIfaceExt,
	lookupOrig, lookupIfaceTc,
	newIfaceName, newIfaceNames,
	extendIfaceIdEnv, extendIfaceTyVarEnv,
	tcIfaceGlobal, tcIfaceTyCon, tcIfaceClass, tcIfaceExtId,
	tcIfaceTyVar, tcIfaceDataCon, tcIfaceLclId,

	-- Name-cache stuff
	allocateGlobalBinder, initNameCache
   ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcIface( tcImportDecl )

import TcRnMonad
import IfaceType	( IfaceExtName(..), IfaceTyCon(..), ifaceTyConName )
import HscTypes		( NameCache(..), HscEnv(..), 
			  TyThing, tyThingClass, tyThingTyCon, 
			  ExternalPackageState(..), OrigNameCache, lookupType )
import TyCon		( TyCon, tyConName )
import Class		( Class )
import DataCon		( DataCon, dataConWorkId, dataConName )
import Var		( TyVar, Id, varName )
import Name		( Name, nameUnique, nameModule, 
			  nameOccName, nameSrcLoc,
			  getOccName, nameParent_maybe,
		  	  isWiredInName, nameIsLocalOrFrom, mkIPName,
			  mkExternalName, mkInternalName )
import NameEnv
import OccName		( OccName, isTupleOcc_maybe, tcName, dataName,
			  lookupOccEnv, unitOccEnv, extendOccEnv, extendOccEnvList )
import PrelNames	( gHC_PRIM_Name, pREL_TUP_Name )
import TysWiredIn	( intTyCon, boolTyCon, charTyCon, listTyCon, parrTyCon, 
			  tupleTyCon, tupleCon )
import HscTypes		( ExternalPackageState, NameCache, TyThing(..) )
import Module		( Module, ModuleName, moduleName, mkPackageModule, 
			  emptyModuleEnv, lookupModuleEnvByName, extendModuleEnv_C )
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
  = case lookupOrigNameCache (nsNames name_supply) (moduleName mod) occ of
	-- A hit in the cache!  We are at the binding site of the name.
	-- This is the moment when we know the defining Module and SrcLoc
	-- of the Name, so we set these fields in the Name we return.
	--
	-- This is essential, to get the right Module in a Name.
	-- Also: then (bogus) multiple bindings of the same Name
	-- 		get different SrcLocs can can be reported as such.
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

lookupOrig :: ModuleName -> OccName -> TcRnIf a b Name
-- This one starts with a ModuleName, not a Module, because 
-- we may be simply looking at an occurrence M.x in an interface file.
-- We may enounter this well before finding the binding site for M.x
--
-- So, even if we get a miss in the original-name cache, we 
-- make a new External Name. 
-- We fake up 
--	Module to AnotherPackage
-- 	SrcLoc to noSrcLoc
--	Parent no Nothing
-- They'll be overwritten, in due course, by LoadIface.loadDecl.

lookupOrig mod_name occ 
  = do 	{ 	-- First ensure that mod_name and occ are evaluated
		-- If not, chaos can ensue:
		-- 	we read the name-cache
		-- 	then pull on mod (say)
		--	which does some stuff that modifies the name cache
		-- This did happen, with tycon_mod in TcIface.tcIfaceAlt (DataAlt..)
	  mod `seq` occ `seq` return ()	
    
	; name_supply <- getNameCache
    	; case lookupOrigNameCache (nsNames name_supply) mod_name occ of {
	      Just name -> returnM name ;
	      Nothing   -> do 

	{ let { (us', us1)      = splitUniqSupply (nsUniqs name_supply)
	      ;	uniq   	  	= uniqFromSupply us1
	      ;	name            = mkExternalName uniq tmp_mod occ Nothing noSrcLoc
	      ;	new_cache       = extend_name_cache (nsNames name_supply) tmp_mod occ name
	      ;	new_name_supply = name_supply {nsUniqs = us', nsNames = new_cache}
	      ;	tmp_mod    	= mkPackageModule mod_name 
			-- Guess at the package-ness for now, becuase we don't know whether
			-- this imported module is from the home package or not.
			-- If we ever need it, we'll open its interface, and update the cache
			-- with a better name (newGlobalBinder)
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
lookupOrigNameCache :: OrigNameCache -> ModuleName -> OccName -> Maybe Name
lookupOrigNameCache nc mod_name occ
  | mod_name == pREL_TUP_Name || mod_name == gHC_PRIM_Name,	-- Boxed tuples from one, 
    Just tup_info <- isTupleOcc_maybe occ			-- unboxed from the other
  = 	-- Special case for tuples; there are too many
	-- of them to pre-populate the original-name cache
    Just (mk_tup_name tup_info)
  where
    mk_tup_name (ns, boxity, arity)
	| ns == tcName   = tyConName (tupleTyCon boxity arity)
	| ns == dataName = dataConName (tupleCon boxity arity)
	| otherwise      = varName (dataConWorkId (tupleCon boxity arity))

lookupOrigNameCache nc mod_name occ	-- The normal case
  = case lookupModuleEnvByName nc mod_name of
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
		Getting from Names to TyThings
%*									*
%************************************************************************

\begin{code}
tcIfaceGlobal :: Name -> IfM a TyThing
tcIfaceGlobal name
  = do	{ eps <- getEps
	; hpt <- getHpt
	; case lookupType hpt (eps_PTE eps) name of {
	    Just thing -> return thing ;
	    Nothing    -> 

	setLclEnv () $ do	-- This gets us back to IfG, mainly to 
				-- pacify get_type_env; rather untidy
	{ env <- getGblEnv
	; case if_rec_types env of
	    Just (mod, get_type_env) 
		| nameIsLocalOrFrom mod name
		-> do 		-- It's defined in the module being compiled
	  	{ type_env <- get_type_env
		; case lookupNameEnv type_env name of
			Just thing -> return thing
			Nothing	   -> pprPanic "tcIfaceGlobal (local): not found:"  
						(ppr name $$ ppr type_env) }

	    other -> tcImportDecl name 	-- It's imported; go get it
    }}}

tcIfaceTyCon :: IfaceTyCon -> IfL TyCon
tcIfaceTyCon IfaceIntTc  = return intTyCon
tcIfaceTyCon IfaceBoolTc = return boolTyCon
tcIfaceTyCon IfaceCharTc = return charTyCon
tcIfaceTyCon IfaceListTc = return listTyCon
tcIfaceTyCon IfacePArrTc = return parrTyCon
tcIfaceTyCon (IfaceTupTc bx ar) = return (tupleTyCon bx ar)
tcIfaceTyCon (IfaceTc ext_nm) = do { name <- lookupIfaceExt ext_nm
				   ; thing <- tcIfaceGlobal name
				   ; return (tyThingTyCon thing) }

tcIfaceClass :: IfaceExtName -> IfL Class
tcIfaceClass rdr_name = do { name <- lookupIfaceExt rdr_name
			   ; thing <- tcIfaceGlobal name
			   ; return (tyThingClass thing) }

tcIfaceDataCon :: IfaceExtName -> IfL DataCon
tcIfaceDataCon gbl = do { name <- lookupIfaceExt gbl
			; thing <- tcIfaceGlobal name
		 	; case thing of
				ADataCon dc -> return dc
				other   -> pprPanic "tcIfaceExtDC" (ppr gbl $$ ppr name$$ ppr thing) }

tcIfaceExtId :: IfaceExtName -> IfL Id
tcIfaceExtId gbl = do { name <- lookupIfaceExt gbl
		      ; thing <- tcIfaceGlobal name
		      ; case thing of
			  AnId id -> return id
			  other   -> pprPanic "tcIfaceExtId" (ppr gbl $$ ppr name$$ ppr thing) }

------------------------------------------
tcIfaceLclId :: OccName -> IfL Id
tcIfaceLclId occ
  = do	{ lcl <- getLclEnv
	; return (lookupOccEnv (if_id_env lcl) occ
		  `orElse` 
		  pprPanic "tcIfaceLclId" (ppr occ)) }

tcIfaceTyVar :: OccName -> IfL TyVar
tcIfaceTyVar occ
  = do	{ lcl <- getLclEnv
	; return (lookupOccEnv (if_tv_env lcl) occ
		  `orElse`
		  pprPanic "tcIfaceTyVar" (ppr occ)) }

extendIfaceIdEnv :: [Id] -> IfL a -> IfL a
extendIfaceIdEnv ids thing_inside
  = do	{ env <- getLclEnv
	; let { id_env' = extendOccEnvList (if_id_env env) pairs
	      ;	pairs   = [(getOccName id, id) | id <- ids] }
	; setLclEnv (env { if_id_env = id_env' }) thing_inside }

extendIfaceTyVarEnv :: [TyVar] -> IfL a -> IfL a
extendIfaceTyVarEnv tyvars thing_inside
  = do	{ env <- getLclEnv
	; let { tv_env' = extendOccEnvList (if_tv_env env) pairs
	      ;	pairs   = [(getOccName tv, tv) | tv <- tyvars] }
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
