%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnEnv]{Environment manipulation for the renamer monad}

\begin{code}
module RnEnv where		-- Export everything

#include "HsVersions.h"

import {-# SOURCE #-} RnHiFiles( loadInterface )

import FlattenInfo      ( namesNeededForFlattening )
import HsSyn
import RdrHsSyn		( RdrNameHsType, RdrNameFixitySig, extractHsTyRdrTyVars )
import RdrName		( RdrName, rdrNameModule, rdrNameOcc, isQual, isUnqual, isOrig,
			  mkRdrUnqual, mkRdrQual, setRdrNameSpace, rdrNameOcc,
			  lookupRdrEnv, rdrEnvToList, elemRdrEnv, 
			  extendRdrEnv, addListToRdrEnv, emptyRdrEnv,
			  isExact_maybe, unqualifyRdrName
			)
import HsTypes		( hsTyVarName, replaceTyVarName )
import HscTypes		( Provenance(..), pprNameProvenance, hasBetterProv,
			  ImportReason(..), GlobalRdrEnv, GlobalRdrElt(..), 
			  GenAvailInfo(..), AvailInfo, Avails, 
			  ModIface(..), NameCache(..), OrigNameCache,
			  Deprecations(..), lookupDeprec, isLocalGRE,
			  extendLocalRdrEnv, availName, availNames,
			  lookupFixity
			)
import TcRnMonad
import Name		( Name, getName, nameIsLocalOrFrom, 
			  isWiredInName, mkInternalName, mkExternalName, mkIPName, 
			  nameSrcLoc, nameOccName, setNameSrcLoc, nameModule	)
import NameSet
import OccName		( OccName, tcName, isDataOcc, occNameFlavour, reportIfUnused )
import Module		( Module, ModuleName, moduleName, mkHomeModule,
			  lookupModuleEnv, lookupModuleEnvByName, extendModuleEnv_C )
import PrelNames	( mkUnboundName, intTyConName, 
			  boolTyConName, funTyConName,
			  unpackCStringName, unpackCStringFoldrName, unpackCStringUtf8Name,
			  eqStringName, printName, integerTyConName,
			  bindIOName, returnIOName, failIOName, thenIOName,
			  rOOT_MAIN_Name
			)
#ifdef GHCI	
import DsMeta		( templateHaskellNames, qTyConName )
#endif
import TysWiredIn	( unitTyCon )	-- A little odd
import Finder		( findModule )
import FiniteMap
import UniqSupply
import SrcLoc		( SrcLoc, importedSrcLoc )
import Outputable
import ListSetOps	( removeDups, equivClasses )
import BasicTypes	( mapIPName, FixitySig(..) )
import List		( nub )
import CmdLineOpts
import FastString	( FastString )
\end{code}

%*********************************************************
%*							*
\subsection{Making new names}
%*							*
%*********************************************************

\begin{code}
newTopBinder :: Module -> RdrName -> SrcLoc -> TcRn m Name
newTopBinder mod rdr_name loc
  | Just name <- isExact_maybe rdr_name
  = returnM name

  | isOrig rdr_name
  = ASSERT( rdr_mod == moduleName mod || rdr_mod == rOOT_MAIN_Name )
	-- When reading External Core we get Orig names as binders, 
	-- but they should agree with the module gotten from the monad
	--
	-- Except for the ":Main.main = ..." definition inserted into 
	-- the Main module
	--
	-- Because of this latter case, we take the module from the RdrName,
	-- not from the environment.  In principle, it'd be fine to have an
	-- arbitrary mixture of external core definitions in a single module,
	-- (apart from module-initialisation issues, perhaps).
    newGlobalName (mkHomeModule rdr_mod) (rdrNameOcc rdr_name) loc

  | otherwise
  = newGlobalName mod (rdrNameOcc rdr_name) loc
  where
    rdr_mod = rdrNameModule rdr_name

newGlobalName :: Module -> OccName -> SrcLoc -> TcRn m Name
newGlobalName mod occ loc
  = 	-- First check the cache
    getNameCache		`thenM` \ name_supply -> 
    case lookupOrigNameCache (nsNames name_supply) mod occ of

	-- A hit in the cache!  We are at the binding site of the name.
	-- This is the moment when we know the defining SrcLoc
	-- of the Name, so we set the SrcLoc of the name we return.
	--
	-- Main reason: then (bogus) multiple bindings of the same Name
	-- 		get different SrcLocs can can be reported as such.
	--
	-- Possible other reason: it might be in the cache because we
	-- 	encountered an occurrence before the binding site for an
	--	implicitly-imported Name.  Perhaps the current SrcLoc is
	--	better... but not really: it'll still just say 'imported'
	--
	-- IMPORTANT: Don't mess with wired-in names.  
	-- 	      Their wired-in-ness is in the SrcLoc

	Just name | isWiredInName name -> returnM name
		  | otherwise	       -> returnM (setNameSrcLoc name loc)
		     
	-- Miss in the cache!
	-- Build a completely new Name, and put it in the cache
	Nothing -> addNewName name_supply mod occ loc

-- Look up a "system name" in the name cache.
-- This is done by the type checker... 
lookupSysName :: Name			-- Base name
	      -> (OccName -> OccName) 	-- Occurrence name modifier
	      -> TcRn m Name		-- System name
lookupSysName base_name mk_sys_occ
  = newGlobalName (nameModule base_name)
		  (mk_sys_occ (nameOccName base_name))
		  (nameSrcLoc base_name)    


newGlobalNameFromRdrName rdr_name		-- Qualified original name
 = newGlobalName2 (rdrNameModule rdr_name) (rdrNameOcc rdr_name)

newGlobalName2 :: ModuleName -> OccName -> TcRn m Name
  -- This one starts with a ModuleName, not a Module, because 
  -- we may be simply looking at an occurrence M.x in an interface file.
  --
  -- Used for *occurrences*.  Even if we get a miss in the
  -- original-name cache, we make a new External Name.
  -- We get its Module either from the OrigNameCache, or (if this
  -- is the first Name from that module) from the Finder
  --
  -- In the case of a miss, we have to make up the SrcLoc, but that's
  -- OK: it must be an implicitly-imported Name, and that never occurs
  -- in an error message.

newGlobalName2 mod_name occ
  = getNameCache		`thenM` \ name_supply ->
    let
	new_name mod = addNewName name_supply mod occ importedSrcLoc
    in
    case lookupModuleEnvByName (nsNames name_supply) mod_name of
      Just (mod, occ_env) -> 	
	-- There are some names from this module already
	-- Next, look up in the OccNameEnv
	case lookupFM occ_env occ of
	     Just name -> returnM name
	     Nothing   -> new_name mod

      Nothing   -> 	-- No names from this module yet
	ioToTcRn (findModule mod_name)		`thenM` \ mb_loc ->
	case mb_loc of
	    Right (mod, _) -> new_name mod
	    Left files     -> 
		getDOpts `thenM` \ dflags ->
	        addErr (noIfaceErr dflags mod_name False files)	`thenM_`
			-- Things have really gone wrong at this point,
			-- so having the wrong package info in the 
			-- Module is the least of our worries.
		new_name (mkHomeModule mod_name)


newIPName rdr_name_ip
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
		     name_ip	 = mapIPName mk_name rdr_name_ip
		     mk_name rdr_name = mkIPName uniq (rdrNameOcc rdr_name)
		     new_ipcache = addToFM ipcache key name_ip
		     new_ns	 = name_supply {nsUniqs = us', nsIPs = new_ipcache}
    where 
	key = rdr_name_ip	-- Ensures that ?x and %x get distinct Names

-- A local helper function
addNewName name_supply mod occ loc
  = setNameCache new_name_supply 	`thenM_`
    returnM name
  where
    (new_name_supply, name) = newExternalName name_supply mod occ loc


newExternalName :: NameCache -> Module -> OccName -> SrcLoc 
		  -> (NameCache,Name)
-- Allocate a new unique, manufacture a new External Name,
-- put it in the cache, and return the two
newExternalName name_supply mod occ loc
  = (new_name_supply, name)
  where
     (us', us1)      = splitUniqSupply (nsUniqs name_supply)
     uniq   	     = uniqFromSupply us1
     name            = mkExternalName uniq mod occ loc
     new_cache       = extend_name_cache (nsNames name_supply) mod occ name
     new_name_supply = name_supply {nsUniqs = us', nsNames = new_cache}

lookupOrigNameCache :: OrigNameCache -> Module -> OccName -> Maybe Name
lookupOrigNameCache nc mod occ
  = case lookupModuleEnv nc mod of
	Nothing 	  -> Nothing
	Just (_, occ_env) -> lookupFM occ_env occ

extendOrigNameCache :: OrigNameCache -> Name -> OrigNameCache
extendOrigNameCache nc name 
  = extend_name_cache nc (nameModule name) (nameOccName name) name

extend_name_cache :: OrigNameCache -> Module -> OccName -> Name -> OrigNameCache
extend_name_cache nc mod occ name
  = extendModuleEnv_C combine nc mod (mod, unitFM occ name)
  where
    combine (mod, occ_env) _ = (mod, addToFM occ_env occ name)
\end{code}

%*********************************************************
%*							*
\subsection{Looking up names}
%*							*
%*********************************************************

Looking up a name in the RnEnv.

\begin{code}
lookupBndrRn rdr_name
  = getLocalRdrEnv		`thenM` \ local_env ->
    case lookupRdrEnv local_env rdr_name of 
	  Just name -> returnM name
	  Nothing   -> lookupTopBndrRn rdr_name

lookupTopBndrRn rdr_name
-- Look up a top-level local binder.   We may be looking up an unqualified 'f',
-- and there may be several imported 'f's too, which must not confuse us.
-- So we have to filter out the non-local ones.
-- A separate function (importsFromLocalDecls) reports duplicate top level
-- decls, so here it's safe just to choose an arbitrary one.

-- There should never be a qualified name in a binding position in Haskell,
-- but there can be if we have read in an external-Core file.
-- The Haskell parser checks for the illegal qualified name in Haskell 
-- source files, so we don't need to do so here.

  = getModeRn			`thenM` \ mode ->
    case mode of
	InterfaceMode mod -> 
	    getSrcLocM		`thenM` \ loc ->
	    newTopBinder mod rdr_name loc

	other -> lookupTopSrcBndr rdr_name

lookupTopSrcBndr :: RdrName -> TcRn m Name
lookupTopSrcBndr rdr_name
  = lookupTopSrcBndr_maybe rdr_name	`thenM` \ maybe_name ->
    case maybe_name of
	Just name -> returnM name
	Nothing	  -> unboundName rdr_name
		  	        

lookupTopSrcBndr_maybe :: RdrName -> TcRn m (Maybe Name)
-- Look up a source-code binder 

-- Ignores imported names; for example, this is OK:
--	import Foo( f )
--	infix 9 f	-- The 'f' here does not need to be qualified
--	f x = x		-- Nor here, of course

lookupTopSrcBndr_maybe rdr_name
  | Just name <- isExact_maybe rdr_name
	-- This is here just to catch the PrelBase defn of (say) [] and similar
	-- The parser reads the special syntax and returns an Exact RdrName
	-- But the global_env contains only Qual RdrNames, so we won't
	-- find it there; instead just get the name via the Orig route
	--
  	-- We are at a binding site for the name, so check first that it 
	-- the current module is the correct one; otherwise GHC can get
	-- very confused indeed.  This test rejects code like
	--	data T = (,) Int Int
	-- unless we are in GHC.Tup
  = getModule				`thenM` \ mod -> 
    checkErr (moduleName mod == moduleName (nameModule name))
	     (badOrigBinding rdr_name)	`thenM_`
    returnM (Just name)

  | otherwise
  = getGlobalRdrEnv			`thenM` \ global_env ->
    case lookupRdrEnv global_env rdr_name of
	  Nothing   -> returnM Nothing
	  Just gres -> case [gre_name gre | gre <- gres, isLocalGRE gre] of
			 []     -> returnM Nothing
			 (n:ns) -> returnM (Just n)
	      

-- lookupSigOccRn is used for type signatures and pragmas
-- Is this valid?
--   module A
--	import M( f )
--	f :: Int -> Int
--	f x = x
-- It's clear that the 'f' in the signature must refer to A.f
-- The Haskell98 report does not stipulate this, but it will!
-- So we must treat the 'f' in the signature in the same way
-- as the binding occurrence of 'f', using lookupBndrRn
lookupSigOccRn :: RdrName -> RnM Name
lookupSigOccRn = lookupBndrRn

-- lookupInstDeclBndr is used for the binders in an 
-- instance declaration.   Here we use the class name to
-- disambiguate.  

lookupInstDeclBndr :: Name -> RdrName -> RnM Name
	-- We use the selector name as the binder
lookupInstDeclBndr cls_name rdr_name
  | isUnqual rdr_name
  = 	-- Find all the things the class op name maps to
	-- and pick the one with the right parent name
    getGblEnv				`thenM` \ gbl_env ->
    let
	avail_env = imp_env (tcg_imports gbl_env)
        occ       = rdrNameOcc rdr_name
    in
    case lookupAvailEnv_maybe avail_env cls_name of
	Nothing -> 
	    -- If the class itself isn't in scope, then cls_name will
	    -- be unboundName, and there'll already be an error for
	    -- that in the error list.  Example:
	    -- e.g.   import Prelude hiding( Ord )
	    --	    instance Ord T where ...
	    -- The program is wrong, but that should not cause a crash.
		returnM (mkUnboundName rdr_name)

	Just (AvailTC _ ns) -> case [n | n <- ns, nameOccName n == occ] of
				(n:ns)-> ASSERT( null ns ) returnM n
				[]    -> unboundName rdr_name

	other		    -> pprPanic "lookupInstDeclBndr" (ppr cls_name)


  | otherwise	 	-- Occurs in derived instances, where we just
			-- refer directly to the right method, and avail_env
			-- isn't available
  = ASSERT2( not (isQual rdr_name), ppr rdr_name )
	  -- NB: qualified names are rejected by the parser
    lookupOrigName rdr_name


lookupSysBndr :: RdrName -> RnM Name
-- Used for the 'system binders' in a data type or class declaration
-- Do *not* look up in the RdrEnv; these system binders are never in scope
-- Instead, get the module from the monad... but remember that
-- where the module is depends on whether we are renaming source or 
-- interface file stuff
lookupSysBndr rdr_name
  = getSrcLocM		`thenM` \ loc ->
    getModeRn		`thenM` \ mode ->
    case mode of
	InterfaceMode mod -> newTopBinder mod rdr_name loc
	other 		  -> getModule	`thenM` \ mod ->
			     newTopBinder mod rdr_name loc

-- lookupOccRn looks up an occurrence of a RdrName
lookupOccRn :: RdrName -> RnM Name
lookupOccRn rdr_name
  = getLocalRdrEnv			`thenM` \ local_env ->
    case lookupRdrEnv local_env rdr_name of
	  Just name -> returnM name
	  Nothing   -> lookupGlobalOccRn rdr_name

-- lookupGlobalOccRn is like lookupOccRn, except that it looks in the global 
-- environment.  It's used only for
--	record field names
--	class op names in class and instance decls

lookupGlobalOccRn rdr_name
  = getModeRn 		`thenM` \ mode ->
    case mode of
	InterfaceMode mod -> lookupIfaceName mod rdr_name 
	SourceMode 	  -> lookupSrcName       rdr_name 

	CmdLineMode 
	 | not (isQual rdr_name) -> 
		lookupSrcName rdr_name

		-- We allow qualified names on the command line to refer to 
		-- *any* name exported by any module in scope, just as if 
		-- there was an "import qualified M" declaration for every 
		-- module.
		--
		-- First look up the name in the normal environment.  If
		-- it isn't there, we manufacture a new occurrence of an
		-- original name.
	 | otherwise -> 
		lookupSrcName_maybe rdr_name	`thenM` \ mb_name ->
		case mb_name of
		  Just name -> returnM name
		  Nothing   -> lookupQualifiedName rdr_name

-- A qualified name on the command line can refer to any module at all: we
-- try to load the interface if we don't already have it.
lookupQualifiedName :: RdrName -> TcRn m Name
lookupQualifiedName rdr_name
 = let 
       mod = rdrNameModule rdr_name
       occ = rdrNameOcc rdr_name
   in
   loadInterface (ppr rdr_name) mod (ImportByUser False) `thenM` \ iface ->
   case  [ name | (_,avails) <- mi_exports iface,
    	   avail	     <- avails,
    	   name 	     <- availNames avail,
    	   nameOccName name == occ ] of
      (n:ns) -> ASSERT (null ns) returnM n
      _      -> unboundName rdr_name

lookupSrcName :: RdrName -> TcRn m Name
lookupSrcName rdr_name
  = lookupSrcName_maybe rdr_name	`thenM` \ mb_name ->
    case mb_name of
	Nothing   -> unboundName rdr_name
	Just name -> returnM name
			
lookupSrcName_maybe :: RdrName -> TcRn m (Maybe Name)
lookupSrcName_maybe rdr_name
  | Just name <- isExact_maybe rdr_name	-- Can occur in source code too
  = returnM (Just name)

  | isOrig rdr_name			-- An original name
  = newGlobalNameFromRdrName rdr_name	`thenM` \ name ->
    returnM (Just name)

  | otherwise
  = lookupGRE rdr_name 	`thenM` \ mb_gre ->
    case mb_gre of
	Nothing  -> returnM Nothing
	Just gre -> returnM (Just (gre_name gre))

lookupGRE :: RdrName -> TcRn m (Maybe GlobalRdrElt)
lookupGRE rdr_name
  = getGlobalRdrEnv			`thenM` \ global_env ->
    case lookupRdrEnv global_env rdr_name of
	Just [gre] -> case gre_deprec gre of
			Nothing -> returnM (Just gre)
			Just _  -> warnDeprec gre	`thenM_`
				   returnM (Just gre)
	Just stuff@(gre : _) -> addNameClashErrRn rdr_name stuff	`thenM_`
			        returnM (Just gre)
	Nothing		     -> return Nothing
			
lookupIfaceName :: Module -> RdrName -> TcRn m Name
  	-- An Unqual is allowed; interface files contain 
	-- unqualified names for locally-defined things, such as
	-- constructors of a data type.
lookupIfaceName mod rdr_name
  | isUnqual rdr_name = newGlobalName mod (rdrNameOcc rdr_name) importedSrcLoc
  | otherwise	      = lookupOrigName rdr_name

lookupOrigName :: RdrName -> TcRn m Name
	-- Just for original or exact names
lookupOrigName rdr_name
  | Just n <- isExact_maybe rdr_name 
	-- This happens in derived code, which we 
	-- rename in InterfaceMode
  = returnM n

  | otherwise	-- Usually Orig, but can be a Qual when 
		-- we are reading a .hi-boot file
  = newGlobalNameFromRdrName rdr_name


dataTcOccs :: RdrName -> [RdrName]
-- If the input is a data constructor, return both it and a type
-- constructor.  This is useful when we aren't sure which we are
-- looking at.
--
-- ToDo: If the user typed "[]" or "(,,)", we'll generate an Exact RdrName,
--	 and we don't have a systematic way to find the TyCon's Name from
--	 the DataCon's name.  Sigh
dataTcOccs rdr_name
  | isDataOcc occ = [rdr_name_tc, rdr_name]
  | otherwise 	  = [rdr_name]
  where    
    occ 	= rdrNameOcc rdr_name
    rdr_name_tc = setRdrNameSpace rdr_name tcName
\end{code}

\begin{code}
unboundName rdr_name = addErr (unknownNameErr rdr_name)	`thenM_`
		       returnM (mkUnboundName rdr_name)
\end{code}

%*********************************************************
%*							*
		Fixities
%*							*
%*********************************************************

\begin{code}
--------------------------------
bindLocalFixities :: [RdrNameFixitySig] -> RnM a -> RnM a
-- Used for nested fixity decls
-- No need to worry about type constructors here,
-- Should check for duplicates but we don't
bindLocalFixities fixes thing_inside
  | null fixes = thing_inside
  | otherwise  = mappM rn_sig fixes	`thenM` \ new_bit ->
		 extendFixityEnv new_bit thing_inside
  where
    rn_sig (FixitySig v fix src_loc)
	= addSrcLoc src_loc $
	  lookupSigOccRn v		`thenM` \ new_v ->
	  returnM (new_v, FixitySig new_v fix src_loc)
\end{code}

--------------------------------
lookupFixity is a bit strange.  

* Nested local fixity decls are put in the local fixity env, which we
  find with getFixtyEnv

* Imported fixities are found in the HIT or PIT

* Top-level fixity decls in this module may be for Names that are
    either  Global	   (constructors, class operations)
    or 	    Local/Exported (everything else)
  (See notes with RnNames.getLocalDeclBinders for why we have this split.)
  We put them all in the local fixity environment

\begin{code}
lookupFixityRn :: Name -> RnM Fixity
lookupFixityRn name
  = getModule				`thenM` \ this_mod ->
    if nameIsLocalOrFrom this_mod name
    then	-- It's defined in this module
	getFixityEnv		`thenM` \ local_fix_env ->
	returnM (lookupFixity local_fix_env name)

    else	-- It's imported
      -- For imported names, we have to get their fixities by doing a
      -- loadHomeInterface, and consulting the Ifaces that comes back
      -- from that, because the interface file for the Name might not
      -- have been loaded yet.  Why not?  Suppose you import module A,
      -- which exports a function 'f', thus;
      --        module CurrentModule where
      --	  import A( f )
      -- 	module A( f ) where
      --	  import B( f )
      -- Then B isn't loaded right away (after all, it's possible that
      -- nothing from B will be used).  When we come across a use of
      -- 'f', we need to know its fixity, and it's then, and only
      -- then, that we load B.hi.  That is what's happening here.
        loadInterface doc name_mod ImportBySystem	`thenM` \ iface ->
	returnM (lookupFixity (mi_fixities iface) name)
  where
    doc      = ptext SLIT("Checking fixity for") <+> ppr name
    name_mod = moduleName (nameModule name)
\end{code}


%*********************************************************
%*							*
\subsection{Implicit free vars and sugar names}
%*							*
%*********************************************************

@getXImplicitFVs@ forces the renamer to slurp in some things which aren't
mentioned explicitly, but which might be needed by the type checker.

\begin{code}
implicitStmtFVs source_fvs 	-- Compiling a statement
  = stmt_fvs `plusFV` implicitModuleFVs source_fvs
  where
    stmt_fvs = mkFVs [printName, bindIOName, thenIOName, returnIOName, failIOName, 
		      integerTyConName]
		-- These are all needed implicitly when compiling a statement
		-- See TcModule.tc_stmts
	-- Reason for integerTyConName: consider this in GHCi
	--	ghci>  []
	-- We get an ambigous constraint (Show a), which we now default just like
	-- numeric types... but unless we have the instance decl for Integer we 
	-- won't find a valid default!

implicitModuleFVs source_fvs
  = mkTemplateHaskellFVs source_fvs 	`plusFV` 
    namesNeededForFlattening		`plusFV`
    ubiquitousNames


thProxyName :: NameSet
mkTemplateHaskellFVs :: NameSet -> NameSet
	-- This is a bit of a hack.  When we see the Template-Haskell construct
	--	[| expr |]
	-- we are going to need lots of the ``smart constructors'' defined in
	-- the main Template Haskell data type module.  Rather than treat them
	-- all as free vars at every occurrence site, we just make the Q type
	-- consructor a free var.... and then use that here to haul in the others

#ifdef GHCI
---------------	Template Haskell enabled --------------
thProxyName = unitFV qTyConName

mkTemplateHaskellFVs source_fvs
  | qTyConName `elemNameSet` source_fvs = templateHaskellNames
  | otherwise			        = emptyFVs

#else
---------------	Template Haskell disabled --------------

thProxyName 			= emptyFVs
mkTemplateHaskellFVs source_fvs = emptyFVs
#endif
--------------------------------------------------------

-- ubiquitous_names are loaded regardless, because 
-- they are needed in virtually every program
ubiquitousNames 
  = mkFVs [unpackCStringName, unpackCStringFoldrName, 
	   unpackCStringUtf8Name, eqStringName,
		-- Virtually every program has error messages in it somewhere
    	   getName unitTyCon, funTyConName, boolTyConName, intTyConName]
		-- Add occurrences for very frequently used types.
		--  	 (e.g. we don't want to be bothered with making 
		--	  funTyCon a free var at every function application!)
\end{code}

%************************************************************************
%*									*
\subsection{Re-bindable desugaring names}
%*									*
%************************************************************************

Haskell 98 says that when you say "3" you get the "fromInteger" from the
Standard Prelude, regardless of what is in scope.   However, to experiment
with having a language that is less coupled to the standard prelude, we're
trying a non-standard extension that instead gives you whatever "Prelude.fromInteger"
happens to be in scope.  Then you can
	import Prelude ()
	import MyPrelude as Prelude
to get the desired effect.

At the moment this just happens for
  * fromInteger, fromRational on literals (in expressions and patterns)
  * negate (in expressions)
  * minus  (arising from n+k patterns)
  * "do" notation

We store the relevant Name in the HsSyn tree, in 
  * HsIntegral/HsFractional	
  * NegApp
  * NPlusKPatIn
  * HsDo
respectively.  Initially, we just store the "standard" name (PrelNames.fromIntegralName,
fromRationalName etc), but the renamer changes this to the appropriate user
name if Opt_NoImplicitPrelude is on.  That is what lookupSyntaxName does.

We treat the orignal (standard) names as free-vars too, because the type checker
checks the type of the user thing against the type of the standard thing.

\begin{code}
lookupSyntaxName :: Name 			-- The standard name
	         -> RnM (Name, FreeVars)	-- Possibly a non-standard name
lookupSyntaxName std_name
  = doptM Opt_NoImplicitPrelude		`thenM` \ no_prelude -> 
    if not no_prelude then normal_case
    else
    getModeRn				`thenM` \ mode ->
    if isInterfaceMode mode then normal_case
	-- Happens for 'derived' code where we don't want to rebind
    else
	-- Get the similarly named thing from the local environment
    lookupOccRn (mkRdrUnqual (nameOccName std_name)) `thenM` \ usr_name ->
    returnM (usr_name, mkFVs [usr_name, std_name])
  where
    normal_case = returnM (std_name, unitFV std_name)

lookupSyntaxNames :: [Name]				-- Standard names
		  -> RnM (ReboundNames Name, FreeVars)	-- See comments with HsExpr.ReboundNames
lookupSyntaxNames std_names
  = doptM Opt_NoImplicitPrelude		`thenM` \ no_prelude -> 
    if not no_prelude then normal_case 
    else
    getModeRn				`thenM` \ mode ->
    if isInterfaceMode mode then normal_case
    else
    	-- Get the similarly named thing from the local environment
    mappM (lookupOccRn . mkRdrUnqual . nameOccName) std_names 	`thenM` \ usr_names ->

    returnM (std_names `zip` map HsVar usr_names, mkFVs std_names `plusFV` mkFVs usr_names)
  where
    normal_case = returnM (std_names `zip` map HsVar std_names, mkFVs std_names)
\end{code}


%*********************************************************
%*							*
\subsection{Binding}
%*							*
%*********************************************************

\begin{code}
newLocalsRn :: [(RdrName,SrcLoc)]
	    -> RnM [Name]
newLocalsRn rdr_names_w_loc
 =  newUniqueSupply 		`thenM` \ us ->
    let
	uniqs	   = uniqsFromSupply us
	names	   = [ mkInternalName uniq (rdrNameOcc rdr_name) loc
		     | ((rdr_name,loc), uniq) <- rdr_names_w_loc `zip` uniqs
		     ]
    in
    returnM names


bindLocatedLocalsRn :: SDoc	-- Documentation string for error message
	   	    -> [(RdrName,SrcLoc)]
	    	    -> ([Name] -> RnM a)
	    	    -> RnM a
bindLocatedLocalsRn doc_str rdr_names_w_loc enclosed_scope
  = getModeRn 			`thenM` \ mode ->
    getLocalRdrEnv		`thenM` \ local_env ->
    getGlobalRdrEnv		`thenM` \ global_env ->

	-- Check for duplicate names
    checkDupOrQualNames doc_str rdr_names_w_loc	`thenM_`

    	-- Warn about shadowing, but only in source modules
    let
      check_shadow (rdr_name,loc)
	|  rdr_name `elemRdrEnv` local_env 
 	|| rdr_name `elemRdrEnv` global_env 
	= addSrcLoc loc $ addWarn (shadowedNameWarn rdr_name)
        | otherwise 
	= returnM ()
    in

    (case mode of
	SourceMode -> ifOptM Opt_WarnNameShadowing	$
		      mappM_ check_shadow rdr_names_w_loc
	other	   -> returnM ()
    )					`thenM_`

    newLocalsRn rdr_names_w_loc		`thenM` \ names ->
    let
	new_local_env = addListToRdrEnv local_env (map fst rdr_names_w_loc `zip` names)
    in
    setLocalRdrEnv new_local_env (enclosed_scope names)

bindCoreLocalRn :: RdrName -> (Name -> RnM a) -> RnM a
  -- A specialised variant when renaming stuff from interface
  -- files (of which there is a lot)
  --	* one at a time
  --	* no checks for shadowing
  -- 	* always imported
  -- 	* deal with free vars
bindCoreLocalRn rdr_name enclosed_scope
  = getSrcLocM 		`thenM` \ loc ->
    getLocalRdrEnv		`thenM` \ name_env ->
    newUnique			`thenM` \ uniq ->
    let
	name	     = mkInternalName uniq (rdrNameOcc rdr_name) loc
	new_name_env = extendRdrEnv name_env rdr_name name
    in
    setLocalRdrEnv new_name_env (enclosed_scope name)

bindCoreLocalsRn []     thing_inside = thing_inside []
bindCoreLocalsRn (b:bs) thing_inside = bindCoreLocalRn b	$ \ name' ->
				       bindCoreLocalsRn bs	$ \ names' ->
				       thing_inside (name':names')

bindLocalNames names enclosed_scope
  = getLocalRdrEnv 		`thenM` \ name_env ->
    setLocalRdrEnv (extendLocalRdrEnv name_env names)
		    enclosed_scope

bindLocalNamesFV names enclosed_scope
  = bindLocalNames names $
    enclosed_scope `thenM` \ (thing, fvs) ->
    returnM (thing, delListFromNameSet fvs names)


-------------------------------------
bindLocalRn doc rdr_name enclosed_scope
  = getSrcLocM 				`thenM` \ loc ->
    bindLocatedLocalsRn doc [(rdr_name,loc)]	$ \ (n:ns) ->
    ASSERT( null ns )
    enclosed_scope n

bindLocalsRn doc rdr_names enclosed_scope
  = getSrcLocM		`thenM` \ loc ->
    bindLocatedLocalsRn doc
			(rdr_names `zip` repeat loc)
		 	enclosed_scope

	-- binLocalsFVRn is the same as bindLocalsRn
	-- except that it deals with free vars
bindLocalsFV doc rdr_names enclosed_scope
  = bindLocalsRn doc rdr_names		$ \ names ->
    enclosed_scope names		`thenM` \ (thing, fvs) ->
    returnM (thing, delListFromNameSet fvs names)

-------------------------------------
extendTyVarEnvFVRn :: [Name] -> RnM (a, FreeVars) -> RnM (a, FreeVars)
	-- This tiresome function is used only in rnSourceDecl on InstDecl
extendTyVarEnvFVRn tyvars enclosed_scope
  = bindLocalNames tyvars enclosed_scope 	`thenM` \ (thing, fvs) -> 
    returnM (thing, delListFromNameSet fvs tyvars)

bindTyVarsRn :: SDoc -> [HsTyVarBndr RdrName]
	      -> ([HsTyVarBndr Name] -> RnM a)
	      -> RnM a
bindTyVarsRn doc_str tyvar_names enclosed_scope
  = getSrcLocM					`thenM` \ loc ->
    let
	located_tyvars = [(hsTyVarName tv, loc) | tv <- tyvar_names] 
    in
    bindLocatedLocalsRn doc_str located_tyvars	$ \ names ->
    enclosed_scope (zipWith replaceTyVarName tyvar_names names)

bindPatSigTyVars :: [RdrNameHsType] -> ([Name] -> RnM a) -> RnM a
  -- Find the type variables in the pattern type 
  -- signatures that must be brought into scope

bindPatSigTyVars tys thing_inside
  = getLocalRdrEnv		`thenM` \ name_env ->
    getSrcLocM			`thenM` \ loc ->
    let
	forall_tyvars  = nub [ tv | ty <- tys,
				    tv <- extractHsTyRdrTyVars ty, 
				    not (tv `elemFM` name_env)
			 ]
		-- The 'nub' is important.  For example:
		--	f (x :: t) (y :: t) = ....
		-- We don't want to complain about binding t twice!

	located_tyvars = [(tv, loc) | tv <- forall_tyvars] 
	doc_sig        = text "In a pattern type-signature"
    in
    bindLocatedLocalsRn doc_sig located_tyvars thing_inside

bindPatSigTyVarsFV :: [RdrNameHsType]
		   -> RnM (a, FreeVars)
	  	   -> RnM (a, FreeVars)
bindPatSigTyVarsFV tys thing_inside
  = bindPatSigTyVars tys	$ \ tvs ->
    thing_inside		`thenM` \ (result,fvs) ->
    returnM (result, fvs `delListFromNameSet` tvs)

-------------------------------------
checkDupOrQualNames, checkDupNames :: SDoc
				   -> [(RdrName, SrcLoc)]
				   -> TcRn m ()
	-- Works in any variant of the renamer monad

checkDupOrQualNames doc_str rdr_names_w_loc
  =	-- Qualified names in patterns are now rejected by the parser
	-- but I'm not 100% certain that it finds all cases, so I've left
	-- this check in for now.  Should go eventually.
	--	Hmm.  Sooner rather than later.. data type decls
--     mappM_ (qualNameErr doc_str) quals 	`thenM_`
    checkDupNames doc_str rdr_names_w_loc
  where
    quals = filter (isQual . fst) rdr_names_w_loc
    
checkDupNames doc_str rdr_names_w_loc
  = 	-- Check for duplicated names in a binding group
    mappM_ (dupNamesErr doc_str) dups
  where
    (_, dups) = removeDups (\(n1,l1) (n2,l2) -> n1 `compare` n2) rdr_names_w_loc
\end{code}


%************************************************************************
%*									*
\subsection{GlobalRdrEnv}
%*									*
%************************************************************************

\begin{code}
mkGlobalRdrEnv :: ModuleName		-- Imported module (after doing the "as M" name change)
	       -> Bool			-- True <=> want unqualified import
	       -> (Name -> Provenance)
	       -> Avails		-- Whats imported
	       -> Deprecations
	       -> GlobalRdrEnv

mkGlobalRdrEnv this_mod unqual_imp mk_provenance avails deprecs
  = gbl_env2
  where
 	-- Make the name environment.  We're talking about a 
	-- single module here, so there must be no name clashes.
	-- In practice there only ever will be if it's the module
	-- being compiled.

	-- Add qualified names for the things that are available
	-- (Qualified names are always imported)
    gbl_env1 = foldl add_avail emptyRdrEnv avails

	-- Add unqualified names
    gbl_env2 | unqual_imp = foldl add_unqual gbl_env1 (rdrEnvToList gbl_env1)
	     | otherwise  = gbl_env1

    add_unqual env (qual_name, elts)
	= foldl add_one env elts
	where
	  add_one env elt = addOneToGlobalRdrEnv env unqual_name elt
	  unqual_name     = unqualifyRdrName qual_name
	-- The qualified import should only have added one 
	-- binding for each qualified name!  But if there's an error in
	-- the module (multiple bindings for the same name) we may get
	-- duplicates.  So the simple thing is to do the fold.

    add_avail :: GlobalRdrEnv -> AvailInfo -> GlobalRdrEnv
    add_avail env avail = foldl (add_name (availName avail)) env (availNames avail)

    add_name parent env name 	-- Add qualified name only
	= addOneToGlobalRdrEnv env (mkRdrQual this_mod occ) elt
	where
	  occ  = nameOccName name
	  elt  = GRE {gre_name   = name,
		      gre_parent = if name == parent 
				   then Nothing 
				   else Just parent, 
		      gre_prov   = mk_provenance name, 
		      gre_deprec = lookupDeprec deprecs name}
\end{code}

\begin{code}
plusGlobalRdrEnv :: GlobalRdrEnv -> GlobalRdrEnv -> GlobalRdrEnv
plusGlobalRdrEnv env1 env2 = plusFM_C combine_globals env1 env2

addOneToGlobalRdrEnv :: GlobalRdrEnv -> RdrName -> GlobalRdrElt -> GlobalRdrEnv
addOneToGlobalRdrEnv env rdr_name name = addToFM_C combine_globals env rdr_name [name]

delOneFromGlobalRdrEnv :: GlobalRdrEnv -> RdrName -> GlobalRdrEnv 
delOneFromGlobalRdrEnv env rdr_name = delFromFM env rdr_name

combine_globals :: [GlobalRdrElt] 	-- Old
		-> [GlobalRdrElt]	-- New
		-> [GlobalRdrElt]
combine_globals ns_old ns_new	-- ns_new is often short
  = foldr add ns_old ns_new
  where
    add n ns | any (is_duplicate n) ns_old = map (choose n) ns	-- Eliminate duplicates
	     | otherwise	           = n:ns

    choose n m | n `beats` m = n
	       | otherwise   = m

    g1 `beats` g2 = gre_name g1 == gre_name g2 && 
		    gre_prov g1 `hasBetterProv` gre_prov g2

    is_duplicate :: GlobalRdrElt -> GlobalRdrElt -> Bool
    is_duplicate g1 g2 | isLocalGRE g1 && isLocalGRE g2 = False
    is_duplicate g1 g2 = gre_name g1 == gre_name g2
\end{code}

We treat two bindings of a locally-defined name as a duplicate,
because they might be two separate, local defns and we want to report
and error for that, {\em not} eliminate a duplicate.

On the other hand, if you import the same name from two different
import statements, we {\em do} want to eliminate the duplicate, not report
an error.

If a module imports itself then there might be a local defn and an imported
defn of the same name; in this case the names will compare as equal, but
will still have different provenances.


%************************************************************************
%*									*
\subsection{Free variable manipulation}
%*									*
%************************************************************************

\begin{code}
-- A useful utility
mapFvRn f xs = mappM f xs	`thenM` \ stuff ->
	       let
		  (ys, fvs_s) = unzip stuff
	       in
	       returnM (ys, plusFVs fvs_s)
\end{code}


%************************************************************************
%*									*
\subsection{Envt utility functions}
%*									*
%************************************************************************

\begin{code}
warnUnusedModules :: [ModuleName] -> TcRn m ()
warnUnusedModules mods
  = ifOptM Opt_WarnUnusedImports (mappM_ (addWarn . unused_mod) mods)
  where
    unused_mod m = vcat [ptext SLIT("Module") <+> quotes (ppr m) <+> 
			   text "is imported, but nothing from it is used",
			 parens (ptext SLIT("except perhaps instances visible in") <+>
				   quotes (ppr m))]

warnUnusedImports, warnUnusedTopBinds :: [GlobalRdrElt] -> TcRn m ()
warnUnusedImports gres  = ifOptM Opt_WarnUnusedImports (warnUnusedGREs gres)
warnUnusedTopBinds gres = ifOptM Opt_WarnUnusedBinds   (warnUnusedGREs gres)

warnUnusedLocalBinds, warnUnusedMatches :: [Name] -> TcRn m ()
warnUnusedLocalBinds names = ifOptM Opt_WarnUnusedBinds   (warnUnusedLocals names)
warnUnusedMatches    names = ifOptM Opt_WarnUnusedMatches (warnUnusedLocals names)

-------------------------
--	Helpers
warnUnusedGREs   gres  = warnUnusedBinds [(n,p) | GRE {gre_name = n, gre_prov = p} <- gres]
warnUnusedLocals names = warnUnusedBinds [(n,LocalDef) | n<-names]

warnUnusedBinds :: [(Name,Provenance)] -> TcRn m ()
warnUnusedBinds names
  = mappM_ warnUnusedGroup groups
  where
	-- Group by provenance
   groups = equivClasses cmp (filter reportable names)
   (_,prov1) `cmp` (_,prov2) = prov1 `compare` prov2
 
   reportable (name,_) = reportIfUnused (nameOccName name)


-------------------------

warnUnusedGroup :: [(Name,Provenance)] -> TcRn m ()
warnUnusedGroup names
  = addSrcLoc def_loc	$
    addWarn		$
    sep [msg <> colon, nest 4 (fsep (punctuate comma (map (ppr.fst) names)))]
  where
    (name1, prov1) = head names
    loc1  	   = nameSrcLoc name1
    (def_loc, msg) = case prov1 of
			LocalDef 			   -> (loc1, unused_msg)
			NonLocalDef (UserImport mod loc _) -> (loc,  imp_from mod)

    unused_msg   = text "Defined but not used"
    imp_from mod = text "Imported from" <+> quotes (ppr mod) <+> text "but not used"
\end{code}

\begin{code}
addNameClashErrRn rdr_name (np1:nps)
  = addErr (vcat [ptext SLIT("Ambiguous occurrence") <+> quotes (ppr rdr_name),
		  ptext SLIT("It could refer to") <+> vcat (msg1 : msgs)])
  where
    msg1 = ptext  SLIT("either") <+> mk_ref np1
    msgs = [ptext SLIT("    or") <+> mk_ref np | np <- nps]
    mk_ref gre = quotes (ppr (gre_name gre)) <> comma <+> pprNameProvenance gre

shadowedNameWarn shadow
  = hsep [ptext SLIT("This binding for"), 
	       quotes (ppr shadow),
	       ptext SLIT("shadows an existing binding")]

unknownNameErr name
  = sep [text flavour, ptext SLIT("not in scope:"), quotes (ppr name)]
  where
    flavour = occNameFlavour (rdrNameOcc name)

badOrigBinding name
  = ptext SLIT("Illegal binding of built-in syntax:") <+> ppr (rdrNameOcc name)
	-- The rdrNameOcc is because we don't want to print Prelude.(,)

qualNameErr descriptor (name,loc)
  = addSrcLoc loc $
    addErr (vcat [ ptext SLIT("Invalid use of qualified name") <+> quotes (ppr name),
		     descriptor])

dupNamesErr descriptor ((name,loc) : dup_things)
  = addSrcLoc loc $
    addErr ((ptext SLIT("Conflicting definitions for") <+> quotes (ppr name))
	      $$ 
	      descriptor)

noIfaceErr dflags mod_name boot_file files
  = ptext SLIT("Could not find interface file for") <+> quotes (ppr mod_name)
    $$ extra
  where 
   extra
    | verbosity dflags < 3 = 
        text "(use -v to see a list of the files searched for)"
    | otherwise =
        hang (ptext SLIT("locations searched:")) 4 (vcat (map text files))

warnDeprec :: GlobalRdrElt -> TcRn m ()
warnDeprec (GRE {gre_name = name, gre_deprec = Just txt})
  = ifOptM Opt_WarnDeprecations	$
    addWarn (sep [ text (occNameFlavour (nameOccName name)) <+> 
		     quotes (ppr name) <+> text "is deprecated:", 
		     nest 4 (ppr txt) ])
\end{code}

