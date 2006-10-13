%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-2006
%
\section[RnEnv]{Environment manipulation for the renamer monad}

\begin{code}
module RnEnv ( 
	newTopSrcBinder, lookupFamInstDeclBndr,
	lookupLocatedBndrRn, lookupBndrRn, 
	lookupLocatedTopBndrRn, lookupTopBndrRn,
	lookupLocatedOccRn, lookupOccRn, 
	lookupLocatedGlobalOccRn, lookupGlobalOccRn,
	lookupLocalDataTcNames, lookupSrcOcc_maybe,
	lookupFixityRn, lookupTyFixityRn, lookupLocatedSigOccRn, 
	lookupLocatedInstDeclBndr,
	lookupSyntaxName, lookupSyntaxTable, lookupImportedName,
	lookupGreRn, lookupGreRn_maybe,

	newLocalsRn, newIPNameRn,
	bindLocalNames, bindLocalNamesFV,
	bindLocatedLocalsFV, bindLocatedLocalsRn,
	bindSigTyVarsFV, bindPatSigTyVars, bindPatSigTyVarsFV,
	bindTyVarsRn, extendTyVarEnvFVRn,
	bindLocalFixities,

	checkDupNames, mapFvRn,
	warnUnusedMatches, warnUnusedModules, warnUnusedImports, 
	warnUnusedTopBinds, warnUnusedLocalBinds,
	dataTcOccs, unknownNameErr,
    ) where

#include "HsVersions.h"

import LoadIface	( loadInterfaceForName, loadSrcInterface )
import IfaceEnv		( lookupOrig, newGlobalBinder, newIPName )
import HsSyn		( FixitySig(..), HsExpr(..), SyntaxExpr, SyntaxTable,
			  LHsTyVarBndr, LHsType, 
			  Fixity, hsLTyVarLocNames, replaceTyVarName )
import RdrHsSyn		( extractHsTyRdrTyVars )
import RdrName		( RdrName, isQual, isUnqual, isOrig_maybe,
			  isQual_maybe,
			  mkRdrUnqual, setRdrNameSpace, rdrNameOcc,
			  pprGlobalRdrEnv, lookupGRE_RdrName, 
			  isExact_maybe, isSrcRdrName,
			  Parent(..),
			  GlobalRdrElt(..), GlobalRdrEnv, lookupGlobalRdrEnv, 
			  isLocalGRE, extendLocalRdrEnv, elemLocalRdrEnv, lookupLocalRdrEnv,
			  Provenance(..), pprNameProvenance,
			  importSpecLoc, importSpecModule
			)
import HscTypes		( availNames, ModIface(..), FixItem(..), lookupFixity )
import TcRnMonad
import Name		( Name, nameIsLocalOrFrom, mkInternalName, isWiredInName,
			  nameSrcLoc, nameOccName, nameModule, isExternalName )
import NameSet
import OccName		( tcName, isDataOcc, pprNonVarNameSpace, occNameSpace,
			  reportIfUnused )
import Module		( Module, ModuleName )
import PrelNames	( mkUnboundName, rOOT_MAIN, iNTERACTIVE, consDataConKey, hasKey )
import UniqSupply
import BasicTypes	( IPName, mapIPName )
import SrcLoc		( SrcSpan, srcSpanStart, Located(..), eqLocated, unLoc,
			  srcLocSpan, getLoc, combineSrcSpans, isOneLineSpan )
import Outputable
import Util		( sortLe )
import ListSetOps	( removeDups )
import List		( nubBy )
import Monad		( when )
import DynFlags
\end{code}

%*********************************************************
%*							*
		Source-code binders
%*							*
%*********************************************************

\begin{code}
newTopSrcBinder :: Module -> Located RdrName -> RnM Name
newTopSrcBinder this_mod (L loc rdr_name)
  | Just name <- isExact_maybe rdr_name
  =	-- This is here to catch 
	--   (a) Exact-name binders created by Template Haskell
	--   (b) The PrelBase defn of (say) [] and similar, for which
	--	 the parser reads the special syntax and returns an Exact RdrName
   	-- We are at a binding site for the name, so check first that it 
	-- the current module is the correct one; otherwise GHC can get
	-- very confused indeed. This test rejects code like
	--	data T = (,) Int Int
	-- unless we are in GHC.Tup
    ASSERT2( isExternalName name,  ppr name )
    do	{ checkM (this_mod == nameModule name)
	         (addErrAt loc (badOrigBinding rdr_name))
	; return name }


  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do	{ checkM (rdr_mod == this_mod || rdr_mod == rOOT_MAIN)
	         (addErrAt loc (badOrigBinding rdr_name))
	-- When reading External Core we get Orig names as binders, 
	-- but they should agree with the module gotten from the monad
	--
	-- We can get built-in syntax showing up here too, sadly.  If you type
	--	data T = (,,,)
	-- the constructor is parsed as a type, and then RdrHsSyn.tyConToDataCon 
	-- uses setRdrNameSpace to make it into a data constructors.  At that point
	-- the nice Exact name for the TyCon gets swizzled to an Orig name.
	-- Hence the badOrigBinding error message.
	--
	-- Except for the ":Main.main = ..." definition inserted into 
	-- the Main module; ugh!

	-- Because of this latter case, we call newGlobalBinder with a module from 
	-- the RdrName, not from the environment.  In principle, it'd be fine to 
	-- have an arbitrary mixture of external core definitions in a single module,
	-- (apart from module-initialisation issues, perhaps).
	; newGlobalBinder rdr_mod rdr_occ (srcSpanStart loc) }
		--TODO, should pass the whole span

  | otherwise
  = do	{ checkM (not (isQual rdr_name))
	         (addErrAt loc (badQualBndrErr rdr_name))
	 	-- Binders should not be qualified; if they are, and with a different
		-- module name, we we get a confusing "M.T is not in scope" error later
	; newGlobalBinder this_mod (rdrNameOcc rdr_name) (srcSpanStart loc) }
\end{code}

%*********************************************************
%*							*
	Source code occurrences
%*							*
%*********************************************************

Looking up a name in the RnEnv.

\begin{code}
lookupLocatedBndrRn :: Located RdrName -> RnM (Located Name)
lookupLocatedBndrRn = wrapLocM lookupBndrRn

lookupBndrRn :: RdrName -> RnM Name
-- NOTE: assumes that the SrcSpan of the binder has already been setSrcSpan'd
lookupBndrRn rdr_name
  = getLocalRdrEnv		`thenM` \ local_env ->
    case lookupLocalRdrEnv local_env rdr_name of 
	  Just name -> returnM name
	  Nothing   -> lookupTopBndrRn rdr_name

lookupLocatedTopBndrRn :: Located RdrName -> RnM (Located Name)
lookupLocatedTopBndrRn = wrapLocM lookupTopBndrRn

lookupTopBndrRn :: RdrName -> RnM Name
-- Look up a top-level source-code binder.   We may be looking up an unqualified 'f',
-- and there may be several imported 'f's too, which must not confuse us.
-- For example, this is OK:
--	import Foo( f )
--	infix 9 f	-- The 'f' here does not need to be qualified
--	f x = x		-- Nor here, of course
-- So we have to filter out the non-local ones.
--
-- A separate function (importsFromLocalDecls) reports duplicate top level
-- decls, so here it's safe just to choose an arbitrary one.
--
-- There should never be a qualified name in a binding position in Haskell,
-- but there can be if we have read in an external-Core file.
-- The Haskell parser checks for the illegal qualified name in Haskell 
-- source files, so we don't need to do so here.

lookupTopBndrRn rdr_name
  | Just name <- isExact_maybe rdr_name
  = returnM name

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name	
	-- This deals with the case of derived bindings, where
	-- we don't bother to call newTopSrcBinder first
	-- We assume there is no "parent" name
  = do	{ loc <- getSrcSpanM
	; newGlobalBinder rdr_mod rdr_occ (srcSpanStart loc) }

  | otherwise
  = do	{ mb_gre <- lookupGreLocalRn rdr_name
	; case mb_gre of
		Nothing  -> unboundName rdr_name
		Just gre -> returnM (gre_name gre) }
	      
-- lookupLocatedSigOccRn is used for type signatures and pragmas
-- Is this valid?
--   module A
--	import M( f )
--	f :: Int -> Int
--	f x = x
-- It's clear that the 'f' in the signature must refer to A.f
-- The Haskell98 report does not stipulate this, but it will!
-- So we must treat the 'f' in the signature in the same way
-- as the binding occurrence of 'f', using lookupBndrRn
lookupLocatedSigOccRn :: Located RdrName -> RnM (Located Name)
lookupLocatedSigOccRn = lookupLocatedBndrRn

-- lookupInstDeclBndr is used for the binders in an 
-- instance declaration.   Here we use the class name to
-- disambiguate.  

lookupLocatedInstDeclBndr :: Name -> Located RdrName -> RnM (Located Name)
lookupLocatedInstDeclBndr cls rdr = wrapLocM (lookupInstDeclBndr cls) rdr

lookupInstDeclBndr :: Name -> RdrName -> RnM Name
-- This is called on the method name on the left-hand side of an 
-- instance declaration binding. eg.  instance Functor T where
--                                       fmap = ...
--                                       ^^^^ called on this
-- Regardless of how many unqualified fmaps are in scope, we want
-- the one that comes from the Functor class.
lookupInstDeclBndr cls_name rdr_name
  | isUnqual rdr_name	-- Find all the things the rdr-name maps to
  = do	{		-- and pick the one with the right parent name
	  let { is_op gre@(GRE {gre_par = ParentIs n}) = cls_name == n
	      ;	is_op other			       = False
	      ; occ	      = rdrNameOcc rdr_name
	      ; lookup_fn env = filter is_op (lookupGlobalRdrEnv env occ) }
	; mb_gre <- lookupGreRn_help rdr_name lookup_fn
	; case mb_gre of
	    Just gre -> return (gre_name gre)
	    Nothing  -> do { addErr (unknownInstBndrErr cls_name rdr_name)
			   ; traceRn (text "lookupInstDeclBndr" <+> ppr rdr_name)
			   ; return (mkUnboundName rdr_name) } }

  | otherwise	-- Occurs in derived instances, where we just
		-- refer directly to the right method
  = ASSERT2( not (isQual rdr_name), ppr rdr_name )
	  -- NB: qualified names are rejected by the parser
    lookupImportedName rdr_name

newIPNameRn :: IPName RdrName -> TcRnIf m n (IPName Name)
newIPNameRn ip_rdr = newIPName (mapIPName rdrNameOcc ip_rdr)

-- Looking up family names in type instances is a subtle affair.  The family
-- may be imported, in which case we need to lookup the occurence of a global
-- name.  Alternatively, the family may be in the same binding group (and in
-- fact in a declaration processed later), and we need to create a new top
-- source binder.
--
-- So, also this is strictly speaking an occurence, we cannot raise an error
-- message yet for instances without a family declaration.  This will happen
-- during renaming the type instance declaration in RnSource.rnTyClDecl.
--
lookupFamInstDeclBndr :: Module -> Located RdrName -> RnM Name
lookupFamInstDeclBndr mod lrdr_name@(L _ rdr_name)
  | not (isSrcRdrName rdr_name)
  = lookupImportedName rdr_name	

  | otherwise
  =	-- First look up the name in the normal environment.
   lookupGreRn_maybe rdr_name		`thenM` \ mb_gre ->
   case mb_gre of {
	Just gre -> returnM (gre_name gre) ;
	Nothing  -> newTopSrcBinder mod lrdr_name }

--------------------------------------------------
--		Occurrences
--------------------------------------------------

lookupLocatedOccRn :: Located RdrName -> RnM (Located Name)
lookupLocatedOccRn = wrapLocM lookupOccRn

-- lookupOccRn looks up an occurrence of a RdrName
lookupOccRn :: RdrName -> RnM Name
lookupOccRn rdr_name
  = getLocalRdrEnv			`thenM` \ local_env ->
    case lookupLocalRdrEnv local_env rdr_name of
	  Just name -> returnM name
	  Nothing   -> lookupGlobalOccRn rdr_name

lookupLocatedGlobalOccRn :: Located RdrName -> RnM (Located Name)
lookupLocatedGlobalOccRn = wrapLocM lookupGlobalOccRn

lookupGlobalOccRn :: RdrName -> RnM Name
-- lookupGlobalOccRn is like lookupOccRn, except that it looks in the global 
-- environment.  It's used only for
--	record field names
--	class op names in class and instance decls

lookupGlobalOccRn rdr_name
  | not (isSrcRdrName rdr_name)
  = lookupImportedName rdr_name	

  | otherwise
  =	-- First look up the name in the normal environment.
   lookupGreRn_maybe rdr_name		`thenM` \ mb_gre ->
   case mb_gre of {
	Just gre -> returnM (gre_name gre) ;
	Nothing   -> 

	-- We allow qualified names on the command line to refer to 
	--  *any* name exported by any module in scope, just as if 
	-- there was an "import qualified M" declaration for every 
	-- module.
   getModule 		`thenM` \ mod ->
   if isQual rdr_name && mod == iNTERACTIVE then	
					-- This test is not expensive,
	lookupQualifiedName rdr_name	-- and only happens for failed lookups
   else	
	unboundName rdr_name }

lookupImportedName :: RdrName -> TcRnIf m n Name
-- Lookup the occurrence of an imported name
-- The RdrName is *always* qualified or Exact
-- Treat it as an original name, and conjure up the Name
-- Usually it's Exact or Orig, but it can be Qual if it
--	comes from an hi-boot file.  (This minor infelicity is 
--	just to reduce duplication in the parser.)
lookupImportedName rdr_name
  | Just n <- isExact_maybe rdr_name 
	-- This happens in derived code
  = returnM n

	-- Always Orig, even when reading a .hi-boot file
  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = lookupOrig rdr_mod rdr_occ

  | otherwise
  = pprPanic "RnEnv.lookupImportedName" (ppr rdr_name)

unboundName :: RdrName -> RnM Name
unboundName rdr_name 
  = do	{ addErr (unknownNameErr rdr_name)
	; env <- getGlobalRdrEnv;
	; traceRn (vcat [unknownNameErr rdr_name, 
			 ptext SLIT("Global envt is:"),
			 nest 3 (pprGlobalRdrEnv env)])
	; returnM (mkUnboundName rdr_name) }

--------------------------------------------------
--	Lookup in the Global RdrEnv of the module
--------------------------------------------------

lookupSrcOcc_maybe :: RdrName -> RnM (Maybe Name)
-- No filter function; does not report an error on failure
lookupSrcOcc_maybe rdr_name
  = do	{ mb_gre <- lookupGreRn_maybe rdr_name
	; case mb_gre of
		Nothing  -> returnM Nothing
		Just gre -> returnM (Just (gre_name gre)) }
	
-------------------------
lookupGreRn_maybe :: RdrName -> RnM (Maybe GlobalRdrElt)
-- Just look up the RdrName in the GlobalRdrEnv
lookupGreRn_maybe rdr_name 
  = lookupGreRn_help rdr_name (lookupGRE_RdrName rdr_name)

lookupGreRn :: RdrName -> RnM GlobalRdrElt
-- If not found, add error message, and return a fake GRE
lookupGreRn rdr_name 
  = do	{ mb_gre <- lookupGreRn_maybe rdr_name
	; case mb_gre of {
	    Just gre -> return gre ;
	    Nothing  -> do
	{ name <- unboundName rdr_name
	; return (GRE { gre_name = name, gre_par = NoParent,
		        gre_prov = LocalDef }) }}}

lookupGreLocalRn :: RdrName -> RnM (Maybe GlobalRdrElt)
-- Similar, but restricted to locally-defined things
lookupGreLocalRn rdr_name 
  = lookupGreRn_help rdr_name lookup_fn
  where
    lookup_fn env = filter isLocalGRE (lookupGRE_RdrName rdr_name env)

lookupGreRn_help :: RdrName			-- Only used in error message
		 -> (GlobalRdrEnv -> [GlobalRdrElt])	-- Lookup function
		 -> RnM (Maybe GlobalRdrElt)
-- Checks for exactly one match; reports deprecations
-- Returns Nothing, without error, if too few
lookupGreRn_help rdr_name lookup 
  = do	{ env <- getGlobalRdrEnv
	; case lookup env of
	    []	  -> returnM Nothing
	    [gre] -> returnM (Just gre)
	    gres  -> do { addNameClashErrRn rdr_name gres
			; returnM (Just (head gres)) } }

------------------------------
--	GHCi support
------------------------------

-- A qualified name on the command line can refer to any module at all: we
-- try to load the interface if we don't already have it.
lookupQualifiedName :: RdrName -> RnM Name
lookupQualifiedName rdr_name
  | Just (mod,occ) <- isQual_maybe rdr_name
   -- Note: we want to behave as we would for a source file import here,
   -- and respect hiddenness of modules/packages, hence loadSrcInterface.
   = loadSrcInterface doc mod False	`thenM` \ iface ->

   case  [ (mod,occ) | 
	   (mod,avails) <- mi_exports iface,
    	   avail	<- avails,
    	   name 	<- availNames avail,
    	   name == occ ] of
      ((mod,occ):ns) -> ASSERT (null ns) 
			lookupOrig mod occ
      _ -> unboundName rdr_name

  | otherwise
  = pprPanic "RnEnv.lookupQualifiedName" (ppr rdr_name)
  where
    doc = ptext SLIT("Need to find") <+> ppr rdr_name
\end{code}

%*********************************************************
%*							*
		Fixities
%*							*
%*********************************************************

\begin{code}
lookupLocalDataTcNames :: RdrName -> RnM [Name]
-- GHC extension: look up both the tycon and data con 
-- for con-like things
-- Complain if neither is in scope
lookupLocalDataTcNames rdr_name
  | Just n <- isExact_maybe rdr_name	
	-- Special case for (:), which doesn't get into the GlobalRdrEnv
  = return [n]	-- For this we don't need to try the tycon too
  | otherwise
  = do	{ mb_gres <- mapM lookupGreLocalRn (dataTcOccs rdr_name)
	; case [gre_name gre | Just gre <- mb_gres] of
	    [] -> do { addErr (unknownNameErr rdr_name)
		     ; return [] }
	    names -> return names
    }

--------------------------------
bindLocalFixities :: [FixitySig RdrName] -> RnM a -> RnM a
-- Used for nested fixity decls
-- No need to worry about type constructors here,
-- Should check for duplicates but we don't
bindLocalFixities fixes thing_inside
  | null fixes = thing_inside
  | otherwise  = mappM rn_sig fixes	`thenM` \ new_bit ->
		 extendFixityEnv new_bit thing_inside
  where
    rn_sig (FixitySig lv@(L loc v) fix)
	= addLocM lookupBndrRn lv	`thenM` \ new_v ->
	  returnM (new_v, (FixItem (rdrNameOcc v) fix loc))
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
	traceRn (text "lookupFixityRn" <+> (ppr name $$ ppr local_fix_env)) `thenM_`
	returnM (lookupFixity local_fix_env name)

    else	-- It's imported
      -- For imported names, we have to get their fixities by doing a
      -- loadInterfaceForName, and consulting the Ifaces that comes back
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
      --
      -- loadInterfaceForName will find B.hi even if B is a hidden module,
      -- and that's what we want.
        loadInterfaceForName doc name	`thenM` \ iface ->
	returnM (mi_fix_fn iface (nameOccName name))
  where
    doc = ptext SLIT("Checking fixity for") <+> ppr name

---------------
lookupTyFixityRn :: Located Name -> RnM Fixity
lookupTyFixityRn (L loc n)
  = do	{ glaExts <- doptM Opt_GlasgowExts
	; when (not glaExts) (addWarnAt loc (infixTyConWarn n))
	; lookupFixityRn n }

---------------
dataTcOccs :: RdrName -> [RdrName]
-- If the input is a data constructor, return both it and a type
-- constructor.  This is useful when we aren't sure which we are
-- looking at.
dataTcOccs rdr_name
  | Just n <- isExact_maybe rdr_name		-- Ghastly special case
  , n `hasKey` consDataConKey = [rdr_name]	-- see note below
  | isDataOcc occ 	      = [rdr_name_tc, rdr_name]
  | otherwise 	  	      = [rdr_name]
  where    
    occ 	= rdrNameOcc rdr_name
    rdr_name_tc = setRdrNameSpace rdr_name tcName

-- If the user typed "[]" or "(,,)", we'll generate an Exact RdrName,
-- and setRdrNameSpace generates an Orig, which is fine
-- But it's not fine for (:), because there *is* no corresponding type
-- constructor.  If we generate an Orig tycon for GHC.Base.(:), it'll
-- appear to be in scope (because Orig's simply allocate a new name-cache
-- entry) and then we get an error when we use dataTcOccs in 
-- TcRnDriver.tcRnGetInfo.  Large sigh.
\end{code}

%************************************************************************
%*									*
			Rebindable names
	Dealing with rebindable syntax is driven by the 
	Opt_NoImplicitPrelude dynamic flag.

	In "deriving" code we don't want to use rebindable syntax
	so we switch off the flag locally

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
  * NPlusKPat
  * HsDo
respectively.  Initially, we just store the "standard" name (PrelNames.fromIntegralName,
fromRationalName etc), but the renamer changes this to the appropriate user
name if Opt_NoImplicitPrelude is on.  That is what lookupSyntaxName does.

We treat the orignal (standard) names as free-vars too, because the type checker
checks the type of the user thing against the type of the standard thing.

\begin{code}
lookupSyntaxName :: Name 				-- The standard name
	         -> RnM (SyntaxExpr Name, FreeVars)	-- Possibly a non-standard name
lookupSyntaxName std_name
  = doptM Opt_ImplicitPrelude		`thenM` \ implicit_prelude -> 
    if implicit_prelude then normal_case
    else
	-- Get the similarly named thing from the local environment
    lookupOccRn (mkRdrUnqual (nameOccName std_name)) `thenM` \ usr_name ->
    returnM (HsVar usr_name, unitFV usr_name)
  where
    normal_case = returnM (HsVar std_name, emptyFVs)

lookupSyntaxTable :: [Name]				-- Standard names
		  -> RnM (SyntaxTable Name, FreeVars)	-- See comments with HsExpr.ReboundNames
lookupSyntaxTable std_names
  = doptM Opt_ImplicitPrelude		`thenM` \ implicit_prelude -> 
    if implicit_prelude then normal_case 
    else
    	-- Get the similarly named thing from the local environment
    mappM (lookupOccRn . mkRdrUnqual . nameOccName) std_names 	`thenM` \ usr_names ->

    returnM (std_names `zip` map HsVar usr_names, mkFVs usr_names)
  where
    normal_case = returnM (std_names `zip` map HsVar std_names, emptyFVs)
\end{code}


%*********************************************************
%*							*
\subsection{Binding}
%*							*
%*********************************************************

\begin{code}
newLocalsRn :: [Located RdrName] -> RnM [Name]
newLocalsRn rdr_names_w_loc
  = newUniqueSupply 		`thenM` \ us ->
    returnM (zipWith mk rdr_names_w_loc (uniqsFromSupply us))
  where
    mk (L loc rdr_name) uniq
	| Just name <- isExact_maybe rdr_name = name
		-- This happens in code generated by Template Haskell 
	| otherwise = ASSERT2( isUnqual rdr_name, ppr rdr_name )
			-- We only bind unqualified names here
			-- lookupRdrEnv doesn't even attempt to look up a qualified RdrName
		      mkInternalName uniq (rdrNameOcc rdr_name) (srcSpanStart loc)

bindLocatedLocalsRn :: SDoc	-- Documentation string for error message
	   	    -> [Located RdrName]
	    	    -> ([Name] -> RnM a)
	    	    -> RnM a
bindLocatedLocalsRn doc_str rdr_names_w_loc enclosed_scope
  = 	-- Check for duplicate names
    checkDupNames doc_str rdr_names_w_loc	`thenM_`

    	-- Warn about shadowing, but only in source modules
    ifOptM Opt_WarnNameShadowing 
      (checkShadowing doc_str rdr_names_w_loc)	`thenM_`

	-- Make fresh Names and extend the environment
    newLocalsRn rdr_names_w_loc		`thenM` \ names ->
    getLocalRdrEnv			`thenM` \ local_env ->
    setLocalRdrEnv (extendLocalRdrEnv local_env names)
		   (enclosed_scope names)


bindLocalNames :: [Name] -> RnM a -> RnM a
bindLocalNames names enclosed_scope
  = getLocalRdrEnv 		`thenM` \ name_env ->
    setLocalRdrEnv (extendLocalRdrEnv name_env names)
		    enclosed_scope

bindLocalNamesFV :: [Name] -> RnM (a, FreeVars) -> RnM (a, FreeVars)
bindLocalNamesFV names enclosed_scope
  = do	{ (result, fvs) <- bindLocalNames names enclosed_scope
	; returnM (result, delListFromNameSet fvs names) }


-------------------------------------
	-- binLocalsFVRn is the same as bindLocalsRn
	-- except that it deals with free vars
bindLocatedLocalsFV :: SDoc -> [Located RdrName] -> ([Name] -> RnM (a,FreeVars))
  -> RnM (a, FreeVars)
bindLocatedLocalsFV doc rdr_names enclosed_scope
  = bindLocatedLocalsRn doc rdr_names	$ \ names ->
    enclosed_scope names		`thenM` \ (thing, fvs) ->
    returnM (thing, delListFromNameSet fvs names)

-------------------------------------
bindTyVarsRn :: SDoc -> [LHsTyVarBndr RdrName]
	      -> ([LHsTyVarBndr Name] -> RnM a)
	      -> RnM a
-- Haskell-98 binding of type variables; e.g. within a data type decl
bindTyVarsRn doc_str tyvar_names enclosed_scope
  = let
	located_tyvars = hsLTyVarLocNames tyvar_names
    in
    bindLocatedLocalsRn doc_str located_tyvars	$ \ names ->
    enclosed_scope (zipWith replace tyvar_names names)
    where 
	replace (L loc n1) n2 = L loc (replaceTyVarName n1 n2)

bindPatSigTyVars :: [LHsType RdrName] -> ([Name] -> RnM a) -> RnM a
  -- Find the type variables in the pattern type 
  -- signatures that must be brought into scope
bindPatSigTyVars tys thing_inside
  = do 	{ scoped_tyvars <- doptM Opt_ScopedTypeVariables
	; if not scoped_tyvars then 
		thing_inside []
	  else 
    do 	{ name_env <- getLocalRdrEnv
	; let locd_tvs  = [ tv | ty <- tys
			       , tv <- extractHsTyRdrTyVars ty
			       , not (unLoc tv `elemLocalRdrEnv` name_env) ]
	      nubbed_tvs = nubBy eqLocated locd_tvs
		-- The 'nub' is important.  For example:
		--	f (x :: t) (y :: t) = ....
		-- We don't want to complain about binding t twice!

	; bindLocatedLocalsRn doc_sig nubbed_tvs thing_inside }}
  where
    doc_sig = text "In a pattern type-signature"

bindPatSigTyVarsFV :: [LHsType RdrName]
		   -> RnM (a, FreeVars)
	  	   -> RnM (a, FreeVars)
bindPatSigTyVarsFV tys thing_inside
  = bindPatSigTyVars tys	$ \ tvs ->
    thing_inside		`thenM` \ (result,fvs) ->
    returnM (result, fvs `delListFromNameSet` tvs)

bindSigTyVarsFV :: [Name]
		-> RnM (a, FreeVars)
	  	-> RnM (a, FreeVars)
bindSigTyVarsFV tvs thing_inside
  = do	{ scoped_tyvars <- doptM Opt_ScopedTypeVariables
	; if not scoped_tyvars then 
		thing_inside 
	  else
		bindLocalNamesFV tvs thing_inside }

extendTyVarEnvFVRn :: [Name] -> RnM (a, FreeVars) -> RnM (a, FreeVars)
	-- This function is used only in rnSourceDecl on InstDecl
extendTyVarEnvFVRn tyvars thing_inside = bindLocalNamesFV tyvars thing_inside

-------------------------------------
checkDupNames :: SDoc
	      -> [Located RdrName]
	      -> RnM ()
checkDupNames doc_str rdr_names_w_loc
  = 	-- Check for duplicated names in a binding group
    mappM_ (dupNamesErr doc_str) dups
  where
    (_, dups) = removeDups (\n1 n2 -> unLoc n1 `compare` unLoc n2) rdr_names_w_loc

-------------------------------------
checkShadowing doc_str loc_rdr_names
  = getLocalRdrEnv		`thenM` \ local_env ->
    getGlobalRdrEnv		`thenM` \ global_env ->
    let
      check_shadow (L loc rdr_name)
	|  rdr_name `elemLocalRdrEnv` local_env 
 	|| not (null (lookupGRE_RdrName rdr_name global_env ))
	= addWarnAt loc (shadowedNameWarn doc_str rdr_name)
        | otherwise = returnM ()
    in
    mappM_ check_shadow loc_rdr_names
\end{code}


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
warnUnusedModules :: [(ModuleName,SrcSpan)] -> RnM ()
warnUnusedModules mods
  = ifOptM Opt_WarnUnusedImports (mappM_ bleat mods)
  where
    bleat (mod,loc) = addWarnAt loc (mk_warn mod)
    mk_warn m = vcat [ptext SLIT("Module") <+> quotes (ppr m)
			<+> text "is imported, but nothing from it is used,",
		      nest 2 (ptext SLIT("except perhaps instances visible in") 
			<+> quotes (ppr m)),
		      ptext SLIT("To suppress this warning, use:") 
			<+> ptext SLIT("import") <+> ppr m <> parens empty ]


warnUnusedImports, warnUnusedTopBinds :: [GlobalRdrElt] -> RnM ()
warnUnusedImports gres  = ifOptM Opt_WarnUnusedImports (warnUnusedGREs gres)
warnUnusedTopBinds gres = ifOptM Opt_WarnUnusedBinds   (warnUnusedGREs gres)

warnUnusedLocalBinds, warnUnusedMatches :: [Name] -> RnM ()
warnUnusedLocalBinds names = ifOptM Opt_WarnUnusedBinds   (warnUnusedLocals names)
warnUnusedMatches    names = ifOptM Opt_WarnUnusedMatches (warnUnusedLocals names)

-------------------------
--	Helpers
warnUnusedGREs gres 
 = warnUnusedBinds [(n,Just p) | GRE {gre_name = n, gre_prov = p} <- gres]

warnUnusedLocals names
 = warnUnusedBinds [(n,Nothing) | n<-names]

warnUnusedBinds :: [(Name,Maybe Provenance)] -> RnM ()
warnUnusedBinds names  = mappM_ warnUnusedName (filter reportable names)
 where reportable (name,_) 
	| isWiredInName name = False	-- Don't report unused wired-in names
					-- Otherwise we get a zillion warnings
					-- from Data.Tuple
	| otherwise = reportIfUnused (nameOccName name)

-------------------------

warnUnusedName :: (Name, Maybe Provenance) -> RnM ()
warnUnusedName (name, prov)
  = addWarnAt loc $
    sep [msg <> colon, 
	 nest 2 $ pprNonVarNameSpace (occNameSpace (nameOccName name))
			<+> quotes (ppr name)]
	-- TODO should be a proper span
  where
    (loc,msg) = case prov of
		  Just (Imported is)
		        -> (importSpecLoc imp_spec, imp_from (importSpecModule imp_spec))
		        where
			  imp_spec = head is
		  other -> (srcLocSpan (nameSrcLoc name), unused_msg)

    unused_msg   = text "Defined but not used"
    imp_from mod = text "Imported from" <+> quotes (ppr mod) <+> text "but not used"
\end{code}

\begin{code}
addNameClashErrRn rdr_name names
  = addErr (vcat [ptext SLIT("Ambiguous occurrence") <+> quotes (ppr rdr_name),
		  ptext SLIT("It could refer to") <+> vcat (msg1 : msgs)])
  where
    (np1:nps) = names
    msg1 = ptext  SLIT("either") <+> mk_ref np1
    msgs = [ptext SLIT("    or") <+> mk_ref np | np <- nps]
    mk_ref gre = quotes (ppr (gre_name gre)) <> comma <+> pprNameProvenance gre

shadowedNameWarn doc shadow
  = hsep [ptext SLIT("This binding for"), 
	       quotes (ppr shadow),
	       ptext SLIT("shadows an existing binding")]
    $$ doc

unknownNameErr rdr_name
  = sep [ptext SLIT("Not in scope:"), 
	 nest 2 $ pprNonVarNameSpace (occNameSpace (rdrNameOcc rdr_name))
		  <+> quotes (ppr rdr_name)]

unknownInstBndrErr cls op
  = quotes (ppr op) <+> ptext SLIT("is not a (visible) method of class") <+> quotes (ppr cls)

badOrigBinding name
  = ptext SLIT("Illegal binding of built-in syntax:") <+> ppr (rdrNameOcc name)
	-- The rdrNameOcc is because we don't want to print Prelude.(,)

dupNamesErr :: SDoc -> [Located RdrName] -> RnM ()
dupNamesErr descriptor located_names
  = addErrAt big_loc $
    vcat [ptext SLIT("Conflicting definitions for") <+> quotes (ppr name1),
	  locations, descriptor]
  where
    L _ name1 = head located_names
    locs      = map getLoc located_names
    big_loc   = foldr1 combineSrcSpans locs
    one_line  = isOneLineSpan big_loc
    locations | one_line  = empty 
	      | otherwise = ptext SLIT("Bound at:") <+> 
			    vcat (map ppr (sortLe (<=) locs))

badQualBndrErr rdr_name
  = ptext SLIT("Qualified name in binding position:") <+> ppr rdr_name

infixTyConWarn op
  = vcat [ftext FSLIT("Accepting non-standard infix type constructor") <+> quotes (ppr op),
	  ftext FSLIT("Use -fglasgow-exts to avoid this warning")]
\end{code}
