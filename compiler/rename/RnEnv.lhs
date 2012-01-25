%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-2006
%
\section[RnEnv]{Environment manipulation for the renamer monad}

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module RnEnv ( 
	newTopSrcBinder, 
	lookupLocatedTopBndrRn, lookupTopBndrRn,
	lookupLocatedOccRn, lookupOccRn, lookupLocalOccRn_maybe, lookupPromotedOccRn,
        lookupGlobalOccRn, lookupGlobalOccRn_maybe,

	HsSigCtxt(..), lookupLocalDataTcNames, lookupSigOccRn,

	lookupFixityRn, lookupTyFixityRn, 
	lookupInstDeclBndr, lookupSubBndrOcc, greRdrName,
        lookupSubBndrGREs, lookupConstructorFields,
	lookupSyntaxName, lookupSyntaxTable, lookupIfThenElse,
	lookupGreRn, lookupGreLocalRn, lookupGreRn_maybe,
	getLookupOccRn, addUsedRdrNames,

	newLocalBndrRn, newLocalBndrsRn,
	bindLocalName, bindLocalNames, bindLocalNamesFV, 
	MiniFixityEnv, emptyFsEnv, extendFsEnv, lookupFsEnv,
	addLocalFixities,
	bindLocatedLocalsFV, bindLocatedLocalsRn,
	bindSigTyVarsFV, bindPatSigTyVars, bindPatSigTyVarsFV,
	extendTyVarEnvFVRn,

	checkDupRdrNames, checkDupAndShadowedRdrNames,
        checkDupNames, checkDupAndShadowedNames, 
	addFvRn, mapFvRn, mapMaybeFvRn, mapFvRnCPS,
	warnUnusedMatches,
	warnUnusedTopBinds, warnUnusedLocalBinds,
	dataTcOccs, unknownNameErr, kindSigErr, dataKindsErr, perhapsForallMsg,

        HsDocContext(..), docOfHsDocContext
    ) where

#include "HsVersions.h"

import LoadIface	( loadInterfaceForName, loadSrcInterface )
import IfaceEnv
import HsSyn
import RdrHsSyn		( extractHsTyRdrTyVars )
import RdrName
import HscTypes
import TcEnv		( tcLookupDataCon, tcLookupField, isBrackStage )
import TcRnMonad
import Id		( isRecordSelector )
import Name
import NameSet
import NameEnv
import Avail
import Module           ( ModuleName, moduleName )
import UniqFM
import DataCon		( dataConFieldLabels )
import PrelNames        ( mkUnboundName, rOOT_MAIN, forall_tv_RDR )
import ErrUtils		( Message )
import SrcLoc
import Outputable
import Util
import Maybes
import ListSetOps	( removeDups )
import DynFlags
import FastString
import Control.Monad
import Data.List
import qualified Data.Set as Set
\end{code}

\begin{code}
-- XXX
thenM :: Monad a => a b -> (b -> a c) -> a c
thenM = (>>=)
\end{code}

%*********************************************************
%*							*
		Source-code binders
%*							*
%*********************************************************

\begin{code}
newTopSrcBinder :: Located RdrName -> RnM Name
newTopSrcBinder (L loc rdr_name)
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
    if isExternalName name then
      do { this_mod <- getModule
         ; unless (this_mod == nameModule name)
      	          (addErrAt loc (badOrigBinding rdr_name))
         ; return name }
    else   -- See Note [Binders in Template Haskell] in Convert.hs
      do { let occ = nameOccName name
         ; occ `seq` return ()	-- c.f. seq in newGlobalBinder
         ; this_mod <- getModule
         ; updNameCache $ \ ns ->
           let name' = mkExternalName (nameUnique name) this_mod occ loc
               ns'   = ns { nsNames = extendNameCache (nsNames ns) this_mod occ name' }
           in (ns', name') }

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do	{ this_mod <- getModule
        ; unless (rdr_mod == this_mod || rdr_mod == rOOT_MAIN)
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
	; newGlobalBinder rdr_mod rdr_occ loc }
		--TODO, should pass the whole span

  | otherwise
  = do	{ unless (not (isQual rdr_name))
	         (addErrAt loc (badQualBndrErr rdr_name))
	 	-- Binders should not be qualified; if they are, and with a different
		-- module name, we we get a confusing "M.T is not in scope" error later

	; stage <- getStage
	; if isBrackStage stage then
	        -- We are inside a TH bracket, so make an *Internal* name
		-- See Note [Top-level Names in Template Haskell decl quotes] in RnNames
	     do { uniq <- newUnique
	        ; return (mkInternalName uniq (rdrNameOcc rdr_name) loc) } 
	  else	
	  	-- Normal case
             do { this_mod <- getModule
                ; newGlobalBinder this_mod (rdrNameOcc rdr_name) loc } }
\end{code}

%*********************************************************
%*							*
	Source code occurrences
%*							*
%*********************************************************

Looking up a name in the RnEnv.

Note [Type and class operator definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to reject all of these unless we have -XTypeOperators (Trac #3265)
   data a :*: b  = ...
   class a :*: b where ...
   data (:*:) a b  = ....
   class (:*:) a b where ...
The latter two mean that we are not just looking for a
*syntactically-infix* declaration, but one that uses an operator
OccName.  We use OccName.isSymOcc to detect that case, which isn't
terribly efficient, but there seems to be no better way.

\begin{code}
lookupTopBndrRn :: RdrName -> RnM Name
lookupTopBndrRn n = do nopt <- lookupTopBndrRn_maybe n
                       case nopt of 
                         Just n' -> return n'
                         Nothing -> do traceRn $ text "lookupTopBndrRn"
                                       unboundName WL_LocalTop n

lookupLocatedTopBndrRn :: Located RdrName -> RnM (Located Name)
lookupLocatedTopBndrRn = wrapLocM lookupTopBndrRn

lookupTopBndrRn_maybe :: RdrName -> RnM (Maybe Name)
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

lookupTopBndrRn_maybe rdr_name
  | Just name <- isExact_maybe rdr_name
  = do { name' <- lookupExactOcc name; return (Just name') }

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name	
	-- This deals with the case of derived bindings, where
	-- we don't bother to call newTopSrcBinder first
	-- We assume there is no "parent" name
  = do	{ loc <- getSrcSpanM
        ; n <- newGlobalBinder rdr_mod rdr_occ loc 
        ; return (Just n)}

  | otherwise
  = do	{  -- Check for operators in type or class declarations
           -- See Note [Type and class operator definitions]
          let occ = rdrNameOcc rdr_name
        ; when (isTcOcc occ && isSymOcc occ)
               (do { op_ok <- xoptM Opt_TypeOperators
	           ; unless op_ok (addErr (opDeclErr rdr_name)) })

    	; mb_gre <- lookupGreLocalRn rdr_name
	; case mb_gre of
		Nothing  -> return Nothing
		Just gre -> return (Just $ gre_name gre) }
	      

-----------------------------------------------
lookupExactOcc :: Name -> RnM Name
-- See Note [Looking up Exact RdrNames]
lookupExactOcc name
  | isExternalName name 
  = return name
  | otherwise           
  = do { env <- getGlobalRdrEnv
       ; let gres = lookupGRE_Name env name
       ; case gres of
           []    -> return name
           [gre] -> return (gre_name gre)
           _     -> pprPanic "lookupExactOcc" (ppr name $$ ppr gres) }

-----------------------------------------------
lookupInstDeclBndr :: Name -> SDoc -> RdrName -> RnM Name
-- This is called on the method name on the left-hand side of an 
-- instance declaration binding. eg.  instance Functor T where
--                                       fmap = ...
--                                       ^^^^ called on this
-- Regardless of how many unqualified fmaps are in scope, we want
-- the one that comes from the Functor class.
--
-- Furthermore, note that we take no account of whether the 
-- name is only in scope qualified.  I.e. even if method op is
-- in scope as M.op, we still allow plain 'op' on the LHS of
-- an instance decl
--
-- The "what" parameter says "method" or "associated type",
-- depending on what we are looking up
lookupInstDeclBndr cls what rdr
  = do { when (isQual rdr)
       	      (addErr (badQualBndrErr rdr)) 
	       	-- In an instance decl you aren't allowed
      	     	-- to use a qualified name for the method
		-- (Although it'd make perfect sense.)
       ; lookupSubBndrOcc (ParentIs cls) doc rdr }
  where
    doc = what <+> ptext (sLit "of class") <+> quotes (ppr cls)

-----------------------------------------------
lookupConstructorFields :: Name -> RnM [Name]
-- Look up the fields of a given constructor
--   *	For constructors from this module, use the record field env,
--	which is itself gathered from the (as yet un-typechecked)
--	data type decls
-- 
--    *	For constructors from imported modules, use the *type* environment
--	since imported modles are already compiled, the info is conveniently
--	right there

lookupConstructorFields con_name
  = do	{ this_mod <- getModule
	; if nameIsLocalOrFrom this_mod con_name then
	  do { RecFields field_env _ <- getRecFieldEnv
	     ; return (lookupNameEnv field_env con_name `orElse` []) }
	  else 
	  do { con <- tcLookupDataCon con_name
	     ; return (dataConFieldLabels con) } }

-----------------------------------------------
-- Used for record construction and pattern matching
-- When the -XDisambiguateRecordFields flag is on, take account of the
-- constructor name to disambiguate which field to use; it's just the
-- same as for instance decls
-- 
-- NB: Consider this:
--	module Foo where { data R = R { fld :: Int } }
--	module Odd where { import Foo; fld x = x { fld = 3 } }
-- Arguably this should work, because the reference to 'fld' is
-- unambiguous because there is only one field id 'fld' in scope.
-- But currently it's rejected.

lookupSubBndrOcc :: Parent  -- NoParent   => just look it up as usual
		    	    -- ParentIs p => use p to disambiguate
                 -> SDoc -> RdrName 
                 -> RnM Name
lookupSubBndrOcc parent doc rdr_name
  | Just n <- isExact_maybe rdr_name   -- This happens in derived code
  = lookupExactOcc n

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = lookupOrig rdr_mod rdr_occ

  | otherwise	-- Find all the things the rdr-name maps to
  = do	{	-- and pick the one with the right parent namep
	  env <- getGlobalRdrEnv
	; case lookupSubBndrGREs env parent rdr_name of
		-- NB: lookupGlobalRdrEnv, not lookupGRE_RdrName!
		--     The latter does pickGREs, but we want to allow 'x'
		--     even if only 'M.x' is in scope
	    [gre] -> do { addUsedRdrName gre (used_rdr_name gre)
                          -- Add a usage; this is an *occurrence* site
                        ; return (gre_name gre) }
	    []    -> do { addErr (unknownSubordinateErr doc rdr_name)
			; return (mkUnboundName rdr_name) }
	    gres  -> do { addNameClashErrRn rdr_name gres
			; return (gre_name (head gres)) } }
  where
    -- Note [Usage for sub-bndrs]
    used_rdr_name gre
      | isQual rdr_name = rdr_name
      | otherwise       = greRdrName gre

greRdrName :: GlobalRdrElt -> RdrName
greRdrName gre
  = case gre_prov gre of
      LocalDef    -> unqual_rdr
      Imported is -> used_rdr_name_from_is is

  where 
    occ = nameOccName (gre_name gre)
    unqual_rdr = mkRdrUnqual occ

    used_rdr_name_from_is imp_specs	-- rdr_name is unqualified
      | not (all (is_qual . is_decl) imp_specs) 
      = unqual_rdr  -- An unqualified import is available
      | otherwise
      = 	    -- Only qualified imports available, so make up 
		    -- a suitable qualifed name from the first imp_spec
        ASSERT( not (null imp_specs) )
        mkRdrQual (is_as (is_decl (head imp_specs))) occ

lookupSubBndrGREs :: GlobalRdrEnv -> Parent -> RdrName -> [GlobalRdrElt]
-- If Parent = NoParent, just do a normal lookup
-- If Parent = Parent p then find all GREs that
--   (a) have parent p
--   (b) for Unqual, are in scope qualified or unqualified
--       for Qual, are in scope with that qualification
lookupSubBndrGREs env parent rdr_name
  = case parent of
      NoParent   -> pickGREs rdr_name gres
      ParentIs p 
        | isUnqual rdr_name -> filter (parent_is p) gres
        | otherwise         -> filter (parent_is p) (pickGREs rdr_name gres)

  where
    gres = lookupGlobalRdrEnv env (rdrNameOcc rdr_name)

    parent_is p (GRE { gre_par = ParentIs p' }) = p == p'
    parent_is _ _                               = False
\end{code}

Note [Looking up Exact RdrNames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Exact RdrNames are generated by Template Haskell.  See Note [Binders
in Template Haskell] in Convert.

For data types and classes have Exact system Names in the binding
positions for constructors, TyCons etc.  For example
    [d| data T = MkT Int |]
when we splice in and Convert to HsSyn RdrName, we'll get
    data (Exact (system Name "T")) = (Exact (system Name "MkT")) ...

But, constructors and the like need External Names, not System Names!
So we do the following

 * In RnEnv.newGlobalBinder we spot Exact RdrNames that wrap a
   non-External Name, and make an External name for it. This is
   the name that goes in the GlobalRdrEnv

 * When looking up an occurrence of an Exact name, done in
   RnEnv.lookupExactOcc, we find the Name with the right unique in the
   GlobalRdrEnv, and use the on from the envt -- it will be an
   External Name in the case of the data type/constructor above.

 * Exact names are also use for purely local binders generated
   by TH, such as    \x_33. x_33
   Both binder and occurrence are Exact RdrNames.  The occurrence
   gets looked up in the LocalRdrEnv by RnEnv.lookupOccRn, and 
   misses, because lookupLocalRdrEnv always returns Nothing for
   an Exact Name.  Now we fall through to lookupExactOcc, which
   will find the Name is not in the GlobalRdrEnv, so we just use
   the Exact supplied Name.


Note [Usage for sub-bndrs]
~~~~~~~~~~~~~~~~~~~~~~~~~~
If you have this
   import qualified M( C( f ) ) 
   instance M.C T where
     f x = x
then is the qualified import M.f used?  Obviously yes.
But the RdrName used in the instance decl is unqualified.  In effect,
we fill in the qualification by looking for f's whose class is M.C
But when adding to the UsedRdrNames we must make that qualification
explicit (saying "used  M.f"), otherwise we get "Redundant import of M.f".

So we make up a suitable (fake) RdrName.  But be careful
   import qualifed M
   import M( C(f) )
   instance C T where
     f x = x
Here we want to record a use of 'f', not of 'M.f', otherwise
we'll miss the fact that the qualified import is redundant.

--------------------------------------------------
--		Occurrences
--------------------------------------------------

\begin{code}
getLookupOccRn :: RnM (Name -> Maybe Name)
getLookupOccRn
  = getLocalRdrEnv			`thenM` \ local_env ->
    return (lookupLocalRdrOcc local_env . nameOccName)

lookupLocatedOccRn :: Located RdrName -> RnM (Located Name)
lookupLocatedOccRn = wrapLocM lookupOccRn

lookupLocalOccRn_maybe :: RdrName -> RnM (Maybe Name)
-- Just look in the local environment
lookupLocalOccRn_maybe rdr_name 
  = do { local_env <- getLocalRdrEnv
       ; return (lookupLocalRdrEnv local_env rdr_name) }

-- lookupOccRn looks up an occurrence of a RdrName
lookupOccRn :: RdrName -> RnM Name
lookupOccRn rdr_name = do
  opt_name <- lookupOccRn_maybe rdr_name
  maybe (unboundName WL_Any rdr_name) return opt_name

-- lookupPromotedOccRn looks up an optionally promoted RdrName.
lookupPromotedOccRn :: RdrName -> RnM Name
-- see Note [Demotion] in OccName
lookupPromotedOccRn rdr_name = do {
    -- 1. lookup the name
    opt_name <- lookupOccRn_maybe rdr_name 
  ; case opt_name of
      -- 1.a. we found it!
      Just name -> return name
      -- 1.b. we did not find it -> 2
      Nothing -> do {
  ; -- 2. maybe it was implicitly promoted
    case demoteRdrName rdr_name of
      -- 2.a it was not in a promoted namespace
      Nothing -> err
      -- 2.b let's try every thing again -> 3
      Just demoted_rdr_name -> do {
  ; data_kinds <- xoptM Opt_DataKinds
    -- 3. lookup again
  ; opt_demoted_name <- lookupOccRn_maybe demoted_rdr_name ;
  ; case opt_demoted_name of
      -- 3.a. it was implicitly promoted, but confirm that we can promote
      -- JPM: We could try to suggest turning on DataKinds here
      Just demoted_name -> if data_kinds then return demoted_name else err
      -- 3.b. use rdr_name to have a correct error message
      Nothing -> err } } }
  where err = unboundName WL_Any rdr_name

-- lookupOccRn looks up an occurrence of a RdrName
lookupOccRn_maybe :: RdrName -> RnM (Maybe Name)
lookupOccRn_maybe rdr_name
  = do { local_env <- getLocalRdrEnv
       ; case lookupLocalRdrEnv local_env rdr_name of {
          Just name -> return (Just name) ;
          Nothing   -> do
       { mb_name <- lookupGlobalOccRn_maybe rdr_name
       ; case mb_name of {
                Just name  -> return (Just name) ;
                Nothing -> do
       { -- We allow qualified names on the command line to refer to
         --  *any* name exported by any module in scope, just as if there
         -- was an "import qualified M" declaration for every module.
         allow_qual <- doptM Opt_ImplicitImportQualified
       ; is_ghci <- getIsGHCi
               -- This test is not expensive,
               -- and only happens for failed lookups
       ; if isQual rdr_name && allow_qual && is_ghci
         then lookupQualifiedName rdr_name
         else do { traceRn (text "lookupOccRn" <+> ppr rdr_name)
                 ; return Nothing } } } } } }


lookupGlobalOccRn :: RdrName -> RnM Name
-- lookupGlobalOccRn is like lookupOccRn, except that it looks in the global 
-- environment.  Adds an error message if the RdrName is not in scope.
lookupGlobalOccRn rdr_name
  = do { mb_name <- lookupGlobalOccRn_maybe rdr_name
       ; case mb_name of
           Just n  -> return n
           Nothing -> do { traceRn (text "lookupGlobalOccRn" <+> ppr rdr_name)
                         ; unboundName WL_Global rdr_name } }

lookupGlobalOccRn_maybe :: RdrName -> RnM (Maybe Name)
-- No filter function; does not report an error on failure

lookupGlobalOccRn_maybe rdr_name
  | Just n <- isExact_maybe rdr_name   -- This happens in derived code
  = do { n' <- lookupExactOcc n; return (Just n') }

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do { n <- lookupOrig rdr_mod rdr_occ
       ; return (Just n) }

  | otherwise
  = do	{ mb_gre <- lookupGreRn_maybe rdr_name
	; case mb_gre of
		Nothing  -> return Nothing
		Just gre -> return (Just (gre_name gre)) }


--------------------------------------------------
--	Lookup in the Global RdrEnv of the module
--------------------------------------------------

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
	{ traceRn (text "lookupGreRn" <+> ppr rdr_name)
        ; name <- unboundName WL_Global rdr_name
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
	    []	  -> return Nothing
	    [gre] -> do { addUsedRdrName gre rdr_name
                        ; return (Just gre) }
	    gres  -> do { addNameClashErrRn rdr_name gres
			; return (Just (head gres)) } }

addUsedRdrName :: GlobalRdrElt -> RdrName -> RnM ()
-- Record usage of imported RdrNames
addUsedRdrName gre rdr
  | isLocalGRE gre = return ()
  | otherwise      = do { env <- getGblEnv
       			; updMutVar (tcg_used_rdrnames env)
		                    (\s -> Set.insert rdr s) }

addUsedRdrNames :: [RdrName] -> RnM ()
-- Record used sub-binders
-- We don't check for imported-ness here, because it's inconvenient
-- and not stritly necessary.
addUsedRdrNames rdrs
  = do { env <- getGblEnv
       ; updMutVar (tcg_used_rdrnames env)
	 	   (\s -> foldr Set.insert s rdrs) }

------------------------------
--	GHCi support
------------------------------

-- A qualified name on the command line can refer to any module at all: we
-- try to load the interface if we don't already have it.
lookupQualifiedName :: RdrName -> RnM (Maybe Name)
lookupQualifiedName rdr_name
  | Just (mod,occ) <- isQual_maybe rdr_name
   -- Note: we want to behave as we would for a source file import here,
   -- and respect hiddenness of modules/packages, hence loadSrcInterface.
   = loadSrcInterface doc mod False Nothing	`thenM` \ iface ->

   case  [ name
	 | avail <- mi_exports iface,
    	   name  <- availNames avail,
    	   nameOccName name == occ ] of
      (n:ns) -> ASSERT (null ns) return (Just n)
      _ -> do { traceRn (text "lookupQualified" <+> ppr rdr_name)
              ; return Nothing }

  | otherwise
  = pprPanic "RnEnv.lookupQualifiedName" (ppr rdr_name)
  where
    doc = ptext (sLit "Need to find") <+> ppr rdr_name
\end{code}

Note [Looking up signature names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lookupSigOccRn is used for type signatures and pragmas
Is this valid?
  module A
	import M( f )
	f :: Int -> Int
	f x = x
It's clear that the 'f' in the signature must refer to A.f
The Haskell98 report does not stipulate this, but it will!
So we must treat the 'f' in the signature in the same way
as the binding occurrence of 'f', using lookupBndrRn

However, consider this case:
	import M( f )
	f :: Int -> Int
	g x = x
We don't want to say 'f' is out of scope; instead, we want to
return the imported 'f', so that later on the reanamer will
correctly report "misplaced type sig".

\begin{code}
data HsSigCtxt 
  = HsBootCtxt		     -- Top level of a hs-boot file
  | TopSigCtxt		     -- At top level
  | LocalBindCtxt NameSet    -- In a local binding, binding these names
  | ClsDeclCtxt   Name	     -- Class decl for this class
  | InstDeclCtxt  Name	     -- Intsance decl for this class

lookupSigOccRn :: HsSigCtxt
	       -> Sig RdrName
	       -> Located RdrName -> RnM (Located Name)
lookupSigOccRn ctxt sig
  = wrapLocM $ \ rdr_name -> 
    do { mb_name <- lookupBindGroupOcc ctxt (hsSigDoc sig) rdr_name
       ; case mb_name of
	   Left err   -> do { addErr err; return (mkUnboundName rdr_name) }
	   Right name -> return name }

lookupBindGroupOcc :: HsSigCtxt
	           -> SDoc     
	           -> RdrName -> RnM (Either Message Name)
-- Looks up the RdrName, expecting it to resolve to one of the 
-- bound names passed in.  If not, return an appropriate error message
--
-- See Note [Looking up signature names]
lookupBindGroupOcc ctxt what rdr_name
  | Just n <- isExact_maybe rdr_name
  = do { n' <- lookupExactOcc n
       ; return (Right n') }  -- Maybe we should check the side conditions
       	 	      	      -- but it's a pain, and Exact things only show
			      -- up when you know what you are doing

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do { n' <- lookupOrig rdr_mod rdr_occ
       ; return (Right n') }

  | otherwise
  = case ctxt of 
      HsBootCtxt       -> lookup_top		    
      TopSigCtxt       -> lookup_top
      LocalBindCtxt ns -> lookup_group ns
      ClsDeclCtxt  cls -> lookup_cls_op cls
      InstDeclCtxt cls -> lookup_cls_op cls
  where
    lookup_cls_op cls
      = do { env <- getGlobalRdrEnv 
           ; let gres = lookupSubBndrGREs env (ParentIs cls) rdr_name
           ; case gres of
               []      -> return (Left (unknownSubordinateErr doc rdr_name))
               (gre:_) -> return (Right (gre_name gre)) }
                        -- If there is more than one local GRE for the 
                        -- same OccName 'f', that will be reported separately
                        -- as a duplicate top-level binding for 'f'
      where
        doc = ptext (sLit "method of class") <+> quotes (ppr cls)

    lookup_top
      = do { env <- getGlobalRdrEnv 
           ; let gres = lookupGlobalRdrEnv env (rdrNameOcc rdr_name)
           ; case filter isLocalGRE gres of
               [] | null gres -> bale_out_with empty
                  | otherwise -> bale_out_with (bad_msg (ptext (sLit "an imported value")))
               (gre:_) 
                  | ParentIs {} <- gre_par gre
		  -> bale_out_with (bad_msg (ptext (sLit "a record selector or class method")))
		  | otherwise
                  -> return (Right (gre_name gre)) }

    lookup_group bound_names
      = do { mb_name <- lookupOccRn_maybe rdr_name
           ; case mb_name of
               Just n  
                 | n `elemNameSet` bound_names -> return (Right n)
                 | otherwise                   -> bale_out_with local_msg
               Nothing                         -> bale_out_with empty }

    bale_out_with msg 
  	= return (Left (sep [ ptext (sLit "The") <+> what
  				<+> ptext (sLit "for") <+> quotes (ppr rdr_name)
  			   , nest 2 $ ptext (sLit "lacks an accompanying binding")]
  		       $$ nest 2 msg))

    local_msg = parens $ ptext (sLit "The")  <+> what <+> ptext (sLit "must be given where")
  			   <+> quotes (ppr rdr_name) <+> ptext (sLit "is declared")

    bad_msg thing = parens $ ptext (sLit "You cannot give a") <+> what
    			  <+> ptext (sLit "for") <+> thing


---------------
lookupLocalDataTcNames :: NameSet -> SDoc -> RdrName -> RnM [Name]
-- GHC extension: look up both the tycon and data con 
-- for con-like things.  Used for top-level fixity signatures
-- Complain if neither is in scope
lookupLocalDataTcNames bndr_set what rdr_name
  | Just n <- isExact_maybe rdr_name	
	-- Special case for (:), which doesn't get into the GlobalRdrEnv
  = do { n' <- lookupExactOcc n; return [n'] }	-- For this we don't need to try the tycon too
  | otherwise
  = do	{ mb_gres <- mapM (lookupBindGroupOcc (LocalBindCtxt bndr_set) what)
			  (dataTcOccs rdr_name)
	; let (errs, names) = splitEithers mb_gres
	; when (null names) (addErr (head errs))	-- Bleat about one only
	; return names }

dataTcOccs :: RdrName -> [RdrName]
-- If the input is a data constructor, return both it and a type
-- constructor.  This is useful when we aren't sure which we are
-- looking at.
dataTcOccs rdr_name
  | isDataOcc occ 	      = [rdr_name, rdr_name_tc]
  | otherwise 	  	      = [rdr_name]
  where    
    occ 	= rdrNameOcc rdr_name
    rdr_name_tc = setRdrNameSpace rdr_name tcName
\end{code}


%*********************************************************
%*							*
		Fixities
%*							*
%*********************************************************

\begin{code}
--------------------------------
type FastStringEnv a = UniqFM a		-- Keyed by FastString


emptyFsEnv  :: FastStringEnv a
lookupFsEnv :: FastStringEnv a -> FastString -> Maybe a
extendFsEnv :: FastStringEnv a -> FastString -> a -> FastStringEnv a

emptyFsEnv  = emptyUFM
lookupFsEnv = lookupUFM
extendFsEnv = addToUFM

--------------------------------
type MiniFixityEnv = FastStringEnv (Located Fixity)
	-- Mini fixity env for the names we're about 
	-- to bind, in a single binding group
	--
	-- It is keyed by the *FastString*, not the *OccName*, because
	-- the single fixity decl	infix 3 T
	-- affects both the data constructor T and the type constrctor T
	--
	-- We keep the location so that if we find
	-- a duplicate, we can report it sensibly

--------------------------------
-- Used for nested fixity decls to bind names along with their fixities.
-- the fixities are given as a UFM from an OccName's FastString to a fixity decl

addLocalFixities :: MiniFixityEnv -> [Name] -> RnM a -> RnM a
addLocalFixities mini_fix_env names thing_inside
  = extendFixityEnv (mapCatMaybes find_fixity names) thing_inside
  where
    find_fixity name 
      = case lookupFsEnv mini_fix_env (occNameFS occ) of
          Just (L _ fix) -> Just (name, FixItem occ fix)
          Nothing        -> Nothing
      where
        occ = nameOccName name
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
    then do	-- It's defined in this module
      local_fix_env <- getFixityEnv		
      traceRn (text "lookupFixityRn: looking up name in local environment:" <+> 
               vcat [ppr name, ppr local_fix_env])
      return $ lookupFixity local_fix_env name
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
        loadInterfaceForName doc name	`thenM` \ iface -> do {
          traceRn (text "lookupFixityRn: looking up name in iface cache and found:" <+> 
                   vcat [ppr name, ppr $ mi_fix_fn iface (nameOccName name)]);
	   return (mi_fix_fn iface (nameOccName name))
                                                           }
  where
    doc = ptext (sLit "Checking fixity for") <+> ppr name

---------------
lookupTyFixityRn :: Located Name -> RnM Fixity
lookupTyFixityRn (L _ n) = lookupFixityRn n

\end{code}

%************************************************************************
%*									*
			Rebindable names
	Dealing with rebindable syntax is driven by the 
	Opt_RebindableSyntax dynamic flag.

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
  * HsIntegral/HsFractional/HsIsString
  * NegApp
  * NPlusKPat
  * HsDo
respectively.  Initially, we just store the "standard" name (PrelNames.fromIntegralName,
fromRationalName etc), but the renamer changes this to the appropriate user
name if Opt_NoImplicitPrelude is on.  That is what lookupSyntaxName does.

We treat the orignal (standard) names as free-vars too, because the type checker
checks the type of the user thing against the type of the standard thing.

\begin{code}
lookupIfThenElse :: RnM (Maybe (SyntaxExpr Name), FreeVars)
-- Different to lookupSyntaxName because in the non-rebindable
-- case we desugar directly rather than calling an existing function
-- Hence the (Maybe (SyntaxExpr Name)) return type
lookupIfThenElse 
  = do { rebind <- xoptM Opt_RebindableSyntax
       ; if not rebind 
         then return (Nothing, emptyFVs)
         else do { ite <- lookupOccRn (mkVarUnqual (fsLit "ifThenElse"))
                 ; return (Just (HsVar ite), unitFV ite) } }

lookupSyntaxName :: Name 				-- The standard name
	         -> RnM (SyntaxExpr Name, FreeVars)	-- Possibly a non-standard name
lookupSyntaxName std_name
  = xoptM Opt_RebindableSyntax		`thenM` \ rebindable_on -> 
    if not rebindable_on then normal_case 
    else
	-- Get the similarly named thing from the local environment
    lookupOccRn (mkRdrUnqual (nameOccName std_name)) `thenM` \ usr_name ->
    return (HsVar usr_name, unitFV usr_name)
  where
    normal_case = return (HsVar std_name, emptyFVs)

lookupSyntaxTable :: [Name]				-- Standard names
		  -> RnM (SyntaxTable Name, FreeVars)	-- See comments with HsExpr.ReboundNames
lookupSyntaxTable std_names
  = xoptM Opt_RebindableSyntax		`thenM` \ rebindable_on -> 
    if not rebindable_on then normal_case 
    else
    	-- Get the similarly named thing from the local environment
    mapM (lookupOccRn . mkRdrUnqual . nameOccName) std_names 	`thenM` \ usr_names ->

    return (std_names `zip` map HsVar usr_names, mkFVs usr_names)
  where
    normal_case = return (std_names `zip` map HsVar std_names, emptyFVs)
\end{code}


%*********************************************************
%*							*
\subsection{Binding}
%*							*
%*********************************************************

\begin{code}
newLocalBndrRn :: Located RdrName -> RnM Name
-- Used for non-top-level binders.  These should
-- never be qualified.
newLocalBndrRn (L loc rdr_name)
  | Just name <- isExact_maybe rdr_name 
  = return name	-- This happens in code generated by Template Haskell
    	   	-- See Note [Binders in Template Haskell] in Convert.lhs
  | otherwise
  = do { unless (isUnqual rdr_name)
	        (addErrAt loc (badQualBndrErr rdr_name))
       ; uniq <- newUnique
       ; return (mkInternalName uniq (rdrNameOcc rdr_name) loc) }

newLocalBndrsRn :: [Located RdrName] -> RnM [Name]
newLocalBndrsRn = mapM newLocalBndrRn

---------------------
bindLocatedLocalsRn :: [Located RdrName]
	    	    -> ([Name] -> RnM a)
	    	    -> RnM a
bindLocatedLocalsRn rdr_names_w_loc enclosed_scope
  = do { checkDupAndShadowedRdrNames rdr_names_w_loc

	-- Make fresh Names and extend the environment
       ; names <- newLocalBndrsRn rdr_names_w_loc
       ; bindLocalNames names (enclosed_scope names) }

bindLocalNames :: [Name] -> RnM a -> RnM a
bindLocalNames names enclosed_scope
  = do { name_env <- getLocalRdrEnv
       ; setLocalRdrEnv (extendLocalRdrEnvList name_env names)
		        enclosed_scope }

bindLocalName :: Name -> RnM a -> RnM a
bindLocalName name enclosed_scope
  = do { name_env <- getLocalRdrEnv
       ; setLocalRdrEnv (extendLocalRdrEnv name_env name)
		        enclosed_scope }

bindLocalNamesFV :: [Name] -> RnM (a, FreeVars) -> RnM (a, FreeVars)
bindLocalNamesFV names enclosed_scope
  = do	{ (result, fvs) <- bindLocalNames names enclosed_scope
	; return (result, delFVs names fvs) }


-------------------------------------
	-- binLocalsFVRn is the same as bindLocalsRn
	-- except that it deals with free vars
bindLocatedLocalsFV :: [Located RdrName] 
                    -> ([Name] -> RnM (a,FreeVars)) -> RnM (a, FreeVars)
bindLocatedLocalsFV rdr_names enclosed_scope
  = bindLocatedLocalsRn rdr_names	$ \ names ->
    enclosed_scope names		`thenM` \ (thing, fvs) ->
    return (thing, delFVs names fvs)

-------------------------------------
bindPatSigTyVars :: [LHsType RdrName] -> ([Name] -> RnM a) -> RnM a
  -- Find the type variables in the pattern type 
  -- signatures that must be brought into scope
bindPatSigTyVars tys thing_inside
  = do 	{ scoped_tyvars <- xoptM Opt_ScopedTypeVariables
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

	; bindLocatedLocalsRn nubbed_tvs thing_inside }}

bindPatSigTyVarsFV :: [LHsType RdrName]
		   -> RnM (a, FreeVars)
	  	   -> RnM (a, FreeVars)
bindPatSigTyVarsFV tys thing_inside
  = bindPatSigTyVars tys	$ \ tvs ->
    thing_inside		`thenM` \ (result,fvs) ->
    return (result, fvs `delListFromNameSet` tvs)

bindSigTyVarsFV :: [Name]
		-> RnM (a, FreeVars)
	  	-> RnM (a, FreeVars)
bindSigTyVarsFV tvs thing_inside
  = do	{ scoped_tyvars <- xoptM Opt_ScopedTypeVariables
	; if not scoped_tyvars then 
		thing_inside 
	  else
		bindLocalNamesFV tvs thing_inside }

extendTyVarEnvFVRn :: [Name] -> RnM (a, FreeVars) -> RnM (a, FreeVars)
	-- This function is used only in rnSourceDecl on InstDecl
extendTyVarEnvFVRn tyvars thing_inside = bindLocalNamesFV tyvars thing_inside

-------------------------------------
checkDupRdrNames :: [Located RdrName] -> RnM ()
-- Check for duplicated names in a binding group
checkDupRdrNames rdr_names_w_loc
  = mapM_ (dupNamesErr getLoc) dups
  where
    (_, dups) = removeDups (\n1 n2 -> unLoc n1 `compare` unLoc n2) rdr_names_w_loc

checkDupNames :: [Name] -> RnM ()
-- Check for duplicated names in a binding group
checkDupNames names
  = mapM_ (dupNamesErr nameSrcSpan) dups
  where
    (_, dups) = removeDups (\n1 n2 -> nameOccName n1 `compare` nameOccName n2) $
                filterOut isSystemName names
		-- See Note [Binders in Template Haskell] in Convert

---------------------
checkDupAndShadowedRdrNames :: [Located RdrName] -> RnM ()
checkDupAndShadowedRdrNames loc_rdr_names
  = do	{ checkDupRdrNames loc_rdr_names
	; envs <- getRdrEnvs
	; checkShadowedOccs envs loc_occs }
  where
    loc_occs = [(loc,rdrNameOcc rdr) | L loc rdr <- loc_rdr_names]

checkDupAndShadowedNames :: (GlobalRdrEnv, LocalRdrEnv) -> [Name] -> RnM ()
checkDupAndShadowedNames envs names
  = do { checkDupNames names
       ; checkShadowedOccs envs loc_occs }
  where
    loc_occs = [(nameSrcSpan name, nameOccName name) | name <- names]

-------------------------------------
checkShadowedOccs :: (GlobalRdrEnv, LocalRdrEnv) -> [(SrcSpan,OccName)] -> RnM ()
checkShadowedOccs (global_env,local_env) loc_occs
  = ifWOptM Opt_WarnNameShadowing $ 
    do	{ traceRn (text "shadow" <+> ppr loc_occs)
	; mapM_ check_shadow loc_occs }
  where
    check_shadow (loc, occ)
        | startsWithUnderscore occ = return ()	-- Do not report shadowing for "_x"
	  		       	     	    	-- See Trac #3262
	| Just n <- mb_local = complain [ptext (sLit "bound at") <+> ppr (nameSrcLoc n)]
	| otherwise = do { gres' <- filterM is_shadowed_gre gres
			 ; complain (map pprNameProvenance gres') }
	where
	  complain []      = return ()
	  complain pp_locs = addWarnAt loc (shadowedNameWarn occ pp_locs)
	  mb_local = lookupLocalRdrOcc local_env occ
          gres     = lookupGRE_RdrName (mkRdrUnqual occ) global_env
		-- Make an Unqualified RdrName and look that up, so that
		-- we don't find any GREs that are in scope qualified-only

    is_shadowed_gre :: GlobalRdrElt -> RnM Bool	
	-- Returns False for record selectors that are shadowed, when
	-- punning or wild-cards are on (cf Trac #2723)
    is_shadowed_gre gre@(GRE { gre_par = ParentIs _ })
	= do { dflags <- getDOpts
	     ; if (xopt Opt_RecordPuns dflags || xopt Opt_RecordWildCards dflags) 
	       then do { is_fld <- is_rec_fld gre; return (not is_fld) }
	       else return True }
    is_shadowed_gre _other = return True

    is_rec_fld gre	-- Return True for record selector ids
	| isLocalGRE gre = do { RecFields _ fld_set <- getRecFieldEnv
			      ; return (gre_name gre `elemNameSet` fld_set) }
	| otherwise	 = do { sel_id <- tcLookupField (gre_name gre)
			      ; return (isRecordSelector sel_id) }
\end{code}


%************************************************************************
%*									*
               What to do when a lookup fails
%*                                                                      *
%************************************************************************

\begin{code}
data WhereLooking = WL_Any        -- Any binding
                  | WL_Global     -- Any top-level binding (local or imported)
                  | WL_LocalTop   -- Any top-level binding in this module

unboundName :: WhereLooking -> RdrName -> RnM Name
unboundName where_look rdr_name
  = do  { show_helpful_errors <- doptM Opt_HelpfulErrors
        ; let err = unknownNameErr rdr_name
        ; if not show_helpful_errors
          then addErr err
          else do { extra_err <- unknownNameSuggestErr where_look rdr_name
                  ; addErr (err $$ extra_err) }

        ; env <- getGlobalRdrEnv;
	; traceRn (vcat [unknownNameErr rdr_name, 
			 ptext (sLit "Global envt is:"),
			 nest 3 (pprGlobalRdrEnv env)])

        ; return (mkUnboundName rdr_name) }

unknownNameErr :: RdrName -> SDoc
unknownNameErr rdr_name
  = vcat [ hang (ptext (sLit "Not in scope:")) 
	      2 (pprNonVarNameSpace (occNameSpace (rdrNameOcc rdr_name))
			  <+> quotes (ppr rdr_name))
	 , extra ]
  where
    extra | rdr_name == forall_tv_RDR = perhapsForallMsg
	  | otherwise 		      = empty

type HowInScope = Either SrcSpan ImpDeclSpec
     -- Left loc    =>  locally bound at loc
     -- Right ispec =>  imported as specified by ispec

unknownNameSuggestErr :: WhereLooking -> RdrName -> RnM SDoc
unknownNameSuggestErr where_look tried_rdr_name
  = do { local_env <- getLocalRdrEnv
       ; global_env <- getGlobalRdrEnv

       ; let all_possibilities :: [(String, (RdrName, HowInScope))]
             all_possibilities
                =  [ (showSDoc (ppr r), (r, Left loc))
                   | (r,loc) <- local_possibilities local_env ]
                ++ [ (showSDoc (ppr r), rp) | (r,rp) <- global_possibilities global_env ]

             suggest = fuzzyLookup (showSDoc (ppr tried_rdr_name)) all_possibilities
             perhaps = ptext (sLit "Perhaps you meant")
             extra_err = case suggest of
                           []  -> empty
                           [p] -> perhaps <+> pp_item p
                           ps  -> sep [ perhaps <+> ptext (sLit "one of these:")
                                      , nest 2 (pprWithCommas pp_item ps) ]
       ; return extra_err }
  where
    pp_item :: (RdrName, HowInScope) -> SDoc
    pp_item (rdr, Left loc) = quotes (ppr rdr) <+> loc' -- Locally defined
        where loc' = case loc of
                     UnhelpfulSpan l -> parens (ppr l)
                     RealSrcSpan l -> parens (ptext (sLit "line") <+> int (srcSpanStartLine l))
    pp_item (rdr, Right is) = quotes (ppr rdr) <+>   -- Imported
                              parens (ptext (sLit "imported from") <+> ppr (is_mod is))

    tried_occ     = rdrNameOcc tried_rdr_name
    tried_is_sym  = isSymOcc tried_occ
    tried_ns      = occNameSpace tried_occ
    tried_is_qual = isQual tried_rdr_name

    correct_name_space occ =  occNameSpace occ == tried_ns
                           && isSymOcc occ == tried_is_sym
        -- Treat operator and non-operators as non-matching
        -- This heuristic avoids things like
        --      Not in scope 'f'; perhaps you meant '+' (from Prelude)

    local_ok = case where_look of { WL_Any -> True; _ -> False }
    local_possibilities :: LocalRdrEnv -> [(RdrName, SrcSpan)]
    local_possibilities env
      | tried_is_qual = []
      | not local_ok  = []
      | otherwise     = [ (mkRdrUnqual occ, nameSrcSpan name)
                      	| name <- occEnvElts env
                      	, let occ = nameOccName name
                      	, correct_name_space occ]

    gre_ok :: GlobalRdrElt -> Bool
    gre_ok = case where_look of
                   WL_LocalTop -> isLocalGRE
                   _           -> \_ -> True

    global_possibilities :: GlobalRdrEnv -> [(RdrName, (RdrName, HowInScope))]
    global_possibilities global_env
      | tried_is_qual = [ (rdr_qual, (rdr_qual, how))
                        | gre <- globalRdrEnvElts global_env
                        , gre_ok gre
                        , let name = gre_name gre
        		      occ  = nameOccName name
                        , correct_name_space occ
                        , (mod, how) <- quals_in_scope name (gre_prov gre)
                        , let rdr_qual = mkRdrQual mod occ ]

      | otherwise = [ (rdr_unqual, pair)
                    | gre <- globalRdrEnvElts global_env
                    , gre_ok gre
                    , let name = gre_name gre
                          prov = gre_prov gre
                          occ  = nameOccName name
                          rdr_unqual = mkRdrUnqual occ
                    , correct_name_space occ
                    , pair <- case (unquals_in_scope name prov, quals_only occ prov) of
                                (how:_, _)    -> [ (rdr_unqual, how) ]
                                ([],    pr:_) -> [ pr ]  -- See Note [Only-quals]
                                ([],    [])   -> [] ]

              -- Note [Only-quals]
              -- The second alternative returns those names with the same
              -- OccName as the one we tried, but live in *qualified* imports
       	      -- e.g. if you have:
       	      --
       	      -- > import qualified Data.Map as Map
       	      -- > foo :: Map
       	      --
       	      -- then we suggest @Map.Map@.

    --------------------
    unquals_in_scope :: Name -> Provenance -> [HowInScope]
    unquals_in_scope n LocalDef      = [ Left (nameSrcSpan n) ]
    unquals_in_scope _ (Imported is) = [ Right ispec
                                       | i <- is, let ispec = is_decl i
                                       , not (is_qual ispec) ]

    --------------------
    quals_in_scope :: Name -> Provenance -> [(ModuleName, HowInScope)]
    -- Ones for which the qualified version is in scope
    quals_in_scope n LocalDef      = case nameModule_maybe n of
                                       Nothing -> []
                                       Just m  -> [(moduleName m, Left (nameSrcSpan n))]
    quals_in_scope _ (Imported is) = [ (is_as ispec, Right ispec)
                                     | i <- is, let ispec = is_decl i ]

    --------------------
    quals_only :: OccName -> Provenance -> [(RdrName, HowInScope)]
    -- Ones for which *only* the qualified version is in scope
    quals_only _   LocalDef      = []
    quals_only occ (Imported is) = [ (mkRdrQual (is_as ispec) occ, Right ispec)
                                   | i <- is, let ispec = is_decl i, is_qual ispec ]
\end{code}

%************************************************************************
%*									*
\subsection{Free variable manipulation}
%*									*
%************************************************************************

\begin{code}
-- A useful utility
addFvRn :: FreeVars -> RnM (thing, FreeVars) -> RnM (thing, FreeVars)
addFvRn fvs1 thing_inside = do { (res, fvs2) <- thing_inside
                               ; return (res, fvs1 `plusFV` fvs2) }

mapFvRn :: (a -> RnM (b, FreeVars)) -> [a] -> RnM ([b], FreeVars)
mapFvRn f xs = do stuff <- mapM f xs
                  case unzip stuff of
                      (ys, fvs_s) -> return (ys, plusFVs fvs_s)

mapMaybeFvRn :: (a -> RnM (b, FreeVars)) -> Maybe a -> RnM (Maybe b, FreeVars)
mapMaybeFvRn _ Nothing = return (Nothing, emptyFVs)
mapMaybeFvRn f (Just x) = do { (y, fvs) <- f x; return (Just y, fvs) }

-- because some of the rename functions are CPSed:
-- maps the function across the list from left to right; 
-- collects all the free vars into one set
mapFvRnCPS :: (a  -> (b   -> RnM c) -> RnM c) 
           -> [a] -> ([b] -> RnM c) -> RnM c

mapFvRnCPS _ []     cont = cont []
mapFvRnCPS f (x:xs) cont = f x 		   $ \ x' -> 
                           mapFvRnCPS f xs $ \ xs' ->
                           cont (x':xs')
\end{code}


%************************************************************************
%*									*
\subsection{Envt utility functions}
%*									*
%************************************************************************

\begin{code}
warnUnusedTopBinds :: [GlobalRdrElt] -> RnM ()
warnUnusedTopBinds gres
    = ifWOptM Opt_WarnUnusedBinds
    $ do isBoot <- tcIsHsBoot
         let noParent gre = case gre_par gre of
                            NoParent -> True
                            ParentIs _ -> False
             -- Don't warn about unused bindings with parents in
             -- .hs-boot files, as you are sometimes required to give
             -- unused bindings (trac #3449).
             gres' = if isBoot then filter noParent gres
                               else                 gres
         warnUnusedGREs gres'

warnUnusedLocalBinds, warnUnusedMatches :: [Name] -> FreeVars -> RnM ()
warnUnusedLocalBinds = check_unused Opt_WarnUnusedBinds
warnUnusedMatches    = check_unused Opt_WarnUnusedMatches

check_unused :: WarningFlag -> [Name] -> FreeVars -> RnM ()
check_unused flag bound_names used_names
 = ifWOptM flag (warnUnusedLocals (filterOut (`elemNameSet` used_names) bound_names))

-------------------------
--	Helpers
warnUnusedGREs :: [GlobalRdrElt] -> RnM ()
warnUnusedGREs gres 
 = warnUnusedBinds [(n,p) | GRE {gre_name = n, gre_prov = p} <- gres]

warnUnusedLocals :: [Name] -> RnM ()
warnUnusedLocals names
 = warnUnusedBinds [(n,LocalDef) | n<-names]

warnUnusedBinds :: [(Name,Provenance)] -> RnM ()
warnUnusedBinds names  = mapM_ warnUnusedName (filter reportable names)
 where reportable (name,_) 
	| isWiredInName name = False	-- Don't report unused wired-in names
					-- Otherwise we get a zillion warnings
					-- from Data.Tuple
	| otherwise = not (startsWithUnderscore (nameOccName name))

-------------------------

warnUnusedName :: (Name, Provenance) -> RnM ()
warnUnusedName (name, LocalDef)
  = addUnusedWarning name (nameSrcSpan name)
		     (ptext (sLit "Defined but not used"))

warnUnusedName (name, Imported is)
  = mapM_ warn is
  where
    warn spec = addUnusedWarning name span msg
	where
	   span = importSpecLoc spec
	   pp_mod = quotes (ppr (importSpecModule spec))
	   msg = ptext (sLit "Imported from") <+> pp_mod <+> ptext (sLit "but not used")

addUnusedWarning :: Name -> SrcSpan -> SDoc -> RnM ()
addUnusedWarning name span msg
  = addWarnAt span $
    sep [msg <> colon, 
	 nest 2 $ pprNonVarNameSpace (occNameSpace (nameOccName name))
			<+> quotes (ppr name)]
\end{code}

\begin{code}
addNameClashErrRn :: RdrName -> [GlobalRdrElt] -> RnM ()
addNameClashErrRn rdr_name names
  = addErr (vcat [ptext (sLit "Ambiguous occurrence") <+> quotes (ppr rdr_name),
		  ptext (sLit "It could refer to") <+> vcat (msg1 : msgs)])
  where
    (np1:nps) = names
    msg1 = ptext  (sLit "either") <+> mk_ref np1
    msgs = [ptext (sLit "    or") <+> mk_ref np | np <- nps]
    mk_ref gre = sep [quotes (ppr (gre_name gre)) <> comma, pprNameProvenance gre]

shadowedNameWarn :: OccName -> [SDoc] -> SDoc
shadowedNameWarn occ shadowed_locs
  = sep [ptext (sLit "This binding for") <+> quotes (ppr occ)
	    <+> ptext (sLit "shadows the existing binding") <> plural shadowed_locs,
	 nest 2 (vcat shadowed_locs)]

perhapsForallMsg :: SDoc
perhapsForallMsg 
  = vcat [ ptext (sLit "Perhaps you intended to use -XExplicitForAll or similar flag")
	 , ptext (sLit "to enable explicit-forall syntax: forall <tvs>. <type>")]

unknownSubordinateErr :: SDoc -> RdrName -> SDoc
unknownSubordinateErr doc op	-- Doc is "method of class" or 
				-- "field of constructor"
  = quotes (ppr op) <+> ptext (sLit "is not a (visible)") <+> doc

badOrigBinding :: RdrName -> SDoc
badOrigBinding name
  = ptext (sLit "Illegal binding of built-in syntax:") <+> ppr (rdrNameOcc name)
	-- The rdrNameOcc is because we don't want to print Prelude.(,)

dupNamesErr :: Outputable n => (n -> SrcSpan) -> [n] -> RnM ()
dupNamesErr get_loc names
  = addErrAt big_loc $
    vcat [ptext (sLit "Conflicting definitions for") <+> quotes (ppr (head names)),
	  locations]
  where
    locs      = map get_loc names
    big_loc   = foldr1 combineSrcSpans locs
    locations = ptext (sLit "Bound at:") <+> vcat (map ppr (sortLe (<=) locs))

kindSigErr :: Outputable a => a -> SDoc
kindSigErr thing
  = hang (ptext (sLit "Illegal kind signature for") <+> quotes (ppr thing))
       2 (ptext (sLit "Perhaps you intended to use -XKindSignatures"))

dataKindsErr :: Outputable a => a -> SDoc
dataKindsErr thing
  = hang (ptext (sLit "Illegal kind:") <+> quotes (ppr thing))
       2 (ptext (sLit "Perhaps you intended to use -XDataKinds"))


badQualBndrErr :: RdrName -> SDoc
badQualBndrErr rdr_name
  = ptext (sLit "Qualified name in binding position:") <+> ppr rdr_name

opDeclErr :: RdrName -> SDoc
opDeclErr n 
  = hang (ptext (sLit "Illegal declaration of a type or class operator") <+> quotes (ppr n))
       2 (ptext (sLit "Use -XTypeOperators to declare operators in type and declarations"))
\end{code}


%************************************************************************
%*									*
\subsection{Contexts for renaming errors}
%*									*
%************************************************************************

\begin{code}

data HsDocContext
  = TypeSigCtx SDoc
  | PatCtx
  | SpecInstSigCtx
  | DefaultDeclCtx
  | ForeignDeclCtx (Located RdrName)
  | DerivDeclCtx
  | RuleCtx FastString
  | TyDataCtx (Located RdrName)
  | TySynCtx (Located RdrName)
  | TyFamilyCtx (Located RdrName)
  | ConDeclCtx (Located RdrName)
  | ClassDeclCtx (Located RdrName)
  | ExprWithTySigCtx
  | TypBrCtx
  | HsTypeCtx
  | GHCiCtx
  | SpliceTypeCtx (LHsType RdrName)
  | ClassInstanceCtx
  | VectDeclCtx (Located RdrName)

docOfHsDocContext :: HsDocContext -> SDoc
docOfHsDocContext (TypeSigCtx doc) = text "In the type signature for" <+> doc
docOfHsDocContext PatCtx = text "In a pattern type-signature"
docOfHsDocContext SpecInstSigCtx = text "In a SPECIALISE instance pragma"
docOfHsDocContext DefaultDeclCtx = text "In a `default' declaration"
docOfHsDocContext (ForeignDeclCtx name) = ptext (sLit "In the foreign declaration for") <+> ppr name
docOfHsDocContext DerivDeclCtx = text "In a deriving declaration"
docOfHsDocContext (RuleCtx name) = text "In the transformation rule" <+> ftext name
docOfHsDocContext (TyDataCtx tycon) = text "In the data type declaration for" <+> quotes (ppr tycon)
docOfHsDocContext (TySynCtx name) = text "In the declaration for type synonym" <+> quotes (ppr name)
docOfHsDocContext (TyFamilyCtx name) = text "In the declaration for type family" <+> quotes (ppr name)
docOfHsDocContext (ConDeclCtx name) = text "In the definition of data constructor" <+> quotes (ppr name)
docOfHsDocContext (ClassDeclCtx name) = text "In the declaration for class" 	<+> ppr name
docOfHsDocContext ExprWithTySigCtx = text "In an expression type signature"
docOfHsDocContext TypBrCtx = ptext (sLit "In a Template-Haskell quoted type")
docOfHsDocContext HsTypeCtx = text "In a type argument"
docOfHsDocContext GHCiCtx = ptext (sLit "In GHCi input")
docOfHsDocContext (SpliceTypeCtx hs_ty) = ptext (sLit "In the spliced type") <+> ppr hs_ty
docOfHsDocContext ClassInstanceCtx = ptext (sLit "TcSplice.reifyInstances")
docOfHsDocContext (VectDeclCtx tycon) = ptext (sLit "In the VECTORISE pragma for type constructor") <+> quotes (ppr tycon)

\end{code}
