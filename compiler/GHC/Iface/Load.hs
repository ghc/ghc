{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Loading interface files
-}

{-# LANGUAGE CPP, BangPatterns, RecordWildCards, NondecreasingIndentation #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GHC.Iface.Load (
        -- Importing one thing
        tcLookupImported_maybe, importDecl,
        checkWiredInTyCon, ifCheckWiredInThing,

        -- RnM/TcM functions
        loadModuleInterface, loadModuleInterfaces,
        loadSrcInterface, loadSrcInterface_maybe,
        loadInterfaceForName, loadInterfaceForNameMaybe, loadInterfaceForModule,

        -- IfM functions
        loadInterface,
        loadSysInterface, loadUserInterface, loadPluginInterface,
        findAndReadIface, readIface,    -- Used when reading the module's old interface
        loadDecls,      -- Should move to GHC.IfaceToCore and be renamed
        initExternalPackageState,
        moduleFreeHolesPrecise,
        needWiredInHomeIface, loadWiredInHomeIface,

        pprModIfaceSimple,
        ifaceStats, pprModIface, showIface
   ) where

#include "HsVersions.h"

import GhcPrelude

import {-# SOURCE #-} GHC.IfaceToCore
   ( tcIfaceDecl, tcIfaceRules, tcIfaceInst, tcIfaceFamInst
   , tcIfaceAnnotations, tcIfaceCompleteSigs )

import DynFlags
import GHC.Iface.Syntax
import GHC.Iface.Env
import HscTypes

import BasicTypes hiding (SuccessFlag(..))
import TcRnMonad

import Constants
import PrelNames
import PrelInfo
import PrimOp   ( allThePrimOps, primOpFixity, primOpOcc )
import MkId     ( seqId )
import TysPrim  ( funTyConName )
import Rules
import TyCon
import Annotations
import InstEnv
import FamInstEnv
import Name
import NameEnv
import Avail
import Module
import Maybes
import ErrUtils
import Finder
import UniqFM
import SrcLoc
import Outputable
import GHC.Iface.Binary
import Panic
import Util
import FastString
import Fingerprint
import Hooks
import FieldLabel
import GHC.Iface.Rename
import UniqDSet
import Plugins

import Control.Monad
import Control.Exception
import Data.IORef
import System.FilePath

{-
************************************************************************
*                                                                      *
*      tcImportDecl is the key function for "faulting in"              *
*      imported things
*                                                                      *
************************************************************************

The main idea is this.  We are chugging along type-checking source code, and
find a reference to GHC.Base.map.  We call tcLookupGlobal, which doesn't find
it in the EPS type envt.  So it
        1 loads GHC.Base.hi
        2 gets the decl for GHC.Base.map
        3 typechecks it via tcIfaceDecl
        4 and adds it to the type env in the EPS

Note that DURING STEP 4, we may find that map's type mentions a type
constructor that also

Notice that for imported things we read the current version from the EPS
mutable variable.  This is important in situations like
        ...$(e1)...$(e2)...
where the code that e1 expands to might import some defns that
also turn out to be needed by the code that e2 expands to.
-}

tcLookupImported_maybe :: Name -> TcM (MaybeErr MsgDoc TyThing)
-- Returns (Failed err) if we can't find the interface file for the thing
tcLookupImported_maybe name
  = do  { hsc_env <- getTopEnv
        ; mb_thing <- liftIO (lookupTypeHscEnv hsc_env name)
        ; case mb_thing of
            Just thing -> return (Succeeded thing)
            Nothing    -> tcImportDecl_maybe name }

tcImportDecl_maybe :: Name -> TcM (MaybeErr MsgDoc TyThing)
-- Entry point for *source-code* uses of importDecl
tcImportDecl_maybe name
  | Just thing <- wiredInNameTyThing_maybe name
  = do  { when (needWiredInHomeIface thing)
               (initIfaceTcRn (loadWiredInHomeIface name))
                -- See Note [Loading instances for wired-in things]
        ; return (Succeeded thing) }
  | otherwise
  = initIfaceTcRn (importDecl name)

importDecl :: Name -> IfM lcl (MaybeErr MsgDoc TyThing)
-- Get the TyThing for this Name from an interface file
-- It's not a wired-in thing -- the caller caught that
importDecl name
  = ASSERT( not (isWiredInName name) )
    do  { traceIf nd_doc

        -- Load the interface, which should populate the PTE
        ; mb_iface <- ASSERT2( isExternalName name, ppr name )
                      loadInterface nd_doc (nameModule name) ImportBySystem
        ; case mb_iface of {
                Failed err_msg  -> return (Failed err_msg) ;
                Succeeded _ -> do

        -- Now look it up again; this time we should find it
        { eps <- getEps
        ; case lookupTypeEnv (eps_PTE eps) name of
            Just thing -> return $ Succeeded thing
            Nothing    -> let doc = whenPprDebug (found_things_msg eps $$ empty)
                                    $$ not_found_msg
                          in return $ Failed doc
    }}}
  where
    nd_doc = text "Need decl for" <+> ppr name
    not_found_msg = hang (text "Can't find interface-file declaration for" <+>
                                pprNameSpace (nameNameSpace name) <+> ppr name)
                       2 (vcat [text "Probable cause: bug in .hi-boot file, or inconsistent .hi file",
                                text "Use -ddump-if-trace to get an idea of which file caused the error"])
    found_things_msg eps =
        hang (text "Found the following declarations in" <+> ppr (nameModule name) <> colon)
           2 (vcat (map ppr $ filter is_interesting $ nameEnvElts $ eps_PTE eps))
      where
        is_interesting thing = nameModule name == nameModule (getName thing)


{-
************************************************************************
*                                                                      *
           Checks for wired-in things
*                                                                      *
************************************************************************

Note [Loading instances for wired-in things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to make sure that we have at least *read* the interface files
for any module with an instance decl or RULE that we might want.

* If the instance decl is an orphan, we have a whole separate mechanism
  (loadOrphanModules)

* If the instance decl is not an orphan, then the act of looking at the
  TyCon or Class will force in the defining module for the
  TyCon/Class, and hence the instance decl

* BUT, if the TyCon is a wired-in TyCon, we don't really need its interface;
  but we must make sure we read its interface in case it has instances or
  rules.  That is what GHC.Iface.Load.loadWiredInHomeIface does.  It's called
  from GHC.IfaceToCore.{tcImportDecl, checkWiredInTyCon, ifCheckWiredInThing}

* HOWEVER, only do this for TyCons.  There are no wired-in Classes.  There
  are some wired-in Ids, but we don't want to load their interfaces. For
  example, Control.Exception.Base.recSelError is wired in, but that module
  is compiled late in the base library, and we don't want to force it to
  load before it's been compiled!

All of this is done by the type checker. The renamer plays no role.
(It used to, but no longer.)
-}

checkWiredInTyCon :: TyCon -> TcM ()
-- Ensure that the home module of the TyCon (and hence its instances)
-- are loaded. See Note [Loading instances for wired-in things]
-- It might not be a wired-in tycon (see the calls in TcUnify),
-- in which case this is a no-op.
checkWiredInTyCon tc
  | not (isWiredInName tc_name)
  = return ()
  | otherwise
  = do  { mod <- getModule
        ; traceIf (text "checkWiredInTyCon" <+> ppr tc_name $$ ppr mod)
        ; ASSERT( isExternalName tc_name )
          when (mod /= nameModule tc_name)
               (initIfaceTcRn (loadWiredInHomeIface tc_name))
                -- Don't look for (non-existent) Float.hi when
                -- compiling Float.hs, which mentions Float of course
                -- A bit yukky to call initIfaceTcRn here
        }
  where
    tc_name = tyConName tc

ifCheckWiredInThing :: TyThing -> IfL ()
-- Even though we are in an interface file, we want to make
-- sure the instances of a wired-in thing are loaded (imagine f :: Double -> Double)
-- Ditto want to ensure that RULES are loaded too
-- See Note [Loading instances for wired-in things]
ifCheckWiredInThing thing
  = do  { mod <- getIfModule
                -- Check whether we are typechecking the interface for this
                -- very module.  E.g when compiling the base library in --make mode
                -- we may typecheck GHC.Base.hi. At that point, GHC.Base is not in
                -- the HPT, so without the test we'll demand-load it into the PIT!
                -- C.f. the same test in checkWiredInTyCon above
        ; let name = getName thing
        ; ASSERT2( isExternalName name, ppr name )
          when (needWiredInHomeIface thing && mod /= nameModule name)
               (loadWiredInHomeIface name) }

needWiredInHomeIface :: TyThing -> Bool
-- Only for TyCons; see Note [Loading instances for wired-in things]
needWiredInHomeIface (ATyCon {}) = True
needWiredInHomeIface _           = False


{-
************************************************************************
*                                                                      *
        loadSrcInterface, loadOrphanModules, loadInterfaceForName

                These three are called from TcM-land
*                                                                      *
************************************************************************
-}

-- | Load the interface corresponding to an @import@ directive in
-- source code.  On a failure, fail in the monad with an error message.
loadSrcInterface :: SDoc
                 -> ModuleName
                 -> IsBootInterface     -- {-# SOURCE #-} ?
                 -> Maybe FastString    -- "package", if any
                 -> RnM ModIface

loadSrcInterface doc mod want_boot maybe_pkg
  = do { res <- loadSrcInterface_maybe doc mod want_boot maybe_pkg
       ; case res of
           Failed err      -> failWithTc err
           Succeeded iface -> return iface }

-- | Like 'loadSrcInterface', but returns a 'MaybeErr'.
loadSrcInterface_maybe :: SDoc
                       -> ModuleName
                       -> IsBootInterface     -- {-# SOURCE #-} ?
                       -> Maybe FastString    -- "package", if any
                       -> RnM (MaybeErr MsgDoc ModIface)

loadSrcInterface_maybe doc mod want_boot maybe_pkg
  -- We must first find which Module this import refers to.  This involves
  -- calling the Finder, which as a side effect will search the filesystem
  -- and create a ModLocation.  If successful, loadIface will read the
  -- interface; it will call the Finder again, but the ModLocation will be
  -- cached from the first search.
  = do { hsc_env <- getTopEnv
       ; res <- liftIO $ findImportedModule hsc_env mod maybe_pkg
       ; case res of
           Found _ mod -> initIfaceTcRn $ loadInterface doc mod (ImportByUser want_boot)
           -- TODO: Make sure this error message is good
           err         -> return (Failed (cannotFindModule (hsc_dflags hsc_env) mod err)) }

-- | Load interface directly for a fully qualified 'Module'.  (This is a fairly
-- rare operation, but in particular it is used to load orphan modules
-- in order to pull their instances into the global package table and to
-- handle some operations in GHCi).
loadModuleInterface :: SDoc -> Module -> TcM ModIface
loadModuleInterface doc mod = initIfaceTcRn (loadSysInterface doc mod)

-- | Load interfaces for a collection of modules.
loadModuleInterfaces :: SDoc -> [Module] -> TcM ()
loadModuleInterfaces doc mods
  | null mods = return ()
  | otherwise = initIfaceTcRn (mapM_ load mods)
  where
    load mod = loadSysInterface (doc <+> parens (ppr mod)) mod

-- | Loads the interface for a given Name.
-- Should only be called for an imported name;
-- otherwise loadSysInterface may not find the interface
loadInterfaceForName :: SDoc -> Name -> TcRn ModIface
loadInterfaceForName doc name
  = do { when debugIsOn $  -- Check pre-condition
         do { this_mod <- getModule
            ; MASSERT2( not (nameIsLocalOrFrom this_mod name), ppr name <+> parens doc ) }
      ; ASSERT2( isExternalName name, ppr name )
        initIfaceTcRn $ loadSysInterface doc (nameModule name) }

-- | Only loads the interface for external non-local names.
loadInterfaceForNameMaybe :: SDoc -> Name -> TcRn (Maybe ModIface)
loadInterfaceForNameMaybe doc name
  = do { this_mod <- getModule
       ; if nameIsLocalOrFrom this_mod name || not (isExternalName name)
         then return Nothing
         else Just <$> (initIfaceTcRn $ loadSysInterface doc (nameModule name))
       }

-- | Loads the interface for a given Module.
loadInterfaceForModule :: SDoc -> Module -> TcRn ModIface
loadInterfaceForModule doc m
  = do
    -- Should not be called with this module
    when debugIsOn $ do
      this_mod <- getModule
      MASSERT2( this_mod /= m, ppr m <+> parens doc )
    initIfaceTcRn $ loadSysInterface doc m

{-
*********************************************************
*                                                      *
                loadInterface

        The main function to load an interface
        for an imported module, and put it in
        the External Package State
*                                                      *
*********************************************************
-}

-- | An 'IfM' function to load the home interface for a wired-in thing,
-- so that we're sure that we see its instance declarations and rules
-- See Note [Loading instances for wired-in things]
loadWiredInHomeIface :: Name -> IfM lcl ()
loadWiredInHomeIface name
  = ASSERT( isWiredInName name )
    do _ <- loadSysInterface doc (nameModule name); return ()
  where
    doc = text "Need home interface for wired-in thing" <+> ppr name

------------------
-- | Loads a system interface and throws an exception if it fails
loadSysInterface :: SDoc -> Module -> IfM lcl ModIface
loadSysInterface doc mod_name = loadInterfaceWithException doc mod_name ImportBySystem

------------------
-- | Loads a user interface and throws an exception if it fails. The first parameter indicates
-- whether we should import the boot variant of the module
loadUserInterface :: IsBootInterface -> SDoc -> Module -> IfM lcl ModIface
loadUserInterface is_boot doc mod_name
  = loadInterfaceWithException doc mod_name (ImportByUser is_boot)

loadPluginInterface :: SDoc -> Module -> IfM lcl ModIface
loadPluginInterface doc mod_name
  = loadInterfaceWithException doc mod_name ImportByPlugin

------------------
-- | A wrapper for 'loadInterface' that throws an exception if it fails
loadInterfaceWithException :: SDoc -> Module -> WhereFrom -> IfM lcl ModIface
loadInterfaceWithException doc mod_name where_from
  = withException (loadInterface doc mod_name where_from)

------------------
loadInterface :: SDoc -> Module -> WhereFrom
              -> IfM lcl (MaybeErr MsgDoc ModIface)

-- loadInterface looks in both the HPT and PIT for the required interface
-- If not found, it loads it, and puts it in the PIT (always).

-- If it can't find a suitable interface file, we
--      a) modify the PackageIfaceTable to have an empty entry
--              (to avoid repeated complaints)
--      b) return (Left message)
--
-- It's not necessarily an error for there not to be an interface
-- file -- perhaps the module has changed, and that interface
-- is no longer used

loadInterface doc_str mod from
  | isHoleModule mod
  -- Hole modules get special treatment
  = do dflags <- getDynFlags
       -- Redo search for our local hole module
       loadInterface doc_str (mkModule (thisPackage dflags) (moduleName mod)) from
  | otherwise
  = withTimingSilentD (text "loading interface") (pure ()) $
    do  {       -- Read the state
          (eps,hpt) <- getEpsAndHpt
        ; gbl_env <- getGblEnv

        ; traceIf (text "Considering whether to load" <+> ppr mod <+> ppr from)

                -- Check whether we have the interface already
        ; dflags <- getDynFlags
        ; case lookupIfaceByModule hpt (eps_PIT eps) mod of {
            Just iface
                -> return (Succeeded iface) ;   -- Already loaded
                        -- The (src_imp == mi_boot iface) test checks that the already-loaded
                        -- interface isn't a boot iface.  This can conceivably happen,
                        -- if an earlier import had a before we got to real imports.   I think.
            _ -> do {

        -- READ THE MODULE IN
        ; read_result <- case (wantHiBootFile dflags eps mod from) of
                           Failed err             -> return (Failed err)
                           Succeeded hi_boot_file -> computeInterface doc_str hi_boot_file mod
        ; case read_result of {
            Failed err -> do
                { let fake_iface = emptyFullModIface mod

                ; updateEps_ $ \eps ->
                        eps { eps_PIT = extendModuleEnv (eps_PIT eps) (mi_module fake_iface) fake_iface }
                        -- Not found, so add an empty iface to
                        -- the EPS map so that we don't look again

                ; return (Failed err) } ;

        -- Found and parsed!
        -- We used to have a sanity check here that looked for:
        --  * System importing ..
        --  * a home package module ..
        --  * that we know nothing about (mb_dep == Nothing)!
        --
        -- But this is no longer valid because thNameToGhcName allows users to
        -- cause the system to load arbitrary interfaces (by supplying an appropriate
        -- Template Haskell original-name).
            Succeeded (iface, loc) ->
        let
            loc_doc = text loc
        in
        initIfaceLcl (mi_semantic_module iface) loc_doc (mi_boot iface) $ do

        dontLeakTheHPT $ do

        --      Load the new ModIface into the External Package State
        -- Even home-package interfaces loaded by loadInterface
        --      (which only happens in OneShot mode; in Batch/Interactive
        --      mode, home-package modules are loaded one by one into the HPT)
        -- are put in the EPS.
        --
        -- The main thing is to add the ModIface to the PIT, but
        -- we also take the
        --      IfaceDecls, IfaceClsInst, IfaceFamInst, IfaceRules,
        -- out of the ModIface and put them into the big EPS pools

        -- NB: *first* we do loadDecl, so that the provenance of all the locally-defined
        ---    names is done correctly (notably, whether this is an .hi file or .hi-boot file).
        --     If we do loadExport first the wrong info gets into the cache (unless we
        --      explicitly tag each export which seems a bit of a bore)

        ; ignore_prags      <- goptM Opt_IgnoreInterfacePragmas
        ; new_eps_decls     <- loadDecls ignore_prags (mi_decls iface)
        ; new_eps_insts     <- mapM tcIfaceInst (mi_insts iface)
        ; new_eps_fam_insts <- mapM tcIfaceFamInst (mi_fam_insts iface)
        ; new_eps_rules     <- tcIfaceRules ignore_prags (mi_rules iface)
        ; new_eps_anns      <- tcIfaceAnnotations (mi_anns iface)
        ; new_eps_complete_sigs <- tcIfaceCompleteSigs (mi_complete_sigs iface)

        ; let { final_iface = iface {
                                mi_decls     = panic "No mi_decls in PIT",
                                mi_insts     = panic "No mi_insts in PIT",
                                mi_fam_insts = panic "No mi_fam_insts in PIT",
                                mi_rules     = panic "No mi_rules in PIT",
                                mi_anns      = panic "No mi_anns in PIT"
                              }
               }

        ; let bad_boot = mi_boot iface && fmap fst (if_rec_types gbl_env) == Just mod
                            -- Warn warn against an EPS-updating import
                            -- of one's own boot file! (one-shot only)
                            -- See Note [Loading your own hi-boot file]
                            -- in GHC.Iface.Utils.

        ; WARN( bad_boot, ppr mod )
          updateEps_  $ \ eps ->
           if elemModuleEnv mod (eps_PIT eps) || is_external_sig dflags iface
                then eps
           else if bad_boot
                -- See Note [Loading your own hi-boot file]
                then eps { eps_PTE = addDeclsToPTE (eps_PTE eps) new_eps_decls }
           else
                eps {
                  eps_PIT          = extendModuleEnv (eps_PIT eps) mod final_iface,
                  eps_PTE          = addDeclsToPTE   (eps_PTE eps) new_eps_decls,
                  eps_rule_base    = extendRuleBaseList (eps_rule_base eps)
                                                        new_eps_rules,
                  eps_complete_matches
                                   = extendCompleteMatchMap
                                         (eps_complete_matches eps)
                                         new_eps_complete_sigs,
                  eps_inst_env     = extendInstEnvList (eps_inst_env eps)
                                                       new_eps_insts,
                  eps_fam_inst_env = extendFamInstEnvList (eps_fam_inst_env eps)
                                                          new_eps_fam_insts,
                  eps_ann_env      = extendAnnEnvList (eps_ann_env eps)
                                                      new_eps_anns,
                  eps_mod_fam_inst_env
                                   = let
                                       fam_inst_env =
                                         extendFamInstEnvList emptyFamInstEnv
                                                              new_eps_fam_insts
                                     in
                                     extendModuleEnv (eps_mod_fam_inst_env eps)
                                                     mod
                                                     fam_inst_env,
                  eps_stats        = addEpsInStats (eps_stats eps)
                                                   (length new_eps_decls)
                                                   (length new_eps_insts)
                                                   (length new_eps_rules) }

        ; -- invoke plugins
          res <- withPlugins dflags interfaceLoadAction final_iface
        ; return (Succeeded res)
    }}}}

{- Note [Loading your own hi-boot file]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking, when compiling module M, we should not
load M.hi boot into the EPS.  After all, we are very shortly
going to have full information about M.  Moreover, see
Note [Do not update EPS with your own hi-boot] in GHC.Iface.Utils.

But there is a HORRIBLE HACK here.

* At the end of tcRnImports, we call checkFamInstConsistency to
  check consistency of imported type-family instances
  See Note [The type family instance consistency story] in FamInst

* Alas, those instances may refer to data types defined in M,
  if there is a M.hs-boot.

* And that means we end up loading M.hi-boot, because those
  data types are not yet in the type environment.

But in this weird case, /all/ we need is the types. We don't need
instances, rules etc.  And if we put the instances in the EPS
we get "duplicate instance" warnings when we compile the "real"
instance in M itself.  Hence the strange business of just updateing
the eps_PTE.

This really happens in practice.  The module HsExpr.hs gets
"duplicate instance" errors if this hack is not present.

This is a mess.


Note [HPT space leak] (#15111)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In IfL, we defer some work until it is demanded using forkM, such
as building TyThings from IfaceDecls. These thunks are stored in
the ExternalPackageState, and they might never be poked.  If we're
not careful, these thunks will capture the state of the loaded
program when we read an interface file, and retain all that data
for ever.

Therefore, when loading a package interface file , we use a "clean"
version of the HscEnv with all the data about the currently loaded
program stripped out. Most of the fields can be panics because
we'll never read them, but hsc_HPT needs to be empty because this
interface will cause other interfaces to be loaded recursively, and
when looking up those interfaces we use the HPT in loadInterface.
We know that none of the interfaces below here can refer to
home-package modules however, so it's safe for the HPT to be empty.
-}

dontLeakTheHPT :: IfL a -> IfL a
dontLeakTheHPT thing_inside = do
  let
    cleanTopEnv HscEnv{..} =
       let
         -- wrinkle: when we're typechecking in --backpack mode, the
         -- instantiation of a signature might reside in the HPT, so
         -- this case breaks the assumption that EPS interfaces only
         -- refer to other EPS interfaces. We can detect when we're in
         -- typechecking-only mode by using hscTarget==HscNothing, and
         -- in that case we don't empty the HPT.  (admittedly this is
         -- a bit of a hack, better suggestions welcome). A number of
         -- tests in testsuite/tests/backpack break without this
         -- tweak.
         !hpt | hscTarget hsc_dflags == HscNothing = hsc_HPT
              | otherwise = emptyHomePackageTable
       in
       HscEnv {  hsc_targets      = panic "cleanTopEnv: hsc_targets"
              ,  hsc_mod_graph    = panic "cleanTopEnv: hsc_mod_graph"
              ,  hsc_IC           = panic "cleanTopEnv: hsc_IC"
              ,  hsc_HPT          = hpt
              , .. }

  updTopEnv cleanTopEnv $ do
  !_ <- getTopEnv        -- force the updTopEnv
  thing_inside


-- | Returns @True@ if a 'ModIface' comes from an external package.
-- In this case, we should NOT load it into the EPS; the entities
-- should instead come from the local merged signature interface.
is_external_sig :: DynFlags -> ModIface -> Bool
is_external_sig dflags iface =
    -- It's a signature iface...
    mi_semantic_module iface /= mi_module iface &&
    -- and it's not from the local package
    moduleUnitId (mi_module iface) /= thisPackage dflags

-- | This is an improved version of 'findAndReadIface' which can also
-- handle the case when a user requests @p[A=<B>]:M@ but we only
-- have an interface for @p[A=<A>]:M@ (the indefinite interface.
-- If we are not trying to build code, we load the interface we have,
-- *instantiating it* according to how the holes are specified.
-- (Of course, if we're actually building code, this is a hard error.)
--
-- In the presence of holes, 'computeInterface' has an important invariant:
-- to load module M, its set of transitively reachable requirements must
-- have an up-to-date local hi file for that requirement.  Note that if
-- we are loading the interface of a requirement, this does not
-- apply to the requirement itself; e.g., @p[A=<A>]:A@ does not require
-- A.hi to be up-to-date (and indeed, we MUST NOT attempt to read A.hi, unless
-- we are actually typechecking p.)
computeInterface ::
       SDoc -> IsBootInterface -> Module
    -> TcRnIf gbl lcl (MaybeErr MsgDoc (ModIface, FilePath))
computeInterface doc_str hi_boot_file mod0 = do
    MASSERT( not (isHoleModule mod0) )
    dflags <- getDynFlags
    case splitModuleInsts mod0 of
        (imod, Just indef) | not (unitIdIsDefinite (thisPackage dflags)) -> do
            r <- findAndReadIface doc_str imod mod0 hi_boot_file
            case r of
                Succeeded (iface0, path) -> do
                    hsc_env <- getTopEnv
                    r <- liftIO $
                        rnModIface hsc_env (indefUnitIdInsts (indefModuleUnitId indef))
                                   Nothing iface0
                    case r of
                        Right x -> return (Succeeded (x, path))
                        Left errs -> liftIO . throwIO . mkSrcErr $ errs
                Failed err -> return (Failed err)
        (mod, _) ->
            findAndReadIface doc_str mod mod0 hi_boot_file

-- | Compute the signatures which must be compiled in order to
-- load the interface for a 'Module'.  The output of this function
-- is always a subset of 'moduleFreeHoles'; it is more precise
-- because in signature @p[A=<A>,B=<B>]:B@, although the free holes
-- are A and B, B might not depend on A at all!
--
-- If this is invoked on a signature, this does NOT include the
-- signature itself; e.g. precise free module holes of
-- @p[A=<A>,B=<B>]:B@ never includes B.
moduleFreeHolesPrecise
    :: SDoc -> Module
    -> TcRnIf gbl lcl (MaybeErr MsgDoc (UniqDSet ModuleName))
moduleFreeHolesPrecise doc_str mod
 | moduleIsDefinite mod = return (Succeeded emptyUniqDSet)
 | otherwise =
   case splitModuleInsts mod of
    (imod, Just indef) -> do
        let insts = indefUnitIdInsts (indefModuleUnitId indef)
        traceIf (text "Considering whether to load" <+> ppr mod <+>
                 text "to compute precise free module holes")
        (eps, hpt) <- getEpsAndHpt
        case tryEpsAndHpt eps hpt `firstJust` tryDepsCache eps imod insts of
            Just r -> return (Succeeded r)
            Nothing -> readAndCache imod insts
    (_, Nothing) -> return (Succeeded emptyUniqDSet)
  where
    tryEpsAndHpt eps hpt =
        fmap mi_free_holes (lookupIfaceByModule hpt (eps_PIT eps) mod)
    tryDepsCache eps imod insts =
        case lookupInstalledModuleEnv (eps_free_holes eps) imod of
            Just ifhs  -> Just (renameFreeHoles ifhs insts)
            _otherwise -> Nothing
    readAndCache imod insts = do
        mb_iface <- findAndReadIface (text "moduleFreeHolesPrecise" <+> doc_str) imod mod False
        case mb_iface of
            Succeeded (iface, _) -> do
                let ifhs = mi_free_holes iface
                -- Cache it
                updateEps_ (\eps ->
                    eps { eps_free_holes = extendInstalledModuleEnv (eps_free_holes eps) imod ifhs })
                return (Succeeded (renameFreeHoles ifhs insts))
            Failed err -> return (Failed err)

wantHiBootFile :: DynFlags -> ExternalPackageState -> Module -> WhereFrom
               -> MaybeErr MsgDoc IsBootInterface
-- Figure out whether we want Foo.hi or Foo.hi-boot
wantHiBootFile dflags eps mod from
  = case from of
       ImportByUser usr_boot
          | usr_boot && not this_package
          -> Failed (badSourceImport mod)
          | otherwise -> Succeeded usr_boot

       ImportByPlugin
          -> Succeeded False

       ImportBySystem
          | not this_package   -- If the module to be imported is not from this package
          -> Succeeded False   -- don't look it up in eps_is_boot, because that is keyed
                               -- on the ModuleName of *home-package* modules only.
                               -- We never import boot modules from other packages!

          | otherwise
          -> case lookupUFM (eps_is_boot eps) (moduleName mod) of
                Just (ModuleNameWithIsBoot _ is_boot) -> Succeeded is_boot
                Nothing           -> Succeeded False
                     -- The boot-ness of the requested interface,
                     -- based on the dependencies in directly-imported modules
  where
    this_package = thisPackage dflags == moduleUnitId mod

badSourceImport :: Module -> SDoc
badSourceImport mod
  = hang (text "You cannot {-# SOURCE #-} import a module from another package")
       2 (text "but" <+> quotes (ppr mod) <+> ptext (sLit "is from package")
          <+> quotes (ppr (moduleUnitId mod)))

-----------------------------------------------------
--      Loading type/class/value decls
-- We pass the full Module name here, replete with
-- its package info, so that we can build a Name for
-- each binder with the right package info in it
-- All subsequent lookups, including crucially lookups during typechecking
-- the declaration itself, will find the fully-glorious Name
--
-- We handle ATs specially.  They are not main declarations, but also not
-- implicit things (in particular, adding them to `implicitTyThings' would mess
-- things up in the renaming/type checking of source programs).
-----------------------------------------------------

addDeclsToPTE :: PackageTypeEnv -> [(Name,TyThing)] -> PackageTypeEnv
addDeclsToPTE pte things = extendNameEnvList pte things

loadDecls :: Bool
          -> [(Fingerprint, IfaceDecl)]
          -> IfL [(Name,TyThing)]
loadDecls ignore_prags ver_decls
   = do { thingss <- mapM (loadDecl ignore_prags) ver_decls
        ; return (concat thingss)
        }

loadDecl :: Bool                    -- Don't load pragmas into the decl pool
          -> (Fingerprint, IfaceDecl)
          -> IfL [(Name,TyThing)]   -- The list can be poked eagerly, but the
                                    -- TyThings are forkM'd thunks
loadDecl ignore_prags (_version, decl)
  = do  {       -- Populate the name cache with final versions of all
                -- the names associated with the decl
          let main_name = ifName decl

        -- Typecheck the thing, lazily
        -- NB. Firstly, the laziness is there in case we never need the
        -- declaration (in one-shot mode), and secondly it is there so that
        -- we don't look up the occurrence of a name before calling mk_new_bndr
        -- on the binder.  This is important because we must get the right name
        -- which includes its nameParent.

        ; thing <- forkM doc $ do { bumpDeclStats main_name
                                  ; tcIfaceDecl ignore_prags decl }

        -- Populate the type environment with the implicitTyThings too.
        --
        -- Note [Tricky iface loop]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~
        -- Summary: The delicate point here is that 'mini-env' must be
        -- buildable from 'thing' without demanding any of the things
        -- 'forkM'd by tcIfaceDecl.
        --
        -- In more detail: Consider the example
        --      data T a = MkT { x :: T a }
        -- The implicitTyThings of T are:  [ <datacon MkT>, <selector x>]
        -- (plus their workers, wrappers, coercions etc etc)
        --
        -- We want to return an environment
        --      [ "MkT" -> <datacon MkT>, "x" -> <selector x>, ... ]
        -- (where the "MkT" is the *Name* associated with MkT, etc.)
        --
        -- We do this by mapping the implicit_names to the associated
        -- TyThings.  By the invariant on ifaceDeclImplicitBndrs and
        -- implicitTyThings, we can use getOccName on the implicit
        -- TyThings to make this association: each Name's OccName should
        -- be the OccName of exactly one implicitTyThing.  So the key is
        -- to define a "mini-env"
        --
        -- [ 'MkT' -> <datacon MkT>, 'x' -> <selector x>, ... ]
        -- where the 'MkT' here is the *OccName* associated with MkT.
        --
        -- However, there is a subtlety: due to how type checking needs
        -- to be staged, we can't poke on the forkM'd thunks inside the
        -- implicitTyThings while building this mini-env.
        -- If we poke these thunks too early, two problems could happen:
        --    (1) When processing mutually recursive modules across
        --        hs-boot boundaries, poking too early will do the
        --        type-checking before the recursive knot has been tied,
        --        so things will be type-checked in the wrong
        --        environment, and necessary variables won't be in
        --        scope.
        --
        --    (2) Looking up one OccName in the mini_env will cause
        --        others to be looked up, which might cause that
        --        original one to be looked up again, and hence loop.
        --
        -- The code below works because of the following invariant:
        -- getOccName on a TyThing does not force the suspended type
        -- checks in order to extract the name. For example, we don't
        -- poke on the "T a" type of <selector x> on the way to
        -- extracting <selector x>'s OccName. Of course, there is no
        -- reason in principle why getting the OccName should force the
        -- thunks, but this means we need to be careful in
        -- implicitTyThings and its helper functions.
        --
        -- All a bit too finely-balanced for my liking.

        -- This mini-env and lookup function mediates between the
        --'Name's n and the map from 'OccName's to the implicit TyThings
        ; let mini_env = mkOccEnv [(getOccName t, t) | t <- implicitTyThings thing]
              lookup n = case lookupOccEnv mini_env (getOccName n) of
                           Just thing -> thing
                           Nothing    ->
                             pprPanic "loadDecl" (ppr main_name <+> ppr n $$ ppr (decl))

        ; implicit_names <- mapM lookupIfaceTop (ifaceDeclImplicitBndrs decl)

--         ; traceIf (text "Loading decl for " <> ppr main_name $$ ppr implicit_names)
        ; return $ (main_name, thing) :
                      -- uses the invariant that implicit_names and
                      -- implicitTyThings are bijective
                      [(n, lookup n) | n <- implicit_names]
        }
  where
    doc = text "Declaration for" <+> ppr (ifName decl)

bumpDeclStats :: Name -> IfL ()         -- Record that one more declaration has actually been used
bumpDeclStats name
  = do  { traceIf (text "Loading decl for" <+> ppr name)
        ; updateEps_ (\eps -> let stats = eps_stats eps
                              in eps { eps_stats = stats { n_decls_out = n_decls_out stats + 1 } })
        }

{-
*********************************************************
*                                                      *
\subsection{Reading an interface file}
*                                                      *
*********************************************************

Note [Home module load error]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the sought-for interface is in the current package (as determined
by -package-name flag) then it jolly well should already be in the HPT
because we process home-package modules in dependency order.  (Except
in one-shot mode; see notes with hsc_HPT decl in HscTypes).

It is possible (though hard) to get this error through user behaviour.
  * Suppose package P (modules P1, P2) depends on package Q (modules Q1,
    Q2, with Q2 importing Q1)
  * We compile both packages.
  * Now we edit package Q so that it somehow depends on P
  * Now recompile Q with --make (without recompiling P).
  * Then Q1 imports, say, P1, which in turn depends on Q2. So Q2
    is a home-package module which is not yet in the HPT!  Disaster.

This actually happened with P=base, Q=ghc-prim, via the AMP warnings.
See #8320.
-}

findAndReadIface :: SDoc
                 -- The unique identifier of the on-disk module we're
                 -- looking for
                 -> InstalledModule
                 -- The *actual* module we're looking for.  We use
                 -- this to check the consistency of the requirements
                 -- of the module we read out.
                 -> Module
                 -> IsBootInterface     -- True  <=> Look for a .hi-boot file
                                        -- False <=> Look for .hi file
                 -> TcRnIf gbl lcl (MaybeErr MsgDoc (ModIface, FilePath))
        -- Nothing <=> file not found, or unreadable, or illegible
        -- Just x  <=> successfully found and parsed

        -- It *doesn't* add an error to the monad, because
        -- sometimes it's ok to fail... see notes with loadInterface
findAndReadIface doc_str mod wanted_mod_with_insts hi_boot_file
  = do traceIf (sep [hsep [text "Reading",
                           if hi_boot_file
                             then text "[boot]"
                             else Outputable.empty,
                           text "interface for",
                           ppr mod <> semi],
                     nest 4 (text "reason:" <+> doc_str)])

       -- Check for GHC.Prim, and return its static interface
       -- TODO: make this check a function
       if mod `installedModuleEq` gHC_PRIM
           then do
               iface <- getHooked ghcPrimIfaceHook ghcPrimIface
               return (Succeeded (iface,
                                   "<built in interface for GHC.Prim>"))
           else do
               dflags <- getDynFlags
               -- Look for the file
               hsc_env <- getTopEnv
               mb_found <- liftIO (findExactModule hsc_env mod)
               case mb_found of
                   InstalledFound loc mod -> do
                       -- Found file, so read it
                       let file_path = addBootSuffix_maybe hi_boot_file
                                                           (ml_hi_file loc)

                       -- See Note [Home module load error]
                       if installedModuleUnitId mod `installedUnitIdEq` thisPackage dflags &&
                          not (isOneShot (ghcMode dflags))
                           then return (Failed (homeModError mod loc))
                           else do r <- read_file file_path
                                   checkBuildDynamicToo r
                                   return r
                   err -> do
                       traceIf (text "...not found")
                       dflags <- getDynFlags
                       return (Failed (cannotFindInterface dflags
                                           (installedModuleName mod) err))
    where read_file file_path = do
              traceIf (text "readIFace" <+> text file_path)
              -- Figure out what is recorded in mi_module.  If this is
              -- a fully definite interface, it'll match exactly, but
              -- if it's indefinite, the inside will be uninstantiated!
              dflags <- getDynFlags
              let wanted_mod =
                    case splitModuleInsts wanted_mod_with_insts of
                        (_, Nothing) -> wanted_mod_with_insts
                        (_, Just indef_mod) ->
                          indefModuleToModule dflags
                            (generalizeIndefModule indef_mod)
              read_result <- readIface wanted_mod file_path
              case read_result of
                Failed err -> return (Failed (badIfaceFile file_path err))
                Succeeded iface -> return (Succeeded (iface, file_path))
                            -- Don't forget to fill in the package name...
          checkBuildDynamicToo (Succeeded (iface, filePath)) = do
              dflags <- getDynFlags
              -- Indefinite interfaces are ALWAYS non-dynamic, and
              -- that's OK.
              let is_definite_iface = moduleIsDefinite (mi_module iface)
              when is_definite_iface $
                whenGeneratingDynamicToo dflags $ withDoDynamicToo $ do
                  let ref = canGenerateDynamicToo dflags
                      dynFilePath = addBootSuffix_maybe hi_boot_file
                                  $ replaceExtension filePath (dynHiSuf dflags)
                  r <- read_file dynFilePath
                  case r of
                      Succeeded (dynIface, _)
                       | mi_mod_hash (mi_final_exts iface) == mi_mod_hash (mi_final_exts dynIface) ->
                          return ()
                       | otherwise ->
                          do traceIf (text "Dynamic hash doesn't match")
                             liftIO $ writeIORef ref False
                      Failed err ->
                          do traceIf (text "Failed to load dynamic interface file:" $$ err)
                             liftIO $ writeIORef ref False
          checkBuildDynamicToo _ = return ()

-- @readIface@ tries just the one file.

readIface :: Module -> FilePath
          -> TcRnIf gbl lcl (MaybeErr MsgDoc ModIface)
        -- Failed err    <=> file not found, or unreadable, or illegible
        -- Succeeded iface <=> successfully found and parsed

readIface wanted_mod file_path
  = do  { res <- tryMostM $
                 readBinIface CheckHiWay QuietBinIFaceReading file_path
        ; dflags <- getDynFlags
        ; case res of
            Right iface
                -- NB: This check is NOT just a sanity check, it is
                -- critical for correctness of recompilation checking
                -- (it lets us tell when -this-unit-id has changed.)
                | wanted_mod == actual_mod
                                -> return (Succeeded iface)
                | otherwise     -> return (Failed err)
                where
                  actual_mod = mi_module iface
                  err = hiModuleNameMismatchWarn dflags wanted_mod actual_mod

            Left exn    -> return (Failed (text (showException exn)))
    }

{-
*********************************************************
*                                                       *
        Wired-in interface for GHC.Prim
*                                                       *
*********************************************************
-}

initExternalPackageState :: ExternalPackageState
initExternalPackageState
  = EPS {
      eps_is_boot          = emptyUFM,
      eps_PIT              = emptyPackageIfaceTable,
      eps_free_holes       = emptyInstalledModuleEnv,
      eps_PTE              = emptyTypeEnv,
      eps_inst_env         = emptyInstEnv,
      eps_fam_inst_env     = emptyFamInstEnv,
      eps_rule_base        = mkRuleBase builtinRules,
        -- Initialise the EPS rule pool with the built-in rules
      eps_mod_fam_inst_env
                           = emptyModuleEnv,
      eps_complete_matches = emptyUFM,
      eps_ann_env          = emptyAnnEnv,
      eps_stats = EpsStats { n_ifaces_in = 0, n_decls_in = 0, n_decls_out = 0
                           , n_insts_in = 0, n_insts_out = 0
                           , n_rules_in = length builtinRules, n_rules_out = 0 }
    }

{-
*********************************************************
*                                                       *
        Wired-in interface for GHC.Prim
*                                                       *
*********************************************************
-}

ghcPrimIface :: ModIface
ghcPrimIface
  = empty_iface {
        mi_exports  = ghcPrimExports,
        mi_decls    = [],
        mi_fixities = fixities,
        mi_final_exts = (mi_final_exts empty_iface){ mi_fix_fn = mkIfaceFixCache fixities }
        }
  where
    empty_iface = emptyFullModIface gHC_PRIM

    -- The fixities listed here for @`seq`@ or @->@ should match
    -- those in primops.txt.pp (from which Haddock docs are generated).
    fixities = (getOccName seqId, Fixity NoSourceText 0 InfixR)
             : (occName funTyConName, funTyFixity)  -- trac #10145
             : mapMaybe mkFixity allThePrimOps
    mkFixity op = (,) (primOpOcc op) <$> primOpFixity op

{-
*********************************************************
*                                                      *
\subsection{Statistics}
*                                                      *
*********************************************************
-}

ifaceStats :: ExternalPackageState -> SDoc
ifaceStats eps
  = hcat [text "Renamer stats: ", msg]
  where
    stats = eps_stats eps
    msg = vcat
        [int (n_ifaces_in stats) <+> text "interfaces read",
         hsep [ int (n_decls_out stats), text "type/class/variable imported, out of",
                int (n_decls_in stats), text "read"],
         hsep [ int (n_insts_out stats), text "instance decls imported, out of",
                int (n_insts_in stats), text "read"],
         hsep [ int (n_rules_out stats), text "rule decls imported, out of",
                int (n_rules_in stats), text "read"]
        ]

{-
************************************************************************
*                                                                      *
                Printing interfaces
*                                                                      *
************************************************************************

Note [Name qualification with --show-iface]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to disambiguate between identifiers from different modules, we qualify
all names that don't originate in the current module. In order to keep visual
noise as low as possible, we keep local names unqualified.

For some background on this choice see trac #15269.
-}

-- | Read binary interface, and print it out
showIface :: HscEnv -> FilePath -> IO ()
showIface hsc_env filename = do
   -- skip the hi way check; we don't want to worry about profiled vs.
   -- non-profiled interfaces, for example.
   iface <- initTcRnIf 's' hsc_env () () $
       readBinIface IgnoreHiWay TraceBinIFaceReading filename
   let dflags = hsc_dflags hsc_env
       -- See Note [Name qualification with --show-iface]
       qualifyImportedNames mod _
           | mod == mi_module iface = NameUnqual
           | otherwise              = NameNotInScope1
       print_unqual = QueryQualify qualifyImportedNames
                                   neverQualifyModules
                                   neverQualifyPackages
   putLogMsg dflags NoReason SevDump noSrcSpan
      (mkDumpStyle dflags print_unqual) (pprModIface iface)

-- Show a ModIface but don't display details; suitable for ModIfaces stored in
-- the EPT.
pprModIfaceSimple :: ModIface -> SDoc
pprModIfaceSimple iface = ppr (mi_module iface) $$ pprDeps (mi_deps iface) $$ nest 2 (vcat (map pprExport (mi_exports iface)))

pprModIface :: ModIface -> SDoc
-- Show a ModIface
pprModIface iface@ModIface{ mi_final_exts = exts }
 = vcat [ text "interface"
                <+> ppr (mi_module iface) <+> pp_hsc_src (mi_hsc_src iface)
                <+> (if mi_orphan exts then text "[orphan module]" else Outputable.empty)
                <+> (if mi_finsts exts then text "[family instance module]" else Outputable.empty)
                <+> (if mi_hpc iface then text "[hpc]" else Outputable.empty)
                <+> integer hiVersion
        , nest 2 (text "interface hash:" <+> ppr (mi_iface_hash exts))
        , nest 2 (text "ABI hash:" <+> ppr (mi_mod_hash exts))
        , nest 2 (text "export-list hash:" <+> ppr (mi_exp_hash exts))
        , nest 2 (text "orphan hash:" <+> ppr (mi_orphan_hash exts))
        , nest 2 (text "flag hash:" <+> ppr (mi_flag_hash exts))
        , nest 2 (text "opt_hash:" <+> ppr (mi_opt_hash exts))
        , nest 2 (text "hpc_hash:" <+> ppr (mi_hpc_hash exts))
        , nest 2 (text "plugin_hash:" <+> ppr (mi_plugin_hash exts))
        , nest 2 (text "sig of:" <+> ppr (mi_sig_of iface))
        , nest 2 (text "used TH splices:" <+> ppr (mi_used_th iface))
        , nest 2 (text "where")
        , text "exports:"
        , nest 2 (vcat (map pprExport (mi_exports iface)))
        , pprDeps (mi_deps iface)
        , vcat (map pprUsage (mi_usages iface))
        , vcat (map pprIfaceAnnotation (mi_anns iface))
        , pprFixities (mi_fixities iface)
        , vcat [ppr ver $$ nest 2 (ppr decl) | (ver,decl) <- mi_decls iface]
        , vcat (map ppr (mi_insts iface))
        , vcat (map ppr (mi_fam_insts iface))
        , vcat (map ppr (mi_rules iface))
        , ppr (mi_warns iface)
        , pprTrustInfo (mi_trust iface)
        , pprTrustPkg (mi_trust_pkg iface)
        , vcat (map ppr (mi_complete_sigs iface))
        , text "module header:" $$ nest 2 (ppr (mi_doc_hdr iface))
        , text "declaration docs:" $$ nest 2 (ppr (mi_decl_docs iface))
        , text "arg docs:" $$ nest 2 (ppr (mi_arg_docs iface))
        ]
  where
    pp_hsc_src HsBootFile = text "[boot]"
    pp_hsc_src HsigFile = text "[hsig]"
    pp_hsc_src HsSrcFile = Outputable.empty

{-
When printing export lists, we print like this:
        Avail   f               f
        AvailTC C [C, x, y]     C(x,y)
        AvailTC C [x, y]        C!(x,y)         -- Exporting x, y but not C
-}

pprExport :: IfaceExport -> SDoc
pprExport (Avail n)         = ppr n
pprExport (AvailTC _ [] []) = Outputable.empty
pprExport (AvailTC n ns0 fs)
  = case ns0 of
      (n':ns) | n==n' -> ppr n <> pp_export ns fs
      _               -> ppr n <> vbar <> pp_export ns0 fs
  where
    pp_export []    [] = Outputable.empty
    pp_export names fs = braces (hsep (map ppr names ++ map (ppr . flLabel) fs))

pprUsage :: Usage -> SDoc
pprUsage usage@UsagePackageModule{}
  = pprUsageImport usage usg_mod
pprUsage usage@UsageHomeModule{}
  = pprUsageImport usage usg_mod_name $$
    nest 2 (
        maybe Outputable.empty (\v -> text "exports: " <> ppr v) (usg_exports usage) $$
        vcat [ ppr n <+> ppr v | (n,v) <- usg_entities usage ]
        )
pprUsage usage@UsageFile{}
  = hsep [text "addDependentFile",
          doubleQuotes (text (usg_file_path usage)),
          ppr (usg_file_hash usage)]
pprUsage usage@UsageMergedRequirement{}
  = hsep [text "merged", ppr (usg_mod usage), ppr (usg_mod_hash usage)]

pprUsageImport :: Outputable a => Usage -> (Usage -> a) -> SDoc
pprUsageImport usage usg_mod'
  = hsep [text "import", safe, ppr (usg_mod' usage),
                       ppr (usg_mod_hash usage)]
    where
        safe | usg_safe usage = text "safe"
             | otherwise      = text " -/ "

pprDeps :: Dependencies -> SDoc
pprDeps (Deps { dep_mods = mods, dep_pkgs = pkgs, dep_orphs = orphs,
                dep_finsts = finsts })
  = vcat [text "module dependencies:" <+> fsep (map ppr_mod mods),
          text "package dependencies:" <+> fsep (map ppr_pkg pkgs),
          text "orphans:" <+> fsep (map ppr orphs),
          text "family instance modules:" <+> fsep (map ppr finsts)
        ]
  where
    ppr_mod (ModuleNameWithIsBoot mod_name boot) = ppr mod_name <+> ppr_boot boot
    ppr_pkg (pkg,trust_req)  = ppr pkg <>
                               (if trust_req then text "*" else Outputable.empty)
    ppr_boot True  = text "[boot]"
    ppr_boot False = Outputable.empty

pprFixities :: [(OccName, Fixity)] -> SDoc
pprFixities []    = Outputable.empty
pprFixities fixes = text "fixities" <+> pprWithCommas pprFix fixes
                  where
                    pprFix (occ,fix) = ppr fix <+> ppr occ

pprTrustInfo :: IfaceTrustInfo -> SDoc
pprTrustInfo trust = text "trusted:" <+> ppr trust

pprTrustPkg :: Bool -> SDoc
pprTrustPkg tpkg = text "require own pkg trusted:" <+> ppr tpkg

instance Outputable Warnings where
    ppr = pprWarns

pprWarns :: Warnings -> SDoc
pprWarns NoWarnings         = Outputable.empty
pprWarns (WarnAll txt)  = text "Warn all" <+> ppr txt
pprWarns (WarnSome prs) = text "Warnings"
                        <+> vcat (map pprWarning prs)
    where pprWarning (name, txt) = ppr name <+> ppr txt

pprIfaceAnnotation :: IfaceAnnotation -> SDoc
pprIfaceAnnotation (IfaceAnnotation { ifAnnotatedTarget = target, ifAnnotatedValue = serialized })
  = ppr target <+> text "annotated by" <+> ppr serialized

{-
*********************************************************
*                                                       *
\subsection{Errors}
*                                                       *
*********************************************************
-}

badIfaceFile :: String -> SDoc -> SDoc
badIfaceFile file err
  = vcat [text "Bad interface file:" <+> text file,
          nest 4 err]

hiModuleNameMismatchWarn :: DynFlags -> Module -> Module -> MsgDoc
hiModuleNameMismatchWarn dflags requested_mod read_mod
 | moduleUnitId requested_mod == moduleUnitId read_mod =
    sep [text "Interface file contains module" <+> quotes (ppr read_mod) <> comma,
         text "but we were expecting module" <+> quotes (ppr requested_mod),
         sep [text "Probable cause: the source code which generated interface file",
             text "has an incompatible module name"
            ]
        ]
 | otherwise =
  -- ToDo: This will fail to have enough qualification when the package IDs
  -- are the same
  withPprStyle (mkUserStyle dflags alwaysQualify AllTheWay) $
    -- we want the Modules below to be qualified with package names,
    -- so reset the PrintUnqualified setting.
    hsep [ text "Something is amiss; requested module "
         , ppr requested_mod
         , text "differs from name found in the interface file"
         , ppr read_mod
         , parens (text "if these names look the same, try again with -dppr-debug")
         ]

homeModError :: InstalledModule -> ModLocation -> SDoc
-- See Note [Home module load error]
homeModError mod location
  = text "attempting to use module " <> quotes (ppr mod)
    <> (case ml_hs_file location of
           Just file -> space <> parens (text file)
           Nothing   -> Outputable.empty)
    <+> text "which is not loaded"
