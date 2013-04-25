%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Type checking of type signatures in interface files

\begin{code}
module TcIface ( 
        tcImportDecl, importDecl, checkWiredInTyCon, tcHiBootIface, typecheckIface, 
        tcIfaceDecl, tcIfaceInst, tcIfaceFamInst, tcIfaceRules,
        tcIfaceVectInfo, tcIfaceAnnotations, 
        tcIfaceExpr,    -- Desired by HERMIT (Trac #7683)
        tcIfaceGlobal, 
        tcExtCoreBindings
 ) where

#include "HsVersions.h"

import IfaceSyn
import LoadIface
import IfaceEnv
import BuildTyCl
import TcRnMonad
import TcType
import Type
import Coercion
import TypeRep
import HscTypes
import Annotations
import InstEnv
import FamInstEnv
import CoreSyn
import CoreUtils
import CoreUnfold
import CoreLint
import WorkWrap                     ( mkWrapper )
import MkCore                       ( castBottomExpr )
import Id
import MkId
import IdInfo
import Class
import TyCon
import CoAxiom
import DataCon
import PrelNames
import TysWiredIn
import TysPrim          ( superKindTyConName )
import BasicTypes       ( Arity, strongLoopBreaker )
import Literal
import qualified Var
import VarEnv
import VarSet
import Name
import NameEnv
import NameSet
import OccurAnal        ( occurAnalyseExpr )
import Demand           ( isBottomingSig )
import Module
import UniqFM
import UniqSupply
import Outputable       
import ErrUtils
import Maybes
import SrcLoc
import DynFlags
import Util
import FastString

import Control.Monad
\end{code}

This module takes

        IfaceDecl -> TyThing
        IfaceType -> Type
        etc

An IfaceDecl is populated with RdrNames, and these are not renamed to
Names before typechecking, because there should be no scope errors etc.

        -- For (b) consider: f = \$(...h....)
        -- where h is imported, and calls f via an hi-boot file.  
        -- This is bad!  But it is not seen as a staging error, because h
        -- is indeed imported.  We don't want the type-checker to black-hole 
        -- when simplifying and compiling the splice!
        --
        -- Simple solution: discard any unfolding that mentions a variable
        -- bound in this module (and hence not yet processed).
        -- The discarding happens when forkM finds a type error.

%************************************************************************
%*                                                                      *
%*      tcImportDecl is the key function for "faulting in"              *
%*      imported things
%*                                                                      *
%************************************************************************

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

\begin{code}
tcImportDecl :: Name -> TcM TyThing
-- Entry point for *source-code* uses of importDecl
tcImportDecl name 
  | Just thing <- wiredInNameTyThing_maybe name
  = do  { when (needWiredInHomeIface thing)
               (initIfaceTcRn (loadWiredInHomeIface name))
                -- See Note [Loading instances for wired-in things]
        ; return thing }
  | otherwise
  = do  { traceIf (text "tcImportDecl" <+> ppr name)
        ; mb_thing <- initIfaceTcRn (importDecl name)
        ; case mb_thing of
            Succeeded thing -> return thing
            Failed err      -> failWithTc err }

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
            Just thing -> return (Succeeded thing)
            Nothing    -> return (Failed not_found_msg)
    }}}
  where
    nd_doc = ptext (sLit "Need decl for") <+> ppr name
    not_found_msg = hang (ptext (sLit "Can't find interface-file declaration for") <+>
                                pprNameSpace (occNameSpace (nameOccName name)) <+> ppr name)
                       2 (vcat [ptext (sLit "Probable cause: bug in .hi-boot file, or inconsistent .hi file"),
                                ptext (sLit "Use -ddump-if-trace to get an idea of which file caused the error")])
\end{code}

%************************************************************************
%*                                                                      *
           Checks for wired-in things
%*                                                                      *
%************************************************************************

Note [Loading instances for wired-in things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to make sure that we have at least *read* the interface files
for any module with an instance decl or RULE that we might want.  

* If the instance decl is an orphan, we have a whole separate mechanism
  (loadOprhanModules)

* If the instance decl not an orphan, then the act of looking at the
  TyCon or Class will force in the defining module for the
  TyCon/Class, and hence the instance decl

* BUT, if the TyCon is a wired-in TyCon, we don't really need its interface;
  but we must make sure we read its interface in case it has instances or
  rules.  That is what LoadIface.loadWiredInHomeInterface does.  It's called
  from TcIface.{tcImportDecl, checkWiredInTyCon, ifCheckWiredInThing}

* HOWEVER, only do this for TyCons.  There are no wired-in Classes.  There
  are some wired-in Ids, but we don't want to load their interfaces. For
  example, Control.Exception.Base.recSelError is wired in, but that module
  is compiled late in the base library, and we don't want to force it to
  load before it's been compiled!

All of this is done by the type checker. The renamer plays no role.
(It used to, but no longer.)


\begin{code}
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
        ; ASSERT( isExternalName tc_name ) 
          when (mod /= nameModule tc_name)
               (initIfaceTcRn (loadWiredInHomeIface tc_name))
                -- Don't look for (non-existent) Float.hi when
                -- compiling Float.lhs, which mentions Float of course
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
\end{code}

%************************************************************************
%*                                                                      *
                Type-checking a complete interface
%*                                                                      *
%************************************************************************

Suppose we discover we don't need to recompile.  Then we must type
check the old interface file.  This is a bit different to the
incremental type checking we do as we suck in interface files.  Instead
we do things similarly as when we are typechecking source decls: we
bring into scope the type envt for the interface all at once, using a
knot.  Remember, the decls aren't necessarily in dependency order --
and even if they were, the type decls might be mutually recursive.

\begin{code}
typecheckIface :: ModIface      -- Get the decls from here
               -> TcRnIf gbl lcl ModDetails
typecheckIface iface
  = initIfaceTc iface $ \ tc_env_var -> do
        -- The tc_env_var is freshly allocated, private to 
        -- type-checking this particular interface
        {       -- Get the right set of decls and rules.  If we are compiling without -O
                -- we discard pragmas before typechecking, so that we don't "see"
                -- information that we shouldn't.  From a versioning point of view
                -- It's not actually *wrong* to do so, but in fact GHCi is unable 
                -- to handle unboxed tuples, so it must not see unfoldings.
          ignore_prags <- goptM Opt_IgnoreInterfacePragmas

                -- Typecheck the decls.  This is done lazily, so that the knot-tying
                -- within this single module work out right.  In the If monad there is
                -- no global envt for the current interface; instead, the knot is tied
                -- through the if_rec_types field of IfGblEnv
        ; names_w_things <- loadDecls ignore_prags (mi_decls iface)
        ; let type_env = mkNameEnv names_w_things
        ; writeMutVar tc_env_var type_env

                -- Now do those rules, instances and annotations
        ; insts     <- mapM tcIfaceInst (mi_insts iface)
        ; fam_insts <- mapM tcIfaceFamInst (mi_fam_insts iface)
        ; rules     <- tcIfaceRules ignore_prags (mi_rules iface)
        ; anns      <- tcIfaceAnnotations (mi_anns iface)

                -- Vectorisation information
        ; vect_info <- tcIfaceVectInfo (mi_module iface) type_env (mi_vect_info iface)

                -- Exports
        ; exports <- ifaceExportNames (mi_exports iface)

                -- Finished
        ; traceIf (vcat [text "Finished typechecking interface for" <+> ppr (mi_module iface),
                         text "Type envt:" <+> ppr type_env])
        ; return $ ModDetails { md_types     = type_env
                              , md_insts     = insts
                              , md_fam_insts = fam_insts
                              , md_rules     = rules
                              , md_anns      = anns
                              , md_vect_info = vect_info
                              , md_exports   = exports
                              }
    }
\end{code}


%************************************************************************
%*                                                                      *
                Type and class declarations
%*                                                                      *
%************************************************************************

\begin{code}
tcHiBootIface :: HscSource -> Module -> TcRn ModDetails
-- Load the hi-boot iface for the module being compiled,
-- if it indeed exists in the transitive closure of imports
-- Return the ModDetails, empty if no hi-boot iface
tcHiBootIface hsc_src mod
  | isHsBoot hsc_src            -- Already compiling a hs-boot file
  = return emptyModDetails
  | otherwise
  = do  { traceIf (text "loadHiBootInterface" <+> ppr mod)

        ; mode <- getGhcMode
        ; if not (isOneShot mode)
                -- In --make and interactive mode, if this module has an hs-boot file
                -- we'll have compiled it already, and it'll be in the HPT
                -- 
                -- We check wheher the interface is a *boot* interface.
                -- It can happen (when using GHC from Visual Studio) that we
                -- compile a module in TypecheckOnly mode, with a stable, 
                -- fully-populated HPT.  In that case the boot interface isn't there
                -- (it's been replaced by the mother module) so we can't check it.
                -- And that's fine, because if M's ModInfo is in the HPT, then 
                -- it's been compiled once, and we don't need to check the boot iface
          then do { hpt <- getHpt
                  ; case lookupUFM hpt (moduleName mod) of
                      Just info | mi_boot (hm_iface info) 
                                -> return (hm_details info)
                      _ -> return emptyModDetails }
          else do

        -- OK, so we're in one-shot mode.  
        -- In that case, we're read all the direct imports by now, 
        -- so eps_is_boot will record if any of our imports mention us by 
        -- way of hi-boot file
        { eps <- getEps
        ; case lookupUFM (eps_is_boot eps) (moduleName mod) of {
            Nothing -> return emptyModDetails ; -- The typical case

            Just (_, False) -> failWithTc moduleLoop ;
                -- Someone below us imported us!
                -- This is a loop with no hi-boot in the way
                
            Just (_mod, True) ->        -- There's a hi-boot interface below us
                
    do  { read_result <- findAndReadIface 
                                need mod
                                True    -- Hi-boot file

        ; case read_result of
                Failed err               -> failWithTc (elaborate err)
                Succeeded (iface, _path) -> typecheckIface iface
    }}}}
  where
    need = ptext (sLit "Need the hi-boot interface for") <+> ppr mod
                 <+> ptext (sLit "to compare against the Real Thing")

    moduleLoop = ptext (sLit "Circular imports: module") <+> quotes (ppr mod) 
                     <+> ptext (sLit "depends on itself")

    elaborate err = hang (ptext (sLit "Could not find hi-boot interface for") <+> 
                          quotes (ppr mod) <> colon) 4 err
\end{code}


%************************************************************************
%*                                                                      *
                Type and class declarations
%*                                                                      *
%************************************************************************

When typechecking a data type decl, we *lazily* (via forkM) typecheck
the constructor argument types.  This is in the hope that we may never
poke on those argument types, and hence may never need to load the
interface files for types mentioned in the arg types.

E.g.    
        data Foo.S = MkS Baz.T
Mabye we can get away without even loading the interface for Baz!

This is not just a performance thing.  Suppose we have
        data Foo.S = MkS Baz.T
        data Baz.T = MkT Foo.S
(in different interface files, of course).
Now, first we load and typecheck Foo.S, and add it to the type envt.  
If we do explore MkS's argument, we'll load and typecheck Baz.T.
If we explore MkT's argument we'll find Foo.S already in the envt.  

If we typechecked constructor args eagerly, when loading Foo.S we'd try to
typecheck the type Baz.T.  So we'd fault in Baz.T... and then need Foo.S...
which isn't done yet.

All very cunning. However, there is a rather subtle gotcha which bit
me when developing this stuff.  When we typecheck the decl for S, we
extend the type envt with S, MkS, and all its implicit Ids.  Suppose
(a bug, but it happened) that the list of implicit Ids depended in
turn on the constructor arg types.  Then the following sequence of
events takes place:
        * we build a thunk <t> for the constructor arg tys
        * we build a thunk for the extended type environment (depends on <t>)
        * we write the extended type envt into the global EPS mutvar
        
Now we look something up in the type envt
        * that pulls on <t>
        * which reads the global type envt out of the global EPS mutvar
        * but that depends in turn on <t>

It's subtle, because, it'd work fine if we typechecked the constructor args 
eagerly -- they don't need the extended type envt.  They just get the extended
type envt by accident, because they look at it later.

What this means is that the implicitTyThings MUST NOT DEPEND on any of
the forkM stuff.


\begin{code}
tcIfaceDecl :: Bool     -- True <=> discard IdInfo on IfaceId bindings
            -> IfaceDecl
            -> IfL TyThing
tcIfaceDecl = tc_iface_decl NoParentTyCon

tc_iface_decl :: TyConParent    -- For nested declarations
              -> Bool   -- True <=> discard IdInfo on IfaceId bindings
              -> IfaceDecl
              -> IfL TyThing
tc_iface_decl _ ignore_prags (IfaceId {ifName = occ_name, ifType = iface_type, 
                                       ifIdDetails = details, ifIdInfo = info})
  = do  { name <- lookupIfaceTop occ_name
        ; ty <- tcIfaceType iface_type
        ; details <- tcIdDetails ty details
        ; info <- tcIdInfo ignore_prags name ty info
        ; return (AnId (mkGlobalId details name ty info)) }

tc_iface_decl parent _ (IfaceData {ifName = occ_name, 
                          ifCType = cType, 
                          ifTyVars = tv_bndrs, 
                          ifCtxt = ctxt, ifGadtSyntax = gadt_syn,
                          ifCons = rdr_cons, 
                          ifRec = is_rec, ifPromotable = is_prom, 
                          ifAxiom = mb_axiom_name })
  = bindIfaceTyVars_AT tv_bndrs $ \ tyvars -> do
    { tc_name <- lookupIfaceTop occ_name
    ; tycon <- fixM $ \ tycon -> do
            { stupid_theta <- tcIfaceCtxt ctxt
            ; parent' <- tc_parent tyvars mb_axiom_name
            ; cons <- tcIfaceDataCons tc_name tycon tyvars rdr_cons
            ; return (buildAlgTyCon tc_name tyvars cType stupid_theta 
                                    cons is_rec is_prom gadt_syn parent') }
    ; traceIf (text "tcIfaceDecl4" <+> ppr tycon)
    ; return (ATyCon tycon) }
  where
    tc_parent :: [TyVar] -> Maybe Name -> IfL TyConParent
    tc_parent _ Nothing = return parent
    tc_parent tyvars (Just ax_name)
      = ASSERT( isNoParent parent )
        do { ax <- tcIfaceCoAxiom ax_name
           ; let fam_tc = coAxiomTyCon ax
                 ax_unbr = toUnbranchedAxiom ax
                 -- data families don't have branches:
                 branch = coAxiomSingleBranch ax_unbr
                 ax_tvs = coAxBranchTyVars branch
                 ax_lhs = coAxBranchLHS branch
                 subst = zipTopTvSubst ax_tvs (mkTyVarTys tyvars)
                            -- The subst matches the tyvar of the TyCon
                            -- with those from the CoAxiom.  They aren't
                            -- necessarily the same, since the two may be
                            -- gotten from separate interface-file declarations
           ; return (FamInstTyCon ax_unbr fam_tc (substTys subst ax_lhs)) }

tc_iface_decl parent _ (IfaceSyn {ifName = occ_name, ifTyVars = tv_bndrs, 
                                  ifSynRhs = mb_rhs_ty,
                                  ifSynKind = kind })
   = bindIfaceTyVars_AT tv_bndrs $ \ tyvars -> do
     { tc_name  <- lookupIfaceTop occ_name
     ; rhs_kind <- tcIfaceKind kind     -- Note [Synonym kind loop]
     ; rhs      <- forkM (mk_doc tc_name) $ 
                   tc_syn_rhs mb_rhs_ty
     ; tycon    <- buildSynTyCon tc_name tyvars rhs rhs_kind parent
     ; return (ATyCon tycon) }
   where
     mk_doc n = ptext (sLit "Type syonym") <+> ppr n
     tc_syn_rhs (SynFamilyTyCon a b) = return (SynFamilyTyCon a b)
     tc_syn_rhs (SynonymTyCon ty)    = do { rhs_ty <- tcIfaceType ty
                                          ; return (SynonymTyCon rhs_ty) }

tc_iface_decl _parent ignore_prags
            (IfaceClass {ifCtxt = rdr_ctxt, ifName = tc_occ,
                         ifTyVars = tv_bndrs, ifFDs = rdr_fds, 
                         ifATs = rdr_ats, ifSigs = rdr_sigs, 
                         ifRec = tc_isrec })
-- ToDo: in hs-boot files we should really treat abstract classes specially,
--       as we do abstract tycons
  = bindIfaceTyVars tv_bndrs $ \ tyvars -> do
    { tc_name <- lookupIfaceTop tc_occ
    ; traceIf (text "tc-iface-class1" <+> ppr tc_occ)
    ; ctxt <- mapM tc_sc rdr_ctxt
    ; traceIf (text "tc-iface-class2" <+> ppr tc_occ)
    ; sigs <- mapM tc_sig rdr_sigs
    ; fds  <- mapM tc_fd rdr_fds
    ; traceIf (text "tc-iface-class3" <+> ppr tc_occ)
    ; cls  <- fixM $ \ cls -> do
              { ats  <- mapM (tc_at cls) rdr_ats
              ; traceIf (text "tc-iface-class4" <+> ppr tc_occ)
              ; buildClass ignore_prags tc_name tyvars ctxt fds ats sigs tc_isrec }
    ; return (ATyCon (classTyCon cls)) }
  where
   tc_sc pred = forkM (mk_sc_doc pred) (tcIfaceType pred)
        -- The *length* of the superclasses is used by buildClass, and hence must
        -- not be inside the thunk.  But the *content* maybe recursive and hence
        -- must be lazy (via forkM).  Example:
        --     class C (T a) => D a where
        --       data T a
        -- Here the associated type T is knot-tied with the class, and
        -- so we must not pull on T too eagerly.  See Trac #5970
   mk_sc_doc pred = ptext (sLit "Superclass") <+> ppr pred

   tc_sig (IfaceClassOp occ dm rdr_ty)
     = do { op_name <- lookupIfaceTop occ
          ; op_ty   <- forkM (mk_op_doc op_name rdr_ty) (tcIfaceType rdr_ty)
                -- Must be done lazily for just the same reason as the 
                -- type of a data con; to avoid sucking in types that
                -- it mentions unless it's necessary to do so
          ; return (op_name, dm, op_ty) }

   tc_at cls (IfaceAT tc_decl defs_decls)
     = do ATyCon tc <- tc_iface_decl (AssocFamilyTyCon cls) ignore_prags tc_decl
          defs <- mapM tc_ax_branch defs_decls
          return (tc, defs)

   mk_op_doc op_name op_ty = ptext (sLit "Class op") <+> sep [ppr op_name, ppr op_ty]

   tc_fd (tvs1, tvs2) = do { tvs1' <- mapM tcIfaceTyVar tvs1
                           ; tvs2' <- mapM tcIfaceTyVar tvs2
                           ; return (tvs1', tvs2') }

tc_iface_decl _ _ (IfaceForeign {ifName = rdr_name, ifExtName = ext_name})
  = do  { name <- lookupIfaceTop rdr_name
        ; return (ATyCon (mkForeignTyCon name ext_name 
                                         liftedTypeKind 0)) }

tc_iface_decl _ _ (IfaceAxiom {ifName = ax_occ, ifTyCon = tc, ifAxBranches = branches})
  = do { tc_name     <- lookupIfaceTop ax_occ
       ; tc_tycon    <- tcIfaceTyCon tc
       ; tc_branches <- mapM tc_ax_branch branches
       ; let axiom = CoAxiom { co_ax_unique   = nameUnique tc_name
                             , co_ax_name     = tc_name
                             , co_ax_tc       = tc_tycon
                             , co_ax_branches = toBranchList tc_branches
                             , co_ax_implicit = False }
       ; return (ACoAxiom axiom) }

tc_ax_branch :: IfaceAxBranch -> IfL CoAxBranch
tc_ax_branch (IfaceAxBranch { ifaxbTyVars = tv_bndrs, ifaxbLHS = lhs, ifaxbRHS = rhs })
  = bindIfaceTyVars tv_bndrs $ \ tvs -> do  -- Variables will all be fresh
    { tc_lhs <- mapM tcIfaceType lhs
    ; tc_rhs <- tcIfaceType rhs
    ; return (CoAxBranch { cab_loc = noSrcSpan
                         , cab_tvs = tvs
                         , cab_lhs = tc_lhs
                         , cab_rhs = tc_rhs } ) }

tcIfaceDataCons :: Name -> TyCon -> [TyVar] -> IfaceConDecls -> IfL AlgTyConRhs
tcIfaceDataCons tycon_name tycon _ if_cons
  = case if_cons of
        IfAbstractTyCon dis -> return (AbstractTyCon dis)
        IfDataFamTyCon  -> return DataFamilyTyCon
        IfDataTyCon cons -> do  { data_cons <- mapM tc_con_decl cons
                                ; return (mkDataTyConRhs data_cons) }
        IfNewTyCon con   -> do  { data_con <- tc_con_decl con
                                ; mkNewTyConRhs tycon_name tycon data_con }
  where
    tc_con_decl (IfCon { ifConInfix = is_infix, 
                         ifConUnivTvs = univ_tvs, ifConExTvs = ex_tvs,
                         ifConOcc = occ, ifConCtxt = ctxt, ifConEqSpec = spec,
                         ifConArgTys = args, ifConFields = field_lbls,
                         ifConStricts = if_stricts})
     = bindIfaceTyVars univ_tvs $ \ univ_tyvars -> do
       bindIfaceTyVars ex_tvs    $ \ ex_tyvars -> do
        { name  <- lookupIfaceTop occ

        -- Read the context and argument types, but lazily for two reasons
        -- (a) to avoid looking tugging on a recursive use of 
        --     the type itself, which is knot-tied
        -- (b) to avoid faulting in the component types unless 
        --     they are really needed
        ; ~(eq_spec, theta, arg_tys) <- forkM (mk_doc name) $
             do { eq_spec <- tcIfaceEqSpec spec
                ; theta   <- tcIfaceCtxt ctxt
                ; arg_tys <- mapM tcIfaceType args
                ; return (eq_spec, theta, arg_tys) }
        ; lbl_names <- mapM lookupIfaceTop field_lbls

        ; stricts <- mapM tc_strict if_stricts

        -- Remember, tycon is the representation tycon
        ; let orig_res_ty = mkFamilyTyConApp tycon 
                                (substTyVars (mkTopTvSubst eq_spec) univ_tyvars)

        ; buildDataCon (pprPanic "tcIfaceDataCons: FamInstEnvs" (ppr name))
                       name is_infix
                       stricts lbl_names
                       univ_tyvars ex_tyvars 
                       eq_spec theta 
                       arg_tys orig_res_ty tycon
        }
    mk_doc con_name = ptext (sLit "Constructor") <+> ppr con_name

    tc_strict IfNoBang = return HsNoBang
    tc_strict IfStrict = return HsStrict
    tc_strict IfUnpack = return (HsUnpack Nothing)
    tc_strict (IfUnpackCo if_co) = do { co <- tcIfaceCo if_co
                                      ; return (HsUnpack (Just co)) }

tcIfaceEqSpec :: [(OccName, IfaceType)] -> IfL [(TyVar, Type)]
tcIfaceEqSpec spec
  = mapM do_item spec
  where
    do_item (occ, if_ty) = do { tv <- tcIfaceTyVar (occNameFS occ)
                              ; ty <- tcIfaceType if_ty
                              ; return (tv,ty) }
\end{code}

Note [Synonym kind loop]
~~~~~~~~~~~~~~~~~~~~~~~~
Notice that we eagerly grab the *kind* from the interface file, but
build a forkM thunk for the *rhs* (and family stuff).  To see why, 
consider this (Trac #2412)

M.hs:       module M where { import X; data T = MkT S }
X.hs:       module X where { import {-# SOURCE #-} M; type S = T }
M.hs-boot:  module M where { data T }

When kind-checking M.hs we need S's kind.  But we do not want to
find S's kind from (typeKind S-rhs), because we don't want to look at
S-rhs yet!  Since S is imported from X.hi, S gets just one chance to
be defined, and we must not do that until we've finished with M.T.

Solution: record S's kind in the interface file; now we can safely
look at it.

%************************************************************************
%*                                                                      *
                Instances
%*                                                                      *
%************************************************************************

\begin{code}
tcIfaceInst :: IfaceClsInst -> IfL ClsInst
tcIfaceInst (IfaceClsInst { ifDFun = dfun_occ, ifOFlag = oflag
                          , ifInstCls = cls, ifInstTys = mb_tcs })
  = do { dfun <- forkM (ptext (sLit "Dict fun") <+> ppr dfun_occ) $
                 tcIfaceExtId dfun_occ
       ; let mb_tcs' = map (fmap ifaceTyConName) mb_tcs
       ; return (mkImportedInstance cls mb_tcs' dfun oflag) }

tcIfaceFamInst :: IfaceFamInst -> IfL (FamInst Branched)
tcIfaceFamInst (IfaceFamInst { ifFamInstFam = fam, ifFamInstTys = mb_tcss
                             , ifFamInstBranched = branched, ifFamInstAxiom = axiom_name } )
    = do { axiom' <- forkM (ptext (sLit "Axiom") <+> ppr axiom_name) $
                     tcIfaceCoAxiom axiom_name
         ; let mb_tcss' = map (map (fmap ifaceTyConName)) mb_tcss
         ; return (mkImportedFamInst fam branched mb_tcss' axiom') }
\end{code}


%************************************************************************
%*                                                                      *
                Rules
%*                                                                      *
%************************************************************************

We move a IfaceRule from eps_rules to eps_rule_base when all its LHS free vars
are in the type environment.  However, remember that typechecking a Rule may 
(as a side effect) augment the type envt, and so we may need to iterate the process.

\begin{code}
tcIfaceRules :: Bool            -- True <=> ignore rules
             -> [IfaceRule]
             -> IfL [CoreRule]
tcIfaceRules ignore_prags if_rules
  | ignore_prags = return []
  | otherwise    = mapM tcIfaceRule if_rules

tcIfaceRule :: IfaceRule -> IfL CoreRule
tcIfaceRule (IfaceRule {ifRuleName = name, ifActivation = act, ifRuleBndrs = bndrs,
                        ifRuleHead = fn, ifRuleArgs = args, ifRuleRhs = rhs,
                        ifRuleAuto = auto })
  = do  { ~(bndrs', args', rhs') <- 
                -- Typecheck the payload lazily, in the hope it'll never be looked at
                forkM (ptext (sLit "Rule") <+> ftext name) $
                bindIfaceBndrs bndrs                      $ \ bndrs' ->
                do { args' <- mapM tcIfaceExpr args
                   ; rhs'  <- tcIfaceExpr rhs
                   ; return (bndrs', args', rhs') }
        ; let mb_tcs = map ifTopFreeName args
        ; return (Rule { ru_name = name, ru_fn = fn, ru_act = act, 
                          ru_bndrs = bndrs', ru_args = args', 
                          ru_rhs = occurAnalyseExpr rhs', 
                          ru_rough = mb_tcs,
                          ru_auto = auto,
                          ru_local = False }) } -- An imported RULE is never for a local Id
                                                -- or, even if it is (module loop, perhaps)
                                                -- we'll just leave it in the non-local set
  where
        -- This function *must* mirror exactly what Rules.topFreeName does
        -- We could have stored the ru_rough field in the iface file
        -- but that would be redundant, I think.
        -- The only wrinkle is that we must not be deceived by
        -- type syononyms at the top of a type arg.  Since
        -- we can't tell at this point, we are careful not
        -- to write them out in coreRuleToIfaceRule
    ifTopFreeName :: IfaceExpr -> Maybe Name
    ifTopFreeName (IfaceType (IfaceTyConApp tc _ )) = Just (ifaceTyConName tc)
    ifTopFreeName (IfaceApp f _)                    = ifTopFreeName f
    ifTopFreeName (IfaceExt n)                      = Just n
    ifTopFreeName _                                 = Nothing
\end{code}


%************************************************************************
%*                                                                      *
                Annotations
%*                                                                      *
%************************************************************************

\begin{code}
tcIfaceAnnotations :: [IfaceAnnotation] -> IfL [Annotation]
tcIfaceAnnotations = mapM tcIfaceAnnotation

tcIfaceAnnotation :: IfaceAnnotation -> IfL Annotation
tcIfaceAnnotation (IfaceAnnotation target serialized) = do
    target' <- tcIfaceAnnTarget target
    return $ Annotation {
        ann_target = target',
        ann_value = serialized
    }

tcIfaceAnnTarget :: IfaceAnnTarget -> IfL (AnnTarget Name)
tcIfaceAnnTarget (NamedTarget occ) = do
    name <- lookupIfaceTop occ
    return $ NamedTarget name
tcIfaceAnnTarget (ModuleTarget mod) = do
    return $ ModuleTarget mod

\end{code}


%************************************************************************
%*                                                                      *
                Vectorisation information
%*                                                                      *
%************************************************************************

\begin{code}
-- We need access to the type environment as we need to look up information about type constructors
-- (i.e., their data constructors and whether they are class type constructors).  If a vectorised
-- type constructor or class is defined in the same module as where it is vectorised, we cannot
-- look that information up from the type constructor that we obtained via a 'forkM'ed
-- 'tcIfaceTyCon' without recursively loading the interface that we are already type checking again
-- and again and again...
--
tcIfaceVectInfo :: Module -> TypeEnv -> IfaceVectInfo -> IfL VectInfo
tcIfaceVectInfo mod typeEnv (IfaceVectInfo 
                             { ifaceVectInfoVar            = vars
                             , ifaceVectInfoTyCon          = tycons
                             , ifaceVectInfoTyConReuse     = tyconsReuse
                             , ifaceVectInfoParallelVars   = parallelVars
                             , ifaceVectInfoParallelTyCons = parallelTyCons
                             })
  = do { let parallelTyConsSet = mkNameSet parallelTyCons
       ; vVars         <- mapM vectVarMapping                  vars
       ; let varsSet = mkVarSet (map fst vVars)
       ; tyConRes1     <- mapM (vectTyConVectMapping varsSet)  tycons
       ; tyConRes2     <- mapM (vectTyConReuseMapping varsSet) tyconsReuse
       ; vParallelVars <- mapM vectVar                         parallelVars
       ; let (vTyCons, vDataCons, vScSels) = unzip3 (tyConRes1 ++ tyConRes2)
       ; return $ VectInfo 
                  { vectInfoVar            = mkVarEnv  vVars `extendVarEnvList` concat vScSels
                  , vectInfoTyCon          = mkNameEnv vTyCons
                  , vectInfoDataCon        = mkNameEnv (concat vDataCons)
                  , vectInfoParallelVars   = mkVarSet  vParallelVars
                  , vectInfoParallelTyCons = parallelTyConsSet
                  }
       }
  where
    vectVarMapping name 
      = do { vName <- lookupOrig mod (mkLocalisedOccName mod mkVectOcc name)
           ; var   <- forkM (ptext (sLit "vect var")  <+> ppr name)  $
                        tcIfaceExtId name
           ; vVar  <- forkM (ptext (sLit "vect vVar [mod =") <+> 
                             ppr mod <> ptext (sLit "; nameModule =") <+> 
                             ppr (nameModule name) <> ptext (sLit "]") <+> ppr vName) $
                       tcIfaceExtId vName
           ; return (var, (var, vVar))
           }
      -- where
      --   lookupLocalOrExternalId name
      --     = do { let mb_id = lookupTypeEnv typeEnv name
      --          ; case mb_id of
      --                -- id is local
      --              Just (AnId id) -> return id
      --                -- name is not an Id => internal inconsistency
      --              Just _         -> notAnIdErr
      --                -- Id is external
      --              Nothing        -> tcIfaceExtId name
      --          }
      -- 
      --   notAnIdErr = pprPanic "TcIface.tcIfaceVectInfo: not an id" (ppr name)

    vectVar name 
      = forkM (ptext (sLit "vect scalar var")  <+> ppr name)  $
          tcIfaceExtId name

    vectTyConVectMapping vars name
      = do { vName  <- lookupOrig mod (mkLocalisedOccName mod mkVectTyConOcc name)
           ; vectTyConMapping vars name vName
           }

    vectTyConReuseMapping vars name
      = vectTyConMapping vars name name

    vectTyConMapping vars name vName
      = do { tycon  <- lookupLocalOrExternalTyCon name
           ; vTycon <- forkM (ptext (sLit "vTycon of") <+> ppr vName) $ 
                         lookupLocalOrExternalTyCon vName

               -- Map the data constructors of the original type constructor to those of the
               -- vectorised type constructor /unless/ the type constructor was vectorised
               -- abstractly; if it was vectorised abstractly, the workers of its data constructors
               -- do not appear in the set of vectorised variables.
               --
               -- NB: This is lazy!  We don't pull at the type constructors before we actually use
               --     the data constructor mapping.
           ; let isAbstract | isClassTyCon tycon = False
                            | datacon:_ <- tyConDataCons tycon 
                                                 = not $ dataConWrapId datacon `elemVarSet` vars
                            | otherwise          = True
                 vDataCons  | isAbstract = []
                            | otherwise  = [ (dataConName datacon, (datacon, vDatacon))
                                           | (datacon, vDatacon) <- zip (tyConDataCons tycon)
                                                                        (tyConDataCons vTycon)
                                           ]

                   -- Map the (implicit) superclass and methods selectors as they don't occur in
                   -- the var map.
                 vScSels    | Just cls  <- tyConClass_maybe tycon
                            , Just vCls <- tyConClass_maybe vTycon 
                            = [ (sel, (sel, vSel))
                              | (sel, vSel) <- zip (classAllSelIds cls) (classAllSelIds vCls)
                              ]
                            | otherwise
                            = []

           ; return ( (name, (tycon, vTycon))          -- (T, T_v)
                    , vDataCons                        -- list of (Ci, Ci_v)
                    , vScSels                          -- list of (seli, seli_v)
                    )
           }
      where
          -- we need a fully defined version of the type constructor to be able to extract
          -- its data constructors etc.
        lookupLocalOrExternalTyCon name
          = do { let mb_tycon = lookupTypeEnv typeEnv name
               ; case mb_tycon of
                     -- tycon is local
                   Just (ATyCon tycon) -> return tycon
                     -- name is not a tycon => internal inconsistency
                   Just _              -> notATyConErr
                     -- tycon is external
                   Nothing             -> tcIfaceTyCon (IfaceTc name)
               }

        notATyConErr = pprPanic "TcIface.tcIfaceVectInfo: not a tycon" (ppr name)
\end{code}

%************************************************************************
%*                                                                      *
                        Types
%*                                                                      *
%************************************************************************

\begin{code}
tcIfaceType :: IfaceType -> IfL Type
tcIfaceType (IfaceTyVar n)         = do { tv <- tcIfaceTyVar n; return (TyVarTy tv) }
tcIfaceType (IfaceAppTy t1 t2)     = do { t1' <- tcIfaceType t1; t2' <- tcIfaceType t2; return (AppTy t1' t2') }
tcIfaceType (IfaceLitTy l)         = do { l1 <- tcIfaceTyLit l; return (LitTy l1) }
tcIfaceType (IfaceFunTy t1 t2)     = do { t1' <- tcIfaceType t1; t2' <- tcIfaceType t2; return (FunTy t1' t2') }
tcIfaceType (IfaceTyConApp tc tks) = do { tc' <- tcIfaceTyCon tc
                                        ; tks' <- tcIfaceTcArgs (tyConKind tc') tks 
                                        ; return (mkTyConApp tc' tks') }
tcIfaceType (IfaceForAllTy tv t)  = bindIfaceTyVar tv $ \ tv' -> do { t' <- tcIfaceType t; return (ForAllTy tv' t') }
tcIfaceType t@(IfaceCoConApp {})  = pprPanic "tcIfaceType" (ppr t)

tcIfaceTypes :: [IfaceType] -> IfL [Type]
tcIfaceTypes tys = mapM tcIfaceType tys

tcIfaceTcArgs :: Kind -> [IfaceType] -> IfL [Type]
tcIfaceTcArgs _ [] 
  = return []
tcIfaceTcArgs kind (tk:tks)
  = case splitForAllTy_maybe kind of
      Nothing         -> tcIfaceTypes (tk:tks)
      Just (_, kind') -> do { k'   <- tcIfaceKind tk
                            ; tks' <- tcIfaceTcArgs kind' tks
                            ; return (k':tks') }
  
-----------------------------------------
tcIfaceCtxt :: IfaceContext -> IfL ThetaType
tcIfaceCtxt sts = mapM tcIfaceType sts

-----------------------------------------
tcIfaceTyLit :: IfaceTyLit -> IfL TyLit
tcIfaceTyLit (IfaceNumTyLit n) = return (NumTyLit n)
tcIfaceTyLit (IfaceStrTyLit n) = return (StrTyLit n)

-----------------------------------------
tcIfaceKind :: IfaceKind -> IfL Kind   -- See Note [Checking IfaceTypes vs IfaceKinds]
tcIfaceKind (IfaceTyVar n)        = do { tv <- tcIfaceTyVar n; return (TyVarTy tv) }
tcIfaceKind (IfaceAppTy t1 t2)    = do { t1' <- tcIfaceKind t1; t2' <- tcIfaceKind t2; return (AppTy t1' t2') }
tcIfaceKind (IfaceFunTy t1 t2)    = do { t1' <- tcIfaceKind t1; t2' <- tcIfaceKind t2; return (FunTy t1' t2') }
tcIfaceKind (IfaceTyConApp tc ts) = do { tc' <- tcIfaceKindCon tc; ts' <- tcIfaceKinds ts; return (mkTyConApp tc' ts') }
tcIfaceKind (IfaceForAllTy tv t)  = bindIfaceTyVar tv $ \ tv' -> do { t' <- tcIfaceKind t; return (ForAllTy tv' t') }
tcIfaceKind t                     = pprPanic "tcIfaceKind" (ppr t)  -- IfaceCoApp, IfaceLitTy

tcIfaceKinds :: [IfaceKind] -> IfL [Kind]
tcIfaceKinds tys = mapM tcIfaceKind tys
\end{code}

Note [Checking IfaceTypes vs IfaceKinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to know whether we are checking a *type* or a *kind*.
Consider   module M where
             Proxy :: forall k. k -> *
             data T = T
and consider the two IfaceTypes
      M.Proxy * M.T{tc}
      M.Proxy 'M.T{tc} 'M.T(d}
The first is conventional, but in the latter we use the promoted
type constructor (as a kind) and data constructor (as a type).  However, 
the Name of the promoted type constructor is just M.T; it's the *same name*
as the ordinary type constructor.  

We could add a "promoted" flag to an IfaceTyCon, but that's a bit heavy.
Instead we use context to distinguish, as in the source language.  
  - When checking a kind, we look up M.T{tc} and promote it
  - When checking a type, we look up M.T{tc} and don't promote it
                                 and M.T{d}  and promote it
    See tcIfaceKindCon and tcIfaceKTyCon respectively

This context business is why we need tcIfaceTcArgs.


%************************************************************************
%*                                                                      *
                        Coercions
%*                                                                      *
%************************************************************************

\begin{code}
tcIfaceCo :: IfaceType -> IfL Coercion
tcIfaceCo (IfaceTyVar n)        = mkCoVarCo <$> tcIfaceCoVar n
tcIfaceCo (IfaceAppTy t1 t2)    = mkAppCo <$> tcIfaceCo t1 <*> tcIfaceCo t2
tcIfaceCo (IfaceFunTy t1 t2)    = mkFunCo <$> tcIfaceCo t1 <*> tcIfaceCo t2
tcIfaceCo (IfaceTyConApp tc ts) = mkTyConAppCo <$> tcIfaceTyCon tc <*> mapM tcIfaceCo ts
tcIfaceCo t@(IfaceLitTy _)      = mkReflCo <$> tcIfaceType t
tcIfaceCo (IfaceCoConApp tc ts) = tcIfaceCoApp tc ts
tcIfaceCo (IfaceForAllTy tv t)  = bindIfaceTyVar tv $ \ tv' ->
                                  mkForAllCo tv' <$> tcIfaceCo t

tcIfaceCoApp :: IfaceCoCon -> [IfaceType] -> IfL Coercion
tcIfaceCoApp IfaceReflCo      [t]     = Refl         <$> tcIfaceType t
tcIfaceCoApp (IfaceCoAx n i)  ts      = AxiomInstCo  <$> tcIfaceCoAxiom n
                                                     <*> pure i
                                                     <*> mapM tcIfaceCo ts
tcIfaceCoApp IfaceUnsafeCo    [t1,t2] = UnsafeCo     <$> tcIfaceType t1 <*> tcIfaceType t2
tcIfaceCoApp IfaceSymCo       [t]     = SymCo        <$> tcIfaceCo t
tcIfaceCoApp IfaceTransCo     [t1,t2] = TransCo      <$> tcIfaceCo t1 <*> tcIfaceCo t2
tcIfaceCoApp IfaceInstCo      [t1,t2] = InstCo       <$> tcIfaceCo t1 <*> tcIfaceType t2
tcIfaceCoApp (IfaceNthCo d)   [t]     = NthCo d      <$> tcIfaceCo t
tcIfaceCoApp (IfaceLRCo lr)   [t]     = LRCo lr      <$> tcIfaceCo t
tcIfaceCoApp cc ts = pprPanic "toIfaceCoApp" (ppr cc <+> ppr ts)

tcIfaceCoVar :: FastString -> IfL CoVar
tcIfaceCoVar = tcIfaceLclId
\end{code}


%************************************************************************
%*                                                                      *
                        Core
%*                                                                      *
%************************************************************************

\begin{code}
tcIfaceExpr :: IfaceExpr -> IfL CoreExpr
tcIfaceExpr (IfaceType ty)
  = Type <$> tcIfaceType ty

tcIfaceExpr (IfaceCo co)
  = Coercion <$> tcIfaceCo co

tcIfaceExpr (IfaceCast expr co)
  = Cast <$> tcIfaceExpr expr <*> tcIfaceCo co

tcIfaceExpr (IfaceLcl name)
  = Var <$> tcIfaceLclId name

tcIfaceExpr (IfaceExt gbl)
  = Var <$> tcIfaceExtId gbl

tcIfaceExpr (IfaceLit lit)
  = do lit' <- tcIfaceLit lit
       return (Lit lit')
 
tcIfaceExpr (IfaceFCall cc ty) = do
    ty' <- tcIfaceType ty
    u <- newUnique
    dflags <- getDynFlags
    return (Var (mkFCallId dflags u cc ty'))

tcIfaceExpr (IfaceTuple boxity args)  = do
    args' <- mapM tcIfaceExpr args
    -- Put the missing type arguments back in
    let con_args = map (Type . exprType) args' ++ args'
    return (mkApps (Var con_id) con_args)
  where
    arity = length args
    con_id = dataConWorkId (tupleCon boxity arity)
    

tcIfaceExpr (IfaceLam bndr body)
  = bindIfaceBndr bndr $ \bndr' ->
    Lam bndr' <$> tcIfaceExpr body

tcIfaceExpr (IfaceApp fun arg)
  = App <$> tcIfaceExpr fun <*> tcIfaceExpr arg

tcIfaceExpr (IfaceECase scrut ty) 
  = do { scrut' <- tcIfaceExpr scrut 
       ; ty' <- tcIfaceType ty
       ; return (castBottomExpr scrut' ty') }

tcIfaceExpr (IfaceCase scrut case_bndr alts)  = do
    scrut' <- tcIfaceExpr scrut
    case_bndr_name <- newIfaceName (mkVarOccFS case_bndr)
    let
        scrut_ty   = exprType scrut'
        case_bndr' = mkLocalId case_bndr_name scrut_ty
        tc_app     = splitTyConApp scrut_ty
                -- NB: Won't always succeed (polymorphic case)
                --     but won't be demanded in those cases
                -- NB: not tcSplitTyConApp; we are looking at Core here
                --     look through non-rec newtypes to find the tycon that
                --     corresponds to the datacon in this case alternative

    extendIfaceIdEnv [case_bndr'] $ do
     alts' <- mapM (tcIfaceAlt scrut' tc_app) alts
     return (Case scrut' case_bndr' (coreAltsType alts') alts')

tcIfaceExpr (IfaceLet (IfaceNonRec (IfLetBndr fs ty info) rhs) body)
  = do  { name    <- newIfaceName (mkVarOccFS fs)
        ; ty'     <- tcIfaceType ty
        ; id_info <- tcIdInfo False {- Don't ignore prags; we are inside one! -}
                              name ty' info
        ; let id = mkLocalIdWithInfo name ty' id_info
        ; rhs' <- tcIfaceExpr rhs
        ; body' <- extendIfaceIdEnv [id] (tcIfaceExpr body)
        ; return (Let (NonRec id rhs') body') }

tcIfaceExpr (IfaceLet (IfaceRec pairs) body)
  = do { ids <- mapM tc_rec_bndr (map fst pairs)
       ; extendIfaceIdEnv ids $ do
       { pairs' <- zipWithM tc_pair pairs ids
       ; body' <- tcIfaceExpr body
       ; return (Let (Rec pairs') body') } }
 where
   tc_rec_bndr (IfLetBndr fs ty _) 
     = do { name <- newIfaceName (mkVarOccFS fs)  
          ; ty'  <- tcIfaceType ty
          ; return (mkLocalId name ty') }
   tc_pair (IfLetBndr _ _ info, rhs) id
     = do { rhs' <- tcIfaceExpr rhs
          ; id_info <- tcIdInfo False {- Don't ignore prags; we are inside one! -}
                                (idName id) (idType id) info
          ; return (setIdInfo id id_info, rhs') }

tcIfaceExpr (IfaceTick tickish expr) = do
    expr' <- tcIfaceExpr expr
    tickish' <- tcIfaceTickish tickish
    return (Tick tickish' expr')

-------------------------
tcIfaceTickish :: IfaceTickish -> IfM lcl (Tickish Id)
tcIfaceTickish (IfaceHpcTick modl ix)   = return (HpcTick modl ix)
tcIfaceTickish (IfaceSCC  cc tick push) = return (ProfNote cc tick push)

-------------------------
tcIfaceLit :: Literal -> IfL Literal
-- Integer literals deserialise to (LitInteger i <error thunk>) 
-- so tcIfaceLit just fills in the type.
-- See Note [Integer literals] in Literal
tcIfaceLit (LitInteger i _)
  = do t <- tcIfaceTyCon (IfaceTc integerTyConName)
       return (mkLitInteger i (mkTyConTy t))
tcIfaceLit lit = return lit

-------------------------
tcIfaceAlt :: CoreExpr -> (TyCon, [Type])
           -> (IfaceConAlt, [FastString], IfaceExpr)
           -> IfL (AltCon, [TyVar], CoreExpr)
tcIfaceAlt _ _ (IfaceDefault, names, rhs)
  = ASSERT( null names ) do
    rhs' <- tcIfaceExpr rhs
    return (DEFAULT, [], rhs')
  
tcIfaceAlt _ _ (IfaceLitAlt lit, names, rhs)
  = ASSERT( null names ) do
    lit' <- tcIfaceLit lit
    rhs' <- tcIfaceExpr rhs
    return (LitAlt lit', [], rhs')

-- A case alternative is made quite a bit more complicated
-- by the fact that we omit type annotations because we can
-- work them out.  True enough, but its not that easy!
tcIfaceAlt scrut (tycon, inst_tys) (IfaceDataAlt data_occ, arg_strs, rhs)
  = do  { con <- tcIfaceDataCon data_occ
        ; when (debugIsOn && not (con `elem` tyConDataCons tycon))
               (failIfM (ppr scrut $$ ppr con $$ ppr tycon $$ ppr (tyConDataCons tycon)))
        ; tcIfaceDataAlt con inst_tys arg_strs rhs }

tcIfaceDataAlt :: DataCon -> [Type] -> [FastString] -> IfaceExpr
               -> IfL (AltCon, [TyVar], CoreExpr)
tcIfaceDataAlt con inst_tys arg_strs rhs
  = do  { us <- newUniqueSupply
        ; let uniqs = uniqsFromSupply us
        ; let (ex_tvs, arg_ids)
                      = dataConRepFSInstPat arg_strs uniqs con inst_tys

        ; rhs' <- extendIfaceTyVarEnv ex_tvs    $
                  extendIfaceIdEnv arg_ids      $
                  tcIfaceExpr rhs
        ; return (DataAlt con, ex_tvs ++ arg_ids, rhs') }
\end{code}


\begin{code}
tcExtCoreBindings :: [IfaceBinding] -> IfL CoreProgram  -- Used for external core
tcExtCoreBindings []     = return []
tcExtCoreBindings (b:bs) = do_one b (tcExtCoreBindings bs)

do_one :: IfaceBinding -> IfL [CoreBind] -> IfL [CoreBind]
do_one (IfaceNonRec bndr rhs) thing_inside
  = do  { rhs' <- tcIfaceExpr rhs
        ; bndr' <- newExtCoreBndr bndr
        ; extendIfaceIdEnv [bndr'] $ do 
        { core_binds <- thing_inside
        ; return (NonRec bndr' rhs' : core_binds) }}

do_one (IfaceRec pairs) thing_inside
  = do  { bndrs' <- mapM newExtCoreBndr bndrs
        ; extendIfaceIdEnv bndrs' $ do
        { rhss' <- mapM tcIfaceExpr rhss
        ; core_binds <- thing_inside
        ; return (Rec (bndrs' `zip` rhss') : core_binds) }}
  where
    (bndrs,rhss) = unzip pairs
\end{code}


%************************************************************************
%*                                                                      *
                IdInfo
%*                                                                      *
%************************************************************************

\begin{code}
tcIdDetails :: Type -> IfaceIdDetails -> IfL IdDetails
tcIdDetails _  IfVanillaId = return VanillaId
tcIdDetails ty (IfDFunId ns)
  = return (DFunId ns (isNewTyCon (classTyCon cls)))
  where
    (_, _, cls, _) = tcSplitDFunTy ty

tcIdDetails _ (IfRecSelId tc naughty)
  = do { tc' <- tcIfaceTyCon tc
       ; return (RecSelId { sel_tycon = tc', sel_naughty = naughty }) }

tcIdInfo :: Bool -> Name -> Type -> IfaceIdInfo -> IfL IdInfo
tcIdInfo ignore_prags name ty info 
  | ignore_prags = return vanillaIdInfo
  | otherwise    = case info of
                        NoInfo       -> return vanillaIdInfo
                        HasInfo info -> foldlM tcPrag init_info info
  where
    -- Set the CgInfo to something sensible but uninformative before
    -- we start; default assumption is that it has CAFs
    init_info = vanillaIdInfo

    tcPrag :: IdInfo -> IfaceInfoItem -> IfL IdInfo
    tcPrag info HsNoCafRefs        = return (info `setCafInfo`   NoCafRefs)
    tcPrag info (HsArity arity)    = return (info `setArityInfo` arity)
    tcPrag info (HsStrictness str) = return (info `setStrictnessInfo` str)
    tcPrag info (HsInline prag)    = return (info `setInlinePragInfo` prag)

        -- The next two are lazy, so they don't transitively suck stuff in
    tcPrag info (HsUnfold lb if_unf) 
      = do { unf <- tcUnfolding name ty info if_unf
           ; let info1 | lb        = info `setOccInfo` strongLoopBreaker
                       | otherwise = info
           ; return (info1 `setUnfoldingInfoLazily` unf) }
\end{code}

\begin{code}
tcUnfolding :: Name -> Type -> IdInfo -> IfaceUnfolding -> IfL Unfolding
tcUnfolding name _ info (IfCoreUnfold stable if_expr)
  = do  { dflags <- getDynFlags
        ; mb_expr <- tcPragExpr name if_expr
        ; let unf_src = if stable then InlineStable else InlineRhs
        ; return (case mb_expr of
                    Nothing   -> NoUnfolding
                    Just expr -> mkUnfolding dflags unf_src
                                             True {- Top level -} 
                                             is_bottoming
                                             expr) }
  where
     -- Strictness should occur before unfolding!
    is_bottoming = isBottomingSig $ strictnessInfo info              

tcUnfolding name _ _ (IfCompulsory if_expr)
  = do  { mb_expr <- tcPragExpr name if_expr
        ; return (case mb_expr of
                    Nothing   -> NoUnfolding
                    Just expr -> mkCompulsoryUnfolding expr) }

tcUnfolding name _ _ (IfInlineRule arity unsat_ok boring_ok if_expr)
  = do  { mb_expr <- tcPragExpr name if_expr
        ; return (case mb_expr of
                    Nothing   -> NoUnfolding
                    Just expr -> mkCoreUnfolding InlineStable True expr arity 
                                                 (UnfWhen unsat_ok boring_ok))
    }

tcUnfolding name dfun_ty _ (IfDFunUnfold ops)
  = do { mb_ops1 <- forkM_maybe doc $ mapM tc_arg ops
       ; return (case mb_ops1 of
                    Nothing   -> noUnfolding
                    Just ops1 -> mkDFunUnfolding dfun_ty ops1) }
  where
    doc = text "Class ops for dfun" <+> ppr name
    tc_arg (DFunPolyArg  e) = do { e' <- tcIfaceExpr e; return (DFunPolyArg e') }
    tc_arg (DFunLamArg i)   = return (DFunLamArg i)

tcUnfolding name ty info (IfExtWrapper arity wkr)
  = tcIfaceWrapper name ty info arity (tcIfaceExtId wkr)
tcUnfolding name ty info (IfLclWrapper arity wkr)
  = tcIfaceWrapper name ty info arity (tcIfaceLclId wkr)

-------------
tcIfaceWrapper :: Name -> Type -> IdInfo -> Arity -> IfL Id -> IfL Unfolding
tcIfaceWrapper name ty info arity get_worker
  = do  { mb_wkr_id <- forkM_maybe doc get_worker
        ; us <- newUniqueSupply
        ; dflags <- getDynFlags
        ; return (case mb_wkr_id of
                     Nothing     -> noUnfolding
                     Just wkr_id -> make_inline_rule dflags wkr_id us) }
  where
    doc = text "Worker for" <+> ppr name

    make_inline_rule dflags wkr_id us 
        = mkWwInlineRule wkr_id
                         (initUs_ us (mkWrapper dflags ty strict_sig) wkr_id) 
                         arity
        -- Again we rely here on strictness info 
        -- always appearing before unfolding
    strict_sig = strictnessInfo info
\end{code}

For unfoldings we try to do the job lazily, so that we never type check
an unfolding that isn't going to be looked at.

\begin{code}
tcPragExpr :: Name -> IfaceExpr -> IfL (Maybe CoreExpr)
tcPragExpr name expr
  = forkM_maybe doc $ do
    core_expr' <- tcIfaceExpr expr

                -- Check for type consistency in the unfolding
    whenGOptM Opt_DoCoreLinting $ do
        in_scope <- get_in_scope
        case lintUnfolding noSrcLoc in_scope core_expr' of
          Nothing       -> return ()
          Just fail_msg -> do { mod <- getIfModule 
                              ; pprPanic "Iface Lint failure" 
                                  (vcat [ ptext (sLit "In interface for") <+> ppr mod
                                        , hang doc 2 fail_msg
                                        , ppr name <+> equals <+> ppr core_expr'
                                        , ptext (sLit "Iface expr =") <+> ppr expr ]) }
    return core_expr'
  where
    doc = text "Unfolding of" <+> ppr name

    get_in_scope :: IfL [Var] -- Totally disgusting; but just for linting
    get_in_scope        
        = do { (gbl_env, lcl_env) <- getEnvs
             ; rec_ids <- case if_rec_types gbl_env of
                            Nothing -> return []
                            Just (_, get_env) -> do
                               { type_env <- setLclEnv () get_env
                               ; return (typeEnvIds type_env) }
             ; return (varEnvElts (if_tv_env lcl_env) ++
                       varEnvElts (if_id_env lcl_env) ++
                       rec_ids) }
\end{code}



%************************************************************************
%*                                                                      *
                Getting from Names to TyThings
%*                                                                      *
%************************************************************************

\begin{code}
tcIfaceGlobal :: Name -> IfL TyThing
tcIfaceGlobal name
  | Just thing <- wiredInNameTyThing_maybe name
        -- Wired-in things include TyCons, DataCons, and Ids
        -- Even though we are in an interface file, we want to make
        -- sure the instances and RULES of this thing (particularly TyCon) are loaded 
        -- Imagine: f :: Double -> Double
  = do { ifCheckWiredInThing thing; return thing }
  | otherwise
  = do  { env <- getGblEnv
        ; case if_rec_types env of {    -- Note [Tying the knot]
            Just (mod, get_type_env) 
                | nameIsLocalOrFrom mod name
                -> do           -- It's defined in the module being compiled
                { type_env <- setLclEnv () get_type_env         -- yuk
                ; case lookupNameEnv type_env name of
                        Just thing -> return thing
                        Nothing   -> pprPanic "tcIfaceGlobal (local): not found:"  
                                                (ppr name $$ ppr type_env) }

          ; _ -> do

        { hsc_env <- getTopEnv
        ; mb_thing <- liftIO (lookupTypeHscEnv hsc_env name)
        ; case mb_thing of {
            Just thing -> return thing ;
            Nothing    -> do

        { mb_thing <- importDecl name   -- It's imported; go get it
        ; case mb_thing of
            Failed err      -> failIfM err
            Succeeded thing -> return thing
    }}}}}

-- Note [Tying the knot]
-- ~~~~~~~~~~~~~~~~~~~~~
-- The if_rec_types field is used in two situations:
--
-- a) Compiling M.hs, which indiretly imports Foo.hi, which mentions M.T
--    Then we look up M.T in M's type environment, which is splatted into if_rec_types
--    after we've built M's type envt.
--
-- b) In ghc --make, during the upsweep, we encounter M.hs, whose interface M.hi
--    is up to date.  So we call typecheckIface on M.hi.  This splats M.T into 
--    if_rec_types so that the (lazily typechecked) decls see all the other decls
--
-- In case (b) it's important to do the if_rec_types check *before* looking in the HPT
-- Because if M.hs also has M.hs-boot, M.T will *already be* in the HPT, but in its
-- emasculated form (e.g. lacking data constructors).

tcIfaceTyCon :: IfaceTyCon -> IfL TyCon
tcIfaceTyCon (IfaceTc name) 
  = do { thing <- tcIfaceGlobal name
       ; case thing of    -- A "type constructor" can be a promoted data constructor
                          --           c.f. Trac #5881
           ATyCon   tc -> return tc
           ADataCon dc -> return (promoteDataCon dc)
           _ -> pprPanic "tcIfaceTyCon" (ppr name $$ ppr thing) }

tcIfaceKindCon :: IfaceTyCon -> IfL TyCon
tcIfaceKindCon (IfaceTc name) 
  = do { thing <- tcIfaceGlobal name
       ; case thing of    -- A "type constructor" here is a promoted type constructor
                          --           c.f. Trac #5881
           ATyCon tc 
             | isSuperKind (tyConKind tc) 
             -> return tc   -- Mainly just '*' or 'AnyK'
             | Just prom_tc <- promotableTyCon_maybe tc
             -> return prom_tc

           _ -> pprPanic "tcIfaceKindCon" (ppr name $$ ppr thing) }

tcIfaceCoAxiom :: Name -> IfL (CoAxiom Branched)
tcIfaceCoAxiom name = do { thing <- tcIfaceGlobal name
                         ; return (tyThingCoAxiom thing) }

tcIfaceDataCon :: Name -> IfL DataCon
tcIfaceDataCon name = do { thing <- tcIfaceGlobal name
                         ; case thing of
                                ADataCon dc -> return dc
                                _       -> pprPanic "tcIfaceExtDC" (ppr name$$ ppr thing) }

tcIfaceExtId :: Name -> IfL Id
tcIfaceExtId name = do { thing <- tcIfaceGlobal name
                       ; case thing of
                          AnId id -> return id
                          _       -> pprPanic "tcIfaceExtId" (ppr name$$ ppr thing) }
\end{code}

%************************************************************************
%*                                                                      *
                Bindings
%*                                                                      *
%************************************************************************

\begin{code}
bindIfaceBndr :: IfaceBndr -> (CoreBndr -> IfL a) -> IfL a
bindIfaceBndr (IfaceIdBndr (fs, ty)) thing_inside
  = do  { name <- newIfaceName (mkVarOccFS fs)
        ; ty' <- tcIfaceType ty
        ; let id = mkLocalId name ty'
        ; extendIfaceIdEnv [id] (thing_inside id) }
bindIfaceBndr (IfaceTvBndr bndr) thing_inside
  = bindIfaceTyVar bndr thing_inside
    
bindIfaceBndrs :: [IfaceBndr] -> ([CoreBndr] -> IfL a) -> IfL a
bindIfaceBndrs []     thing_inside = thing_inside []
bindIfaceBndrs (b:bs) thing_inside
  = bindIfaceBndr b     $ \ b' ->
    bindIfaceBndrs bs   $ \ bs' ->
    thing_inside (b':bs')

-----------------------
newExtCoreBndr :: IfaceLetBndr -> IfL Id
newExtCoreBndr (IfLetBndr var ty _)    -- Ignoring IdInfo for now
  = do  { mod <- getIfModule
        ; name <- newGlobalBinder mod (mkVarOccFS var) noSrcSpan
        ; ty' <- tcIfaceType ty
        ; return (mkLocalId name ty') }

-----------------------
bindIfaceTyVar :: IfaceTvBndr -> (TyVar -> IfL a) -> IfL a
bindIfaceTyVar (occ,kind) thing_inside
  = do  { name <- newIfaceName (mkTyVarOccFS occ)
        ; tyvar <- mk_iface_tyvar name kind
        ; extendIfaceTyVarEnv [tyvar] (thing_inside tyvar) }

bindIfaceTyVars :: [IfaceTvBndr] -> ([TyVar] -> IfL a) -> IfL a
bindIfaceTyVars bndrs thing_inside
  = do { names <- newIfaceNames (map mkTyVarOccFS occs)
        ; let (kis_kind, tys_kind) = span isSuperIfaceKind kinds
              (kis_name, tys_name) = splitAt (length kis_kind) names
          -- We need to bring the kind variables in scope since type
          -- variables may mention them.
        ; kvs <- zipWithM mk_iface_tyvar kis_name kis_kind
        ; extendIfaceTyVarEnv kvs $ do
        { tvs <- zipWithM mk_iface_tyvar tys_name tys_kind
        ; extendIfaceTyVarEnv tvs (thing_inside (kvs ++ tvs)) } }
  where
    (occs,kinds) = unzip bndrs

isSuperIfaceKind :: IfaceKind -> Bool
isSuperIfaceKind (IfaceTyConApp (IfaceTc n) []) = n == superKindTyConName
isSuperIfaceKind _ = False

mk_iface_tyvar :: Name -> IfaceKind -> IfL TyVar
mk_iface_tyvar name ifKind
   = do { kind <- tcIfaceKind ifKind
        ; return (Var.mkTyVar name kind) }

bindIfaceTyVars_AT :: [IfaceTvBndr] -> ([TyVar] -> IfL a) -> IfL a
-- Used for type variable in nested associated data/type declarations
-- where some of the type variables are already in scope
--    class C a where { data T a b }
-- Here 'a' is in scope when we look at the 'data T'
bindIfaceTyVars_AT [] thing_inside
  = thing_inside []
bindIfaceTyVars_AT (b@(tv_occ,_) : bs) thing_inside
  = do { mb_tv <- lookupIfaceTyVar tv_occ
       ; let bind_b :: (TyVar -> IfL a) -> IfL a
             bind_b = case mb_tv of
                        Just b' -> \k -> k b'
                        Nothing -> bindIfaceTyVar b
       ; bind_b $ \b' ->
         bindIfaceTyVars_AT bs $ \bs' ->
         thing_inside (b':bs') }
\end{code}

