{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | This module implements interface renaming, which is
-- used to rewrite interface files on the fly when we
-- are doing indefinite typechecking and need instantiations
-- of modules which do not necessarily exist yet.

module GHC.Iface.Rename (
    rnModIface,
    rnModExports,
    tcRnModIface,
    tcRnModExports,
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Env

import GHC.Tc.Utils.Monad

import GHC.Iface.Syntax
import GHC.Iface.Env
import {-# SOURCE #-} GHC.Iface.Load -- a bit vexing

import GHC.Unit
import GHC.Unit.State
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Deps

import GHC.Types.SrcLoc
import GHC.Types.Unique.FM
import GHC.Types.Avail
import GHC.Types.FieldLabel
import GHC.Types.Var
import GHC.Types.Basic
import GHC.Types.Name
import GHC.Types.Name.Shape

import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Fingerprint
import GHC.Utils.Panic

import GHC.Data.Bag

import qualified Data.Traversable as T

import Data.IORef

tcRnMsgMaybe :: IO (Either ErrorMessages a) -> TcM a
tcRnMsgMaybe do_this = do
    r <- liftIO $ do_this
    case r of
        Left errs -> do
            addMessages (emptyBag, errs)
            failM
        Right x -> return x

tcRnModIface :: [(ModuleName, Module)] -> Maybe NameShape -> ModIface -> TcM ModIface
tcRnModIface x y z = do
    hsc_env <- getTopEnv
    tcRnMsgMaybe $ rnModIface hsc_env x y z

tcRnModExports :: [(ModuleName, Module)] -> ModIface -> TcM [AvailInfo]
tcRnModExports x y = do
    hsc_env <- getTopEnv
    tcRnMsgMaybe $ rnModExports hsc_env x y

failWithRn :: SDoc -> ShIfM a
failWithRn doc = do
    errs_var <- fmap sh_if_errs getGblEnv
    dflags <- getDynFlags
    errs <- readTcRef errs_var
    -- TODO: maybe associate this with a source location?
    writeTcRef errs_var (errs `snocBag` mkPlainErrMsg dflags noSrcSpan doc)
    failM

-- | What we have is a generalized ModIface, which corresponds to
-- a module that looks like p[A=\<A>]:B.  We need a *specific* ModIface, e.g.
-- p[A=q():A]:B (or maybe even p[A=\<B>]:B) which we load
-- up (either to merge it, or to just use during typechecking).
--
-- Suppose we have:
--
--  p[A=\<A>]:M  ==>  p[A=q():A]:M
--
-- Substitute all occurrences of \<A> with q():A (renameHoleModule).
-- Then, for any Name of form {A.T}, replace the Name with
-- the Name according to the exports of the implementing module.
-- This works even for p[A=\<B>]:M, since we just read in the
-- exports of B.hi, which is assumed to be ready now.
--
-- This function takes an optional 'NameShape', which can be used
-- to further refine the identities in this interface: suppose
-- we read a declaration for {H.T} but we actually know that this
-- should be Foo.T; then we'll also rename this (this is used
-- when loading an interface to merge it into a requirement.)
rnModIface :: HscEnv -> [(ModuleName, Module)] -> Maybe NameShape
           -> ModIface -> IO (Either ErrorMessages ModIface)
rnModIface hsc_env insts nsubst iface =
    initRnIface hsc_env iface insts nsubst $ do
        mod <- rnModule (mi_module iface)
        sig_of <- case mi_sig_of iface of
                    Nothing -> return Nothing
                    Just x  -> fmap Just (rnModule x)
        exports <- mapM rnAvailInfo (mi_exports iface)
        decls <- mapM rnIfaceDecl' (mi_decls iface)
        insts <- mapM rnIfaceClsInst (mi_insts iface)
        fams <- mapM rnIfaceFamInst (mi_fam_insts iface)
        deps <- rnDependencies (mi_deps iface)
        -- TODO:
        -- mi_rules
        return iface { mi_module = mod
                     , mi_sig_of = sig_of
                     , mi_insts = insts
                     , mi_fam_insts = fams
                     , mi_exports = exports
                     , mi_decls = decls
                     , mi_deps = deps }

-- | Rename just the exports of a 'ModIface'.  Useful when we're doing
-- shaping prior to signature merging.
rnModExports :: HscEnv -> [(ModuleName, Module)] -> ModIface -> IO (Either ErrorMessages [AvailInfo])
rnModExports hsc_env insts iface
    = initRnIface hsc_env iface insts Nothing
    $ mapM rnAvailInfo (mi_exports iface)

rnDependencies :: Rename Dependencies
rnDependencies deps = do
    orphs  <- rnDepModules dep_orphs deps
    finsts <- rnDepModules dep_finsts deps
    return deps { dep_orphs = orphs, dep_finsts = finsts }

rnDepModules :: (Dependencies -> [Module]) -> Dependencies -> ShIfM [Module]
rnDepModules sel deps = do
    hsc_env <- getTopEnv
    hmap <- getHoleSubst
    -- NB: It's not necessary to test if we're doing signature renaming,
    -- because ModIface will never contain module reference for itself
    -- in these dependencies.
    fmap (nubSort . concat) . T.forM (sel deps) $ \mod -> do
        -- For holes, its necessary to "see through" the instantiation
        -- of the hole to get accurate family instance dependencies.
        -- For example, if B imports <A>, and <A> is instantiated with
        -- F, we must grab and include all of the dep_finsts from
        -- F to have an accurate transitive dep_finsts list.
        --
        -- However, we MUST NOT do this for regular modules.
        -- First, for efficiency reasons, doing this
        -- bloats the dep_finsts list, because we *already* had
        -- those modules in the list (it wasn't a hole module, after
        -- all). But there's a second, more important correctness
        -- consideration: we perform module renaming when running
        -- --abi-hash.  In this case, GHC's contract to the user is that
        -- it will NOT go and read out interfaces of any dependencies
        -- (https://github.com/haskell/cabal/issues/3633); the point of
        -- --abi-hash is just to get a hash of the on-disk interfaces
        -- for this *specific* package.  If we go off and tug on the
        -- interface for /everything/ in dep_finsts, we're gonna have a
        -- bad time.  (It's safe to do this for hole modules, though,
        -- because the hmap for --abi-hash is always trivial, so the
        -- interface we request is local.  Though, maybe we ought
        -- not to do it in this case either...)
        --
        -- This mistake was bug #15594.
        let mod' = renameHoleModule (hsc_units hsc_env) hmap mod
        if isHoleModule mod
          then do iface <- liftIO . initIfaceCheck (text "rnDepModule") hsc_env
                                  $ loadSysInterface (text "rnDepModule") mod'
                  return (mod' : sel (mi_deps iface))
          else return [mod']

{-
************************************************************************
*                                                                      *
                        ModIface substitution
*                                                                      *
************************************************************************
-}

-- | Run a computation in the 'ShIfM' monad.
initRnIface :: HscEnv -> ModIface -> [(ModuleName, Module)] -> Maybe NameShape
            -> ShIfM a -> IO (Either ErrorMessages a)
initRnIface hsc_env iface insts nsubst do_this = do
    errs_var <- newIORef emptyBag
    let hsubst = listToUFM insts
        rn_mod = renameHoleModule (hsc_units hsc_env) hsubst
        env = ShIfEnv {
            sh_if_module = rn_mod (mi_module iface),
            sh_if_semantic_module = rn_mod (mi_semantic_module iface),
            sh_if_hole_subst = listToUFM insts,
            sh_if_shape = nsubst,
            sh_if_errs = errs_var
        }
    -- Modeled off of 'initTc'
    res <- initTcRnIf 'c' hsc_env env () $ tryM do_this
    msgs <- readIORef errs_var
    case res of
        Left _                          -> return (Left msgs)
        Right r | not (isEmptyBag msgs) -> return (Left msgs)
                | otherwise             -> return (Right r)

-- | Environment for 'ShIfM' monads.
data ShIfEnv = ShIfEnv {
        -- What we are renaming the ModIface to.  It assumed that
        -- the original mi_module of the ModIface is
        -- @generalizeModule (mi_module iface)@.
        sh_if_module :: Module,
        -- The semantic module that we are renaming to
        sh_if_semantic_module :: Module,
        -- Cached hole substitution, e.g.
        -- @sh_if_hole_subst == listToUFM . unitIdInsts . moduleUnit . sh_if_module@
        sh_if_hole_subst :: ShHoleSubst,
        -- An optional name substitution to be applied when renaming
        -- the names in the interface.  If this is 'Nothing', then
        -- we just load the target interface and look at the export
        -- list to determine the renaming.
        sh_if_shape :: Maybe NameShape,
        -- Mutable reference to keep track of errors (similar to 'tcl_errs')
        sh_if_errs :: IORef ErrorMessages
    }

getHoleSubst :: ShIfM ShHoleSubst
getHoleSubst = fmap sh_if_hole_subst getGblEnv

type ShIfM = TcRnIf ShIfEnv ()
type Rename a = a -> ShIfM a


rnModule :: Rename Module
rnModule mod = do
    hmap <- getHoleSubst
    unit_state <- hsc_units <$> getTopEnv
    return (renameHoleModule unit_state hmap mod)

rnAvailInfo :: Rename AvailInfo
rnAvailInfo (Avail c) = Avail <$> rnGreName c
rnAvailInfo (AvailTC n ns) = do
    -- Why don't we rnIfaceGlobal the availName itself?  It may not
    -- actually be exported by the module it putatively is from, in
    -- which case we won't be able to tell what the name actually
    -- is.  But for the availNames they MUST be exported, so they
    -- will rename fine.
    ns' <- mapM rnGreName ns
    case ns' of
        [] -> panic "rnAvailInfoEmpty AvailInfo"
        (rep:rest) -> ASSERT2( all ((== childModule rep) . childModule) rest, ppr rep $$ hcat (map ppr rest) ) do
                         n' <- setNameModule (Just (childModule rep)) n
                         return (AvailTC n' ns')
  where
    childModule = nameModule . greNameMangledName

rnGreName :: Rename GreName
rnGreName (NormalGreName n) = NormalGreName <$> rnIfaceGlobal n
rnGreName (FieldGreName fl) = FieldGreName  <$> rnFieldLabel fl

rnFieldLabel :: Rename FieldLabel
rnFieldLabel (FieldLabel l b sel) = do
    sel' <- rnIfaceGlobal sel
    return (FieldLabel l b sel')




-- | The key function.  This gets called on every Name embedded
-- inside a ModIface.  Our job is to take a Name from some
-- generalized unit ID p[A=\<A>, B=\<B>], and change
-- it to the correct name for a (partially) instantiated unit
-- ID, e.g. p[A=q[]:A, B=\<B>].
--
-- There are two important things to do:
--
-- If a hole is substituted with a real module implementation,
-- we need to look at that actual implementation to determine what
-- the true identity of this name should be.  We'll do this by
-- loading that module's interface and looking at the mi_exports.
--
-- However, there is one special exception: when we are loading
-- the interface of a requirement.  In this case, we may not have
-- the "implementing" interface, because we are reading this
-- interface precisely to "merge it in".
--
--     External case:
--         p[A=\<B>]:A (and thisUnitId is something else)
--     We are loading this in order to determine B.hi!  So
--     don't load B.hi to find the exports.
--
--     Local case:
--         p[A=\<A>]:A (and thisUnitId is p[A=\<A>])
--     This should not happen, because the rename is not necessary
--     in this case, but if it does we shouldn't load A.hi!
--
-- Compare me with 'tcIfaceGlobal'!

-- In effect, this function needs compute the name substitution on the
-- fly.  What it has is the name that we would like to substitute.
-- If the name is not a hole name {M.x} (e.g. isHoleModule) then
-- no renaming can take place (although the inner hole structure must
-- be updated to account for the hole module renaming.)
rnIfaceGlobal :: Name -> ShIfM Name
rnIfaceGlobal n = do
    hsc_env <- getTopEnv
    let unit_state = hsc_units hsc_env
        home_unit  = hsc_home_unit hsc_env
    iface_semantic_mod <- fmap sh_if_semantic_module getGblEnv
    mb_nsubst <- fmap sh_if_shape getGblEnv
    hmap <- getHoleSubst
    let m = nameModule n
        m' = renameHoleModule unit_state hmap m
    case () of
       -- Did we encounter {A.T} while renaming p[A=<B>]:A? If so,
       -- do NOT assume B.hi is available.
       -- In this case, rename {A.T} to {B.T} but don't look up exports.
     _ | m' == iface_semantic_mod
       , isHoleModule m'
      -- NB: this could be Nothing for computeExports, we have
      -- nothing to say.
      -> do n' <- setNameModule (Just m') n
            case mb_nsubst of
                Nothing -> return n'
                Just nsubst ->
                    case maybeSubstNameShape nsubst n' of
                        -- TODO: would love to have context
                        -- TODO: This will give an unpleasant message if n'
                        -- is a constructor; then we'll suggest adding T
                        -- but it won't work.
                        Nothing -> failWithRn $ vcat [
                            text "The identifier" <+> ppr (occName n') <+>
                                text "does not exist in the local signature.",
                            parens (text "Try adding it to the export list of the hsig file.")
                            ]
                        Just n'' -> return n''
       -- Fastpath: we are renaming p[H=<H>]:A.T, in which case the
       -- export list is irrelevant.
       | not (isHoleModule m)
      -> setNameModule (Just m') n
       -- The substitution was from <A> to p[]:A.
       -- But this does not mean {A.T} goes to p[]:A.T:
       -- p[]:A may reexport T from somewhere else.  Do the name
       -- substitution.  Furthermore, we need
       -- to make sure we pick the accurate name NOW,
       -- or we might accidentally reject a merge.
       | otherwise
      -> do -- Make sure we look up the local interface if substitution
            -- went from <A> to <B>.
            let m'' = if isHoleModule m'
                        -- Pull out the local guy!!
                        then mkHomeModule home_unit (moduleName m')
                        else m'
            iface <- liftIO . initIfaceCheck (text "rnIfaceGlobal") hsc_env
                            $ loadSysInterface (text "rnIfaceGlobal") m''
            let nsubst = mkNameShape (moduleName m) (mi_exports iface)
            case maybeSubstNameShape nsubst n of
                Nothing -> failWithRn $ vcat [
                    text "The identifier" <+> ppr (occName n) <+>
                        -- NB: report m' because it's more user-friendly
                        text "does not exist in the signature for" <+> ppr m',
                    parens (text "Try adding it to the export list in that hsig file.")
                    ]
                Just n' -> return n'

-- | Rename an implicit name, e.g., a DFun or coercion axiom.
-- Here is where we ensure that DFuns have the correct module as described in
-- Note [rnIfaceNeverExported].
rnIfaceNeverExported :: Name -> ShIfM Name
rnIfaceNeverExported name = do
    hmap <- getHoleSubst
    unit_state <- hsc_units <$> getTopEnv
    iface_semantic_mod <- fmap sh_if_semantic_module getGblEnv
    let m = renameHoleModule unit_state hmap $ nameModule name
    -- Doublecheck that this DFun/coercion axiom was, indeed, locally defined.
    MASSERT2( iface_semantic_mod == m, ppr iface_semantic_mod <+> ppr m )
    setNameModule (Just m) name

-- Note [rnIfaceNeverExported]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- For the high-level overview, see
-- Note [Handling never-exported TyThings under Backpack]
--
-- When we see a reference to an entity that was defined in a signature,
-- 'rnIfaceGlobal' relies on the identifier in question being part of the
-- exports of the implementing 'ModIface', so that we can use the exports to
-- decide how to rename the identifier.  Unfortunately, references to 'DFun's
-- and 'CoAxiom's will run into trouble under this strategy, because they are
-- never exported.
--
-- Let us consider first what should happen in the absence of promotion.  In
-- this setting, a reference to a 'DFun' or a 'CoAxiom' can only occur inside
-- the signature *that is defining it* (as there are no Core terms in
-- typechecked-only interface files, there's no way for a reference to occur
-- besides from the defining 'ClsInst' or closed type family).  Thus,
-- it doesn't really matter what names we give the DFun/CoAxiom, as long
-- as it's consistent between the declaration site and the use site.
--
-- We have to make sure that these bogus names don't get propagated,
-- but it is fine: see Note [Signature merging DFuns] for the fixups
-- to the names we do before writing out the merged interface.
-- (It's even easier for instantiation, since the DFuns all get
-- dropped entirely; the instances are reexported implicitly.)
--
-- Unfortunately, this strategy is not enough in the presence of promotion
-- (see bug #13149), where modules which import the signature may make
-- reference to their coercions.  It's not altogether clear how to
-- fix this case, but it is definitely a bug!

-- PILES AND PILES OF BOILERPLATE

-- | Rename an 'IfaceClsInst', with special handling for an associated
-- dictionary function.
rnIfaceClsInst :: Rename IfaceClsInst
rnIfaceClsInst cls_inst = do
    n <- rnIfaceGlobal (ifInstCls cls_inst)
    tys <- mapM rnMaybeIfaceTyCon (ifInstTys cls_inst)

    dfun <- rnIfaceNeverExported (ifDFun cls_inst)
    return cls_inst { ifInstCls = n
                    , ifInstTys = tys
                    , ifDFun = dfun
                    }

rnMaybeIfaceTyCon :: Rename (Maybe IfaceTyCon)
rnMaybeIfaceTyCon Nothing = return Nothing
rnMaybeIfaceTyCon (Just tc) = Just <$> rnIfaceTyCon tc

rnIfaceFamInst :: Rename IfaceFamInst
rnIfaceFamInst d = do
    fam <- rnIfaceGlobal (ifFamInstFam d)
    tys <- mapM rnMaybeIfaceTyCon (ifFamInstTys d)
    axiom <- rnIfaceGlobal (ifFamInstAxiom d)
    return d { ifFamInstFam = fam, ifFamInstTys = tys, ifFamInstAxiom = axiom }

rnIfaceDecl' :: Rename (Fingerprint, IfaceDecl)
rnIfaceDecl' (fp, decl) = (,) fp <$> rnIfaceDecl decl

rnIfaceDecl :: Rename IfaceDecl
rnIfaceDecl d@IfaceId{} = do
            name <- case ifIdDetails d of
                      IfDFunId -> rnIfaceNeverExported (ifName d)
                      _ | isDefaultMethodOcc (occName (ifName d))
                        -> rnIfaceNeverExported (ifName d)
                      -- Typeable bindings. See Note [Grand plan for Typeable].
                      _ | isTypeableBindOcc (occName (ifName d))
                        -> rnIfaceNeverExported (ifName d)
                        | otherwise -> rnIfaceGlobal (ifName d)
            ty <- rnIfaceType (ifType d)
            details <- rnIfaceIdDetails (ifIdDetails d)
            info <- rnIfaceIdInfo (ifIdInfo d)
            return d { ifName = name
                     , ifType = ty
                     , ifIdDetails = details
                     , ifIdInfo = info
                     }
rnIfaceDecl d@IfaceData{} = do
            name <- rnIfaceGlobal (ifName d)
            binders <- mapM rnIfaceTyConBinder (ifBinders d)
            ctxt <- mapM rnIfaceType (ifCtxt d)
            cons <- rnIfaceConDecls (ifCons d)
            res_kind <- rnIfaceType (ifResKind d)
            parent <- rnIfaceTyConParent (ifParent d)
            return d { ifName = name
                     , ifBinders = binders
                     , ifCtxt = ctxt
                     , ifCons = cons
                     , ifResKind = res_kind
                     , ifParent = parent
                     }
rnIfaceDecl d@IfaceSynonym{} = do
            name <- rnIfaceGlobal (ifName d)
            binders <- mapM rnIfaceTyConBinder (ifBinders d)
            syn_kind <- rnIfaceType (ifResKind d)
            syn_rhs <- rnIfaceType (ifSynRhs d)
            return d { ifName = name
                     , ifBinders = binders
                     , ifResKind = syn_kind
                     , ifSynRhs = syn_rhs
                     }
rnIfaceDecl d@IfaceFamily{} = do
            name <- rnIfaceGlobal (ifName d)
            binders <- mapM rnIfaceTyConBinder (ifBinders d)
            fam_kind <- rnIfaceType (ifResKind d)
            fam_flav <- rnIfaceFamTyConFlav (ifFamFlav d)
            return d { ifName = name
                     , ifBinders = binders
                     , ifResKind = fam_kind
                     , ifFamFlav = fam_flav
                     }
rnIfaceDecl d@IfaceClass{} = do
            name <- rnIfaceGlobal (ifName d)
            binders <- mapM rnIfaceTyConBinder (ifBinders d)
            body <- rnIfaceClassBody (ifBody d)
            return d { ifName    = name
                     , ifBinders = binders
                     , ifBody    = body
                     }
rnIfaceDecl d@IfaceAxiom{} = do
            name <- rnIfaceNeverExported (ifName d)
            tycon <- rnIfaceTyCon (ifTyCon d)
            ax_branches <- mapM rnIfaceAxBranch (ifAxBranches d)
            return d { ifName = name
                     , ifTyCon = tycon
                     , ifAxBranches = ax_branches
                     }
rnIfaceDecl d@IfacePatSyn{} =  do
            name <- rnIfaceGlobal (ifName d)
            let rnPat (n, b) = (,) <$> rnIfaceGlobal n <*> pure b
            pat_matcher <- rnPat (ifPatMatcher d)
            pat_builder <- T.traverse rnPat (ifPatBuilder d)
            pat_univ_bndrs <- mapM rnIfaceForAllBndr (ifPatUnivBndrs d)
            pat_ex_bndrs <- mapM rnIfaceForAllBndr (ifPatExBndrs d)
            pat_prov_ctxt <- mapM rnIfaceType (ifPatProvCtxt d)
            pat_req_ctxt <- mapM rnIfaceType (ifPatReqCtxt d)
            pat_args <- mapM rnIfaceType (ifPatArgs d)
            pat_ty <- rnIfaceType (ifPatTy d)
            return d { ifName = name
                     , ifPatMatcher = pat_matcher
                     , ifPatBuilder = pat_builder
                     , ifPatUnivBndrs = pat_univ_bndrs
                     , ifPatExBndrs = pat_ex_bndrs
                     , ifPatProvCtxt = pat_prov_ctxt
                     , ifPatReqCtxt = pat_req_ctxt
                     , ifPatArgs = pat_args
                     , ifPatTy = pat_ty
                     }

rnIfaceClassBody :: Rename IfaceClassBody
rnIfaceClassBody IfAbstractClass = return IfAbstractClass
rnIfaceClassBody d@IfConcreteClass{} = do
    ctxt <- mapM rnIfaceType (ifClassCtxt d)
    ats <- mapM rnIfaceAT (ifATs d)
    sigs <- mapM rnIfaceClassOp (ifSigs d)
    return d { ifClassCtxt = ctxt, ifATs = ats, ifSigs = sigs }

rnIfaceFamTyConFlav :: Rename IfaceFamTyConFlav
rnIfaceFamTyConFlav (IfaceClosedSynFamilyTyCon (Just (n, axs)))
    = IfaceClosedSynFamilyTyCon . Just <$> ((,) <$> rnIfaceNeverExported n
                                                <*> mapM rnIfaceAxBranch axs)
rnIfaceFamTyConFlav flav = pure flav

rnIfaceAT :: Rename IfaceAT
rnIfaceAT (IfaceAT decl mb_ty)
    = IfaceAT <$> rnIfaceDecl decl <*> T.traverse rnIfaceType mb_ty

rnIfaceTyConParent :: Rename IfaceTyConParent
rnIfaceTyConParent (IfDataInstance n tc args)
    = IfDataInstance <$> rnIfaceGlobal n
                     <*> rnIfaceTyCon tc
                     <*> rnIfaceAppArgs args
rnIfaceTyConParent IfNoParent = pure IfNoParent

rnIfaceConDecls :: Rename IfaceConDecls
rnIfaceConDecls (IfDataTyCon ds)
    = IfDataTyCon <$> mapM rnIfaceConDecl ds
rnIfaceConDecls (IfNewTyCon d) = IfNewTyCon <$> rnIfaceConDecl d
rnIfaceConDecls IfAbstractTyCon = pure IfAbstractTyCon

rnIfaceConDecl :: Rename IfaceConDecl
rnIfaceConDecl d = do
    con_name <- rnIfaceGlobal (ifConName d)
    con_ex_tvs <- mapM rnIfaceBndr (ifConExTCvs d)
    con_user_tvbs <- mapM rnIfaceForAllBndr (ifConUserTvBinders d)
    let rnIfConEqSpec (n,t) = (,) n <$> rnIfaceType t
    con_eq_spec <- mapM rnIfConEqSpec (ifConEqSpec d)
    con_ctxt <- mapM rnIfaceType (ifConCtxt d)
    con_arg_tys <- mapM rnIfaceScaledType (ifConArgTys d)
    con_fields <- mapM rnFieldLabel (ifConFields d)
    let rnIfaceBang (IfUnpackCo co) = IfUnpackCo <$> rnIfaceCo co
        rnIfaceBang bang = pure bang
    con_stricts <- mapM rnIfaceBang (ifConStricts d)
    return d { ifConName = con_name
             , ifConExTCvs = con_ex_tvs
             , ifConUserTvBinders = con_user_tvbs
             , ifConEqSpec = con_eq_spec
             , ifConCtxt = con_ctxt
             , ifConArgTys = con_arg_tys
             , ifConFields = con_fields
             , ifConStricts = con_stricts
             }

rnIfaceClassOp :: Rename IfaceClassOp
rnIfaceClassOp (IfaceClassOp n ty dm) =
    IfaceClassOp <$> rnIfaceGlobal n
                 <*> rnIfaceType ty
                 <*> rnMaybeDefMethSpec dm

rnMaybeDefMethSpec :: Rename (Maybe (DefMethSpec IfaceType))
rnMaybeDefMethSpec (Just (GenericDM ty)) = Just . GenericDM <$> rnIfaceType ty
rnMaybeDefMethSpec mb = return mb

rnIfaceAxBranch :: Rename IfaceAxBranch
rnIfaceAxBranch d = do
    ty_vars <- mapM rnIfaceTvBndr (ifaxbTyVars d)
    lhs <- rnIfaceAppArgs (ifaxbLHS d)
    rhs <- rnIfaceType (ifaxbRHS d)
    return d { ifaxbTyVars = ty_vars
             , ifaxbLHS = lhs
             , ifaxbRHS = rhs }

rnIfaceIdInfo :: Rename IfaceIdInfo
rnIfaceIdInfo = mapM rnIfaceInfoItem

rnIfaceInfoItem :: Rename IfaceInfoItem
rnIfaceInfoItem (HsUnfold lb if_unf)
    = HsUnfold lb <$> rnIfaceUnfolding if_unf
rnIfaceInfoItem i
    = pure i

rnIfaceUnfolding :: Rename IfaceUnfolding
rnIfaceUnfolding (IfCoreUnfold stable if_expr)
    = IfCoreUnfold stable <$> rnIfaceExpr if_expr
rnIfaceUnfolding (IfCompulsory if_expr)
    = IfCompulsory <$> rnIfaceExpr if_expr
rnIfaceUnfolding (IfInlineRule arity unsat_ok boring_ok if_expr)
    = IfInlineRule arity unsat_ok boring_ok <$> rnIfaceExpr if_expr
rnIfaceUnfolding (IfDFunUnfold bs ops)
    = IfDFunUnfold <$> rnIfaceBndrs bs <*> mapM rnIfaceExpr ops

rnIfaceExpr :: Rename IfaceExpr
rnIfaceExpr (IfaceLcl name) = pure (IfaceLcl name)
rnIfaceExpr (IfaceExt gbl) = IfaceExt <$> rnIfaceGlobal gbl
rnIfaceExpr (IfaceType ty) = IfaceType <$> rnIfaceType ty
rnIfaceExpr (IfaceCo co) = IfaceCo <$> rnIfaceCo co
rnIfaceExpr (IfaceTuple sort args) = IfaceTuple sort <$> rnIfaceExprs args
rnIfaceExpr (IfaceLam lam_bndr expr)
    = IfaceLam <$> rnIfaceLamBndr lam_bndr <*> rnIfaceExpr expr
rnIfaceExpr (IfaceApp fun arg)
    = IfaceApp <$> rnIfaceExpr fun <*> rnIfaceExpr arg
rnIfaceExpr (IfaceCase scrut case_bndr alts)
    = IfaceCase <$> rnIfaceExpr scrut
                <*> pure case_bndr
                <*> mapM rnIfaceAlt alts
rnIfaceExpr (IfaceECase scrut ty)
    = IfaceECase <$> rnIfaceExpr scrut <*> rnIfaceType ty
rnIfaceExpr (IfaceLet (IfaceNonRec bndr rhs) body)
    = IfaceLet <$> (IfaceNonRec <$> rnIfaceLetBndr bndr <*> rnIfaceExpr rhs)
               <*> rnIfaceExpr body
rnIfaceExpr (IfaceLet (IfaceRec pairs) body)
    = IfaceLet <$> (IfaceRec <$> mapM (\(bndr, rhs) ->
                                        (,) <$> rnIfaceLetBndr bndr
                                            <*> rnIfaceExpr rhs) pairs)
               <*> rnIfaceExpr body
rnIfaceExpr (IfaceCast expr co)
    = IfaceCast <$> rnIfaceExpr expr <*> rnIfaceCo co
rnIfaceExpr (IfaceLit lit) = pure (IfaceLit lit)
rnIfaceExpr (IfaceFCall cc ty) = IfaceFCall cc <$> rnIfaceType ty
rnIfaceExpr (IfaceTick tickish expr) = IfaceTick tickish <$> rnIfaceExpr expr

rnIfaceBndrs :: Rename [IfaceBndr]
rnIfaceBndrs = mapM rnIfaceBndr

rnIfaceBndr :: Rename IfaceBndr
rnIfaceBndr (IfaceIdBndr (w, fs, ty)) = IfaceIdBndr <$> ((,,) w fs <$> rnIfaceType ty)
rnIfaceBndr (IfaceTvBndr tv_bndr) = IfaceTvBndr <$> rnIfaceTvBndr tv_bndr

rnIfaceTvBndr :: Rename IfaceTvBndr
rnIfaceTvBndr (fs, kind) = (,) fs <$> rnIfaceType kind

rnIfaceTyConBinder :: Rename IfaceTyConBinder
rnIfaceTyConBinder (Bndr tv vis) = Bndr <$> rnIfaceBndr tv <*> pure vis

rnIfaceAlt :: Rename IfaceAlt
rnIfaceAlt (conalt, names, rhs)
     = (,,) <$> rnIfaceConAlt conalt <*> pure names <*> rnIfaceExpr rhs

rnIfaceConAlt :: Rename IfaceConAlt
rnIfaceConAlt (IfaceDataAlt data_occ) = IfaceDataAlt <$> rnIfaceGlobal data_occ
rnIfaceConAlt alt = pure alt

rnIfaceLetBndr :: Rename IfaceLetBndr
rnIfaceLetBndr (IfLetBndr fs ty info jpi)
    = IfLetBndr fs <$> rnIfaceType ty <*> rnIfaceIdInfo info <*> pure jpi

rnIfaceLamBndr :: Rename IfaceLamBndr
rnIfaceLamBndr (bndr, oneshot) = (,) <$> rnIfaceBndr bndr <*> pure oneshot

rnIfaceMCo :: Rename IfaceMCoercion
rnIfaceMCo IfaceMRefl    = pure IfaceMRefl
rnIfaceMCo (IfaceMCo co) = IfaceMCo <$> rnIfaceCo co

rnIfaceCo :: Rename IfaceCoercion
rnIfaceCo (IfaceReflCo ty) = IfaceReflCo <$> rnIfaceType ty
rnIfaceCo (IfaceGReflCo role ty mco)
  = IfaceGReflCo role <$> rnIfaceType ty <*> rnIfaceMCo mco
rnIfaceCo (IfaceFunCo role w co1 co2)
    = IfaceFunCo role <$> rnIfaceCo w <*> rnIfaceCo co1 <*> rnIfaceCo co2
rnIfaceCo (IfaceTyConAppCo role tc cos)
    = IfaceTyConAppCo role <$> rnIfaceTyCon tc <*> mapM rnIfaceCo cos
rnIfaceCo (IfaceAppCo co1 co2)
    = IfaceAppCo <$> rnIfaceCo co1 <*> rnIfaceCo co2
rnIfaceCo (IfaceForAllCo bndr co1 co2)
    = IfaceForAllCo <$> rnIfaceBndr bndr <*> rnIfaceCo co1 <*> rnIfaceCo co2
rnIfaceCo (IfaceFreeCoVar c) = pure (IfaceFreeCoVar c)
rnIfaceCo (IfaceCoVarCo lcl) = IfaceCoVarCo <$> pure lcl
rnIfaceCo (IfaceHoleCo lcl)  = IfaceHoleCo  <$> pure lcl
rnIfaceCo (IfaceAxiomInstCo n i cs)
    = IfaceAxiomInstCo <$> rnIfaceGlobal n <*> pure i <*> mapM rnIfaceCo cs
rnIfaceCo (IfaceUnivCo s r t1 t2)
    = IfaceUnivCo s r <$> rnIfaceType t1 <*> rnIfaceType t2
rnIfaceCo (IfaceSymCo c)
    = IfaceSymCo <$> rnIfaceCo c
rnIfaceCo (IfaceTransCo c1 c2)
    = IfaceTransCo <$> rnIfaceCo c1 <*> rnIfaceCo c2
rnIfaceCo (IfaceInstCo c1 c2)
    = IfaceInstCo <$> rnIfaceCo c1 <*> rnIfaceCo c2
rnIfaceCo (IfaceNthCo d c) = IfaceNthCo d <$> rnIfaceCo c
rnIfaceCo (IfaceLRCo lr c) = IfaceLRCo lr <$> rnIfaceCo c
rnIfaceCo (IfaceSubCo c) = IfaceSubCo <$> rnIfaceCo c
rnIfaceCo (IfaceAxiomRuleCo ax cos)
    = IfaceAxiomRuleCo ax <$> mapM rnIfaceCo cos
rnIfaceCo (IfaceKindCo c) = IfaceKindCo <$> rnIfaceCo c

rnIfaceTyCon :: Rename IfaceTyCon
rnIfaceTyCon (IfaceTyCon n info)
    = IfaceTyCon <$> rnIfaceGlobal n <*> pure info

rnIfaceExprs :: Rename [IfaceExpr]
rnIfaceExprs = mapM rnIfaceExpr

rnIfaceIdDetails :: Rename IfaceIdDetails
rnIfaceIdDetails (IfRecSelId (Left tc) b) = IfRecSelId <$> fmap Left (rnIfaceTyCon tc) <*> pure b
rnIfaceIdDetails (IfRecSelId (Right decl) b) = IfRecSelId <$> fmap Right (rnIfaceDecl decl) <*> pure b
rnIfaceIdDetails details = pure details

rnIfaceType :: Rename IfaceType
rnIfaceType (IfaceFreeTyVar n) = pure (IfaceFreeTyVar n)
rnIfaceType (IfaceTyVar   n)   = pure (IfaceTyVar n)
rnIfaceType (IfaceAppTy t1 t2)
    = IfaceAppTy <$> rnIfaceType t1 <*> rnIfaceAppArgs t2
rnIfaceType (IfaceLitTy l)         = return (IfaceLitTy l)
rnIfaceType (IfaceFunTy af w t1 t2)
    = IfaceFunTy af <$> rnIfaceType w <*> rnIfaceType t1 <*> rnIfaceType t2
rnIfaceType (IfaceTupleTy s i tks)
    = IfaceTupleTy s i <$> rnIfaceAppArgs tks
rnIfaceType (IfaceTyConApp tc tks)
    = IfaceTyConApp <$> rnIfaceTyCon tc <*> rnIfaceAppArgs tks
rnIfaceType (IfaceForAllTy tv t)
    = IfaceForAllTy <$> rnIfaceForAllBndr tv <*> rnIfaceType t
rnIfaceType (IfaceCoercionTy co)
    = IfaceCoercionTy <$> rnIfaceCo co
rnIfaceType (IfaceCastTy ty co)
    = IfaceCastTy <$> rnIfaceType ty <*> rnIfaceCo co

rnIfaceScaledType :: Rename (IfaceMult, IfaceType)
rnIfaceScaledType (m, t) = (,) <$> rnIfaceType m <*> rnIfaceType t

rnIfaceForAllBndr :: Rename (VarBndr IfaceBndr flag)
rnIfaceForAllBndr (Bndr tv vis) = Bndr <$> rnIfaceBndr tv <*> pure vis

rnIfaceAppArgs :: Rename IfaceAppArgs
rnIfaceAppArgs (IA_Arg t a ts) = IA_Arg <$> rnIfaceType t <*> pure a
                                        <*> rnIfaceAppArgs ts
rnIfaceAppArgs IA_Nil = pure IA_Nil
