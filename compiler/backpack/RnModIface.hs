{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | This module implements interface renaming, which is
-- used to rewrite interface files on the fly when we
-- are doing indefinite typechecking and need instantiations
-- of modules which do not necessarily exist yet.

module RnModIface(
    rnModIface,
    rnModExports,
    ) where

#include "HsVersions.h"

import Outputable
import HscTypes
import Module
import UniqFM
import Avail
import IfaceSyn
import FieldLabel
import Var

import Name
import TcRnMonad
import Util
import Fingerprint
import BasicTypes

-- a bit vexing
import {-# SOURCE #-} LoadIface
import DynFlags

import qualified Data.Traversable as T

import NameShape
import IfaceEnv

-- | What we have a generalized ModIface, which corresponds to
-- a module that looks like p[A=<A>]:B.  We need a *specific* ModIface, e.g.
-- p[A=q():A]:B (or maybe even p[A=<B>]:B) which we load
-- up (either to merge it, or to just use during typechecking).
--
-- Suppose we have:
--
--  p[A=<A>]:M  ==>  p[A=q():A]:M
--
-- Substitute all occurrences of <A> with q():A (renameHoleModule).
-- Then, for any Name of form {A.T}, replace the Name with
-- the Name according to the exports of the implementing module.
-- This works even for p[A=<B>]:M, since we just read in the
-- exports of B.hi, which is assumed to be ready now.
--
-- This function takes an optional 'NameShape', which can be used
-- to further refine the identities in this interface: suppose
-- we read a declaration for {H.T} but we actually know that this
-- should be Foo.T; then we'll also rename this (this is used
-- when loading an interface to merge it into a requirement.)
rnModIface :: HscEnv -> [(ModuleName, Module)] -> Maybe NameShape
           -> ModIface -> IO ModIface
rnModIface hsc_env insts nsubst iface = do
    initRnIface hsc_env iface insts nsubst $ do
        mod <- rnModule (mi_module iface)
        sig_of <- case mi_sig_of iface of
                    Nothing -> return Nothing
                    Just x  -> fmap Just (rnModule x)
        exports <- mapM rnAvailInfo (mi_exports iface)
        decls <- mapM rnIfaceDecl' (mi_decls iface)
        insts <- mapM rnIfaceClsInst (mi_insts iface)
        fams <- mapM rnIfaceFamInst (mi_fam_insts iface)
        -- TODO:
        -- mi_rules
        -- mi_vect_info (LOW PRIORITY)
        return iface { mi_module = mod
                     , mi_sig_of = sig_of
                     , mi_insts = insts
                     , mi_fam_insts = fams
                     , mi_exports = exports
                     , mi_decls = decls }

-- | Rename just the exports of a 'ModIface'.  Useful when we're doing
-- shaping prior to signature merging.
rnModExports :: HscEnv -> [(ModuleName, Module)] -> ModIface -> IO [AvailInfo]
rnModExports hsc_env insts iface
    = initRnIface hsc_env iface insts Nothing
    $ mapM rnAvailInfo (mi_exports iface)

{-
************************************************************************
*                                                                      *
                        ModIface substitution
*                                                                      *
************************************************************************
-}

-- | Initialize the 'ShIfM' monad.
initRnIface :: HscEnv -> ModIface -> [(ModuleName, Module)] -> Maybe NameShape
            -> ShIfM a -> IO a
initRnIface hsc_env iface insts nsubst do_this =
    let hsubst = listToUFM insts
        rn_mod = renameHoleModule (hsc_dflags hsc_env) hsubst
        env = ShIfEnv {
            sh_if_module = rn_mod (mi_module iface),
            sh_if_semantic_module = rn_mod (mi_semantic_module iface),
            sh_if_hole_subst = listToUFM insts,
            sh_if_shape = nsubst
        }
    in initTcRnIf 'c' hsc_env env () do_this

-- | Environment for 'ShIfM' monads.
data ShIfEnv = ShIfEnv {
        -- What we are renaming the ModIface to.  It assumed that
        -- the original mi_module of the ModIface is
        -- @generalizeModule (mi_module iface)@.
        sh_if_module :: Module,
        -- The semantic module that we are renaming to
        sh_if_semantic_module :: Module,
        -- Cached hole substitution, e.g.
        -- @sh_if_hole_subst == listToUFM . unitIdInsts . moduleUnitId . sh_if_module@
        sh_if_hole_subst :: ShHoleSubst,
        -- An optional name substitution to be applied when renaming
        -- the names in the interface.  If this is 'Nothing', then
        -- we just load the target interface and look at the export
        -- list to determine the renaming.
        sh_if_shape :: Maybe NameShape
    }

getHoleSubst :: ShIfM ShHoleSubst
getHoleSubst = fmap sh_if_hole_subst getGblEnv

type ShIfM = TcRnIf ShIfEnv ()
type Rename a = a -> ShIfM a


rnModule :: Rename Module
rnModule mod = do
    hmap <- getHoleSubst
    dflags <- getDynFlags
    return (renameHoleModule dflags hmap mod)

rnAvailInfo :: Rename AvailInfo
rnAvailInfo (Avail p n) = Avail p <$> rnIfaceGlobal n
rnAvailInfo (AvailTC n ns fs) = do
    -- Why don't we rnIfaceGlobal the availName itself?  It may not
    -- actually be exported by the module it putatively is from, in
    -- which case we won't be able to tell what the name actually
    -- is.  But for the availNames they MUST be exported, so they
    -- will rename fine.
    ns' <- mapM rnIfaceGlobal ns
    fs' <- mapM rnFieldLabel fs
    case ns' ++ map flSelector fs' of
        [] -> panic "rnAvailInfoEmpty AvailInfo"
        (rep:rest) -> ASSERT2( all ((== nameModule rep) . nameModule) rest, ppr rep $$ hcat (map ppr rest) ) do
                         n' <- setNameModule (Just (nameModule rep)) n
                         return (AvailTC n' ns' fs')

rnFieldLabel :: Rename FieldLabel
rnFieldLabel (FieldLabel l b sel) = do
    sel' <- rnIfaceGlobal sel
    return (FieldLabel l b sel')




-- | The key function.  This gets called on every Name embedded
-- inside a ModIface.  Our job is to take a Name from some
-- generalized unit ID p[A=<A>, B=<B>], and change
-- it to the correct name for a (partially) instantiated unit
-- ID, e.g. p[A=q[]:A, B=<B>].
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
--         p[A=<B>]:A (and thisUnitId is something else)
--     We are loading this in order to determine B.hi!  So
--     don't load B.hi to find the exports.
--
--     Local case:
--         p[A=<A>]:A (and thisUnitId is p[A=<A>])
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
    let dflags = hsc_dflags hsc_env
    iface_semantic_mod <- fmap sh_if_semantic_module getGblEnv
    mb_nsubst <- fmap sh_if_shape getGblEnv
    hmap <- getHoleSubst
    let m = nameModule n
        m' = renameHoleModule dflags hmap m
    case () of
       -- Did we encounter {A.T} while renaming p[A=<B>]:A? If so,
       -- do NOT assume B.hi is available.
       -- In this case, rename {A.T} to {B.T} but don't look up exports.
     _ | m' == iface_semantic_mod
       , isHoleModule m'
      -- NB: this could be Nothing for computeExports, we have
      -- nothing to say.
      -> do fmap (case mb_nsubst of
                   Nothing -> id
                   Just nsubst -> substNameShape nsubst)
                $ setNameModule (Just m') n
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
                        then mkModule (thisPackage dflags) (moduleName m')
                        else m'
            iface <- liftIO . initIfaceCheck (text "rnIfaceGlobal") hsc_env
                            $ loadSysInterface (text "rnIfaceGlobal") m''
            let nsubst = mkNameShape (moduleName m) (mi_exports iface)
            return (substNameShape nsubst n)

-- PILES AND PILES OF BOILERPLATE

-- | Rename an 'IfaceClsInst', with special handling for an associated
-- dictionary function.
rnIfaceClsInst :: Rename IfaceClsInst
rnIfaceClsInst cls_inst = do
    n <- rnIfaceGlobal (ifInstCls cls_inst)
    tys <- mapM rnMaybeIfaceTyCon (ifInstTys cls_inst)

    hmap <- getHoleSubst
    dflags <- getDynFlags

    -- Note [Bogus DFun renamings]
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Every 'IfaceClsInst' is associated with a DFun; in fact, when
    -- we are typechecking only, it is the ONLY place a DFun Id
    -- can appear.  This DFun must refer to a DFun that is defined
    -- elsewhere in the 'ModIface'.
    --
    -- Unfortunately, DFuns are not exported (don't appear in
    -- mi_exports), so we can't look at the exports (as we do in
    -- rnIfaceGlobal) to rename it.
    --
    -- We have to rename it to *something*.  So what we do depends
    -- on the situation:
    --
    --  * If the instance wasn't defined in a signature, the DFun
    --    have a name like p[A=<A>]:B.$fShowFoo.  This is the
    --    easy case: just apply the module substitution to the
    --    unit id and go our merry way.
    --
    --  * If the instance was defined in a signature, we are in
    --    an interesting situation.  Suppose we are instantiating
    --    the signature:
    --
    --      signature H where
    --          instance F T           -- {H.$fxFT}
    --      module H where
    --          instance F T where ... -- p[]:H.$fFT
    --
    --    In an ideal world, we would map {H.$fxFT} to p[]:H.$fFT.
    --    But we have no idea what the correct DFun is: the OccNames
    --    don't match up.  Nor do we really want to wire up {H.$fxFT}
    --    to p[]:H.$fFT: we'd rather have it point at the DFun
    --    from the *signature's* interface, and use that type to
    --    find the actual instance we want to compare against.
    --
    --    So, to handle this case, we have to do several things:
    --
    --      * In 'rnIfaceClsInst', we just blindly rename the
    --        the identifier to something that looks vaguely plausible.
    --        In the instantiating case, we just map {H.$fxFT}
    --        to p[]:H.$fxFT.  In the merging case, we map
    --        {H.$fxFT} to {H2.$fxFT}.
    --
    --      * In 'lookupIfaceTop', we arrange for the top-level DFun
    --        to be assigned the very same identifier we picked
    --        during renaming (p[]:H.$fxFT)
    --
    --      * Finally, in 'tcIfaceInstWithDFunTypeEnv', we make sure
    --        to grab the correct 'TyThing' for the DFun directly
    --        from the local type environment (which was constructed
    --        using 'Name's from 'lookupIfaceTop').
    --
    --    It's all a bit of a giant Rube Goldberg machine, but it
    --    seems to work!  Note that the name we pick here doesn't
    --    really matter, since we throw it out shortly after
    --    (for merging, we rename all of the DFuns so that they
    --    are unique; for instantiation, the final interface never
    --    mentions DFuns since they are implicitly exported.)  The
    --    important thing is that it's consistent everywhere.

    iface_semantic_mod <- fmap sh_if_semantic_module getGblEnv
    let m = renameHoleModule dflags hmap $ nameModule (ifDFun cls_inst)
    -- Doublecheck that this DFun was, indeed, locally defined.
    MASSERT2( iface_semantic_mod == m, ppr iface_semantic_mod <+> ppr m )
    dfun <- setNameModule (Just m) (ifDFun cls_inst)
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
            ty <- rnIfaceType (ifType d)
            details <- rnIfaceIdDetails (ifIdDetails d)
            info <- rnIfaceIdInfo (ifIdInfo d)
            return d { ifType = ty
                     , ifIdDetails = details
                     , ifIdInfo = info
                     }
rnIfaceDecl d@IfaceData{} = do
            binders <- mapM rnIfaceTyConBinder (ifBinders d)
            ctxt <- mapM rnIfaceType (ifCtxt d)
            cons <- rnIfaceConDecls (ifCons d)
            parent <- rnIfaceTyConParent (ifParent d)
            return d { ifBinders = binders
                     , ifCtxt = ctxt
                     , ifCons = cons
                     , ifParent = parent
                     }
rnIfaceDecl d@IfaceSynonym{} = do
            binders <- mapM rnIfaceTyConBinder (ifBinders d)
            syn_kind <- rnIfaceType (ifResKind d)
            syn_rhs <- rnIfaceType (ifSynRhs d)
            return d { ifBinders = binders
                     , ifResKind = syn_kind
                     , ifSynRhs = syn_rhs
                     }
rnIfaceDecl d@IfaceFamily{} = do
            binders <- mapM rnIfaceTyConBinder (ifBinders d)
            fam_kind <- rnIfaceType (ifResKind d)
            fam_flav <- rnIfaceFamTyConFlav (ifFamFlav d)
            return d { ifBinders = binders
                     , ifResKind = fam_kind
                     , ifFamFlav = fam_flav
                     }
rnIfaceDecl d@IfaceClass{} = do
            ctxt <- mapM rnIfaceType (ifCtxt d)
            binders <- mapM rnIfaceTyConBinder (ifBinders d)
            ats <- mapM rnIfaceAT (ifATs d)
            sigs <- mapM rnIfaceClassOp (ifSigs d)
            return d { ifCtxt = ctxt
                     , ifBinders = binders
                     , ifATs = ats
                     , ifSigs = sigs
                     }
rnIfaceDecl d@IfaceAxiom{} = do
            tycon <- rnIfaceTyCon (ifTyCon d)
            ax_branches <- mapM rnIfaceAxBranch (ifAxBranches d)
            return d { ifTyCon = tycon
                     , ifAxBranches = ax_branches
                     }
rnIfaceDecl d@IfacePatSyn{} =  do
            let rnPat (n, b) = (,) <$> rnIfaceGlobal n <*> pure b
            pat_matcher <- rnPat (ifPatMatcher d)
            pat_builder <- T.traverse rnPat (ifPatBuilder d)
            pat_univ_bndrs <- mapM rnIfaceForAllBndr (ifPatUnivBndrs d)
            pat_ex_bndrs <- mapM rnIfaceForAllBndr (ifPatExBndrs d)
            pat_prov_ctxt <- mapM rnIfaceType (ifPatProvCtxt d)
            pat_req_ctxt <- mapM rnIfaceType (ifPatReqCtxt d)
            pat_args <- mapM rnIfaceType (ifPatArgs d)
            pat_ty <- rnIfaceType (ifPatTy d)
            return d { ifPatMatcher = pat_matcher
                     , ifPatBuilder = pat_builder
                     , ifPatUnivBndrs = pat_univ_bndrs
                     , ifPatExBndrs = pat_ex_bndrs
                     , ifPatProvCtxt = pat_prov_ctxt
                     , ifPatReqCtxt = pat_req_ctxt
                     , ifPatArgs = pat_args
                     , ifPatTy = pat_ty
                     }

rnIfaceFamTyConFlav :: Rename IfaceFamTyConFlav
rnIfaceFamTyConFlav (IfaceClosedSynFamilyTyCon (Just (n, axs)))
    = IfaceClosedSynFamilyTyCon . Just <$> ((,) <$> rnIfaceGlobal n
                                                <*> mapM rnIfaceAxBranch axs)
rnIfaceFamTyConFlav flav = pure flav

rnIfaceAT :: Rename IfaceAT
rnIfaceAT (IfaceAT decl mb_ty)
    = IfaceAT <$> rnIfaceDecl decl <*> T.traverse rnIfaceType mb_ty

rnIfaceTyConParent :: Rename IfaceTyConParent
rnIfaceTyConParent (IfDataInstance n tc args)
    = IfDataInstance <$> rnIfaceGlobal n
                     <*> rnIfaceTyCon tc
                     <*> rnIfaceTcArgs args
rnIfaceTyConParent IfNoParent = pure IfNoParent

rnIfaceConDecls :: Rename IfaceConDecls
rnIfaceConDecls (IfDataTyCon ds b fs)
    = IfDataTyCon <$> mapM rnIfaceConDecl ds
                  <*> return b
                  <*> return fs
rnIfaceConDecls (IfNewTyCon d b fs) = IfNewTyCon <$> rnIfaceConDecl d <*> return b <*> return fs
rnIfaceConDecls (IfAbstractTyCon b) = pure (IfAbstractTyCon b)

rnIfaceConDecl :: Rename IfaceConDecl
rnIfaceConDecl d = do
    con_ex_tvs <- mapM rnIfaceForAllBndr (ifConExTvs d)
    let rnIfConEqSpec (n,t) = (,) n <$> rnIfaceType t
    con_eq_spec <- mapM rnIfConEqSpec (ifConEqSpec d)
    con_ctxt <- mapM rnIfaceType (ifConCtxt d)
    con_arg_tys <- mapM rnIfaceType (ifConArgTys d)
    let rnIfaceBang (IfUnpackCo co) = IfUnpackCo <$> rnIfaceCo co
        rnIfaceBang bang = pure bang
    con_stricts <- mapM rnIfaceBang (ifConStricts d)
    return d { ifConExTvs = con_ex_tvs
             , ifConEqSpec = con_eq_spec
             , ifConCtxt = con_ctxt
             , ifConArgTys = con_arg_tys
             , ifConStricts = con_stricts
             }

rnIfaceClassOp :: Rename IfaceClassOp
rnIfaceClassOp (IfaceClassOp n ty dm) = IfaceClassOp n <$> rnIfaceType ty <*> rnMaybeDefMethSpec dm

rnMaybeDefMethSpec :: Rename (Maybe (DefMethSpec IfaceType))
rnMaybeDefMethSpec (Just (GenericDM ty)) = Just . GenericDM <$> rnIfaceType ty
rnMaybeDefMethSpec mb = return mb

rnIfaceAxBranch :: Rename IfaceAxBranch
rnIfaceAxBranch d = do
    ty_vars <- mapM rnIfaceTvBndr (ifaxbTyVars d)
    lhs <- rnIfaceTcArgs (ifaxbLHS d)
    rhs <- rnIfaceType (ifaxbRHS d)
    return d { ifaxbTyVars = ty_vars
             , ifaxbLHS = lhs
             , ifaxbRHS = rhs }

rnIfaceIdInfo :: Rename IfaceIdInfo
rnIfaceIdInfo NoInfo = pure NoInfo
rnIfaceIdInfo (HasInfo is) = HasInfo <$> mapM rnIfaceInfoItem is

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
rnIfaceBndr (IfaceIdBndr (fs, ty)) = IfaceIdBndr <$> ((,) fs <$> rnIfaceType ty)
rnIfaceBndr (IfaceTvBndr tv_bndr) = IfaceIdBndr <$> rnIfaceTvBndr tv_bndr

rnIfaceTvBndr :: Rename IfaceTvBndr
rnIfaceTvBndr (fs, kind) = (,) fs <$> rnIfaceType kind

rnIfaceTyConBinder :: Rename IfaceTyConBinder
rnIfaceTyConBinder (TvBndr tv vis) = TvBndr <$> rnIfaceTvBndr tv <*> pure vis

rnIfaceAlt :: Rename IfaceAlt
rnIfaceAlt (conalt, names, rhs)
     = (,,) <$> rnIfaceConAlt conalt <*> pure names <*> rnIfaceExpr rhs

rnIfaceConAlt :: Rename IfaceConAlt
rnIfaceConAlt (IfaceDataAlt data_occ) = IfaceDataAlt <$> rnIfaceGlobal data_occ
rnIfaceConAlt alt = pure alt

rnIfaceLetBndr :: Rename IfaceLetBndr
rnIfaceLetBndr (IfLetBndr fs ty info)
    = IfLetBndr fs <$> rnIfaceType ty <*> rnIfaceIdInfo info

rnIfaceLamBndr :: Rename IfaceLamBndr
rnIfaceLamBndr (bndr, oneshot) = (,) <$> rnIfaceBndr bndr <*> pure oneshot

rnIfaceCo :: Rename IfaceCoercion
rnIfaceCo (IfaceReflCo role ty) = IfaceReflCo role <$> rnIfaceType ty
rnIfaceCo (IfaceFunCo role co1 co2)
    = IfaceFunCo role <$> rnIfaceCo co1 <*> rnIfaceCo co2
rnIfaceCo (IfaceTyConAppCo role tc cos)
    = IfaceTyConAppCo role <$> rnIfaceTyCon tc <*> mapM rnIfaceCo cos
rnIfaceCo (IfaceAppCo co1 co2)
    = IfaceAppCo <$> rnIfaceCo co1 <*> rnIfaceCo co2
rnIfaceCo (IfaceForAllCo bndr co1 co2)
    = IfaceForAllCo <$> rnIfaceTvBndr bndr <*> rnIfaceCo co1 <*> rnIfaceCo co2
rnIfaceCo (IfaceCoVarCo lcl) = IfaceCoVarCo <$> pure lcl
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
rnIfaceCo (IfaceCoherenceCo c1 c2) = IfaceCoherenceCo <$> rnIfaceCo c1 <*> rnIfaceCo c2

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
rnIfaceType (IfaceTyVar n) = pure (IfaceTyVar n)
rnIfaceType (IfaceAppTy t1 t2)
    = IfaceAppTy <$> rnIfaceType t1 <*> rnIfaceType t2
rnIfaceType (IfaceLitTy l)         = return (IfaceLitTy l)
rnIfaceType (IfaceFunTy t1 t2)
    = IfaceFunTy <$> rnIfaceType t1 <*> rnIfaceType t2
rnIfaceType (IfaceDFunTy t1 t2)
    = IfaceDFunTy <$> rnIfaceType t1 <*> rnIfaceType t2
rnIfaceType (IfaceTupleTy s i tks)
    = IfaceTupleTy s i <$> rnIfaceTcArgs tks
rnIfaceType (IfaceTyConApp tc tks)
    = IfaceTyConApp <$> rnIfaceTyCon tc <*> rnIfaceTcArgs tks
rnIfaceType (IfaceForAllTy tv t)
    = IfaceForAllTy <$> rnIfaceForAllBndr tv <*> rnIfaceType t
rnIfaceType (IfaceCoercionTy co)
    = IfaceCoercionTy <$> rnIfaceCo co
rnIfaceType (IfaceCastTy ty co)
    = IfaceCastTy <$> rnIfaceType ty <*> rnIfaceCo co

rnIfaceForAllBndr :: Rename IfaceForAllBndr
rnIfaceForAllBndr (TvBndr tv vis) = TvBndr <$> rnIfaceTvBndr tv <*> pure vis

rnIfaceTcArgs :: Rename IfaceTcArgs
rnIfaceTcArgs (ITC_Invis t ts) = ITC_Invis <$> rnIfaceType t <*> rnIfaceTcArgs ts
rnIfaceTcArgs (ITC_Vis t ts) = ITC_Vis <$> rnIfaceType t <*> rnIfaceTcArgs ts
rnIfaceTcArgs ITC_Nil = pure ITC_Nil
