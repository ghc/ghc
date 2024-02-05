
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE LambdaCase #-}

{-
(c) The University of Glasgow 2006-2008
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998
-}

-- | Module for constructing interface declaration values
-- from the corresponding 'TyThing's.

module GHC.Iface.Decl
   ( coAxiomToIfaceDecl
   , tyThingToIfaceDecl -- Converting things to their Iface equivalents
   )
where

import GHC.Prelude

import GHC.Tc.Utils.TcType

import GHC.Iface.Syntax

import GHC.CoreToIface

import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Core.Coercion.Axiom
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Core.TyCo.Tidy

import GHC.Types.Id
import GHC.Types.Var.Env
import GHC.Types.Var
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Types.TyThing

import GHC.Utils.Panic.Plain
import GHC.Utils.Misc

import GHC.Data.Maybe
import Data.List ( findIndex, mapAccumL )

{-
************************************************************************
*                                                                      *
                Converting things to their Iface equivalents
*                                                                      *
************************************************************************
-}

tyThingToIfaceDecl :: Bool -> TyThing -> IfaceDecl
tyThingToIfaceDecl _ (AnId id)      = idToIfaceDecl id
tyThingToIfaceDecl _ (ATyCon tycon) = snd (tyConToIfaceDecl emptyTidyEnv tycon)
tyThingToIfaceDecl _ (ACoAxiom ax)  = coAxiomToIfaceDecl ax
tyThingToIfaceDecl show_linear_types (AConLike cl)  = case cl of
    RealDataCon dc -> dataConToIfaceDecl show_linear_types dc -- for ppr purposes only
    PatSynCon ps   -> patSynToIfaceDecl ps

--------------------------
idToIfaceDecl :: Id -> IfaceDecl
-- The Id is already tidied, so that locally-bound names
-- (lambdas, for-alls) already have non-clashing OccNames
-- We can't tidy it here, locally, because it may have
-- free variables in its type or IdInfo
idToIfaceDecl id
  = IfaceId { ifName      = getName id,
              ifType      = toIfaceType (idType id),
              ifIdDetails = toIfaceIdDetails (idDetails id),
              ifIdInfo    = toIfaceIdInfo (idInfo id) }

--------------------------
dataConToIfaceDecl :: Bool -> DataCon -> IfaceDecl
dataConToIfaceDecl show_linear_types dataCon
  = IfaceId { ifName      = getName dataCon,
              ifType      = toIfaceType (dataConDisplayType show_linear_types dataCon),
              ifIdDetails = IfVanillaId,
              ifIdInfo    = [] }

--------------------------
coAxiomToIfaceDecl :: CoAxiom br -> IfaceDecl
-- We *do* tidy Axioms, because they are not (and cannot
-- conveniently be) built in tidy form
coAxiomToIfaceDecl ax@(CoAxiom { co_ax_tc = tycon, co_ax_branches = branches
                               , co_ax_role = role })
 = IfaceAxiom { ifName       = getName ax
              , ifTyCon      = toIfaceTyCon tycon
              , ifRole       = role
              , ifAxBranches = map (coAxBranchToIfaceBranch tycon
                                     (map coAxBranchLHS branch_list))
                                   branch_list }
 where
   branch_list = fromBranches branches

-- 2nd parameter is the list of branch LHSs, in case of a closed type family,
-- for conversion from incompatible branches to incompatible indices.
-- For an open type family the list should be empty.
-- See Note [Storing compatibility] in GHC.Core.Coercion.Axiom
coAxBranchToIfaceBranch :: TyCon -> [[Type]] -> CoAxBranch -> IfaceAxBranch
coAxBranchToIfaceBranch tc lhs_s
                        (CoAxBranch { cab_tvs = tvs, cab_cvs = cvs
                                    , cab_eta_tvs = eta_tvs
                                    , cab_lhs = lhs, cab_roles = roles
                                    , cab_rhs = rhs, cab_incomps = incomps })

  = IfaceAxBranch { ifaxbTyVars  = toIfaceTvBndrs tvs
                  , ifaxbCoVars  = map toIfaceIdBndr cvs
                  , ifaxbEtaTyVars = toIfaceTvBndrs eta_tvs
                  , ifaxbLHS     = toIfaceTcArgs tc lhs
                  , ifaxbRoles   = roles
                  , ifaxbRHS     = toIfaceType rhs
                  , ifaxbIncomps = iface_incomps }
  where
    iface_incomps = map (expectJust
                        . flip findIndex lhs_s
                        . eqTypes
                        . coAxBranchLHS) incomps

-----------------
tyConToIfaceDecl :: TidyEnv -> TyCon -> (TidyEnv, IfaceDecl)
-- We *do* tidy TyCons, because they are not (and cannot
-- conveniently be) built in tidy form
-- The returned TidyEnv is the one after tidying the tyConTyVars
tyConToIfaceDecl env tycon
  | Just clas <- tyConClass_maybe tycon
  = classToIfaceDecl env clas

  | Just syn_rhs <- synTyConRhs_maybe tycon
  = ( tc_env1
    , IfaceSynonym { ifName    = getName tycon,
                     ifRoles   = tyConRoles tycon,
                     ifSynRhs  = if_syn_type syn_rhs,
                     ifBinders = if_binders,
                     ifResKind = if_res_kind
                   })

  | Just fam_flav <- famTyConFlav_maybe tycon
  = ( tc_env1
    , IfaceFamily { ifName    = getName tycon,
                    ifResVar  = mkIfLclName <$> if_res_var,
                    ifFamFlav = to_if_fam_flav fam_flav,
                    ifBinders = if_binders,
                    ifResKind = if_res_kind,
                    ifFamInj  = tyConInjectivityInfo tycon
                  })

  | isAlgTyCon tycon
  = ( tc_env1
    , IfaceData { ifName    = getName tycon,
                  ifBinders = if_binders,
                  ifResKind = if_res_kind,
                  ifCType   = tyConCType_maybe tycon,
                  ifRoles   = tyConRoles tycon,
                  ifCtxt    = tidyToIfaceContext tc_env1 (tyConStupidTheta tycon),
                  ifCons    = ifaceConDecls (algTyConRhs tycon),
                  ifGadtSyntax = isGadtSyntaxTyCon tycon,
                  ifParent  = parent })

  | otherwise  -- FunTyCon, PrimTyCon, promoted TyCon/DataCon
  -- We only convert these TyCons to IfaceTyCons when we are
  -- just about to pretty-print them, not because we are going
  -- to put them into interface files
  = ( env
    , IfaceData { ifName       = getName tycon,
                  ifBinders    = if_binders,
                  ifResKind    = if_res_kind,
                  ifCType      = Nothing,
                  ifRoles      = tyConRoles tycon,
                  ifCtxt       = [],
                  ifCons       = IfDataTyCon False [],
                  ifGadtSyntax = False,
                  ifParent     = IfNoParent })
  where
    -- NOTE: Not all TyCons have `tyConTyVars` field. Forcing this when `tycon`
    -- is one of these TyCons (FunTyCon, PrimTyCon, PromotedDataCon) will cause
    -- an error.
    (tc_env1, tc_binders) = tidyTyConBinders env (tyConBinders tycon)
    tc_tyvars      = binderVars tc_binders
    if_binders     = toIfaceForAllBndrs tc_binders
                     -- No tidying of the binders; they are already tidy
    if_res_kind    = tidyToIfaceType tc_env1 (tyConResKind tycon)
    if_syn_type ty = tidyToIfaceType tc_env1 ty
    if_res_var     = getOccFS `fmap` tyConFamilyResVar_maybe tycon

    parent = case tyConFamInstSig_maybe tycon of
               Just (tc, ty, ax) -> IfDataInstance (coAxiomName ax)
                                                   (toIfaceTyCon tc)
                                                   (tidyToIfaceTcArgs tc_env1 tc ty)
               Nothing           -> IfNoParent

    to_if_fam_flav OpenSynFamilyTyCon             = IfaceOpenSynFamilyTyCon
    to_if_fam_flav AbstractClosedSynFamilyTyCon   = IfaceAbstractClosedSynFamilyTyCon
    to_if_fam_flav (DataFamilyTyCon {})           = IfaceDataFamilyTyCon
    to_if_fam_flav (BuiltInSynFamTyCon {})        = IfaceBuiltInSynFamTyCon
    to_if_fam_flav (ClosedSynFamilyTyCon Nothing) = IfaceClosedSynFamilyTyCon Nothing
    to_if_fam_flav (ClosedSynFamilyTyCon (Just ax))
      = IfaceClosedSynFamilyTyCon (Just (axn, ibr))
      where defs = fromBranches $ coAxiomBranches ax
            lhss = map coAxBranchLHS defs
            ibr  = map (coAxBranchToIfaceBranch tycon lhss) defs
            axn  = coAxiomName ax

    ifaceConDecls (NewTyCon { data_con = con })    = IfNewTyCon  (ifaceConDecl con)
    ifaceConDecls (DataTyCon { data_cons = cons, is_type_data = type_data })
      = IfDataTyCon type_data (map ifaceConDecl cons)
    ifaceConDecls (TupleTyCon { data_con = con })  = IfDataTyCon False [ifaceConDecl con]
    ifaceConDecls (SumTyCon { data_cons = cons })  = IfDataTyCon False (map ifaceConDecl cons)
    ifaceConDecls AbstractTyCon                    = IfAbstractTyCon
        -- The AbstractTyCon case happens when a TyCon has been trimmed
        -- during tidying.
        -- Furthermore, tyThingToIfaceDecl is also used in GHC.Tc.Module
        -- for GHCi, when browsing a module, in which case the
        -- AbstractTyCon and TupleTyCon cases are perfectly sensible.
        -- (Tuple declarations are not serialised into interface files.)

    ifaceConDecl data_con
        = IfCon   { ifConName    = dataConName data_con,
                    ifConInfix   = dataConIsInfix data_con,
                    ifConWrapper = isJust (dataConWrapId_maybe data_con),
                    ifConExTCvs  = map toIfaceBndr ex_tvs',
                    ifConUserTvBinders = toIfaceForAllBndrs user_bndrs',
                    ifConEqSpec  = map (to_eq_spec . eqSpecPair) eq_spec,
                    ifConCtxt    = tidyToIfaceContext con_env2 theta,
                    ifConArgTys  =
                      map (\(Scaled w t) -> (tidyToIfaceType con_env2 w
                                          , (tidyToIfaceType con_env2 t))) arg_tys,
                    ifConFields  = dataConFieldLabels data_con,
                    ifConStricts = map (toIfaceBang con_env2)
                                       (dataConImplBangs data_con),
                    ifConSrcStricts = map toIfaceSrcBang
                                          (dataConSrcBangs data_con)}
        where
          (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _)
            = dataConFullSig data_con
          user_bndrs = dataConUserTyVarBinders data_con

          -- Tidy the univ_tvs of the data constructor to be identical
          -- to the tyConTyVars of the type constructor.  This means
          -- (a) we don't need to redundantly put them into the interface file
          -- (b) when pretty-printing an Iface data declaration in H98-style syntax,
          --     we know that the type variables will line up
          -- The latter (b) is important because we pretty-print type constructors
          -- by converting to Iface syntax and pretty-printing that
          con_env1 = (fst tc_env1, mkVarEnv (zipEqual univ_tvs tc_tyvars))
                     -- A bit grimy, perhaps, but it's simple!

          (con_env2, ex_tvs') = tidyVarBndrs con_env1 ex_tvs
          user_bndrs' = map (tidyUserForAllTyBinder con_env2) user_bndrs
          to_eq_spec (tv,ty) = (tidyTyVar con_env2 tv, tidyToIfaceType con_env2 ty)

          -- By this point, we have tidied every universal and existential
          -- tyvar. Because of the dcUserForAllTyBinders invariant
          -- (see Note [DataCon user type variable binders]), *every*
          -- user-written tyvar must be contained in the substitution that
          -- tidying produced. Therefore, tidying the user-written tyvars is a
          -- simple matter of looking up each variable in the substitution,
          -- which tidyTyCoVarOcc accomplishes.
          tidyUserForAllTyBinder :: TidyEnv -> InvisTVBinder -> InvisTVBinder
          tidyUserForAllTyBinder env (Bndr tv vis) =
            Bndr (tidyTyCoVarOcc env tv) vis

classToIfaceDecl :: TidyEnv -> Class -> (TidyEnv, IfaceDecl)
classToIfaceDecl env clas
  = ( env1
    , IfaceClass { ifName   = getName tycon,
                   ifRoles  = tyConRoles (classTyCon clas),
                   ifBinders = toIfaceForAllBndrs tc_binders,
                   ifBody   = body,
                   ifFDs    = map toIfaceFD clas_fds })
  where
    (_, clas_fds, sc_theta, _, clas_ats, op_stuff)
      = classExtraBigSig clas
    tycon = classTyCon clas

    body | isAbstractTyCon tycon = IfAbstractClass
         | otherwise
         = IfConcreteClass {
                ifClassCtxt   = tidyToIfaceContext env1 sc_theta,
                ifATs    = map toIfaceAT clas_ats,
                ifSigs   = map toIfaceClassOp op_stuff,
                ifMinDef = toIfaceBooleanFormula (classMinimalDef clas)
            }

    (env1, tc_binders) = tidyTyConBinders env (tyConBinders tycon)

    toIfaceAT :: ClassATItem -> IfaceAT
    toIfaceAT (ATI tc def)
      = IfaceAT if_decl (fmap (tidyToIfaceType env2 . fst) def)
      where
        (env2, if_decl) = tyConToIfaceDecl env1 tc

    toIfaceClassOp (sel_id, def_meth)
        = assert (sel_tyvars == binderVars tc_binders) $
          IfaceClassOp (getName sel_id)
                       (tidyToIfaceType env1 op_ty)
                       (fmap toDmSpec def_meth)
        where
                -- Be careful when splitting the type, because of things
                -- like         class Foo a where
                --                op :: (?x :: String) => a -> a
                -- and          class Baz a where
                --                op :: (Ord a) => a -> a
          (sel_tyvars, rho_ty) = splitForAllTyCoVars (idType sel_id)
          op_ty                = funResultTy rho_ty

    toDmSpec :: (Name, DefMethSpec Type) -> DefMethSpec IfaceType
    toDmSpec (_, VanillaDM)       = VanillaDM
    toDmSpec (_, GenericDM dm_ty) = GenericDM (tidyToIfaceType env1 dm_ty)

    toIfaceFD (tvs1, tvs2) = (map (tidyTyVar env1) tvs1
                             ,map (tidyTyVar env1) tvs2)

--------------------------

tidyTyConBinder :: TidyEnv -> TyConBinder -> (TidyEnv, TyConBinder)
-- If the type variable "binder" is in scope, don't re-bind it
-- In a class decl, for example, the ATD binders mention
-- (amd must mention) the class tyvars
tidyTyConBinder env@(_, subst) tvb@(Bndr tv vis)
 = case lookupVarEnv subst tv of
     Just tv' -> (env,  Bndr tv' vis)
     Nothing  -> tidyForAllTyBinder env tvb

tidyTyConBinders :: TidyEnv -> [TyConBinder] -> (TidyEnv, [TyConBinder])
tidyTyConBinders = mapAccumL tidyTyConBinder

tidyTyVar :: TidyEnv -> TyVar -> IfLclName
tidyTyVar (_, subst) tv = toIfaceTyVar (lookupVarEnv subst tv `orElse` tv)
