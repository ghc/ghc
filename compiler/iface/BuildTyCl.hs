{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BuildTyCl (
        buildDataCon,
        buildPatSyn,
        TcMethInfo, MethInfo, buildClass,
        mkNewTyConRhs,
        newImplicitBinder, newTyConRepName
    ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Iface.Env
import GHC.Core.FamInstEnv( FamInstEnvs, mkNewTypeCoAxiom )
import TysWiredIn( isCTupleTyConName )
import TysPrim ( voidPrimTy )
import GHC.Core.DataCon
import GHC.Core.PatSyn
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Basic
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Id.Make
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Types.Id
import TcType

import GHC.Types.SrcLoc( SrcSpan, noSrcSpan )
import GHC.Driver.Session
import TcRnMonad
import GHC.Types.Unique.Supply
import Util
import Outputable


mkNewTyConRhs :: Name -> TyCon -> DataCon -> TcRnIf m n AlgTyConRhs
-- ^ Monadic because it makes a Name for the coercion TyCon
--   We pass the Name of the parent TyCon, as well as the TyCon itself,
--   because the latter is part of a knot, whereas the former is not.
mkNewTyConRhs tycon_name tycon con
  = do  { co_tycon_name <- newImplicitBinder tycon_name mkNewTyCoOcc
        ; let nt_ax = mkNewTypeCoAxiom co_tycon_name tycon etad_tvs etad_roles etad_rhs
        ; traceIf (text "mkNewTyConRhs" <+> ppr nt_ax)
        ; return (NewTyCon { data_con    = con,
                             nt_rhs      = rhs_ty,
                             nt_etad_rhs = (etad_tvs, etad_rhs),
                             nt_co       = nt_ax,
                             nt_lev_poly = isKindLevPoly res_kind } ) }
                             -- Coreview looks through newtypes with a Nothing
                             -- for nt_co, or uses explicit coercions otherwise
  where
    tvs      = tyConTyVars tycon
    roles    = tyConRoles tycon
    res_kind = tyConResKind tycon
    con_arg_ty = case dataConRepArgTys con of
                   [arg_ty] -> arg_ty
                   tys -> pprPanic "mkNewTyConRhs" (ppr con <+> ppr tys)
    rhs_ty = substTyWith (dataConUnivTyVars con)
                         (mkTyVarTys tvs) con_arg_ty
        -- Instantiate the newtype's RHS with the
        -- type variables from the tycon
        -- NB: a newtype DataCon has a type that must look like
        --        forall tvs.  <arg-ty> -> T tvs
        -- Note that we *can't* use dataConInstOrigArgTys here because
        -- the newtype arising from   class Foo a => Bar a where {}
        -- has a single argument (Foo a) that is a *type class*, so
        -- dataConInstOrigArgTys returns [].

    etad_tvs   :: [TyVar]  -- Matched lazily, so that mkNewTypeCo can
    etad_roles :: [Role]   -- return a TyCon without pulling on rhs_ty
    etad_rhs   :: Type     -- See Note [Tricky iface loop] in GHC.Iface.Load
    (etad_tvs, etad_roles, etad_rhs) = eta_reduce (reverse tvs) (reverse roles) rhs_ty

    eta_reduce :: [TyVar]       -- Reversed
               -> [Role]        -- also reversed
               -> Type          -- Rhs type
               -> ([TyVar], [Role], Type)  -- Eta-reduced version
                                           -- (tyvars in normal order)
    eta_reduce (a:as) (_:rs) ty | Just (fun, arg) <- splitAppTy_maybe ty,
                                  Just tv <- getTyVar_maybe arg,
                                  tv == a,
                                  not (a `elemVarSet` tyCoVarsOfType fun)
                                = eta_reduce as rs fun
    eta_reduce tvs rs ty = (reverse tvs, reverse rs, ty)

------------------------------------------------------
buildDataCon :: FamInstEnvs
            -> Name
            -> Bool                     -- Declared infix
            -> TyConRepName
            -> [HsSrcBang]
            -> Maybe [HsImplBang]
                -- See Note [Bangs on imported data constructors] in GHC.Types.Id.Make
           -> [FieldLabel]             -- Field labels
           -> [TyVar]                  -- Universals
           -> [TyCoVar]                -- Existentials
           -> [TyVarBinder]            -- User-written 'TyVarBinder's
           -> [EqSpec]                 -- Equality spec
           -> KnotTied ThetaType       -- Does not include the "stupid theta"
                                       -- or the GADT equalities
           -> [KnotTied Type]          -- Arguments
           -> KnotTied Type            -- Result types
           -> KnotTied TyCon           -- Rep tycon
           -> NameEnv ConTag           -- Maps the Name of each DataCon to its
                                       -- ConTag
           -> TcRnIf m n DataCon
-- A wrapper for DataCon.mkDataCon that
--   a) makes the worker Id
--   b) makes the wrapper Id if necessary, including
--      allocating its unique (hence monadic)
buildDataCon fam_envs src_name declared_infix prom_info src_bangs impl_bangs
             field_lbls univ_tvs ex_tvs user_tvbs eq_spec ctxt arg_tys res_ty
             rep_tycon tag_map
  = do  { wrap_name <- newImplicitBinder src_name mkDataConWrapperOcc
        ; work_name <- newImplicitBinder src_name mkDataConWorkerOcc
        -- This last one takes the name of the data constructor in the source
        -- code, which (for Haskell source anyway) will be in the DataName name
        -- space, and puts it into the VarName name space

        ; traceIf (text "buildDataCon 1" <+> ppr src_name)
        ; us <- newUniqueSupply
        ; dflags <- getDynFlags
        ; let stupid_ctxt = mkDataConStupidTheta rep_tycon arg_tys univ_tvs
              tag = lookupNameEnv_NF tag_map src_name
              -- See Note [Constructor tag allocation], fixes #14657
              data_con = mkDataCon src_name declared_infix prom_info
                                   src_bangs field_lbls
                                   univ_tvs ex_tvs user_tvbs eq_spec ctxt
                                   arg_tys res_ty NoRRI rep_tycon tag
                                   stupid_ctxt dc_wrk dc_rep
              dc_wrk = mkDataConWorkId work_name data_con
              dc_rep = initUs_ us (mkDataConRep dflags fam_envs wrap_name
                                                impl_bangs data_con)

        ; traceIf (text "buildDataCon 2" <+> ppr src_name)
        ; return data_con }


-- The stupid context for a data constructor should be limited to
-- the type variables mentioned in the arg_tys
-- ToDo: Or functionally dependent on?
--       This whole stupid theta thing is, well, stupid.
mkDataConStupidTheta :: TyCon -> [Type] -> [TyVar] -> [PredType]
mkDataConStupidTheta tycon arg_tys univ_tvs
  | null stupid_theta = []      -- The common case
  | otherwise         = filter in_arg_tys stupid_theta
  where
    tc_subst     = zipTvSubst (tyConTyVars tycon)
                              (mkTyVarTys univ_tvs)
    stupid_theta = substTheta tc_subst (tyConStupidTheta tycon)
        -- Start by instantiating the master copy of the
        -- stupid theta, taken from the TyCon

    arg_tyvars      = tyCoVarsOfTypes arg_tys
    in_arg_tys pred = tyCoVarsOfType pred `intersectsVarSet` arg_tyvars


------------------------------------------------------
buildPatSyn :: Name -> Bool
            -> (Id,Bool) -> Maybe (Id, Bool)
            -> ([TyVarBinder], ThetaType) -- ^ Univ and req
            -> ([TyVarBinder], ThetaType) -- ^ Ex and prov
            -> [Type]               -- ^ Argument types
            -> Type                 -- ^ Result type
            -> [FieldLabel]         -- ^ Field labels for
                                    --   a record pattern synonym
            -> PatSyn
buildPatSyn src_name declared_infix matcher@(matcher_id,_) builder
            (univ_tvs, req_theta) (ex_tvs, prov_theta) arg_tys
            pat_ty field_labels
  = -- The assertion checks that the matcher is
    -- compatible with the pattern synonym
    ASSERT2((and [ univ_tvs `equalLength` univ_tvs1
                 , ex_tvs `equalLength` ex_tvs1
                 , pat_ty `eqType` substTy subst pat_ty1
                 , prov_theta `eqTypes` substTys subst prov_theta1
                 , req_theta `eqTypes` substTys subst req_theta1
                 , compareArgTys arg_tys (substTys subst arg_tys1)
                 ])
            , (vcat [ ppr univ_tvs <+> twiddle <+> ppr univ_tvs1
                    , ppr ex_tvs <+> twiddle <+> ppr ex_tvs1
                    , ppr pat_ty <+> twiddle <+> ppr pat_ty1
                    , ppr prov_theta <+> twiddle <+> ppr prov_theta1
                    , ppr req_theta <+> twiddle <+> ppr req_theta1
                    , ppr arg_tys <+> twiddle <+> ppr arg_tys1]))
    mkPatSyn src_name declared_infix
             (univ_tvs, req_theta) (ex_tvs, prov_theta)
             arg_tys pat_ty
             matcher builder field_labels
  where
    ((_:_:univ_tvs1), req_theta1, tau) = tcSplitSigmaTy $ idType matcher_id
    ([pat_ty1, cont_sigma, _], _)      = tcSplitFunTys tau
    (ex_tvs1, prov_theta1, cont_tau)   = tcSplitSigmaTy cont_sigma
    (arg_tys1, _) = (tcSplitFunTys cont_tau)
    twiddle = char '~'
    subst = zipTvSubst (univ_tvs1 ++ ex_tvs1)
                       (mkTyVarTys (binderVars (univ_tvs ++ ex_tvs)))

    -- For a nullary pattern synonym we add a single void argument to the
    -- matcher to preserve laziness in the case of unlifted types.
    -- See #12746
    compareArgTys :: [Type] -> [Type] -> Bool
    compareArgTys [] [x] = x `eqType` voidPrimTy
    compareArgTys arg_tys matcher_arg_tys = arg_tys `eqTypes` matcher_arg_tys


------------------------------------------------------
type TcMethInfo = MethInfo  -- this variant needs zonking
type MethInfo       -- A temporary intermediate, to communicate
                    -- between tcClassSigs and buildClass.
  = ( Name   -- Name of the class op
    , Type   -- Type of the class op
    , Maybe (DefMethSpec (SrcSpan, Type)))
         -- Nothing                    => no default method
         --
         -- Just VanillaDM             => There is an ordinary
         --                               polymorphic default method
         --
         -- Just (GenericDM (loc, ty)) => There is a generic default metho
         --                               Here is its type, and the location
         --                               of the type signature
         --    We need that location /only/ to attach it to the
         --    generic default method's Name; and we need /that/
         --    only to give the right location of an ambiguity error
         --    for the generic default method, spat out by checkValidClass

buildClass :: Name  -- Name of the class/tycon (they have the same Name)
           -> [TyConBinder]                -- Of the tycon
           -> [Role]
           -> [FunDep TyVar]               -- Functional dependencies
           -- Super classes, associated types, method info, minimal complete def.
           -- This is Nothing if the class is abstract.
           -> Maybe (KnotTied ThetaType, [ClassATItem], [KnotTied MethInfo], ClassMinimalDef)
           -> TcRnIf m n Class

buildClass tycon_name binders roles fds Nothing
  = fixM  $ \ rec_clas ->       -- Only name generation inside loop
    do  { traceIf (text "buildClass")

        ; tc_rep_name  <- newTyConRepName tycon_name
        ; let univ_tvs = binderVars binders
              tycon = mkClassTyCon tycon_name binders roles
                                   AbstractTyCon rec_clas tc_rep_name
              result = mkAbstractClass tycon_name univ_tvs fds tycon
        ; traceIf (text "buildClass" <+> ppr tycon)
        ; return result }

buildClass tycon_name binders roles fds
           (Just (sc_theta, at_items, sig_stuff, mindef))
  = fixM  $ \ rec_clas ->       -- Only name generation inside loop
    do  { traceIf (text "buildClass")

        ; datacon_name <- newImplicitBinder tycon_name mkClassDataConOcc
        ; tc_rep_name  <- newTyConRepName tycon_name

        ; op_items <- mapM (mk_op_item rec_clas) sig_stuff
                        -- Build the selector id and default method id

              -- Make selectors for the superclasses
        ; sc_sel_names <- mapM  (newImplicitBinder tycon_name . mkSuperDictSelOcc)
                                (takeList sc_theta [fIRST_TAG..])
        ; let sc_sel_ids = [ mkDictSelId sc_name rec_clas
                           | sc_name <- sc_sel_names]
              -- We number off the Dict superclass selectors, 1, 2, 3 etc so that we
              -- can construct names for the selectors. Thus
              --      class (C a, C b) => D a b where ...
              -- gives superclass selectors
              --      D_sc1, D_sc2
              -- (We used to call them D_C, but now we can have two different
              --  superclasses both called C!)

        ; let use_newtype = isSingleton arg_tys
                -- Use a newtype if the data constructor
                --   (a) has exactly one value field
                --       i.e. exactly one operation or superclass taken together
                --   (b) that value is of lifted type (which they always are, because
                --       we box equality superclasses)
                -- See note [Class newtypes and equality predicates]

                -- We treat the dictionary superclasses as ordinary arguments.
                -- That means that in the case of
                --     class C a => D a
                -- we don't get a newtype with no arguments!
              args       = sc_sel_names ++ op_names
              op_tys     = [ty | (_,ty,_) <- sig_stuff]
              op_names   = [op | (op,_,_) <- sig_stuff]
              arg_tys    = sc_theta ++ op_tys
              rec_tycon  = classTyCon rec_clas
              univ_bndrs = tyConTyVarBinders binders
              univ_tvs   = binderVars univ_bndrs

        ; rep_nm   <- newTyConRepName datacon_name
        ; dict_con <- buildDataCon (panic "buildClass: FamInstEnvs")
                                   datacon_name
                                   False        -- Not declared infix
                                   rep_nm
                                   (map (const no_bang) args)
                                   (Just (map (const HsLazy) args))
                                   [{- No fields -}]
                                   univ_tvs
                                   [{- no existentials -}]
                                   univ_bndrs
                                   [{- No GADT equalities -}]
                                   [{- No theta -}]
                                   arg_tys
                                   (mkTyConApp rec_tycon (mkTyVarTys univ_tvs))
                                   rec_tycon
                                   (mkTyConTagMap rec_tycon)

        ; rhs <- case () of
                  _ | use_newtype
                    -> mkNewTyConRhs tycon_name rec_tycon dict_con
                    | isCTupleTyConName tycon_name
                    -> return (TupleTyCon { data_con = dict_con
                                          , tup_sort = ConstraintTuple })
                    | otherwise
                    -> return (mkDataTyConRhs [dict_con])

        ; let { tycon = mkClassTyCon tycon_name binders roles
                                     rhs rec_clas tc_rep_name
                -- A class can be recursive, and in the case of newtypes
                -- this matters.  For example
                --      class C a where { op :: C b => a -> b -> Int }
                -- Because C has only one operation, it is represented by
                -- a newtype, and it should be a *recursive* newtype.
                -- [If we don't make it a recursive newtype, we'll expand the
                -- newtype like a synonym, but that will lead to an infinite
                -- type]

              ; result = mkClass tycon_name univ_tvs fds
                                 sc_theta sc_sel_ids at_items
                                 op_items mindef tycon
              }
        ; traceIf (text "buildClass" <+> ppr tycon)
        ; return result }
  where
    no_bang = HsSrcBang NoSourceText NoSrcUnpack NoSrcStrict

    mk_op_item :: Class -> TcMethInfo -> TcRnIf n m ClassOpItem
    mk_op_item rec_clas (op_name, _, dm_spec)
      = do { dm_info <- mk_dm_info op_name dm_spec
           ; return (mkDictSelId op_name rec_clas, dm_info) }

    mk_dm_info :: Name -> Maybe (DefMethSpec (SrcSpan, Type))
               -> TcRnIf n m (Maybe (Name, DefMethSpec Type))
    mk_dm_info _ Nothing
      = return Nothing
    mk_dm_info op_name (Just VanillaDM)
      = do { dm_name <- newImplicitBinder op_name mkDefaultMethodOcc
           ; return (Just (dm_name, VanillaDM)) }
    mk_dm_info op_name (Just (GenericDM (loc, dm_ty)))
      = do { dm_name <- newImplicitBinderLoc op_name mkDefaultMethodOcc loc
           ; return (Just (dm_name, GenericDM dm_ty)) }

{-
Note [Class newtypes and equality predicates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        class (a ~ F b) => C a b where
          op :: a -> b

We cannot represent this by a newtype, even though it's not
existential, because there are two value fields (the equality
predicate and op. See #2238

Moreover,
          class (a ~ F b) => C a b where {}
Here we can't use a newtype either, even though there is only
one field, because equality predicates are unboxed, and classes
are boxed.
-}

newImplicitBinder :: Name                       -- Base name
                  -> (OccName -> OccName)       -- Occurrence name modifier
                  -> TcRnIf m n Name            -- Implicit name
-- Called in BuildTyCl to allocate the implicit binders of type/class decls
-- For source type/class decls, this is the first occurrence
-- For iface ones, GHC.Iface.Load has already allocated a suitable name in the cache
newImplicitBinder base_name mk_sys_occ
  = newImplicitBinderLoc base_name mk_sys_occ (nameSrcSpan base_name)

newImplicitBinderLoc :: Name                       -- Base name
                     -> (OccName -> OccName)       -- Occurrence name modifier
                     -> SrcSpan
                     -> TcRnIf m n Name            -- Implicit name
-- Just the same, but lets you specify the SrcSpan
newImplicitBinderLoc base_name mk_sys_occ loc
  | Just mod <- nameModule_maybe base_name
  = newGlobalBinder mod occ loc
  | otherwise           -- When typechecking a [d| decl bracket |],
                        -- TH generates types, classes etc with Internal names,
                        -- so we follow suit for the implicit binders
  = do  { uniq <- newUnique
        ; return (mkInternalName uniq occ loc) }
  where
    occ = mk_sys_occ (nameOccName base_name)

-- | Make the 'TyConRepName' for this 'TyCon'
newTyConRepName :: Name -> TcRnIf gbl lcl TyConRepName
newTyConRepName tc_name
  | Just mod <- nameModule_maybe tc_name
  , (mod, occ) <- tyConRepModOcc mod (nameOccName tc_name)
  = newGlobalBinder mod occ noSrcSpan
  | otherwise
  = newImplicitBinder tc_name mkTyConRepOcc
