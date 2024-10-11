{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}


{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Typechecking class declarations
module GHC.Tc.TyCl.Class
   ( tcClassSigs
   , tcClassDecl2
   , findMethodBind
   , instantiateMethod
   , tcClassMinimalDef
   , HsSigFun
   , mkHsSigFun
   , instDeclCtxt1
   , instDeclCtxt2
   , instDeclCtxt3
   , tcATDefault
   , substATBndrs
   )
where

import GHC.Prelude

import GHC.Hs
import GHC.Tc.Errors.Types
import GHC.Tc.Gen.Sig
import GHC.Tc.Types.Evidence ( idHsWrapper )
import GHC.Tc.Gen.Bind
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.Instantiate( newFamInst, tcSuperSkolTyVars )
import GHC.Tc.Gen.HsType
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Monad
import GHC.Tc.TyCl.Build( TcMethInfo )

import GHC.Core.Type     ( extendTvSubstWithClone, piResultTys )
import GHC.Core.Predicate
import GHC.Core.Multiplicity
import GHC.Core.Class
import GHC.Core.Coercion ( pprCoAxiom )
import GHC.Core.FamInstEnv
import GHC.Core.TyCon

import GHC.Driver.DynFlags

import GHC.Types.Error
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Var
import GHC.Types.Var.Env ( lookupVarEnv )
import GHC.Types.SourceFile (HscSource(..))
import GHC.Types.SrcLoc
import GHC.Types.Basic

import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Data.Maybe
import GHC.Data.BooleanFormula

import Control.Monad
import Data.List ( mapAccumL, partition )
import qualified Data.List.NonEmpty as NE

{-
Dictionary handling
~~~~~~~~~~~~~~~~~~~
Every class implicitly declares a new data type, corresponding to dictionaries
of that class. So, for example:

        class (D a) => C a where
          op1 :: a -> a
          op2 :: forall b. Ord b => a -> b -> b

would implicitly declare

        data CDict a = CDict (D a)
                             (a -> a)
                             (forall b. Ord b => a -> b -> b)

(We could use a record decl, but that means changing more of the existing apparatus.
One step at a time!)

For classes with just one superclass+method, we use a newtype decl instead:

        class C a where
          op :: forallb. a -> b -> b

generates

        newtype CDict a = CDict (forall b. a -> b -> b)

Now DictTy in Type is just a form of type synonym:
        DictTy c t = TyConTy CDict `AppTy` t

Death to "ExpandingDicts".


************************************************************************
*                                                                      *
                Type-checking the class op signatures
*                                                                      *
************************************************************************
-}

tcClassSigs :: Name                -- Name of the class
            -> [LSig GhcRn]
            -> LHsBinds GhcRn
            -> TcM [TcMethInfo]    -- Exactly one for each method
tcClassSigs clas sigs def_methods
  = do { traceTc "tcClassSigs 1" (ppr clas)

       ; gen_dm_prs <- concatMapM (addLocM tc_gen_sig) gen_sigs
       ; let gen_dm_env :: NameEnv (SrcSpan, Type)
             gen_dm_env = mkNameEnv gen_dm_prs

       ; op_info <- concatMapM (addLocM (tc_sig gen_dm_env)) vanilla_sigs

       ; let op_names = mkNameSet [ n | (n,_,_) <- op_info ]
       ; sequence_ [ failWithTc (TcRnBadMethodErr clas n)
                   | n <- dm_bind_names, not (n `elemNameSet` op_names) ]
                   -- Value binding for non class-method (ie no TypeSig)

       ; tcg_env <- getGblEnv
       ; if tcg_src tcg_env == HsigFile
            then
               -- Error if we have value bindings
               -- (Generic signatures without value bindings indicate
               -- that a default of this form is expected to be
               -- provided.)
               case def_methods of
                 []           -> return ()
                 meth : meths -> failWithTc (TcRnIllegalHsigDefaultMethods clas (meth NE.:| meths))
            else
               -- Error for each generic signature without value binding
               sequence_ [ failWithTc (TcRnBadGenericMethod clas n)
                         | (n,_) <- gen_dm_prs, not (n `elem` dm_bind_names) ]

       ; traceTc "tcClassSigs 2" (ppr clas)
       ; return op_info }
  where
    vanilla_sigs :: [Located ([LocatedN Name], LHsSigType GhcRn)] -- AZ temp
    vanilla_sigs = [L (locA loc) (nm,ty) | L loc (ClassOpSig _ False nm ty) <- sigs]
    gen_sigs :: [Located ([LocatedN Name], LHsSigType GhcRn)] -- AZ temp
    gen_sigs     = [L (locA loc) (nm,ty) | L loc (ClassOpSig _ True  nm ty) <- sigs]
    dm_bind_names :: [Name] -- These ones have a value binding in the class decl
    dm_bind_names = [op | L _ (FunBind {fun_id = L _ op}) <- def_methods]

    tc_sig :: NameEnv (SrcSpan, Type) -> ([LocatedN Name], LHsSigType GhcRn)
           -> TcM [TcMethInfo]
    tc_sig gen_dm_env (op_names, op_hs_ty)
      = do { traceTc "ClsSig 1" (ppr op_names)
           ; op_ty <- tcClassSigType op_names op_hs_ty
                   -- Class tyvars already in scope

           ; traceTc "ClsSig 2" (ppr op_names $$ ppr op_ty)
           ; return [ (op_name, op_ty, f op_name) | L _ op_name <- op_names ] }
           where
             f nm | Just lty <- lookupNameEnv gen_dm_env nm = Just (GenericDM lty)
                  | nm `elem` dm_bind_names                 = Just VanillaDM
                  | otherwise                               = Nothing

    tc_gen_sig :: ([LocatedN Name], LHsSigType GhcRn)
                      -> IOEnv (Env TcGblEnv TcLclEnv) [(Name, (SrcSpan, Type))] -- AZ temp
    tc_gen_sig (op_names, gen_hs_ty)
      = do { gen_op_ty <- tcClassSigType op_names gen_hs_ty
           ; return [ (op_name, (locA loc, gen_op_ty))
                                                 | L loc op_name <- op_names ] }

{-
************************************************************************
*                                                                      *
                Class Declarations
*                                                                      *
************************************************************************
-}

tcClassDecl2 :: LTyClDecl GhcRn          -- The class declaration
             -> TcM (LHsBinds GhcTc)

tcClassDecl2 (L _ (ClassDecl {tcdLName = class_name, tcdSigs = sigs,
                                tcdMeths = default_binds}))
  = recoverM (return emptyLHsBinds) $
    setSrcSpan (getLocA class_name) $
    do  { clas <- tcLookupLocatedClass (la2la class_name)

        -- We make a separate binding for each default method.
        -- At one time I used a single AbsBinds for all of them, thus
        -- AbsBind [d] [dm1, dm2, dm3] { dm1 = ...; dm2 = ...; dm3 = ... }
        -- But that desugars into
        --      ds = \d -> (..., ..., ...)
        --      dm1 = \d -> case ds d of (a,b,c) -> a
        -- And since ds is big, it doesn't get inlined, so we don't get good
        -- default methods.  Better to make separate AbsBinds for each

        ; skol_info <- mkSkolemInfo (TyConSkol ClassFlavour (getName class_name))
        ; tc_lvl    <- getTcLevel
        ; let (tyvars, _, _, op_items) = classBigSig clas
              prag_fn = mkPragEnv sigs default_binds
              sig_fn  = mkHsSigFun sigs
              (_skol_subst, clas_tyvars) = tcSuperSkolTyVars tc_lvl skol_info tyvars
                    -- This make skolemTcTyVars, but does not clone,
                    -- so we can put them in scope with tcExtendTyVarEnv
              pred = mkClassPred clas (mkTyVarTys clas_tyvars)
        ; this_dict <- newEvVar pred

        ; let tc_item = tcDefMeth clas clas_tyvars this_dict
                                  default_binds sig_fn prag_fn
        ; dm_binds <- tcExtendTyVarEnv clas_tyvars $
                      mapM tc_item op_items

        ; return (concat dm_binds) }

tcClassDecl2 d = pprPanic "tcClassDecl2" (ppr d)

tcDefMeth :: Class -> [TyVar] -> EvVar -> LHsBinds GhcRn
          -> HsSigFun -> TcPragEnv -> ClassOpItem
          -> TcM (LHsBinds GhcTc)
-- Generate code for default methods
-- This is incompatible with Hugs, which expects a polymorphic
-- default method for every class op, regardless of whether or not
-- the programmer supplied an explicit default decl for the class.
-- (If necessary we can fix that, but we don't have a convenient Id to hand.)

tcDefMeth _ _ _ _ _ prag_fn (sel_id, Nothing)
  = do { -- No default method
         mapM_ (addLocM (badDmPrag sel_id ))
               (lookupPragEnv prag_fn (idName sel_id))
       ; return [] }

tcDefMeth clas tyvars this_dict binds_in hs_sig_fn prag_fn
          (sel_id, Just (dm_name, dm_spec))
  | Just (L bind_loc dm_bind, bndr_loc, prags) <- findMethodBind sel_name binds_in prag_fn
  = do { -- First look up the default method; it should be there!
         -- It can be the ordinary default method
         -- or the generic-default method.  E.g of the latter
         --      class C a where
         --        op :: a -> a -> Bool
         --        default op :: Eq a => a -> a -> Bool
         --        op x y = x==y
         -- The default method we generate is
         --    $gm :: (C a, Eq a) => a -> a -> Bool
         --    $gm x y = x==y

         global_dm_id  <- tcLookupId dm_name
       ; global_dm_id  <- addInlinePrags global_dm_id prags
       ; local_dm_name <- newNameAt (getOccName sel_name) bndr_loc
            -- Base the local_dm_name on the selector name, because
            -- type errors from tcInstanceMethodBody come from here

       ; spec_prags <- discardConstraints $
                       tcSpecPrags global_dm_id prags
       ; let dia = TcRnIgnoreSpecialisePragmaOnDefMethod sel_name

       ; diagnosticTc (not (null spec_prags)) dia

       ; let hs_ty = hs_sig_fn sel_name
                     `orElse` pprPanic "tc_dm" (ppr sel_name)
             -- We need the HsType so that we can bring the right
             -- type variables into scope
             --
             -- Eg.   class C a where
             --          op :: forall b. Eq b => a -> [b] -> a
             --          gen_op :: a -> a
             --          generic gen_op :: D a => a -> a
             -- The "local_dm_ty" is precisely the type in the above
             -- type signatures, ie with no "forall a. C a =>" prefix

             local_dm_ty = instantiateMethod clas global_dm_id (mkTyVarTys tyvars)

             lm_bind     = dm_bind { fun_id = L (l2l bind_loc) local_dm_name }
                             -- Substitute the local_meth_name for the binder
                             -- NB: the binding is always a FunBind

             warn_redundant = case dm_spec of
                                GenericDM {} -> lhsSigTypeContextSpan hs_ty
                                VanillaDM    -> NoRRC
                -- For GenericDM, warn if the user specifies a signature
                -- with redundant constraints; but not for VanillaDM, where
                -- the default method may well be 'error' or something

             ctxt = FunSigCtxt sel_name warn_redundant

       ; let local_dm_id = mkLocalId local_dm_name ManyTy local_dm_ty
             local_dm_sig = CSig { sig_bndr = local_dm_id
                                 , sig_ctxt = ctxt
                                 , sig_loc  = getLocA hs_ty }

       ; (ev_binds, (tc_bind, _))
               <- checkConstraints skol_info tyvars [this_dict] $
                  tcPolyCheck no_prag_fn local_dm_sig
                              (L bind_loc lm_bind)

       ; let export = ABE { abe_poly  = global_dm_id
                          , abe_mono  = local_dm_id
                          , abe_wrap  = idHsWrapper
                          , abe_prags = IsDefaultMethod }
             full_bind = XHsBindsLR $
                         AbsBinds { abs_tvs      = tyvars
                                  , abs_ev_vars  = [this_dict]
                                  , abs_exports  = [export]
                                  , abs_ev_binds = [ev_binds]
                                  , abs_binds    = tc_bind
                                  , abs_sig      = True }

       ; return [L bind_loc full_bind] }

  | otherwise = pprPanic "tcDefMeth" (ppr sel_id)
  where
    skol_info = TyConSkol ClassFlavour (getName clas)
    sel_name = idName sel_id
    no_prag_fn = emptyPragEnv   -- No pragmas for local_meth_id;
                                -- they are all for meth_id

---------------
tcClassMinimalDef :: Name -> [LSig GhcRn] -> [TcMethInfo] -> TcM ClassMinimalDef
tcClassMinimalDef _clas sigs op_info
  = case findMinimalDef sigs of
      Nothing -> return defMindef
      Just mindef -> do
        -- Warn if the given mindef does not imply the default one
        -- That is, the given mindef should at least ensure that the
        -- class ops without default methods are required, since we
        -- have no way to fill them in otherwise
        tcg_env <- getGblEnv
        -- However, only do this test when it's not an hsig file,
        -- since you can't write a default implementation.
        when (tcg_src tcg_env /= HsigFile) $
            whenIsJust (isUnsatisfied (mindef `impliesAtom`) defMindef) $
                       (\bf -> addDiagnosticTc (TcRnWarningMinimalDefIncomplete bf))
        return mindef
  where
    -- By default require all methods without a default implementation
    defMindef :: ClassMinimalDef
    defMindef = mkAnd [ mkVar name
                      | (name, _, Nothing) <- op_info ]

instantiateMethod :: Class -> TcId -> [TcType] -> TcType
-- Take a class operation, say
--      op :: forall ab. C a => forall c. Ix c => (b,c) -> a
-- Instantiate it at [ty1,ty2]
-- Return the "local method type":
--      forall c. Ix x => (ty2,c) -> ty1
instantiateMethod clas sel_id inst_tys
  = assert ok_first_pred local_meth_ty
  where
    rho_ty = piResultTys (idType sel_id) inst_tys
    (first_pred, local_meth_ty) = tcSplitPredFunTy_maybe rho_ty
                `orElse` pprPanic "tcInstanceMethod" (ppr sel_id)

    ok_first_pred = case getClassPredTys_maybe first_pred of
                      Just (clas1, _tys) -> clas == clas1
                      Nothing -> False
              -- The first predicate should be of form (C a b)
              -- where C is the class in question


---------------------------
type HsSigFun = Name -> Maybe (LHsSigType GhcRn)

mkHsSigFun :: [LSig GhcRn] -> HsSigFun
mkHsSigFun sigs = lookupNameEnv env
  where
    env = mkHsSigEnv get_classop_sig sigs

    get_classop_sig :: LSig GhcRn -> Maybe ([LocatedN Name], LHsSigType GhcRn)
    get_classop_sig  (L _ (ClassOpSig _ _ ns hs_ty)) = Just (ns, hs_ty)
    get_classop_sig  _                               = Nothing

---------------------------
findMethodBind  :: Name                 -- Selector
                -> LHsBinds GhcRn       -- A group of bindings
                -> TcPragEnv
                -> Maybe (LHsBind GhcRn, SrcSpan, [LSig GhcRn])
                -- Returns the binding, the binding
                -- site of the method binder, and any inline or
                -- specialisation pragmas
findMethodBind sel_name binds prag_fn
  = foldl' mplus Nothing (map f binds)
  where
    prags    = lookupPragEnv prag_fn sel_name

    f bind@(L _ (FunBind { fun_id = L bndr_loc op_name }))
      | op_name == sel_name
             = Just (bind, locA bndr_loc, prags)
    f _other = Nothing

---------------------------
findMinimalDef :: [LSig GhcRn] -> Maybe ClassMinimalDef
findMinimalDef = firstJusts . map toMinimalDef
  where
    toMinimalDef :: LSig GhcRn -> Maybe ClassMinimalDef
    toMinimalDef (L _ (MinimalSig _ bf)) = Just $ fmap unLoc bf
    toMinimalDef _                       = Nothing

{-
Note [Polymorphic methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    class Foo a where
        op :: forall b. Ord b => a -> b -> b -> b
    instance Foo c => Foo [c] where
        op = e

When typechecking the binding 'op = e', we'll have a meth_id for op
whose type is
      op :: forall c. Foo c => forall b. Ord b => [c] -> b -> b -> b

So tcPolyBinds must be capable of dealing with nested polytypes;
and so it is. See GHC.Tc.Gen.Bind.tcMonoBinds (with type-sig case).

Note [Silly default-method bind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we pass the default method binding to the type checker, it must
look like    op2 = e
not          $dmop2 = e
otherwise the "$dm" stuff comes out error messages.  But we want the
"$dm" to come out in the interface file.  So we typecheck the former,
and wrap it in a let, thus
          $dmop2 = let op2 = e in op2
This makes the error messages right.


************************************************************************
*                                                                      *
                Error messages
*                                                                      *
************************************************************************
-}

{-
badGenericInstanceType :: LHsBinds Name -> SDoc
badGenericInstanceType binds
  = vcat [text "Illegal type pattern in the generic bindings",
          nest 2 (ppr binds)]

missingGenericInstances :: [Name] -> SDoc
missingGenericInstances missing
  = text "Missing type patterns for" <+> pprQuotedList missing

dupGenericInsts :: [(TyCon, InstInfo a)] -> SDoc
dupGenericInsts tc_inst_infos
  = vcat [text "More than one type pattern for a single generic type constructor:",
          nest 2 (vcat (map ppr_inst_ty tc_inst_infos)),
          text "All the type patterns for a generic type constructor must be identical"
    ]
  where
    ppr_inst_ty (_,inst) = ppr (simpleInstInfoTy inst)
-}

badDmPrag :: TcId -> Sig GhcRn -> TcM ()
badDmPrag sel_id prag
  = addErrTc (TcRnDefaultMethodForPragmaLacksBinding sel_id prag)

instDeclCtxt1 :: LHsSigType GhcRn -> SDoc
instDeclCtxt1 hs_inst_ty
  = inst_decl_ctxt (ppr (getLHsInstDeclHead hs_inst_ty))

instDeclCtxt2 :: Type -> SDoc
instDeclCtxt2 dfun_ty
  = instDeclCtxt3 cls tys
  where
    (_,_,cls,tys) = tcSplitDFunTy dfun_ty

instDeclCtxt3 :: Class -> [Type] -> SDoc
instDeclCtxt3 cls cls_tys
  = inst_decl_ctxt (ppr (mkClassPred cls cls_tys))

inst_decl_ctxt :: SDoc -> SDoc
inst_decl_ctxt doc = hang (text "In the instance declaration for")
                        2 (quotes doc)

tcATDefault :: SrcSpan
            -> Subst
            -> NameSet
            -> ClassATItem
            -> TcM [FamInst]
-- ^ Construct default instances for any associated types that
-- aren't given a user definition
-- Returns [] or singleton
tcATDefault loc inst_subst defined_ats (ATI fam_tc defs)
  -- User supplied instances ==> everything is OK
  | tyConName fam_tc `elemNameSet` defined_ats
  = return []

  -- No user instance, have defaults ==> instantiate them
   -- Example:   class C a where { type F a b :: *; type F a b = () }
   --            instance C [x]
   -- Then we want to generate the decl:   type F [x] b = ()
  | Just (rhs_ty, _loc) <- defs
  = do { let (subst', pat_tys') = substATBndrs inst_subst (tyConTyVars fam_tc)
             rhs'     = substTyUnchecked subst' rhs_ty
             tcv' = tyCoVarsOfTypesList pat_tys'
             (tv', cv') = partition isTyVar tcv'
             tvs'     = scopedSort tv'
             cvs'     = scopedSort cv'
       ; rep_tc_name <- newFamInstTyConName (L (noAnnSrcSpan loc) (tyConName fam_tc)) pat_tys'
       ; let axiom = mkSingleCoAxiom Nominal rep_tc_name tvs' [] cvs'
                                     fam_tc pat_tys' rhs'
           -- NB: no validity check. We check validity of default instances
           -- in the class definition. Because type instance arguments cannot
           -- be type family applications and cannot be polytypes, the
           -- validity check is redundant.

       ; traceTc "mk_deflt_at_instance" (vcat [ ppr fam_tc, ppr rhs_ty
                                              , pprCoAxiom axiom ])
       ; fam_inst <- newFamInst SynFamilyInst axiom
       ; return [fam_inst] }

   -- No defaults ==> generate a warning
  | otherwise  -- defs = Nothing
  = do { warnMissingAT (tyConName fam_tc)
       ; return [] }

-- | Apply a substitution to the type variable binders of an associated type
-- family. This is used to compute default instances for associated type
-- families (see 'tcATDefault') as well as @newtype@-derived associated type
-- family instances (see @gen_Newtype_fam_insts@ in "GHC.Tc.Deriv.Generate").
--
-- As a concrete example, consider the following class and associated type
-- family:
--
-- @
--   class C k (a :: k) where
--     type F k a (b :: k) :: Type
--     type F j p q = (Proxy @j p, Proxy @j (q :: j))
-- @
--
-- If a user defines this instance:
--
-- @
-- instance C (Type -> Type) Maybe where {}
-- @
--
-- Then in order to typecheck the default @F@ instance, we must apply the
-- substitution @[k :-> (Type -> Type), a :-> Maybe]@ to @F@'s binders, which
-- are @[k, a, (b :: k)]@. The result should look like this:
--
-- @
--   type F (Type -> Type) Maybe (b :: Type -> Type) =
--     (Proxy @(Type -> Type) Maybe, Proxy @(Type -> Type) (b :: Type -> Type))
-- @
--
-- Making this work requires some care. There are two cases:
--
-- 1. If we encounter a type variable in the domain of the substitution (e.g.,
--    @k@ or @a@), then we apply the substitution directly.
--
-- 2. Otherwise, we substitute into the type variable's kind (e.g., turn
--    @b :: k@ to @b :: Type -> Type@). We then return an extended substitution
--    where the old @b@ (of kind @k@) maps to the new @b@ (of kind @Type -> Type@).
--
--    This step is important to do in case there are later occurrences of @b@,
--    which we must ensure have the correct kind. Otherwise, we might end up
--    with @Proxy \@(Type -> Type) (b :: k)@ on the right-hand side of the
--    default instance, which would be completely wrong.
--
-- Contrast 'substATBndrs' function with similar substitution functions:
--
-- * 'substTyVars' does not substitute into the kinds of each type variable,
--   nor does it extend the substitution. 'substTyVars' is meant for occurrences
--   of type variables, whereas 'substATBndr's is meant for binders.
--
-- * 'substTyVarBndrs' does substitute into kinds and extends the substitution,
--   but it does not apply the substitution to the variables themselves. As
--   such, 'substTyVarBndrs' returns a list of 'TyVar's rather than a list of
--   'Type's.
substATBndrs :: Subst -> [TyVar] -> (Subst, [Type])
substATBndrs = mapAccumL substATBndr
  where
    substATBndr :: Subst -> TyVar -> (Subst, Type)
    substATBndr subst tc_tv
        -- Case (1) in the Haddocks
      | Just ty <- lookupVarEnv (getTvSubstEnv subst) tc_tv
      = (subst, ty)
        -- Case (2) in the Haddocks
      | otherwise
      = (extendTvSubstWithClone subst tc_tv tc_tv', mkTyVarTy tc_tv')
      where
        tc_tv' = updateTyVarKind (substTy subst) tc_tv

warnMissingAT :: Name -> TcM ()
warnMissingAT name
  = do { warn <- woptM Opt_WarnMissingMethods
       ; traceTc "warn" (ppr name <+> ppr warn)
       ; hsc_src <- fmap tcg_src getGblEnv
       -- hs-boot and signatures never need to provide complete "definitions"
       -- of any sort, as they aren't really defining anything, but just
       -- constraining items which are defined elsewhere.
       ; let diag = TcRnIllegalInstance $ IllegalFamilyInstance
                  $ InvalidAssoc $ InvalidAssocInstance
                  $ AssocInstanceMissing name
       ; diagnosticTc  (warn && hsc_src == HsSrcFile) diag
                       }
