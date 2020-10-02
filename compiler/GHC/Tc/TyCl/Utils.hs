{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1999

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Analysis functions over data types. Specifically, detecting recursive types.
--
-- This stuff is only used for source-code decls; it's recorded in interface
-- files for imported data types.
module GHC.Tc.TyCl.Utils(
        RolesInfo,
        inferRoles,
        checkSynCycles,
        checkClassCycles,

        -- * Implicits
        addFamInsts, addTyConsToGblEnv, mkDefaultMethodType,

        -- * Record selectors
        tcRecSelBinds, mkRecSelBinds, mkOneRecordSelector
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Tc.Instance.Family
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Env
import GHC.Tc.Gen.Bind( tcValBinds )
import GHC.Core.TyCo.Rep( Type(..), Coercion(..), MCoercion(..), UnivCoProvenance(..) )
import GHC.Core.Multiplicity
import GHC.Tc.Utils.TcType
import GHC.Core.Predicate
import GHC.Builtin.Types( unitTy, mkBoxedTupleTy )
import GHC.Core.Make( rEC_SEL_ERROR_ID )
import GHC.Hs
import GHC.Core.Class
import GHC.Core.FamInstEnv
import GHC.Core.Type
import GHC.Driver.Types
import GHC.Core.TyCon
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Types.FieldLabel
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set hiding (unitFV)
import GHC.Types.Name.Reader ( mkVarUnqual )
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Core.Coercion ( ltRole )
import GHC.Core.Coercion.Axiom ( toBranchedAxiom )
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Builtin.Uniques ( mkBuiltinUnique )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Data.Maybe
import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Utils.FV as FV
import GHC.Unit.Module
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad

{-
************************************************************************
*                                                                      *
        Cycles in type synonym declarations
*                                                                      *
************************************************************************
-}

synonymTyConsOfType :: Type -> [TyCon]
-- Does not look through type synonyms at all
-- Return a list of synonym tycons
-- Keep this synchronized with 'expandTypeSynonyms'
synonymTyConsOfType ty
  = nameEnvElts (go ty)
  where
     go :: Type -> NameEnv TyCon  -- The NameEnv does duplicate elim
     go (TyConApp tc tys) = go_tc tc `plusNameEnv` go_s tys
     go (LitTy _)         = emptyNameEnv
     go (TyVarTy _)       = emptyNameEnv
     go (AppTy a b)       = go a `plusNameEnv` go b
     go (FunTy _ w a b)   = go w `plusNameEnv` go a `plusNameEnv` go b
     go (ForAllTy _ ty)   = go ty
     go (CastTy ty co)    = go ty `plusNameEnv` go_co co
     go (CoercionTy co)   = go_co co

     -- Note [TyCon cycles through coercions?!]
     -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     -- Although, in principle, it's possible for a type synonym loop
     -- could go through a coercion (since a coercion can refer to
     -- a TyCon or Type), it doesn't seem possible to actually construct
     -- a Haskell program which tickles this case.  Here is an example
     -- program which causes a coercion:
     --
     --   type family Star where
     --       Star = Type
     --
     --   data T :: Star -> Type
     --   data S :: forall (a :: Type). T a -> Type
     --
     -- Here, the application 'T a' must first coerce a :: Type to a :: Star,
     -- witnessed by the type family.  But if we now try to make Type refer
     -- to a type synonym which in turn refers to Star, we'll run into
     -- trouble: we're trying to define and use the type constructor
     -- in the same recursive group.  Possibly this restriction will be
     -- lifted in the future but for now, this code is "just for completeness
     -- sake".
     go_mco MRefl    = emptyNameEnv
     go_mco (MCo co) = go_co co

     go_co (Refl ty)              = go ty
     go_co (GRefl _ ty mco)       = go ty `plusNameEnv` go_mco mco
     go_co (TyConAppCo _ tc cs)   = go_tc tc `plusNameEnv` go_co_s cs
     go_co (AppCo co co')         = go_co co `plusNameEnv` go_co co'
     go_co (ForAllCo _ co co')    = go_co co `plusNameEnv` go_co co'
     go_co (FunCo _ co_mult co co') = go_co co_mult `plusNameEnv` go_co co `plusNameEnv` go_co co'
     go_co (CoVarCo _)            = emptyNameEnv
     go_co (HoleCo {})            = emptyNameEnv
     go_co (AxiomInstCo _ _ cs)   = go_co_s cs
     go_co (UnivCo p _ ty ty')    = go_prov p `plusNameEnv` go ty `plusNameEnv` go ty'
     go_co (SymCo co)             = go_co co
     go_co (TransCo co co')       = go_co co `plusNameEnv` go_co co'
     go_co (NthCo _ _ co)         = go_co co
     go_co (LRCo _ co)            = go_co co
     go_co (InstCo co co')        = go_co co `plusNameEnv` go_co co'
     go_co (KindCo co)            = go_co co
     go_co (SubCo co)             = go_co co
     go_co (AxiomRuleCo _ cs)     = go_co_s cs

     go_prov (PhantomProv co)     = go_co co
     go_prov (ProofIrrelProv co)  = go_co co
     go_prov (PluginProv _)       = emptyNameEnv

     go_tc tc | isTypeSynonymTyCon tc = unitNameEnv (tyConName tc) tc
              | otherwise             = emptyNameEnv
     go_s tys = foldr (plusNameEnv . go) emptyNameEnv tys
     go_co_s cos = foldr (plusNameEnv . go_co) emptyNameEnv cos

-- | A monad for type synonym cycle checking, which keeps
-- track of the TyCons which are known to be acyclic, or
-- a failure message reporting that a cycle was found.
newtype SynCycleM a = SynCycleM {
    runSynCycleM :: SynCycleState -> Either (SrcSpan, SDoc) (a, SynCycleState) }
    deriving (Functor)

type SynCycleState = NameSet

instance Applicative SynCycleM where
    pure x = SynCycleM $ \state -> Right (x, state)
    (<*>) = ap

instance Monad SynCycleM where
    m >>= f = SynCycleM $ \state ->
        case runSynCycleM m state of
            Right (x, state') ->
                runSynCycleM (f x) state'
            Left err -> Left err

failSynCycleM :: SrcSpan -> SDoc -> SynCycleM ()
failSynCycleM loc err = SynCycleM $ \_ -> Left (loc, err)

-- | Test if a 'Name' is acyclic, short-circuiting if we've
-- seen it already.
checkNameIsAcyclic :: Name -> SynCycleM () -> SynCycleM ()
checkNameIsAcyclic n m = SynCycleM $ \s ->
    if n `elemNameSet` s
        then Right ((), s) -- short circuit
        else case runSynCycleM m s of
                Right ((), s') -> Right ((), extendNameSet s' n)
                Left err -> Left err

-- | Checks if any of the passed in 'TyCon's have cycles.
-- Takes the 'Unit' of the home package (as we can avoid
-- checking those TyCons: cycles never go through foreign packages) and
-- the corresponding @LTyClDecl Name@ for each 'TyCon', so we
-- can give better error messages.
checkSynCycles :: Unit -> [TyCon] -> [LTyClDecl GhcRn] -> TcM ()
checkSynCycles this_uid tcs tyclds = do
    case runSynCycleM (mapM_ (go emptyNameSet []) tcs) emptyNameSet of
        Left (loc, err) -> setSrcSpan loc $ failWithTc err
        Right _  -> return ()
  where
    -- Try our best to print the LTyClDecl for locally defined things
    lcl_decls = mkNameEnv (zip (map tyConName tcs) tyclds)

    -- Short circuit if we've already seen this Name and concluded
    -- it was acyclic.
    go :: NameSet -> [TyCon] -> TyCon -> SynCycleM ()
    go so_far seen_tcs tc =
        checkNameIsAcyclic (tyConName tc) $ go' so_far seen_tcs tc

    -- Expand type synonyms, complaining if you find the same
    -- type synonym a second time.
    go' :: NameSet -> [TyCon] -> TyCon -> SynCycleM ()
    go' so_far seen_tcs tc
        | n `elemNameSet` so_far
            = failSynCycleM (getSrcSpan (head seen_tcs)) $
                  sep [ text "Cycle in type synonym declarations:"
                      , nest 2 (vcat (map ppr_decl seen_tcs)) ]
        -- Optimization: we don't allow cycles through external packages,
        -- so once we find a non-local name we are guaranteed to not
        -- have a cycle.
        --
        -- This won't hold once we get recursive packages with Backpack,
        -- but for now it's fine.
        | not (isHoleModule mod ||
               moduleUnit mod == this_uid ||
               isInteractiveModule mod)
            = return ()
        | Just ty <- synTyConRhs_maybe tc =
            go_ty (extendNameSet so_far (tyConName tc)) (tc:seen_tcs) ty
        | otherwise = return ()
      where
        n = tyConName tc
        mod = nameModule n
        ppr_decl tc =
          case lookupNameEnv lcl_decls n of
            Just (L loc decl) -> ppr loc <> colon <+> ppr decl
            Nothing -> ppr (getSrcSpan n) <> colon <+> ppr n
                       <+> text "from external module"
         where
          n = tyConName tc

    go_ty :: NameSet -> [TyCon] -> Type -> SynCycleM ()
    go_ty so_far seen_tcs ty =
        mapM_ (go so_far seen_tcs) (synonymTyConsOfType ty)

{- Note [Superclass cycle check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The superclass cycle check for C decides if we can statically
guarantee that expanding C's superclass cycles transitively is
guaranteed to terminate.  This is a Haskell98 requirement,
but one that we lift with -XUndecidableSuperClasses.

The worry is that a superclass cycle could make the type checker loop.
More precisely, with a constraint (Given or Wanted)
    C ty1 .. tyn
one approach is to instantiate all of C's superclasses, transitively.
We can only do so if that set is finite.

This potential loop occurs only through superclasses.  This, for
example, is fine
  class C a where
    op :: C b => a -> b -> b
even though C's full definition uses C.

Making the check static also makes it conservative.  Eg
  type family F a
  class F a => C a
Here an instance of (F a) might mention C:
  type instance F [a] = C a
and now we'd have a loop.

The static check works like this, starting with C
  * Look at C's superclass predicates
  * If any is a type-function application,
    or is headed by a type variable, fail
  * If any has C at the head, fail
  * If any has a type class D at the head,
    make the same test with D

A tricky point is: what if there is a type variable at the head?
Consider this:
   class f (C f) => C f
   class c       => Id c
and now expand superclasses for constraint (C Id):
     C Id
 --> Id (C Id)
 --> C Id
 --> ....
Each step expands superclasses one layer, and clearly does not terminate.
-}

checkClassCycles :: Class -> Maybe SDoc
-- Nothing  <=> ok
-- Just err <=> possible cycle error
checkClassCycles cls
  = do { (definite_cycle, err) <- go (unitNameSet (getName cls))
                                     cls (mkTyVarTys (classTyVars cls))
       ; let herald | definite_cycle = text "Superclass cycle for"
                    | otherwise      = text "Potential superclass cycle for"
       ; return (vcat [ herald <+> quotes (ppr cls)
                      , nest 2 err, hint]) }
  where
    hint = text "Use UndecidableSuperClasses to accept this"

    -- Expand superclasses starting with (C a b), complaining
    -- if you find the same class a second time, or a type function
    -- or predicate headed by a type variable
    --
    -- NB: this code duplicates TcType.transSuperClasses, but
    --     with more error message generation clobber
    -- Make sure the two stay in sync.
    go :: NameSet -> Class -> [Type] -> Maybe (Bool, SDoc)
    go so_far cls tys = firstJusts $
                        map (go_pred so_far) $
                        immSuperClasses cls tys

    go_pred :: NameSet -> PredType -> Maybe (Bool, SDoc)
       -- Nothing <=> ok
       -- Just (True, err)  <=> definite cycle
       -- Just (False, err) <=> possible cycle
    go_pred so_far pred  -- NB: tcSplitTyConApp looks through synonyms
       | Just (tc, tys) <- tcSplitTyConApp_maybe pred
       = go_tc so_far pred tc tys
       | hasTyVarHead pred
       = Just (False, hang (text "one of whose superclass constraints is headed by a type variable:")
                         2 (quotes (ppr pred)))
       | otherwise
       = Nothing

    go_tc :: NameSet -> PredType -> TyCon -> [Type] -> Maybe (Bool, SDoc)
    go_tc so_far pred tc tys
      | isFamilyTyCon tc
      = Just (False, hang (text "one of whose superclass constraints is headed by a type family:")
                        2 (quotes (ppr pred)))
      | Just cls <- tyConClass_maybe tc
      = go_cls so_far cls tys
      | otherwise   -- Equality predicate, for example
      = Nothing

    go_cls :: NameSet -> Class -> [Type] -> Maybe (Bool, SDoc)
    go_cls so_far cls tys
       | cls_nm `elemNameSet` so_far
       = Just (True, text "one of whose superclasses is" <+> quotes (ppr cls))
       | isCTupleClass cls
       = go so_far cls tys
       | otherwise
       = do { (b,err) <- go  (so_far `extendNameSet` cls_nm) cls tys
          ; return (b, text "one of whose superclasses is" <+> quotes (ppr cls)
                       $$ err) }
       where
         cls_nm = getName cls

{-
************************************************************************
*                                                                      *
        Role inference
*                                                                      *
************************************************************************

Note [Role inference]
~~~~~~~~~~~~~~~~~~~~~
The role inference algorithm datatype definitions to infer the roles on the
parameters. Although these roles are stored in the tycons, we can perform this
algorithm on the built tycons, as long as we don't peek at an as-yet-unknown
roles field! Ah, the magic of laziness.

First, we choose appropriate initial roles. For families and classes, roles
(including initial roles) are N. For datatypes, we start with the role in the
role annotation (if any), or otherwise use Phantom. This is done in
initialRoleEnv1.

The function irGroup then propagates role information until it reaches a
fixpoint, preferring N over (R or P) and R over P. To aid in this, we have a
monad RoleM, which is a combination reader and state monad. In its state are
the current RoleEnv, which gets updated by role propagation, and an update
bit, which we use to know whether or not we've reached the fixpoint. The
environment of RoleM contains the tycon whose parameters we are inferring, and
a VarEnv from parameters to their positions, so we can update the RoleEnv.
Between tycons, this reader information is missing; it is added by
addRoleInferenceInfo.

There are two kinds of tycons to consider: algebraic ones (excluding classes)
and type synonyms. (Remember, families don't participate -- all their parameters
are N.) An algebraic tycon processes each of its datacons, in turn. Note that
a datacon's universally quantified parameters might be different from the parent
tycon's parameters, so we use the datacon's univ parameters in the mapping from
vars to positions. Note also that we don't want to infer roles for existentials
(they're all at N, too), so we put them in the set of local variables. As an
optimisation, we skip any tycons whose roles are already all Nominal, as there
nowhere else for them to go. For synonyms, we just analyse their right-hand sides.

irType walks through a type, looking for uses of a variable of interest and
propagating role information. Because anything used under a phantom position
is at phantom and anything used under a nominal position is at nominal, the
irType function can assume that anything it sees is at representational. (The
other possibilities are pruned when they're encountered.)

The rest of the code is just plumbing.

How do we know that this algorithm is correct? It should meet the following
specification:

Let Z be a role context -- a mapping from variables to roles. The following
rules define the property (Z |- t : r), where t is a type and r is a role:

Z(a) = r'        r' <= r
------------------------- RCVar
Z |- a : r

---------- RCConst
Z |- T : r               -- T is a type constructor

Z |- t1 : r
Z |- t2 : N
-------------- RCApp
Z |- t1 t2 : r

forall i<=n. (r_i is R or N) implies Z |- t_i : r_i
roles(T) = r_1 .. r_n
---------------------------------------------------- RCDApp
Z |- T t_1 .. t_n : R

Z, a:N |- t : r
---------------------- RCAll
Z |- forall a:k.t : r


We also have the following rules:

For all datacon_i in type T, where a_1 .. a_n are universally quantified
and b_1 .. b_m are existentially quantified, and the arguments are t_1 .. t_p,
then if forall j<=p, a_1 : r_1 .. a_n : r_n, b_1 : N .. b_m : N |- t_j : R,
then roles(T) = r_1 .. r_n

roles(->) = R, R
roles(~#) = N, N

With -dcore-lint on, the output of this algorithm is checked in checkValidRoles,
called from checkValidTycon.

Note [Role-checking data constructor arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data T a where
    MkT :: Eq b => F a -> (a->a) -> T (G a)

Then we want to check the roles at which 'a' is used
in MkT's type.  We want to work on the user-written type,
so we need to take into account
  * the arguments:   (F a) and (a->a)
  * the context:     C a b
  * the result type: (G a)   -- this is in the eq_spec


Note [Coercions in role inference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Is (t |> co1) representationally equal to (t |> co2)? Of course they are! Changing
the kind of a type is totally irrelevant to the representation of that type. So,
we want to totally ignore coercions when doing role inference. This includes omitting
any type variables that appear in nominal positions but only within coercions.
-}

type RolesInfo = Name -> [Role]

type RoleEnv = NameEnv [Role]        -- from tycon names to roles

-- This, and any of the functions it calls, must *not* look at the roles
-- field of a tycon we are inferring roles about!
-- See Note [Role inference]
inferRoles :: HscSource -> RoleAnnotEnv -> [TyCon] -> Name -> [Role]
inferRoles hsc_src annots tycons
  = let role_env  = initialRoleEnv hsc_src annots tycons
        role_env' = irGroup role_env tycons in
    \name -> case lookupNameEnv role_env' name of
      Just roles -> roles
      Nothing    -> pprPanic "inferRoles" (ppr name)

initialRoleEnv :: HscSource -> RoleAnnotEnv -> [TyCon] -> RoleEnv
initialRoleEnv hsc_src annots = extendNameEnvList emptyNameEnv .
                                map (initialRoleEnv1 hsc_src annots)

initialRoleEnv1 :: HscSource -> RoleAnnotEnv -> TyCon -> (Name, [Role])
initialRoleEnv1 hsc_src annots_env tc
  | isFamilyTyCon tc      = (name, map (const Nominal) bndrs)
  | isAlgTyCon tc         = (name, default_roles)
  | isTypeSynonymTyCon tc = (name, default_roles)
  | otherwise             = pprPanic "initialRoleEnv1" (ppr tc)
  where name         = tyConName tc
        bndrs        = tyConBinders tc
        argflags     = map tyConBinderArgFlag bndrs
        num_exps     = count isVisibleArgFlag argflags

          -- if the number of annotations in the role annotation decl
          -- is wrong, just ignore it. We check this in the validity check.
        role_annots
          = case lookupRoleAnnot annots_env name of
              Just (L _ (RoleAnnotDecl _ _ annots))
                | annots `lengthIs` num_exps -> map unLoc annots
              _                              -> replicate num_exps Nothing
        default_roles = build_default_roles argflags role_annots

        build_default_roles (argf : argfs) (m_annot : ras)
          | isVisibleArgFlag argf
          = (m_annot `orElse` default_role) : build_default_roles argfs ras
        build_default_roles (_argf : argfs) ras
          = Nominal : build_default_roles argfs ras
        build_default_roles [] [] = []
        build_default_roles _ _ = pprPanic "initialRoleEnv1 (2)"
                                           (vcat [ppr tc, ppr role_annots])

        default_role
          | isClassTyCon tc               = Nominal
          -- Note [Default roles for abstract TyCons in hs-boot/hsig]
          | HsBootFile <- hsc_src
          , isAbstractTyCon tc            = Representational
          | HsigFile   <- hsc_src
          , isAbstractTyCon tc            = Nominal
          | otherwise                     = Phantom

-- Note [Default roles for abstract TyCons in hs-boot/hsig]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- What should the default role for an abstract TyCon be?
--
-- Originally, we inferred phantom role for abstract TyCons
-- in hs-boot files, because the type variables were never used.
--
-- This was silly, because the role of the abstract TyCon
-- was required to match the implementation, and the roles of
-- data types are almost never phantom.  Thus, in ticket #9204,
-- the default was changed so be representational (the most common case).  If
-- the implementing data type was actually nominal, you'd get an easy
-- to understand error, and add the role annotation yourself.
--
-- Then Backpack was added, and with it we added role *subtyping*
-- the matching judgment: if an abstract TyCon has a nominal
-- parameter, it's OK to implement it with a representational
-- parameter.  But now, the representational default is not a good
-- one, because you should *only* request representational if
-- you're planning to do coercions. To be maximally flexible
-- with what data types you will accept, you want the default
-- for hsig files is nominal.  We don't allow role subtyping
-- with hs-boot files (it's good practice to give an exactly
-- accurate role here, because any types that use the abstract
-- type will propagate the role information.)

irGroup :: RoleEnv -> [TyCon] -> RoleEnv
irGroup env tcs
  = let (env', update) = runRoleM env $ mapM_ irTyCon tcs in
    if update
    then irGroup env' tcs
    else env'

irTyCon :: TyCon -> RoleM ()
irTyCon tc
  | isAlgTyCon tc
  = do { old_roles <- lookupRoles tc
       ; unless (all (== Nominal) old_roles) $  -- also catches data families,
                                                -- which don't want or need role inference
         irTcTyVars tc $
         do { mapM_ (irType emptyVarSet) (tyConStupidTheta tc)  -- See #8958
            ; whenIsJust (tyConClass_maybe tc) irClass
            ; mapM_ irDataCon (visibleDataCons $ algTyConRhs tc) }}

  | Just ty <- synTyConRhs_maybe tc
  = irTcTyVars tc $
    irType emptyVarSet ty

  | otherwise
  = return ()

-- any type variable used in an associated type must be Nominal
irClass :: Class -> RoleM ()
irClass cls
  = mapM_ ir_at (classATs cls)
  where
    cls_tvs    = classTyVars cls
    cls_tv_set = mkVarSet cls_tvs

    ir_at at_tc
      = mapM_ (updateRole Nominal) nvars
      where nvars = filter (`elemVarSet` cls_tv_set) $ tyConTyVars at_tc

-- See Note [Role inference]
irDataCon :: DataCon -> RoleM ()
irDataCon datacon
  = setRoleInferenceVars univ_tvs $
    irExTyVars ex_tvs $ \ ex_var_set ->
    mapM_ (irType ex_var_set)
          (map tyVarKind ex_tvs ++ eqSpecPreds eq_spec ++ theta ++ (map scaledThing arg_tys))
      -- See Note [Role-checking data constructor arguments]
  where
    (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _res_ty)
      = dataConFullSig datacon

irType :: VarSet -> Type -> RoleM ()
irType = go
  where
    go lcls ty                 | Just ty' <- coreView ty -- #14101
                               = go lcls ty'
    go lcls (TyVarTy tv)       = unless (tv `elemVarSet` lcls) $
                                 updateRole Representational tv
    go lcls (AppTy t1 t2)      = go lcls t1 >> markNominal lcls t2
    go lcls (TyConApp tc tys)  = do { roles <- lookupRolesX tc
                                    ; zipWithM_ (go_app lcls) roles tys }
    go lcls (ForAllTy tvb ty)  = do { let tv = binderVar tvb
                                          lcls' = extendVarSet lcls tv
                                    ; markNominal lcls (tyVarKind tv)
                                    ; go lcls' ty }
    go lcls (FunTy _ w arg res)  = markNominal lcls w >> go lcls arg >> go lcls res
    go _    (LitTy {})         = return ()
      -- See Note [Coercions in role inference]
    go lcls (CastTy ty _)      = go lcls ty
    go _    (CoercionTy _)     = return ()

    go_app _ Phantom _ = return ()                 -- nothing to do here
    go_app lcls Nominal ty = markNominal lcls ty  -- all vars below here are N
    go_app lcls Representational ty = go lcls ty

irTcTyVars :: TyCon -> RoleM a -> RoleM a
irTcTyVars tc thing
  = setRoleInferenceTc (tyConName tc) $ go (tyConTyVars tc)
  where
    go []       = thing
    go (tv:tvs) = do { markNominal emptyVarSet (tyVarKind tv)
                     ; addRoleInferenceVar tv $ go tvs }

irExTyVars :: [TyVar] -> (TyVarSet -> RoleM a) -> RoleM a
irExTyVars orig_tvs thing = go emptyVarSet orig_tvs
  where
    go lcls []       = thing lcls
    go lcls (tv:tvs) = do { markNominal lcls (tyVarKind tv)
                          ; go (extendVarSet lcls tv) tvs }

markNominal :: TyVarSet   -- local variables
            -> Type -> RoleM ()
markNominal lcls ty = let nvars = fvVarList (FV.delFVs lcls $ get_ty_vars ty) in
                      mapM_ (updateRole Nominal) nvars
  where
     -- get_ty_vars gets all the tyvars (no covars!) from a type *without*
     -- recurring into coercions. Recall: coercions are totally ignored during
     -- role inference. See [Coercions in role inference]
    get_ty_vars :: Type -> FV
    get_ty_vars (TyVarTy tv)      = unitFV tv
    get_ty_vars (AppTy t1 t2)     = get_ty_vars t1 `unionFV` get_ty_vars t2
    get_ty_vars (FunTy _ w t1 t2) = get_ty_vars w `unionFV` get_ty_vars t1 `unionFV` get_ty_vars t2
    get_ty_vars (TyConApp _ tys)  = mapUnionFV get_ty_vars tys
    get_ty_vars (ForAllTy tvb ty) = tyCoFVsBndr tvb (get_ty_vars ty)
    get_ty_vars (LitTy {})        = emptyFV
    get_ty_vars (CastTy ty _)     = get_ty_vars ty
    get_ty_vars (CoercionTy _)    = emptyFV

-- like lookupRoles, but with Nominal tags at the end for oversaturated TyConApps
lookupRolesX :: TyCon -> RoleM [Role]
lookupRolesX tc
  = do { roles <- lookupRoles tc
       ; return $ roles ++ repeat Nominal }

-- gets the roles either from the environment or the tycon
lookupRoles :: TyCon -> RoleM [Role]
lookupRoles tc
  = do { env <- getRoleEnv
       ; case lookupNameEnv env (tyConName tc) of
           Just roles -> return roles
           Nothing    -> return $ tyConRoles tc }

-- tries to update a role; won't ever update a role "downwards"
updateRole :: Role -> TyVar -> RoleM ()
updateRole role tv
  = do { var_ns <- getVarNs
       ; name <- getTyConName
       ; case lookupVarEnv var_ns tv of
           Nothing -> pprPanic "updateRole" (ppr name $$ ppr tv $$ ppr var_ns)
           Just n  -> updateRoleEnv name n role }

-- the state in the RoleM monad
data RoleInferenceState = RIS { role_env  :: RoleEnv
                              , update    :: Bool }

-- the environment in the RoleM monad
type VarPositions = VarEnv Int

-- See [Role inference]
newtype RoleM a = RM { unRM :: Maybe Name -- of the tycon
                            -> VarPositions
                            -> Int          -- size of VarPositions
                            -> RoleInferenceState
                            -> (a, RoleInferenceState) }
    deriving (Functor)

instance Applicative RoleM where
    pure x = RM $ \_ _ _ state -> (x, state)
    (<*>) = ap

instance Monad RoleM where
  a >>= f  = RM $ \m_info vps nvps state ->
                  let (a', state') = unRM a m_info vps nvps state in
                  unRM (f a') m_info vps nvps state'

runRoleM :: RoleEnv -> RoleM () -> (RoleEnv, Bool)
runRoleM env thing = (env', update)
  where RIS { role_env = env', update = update }
          = snd $ unRM thing Nothing emptyVarEnv 0 state
        state = RIS { role_env  = env
                    , update    = False }

setRoleInferenceTc :: Name -> RoleM a -> RoleM a
setRoleInferenceTc name thing = RM $ \m_name vps nvps state ->
                                ASSERT( isNothing m_name )
                                ASSERT( isEmptyVarEnv vps )
                                ASSERT( nvps == 0 )
                                unRM thing (Just name) vps nvps state

addRoleInferenceVar :: TyVar -> RoleM a -> RoleM a
addRoleInferenceVar tv thing
  = RM $ \m_name vps nvps state ->
    ASSERT( isJust m_name )
    unRM thing m_name (extendVarEnv vps tv nvps) (nvps+1) state

setRoleInferenceVars :: [TyVar] -> RoleM a -> RoleM a
setRoleInferenceVars tvs thing
  = RM $ \m_name _vps _nvps state ->
    ASSERT( isJust m_name )
    unRM thing m_name (mkVarEnv (zip tvs [0..])) (panic "setRoleInferenceVars")
         state

getRoleEnv :: RoleM RoleEnv
getRoleEnv = RM $ \_ _ _ state@(RIS { role_env = env }) -> (env, state)

getVarNs :: RoleM VarPositions
getVarNs = RM $ \_ vps _ state -> (vps, state)

getTyConName :: RoleM Name
getTyConName = RM $ \m_name _ _ state ->
                    case m_name of
                      Nothing   -> panic "getTyConName"
                      Just name -> (name, state)

updateRoleEnv :: Name -> Int -> Role -> RoleM ()
updateRoleEnv name n role
  = RM $ \_ _ _ state@(RIS { role_env = role_env }) -> ((),
         case lookupNameEnv role_env name of
           Nothing -> pprPanic "updateRoleEnv" (ppr name)
           Just roles -> let (before, old_role : after) = splitAt n roles in
                         if role `ltRole` old_role
                         then let roles' = before ++ role : after
                                  role_env' = extendNameEnv role_env name roles' in
                              RIS { role_env = role_env', update = True }
                         else state )


{- *********************************************************************
*                                                                      *
                Building implicits
*                                                                      *
********************************************************************* -}

addFamInsts :: [FamInst] -> TcM a -> TcM a
-- Extend (a) the family instance envt
--        (b) the type envt with stuff from data type decls
addFamInsts fam_insts thing_inside
  = tcExtendLocalFamInstEnv fam_insts $
    tcExtendGlobalEnv axioms          $
    do { traceTc "addFamInsts" (pprFamInsts fam_insts)
       ; addTyConsToGblEnv data_rep_tycons thing_inside
                    -- Does not add its axiom; that comes
                    -- from adding the 'axioms' above
       }
  where
    axioms = map (ACoAxiom . toBranchedAxiom . famInstAxiom) fam_insts
    data_rep_tycons = famInstsRepTyCons fam_insts
      -- The representation tycons for 'data instances' declarations

addTyConsToGblEnv :: [TyCon] -> TcM a -> TcM a
-- Given a [TyCon], add to the TcGblEnv
--   * extend the TypeEnv with the tycons
--   * extend the TypeEnv with their implicitTyThings
--   * extend the TypeEnv with any default method Ids
--   * add bindings for record selectors
addTyConsToGblEnv tyclss thing_inside
  = tcExtendTyConEnv tyclss                    $
    tcExtendGlobalEnvImplicit implicit_things  $
    tcExtendGlobalValEnv def_meth_ids          $
    do { traceTc "tcAddTyCons" $ vcat
            [ text "tycons" <+> ppr tyclss
            , text "implicits" <+> ppr implicit_things ]
       ; thing_inside }
 where
   implicit_things = concatMap implicitTyConThings tyclss
   def_meth_ids    = mkDefaultMethodIds tyclss

mkDefaultMethodIds :: [TyCon] -> [Id]
-- We want to put the default-method Ids (both vanilla and generic)
-- into the type environment so that they are found when we typecheck
-- the filled-in default methods of each instance declaration
-- See Note [Default method Ids and Template Haskell]
mkDefaultMethodIds tycons
  = [ mkExportedVanillaId dm_name (mkDefaultMethodType cls sel_id dm_spec)
    | tc <- tycons
    , Just cls <- [tyConClass_maybe tc]
    , (sel_id, Just (dm_name, dm_spec)) <- classOpItems cls ]

mkDefaultMethodType :: Class -> Id -> DefMethSpec Type -> Type
-- Returns the top-level type of the default method
mkDefaultMethodType _ sel_id VanillaDM        = idType sel_id
mkDefaultMethodType cls _   (GenericDM dm_ty) = mkSigmaTy tv_bndrs [pred] dm_ty
   where
     pred      = mkClassPred cls (mkTyVarTys (binderVars cls_bndrs))
     cls_bndrs = tyConBinders (classTyCon cls)
     tv_bndrs  = tyVarSpecToBinders $ tyConInvisTVBinders cls_bndrs
     -- NB: the Class doesn't have TyConBinders; we reach into its
     --     TyCon to get those.  We /do/ need the TyConBinders because
     --     we need the correct visibility: these default methods are
     --     used in code generated by the fill-in for missing
     --     methods in instances (GHC.Tc.TyCl.Instance.mkDefMethBind), and
     --     then typechecked.  So we need the right visibility info
     --     (#13998)

{-
************************************************************************
*                                                                      *
                Building record selectors
*                                                                      *
************************************************************************
-}

{-
Note [Default method Ids and Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#4169):
   class Numeric a where
     fromIntegerNum :: a
     fromIntegerNum = ...

   ast :: Q [Dec]
   ast = [d| instance Numeric Int |]

When we typecheck 'ast' we have done the first pass over the class decl
(in tcTyClDecls), but we have not yet typechecked the default-method
declarations (because they can mention value declarations).  So we
must bring the default method Ids into scope first (so they can be seen
when typechecking the [d| .. |] quote, and typecheck them later.
-}

{-
************************************************************************
*                                                                      *
                Building record selectors
*                                                                      *
************************************************************************
-}

tcRecSelBinds :: [(Id, LHsBind GhcRn)] -> TcM TcGblEnv
tcRecSelBinds sel_bind_prs
  = tcExtendGlobalValEnv [sel_id | (L _ (IdSig _ sel_id)) <- sigs] $
    do { (rec_sel_binds, tcg_env) <- discardWarnings $
                                     -- See Note [Impredicative record selectors]
                                     setXOptM LangExt.ImpredicativeTypes $
                                     tcValBinds TopLevel binds sigs getGblEnv
       ; return (tcg_env `addTypecheckedBinds` map snd rec_sel_binds) }
  where
    sigs = [ L loc (IdSig noExtField sel_id) | (sel_id, _) <- sel_bind_prs
                                             , let loc = getSrcSpan sel_id ]
    binds = [(NonRecursive, unitBag bind) | (_, bind) <- sel_bind_prs]

mkRecSelBinds :: [TyCon] -> TcM [(Id, LHsBind GhcRn)]
-- NB We produce *un-typechecked* bindings, rather like 'deriving'
--    This makes life easier, because the later type checking will add
--    all necessary type abstractions and applications
mkRecSelBinds tycons
  = concatMapM mkRecSelAndUpd [ (tc,fld)
                              | tc <- tycons
                              , fld <- tyConFieldLabels tc ]

-- | Create both a record selector and a record updater binding for a field in a
-- TyCon.  See Note [Record updaters]
mkRecSelAndUpd :: (TyCon, FieldLabel) -> TcM [(Id, LHsBind GhcRn)]
mkRecSelAndUpd (tycon, fl) = do
    -- Make fresh names x1..xN for binding all the fields in the TyCon
    -- (including the one being updated), and a fresh name y for binding the new
    -- value of the field being updated.
    x_vars <- fmap mkNameEnv $ forM (tyConFieldLabels tycon) $ \fl' ->
                  (,) (flSelector fl') <$> newSysName (mkVarOccFS (flLabel fl'))
    let (sel, upd) = mkRecordSelectorAndUpdater all_cons idDetails fl x_vars
    return [sel, upd]
  where
    all_cons = map RealDataCon (tyConDataCons tycon)
    idDetails = RecSelData tycon

-- | Create a record selector binding, but no updater.  This is used for fields
-- in pattern synonyms.  See Note [No updaters for pattern synonyms]
mkOneRecordSelector :: [ConLike] -> RecSelParent -> FieldLabelNoUpdater
                    -> (Id, LHsBind GhcRn)
mkOneRecordSelector all_cons idDetails fl
  = fst $ mkRecordSelectorAndUpdater all_cons idDetails (fl { flUpdate = oops }) oops
  where
    oops = error "mkOneRecordSelector: poked a field needed only for updaters"

mkRecordSelectorAndUpdater :: [ConLike] -> RecSelParent -> FieldLabel
                           -> NameEnv Name
                           -> ((Id, LHsBind GhcRn), (Id, LHsBind GhcRn))
mkRecordSelectorAndUpdater all_cons idDetails fl x_vars =
    ( mk_binding rec_details sel_name sel_ty sel_bind is_naughty
    , mk_binding VanillaId   upd_name upd_ty upd_bind no_updater
    )
  where
    loc      = getSrcSpan sel_name
    lbl      = flLabel fl
    sel_name = flSelector fl
    upd_name = flUpdate fl

    mk_binding :: IdDetails -> Name -> Type  -- What is being bound
               -> LHsBind GhcRn              -- The body of the binding
               -> Bool                       -- Should it be unit instead?
               -> (Id, Located (HsBindLR GhcRn GhcRn))
    mk_binding details name ty bind is_unit
      | is_unit   = (mkExportedLocalId details name unitTy, unit_bind name)
      | otherwise = (mkExportedLocalId details name ty, bind)

    rec_details = RecSelId { sel_tycon = idDetails, sel_naughty = is_naughty }

    -- Find a representative constructor, con1
    cons_w_field = conLikesWithFields all_cons [lbl]
    con1 = ASSERT( not (null cons_w_field) ) head cons_w_field

    -- Selector type; Note [Polymorphic selectors]
    field_ty   = conLikeFieldType con1 lbl
    data_tvbs  = filter (\tvb -> binderVar tvb `elemVarSet` data_tv_set) $
                 conLikeUserTyVarBinders con1
    data_tv_set= tyCoVarsOfTypes inst_tys

    -- See Note [Naughty record selectors] and Note [Naughty record updaters]
    is_naughty = not (tyCoVarsOfType field_ty `subVarSet` data_tv_set)
    no_updater = is_naughty
                  || not (isTauTy field_ty)
                  || not (isLiftedTypeKind (typeKind field_ty))

    -- Make the types for the selector and the updater, which will look like
    --     selector :: ctx => data_ty -> field_ty
    --     updater  :: ctx => data_ty -> (field_ty -> data_ty, field_ty)
    -- respectively.  Regarding the forall-bound type variables,
    -- see Note [Polymorphic selectors].
    sel_ty = the_ty field_ty
    upd_ty = the_ty (mkBoxedTupleTy [ mkVisFunTyMany field_ty data_ty
                                    , field_ty
                                    ])
    -- Make a type like
    --     forall ... . ctx => data_ty -> result_ty
    -- with the right tyvars and context for the selector/updater.
    the_ty :: Type -> Type
    the_ty result_ty = mkForAllTys (tyVarSpecToBinders data_tvbs) $
                          mkPhiTy (conLikeStupidTheta con1) $   -- Urgh!
                          -- req_theta is empty for normal DataCon
                          mkPhiTy req_theta                 $
                          mkVisFunTyMany data_ty            $
                            -- Record selectors are always typed with Many. We
                            -- could improve on it in the case where all the
                            -- fields in all the constructor have multiplicity Many.
                          result_ty

    -- Make the binding: sel (C2 { fld = x }) = x
    --                   sel (C7 { fld = x }) = x
    --    where cons_w_field = [C2,C7]
    sel_bind = mkBind sel_name alts
      where
        alts = map mk_match cons_w_field ++ deflt

        mk_match con = ( [mkRecConPat con [rec_field]]
                       , mkHsVar field_var
                       )
        rec_field = mkHsRecField (mkFieldOcc fl) (mkVarPat field_var)
        field_var = mkInternalName (mkBuiltinUnique 1) (getOccName sel_name) loc

    -- Make the updater binding:
    --   upd z = (\ y -> z { fld = y }, fld z)
    upd_bind = mkBind upd_name [([mkVarPat z_var], expr)]
      where
        y_var = mkInternalName (mkBuiltinUnique 2) (mkVarOccFS lbl) loc
        z_var = mkInternalName (mkBuiltinUnique 3) (mkVarOcc "z") loc

        expr = mkLHsTupleExpr
                   [ mkHsLam [mkVarPat y_var] update_expr
                   , mkHsApp (L loc (HsRecFld noExtField unambiguous_fld))
                             (mkHsVar z_var)
                   ]

        unambiguous_fld = Unambiguous sel_name (L loc (mkVarUnqual (flLabel fl)))

        -- Either z { fld = y } or its desugaring as a case expression.
        -- See Note [Updaters use record update syntax] for why we need the latter.
        update_expr = L loc (if simple_update then rec_upd_expr else case_expr)
        simple_update = null (conLikeExTyCoVars con1)

        -- z { fld = y }
        rec_upd_expr = RecordUpd
            { rupd_ext  = Generated
            , rupd_expr = mkHsVar z_var
            , rupd_flds = [mkHsRecField (L loc unambiguous_fld) (mkHsVar y_var)] }

        -- desugaring of z { fld1 = y }, i.e.
        --     case z of
        --         C2 { fld1 = x1, fld2 = x2, ..., fld = xN }
        --              -> C2 { fld1 = y, fld2 = x2, ..., fldN = xN }
        --         ...
        case_expr = HsCase noExtField (mkHsVar z_var) (mkMatchGroup Generated alts)
        alts = map mk_alt cons_w_field
                   ++ map (uncurry (mkSimpleMatch CaseAlt)) deflt

        -- C2 { fld1 = x1, ..., fld = xN } -> C2 { fld1 = y, fld2 = x2, ..., fldN = xN }
        mk_alt con = mkHsCaseAlt pat expr
          where
            field_labels = conLikeFieldLabels con
            pat  = mkRecConPat con (map (rec_field (mkVarPat . x_var)) field_labels)
            expr = mkRecordCon con (map (rec_field (mkHsVar . con_var)) field_labels)
            con_var fl' = if flSelector fl' == sel_name then y_var else x_var fl'

            -- Used for both pattern and record construction, to create
            --     fldN = k fldN
            -- where k gives the hsRecFieldArg for each field
            rec_field :: (FieldLabelNoUpdater -> a)
                      -> FieldLabelNoUpdater
                      -> LHsRecField' (FieldOcc GhcRn) a
            rec_field k fl' = mkHsRecField (mkFieldOcc fl') (k fl')

            -- The x_vars NameEnv contains a fresh name for every selector name
            -- in the TyCon, i.e. maps fldN to xN.
            x_var :: FieldLabelNoUpdater -> Name
            x_var fl' = lookupNameEnv_NF x_vars (flSelector fl')


    -- These are just boring constructors for bits of syntax, using the SrcSpan
    -- of the field (which is why they are not top-level).
    mkBind :: Name -> [([LPat GhcRn], LHsExpr GhcRn)] -> LHsBind GhcRn
    mkBind name alts = L loc (mkTopFunBind Generated lname alts')
      where
        lname = L loc name
        alts' = map (uncurry (mkSimpleMatch (mkPrefixFunRhs lname))) alts

    mkVarPat :: Name -> LPat GhcRn
    mkVarPat var = L loc (VarPat noExtField (L loc var))

    mkRecConPat :: ConLike -> [LHsRecField GhcRn (XRec GhcRn (Pat GhcRn))] -> LPat GhcRn
    mkRecConPat con rflds = L loc (ConPat noExtField (L loc (getName con))
                                        (RecCon (HsRecFields rflds Nothing)))

    mkHsVar :: Name -> LHsExpr GhcRn
    mkHsVar var = L loc (HsVar noExtField (L loc var))

    mkRecordCon :: ConLike -> [LHsRecField GhcRn (XRec GhcRn (HsExpr GhcRn))] -> LHsExpr GhcRn
    mkRecordCon con rflds = L loc (RecordCon noExtField
                                      (L loc (getName con))
                                      (HsRecFields rflds Nothing))

    mkHsRecField :: Located lbl -> arg -> LHsRecField' lbl arg
    mkHsRecField lbl arg = L loc (HsRecField { hsRecFieldLbl = lbl
                                             , hsRecFieldArg = arg
                                             , hsRecPun = False })

    mkFieldOcc :: FieldLbl update_rep Name -> LFieldOcc GhcRn
    mkFieldOcc fl' = L loc (FieldOcc (flSelector fl')
                                     (L loc (mkVarUnqual (flLabel fl'))))


    -- Add catch-all default case unless the case is exhaustive
    -- We do this explicitly so that we get a nice error message that
    -- mentions this particular record selector
    deflt | all dealt_with all_cons = []
          | otherwise = [( [L loc (WildPat noExtField)]
                         , mkHsApp (L loc (HsVar noExtField
                                         (L loc (getName rEC_SEL_ERROR_ID))))
                                     (L loc (HsLit noExtField msg_lit)))]
    msg_lit = HsStringPrim NoSourceText (bytesFS lbl)

        -- Do not add a default case unless there are unmatched
        -- constructors.  We must take account of GADTs, else we
        -- get overlap warning messages from the pattern-match checker
        -- NB: we need to pass type args for the *representation* TyCon
        --     to dataConCannotMatch, hence the calculation of inst_tys
        --     This matters in data families
        --              data instance T Int a where
        --                 A :: { fld :: Int } -> T Int Bool
        --                 B :: { fld :: Int } -> T Int Char
    dealt_with :: ConLike -> Bool
    dealt_with (PatSynCon _) = False -- We can't predict overlap
    dealt_with con@(RealDataCon dc) =
      con `elem` cons_w_field || dataConCannotMatch inst_tys dc

    (univ_tvs, _, eq_spec, _, req_theta, _, data_ty) = conLikeFullSig con1

    eq_subst = mkTvSubstPrs (map eqSpecPair eq_spec)
    -- inst_tys corresponds to one of the following:
    --
    -- * The arguments to the user-written return type (for GADT constructors).
    --   In this scenario, eq_subst provides a mapping from the universally
    --   quantified type variables to the argument types. Note that eq_subst
    --   does not need to be applied to any other part of the DataCon
    --   (see Note [The dcEqSpec domain invariant] in GHC.Core.DataCon).
    -- * The universally quantified type variables
    --   (for Haskell98-style constructors and pattern synonyms). In these
    --   scenarios, eq_subst is an empty substitution.
    inst_tys = substTyVars eq_subst univ_tvs


    -- Make a binding of unit, for naughty record selectors/updaters:
    --     name :: ()
    --     name = ()
    unit_bind :: Name -> LHsBind GhcRn
    unit_bind name = mkBind name [([], mkLHsTupleExpr [])]



{-
Note [Polymorphic selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We take care to build the type of a polymorphic selector in the right
order, so that visible type application works according to the specification in
the GHC User's Guide (see the "Field selectors and TypeApplications" section).
We won't bother rehashing the entire specification in this Note, but the tricky
part is dealing with GADT constructor fields. Here is an appropriately tricky
example to illustrate the challenges:

  {-# LANGUAGE PolyKinds #-}
  data T a b where
    MkT :: forall b a x.
           { field1 :: forall c. (Num a, Show c) => (Either a c, Proxy b)
           , field2 :: x
           }
        -> T a b

Our goal is to obtain the following type for `field1`:

  field1 :: forall {k} (b :: k) a.
            T a b -> forall c. (Num a, Show c) => (Either a c, Proxy b)

(`field2` is naughty, per Note [Naughty record selectors], so we cannot turn
it into a top-level field selector.)

Some potential gotchas, inspired by #18023:

1. Since the user wrote `forall b a x.` in the type of `MkT`, we want the `b`
   to appear before the `a` when quantified in the type of `field1`.
2. On the other hand, we *don't* want to quantify `x` in the type of `field1`.
   This is because `x` does not appear in the GADT return type, so it is not
   needed in the selector type.
3. Because of PolyKinds, the kind of `b` is generalized to `k`. Moreover, since
   this `k` is not written in the source code, it is inferred (i.e., not
   available for explicit type applications) and thus written as {k} in the type
   of `field1`.

In order to address these gotchas, we start by looking at the
conLikeUserTyVarBinders, which gives the order and specificity of each binder.
This effectively solves (1) and (3). To solve (2), we filter the binders to
leave only those that are needed for the selector type.

Note [Naughty record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A "naughty" field is one for which we can't define a record
selector, because an existential type variable would escape.  For example:
        data T = forall a. MkT { x,y::a }
We obviously can't define
        x (MkT v _) = v
Nevertheless we *do* put a RecSelId into the type environment
so that if the user tries to use 'x' as a selector we can bleat
helpfully, rather than saying unhelpfully that 'x' is not in scope.
Hence the sel_naughty flag, to identify record selectors that don't really exist.

In general, a field is "naughty" if its type mentions a type variable that
isn't in the result type of the constructor.  Note that this *allows*
GADT record selectors (Note [GADT record selectors]) whose types may look
like     sel :: T [a] -> a

For naughty selectors we make a dummy binding
   sel = ()
so that the later type-check will add them to the environment, and they'll be
exported.  The function is never called, because the typechecker spots the
sel_naughty field.

Note [GADT record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For GADTs, we require that all constructors with a common field 'f' have the same
result type (modulo alpha conversion).  [Checked in GHC.Tc.TyCl.checkValidTyCon]
E.g.
        data T where
          T1 { f :: Maybe a } :: T [a]
          T2 { f :: Maybe a, y :: b  } :: T [a]
          T3 :: T Int

and now the selector takes that result type as its argument:
   f :: forall a. T [a] -> Maybe a

Details: the "real" types of T1,T2 are:
   T1 :: forall r a.   (r~[a]) => a -> T r
   T2 :: forall r a b. (r~[a]) => a -> b -> T r

So the selector loooks like this:
   f :: forall a. T [a] -> Maybe a
   f (a:*) (t:T [a])
     = case t of
         T1 c   (g:[a]~[c]) (v:Maybe c)       -> v `cast` Maybe (right (sym g))
         T2 c d (g:[a]~[c]) (v:Maybe c) (w:d) -> v `cast` Maybe (right (sym g))
         T3 -> error "T3 does not have field f"

Note the forall'd tyvars of the selector are just the free tyvars
of the result type; there may be other tyvars in the constructor's
type (e.g. 'b' in T2).

Note the need for casts in the result!

All this applies to updaters (see Note [Record updaters]) as well as selectors;
in the example above we will build this updater:
    $upd:f:T1 :: T [a] -> (Maybe a -> T [a], Maybe a)

If a GADT field has bona-fide existential tyvars that do not appear in the
result type, the selector will be naughty (see Note [Naughty record selectors]).


Note [Selector running example]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's OK to combine GADTs and type families.  Here's a running example:

        data instance T [a] where
          T1 { fld :: b } :: T [Maybe b]

The representation type looks like this
        data :R7T a where
          T1 { fld :: b } :: :R7T (Maybe b)

and there's coercion from the family type to the representation type
        :CoR7T a :: T [a] ~ :R7T a

The selector we want for fld looks like this:

        fld :: forall b. T [Maybe b] -> b
        fld = /\b. \(d::T [Maybe b]).
              case d `cast` :CoR7T (Maybe b) of
                T1 (x::b) -> x

The scrutinee of the case has type :R7T (Maybe b), which can be
gotten by applying the eq_spec to the univ_tvs of the data con.

Note [Impredicative record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are situations where generating code for record selectors requires the
use of ImpredicativeTypes. Here is one example (adapted from #18005):

  type S = (forall b. b -> b) -> Int
  data T = MkT {unT :: S}
         | Dummy

We want to generate HsBinds for unT that look something like this:

  unT :: S
  unT (MkT x) = x
  unT _       = recSelError "unT"#

Note that the type of recSelError is `forall r (a :: TYPE r). Addr# -> a`.
Therefore, when used in the right-hand side of `unT`, GHC attempts to
instantiate `a` with `(forall b. b -> b) -> Int`, which is impredicative.
To make sure that GHC is OK with this, we enable ImpredicativeTypes interally
when typechecking these HsBinds so that the user does not have to.


Note [Record updaters]
~~~~~~~~~~~~~~~~~~~~~~
Given a datatype declaration, we generate a record selector and also a "record
updater" for each field.  For a record type `r` containing a field type `a`, the
updater will have type

    r -> (a -> r, a)

where the first component of the pair sets the field and the second component
returns the existing value.  For example, given the data declaration

    data T y = MkT { foo :: [y], bar :: Int }

we generate the record updaters:

    $upd:foo:MkT :: T y -> ([y] -> T y, [y])
    $upd:foo:MkT x = (\ y -> t { foo = y }, foo x)

    $upd:bar:MkT :: T y -> (Int -> T y, Int)
    $upd:bar:MkT x = (\ y -> t { bar = y }, foo x)

These are used to produce instances of GHC.Records.HasField automatically as
described in Note [HasField instances] in GHC.Tc.Instance.Class.

Note that:

 * The updater's OccName is prefixed with $upd: so it is never valid in user
   code. Its Name never appears in the AvailInfo or GlobalRdrEnv; instead an
   updater is considered to be in scope iff the corresponding field label is in
   scope.

 * The Name of each updater is stored alongside that of the selector in the
   'FieldLabel's in each 'DataCon'.  See the notes in GHC.Types.FieldLabel.

 * Renamed-syntax bindings for both a selector and an updater for each field are
   produced by mkRecordSelectorAndUpdater; these bindings are then type-checked
   together normally.

 * We produce renamed syntax rather than attempting to generate Core terms
   directly because the corresponding Core terms are rather complex.  This is
   because they include the code necessary to evaluate strict fields, and to
   pack/unpack UNPACKed fields, i.e. everything that is handled by the
   constructor wrapper, and by dataConBoxer when desugaring pattern matching.
   See Note [Generating updaters in advance].

 * In some cases we may not be able to generate an updater and will bind its
   name to () instead, even if we can generate the corresponding selector.  See
   Note [Naughty record updaters].

 * We could imagine generating the selector *from* the updater, i.e. build
       $sel:foo:T r = case $upd:foo:T r of (_, x) -> x
   but we don't do so because the updater might be naughty, and for pattern
   synonyms will not exist at all (see Note [No updaters for pattern synonyms]).

 * For GADTs, we insist that all constructors mentioning a field have the same
   type, and reject the definition entirely if not.  Thus if the field does not
   involve an existential (and hence is not naughty) we can make both a selector
   and an updater (see Note [GADT record selectors]).


Note [Updaters use record update syntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Most updaters are defined by generating a record update expression and a call to
the selector function, like this:

    $upd:foo:MkT x = (\ y -> x { foo = y }, foo x)

Since we are generating renamed syntax, we can emit a call to the selector
function, even if it is not technically in scope (due to NoFieldSelectors).
When RecordDotSyntax is implemented, it will redefine the meaning of record
update syntax, but here we really need the "traditional" version.

We use a record update expression, rather than generating an explicit case
statement, because type-checking the explicit case statement is expensive for
datatypes with many constructors and/or many fields.

An annoying wrinkle is #2595: we cannot use record update for some GADTs, even
though the desugaring is in principle type-correct, because the type-checker
rejects the update.  Thus we *do* generate an explicit case statement when the
constructor has existential type variables.  For example, we generate:

    data S a where
      MkS :: { soo :: Either p q, f :: Int } -> S (p,q)

    $sel:soo:MkS :: S (p,q) -> Either p q
    $sel:soo:MkS MkS{soo=x} = x

    $upd:soo:MkS :: S (p,q) -> (Either p q -> S (p,q), Either p q)
    $upd:soo:MkS x = (\ y -> case x of { MkS{soo=x0,f=x1} -> MkS{soo=y,f=x1} }
                     , $sel:soo:MkS x
                     )


Note [Naughty record updaters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are a few cases in which we cannot generate an updater for a field:

1. The field has an existential tyvar that would escape its scope, e.g.
     data T = forall a . MkT { foo :: a }
   This is the same as for selectors (see Note [Naughty record selectors]).

2. The field is higher-rank, e.g.
     data T = MkT { foo :: forall a . a -> a }
   as this would require an impredicative instantiation of (,).

3. The field kind is not Type, e.g.
     data T = MkT { foo :: Addr# }
   as this would require an ill-kinded application of (,).

Every field with a naughty record selector also has a naughty record updater
(because the condition 1 is the same for both).  However, some types will have a
naughty updater but a regular selector (where conditions 2 or 3 apply).

If any of these apply, we bind $upd:foo:MkT to (), just as a naughty record
selector is bound to (). This means that when trying to generate a HasField
instance, we need to check if the updater is () and if so give up.


Note [Generating updaters in advance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We generate a selector and updater for every field when compiling a datatype
declaration, even though this costs a bit of type-checking time.  Why don't we
do this at HasField constraint solving time instead, at least for updaters?

 * For types with bangs and unboxing, it's not entirely simple to generate the
   selector or updater in Core form directly. The current mechanism generates
   them in source code form and compiles it.

 * Some uses of selectors and updaters will inline, but some will not, e.g.
     f x = (x.a, x.b)  -- RecordDotSyntax notation, or
     g x = setField @"a" x ()
   and making a new copy of a perhaps complex updater every time seems
   wasteful.  As soon as we have an updater that is not inlined at 2 or more
   call sites, we win (even on compile time) vs inlining it everywhere


Note [No updaters for pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For record pattern synonyms, we generate a selector function, but not an
updater.  In principle it would be possible to build an updater for
bidirectional pattern synonyms, but not for unidirectional ones.  In any case,
the updater function is not necessary because we do not solve HasField
constraints for fields defined by pattern synonyms.

That is, given

    pattern MkPair{x,y} = (x, y)

you can use `x` as a "record selector" in an expression.  But the constraint
solver will not automatically solve constraints like `HasField "x" (a, b) a`, so
you cannot directly use expressions such as `getField @"x" (True, False)` or
`setField @"x" p False`, and RecordDotSyntax will not natively support record
pattern synonyms.

This can be worked around by the user user manually writing an explicit
HasField instance, such as

   instance HasField "x" (a,b) a where
      hasField (x,y) = (\x' -> (x',y), x)

which will be subject to the usual rules around orphan instances and the
restrictions on when HasField instances can be defined (as described in
Note [Validity checking of HasField instances] in GHC.Tc.Validity).

We could imagine allowing (bidirectional) record pattern synonyms to lead to
automatic HasField constraint solving, but this potentially introduces
incoherent HasField instances, because multiple pattern synonyms (in different
modules) might use the same field name in the same type, and would even lead to
e.g.

    pattern Id{id} = id

introducing an `id` field to *every* type!

Given the possibility of incoherence, and the fact that a reasonable workaround
exists, we do not currently solve HasField constraints for fields defined by
pattern synonyms.  And since we do not need updaters for anything other than
solving HasField constraints, we do not generate them for pattern synonyms.


Note [Calling tcRecSelBinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When type-checking record update bindings, we need to be able to look up the
data constructors for the corresponding datatypes, because the constructors are
used in the definitions.  However, for data constructors in data family
instances, tcTyClsInstDecls adds placeholder bindings added to prevent the use
of promotion (see Note [AFamDataCon: not promoting data family constructors] in
GHC.Tc.Utils.Env).  Thus we cannot call tcReclSelBinds in addTyConsToGblEnv, but
instead have to wait until tcTyClsInstDecls has completed.

-}
