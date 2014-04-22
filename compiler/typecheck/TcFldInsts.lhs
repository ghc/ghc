%
% (c) Adam Gundry 2013
%

TcFldInsts: Creating instances for OverloadedRecordFields

For notes on the implementation of OverloadedRecordFields, see
https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Implementation

See also GHC.Records in the base library.

\begin{code}
module TcFldInsts ( makeOverloadedRecFldInsts ) where

#include "HsVersions.h"

import HsSyn
import TcBinds
import TcInstDcls
import TcRnMonad
import TcValidity
import TcSimplify
import TcMType
import TcType
import InstEnv
import FamInstEnv
import TcEnv
import TcExpr
import MkCore     ( pAT_ERROR_ID )
import Type
import TysWiredIn
import TypeRep
import TyCon
import CoAxiom
import DataCon
import Var
import VarSet
import PrelNames

import Bag
import BasicTypes
import FastString
import Id
import MkId
import IdInfo
import Name
import NameSet
import RdrName
import Outputable
import SrcLoc
import Util

import Maybes     ( isNothing )
import qualified Data.ByteString as BS
\end{code}


Note [Instance scoping for OverloadedRecordFields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For the OverloadedRecordFields classes and type families, the
instances in scope for a given module correspond exactly to the fields
in scope in that module. To achieve this, instances are not exported
using the normal mechanism (extending tcg_insts and
tcg_fam_insts). Instead, only the dfun ids and axioms are exported
(via tcg_binds for dfuns, and tcg_axioms for axioms). Special code in
the constraint solver looks up the relevant instances.

The difference between tcg_fam_insts and tcg_axioms is that the former
will export the family instance as well as the underlying axiom,
whereas the latter will export only the underlying axiom. Similar
distinctions arise in ModGuts and the InteractiveContext.


Note [Availability of type-changing update]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When generating instances of the Upd class and the UpdTy family for a
field `f` of a datatype `T a b c`, we must decide which variables may
be changed when the field is updated. For example, in

    data T a b c = MkT { foo :: (a, b), bar :: a }

an update to `foo` must keep `a` the same, since `a` occurs in the
type of `bar`, but the update may change `b`.  Thus we generate:

    instance t ~ (a, b') => Upd (T a b c) "foo" t
    type instance UpdTy (T a b c) "foo" (a, b') = T a b' c

As `c` does not occur in the type of `foo`, updates must keep it the
same. This is slightly annoying, because a traditional record update
`r { foo = (x, y) }` could change the type. It is a consequence of the
fact that

    type instance UpdTy (T a b c) "foo" (a, b') = T a b' c'

makes no sense, because `c'` isn't bound anywhere.

In general, a type variable can be changed when a field is updated
provided that:

(1) It is not 'fixed', i.e. it does not occur in the type of a
    different field of a relevant data constructor, just as in
    Note [Type of a record update] in TcExpr. (A relevant data
    constructor is one that has the field being updated.)
    In the example above, `a` is fixed.

(2) It occurs in the type of the field being updated. In the example
    above, `c` does not occur in the type of the field.

(3) At least one of the variable's occurrences in the field type is
    'rigid' (not under a type family).

For an example of why (3) restricts update to variables with at least
one rigid occurrence, consider the following:

    type family G a
    data T a = MkT { foo :: G a }

Without the restriction, we would generate this:

    type instance UpdTy (T a) "foo" (G b) = T b

But we can't sensibly pattern-match on type families!

On the other hand, this is okay:

    data U a = MkU { foo :: a -> G a }

While we cannot match on the type family, we can replace it with an
unused variable, and make use of the rigid occurrence:

    type instance UpdTy (U a) "foo" (b -> z) = U b


Note that we have to be particularly careful with kind variables when
PolyKinds is enabled, since the conditions above apply also to them.
Consider the following definition, with kinds made explicit:

    data FC (x :: BOX)(y :: BOX)(f :: x -> *)(g :: y -> x)(a :: y) :: * where
        FC :: { runFC :: f (g a) } -> FC x y f g a

The obvious UpdTy instance is this:

    type instance UpdTy (FC x y f g a) "runFC" (f' (g' a')) = FC x' y' f' g' a'

But this is bogus, because the kind variables x' and y' are not bound
on the left-hand side!

Similarly, kind variables may or may not be fixed. In the following
example, updates to fields of U may change their types or kinds, while
updates to fields of V may change the types but not the kinds:

    data T (a :: x -> *)(b :: x) :: * where
        MkT :: a b -> T a b

    data U (a :: x -> *)(b :: x)(c :: y -> *)(d :: y)
        = MkU { bar :: T a b, baz :: T c d }

    data V (a :: x -> *)(b :: x)(c :: x -> *)(d :: x)
        = MkV { bar :: T a b, baz :: T c d }


\begin{code}
-- | Contains Has and Upd class instances, and FldTy and UpdTy axioms,
-- in that order. Left means that they are bogus (because the field is
-- higher-rank or existential); Right gives the real things.
type FldInstDetails = Either (Name, Name, Name, Name)
                             (InstInfo Name, InstInfo Name,
                                 CoAxiom Unbranched, CoAxiom Unbranched)

-- | Create and typecheck instances from datatype and data instance
-- declarations in the module being compiled.
makeOverloadedRecFldInsts :: [TyClGroup Name] -> [LInstDecl Name]
                          -> TcM TcGblEnv
makeOverloadedRecFldInsts tycl_decls inst_decls
    = do { fld_insts <- mapM makeRecFldInstsFor flds'
         ; tcFldInsts fld_insts }
  where
    (_, flds) = hsTyClDeclsBinders tycl_decls inst_decls
    flds'     = map (\ (x, y, z) -> (occNameFS (rdrNameOcc x), y, z)) flds


-- | Given a (label, selector name, tycon name) triple, construct the
-- appropriate Has, Upd, FldTy and UpdTy instances.
makeRecFldInstsFor :: (FieldLabelString, Name, Name) -> TcM (Name, FldInstDetails)
makeRecFldInstsFor (lbl, sel_name, tycon_name)
  = do { rep_tc <- lookupRepTyConOfSelector tycon_name sel_name

       -- Find a relevant data constructor (one that has this field)
       -- and extract information from the FieldLabel.
       ; let relevant_cons = tyConDataConsWithFields rep_tc [lbl]
             dc            = ASSERT (notNull relevant_cons) head relevant_cons
             (fl, fld_ty0) = dataConFieldLabel dc lbl
             data_ty0      = dataConOrigResTy dc
             is_existential = not (tyVarsOfType fld_ty0
                                      `subVarSet` tyVarsOfType data_ty0)
             FieldLabel _ _ _ has_name upd_name get_name set_name = fl

       -- If the field is universally or existentially quantified,
       -- don't generate any instances.
       ; (_, mb) <- tryTc (checkValidMonoType fld_ty0)
       ; if isNothing mb || is_existential
         then return (sel_name, Left (has_name, upd_name, get_name, set_name))
         else do

           -- Freshen the type variables in the constituent types
           { let univ_tvs     = dataConUnivTyVars dc
           ; (subst0, tyvars) <- tcInstSkolTyVars (univ_tvs ++ dataConExTyVars dc)
           ; let n            = mkStrLitTy lbl
                 r            = substTy subst0 (mkFamilyTyConApp rep_tc
                                                   (mkTyVarTys univ_tvs))
                 data_ty      = substTy subst0 data_ty0
                 fld_ty       = substTy subst0 fld_ty0
                 eq_spec      = substTys subst0 (eqSpecPreds (dataConEqSpec dc))
                 stupid_theta = substTys subst0 (dataConStupidTheta dc)
           ; b <- mkTyVar <$> newSysName (mkVarOcc "b") <*> pure liftedTypeKind

           -- Generate Has instance:
           --     instance (b ~ fld_ty, theta) => Has r n b
           ; has_inst <- mkHasInstInfo has_name sel_name lbl n tyvars
                             (eq_spec ++ stupid_theta) r fld_ty b

           -- Generate FldTy instance:
           --     type instance FldTy data_ty n = fld_ty
           ; get_ax <- mkAxiom get_name fldTyFamName [data_ty, n] fld_ty

           -- Generate Upd instance:
           --     instance (b ~ fld_ty', theta) => Upd r n b
           -- See Note [Availability of type-changing update]
           ; (subst, tyvars') <- updatingSubst lbl relevant_cons tyvars
                                     (rigidTyVarsOfType fld_ty)
           ; let fld_ty'  = substTy subst fld_ty
                 data_ty' = substTy subst data_ty
                 stupid_theta' = substTys subst stupid_theta
           ; upd_inst <- mkUpdInstInfo upd_name lbl n
                             (eq_spec ++ stupid_theta ++ stupid_theta')
                             r b tyvars' fld_ty' relevant_cons rep_tc

           -- Generate UpdTy instance:
           --     type instance UpdTy data_ty n hull_ty = data_ty'
           -- See Note [Calculating the hull type]
           ; hull_ty <- hullType fld_ty'
           ; set_ax  <- mkAxiom set_name updTyFamName
                            [data_ty, n, hull_ty] data_ty'

           -- ; dumpDerivingInfo (hang (text "Overloaded record field instances:")
           --                  2 (vcat [ppr has_inst, ppr get_ax,
           --                           ppr upd_inst, ppr set_ax]))

           ; return (sel_name, Right (has_inst, upd_inst, get_ax, set_ax)) } }

  where

    -- | Make InstInfo for Has thus:
    --     instance forall b tyvars . (b ~ fld_ty, theta) => Has t n b where
    --         getField _ = sel_name
    mkHasInstInfo dfun_name sel_name lbl n tyvars theta t fld_ty b
        = do { hasClass <- tcLookupClass recordHasClassName
             ; let theta' = mkEqPred (mkTyVarTy b) fld_ty : theta
                   dfun   = mkDictFunId dfun_name (b:tyvars) theta' hasClass args
             ; cls_inst <- mkFreshenedClsInst dfun (b:tyvars) hasClass args
             ; return (InstInfo cls_inst inst_bind) }
      where
        args = [t, n, mkTyVarTy b]
        inst_bind = InstBindings bind [] [] True
          where
            bind  = unitBag $ noLoc $ (mkTopFunBind Generated (noLoc getFieldName) [match])
                                          { bind_fvs = placeHolderNames }
            match = mkSimpleMatch [nlWildPat]
                        (noLoc (HsSingleRecFld (mkVarUnqual lbl) sel_name))


    -- | Make InstInfo for Upd thus:
    --     instance forall b tyvars' . (b ~ fld_ty', theta) => Upd t n b where
    --         setField _ (MkT fld1 ... fldn) x = MkT fld1 ... x ... fldn
    --  fld_ty' is fld_ty with fresh tyvars (if type-changing update is possible)
    --  It would be nicer to use record-update syntax, but that isn't
    --  possible because of Trac #2595.
    mkUpdInstInfo dfun_name lbl n theta t b tyvars' fld_ty' relevant_cons rep_tc
        = do { updClass   <- tcLookupClass recordUpdClassName
             ; let args   = [t, n, mkTyVarTy b]
                   theta' = mkEqPred (mkTyVarTy b) fld_ty' : theta
                   dfun   = mkDictFunId dfun_name (b:tyvars') theta' updClass args
             ; cls_inst   <- mkFreshenedClsInst dfun (b:tyvars') updClass args
             ; matches    <- mapM matchCon relevant_cons
             ; return (InstInfo cls_inst (inst_bind matches)) }
      where
        matchCon con
          = do { x <- newSysName (mkVarOcc "x")
               ; vars <- mapM (newSysName . mkVarOccFS . flLabel) (dataConFieldLabels con)
               ; let con_name = dataConName con
                     vars'    = map replace_lbl vars
                     replace_lbl v = if occNameFS (nameOccName v) == lbl then x else v
               ; return $ mkSimpleMatch [nlWildPat, nlConVarPat con_name vars, nlVarPat x]
                                        (nlHsVarApps con_name vars') }

        inst_bind matches = InstBindings bind [] [] True
          where
            bind = unitBag $ noLoc $ (mkTopFunBind Generated (noLoc setFieldName) all_matches)
                                         { bind_fvs = placeHolderNames }
            all_matches | all dealt_with cons = matches
                        | otherwise           = matches ++ [default_match]
            default_match = mkSimpleMatch [nlWildPat, nlWildPat, nlWildPat] $
                                nlHsApp (nlHsVar (getName pAT_ERROR_ID))
                                        (nlHsLit (HsStringPrim msg))
            msg = unsafeMkByteString "setField|overloaded record update: "
                      `BS.append` fastStringToByteString lbl
            cons = tyConDataCons rep_tc
            dealt_with con = con `elem` relevant_cons
                                 || dataConCannotMatch inst_tys con
            inst_tys = substTyVars (mkTopTvSubst (dataConEqSpec dc))
                                   (dataConUnivTyVars dc)
            dc = head relevant_cons


    -- | Make a class instance with freshened type variables.
    -- See Note [Template tyvars are fresh] in InstEnv.
    mkFreshenedClsInst dfun tyvars clas tys
      = do { (subst, tyvars') <- tcInstSkolTyVars tyvars
           ; return $ mkLocalInstance dfun (NoOverlap False) tyvars' clas
                          (substTys subst tys) }


    -- | Make an axiom corresponding to the type family instance
    --    type instance fam_name args = result
    mkAxiom ax_name fam_name args result
      = do { fam <- tcLookupTyCon fam_name
           ; let tyvars = varSetElems (tyVarsOfTypes (result:args))
           ; (subst, tyvars') <- tcInstSkolTyVars tyvars
           ; return $ mkSingleCoAxiom ax_name tyvars' fam (substTys subst args)
                                                          (substTy subst result) }


-- | Given a tycon name and a record selector belonging to that tycon,
-- return the representation tycon that contains the selector.
lookupRepTyConOfSelector :: Name -> Name -> TcM TyCon
lookupRepTyConOfSelector tycon_name sel_name
  = do { tc <- tcLookupTyCon tycon_name
       ; if (isDataFamilyTyCon tc)
         then do { sel_id <- tcLookupId sel_name
                 ; ASSERT (isRecordSelector sel_id)
                   return (recordSelectorTyCon sel_id) }
         else return tc }

-- | Compute a substitution that replaces each tyvar with a fresh
-- variable, if it can be updated; also returns a list of all the
-- tyvars (old and new). See Note [Availability of type-changing update]
updatingSubst :: FieldLabelString -> [DataCon] -> [TyVar] -> TyVarSet ->
                         TcM (TvSubst, [TyVar])
updatingSubst lbl relevant_cons tyvars fld_tvs
      = do { (subst, tyvarss) <- mapAccumLM updateTyVar emptyTvSubst tyvars
           ; return (subst, concat tyvarss) }
      where
        fixed_tvs    = getFixedTyVars [lbl] tyvars relevant_cons
        changeable x = x `elemVarSet` fld_tvs && not (x `elemVarSet` fixed_tvs)

        updateTyVar :: TvSubst -> TyVar -> TcM (TvSubst, [TyVar])
        updateTyVar subst tv
          | changeable tv = do { (subst', tv') <- tcInstSkolTyVar noSrcSpan False subst tv
                               ; return (subst', [tv,tv']) }
          | otherwise     = return (subst, [tv])


rigidTyVarsOfType :: Type -> VarSet
-- ^ Returns free type (not kind) variables of a type, that are not
-- under a type family application.
rigidTyVarsOfType (TyVarTy v)         = unitVarSet v
rigidTyVarsOfType (TyConApp tc tys)   | isDecomposableTyCon tc = rigidTyVarsOfTypes tys
                                      | otherwise              = emptyVarSet
rigidTyVarsOfType (LitTy {})          = emptyVarSet
rigidTyVarsOfType (FunTy arg res)     = rigidTyVarsOfType arg `unionVarSet` rigidTyVarsOfType res
rigidTyVarsOfType (AppTy fun arg)     = rigidTyVarsOfType fun `unionVarSet` rigidTyVarsOfType arg
rigidTyVarsOfType (ForAllTy tyvar ty) = delVarSet (rigidTyVarsOfType ty) tyvar
                                            `unionVarSet` rigidTyVarsOfType (tyVarKind tyvar)

rigidTyVarsOfTypes :: [Type] -> TyVarSet
rigidTyVarsOfTypes tys = foldr (unionVarSet . rigidTyVarsOfType) emptyVarSet tys
\end{code}


Note [Calculating the hull type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
UpdTy must not pattern-match on type families (see Note
[Availability of type-changing update]). For example, given the
datatype

    data T a b = MkT { foo :: (a, Int, F b) }

we generate

    type instance UpdTy (T a b) "foo" (a', Int, x) = T a' b

rather than

    type instance UpdTy (T a b) "foo" (a', Int, F b') = T a' b'.

This is accomplished by the `hullType` function, which returns a type
in which all the type family subexpressions have been replaced with
fresh variables.

\begin{code}
hullType :: Type -> TcM Type
hullType ty@(TyVarTy _)      = return ty
hullType (AppTy f s)         = AppTy <$> hullType f <*> hullType s
hullType ty@(TyConApp tc tys)
  | isDecomposableTyCon tc   = TyConApp tc <$> mapM hullType tys
  | otherwise                = mkTyVarTy <$> (mkTyVar <$> newSysName (mkVarOcc "x")
                                                      <*> pure (typeKind ty))
hullType (FunTy t u)         = FunTy <$> hullType t <*> hullType u
hullType (ForAllTy v ty)     = ForAllTy v <$> hullType ty
hullType ty@(LitTy _)        = return ty
\end{code}


Note [Bogus instances]
~~~~~~~~~~~~~~~~~~~~~~
When a field's type is universally or existentially quantified, we
cannot generate instances for it.  Just like naughty record selectors
(see Note [Naughty record selectors] in TcTyClsDcls), we build bogus
Ids in place of such instances, so that we can detect this when
looking for them. This means we have to be a little careful when
looking up the instances: the bogus Ids are just vanilla bindings of
(), not DFunIds or CoAxioms.

\begin{code}
-- | Typecheck the generated Has, Upd, FldTy and UpdTy instances.
-- This adds the dfuns and axioms to the global environment, but does
-- not add user-visible instances.
tcFldInsts :: [(Name, FldInstDetails)] -> TcM TcGblEnv
tcFldInsts fld_insts
    = updGblEnv (\env -> env { tcg_axioms = axioms ++ tcg_axioms env }) $
        tcExtendGlobalEnvImplicit things $
                 -- Invoke the constraint solver to find uses of
                 -- fields now rather than later
              do { (binds, lie) <- captureConstraints $ tcInstDecls2 [] inst_infos
                 ; ev_binds     <- simplifyTop lie

                 -- See Note [Bogus instances]
                 ; let (bogus_sigs, bogus_binds) = mapAndUnzip mkBogusId bogus_insts
                 ; env <- tcRecSelBinds $ ValBindsOut bogus_binds bogus_sigs

                   -- Don't count the generated instances as uses of the field
                 ; updMutVar (tcg_used_selectors env)
                             (\s -> delListFromNameSet s (map fst fld_insts))

                 ; ASSERT2( isEmptyBag ev_binds , ppr ev_binds)
                   return $ env { tcg_binds = tcg_binds env `unionBags` binds } }
  where
    has_upd (_, Right (has, upd, _, _)) = [has, upd]
    has_upd _                           = []

    get_set (_, Right (_, _, get, set)) = [get, set]
    get_set _                           = []

    inst_infos = concatMap has_upd fld_insts
    axioms     = concatMap (map toBranchedAxiom . get_set) fld_insts
    things     = map ACoAxiom axioms
                     ++ map (AnId . is_dfun . iSpec) inst_infos

    bogus (_, Left (has, upd, get, set)) = [has, upd, get, set]
    bogus _            = []
    bogus_insts = concatMap bogus fld_insts

    mkBogusId :: Name -> (LSig Name, (RecFlag, LHsBinds Name))
    mkBogusId n = (noLoc (IdSig bogus_id), (NonRecursive, unitBag (noLoc bind)))
      where
        bogus_id = mkExportedLocalVar VanillaId n unitTy vanillaIdInfo
        bind     = mkTopFunBind Generated (noLoc n) [mkSimpleMatch [] (mkLHsTupleExpr [])]
\end{code}
