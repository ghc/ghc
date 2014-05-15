%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1996-1998
%

TcTyClsDecls: Typecheck type and class declarations

\begin{code}
{-# LANGUAGE CPP, TupleSections #-}

module TcTyClsDecls (
        tcTyAndClassDecls, tcAddImplicits,

        -- Functions used by TcInstDcls to check
        -- data/type family instance declarations
        kcDataDefn, tcConDecls, dataDeclChecks, checkValidTyCon,
        tcSynFamInstDecl, tcFamTyPats,
        tcAddTyFamInstCtxt, tcAddDataFamInstCtxt,
        wrongKindOfFamily, dataConCtxt, badDataConTyCon
    ) where

#include "HsVersions.h"

import HsSyn
import HscTypes
import BuildTyCl
import TcRnMonad
import TcEnv
import TcValidity
import TcHsSyn
import TcBinds( tcRecSelBinds )
import FunDeps( growThetaTyVars )
import TcTyDecls
import TcClassDcl
import TcHsType
import TcMType
import TcType
import TysWiredIn( unitTy )
import FamInst
import FamInstEnv( isDominatedBy, mkCoAxBranch, mkBranchedCoAxiom )
import Coercion( pprCoAxBranch, ltRole )
import Type
import TypeRep   -- for checkValidRoles
import Kind
import Class
import CoAxiom
import TyCon
import DataCon
import Id
import MkCore           ( rEC_SEL_ERROR_ID )
import IdInfo
import Var
import VarEnv
import VarSet
import Module
import Name
import NameSet
import NameEnv
import Outputable
import Maybes
import Unify
import Util
import SrcLoc
import ListSetOps
import Digraph
import DynFlags
import FastString
import Unique           ( mkBuiltinUnique )
import BasicTypes

import Bag
import Control.Monad
import Data.List
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Type checking for type and class declarations}
%*                                                                      *
%************************************************************************

Note [Grouping of type and class declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcTyAndClassDecls is called on a list of `TyClGroup`s. Each group is a strongly
connected component of mutually dependent types and classes. We kind check and
type check each group separately to enhance kind polymorphism. Take the
following example:

  type Id a = a
  data X = X (Id Int)

If we were to kind check the two declarations together, we would give Id the
kind * -> *, since we apply it to an Int in the definition of X. But we can do
better than that, since Id really is kind polymorphic, and should get kind
forall (k::BOX). k -> k. Since it does not depend on anything else, it can be
kind-checked by itself, hence getting the most general kind. We then kind check
X, which works fine because we then know the polymorphic kind of Id, and simply
instantiate k to *.

Note [Check role annotations in a second pass]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Role inference potentially depends on the types of all of the datacons declared
in a mutually recursive group. The validity of a role annotation, in turn,
depends on the result of role inference. Because the types of datacons might
be ill-formed (see #7175 and Note [Checking GADT return types]) we must check
*all* the tycons in a group for validity before checking *any* of the roles.
Thus, we take two passes over the resulting tycons, first checking for general
validity and then checking for valid role annotations.

\begin{code}

tcTyAndClassDecls :: ModDetails
                  -> [TyClGroup Name]   -- Mutually-recursive groups in dependency order
                  -> TcM TcGblEnv       -- Input env extended by types and classes
                                        -- and their implicit Ids,DataCons
-- Fails if there are any errors
tcTyAndClassDecls boot_details tyclds_s
  = checkNoErrs $       -- The code recovers internally, but if anything gave rise to
                        -- an error we'd better stop now, to avoid a cascade
    fold_env tyclds_s   -- Type check each group in dependency order folding the global env
  where
    fold_env :: [TyClGroup Name] -> TcM TcGblEnv
    fold_env [] = getGblEnv
    fold_env (tyclds:tyclds_s)
      = do { tcg_env <- tcTyClGroup boot_details tyclds
           ; setGblEnv tcg_env $ fold_env tyclds_s }
             -- remaining groups are typecheck in the extended global env

tcTyClGroup :: ModDetails -> TyClGroup Name -> TcM TcGblEnv
-- Typecheck one strongly-connected component of type and class decls
tcTyClGroup boot_details tyclds
  = do {    -- Step 1: kind-check this group and returns the final
            -- (possibly-polymorphic) kind of each TyCon and Class
            -- See Note [Kind checking for type and class decls]
         names_w_poly_kinds <- kcTyClGroup tyclds
       ; traceTc "tcTyAndCl generalized kinds" (ppr names_w_poly_kinds)

            -- Step 2: type-check all groups together, returning
            -- the final TyCons and Classes
       ; let role_annots = extractRoleAnnots tyclds
             decls = group_tyclds tyclds
       ; tyclss <- fixM $ \ rec_tyclss -> do
           { is_boot <- tcIsHsBoot
           ; let rec_flags = calcRecFlags boot_details is_boot
                                          role_annots rec_tyclss

                 -- Populate environment with knot-tied ATyCon for TyCons
                 -- NB: if the decls mention any ill-staged data cons
                 -- (see Note [Recusion and promoting data constructors]
                 -- we will have failed already in kcTyClGroup, so no worries here
           ; tcExtendRecEnv (zipRecTyClss names_w_poly_kinds rec_tyclss) $

                 -- Also extend the local type envt with bindings giving
                 -- the (polymorphic) kind of each knot-tied TyCon or Class
                 -- See Note [Type checking recursive type and class declarations]
             tcExtendKindEnv names_w_poly_kinds              $

                 -- Kind and type check declarations for this group
             concatMapM (tcTyClDecl rec_flags) decls }

           -- Step 3: Perform the validity check
           -- We can do this now because we are done with the recursive knot
           -- Do it before Step 4 (adding implicit things) because the latter
           -- expects well-formed TyCons
       ; tcExtendGlobalEnv tyclss $ do
       { traceTc "Starting validity check" (ppr tyclss)
       ; checkNoErrs $
         mapM_ (recoverM (return ()) . checkValidTyCl) tyclss
           -- We recover, which allows us to report multiple validity errors
           -- the checkNoErrs is necessary to fix #7175.
       ; mapM_ (recoverM (return ()) . checkValidRoleAnnots role_annots) tyclss
           -- See Note [Check role annotations in a second pass]

           -- Step 4: Add the implicit things;
           -- we want them in the environment because
           -- they may be mentioned in interface files
       ; tcExtendGlobalValEnv (mkDefaultMethodIds tyclss) $
         tcAddImplicits tyclss } }

tcAddImplicits :: [TyThing] -> TcM TcGblEnv
tcAddImplicits tyclss
 = tcExtendGlobalEnvImplicit implicit_things $
   tcRecSelBinds rec_sel_binds
 where
   implicit_things = concatMap implicitTyThings tyclss
   rec_sel_binds   = mkRecSelBinds tyclss

zipRecTyClss :: [(Name, Kind)]
             -> [TyThing]           -- Knot-tied
             -> [(Name,TyThing)]
-- Build a name-TyThing mapping for the things bound by decls
-- being careful not to look at the [TyThing]
-- The TyThings in the result list must have a visible ATyCon,
-- because typechecking types (in, say, tcTyClDecl) looks at this outer constructor
zipRecTyClss kind_pairs rec_things
  = [ (name, ATyCon (get name)) | (name, _kind) <- kind_pairs ]
  where
    rec_type_env :: TypeEnv
    rec_type_env = mkTypeEnv rec_things

    get name = case lookupTypeEnv rec_type_env name of
                 Just (ATyCon tc) -> tc
                 other            -> pprPanic "zipRecTyClss" (ppr name <+> ppr other)
\end{code}


%************************************************************************
%*                                                                      *
                Kind checking
%*                                                                      *
%************************************************************************

Note [Kind checking for type and class decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Kind checking is done thus:

   1. Make up a kind variable for each parameter of the *data* type, class,
      and closed type family decls, and extend the kind environment (which is
      in the TcLclEnv)

   2. Dependency-analyse the type *synonyms* (which must be non-recursive),
      and kind-check them in dependency order.  Extend the kind envt.

   3. Kind check the data type and class decls

Synonyms are treated differently to data type and classes,
because a type synonym can be an unboxed type
        type Foo = Int#
and a kind variable can't unify with UnboxedTypeKind
So we infer their kinds in dependency order

We need to kind check all types in the mutually recursive group
before we know the kind of the type variables.  For example:

  class C a where
     op :: D b => a -> b -> b

  class D c where
     bop :: (Monad c) => ...

Here, the kind of the locally-polymorphic type variable "b"
depends on *all the uses of class D*.  For example, the use of
Monad c in bop's type signature means that D must have kind Type->Type.

However type synonyms work differently.  They can have kinds which don't
just involve (->) and *:
        type R = Int#           -- Kind #
        type S a = Array# a     -- Kind * -> #
        type T a b = (# a,b #)  -- Kind * -> * -> (# a,b #)
So we must infer their kinds from their right-hand sides *first* and then
use them, whereas for the mutually recursive data types D we bring into
scope kind bindings D -> k, where k is a kind variable, and do inference.

Open type families
~~~~~~~~~~~~~~~~~~
This treatment of type synonyms only applies to Haskell 98-style synonyms.
General type functions can be recursive, and hence, appear in `alg_decls'.

The kind of an open type family is solely determinded by its kind signature;
hence, only kind signatures participate in the construction of the initial
kind environment (as constructed by `getInitialKind'). In fact, we ignore
instances of families altogether in the following. However, we need to include
the kinds of *associated* families into the construction of the initial kind
environment. (This is handled by `allDecls').

\begin{code}
kcTyClGroup :: TyClGroup Name -> TcM [(Name,Kind)]
-- Kind check this group, kind generalize, and return the resulting local env
-- This bindds the TyCons and Classes of the group, but not the DataCons
-- See Note [Kind checking for type and class decls]
kcTyClGroup (TyClGroup { group_tyclds = decls })
  = do  { mod <- getModule
        ; traceTc "kcTyClGroup" (ptext (sLit "module") <+> ppr mod $$ vcat (map ppr decls))

          -- Kind checking;
          --    1. Bind kind variables for non-synonyms
          --    2. Kind-check synonyms, and bind kinds of those synonyms
          --    3. Kind-check non-synonyms
          --    4. Generalise the inferred kinds
          -- See Note [Kind checking for type and class decls]

          -- Step 1: Bind kind variables for non-synonyms
        ; let (syn_decls, non_syn_decls) = partition (isSynDecl . unLoc) decls
        ; initial_kinds <- getInitialKinds non_syn_decls
        ; traceTc "kcTyClGroup: initial kinds" (ppr initial_kinds)

        -- Step 2: Set initial envt, kind-check the synonyms
        ; lcl_env <- tcExtendKindEnv2 initial_kinds $
                     kcSynDecls (calcSynCycles syn_decls)

        -- Step 3: Set extended envt, kind-check the non-synonyms
        ; setLclEnv lcl_env $
          mapM_ kcLTyClDecl non_syn_decls

             -- Step 4: generalisation
             -- Kind checking done for this group
             -- Now we have to kind generalize the flexis
        ; res <- concatMapM (generaliseTCD (tcl_env lcl_env)) decls

        ; traceTc "kcTyClGroup result" (ppr res)
        ; return res }

  where
    generalise :: TcTypeEnv -> Name -> TcM (Name, Kind)
    -- For polymorphic things this is a no-op
    generalise kind_env name
      = do { let kc_kind = case lookupNameEnv kind_env name of
                               Just (AThing k) -> k
                               _ -> pprPanic "kcTyClGroup" (ppr name $$ ppr kind_env)
           ; kvs <- kindGeneralize (tyVarsOfType kc_kind)
           ; kc_kind' <- zonkTcKind kc_kind    -- Make sure kc_kind' has the final,
                                               -- skolemised kind variables
           ; traceTc "Generalise kind" (vcat [ ppr name, ppr kc_kind, ppr kvs, ppr kc_kind' ])
           ; return (name, mkForAllTys kvs kc_kind') }

    generaliseTCD :: TcTypeEnv -> LTyClDecl Name -> TcM [(Name, Kind)]
    generaliseTCD kind_env (L _ decl)
      | ClassDecl { tcdLName = (L _ name), tcdATs = ats } <- decl
      = do { first <- generalise kind_env name
           ; rest <- mapM ((generaliseFamDecl kind_env) . unLoc) ats
           ; return (first : rest) }

      | FamDecl { tcdFam = fam } <- decl
      = do { res <- generaliseFamDecl kind_env fam
           ; return [res] }

      | ForeignType {} <- decl
      = pprPanic "generaliseTCD" (ppr decl)

      | otherwise
      = do { res <- generalise kind_env (tcdName decl)
           ; return [res] }

    generaliseFamDecl :: TcTypeEnv -> FamilyDecl Name -> TcM (Name, Kind)
    generaliseFamDecl kind_env (FamilyDecl { fdLName = L _ name })
      = generalise kind_env name

mk_thing_env :: [LTyClDecl Name] -> [(Name, TcTyThing)]
mk_thing_env [] = []
mk_thing_env (decl : decls)
  | L _ (ClassDecl { tcdLName = L _ nm, tcdATs = ats }) <- decl
  = (nm, APromotionErr ClassPE) :
    (map (, APromotionErr TyConPE) $ map (unLoc . fdLName . unLoc) ats) ++
    (mk_thing_env decls)

  | otherwise
  = (tcdName (unLoc decl), APromotionErr TyConPE) :
    (mk_thing_env decls)

getInitialKinds :: [LTyClDecl Name] -> TcM [(Name, TcTyThing)]
getInitialKinds decls
  = tcExtendKindEnv2 (mk_thing_env decls) $
    do { pairss <- mapM (addLocM getInitialKind) decls
       ; return (concat pairss) }

-- See Note [Kind-checking strategies] in TcHsType
getInitialKind :: TyClDecl Name -> TcM [(Name, TcTyThing)]
-- Allocate a fresh kind variable for each TyCon and Class
-- For each tycon, return   (tc, AThing k)
--                 where k is the kind of tc, derived from the LHS
--                       of the definition (and probably including
--                       kind unification variables)
--      Example: data T a b = ...
--      return (T, kv1 -> kv2 -> kv3)
--
-- This pass deals with (ie incorporates into the kind it produces)
--   * The kind signatures on type-variable binders
--   * The result kinds signature on a TyClDecl
--
-- ALSO for each datacon, return (dc, APromotionErr RecDataConPE)
-- Note [ARecDataCon: Recursion and promoting data constructors]
--
-- No family instances are passed to getInitialKinds

getInitialKind decl@(ClassDecl { tcdLName = L _ name, tcdTyVars = ktvs, tcdATs = ats })
  = do { (cl_kind, inner_prs) <-
           kcHsTyVarBndrs (kcStrategy decl) ktvs $
           do { inner_prs <- getFamDeclInitialKinds ats
              ; return (constraintKind, inner_prs) }
       ; let main_pr = (name, AThing cl_kind)
       ; return (main_pr : inner_prs) }

getInitialKind decl@(DataDecl { tcdLName = L _ name
                                , tcdTyVars = ktvs
                                , tcdDataDefn = HsDataDefn { dd_kindSig = m_sig
                                                           , dd_cons = cons } })
  = do { (decl_kind, _) <-
           kcHsTyVarBndrs (kcStrategy decl) ktvs $
           do { res_k <- case m_sig of
                           Just ksig -> tcLHsKind ksig
                           Nothing   -> return liftedTypeKind
              ; return (res_k, ()) }
       ; let main_pr = (name, AThing decl_kind)
             inner_prs = [ (unLoc (con_name con), APromotionErr RecDataConPE)
                         | L _ con <- cons ]
       ; return (main_pr : inner_prs) }

getInitialKind (FamDecl { tcdFam = decl }) 
  = getFamDeclInitialKind decl

getInitialKind (ForeignType { tcdLName = L _ name })
  = return [(name, AThing liftedTypeKind)]

getInitialKind decl@(SynDecl {}) 
  = pprPanic "getInitialKind" (ppr decl)

---------------------------------
getFamDeclInitialKinds :: [LFamilyDecl Name] -> TcM [(Name, TcTyThing)]
getFamDeclInitialKinds decls
  = tcExtendKindEnv2 [ (n, APromotionErr TyConPE)
                     | L _ (FamilyDecl { fdLName = L _ n }) <- decls] $
    concatMapM (addLocM getFamDeclInitialKind) decls

getFamDeclInitialKind :: FamilyDecl Name
                      -> TcM [(Name, TcTyThing)]
getFamDeclInitialKind decl@(FamilyDecl { fdLName = L _ name
                                       , fdTyVars = ktvs
                                       , fdKindSig = ksig })
  = do { (fam_kind, _) <-
           kcHsTyVarBndrs (kcStrategyFamDecl decl) ktvs $
           do { res_k <- case ksig of
                           Just k  -> tcLHsKind k
                           Nothing
                             | defaultResToStar -> return liftedTypeKind
                             | otherwise        -> newMetaKindVar
              ; return (res_k, ()) }
       ; return [ (name, AThing fam_kind) ] }
  where
    defaultResToStar = (kcStrategyFamDecl decl == FullKindSignature)

----------------
kcSynDecls :: [SCC (LTyClDecl Name)]
           -> TcM TcLclEnv -- Kind bindings
kcSynDecls [] = getLclEnv
kcSynDecls (group : groups)
  = do  { (n,k) <- kcSynDecl1 group
        ; lcl_env <- tcExtendKindEnv [(n,k)] (kcSynDecls groups)
        ; return lcl_env }

kcSynDecl1 :: SCC (LTyClDecl Name)
           -> TcM (Name,TcKind) -- Kind bindings
kcSynDecl1 (AcyclicSCC (L _ decl)) = kcSynDecl decl
kcSynDecl1 (CyclicSCC decls)       = do { recSynErr decls; failM }
                                     -- Fail here to avoid error cascade
                                     -- of out-of-scope tycons

kcSynDecl :: TyClDecl Name -> TcM (Name, TcKind)
kcSynDecl decl@(SynDecl { tcdTyVars = hs_tvs, tcdLName = L _ name
                        , tcdRhs = rhs })
  -- Returns a possibly-unzonked kind
  = tcAddDeclCtxt decl $
    do { (syn_kind, _) <-
           kcHsTyVarBndrs (kcStrategy decl) hs_tvs $
           do { traceTc "kcd1" (ppr name <+> brackets (ppr hs_tvs))
              ; (_, rhs_kind) <- tcLHsType rhs
              ; traceTc "kcd2" (ppr name)
              ; return (rhs_kind, ()) }
       ; return (name, syn_kind) }
kcSynDecl decl = pprPanic "kcSynDecl" (ppr decl)

------------------------------------------------------------------------
kcLTyClDecl :: LTyClDecl Name -> TcM ()
  -- See Note [Kind checking for type and class decls]
kcLTyClDecl (L loc decl)
  = setSrcSpan loc $ tcAddDeclCtxt decl $ kcTyClDecl decl

kcTyClDecl :: TyClDecl Name -> TcM ()
-- This function is used solely for its side effect on kind variables
-- NB kind signatures on the type variables and
--    result kind signature have aready been dealt with
--    by getInitialKind, so we can ignore them here.

kcTyClDecl (DataDecl { tcdLName = L _ name, tcdTyVars = hs_tvs, tcdDataDefn = defn })
  | HsDataDefn { dd_cons = cons, dd_kindSig = Just _ } <- defn
  = mapM_ (wrapLocM kcConDecl) cons
    -- hs_tvs and dd_kindSig already dealt with in getInitialKind
    -- If dd_kindSig is Just, this must be a GADT-style decl,
    --        (see invariants of DataDefn declaration)
    -- so (a) we don't need to bring the hs_tvs into scope, because the
    --        ConDecls bind all their own variables
    --    (b) dd_ctxt is not allowed for GADT-style decls, so we can ignore it

  | HsDataDefn { dd_ctxt = ctxt, dd_cons = cons } <- defn
  = kcTyClTyVars name hs_tvs $
    do  { _ <- tcHsContext ctxt
        ; mapM_ (wrapLocM kcConDecl) cons }

kcTyClDecl decl@(SynDecl {}) = pprPanic "kcTyClDecl" (ppr decl)

kcTyClDecl (ClassDecl { tcdLName = L _ name, tcdTyVars = hs_tvs
                       , tcdCtxt = ctxt, tcdSigs = sigs })
  = kcTyClTyVars name hs_tvs $
    do  { _ <- tcHsContext ctxt
        ; mapM_ (wrapLocM kc_sig)     sigs }
  where
    kc_sig (TypeSig _ op_ty)    = discardResult (tcHsLiftedType op_ty)
    kc_sig (GenericSig _ op_ty) = discardResult (tcHsLiftedType op_ty)
    kc_sig _                    = return ()

kcTyClDecl (ForeignType {}) = return ()

-- closed type families look at their equations, but other families don't
-- do anything here
kcTyClDecl (FamDecl (FamilyDecl { fdLName = L _ fam_tc_name
                                , fdInfo = ClosedTypeFamily eqns }))
  = do { k <- kcLookupKind fam_tc_name
       ; mapM_ (kcTyFamInstEqn fam_tc_name k) eqns }
kcTyClDecl (FamDecl {})    = return ()

-------------------
kcConDecl :: ConDecl Name -> TcM ()
kcConDecl (ConDecl { con_name = name, con_qvars = ex_tvs
                   , con_cxt = ex_ctxt, con_details = details
                   , con_res = res })
  = addErrCtxt (dataConCtxt name) $
    do { _ <- kcHsTyVarBndrs ParametricKinds ex_tvs $
              do { _ <- tcHsContext ex_ctxt
                 ; mapM_ (tcHsOpenType . getBangType) (hsConDeclArgTys details)
                 ; _ <- tcConRes res
                 ; return (panic "kcConDecl", ()) }
       ; return () }
\end{code}

Note [Recursion and promoting data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't want to allow promotion in a strongly connected component
when kind checking.

Consider:
  data T f = K (f (K Any))

When kind checking the `data T' declaration the local env contains the
mappings:
  T -> AThing <some initial kind>
  K -> ARecDataCon

ANothing is only used for DataCons, and only used during type checking
in tcTyClGroup.


%************************************************************************
%*                                                                      *
\subsection{Type checking}
%*                                                                      *
%************************************************************************

Note [Type checking recursive type and class declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At this point we have completed *kind-checking* of a mutually
recursive group of type/class decls (done in kcTyClGroup). However,
we discarded the kind-checked types (eg RHSs of data type decls);
note that kcTyClDecl returns ().  There are two reasons:

  * It's convenient, because we don't have to rebuild a
    kinded HsDecl (a fairly elaborate type)

  * It's necessary, because after kind-generalisation, the
    TyCons/Classes may now be kind-polymorphic, and hence need
    to be given kind arguments.

Example:
       data T f a = MkT (f a) (T f a)
During kind-checking, we give T the kind T :: k1 -> k2 -> *
and figure out constraints on k1, k2 etc. Then we generalise
to get   T :: forall k. (k->*) -> k -> *
So now the (T f a) in the RHS must be elaborated to (T k f a).

However, during tcTyClDecl of T (above) we will be in a recursive
"knot". So we aren't allowed to look at the TyCon T itself; we are only
allowed to put it (lazily) in the returned structures.  But when
kind-checking the RHS of T's decl, we *do* need to know T's kind (so
that we can correctly elaboarate (T k f a).  How can we get T's kind
without looking at T?  Delicate answer: during tcTyClDecl, we extend

  *Global* env with T -> ATyCon (the (not yet built) TyCon for T)
  *Local*  env with T -> AThing (polymorphic kind of T)

Then:

  * During TcHsType.kcTyVar we look in the *local* env, to get the
    known kind for T.

  * But in TcHsType.ds_type (and ds_var_app in particular) we look in
    the *global* env to get the TyCon. But we must be careful not to
    force the TyCon or we'll get a loop.

This fancy footwork (with two bindings for T) is only necesary for the
TyCons or Classes of this recursive group.  Earlier, finished groups,
live in the global env only.

\begin{code}
tcTyClDecl :: RecTyInfo -> LTyClDecl Name -> TcM [TyThing]
tcTyClDecl rec_info (L loc decl)
  = setSrcSpan loc $ tcAddDeclCtxt decl $
    traceTc "tcTyAndCl-x" (ppr decl) >>
    tcTyClDecl1 NoParentTyCon rec_info decl

  -- "type family" declarations
tcTyClDecl1 :: TyConParent -> RecTyInfo -> TyClDecl Name -> TcM [TyThing]
tcTyClDecl1 parent _rec_info (FamDecl { tcdFam = fd })
  = tcFamDecl1 parent fd

  -- "type" synonym declaration
tcTyClDecl1 _parent rec_info
            (SynDecl { tcdLName = L _ tc_name, tcdTyVars = tvs, tcdRhs = rhs })
  = ASSERT( isNoParent _parent )
    tcTyClTyVars tc_name tvs $ \ tvs' kind ->
    tcTySynRhs rec_info tc_name tvs' kind rhs

  -- "data/newtype" declaration
tcTyClDecl1 _parent rec_info
            (DataDecl { tcdLName = L _ tc_name, tcdTyVars = tvs, tcdDataDefn = defn })
  = ASSERT( isNoParent _parent )
    tcTyClTyVars tc_name tvs $ \ tvs' kind ->
    tcDataDefn rec_info tc_name tvs' kind defn

tcTyClDecl1 _parent rec_info
            (ClassDecl { tcdLName = L _ class_name, tcdTyVars = tvs
            , tcdCtxt = ctxt, tcdMeths = meths
            , tcdFDs = fundeps, tcdSigs = sigs
            , tcdATs = ats, tcdATDefs = at_defs })
  = ASSERT( isNoParent _parent )
    do { (clas, tvs', gen_dm_env) <- fixM $ \ ~(clas,_,_) ->
            tcTyClTyVars class_name tvs $ \ tvs' kind ->
            do { MASSERT( isConstraintKind kind )
                 -- This little knot is just so we can get
                 -- hold of the name of the class TyCon, which we
                 -- need to look up its recursiveness
               ; let tycon_name = tyConName (classTyCon clas)
                     tc_isrec = rti_is_rec rec_info tycon_name
                     roles = rti_roles rec_info tycon_name

               ; ctxt' <- tcHsContext ctxt
               ; ctxt' <- zonkTcTypeToTypes emptyZonkEnv ctxt'
                       -- Squeeze out any kind unification variables
               ; fds'  <- mapM (addLocM tc_fundep) fundeps
               ; (sig_stuff, gen_dm_env) <- tcClassSigs class_name sigs meths
               ; at_stuff <- tcClassATs class_name (AssocFamilyTyCon clas) ats at_defs
               ; mindef <- tcClassMinimalDef class_name sigs sig_stuff
               ; clas <- buildClass
                            class_name tvs' roles ctxt' fds' at_stuff
                            sig_stuff mindef tc_isrec
               ; traceTc "tcClassDecl" (ppr fundeps $$ ppr tvs' $$ ppr fds')
               ; return (clas, tvs', gen_dm_env) }

       ; let { gen_dm_ids = [ AnId (mkExportedLocalId gen_dm_name gen_dm_ty)
                            | (sel_id, GenDefMeth gen_dm_name) <- classOpItems clas
                            , let gen_dm_tau = expectJust "tcTyClDecl1" $
                                               lookupNameEnv gen_dm_env (idName sel_id)
                            , let gen_dm_ty = mkSigmaTy tvs'
                                                      [mkClassPred clas (mkTyVarTys tvs')]
                                                      gen_dm_tau
                            ]
             ; class_ats = map ATyCon (classATs clas) }

       ; return (ATyCon (classTyCon clas) : gen_dm_ids ++ class_ats ) }
         -- NB: Order is important due to the call to `mkGlobalThings' when
         --     tying the the type and class declaration type checking knot.
  where
    tc_fundep (tvs1, tvs2) = do { tvs1' <- mapM tc_fd_tyvar tvs1 ;
                                ; tvs2' <- mapM tc_fd_tyvar tvs2 ;
                                ; return (tvs1', tvs2') }
    tc_fd_tyvar name   -- Scoped kind variables are bound to unification variables
                       -- which are now fixed, so we can zonk
      = do { tv <- tcLookupTyVar name
           ; ty <- zonkTyVarOcc emptyZonkEnv tv
                  -- Squeeze out any kind unification variables
           ; case getTyVar_maybe ty of
               Just tv' -> return tv'
               Nothing  -> pprPanic "tc_fd_tyvar" (ppr name $$ ppr tv $$ ppr ty) }

tcTyClDecl1 _ _
  (ForeignType {tcdLName = L _ tc_name, tcdExtName = tc_ext_name})
  = return [ATyCon (mkForeignTyCon tc_name tc_ext_name liftedTypeKind)]
\end{code}

\begin{code}
tcFamDecl1 :: TyConParent -> FamilyDecl Name -> TcM [TyThing]
tcFamDecl1 parent
            (FamilyDecl {fdInfo = OpenTypeFamily, fdLName = L _ tc_name, fdTyVars = tvs})
  = tcTyClTyVars tc_name tvs $ \ tvs' kind -> do
  { traceTc "open type family:" (ppr tc_name)
  ; checkFamFlag tc_name
  ; let roles = map (const Nominal) tvs'
  ; tycon <- buildSynTyCon tc_name tvs' roles OpenSynFamilyTyCon kind parent
  ; return [ATyCon tycon] }

tcFamDecl1 parent
            (FamilyDecl { fdInfo = ClosedTypeFamily eqns
                        , fdLName = lname@(L _ tc_name), fdTyVars = tvs })
-- Closed type families are a little tricky, because they contain the definition
-- of both the type family and the equations for a CoAxiom.
-- Note: eqns might be empty, in a hs-boot file!
  = do { traceTc "closed type family:" (ppr tc_name)
         -- the variables in the header have no scope:
       ; (tvs', kind) <- tcTyClTyVars tc_name tvs $ \ tvs' kind ->
                         return (tvs', kind)

       ; checkFamFlag tc_name -- make sure we have -XTypeFamilies

         -- check to make sure all the names used in the equations are
         -- consistent
       ; let names = map (tfie_tycon . unLoc) eqns
       ; tcSynFamInstNames lname names

         -- process the equations, creating CoAxBranches
       ; tycon_kind <- kcLookupKind tc_name
       ; branches <- mapM (tcTyFamInstEqn tc_name tycon_kind) eqns

         -- we need the tycon that we will be creating, but it's in scope.
         -- just look it up.
       ; fam_tc <- tcLookupLocatedTyCon lname

         -- create a CoAxiom, with the correct src location. It is Vitally
         -- Important that we do not pass the branches into
         -- newFamInstAxiomName. They have types that have been zonked inside
         -- the knot and we will die if we look at them. This is OK here
         -- because there will only be one axiom, so we don't need to
         -- differentiate names.
         -- See [Zonking inside the knot] in TcHsType
       ; loc <- getSrcSpanM
       ; co_ax_name <- newFamInstAxiomName loc tc_name []

         -- mkBranchedCoAxiom will fail on an empty list of branches, but
         -- we'll never look at co_ax in this case
       ; let co_ax = mkBranchedCoAxiom co_ax_name fam_tc branches

         -- now, finally, build the TyCon
       ; let syn_rhs = if null eqns
                       then AbstractClosedSynFamilyTyCon
                       else ClosedSynFamilyTyCon co_ax
             roles   = map (const Nominal) tvs'
       ; tycon <- buildSynTyCon tc_name tvs' roles syn_rhs kind parent

       ; let result = if null eqns
                      then [ATyCon tycon]
                      else [ATyCon tycon, ACoAxiom co_ax]
       ; return result }
-- We check for instance validity later, when doing validity checking for
-- the tycon

tcFamDecl1 parent
           (FamilyDecl {fdInfo = DataFamily, fdLName = L _ tc_name, fdTyVars = tvs})
  = tcTyClTyVars tc_name tvs $ \ tvs' kind -> do
  { traceTc "data family:" (ppr tc_name)
  ; checkFamFlag tc_name
  ; extra_tvs <- tcDataKindSig kind
  ; let final_tvs = tvs' ++ extra_tvs    -- we may not need these
        roles     = map (const Nominal) final_tvs
        tycon = buildAlgTyCon tc_name final_tvs roles Nothing []
                              DataFamilyTyCon Recursive
                              False   -- Not promotable to the kind level
                              True    -- GADT syntax
                              parent
  ; return [ATyCon tycon] }

tcTySynRhs :: RecTyInfo
           -> Name
           -> [TyVar] -> Kind
           -> LHsType Name -> TcM [TyThing]
tcTySynRhs rec_info tc_name tvs kind hs_ty
  = do { env <- getLclEnv
       ; traceTc "tc-syn" (ppr tc_name $$ ppr (tcl_env env))
       ; rhs_ty <- tcCheckLHsType hs_ty kind
       ; rhs_ty <- zonkTcTypeToType emptyZonkEnv rhs_ty
       ; let roles = rti_roles rec_info tc_name
       ; tycon <- buildSynTyCon tc_name tvs roles (SynonymTyCon rhs_ty)
                                kind NoParentTyCon
       ; return [ATyCon tycon] }

tcDataDefn :: RecTyInfo -> Name
           -> [TyVar] -> Kind
           -> HsDataDefn Name -> TcM [TyThing]
  -- NB: not used for newtype/data instances (whether associated or not)
tcDataDefn rec_info tc_name tvs kind
         (HsDataDefn { dd_ND = new_or_data, dd_cType = cType
                     , dd_ctxt = ctxt, dd_kindSig = mb_ksig
                     , dd_cons = cons })
  = do { extra_tvs <- tcDataKindSig kind
       ; let final_tvs  = tvs ++ extra_tvs
             roles      = rti_roles rec_info tc_name
       ; stupid_tc_theta <- tcHsContext ctxt
       ; stupid_theta    <- zonkTcTypeToTypes emptyZonkEnv stupid_tc_theta
       ; kind_signatures <- xoptM Opt_KindSignatures
       ; is_boot         <- tcIsHsBoot  -- Are we compiling an hs-boot file?

             -- Check that we don't use kind signatures without Glasgow extensions
       ; case mb_ksig of
           Nothing   -> return ()
           Just hs_k -> do { checkTc (kind_signatures) (badSigTyDecl tc_name)
                           ; tc_kind <- tcLHsKind hs_k
                           ; checkKind kind tc_kind
                           ; return () }

       ; h98_syntax <- dataDeclChecks tc_name new_or_data stupid_theta cons

       ; tycon <- fixM $ \ tycon -> do
             { let res_ty = mkTyConApp tycon (mkTyVarTys final_tvs)
             ; data_cons <- tcConDecls new_or_data tycon (final_tvs, res_ty) cons
             ; tc_rhs <-
                 if null cons && is_boot              -- In a hs-boot file, empty cons means
                 then return totallyAbstractTyConRhs  -- "don't know"; hence totally Abstract
                 else case new_or_data of
                   DataType -> return (mkDataTyConRhs data_cons)
                   NewType  -> ASSERT( not (null data_cons) )
                                    mkNewTyConRhs tc_name tycon (head data_cons)
             ; return (buildAlgTyCon tc_name final_tvs roles cType stupid_theta tc_rhs
                                     (rti_is_rec rec_info tc_name)
                                     (rti_promotable rec_info)
                                     (not h98_syntax) NoParentTyCon) }
       ; return [ATyCon tycon] }
\end{code}

%************************************************************************
%*                                                                      *
               Typechecking associated types (in class decls)
               (including the associated-type defaults)
%*                                                                      *
%************************************************************************

Note [Associated type defaults]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following is an example of associated type defaults:
             class C a where
               data D a

               type F a b :: *
               type F a Z = [a]        -- Default
               type F a (S n) = F a n  -- Default

Note that:
  - We can have more than one default definition for a single associated type,
    as long as they do not overlap (same rules as for instances)
  - We can get default definitions only for type families, not data families

\begin{code}
tcClassATs :: Name             -- The class name (not knot-tied)
           -> TyConParent      -- The class parent of this associated type
           -> [LFamilyDecl Name] -- Associated types.
           -> [LTyFamInstDecl Name] -- Associated type defaults.
           -> TcM [ClassATItem]
tcClassATs class_name parent ats at_defs
  = do {  -- Complain about associated type defaults for non associated-types
         sequence_ [ failWithTc (badATErr class_name n)
                   | n <- map (tyFamInstDeclName . unLoc) at_defs
                   , not (n `elemNameSet` at_names) ]
       ; mapM tc_at ats }
  where
    at_names = mkNameSet (map (unLoc . fdLName . unLoc) ats)

    at_defs_map :: NameEnv [LTyFamInstDecl Name]
    -- Maps an AT in 'ats' to a list of all its default defs in 'at_defs'
    at_defs_map = foldr (\at_def nenv -> extendNameEnv_C (++) nenv
                                          (tyFamInstDeclName (unLoc at_def)) [at_def])
                        emptyNameEnv at_defs

    tc_at at = do { [ATyCon fam_tc] <- addLocM (tcFamDecl1 parent) at
                  ; let at_defs = lookupNameEnv at_defs_map (unLoc $ fdLName $ unLoc at)
                                        `orElse` []
                  ; atd <- mapM (tcDefaultAssocDecl fam_tc) at_defs
                  ; return (fam_tc, atd) }

-------------------------
tcDefaultAssocDecl :: TyCon                -- ^ Family TyCon
                   -> LTyFamInstDecl Name  -- ^ RHS
                   -> TcM CoAxBranch       -- ^ Type checked RHS and free TyVars
tcDefaultAssocDecl fam_tc (L loc decl)
  = setSrcSpan loc $
    tcAddTyFamInstCtxt decl $
    do { traceTc "tcDefaultAssocDecl" (ppr decl)
       ; tcSynFamInstDecl fam_tc decl }
    -- We check for well-formedness and validity later, in checkValidClass

-------------------------
tcSynFamInstDecl :: TyCon -> TyFamInstDecl Name -> TcM CoAxBranch
-- Placed here because type family instances appear as
-- default decls in class declarations
tcSynFamInstDecl fam_tc (TyFamInstDecl { tfid_eqn = eqn })
  = do { checkTc (isSynFamilyTyCon fam_tc) (wrongKindOfFamily fam_tc)
       ; tcTyFamInstEqn (tyConName fam_tc) (tyConKind fam_tc) eqn }

-- Checks to make sure that all the names in an instance group are the same
tcSynFamInstNames :: Located Name -> [Located Name] -> TcM ()
tcSynFamInstNames (L _ first) names
  = do { let badNames = filter ((/= first) . unLoc) names
       ; mapM_ (failLocated (wrongNamesInInstGroup first)) badNames }
  where
    failLocated :: (Name -> SDoc) -> Located Name -> TcM ()
    failLocated msg_fun (L loc name)
      = setSrcSpan loc $
        failWithTc (msg_fun name)

kcTyFamInstEqn :: Name -> Kind -> LTyFamInstEqn Name -> TcM ()
kcTyFamInstEqn fam_tc_name kind
    (L loc (TyFamInstEqn { tfie_pats = pats, tfie_rhs = hs_ty }))
  = setSrcSpan loc $
    discardResult $
    tc_fam_ty_pats fam_tc_name kind pats (discardResult . (tcCheckLHsType hs_ty))

tcTyFamInstEqn :: Name -> Kind -> LTyFamInstEqn Name -> TcM CoAxBranch
tcTyFamInstEqn fam_tc_name kind
    (L loc (TyFamInstEqn { tfie_pats = pats, tfie_rhs = hs_ty }))
  = setSrcSpan loc $
    tcFamTyPats fam_tc_name kind pats (discardResult . (tcCheckLHsType hs_ty)) $
       \tvs' pats' res_kind ->
    do { rhs_ty <- tcCheckLHsType hs_ty res_kind
       ; rhs_ty <- zonkTcTypeToType emptyZonkEnv rhs_ty
       ; traceTc "tcTyFamInstEqn" (ppr fam_tc_name <+> ppr tvs')
          -- don't print out the pats here, as they might be zonked inside the knot
       ; return (mkCoAxBranch tvs' pats' rhs_ty loc) }

kcDataDefn :: HsDataDefn Name -> TcKind -> TcM ()
-- Used for 'data instance' only
-- Ordinary 'data' is handled by kcTyClDec
kcDataDefn (HsDataDefn { dd_ctxt = ctxt, dd_cons = cons, dd_kindSig = mb_kind }) res_k
  = do  { _ <- tcHsContext ctxt
        ; checkNoErrs $ mapM_ (wrapLocM kcConDecl) cons
          -- See Note [Failing early in kcDataDefn]
        ; kcResultKind mb_kind res_k }

------------------
kcResultKind :: Maybe (LHsKind Name) -> Kind -> TcM ()
kcResultKind Nothing res_k
  = checkKind res_k liftedTypeKind
      --             type family F a
      -- defaults to type family F a :: *
kcResultKind (Just k) res_k
  = do { k' <- tcLHsKind k
       ; checkKind  k' res_k }
\end{code}

Kind check type patterns and kind annotate the embedded type variables.
     type instance F [a] = rhs

 * Here we check that a type instance matches its kind signature, but we do
   not check whether there is a pattern for each type index; the latter
   check is only required for type synonym instances.

Note [tc_fam_ty_pats vs tcFamTyPats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tc_fam_ty_pats does the type checking of the patterns, but it doesn't
zonk or generate any desugaring. It is used when kind-checking closed
type families.

tcFamTyPats type checks the patterns, zonks, and then calls thing_inside
to generate a desugaring. It is used during type-checking (not kind-checking).

Note [Failing early in kcDataDefn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to use checkNoErrs when calling kcConDecl. This is because kcConDecl
calls tcConDecl, which checks that the return type of a GADT-like constructor
is actually an instance of the type head. Without the checkNoErrs, potentially
two bad things could happen:

 1) Duplicate error messages, because tcConDecl will be called again during
    *type* checking (as opposed to kind checking)
 2) If we just keep blindly forging forward after both kind checking and type
    checking, we can get a panic in rejigConRes. See Trac #8368.

\begin{code}
-----------------
-- Note that we can't use the family TyCon, because this is sometimes called
-- from within a type-checking knot. So, we ask our callers to do a little more
-- work.
-- See Note [tc_fam_ty_pats vs tcFamTyPats]
tc_fam_ty_pats :: Name -- of the family TyCon
               -> Kind -- of the family TyCon
               -> HsWithBndrs [LHsType Name] -- Patterns
               -> (TcKind -> TcM ())       -- Kind checker for RHS
                                           -- result is ignored
               -> TcM ([Kind], [Type], Kind)
-- Check the type patterns of a type or data family instance
--     type instance F <pat1> <pat2> = <type>
-- The 'tyvars' are the free type variables of pats
--
-- NB: The family instance declaration may be an associated one,
-- nested inside an instance decl, thus
--        instance C [a] where
--          type F [a] = ...
-- In that case, the type variable 'a' will *already be in scope*
-- (and, if C is poly-kinded, so will its kind parameter).

tc_fam_ty_pats fam_tc_name kind
               (HsWB { hswb_cts = arg_pats, hswb_kvs = kvars, hswb_tvs = tvars })
               kind_checker
  = do { let (fam_kvs, fam_body) = splitForAllTys kind

         -- We wish to check that the pattern has the right number of arguments
         -- in checkValidFamPats (in TcValidity), so we can do the check *after*
         -- we're done with the knot. But, the splitKindFunTysN below will panic
         -- if there are *too many* patterns. So, we do a preliminary check here.
         -- Note that we don't have enough information at hand to do a full check,
         -- as that requires the full declared arity of the family, which isn't
         -- nearby.
       ; let max_args = length (fst $ splitKindFunTys fam_body)
       ; checkTc (length arg_pats <= max_args) $
           wrongNumberOfParmsErrTooMany max_args

         -- Instantiate with meta kind vars
       ; fam_arg_kinds <- mapM (const newMetaKindVar) fam_kvs
       ; loc <- getSrcSpanM
       ; let (arg_kinds, res_kind)
                 = splitKindFunTysN (length arg_pats) $
                   substKiWith fam_kvs fam_arg_kinds fam_body
             hs_tvs = HsQTvs { hsq_kvs = kvars
                             , hsq_tvs = userHsTyVarBndrs loc tvars }

         -- Kind-check and quantify
         -- See Note [Quantifying over family patterns]
       ; typats <- tcHsTyVarBndrs hs_tvs $ \ _ ->
                   do { kind_checker res_kind
                      ; tcHsArgTys (quotes (ppr fam_tc_name)) arg_pats arg_kinds }

       ; return (fam_arg_kinds, typats, res_kind) }

-- See Note [tc_fam_ty_pats vs tcFamTyPats]
tcFamTyPats :: Name -- of the family ToCon
            -> Kind -- of the family TyCon
            -> HsWithBndrs [LHsType Name] -- patterns
            -> (TcKind -> TcM ())         -- kind-checker for RHS
            -> ([TKVar]              -- Kind and type variables
                -> [TcType]          -- Kind and type arguments
                -> Kind -> TcM a)
            -> TcM a
tcFamTyPats fam_tc_name kind pats kind_checker thing_inside
  = do { (fam_arg_kinds, typats, res_kind)
            <- tc_fam_ty_pats fam_tc_name kind pats kind_checker
       ; let all_args = fam_arg_kinds ++ typats

            -- Find free variables (after zonking) and turn
            -- them into skolems, so that we don't subsequently
            -- replace a meta kind var with AnyK
            -- Very like kindGeneralize
       ; qtkvs <- quantifyTyVars emptyVarSet (tyVarsOfTypes all_args)

            -- Zonk the patterns etc into the Type world
       ; (ze, qtkvs') <- zonkTyBndrsX emptyZonkEnv qtkvs
       ; all_args'    <- zonkTcTypeToTypes ze all_args
       ; res_kind'    <- zonkTcTypeToType  ze res_kind

       ; traceTc "tcFamTyPats" (ppr fam_tc_name)
            -- don't print out too much, as we might be in the knot
       ; tcExtendTyVarEnv qtkvs' $
         thing_inside qtkvs' all_args' res_kind' }
\end{code}

Note [Quantifying over family patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to quantify over two different lots of kind variables:

First, the ones that come from the kinds of the tyvar args of
tcTyVarBndrsKindGen, as usual
  data family Dist a

  -- Proxy :: forall k. k -> *
  data instance Dist (Proxy a) = DP
  -- Generates  data DistProxy = DP
  --            ax8 k (a::k) :: Dist * (Proxy k a) ~ DistProxy k a
  -- The 'k' comes from the tcTyVarBndrsKindGen (a::k)

Second, the ones that come from the kind argument of the type family
which we pick up using the (tyVarsOfTypes typats) in the result of
the thing_inside of tcHsTyvarBndrsGen.
  -- Any :: forall k. k
  data instance Dist Any = DA
  -- Generates  data DistAny k = DA
  --            ax7 k :: Dist k (Any k) ~ DistAny k
  -- The 'k' comes from kindGeneralizeKinds (Any k)

Note [Quantified kind variables of a family pattern]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider   type family KindFam (p :: k1) (q :: k1)
           data T :: Maybe k1 -> k2 -> *
           type instance KindFam (a :: Maybe k) b = T a b -> Int
The HsBSig for the family patterns will be ([k], [a])

Then in the family instance we want to
  * Bring into scope [ "k" -> k:BOX, "a" -> a:k ]
  * Kind-check the RHS
  * Quantify the type instance over k and k', as well as a,b, thus
       type instance [k, k', a:Maybe k, b:k']
                     KindFam (Maybe k) k' a b = T k k' a b -> Int

Notice that in the third step we quantify over all the visibly-mentioned
type variables (a,b), but also over the implicitly mentioned kind varaibles
(k, k').  In this case one is bound explicitly but often there will be
none. The role of the kind signature (a :: Maybe k) is to add a constraint
that 'a' must have that kind, and to bring 'k' into scope.


%************************************************************************
%*                                                                      *
               Data types
%*                                                                      *
%************************************************************************

\begin{code}
dataDeclChecks :: Name -> NewOrData -> ThetaType -> [LConDecl Name] -> TcM Bool
dataDeclChecks tc_name new_or_data stupid_theta cons
  = do {   -- Check that we don't use GADT syntax in H98 world
         gadtSyntax_ok <- xoptM Opt_GADTSyntax
       ; let h98_syntax = consUseH98Syntax cons
       ; checkTc (gadtSyntax_ok || h98_syntax) (badGadtDecl tc_name)

           -- Check that the stupid theta is empty for a GADT-style declaration
       ; checkTc (null stupid_theta || h98_syntax) (badStupidTheta tc_name)

         -- Check that a newtype has exactly one constructor
         -- Do this before checking for empty data decls, so that
         -- we don't suggest -XEmptyDataDecls for newtypes
       ; checkTc (new_or_data == DataType || isSingleton cons)
                (newtypeConError tc_name (length cons))

                -- Check that there's at least one condecl,
         -- or else we're reading an hs-boot file, or -XEmptyDataDecls
       ; empty_data_decls <- xoptM Opt_EmptyDataDecls
       ; is_boot <- tcIsHsBoot  -- Are we compiling an hs-boot file?
       ; checkTc (not (null cons) || empty_data_decls || is_boot)
                 (emptyConDeclsErr tc_name)
       ; return h98_syntax }


-----------------------------------
consUseH98Syntax :: [LConDecl a] -> Bool
consUseH98Syntax (L _ (ConDecl { con_res = ResTyGADT _ }) : _) = False
consUseH98Syntax _                                             = True
                 -- All constructors have same shape

-----------------------------------
tcConDecls :: NewOrData -> TyCon -> ([TyVar], Type)
           -> [LConDecl Name] -> TcM [DataCon]
tcConDecls new_or_data rep_tycon (tmpl_tvs, res_tmpl) cons
  = mapM (addLocM  $ tcConDecl new_or_data rep_tycon tmpl_tvs res_tmpl) cons

tcConDecl :: NewOrData
          -> TyCon             -- Representation tycon
          -> [TyVar] -> Type   -- Return type template (with its template tyvars)
                               --    (tvs, T tys), where T is the family TyCon
          -> ConDecl Name
          -> TcM DataCon

tcConDecl new_or_data rep_tycon tmpl_tvs res_tmpl        -- Data types
          (ConDecl { con_name = name
                   , con_qvars = hs_tvs, con_cxt = hs_ctxt
                   , con_details = hs_details, con_res = hs_res_ty })
  = addErrCtxt (dataConCtxt name) $
    do { traceTc "tcConDecl 1" (ppr name)
       ; (ctxt, arg_tys, res_ty, is_infix, field_lbls, stricts)
           <- tcHsTyVarBndrs hs_tvs $ \ _ ->
              do { ctxt    <- tcHsContext hs_ctxt
                 ; details <- tcConArgs new_or_data hs_details
                 ; res_ty  <- tcConRes hs_res_ty
                 ; let (is_infix, field_lbls, btys) = details
                       (arg_tys, stricts)           = unzip btys
                 ; return (ctxt, arg_tys, res_ty, is_infix, field_lbls, stricts) }

             -- Generalise the kind variables (returning quantified TcKindVars)
             -- and quantify the type variables (substituting their kinds)
             -- REMEMBER: 'tkvs' are:
             --    ResTyH98:  the *existential* type variables only
             --    ResTyGADT: *all* the quantified type variables
             -- c.f. the comment on con_qvars in HsDecls
       ; tkvs <- case res_ty of
                   ResTyH98         -> quantifyTyVars (mkVarSet tmpl_tvs) (tyVarsOfTypes (ctxt++arg_tys))
                   ResTyGADT res_ty -> quantifyTyVars emptyVarSet (tyVarsOfTypes (res_ty:ctxt++arg_tys))

             -- Zonk to Types
       ; (ze, qtkvs) <- zonkTyBndrsX emptyZonkEnv tkvs
       ; arg_tys <- zonkTcTypeToTypes ze arg_tys
       ; ctxt    <- zonkTcTypeToTypes ze ctxt
       ; res_ty  <- case res_ty of
                      ResTyH98     -> return ResTyH98
                      ResTyGADT ty -> ResTyGADT <$> zonkTcTypeToType ze ty

       ; let (univ_tvs, ex_tvs, eq_preds, res_ty') = rejigConRes tmpl_tvs res_tmpl qtkvs res_ty

       ; fam_envs <- tcGetFamInstEnvs
       ; buildDataCon fam_envs (unLoc name) is_infix
                      stricts field_lbls
                      univ_tvs ex_tvs eq_preds ctxt arg_tys
                      res_ty' rep_tycon
                -- NB:  we put data_tc, the type constructor gotten from the
                --      constructor type signature into the data constructor;
                --      that way checkValidDataCon can complain if it's wrong.
       }

tcConArgs :: NewOrData -> HsConDeclDetails Name -> TcM (Bool, [Name], [(TcType, HsBang)])
tcConArgs new_or_data (PrefixCon btys)
  = do { btys' <- mapM (tcConArg new_or_data) btys
       ; return (False, [], btys') }
tcConArgs new_or_data (InfixCon bty1 bty2)
  = do { bty1' <- tcConArg new_or_data bty1
       ; bty2' <- tcConArg new_or_data bty2
       ; return (True, [], [bty1', bty2']) }
tcConArgs new_or_data (RecCon fields)
  = do { btys' <- mapM (tcConArg new_or_data) btys
       ; return (False, field_names, btys') }
  where
    field_names = map (unLoc . cd_fld_name) fields
    btys        = map cd_fld_type fields

tcConArg :: NewOrData -> LHsType Name -> TcM (TcType, HsBang)
tcConArg new_or_data bty
  = do  { traceTc "tcConArg 1" (ppr bty)
        ; arg_ty <- tcHsConArgType new_or_data bty
        ; traceTc "tcConArg 2" (ppr bty)
        ; return (arg_ty, getBangStrictness bty) }

tcConRes :: ResType (LHsType Name) -> TcM (ResType Type)
tcConRes ResTyH98           = return ResTyH98
tcConRes (ResTyGADT res_ty) = do { res_ty' <- tcHsLiftedType res_ty
                                 ; return (ResTyGADT res_ty') }

\end{code}

Note [Checking GADT return types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is a delicacy around checking the return types of a datacon. The
central problem is dealing with a declaration like

  data T a where
    MkT :: a -> Q a

Note that the return type of MkT is totally bogus. When creating the T
tycon, we also need to create the MkT datacon, which must have a "rejigged"
return type. That is, the MkT datacon's type must be transformed to have
a uniform return type with explicit coercions for GADT-like type parameters.
This rejigging is what rejigConRes does. The problem is, though, that checking
that the return type is appropriate is much easier when done over *Type*,
not *HsType*.

So, we want to make rejigConRes lazy and then check the validity of the return
type in checkValidDataCon. But, if the return type is bogus, rejigConRes can't
work -- it will have a failed pattern match. Luckily, if we run
checkValidDataCon before ever looking at the rejigged return type
(checkValidDataCon checks the dataConUserType, which is not rejigged!), we
catch the error before forcing the rejigged type and panicking.

\begin{code}

-- Example
--   data instance T (b,c) where
--      TI :: forall e. e -> T (e,e)
--
-- The representation tycon looks like this:
--   data :R7T b c where
--      TI :: forall b1 c1. (b1 ~ c1) => b1 -> :R7T b1 c1
-- In this case orig_res_ty = T (e,e)

rejigConRes :: [TyVar] -> Type  -- Template for result type; e.g.
                                -- data instance T [a] b c = ...
                                --      gives template ([a,b,c], T [a] b c)
             -> [TyVar]         -- where MkT :: forall x y z. ...
             -> ResType Type
             -> ([TyVar],               -- Universal
                 [TyVar],               -- Existential (distinct OccNames from univs)
                 [(TyVar,Type)],        -- Equality predicates
                 Type)          -- Typechecked return type
        -- We don't check that the TyCon given in the ResTy is
        -- the same as the parent tycon, because checkValidDataCon will do it

rejigConRes tmpl_tvs res_ty dc_tvs ResTyH98
  = (tmpl_tvs, dc_tvs, [], res_ty)
        -- In H98 syntax the dc_tvs are the existential ones
        --      data T a b c = forall d e. MkT ...
        -- The {a,b,c} are tc_tvs, and {d,e} are dc_tvs

rejigConRes tmpl_tvs res_tmpl dc_tvs (ResTyGADT res_ty)
        -- E.g.  data T [a] b c where
        --         MkT :: forall x y z. T [(x,y)] z z
        -- Then we generate
        --      Univ tyvars     Eq-spec
        --          a              a~(x,y)
        --          b              b~z
        --          z
        -- Existentials are the leftover type vars: [x,y]
        -- So we return ([a,b,z], [x,y], [a~(x,y),b~z], T [(x,y)] z z)
  = (univ_tvs, ex_tvs, eq_spec, res_ty)
  where
    Just subst = tcMatchTy (mkVarSet tmpl_tvs) res_tmpl res_ty
                -- This 'Just' pattern is sure to match, because if not
                -- checkValidDataCon will complain first.
               -- See Note [Checking GADT return types]

                -- /Lazily/ figure out the univ_tvs etc
                -- Each univ_tv is either a dc_tv or a tmpl_tv
    (univ_tvs, eq_spec) = foldr choose ([], []) tmpl_tvs
    choose tmpl (univs, eqs)
      | Just ty <- lookupTyVar subst tmpl
      = case tcGetTyVar_maybe ty of
          Just tv | not (tv `elem` univs)
            -> (tv:univs,   eqs)
          _other  -> (new_tmpl:univs, (new_tmpl,ty):eqs)
                     where  -- see Note [Substitution in template variables kinds]
                       new_tmpl = updateTyVarKind (substTy subst) tmpl
      | otherwise = pprPanic "tcResultType" (ppr res_ty)
    ex_tvs = dc_tvs `minusList` univ_tvs
\end{code}

Note [Substitution in template variables kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data List a = Nil | Cons a (List a)
data SList s as where
  SNil :: SList s Nil

We call tcResultType with
  tmpl_tvs = [(k :: BOX), (s :: k -> *), (as :: List k)]
  res_tmpl = SList k s as
  res_ty = ResTyGADT (SList k1 (s1 :: k1 -> *) (Nil k1))

We get subst:
  k -> k1
  s -> s1
  as -> Nil k1

Now we want to find out the universal variables and the equivalences
between some of them and types (GADT).

In this example, k and s are mapped to exactly variables which are not
already present in the universal set, so we just add them without any
coercion.

But 'as' is mapped to 'Nil k1', so we add 'as' to the universal set,
and add the equivalence with 'Nil k1' in 'eqs'.

The problem is that with kind polymorphism, as's kind may now contain
kind variables, and we have to apply the template substitution to it,
which is why we create new_tmpl.

The template substitution only maps kind variables to kind variables,
since GADTs are not kind indexed.

%************************************************************************
%*                                                                      *
                Validity checking
%*                                                                      *
%************************************************************************

Validity checking is done once the mutually-recursive knot has been
tied, so we can look at things freely.

\begin{code}
checkClassCycleErrs :: Class -> TcM ()
checkClassCycleErrs cls
  = unless (null cls_cycles) $ mapM_ recClsErr cls_cycles
  where cls_cycles = calcClassCycles cls

checkValidTyCl :: TyThing -> TcM ()
checkValidTyCl thing
  = setSrcSpan (getSrcSpan thing) $
    addTyThingCtxt thing $
    case thing of
      ATyCon tc -> checkValidTyCon tc
      AnId _    -> return ()  -- Generic default methods are checked
                              -- with their parent class
      ACoAxiom _ -> return () -- Axioms checked with their parent
                              -- closed family tycon
      _         -> pprTrace "checkValidTyCl" (ppr thing) $ return ()

-------------------------
-- For data types declared with record syntax, we require
-- that each constructor that has a field 'f'
--      (a) has the same result type
--      (b) has the same type for 'f'
-- module alpha conversion of the quantified type variables
-- of the constructor.
--
-- Note that we allow existentials to match because the
-- fields can never meet. E.g
--      data T where
--        T1 { f1 :: b, f2 :: a, f3 ::Int } :: T
--        T2 { f1 :: c, f2 :: c, f3 ::Int } :: T
-- Here we do not complain about f1,f2 because they are existential

checkValidTyCon :: TyCon -> TcM ()
checkValidTyCon tc
  | Just cl <- tyConClass_maybe tc
  = checkValidClass cl

  | Just syn_rhs <- synTyConRhs_maybe tc
  = case syn_rhs of
    { ClosedSynFamilyTyCon ax      -> checkValidClosedCoAxiom ax
    ; AbstractClosedSynFamilyTyCon ->
      do { hsBoot <- tcIsHsBoot
         ; checkTc hsBoot $
           ptext (sLit "You may omit the equations in a closed type family") $$
           ptext (sLit "only in a .hs-boot file") }
    ; OpenSynFamilyTyCon           -> return ()
    ; SynonymTyCon ty              -> checkValidType syn_ctxt ty
    ; BuiltInSynFamTyCon _         -> return () }

  | otherwise
  = do { -- Check the context on the data decl
         traceTc "cvtc1" (ppr tc)
       ; checkValidTheta (DataTyCtxt name) (tyConStupidTheta tc)

       ; traceTc "cvtc2" (ppr tc)

       ; dflags          <- getDynFlags
       ; existential_ok  <- xoptM Opt_ExistentialQuantification
       ; gadt_ok         <- xoptM Opt_GADTs
       ; let ex_ok = existential_ok || gadt_ok  -- Data cons can have existential context
       ; mapM_ (checkValidDataCon dflags ex_ok tc) data_cons

        -- Check that fields with the same name share a type
       ; mapM_ check_fields groups }

  where
    syn_ctxt  = TySynCtxt name
    name      = tyConName tc
    data_cons = tyConDataCons tc

    groups = equivClasses cmp_fld (concatMap get_fields data_cons)
    cmp_fld (f1,_) (f2,_) = f1 `compare` f2
    get_fields con = dataConFieldLabels con `zip` repeat con
        -- dataConFieldLabels may return the empty list, which is fine

    -- See Note [GADT record selectors] in MkId.lhs
    -- We must check (a) that the named field has the same
    --                   type in each constructor
    --               (b) that those constructors have the same result type
    --
    -- However, the constructors may have differently named type variable
    -- and (worse) we don't know how the correspond to each other.  E.g.
    --     C1 :: forall a b. { f :: a, g :: b } -> T a b
    --     C2 :: forall d c. { f :: c, g :: c } -> T c d
    --
    -- So what we do is to ust Unify.tcMatchTys to compare the first candidate's
    -- result type against other candidates' types BOTH WAYS ROUND.
    -- If they magically agrees, take the substitution and
    -- apply them to the latter ones, and see if they match perfectly.
    check_fields ((label, con1) : other_fields)
        -- These fields all have the same name, but are from
        -- different constructors in the data type
        = recoverM (return ()) $ mapM_ checkOne other_fields
                -- Check that all the fields in the group have the same type
                -- NB: this check assumes that all the constructors of a given
                -- data type use the same type variables
        where
        (tvs1, _, _, res1) = dataConSig con1
        ts1 = mkVarSet tvs1
        fty1 = dataConFieldType con1 label

        checkOne (_, con2)    -- Do it bothways to ensure they are structurally identical
            = do { checkFieldCompat label con1 con2 ts1 res1 res2 fty1 fty2
                 ; checkFieldCompat label con2 con1 ts2 res2 res1 fty2 fty1 }
            where
                (tvs2, _, _, res2) = dataConSig con2
                ts2 = mkVarSet tvs2
                fty2 = dataConFieldType con2 label
    check_fields [] = panic "checkValidTyCon/check_fields []"

checkValidClosedCoAxiom :: CoAxiom Branched -> TcM ()
checkValidClosedCoAxiom (CoAxiom { co_ax_branches = branches, co_ax_tc = tc })
 = tcAddClosedTypeFamilyDeclCtxt tc $
   do { brListFoldlM_ check_accessibility [] branches
      ; void $ brListMapM (checkValidTyFamInst Nothing tc) branches }
   where
     check_accessibility :: [CoAxBranch]       -- prev branches (in reverse order)
                         -> CoAxBranch         -- cur branch
                         -> TcM [CoAxBranch]   -- cur : prev
               -- Check whether the branch is dominated by earlier
               -- ones and hence is inaccessible
     check_accessibility prev_branches cur_branch
       = do { when (cur_branch `isDominatedBy` prev_branches) $
              setSrcSpan (coAxBranchSpan cur_branch) $
              addErrTc $ inaccessibleCoAxBranch tc cur_branch
            ; return (cur_branch : prev_branches) }

checkFieldCompat :: Name -> DataCon -> DataCon -> TyVarSet
                 -> Type -> Type -> Type -> Type -> TcM ()
checkFieldCompat fld con1 con2 tvs1 res1 res2 fty1 fty2
  = do  { checkTc (isJust mb_subst1) (resultTypeMisMatch fld con1 con2)
        ; checkTc (isJust mb_subst2) (fieldTypeMisMatch fld con1 con2) }
  where
    mb_subst1 = tcMatchTy tvs1 res1 res2
    mb_subst2 = tcMatchTyX tvs1 (expectJust "checkFieldCompat" mb_subst1) fty1 fty2

-------------------------------
checkValidDataCon :: DynFlags -> Bool -> TyCon -> DataCon -> TcM ()
checkValidDataCon dflags existential_ok tc con
  = setSrcSpan (srcLocSpan (getSrcLoc con))     $
    addErrCtxt (dataConCtxt con)                $
    do  { traceTc "checkValidDataCon" (ppr con $$ ppr tc)

          -- Check that the return type of the data constructor
          -- matches the type constructor; eg reject this:
          --   data T a where { MkT :: Bogus a }
          -- c.f. Note [Check role annotations in a second pass]
          --  and Note [Checking GADT return types]
        ; let tc_tvs = tyConTyVars tc
              res_ty_tmpl = mkFamilyTyConApp tc (mkTyVarTys tc_tvs)
              orig_res_ty = dataConOrigResTy con
        ; checkTc (isJust (tcMatchTy (mkVarSet tc_tvs)
                                     res_ty_tmpl
                                     orig_res_ty))
                  (badDataConTyCon con res_ty_tmpl orig_res_ty)

          -- Check that the result type is a *monotype*
          --  e.g. reject this:   MkT :: T (forall a. a->a)
          -- Reason: it's really the argument of an equality constraint
        ; checkValidMonoType orig_res_ty

          -- Check all argument types for validity
        ; checkValidType ctxt (dataConUserType con)

          -- Extra checks for newtype data constructors
        ; when (isNewTyCon tc) (checkNewDataCon con)

          -- Check that UNPACK pragmas and bangs work out
          -- E.g.  reject   data T = MkT {-# UNPACK #-} Int     -- No "!"
          --                data T = MkT {-# UNPACK #-} !a      -- Can't unpack
        ; mapM_ check_bang (zip3 (dataConStrictMarks con) (dataConRepBangs con) [1..])

          -- Check that existentials are allowed if they are used
        ; checkTc (existential_ok || isVanillaDataCon con)
                  (badExistential con)

          -- Check that we aren't doing GADT type refinement on kind variables
          -- e.g reject    data T (a::k) where
          --                  T1 :: T Int
          --                  T2 :: T Maybe
        ; checkTc (not (any (isKindVar . fst) (dataConEqSpec con)))
                  (badGadtKindCon con)

        ; traceTc "Done validity of data con" (ppr con <+> ppr (dataConRepType con))
    }
  where
    ctxt = ConArgCtxt (dataConName con)
    check_bang (HsUserBang (Just want_unpack) has_bang, rep_bang, n)
      | want_unpack, not has_bang
      = addWarnTc (bad_bang n (ptext (sLit "UNPACK pragma lacks '!'")))
      | want_unpack
      , case rep_bang of { HsUnpack {} -> False; _ -> True }
      , not (gopt Opt_OmitInterfacePragmas dflags)
           -- If not optimising, se don't unpack, so don't complain!
           -- See MkId.dataConArgRep, the (HsBang True) case
      = addWarnTc (bad_bang n (ptext (sLit "Ignoring unusable UNPACK pragma")))

    check_bang _
      = return ()

    bad_bang n herald
      = hang herald 2 (ptext (sLit "on the") <+> speakNth n
                       <+> ptext (sLit "argument of") <+> quotes (ppr con))
-------------------------------
checkNewDataCon :: DataCon -> TcM ()
-- Further checks for the data constructor of a newtype
checkNewDataCon con
  = do  { checkTc (isSingleton arg_tys) (newtypeFieldErr con (length arg_tys))
                -- One argument

        ; check_con (null eq_spec) $
          ptext (sLit "A newtype constructor must have a return type of form T a1 ... an")
                -- Return type is (T a b c)

        ; check_con (null theta) $
          ptext (sLit "A newtype constructor cannot have a context in its type")

        ; check_con (null ex_tvs) $
          ptext (sLit "A newtype constructor cannot have existential type variables")
                -- No existentials

        ; checkTc (not (any isBanged (dataConStrictMarks con)))
                  (newtypeStrictError con)
                -- No strictness
    }
  where
    (_univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _res_ty) = dataConFullSig con
    check_con what msg 
       = checkTc what (msg $$ ppr con <+> dcolon <+> ppr (dataConUserType con))

-------------------------------
checkValidClass :: Class -> TcM ()
checkValidClass cls
  = do  { constrained_class_methods <- xoptM Opt_ConstrainedClassMethods
        ; multi_param_type_classes <- xoptM Opt_MultiParamTypeClasses
        ; nullary_type_classes <- xoptM Opt_NullaryTypeClasses
        ; fundep_classes <- xoptM Opt_FunctionalDependencies

        -- Check that the class is unary, unless multiparameter or
        -- nullary type classes are enabled
        ; checkTc (nullary_type_classes || notNull tyvars) (nullaryClassErr cls)
        ; checkTc (multi_param_type_classes || arity <= 1) (classArityErr cls)
        ; checkTc (fundep_classes || null fundeps) (classFunDepsErr cls)

        -- Check the super-classes
        ; checkValidTheta (ClassSCCtxt (className cls)) theta

          -- Now check for cyclic superclasses
        ; checkClassCycleErrs cls

        -- Check the class operations
        ; mapM_ (check_op constrained_class_methods) op_stuff

        -- Check the associated type defaults are well-formed and instantiated
        ; mapM_ check_at_defs at_stuff  }
  where
    (tyvars, fundeps, theta, _, at_stuff, op_stuff) = classExtraBigSig cls
    arity = count isTypeVar tyvars    -- Ignore kind variables

    check_op constrained_class_methods (sel_id, dm)
      = addErrCtxt (classOpCtxt sel_id tau) $ do
        { checkValidTheta ctxt (tail theta)
                -- The 'tail' removes the initial (C a) from the
                -- class itself, leaving just the method type

        ; traceTc "class op type" (ppr op_ty <+> ppr tau)
        ; checkValidType ctxt tau

                -- Check that the type mentions at least one of
                -- the class type variables...or at least one reachable
                -- from one of the class variables.  Example: tc223
                --   class Error e => Game b mv e | b -> mv e where
                --      newBoard :: MonadState b m => m ()
                -- Here, MonadState has a fundep m->b, so newBoard is fine
                -- The check is disabled for nullary type classes,
                -- since there is no possible ambiguity
        ; let grown_tyvars = growThetaTyVars theta (mkVarSet tyvars)
        ; checkTc (arity == 0 || tyVarsOfType tau `intersectsVarSet` grown_tyvars)
                  (noClassTyVarErr cls sel_id)

        ; case dm of
            GenDefMeth dm_name -> do { dm_id <- tcLookupId dm_name
                                     ; checkValidType (FunSigCtxt op_name) (idType dm_id) }
            _                  -> return ()
        }
        where
          ctxt    = FunSigCtxt op_name
          op_name = idName sel_id
          op_ty   = idType sel_id
          (_,theta1,tau1) = tcSplitSigmaTy op_ty
          (_,theta2,tau2)  = tcSplitSigmaTy tau1
          (theta,tau) | constrained_class_methods = (theta1 ++ theta2, tau2)
                      | otherwise = (theta1, mkPhiTy (tail theta1) tau1)
                -- Ugh!  The function might have a type like
                --      op :: forall a. C a => forall b. (Eq b, Eq a) => tau2
                -- With -XConstrainedClassMethods, we want to allow this, even though the inner
                -- forall has an (Eq a) constraint.  Whereas in general, each constraint
                -- in the context of a for-all must mention at least one quantified
                -- type variable.  What a mess!

    check_at_defs (fam_tc, defs)
      = tcAddDefaultAssocDeclCtxt (tyConName fam_tc) $
        mapM_ (checkValidTyFamInst mb_clsinfo fam_tc) defs

    mb_clsinfo = Just (cls, mkVarEnv [ (tv, mkTyVarTy tv) | tv <- tyvars ])

checkFamFlag :: Name -> TcM ()
-- Check that we don't use families without -XTypeFamilies
-- The parser won't even parse them, but I suppose a GHC API
-- client might have a go!
checkFamFlag tc_name
  = do { idx_tys <- xoptM Opt_TypeFamilies
       ; checkTc idx_tys err_msg }
  where
    err_msg = hang (ptext (sLit "Illegal family declaraion for") <+> quotes (ppr tc_name))
                 2 (ptext (sLit "Use TypeFamilies to allow indexed type families"))
\end{code}

%************************************************************************
%*                                                                      *
                Checking role validity
%*                                                                      *
%************************************************************************

\begin{code}
checkValidRoleAnnots :: RoleAnnots -> TyThing -> TcM ()
checkValidRoleAnnots role_annots thing
  = case thing of
    { ATyCon tc
        | isTypeSynonymTyCon tc -> check_no_roles
        | isFamilyTyCon tc      -> check_no_roles
        | isAlgTyCon tc         -> check_roles
        where
          name                   = tyConName tc

     -- Role annotations are given only on *type* variables, but a tycon stores
     -- roles for all variables. So, we drop the kind roles (which are all
     -- Nominal, anyway).
          tyvars                 = tyConTyVars tc
          roles                  = tyConRoles tc
          (kind_vars, type_vars) = span isKindVar tyvars
          type_roles             = dropList kind_vars roles
          role_annot_decl_maybe  = lookupRoleAnnots role_annots name

          check_roles
            = whenIsJust role_annot_decl_maybe $
                \decl@(L loc (RoleAnnotDecl _ the_role_annots)) ->
                addRoleAnnotCtxt name $
                setSrcSpan loc $ do
                { role_annots_ok <- xoptM Opt_RoleAnnotations
                ; checkTc role_annots_ok $ needXRoleAnnotations tc
                ; checkTc (type_vars `equalLength` the_role_annots)
                          (wrongNumberOfRoles type_vars decl)
                ; _ <- zipWith3M checkRoleAnnot type_vars the_role_annots type_roles
                -- Representational or phantom roles for class parameters
                -- quickly lead to incoherence. So, we require
                -- IncoherentInstances to have them. See #8773.
                ; incoherent_roles_ok <- xoptM Opt_IncoherentInstances
                ; checkTc (  incoherent_roles_ok
                          || (not $ isClassTyCon tc)
                          || (all (== Nominal) type_roles))
                          incoherentRoles
                  
                ; lint <- goptM Opt_DoCoreLinting
                ; when lint $ checkValidRoles tc }

          check_no_roles
            = whenIsJust role_annot_decl_maybe illegalRoleAnnotDecl
    ; _ -> return () }

checkRoleAnnot :: TyVar -> Located (Maybe Role) -> Role -> TcM ()
checkRoleAnnot _  (L _ Nothing)   _  = return ()
checkRoleAnnot tv (L _ (Just r1)) r2
  = when (r1 /= r2) $
    addErrTc $ badRoleAnnot (tyVarName tv) r1 r2

-- This is a double-check on the role inference algorithm. It is only run when
-- -dcore-lint is enabled. See Note [Role inference] in TcTyDecls
checkValidRoles :: TyCon -> TcM ()
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism] in CoreLint
checkValidRoles tc
  | isAlgTyCon tc
    -- tyConDataCons returns an empty list for data families
  = mapM_ check_dc_roles (tyConDataCons tc)
  | Just (SynonymTyCon rhs) <- synTyConRhs_maybe tc
  = check_ty_roles (zipVarEnv (tyConTyVars tc) (tyConRoles tc)) Representational rhs
  | otherwise
  = return ()
  where
    check_dc_roles datacon
      = do { traceTc "check_dc_roles" (ppr datacon <+> ppr (tyConRoles tc))
           ; mapM_ (check_ty_roles role_env Representational) $
                    eqSpecPreds eq_spec ++ theta ++ arg_tys }
                    -- See Note [Role-checking data constructor arguments] in TcTyDecls
      where
        (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _res_ty) = dataConFullSig datacon
        univ_roles = zipVarEnv univ_tvs (tyConRoles tc)
              -- zipVarEnv uses zipEqual, but we don't want that for ex_tvs
        ex_roles   = mkVarEnv (zip ex_tvs (repeat Nominal))
        role_env   = univ_roles `plusVarEnv` ex_roles

    check_ty_roles env role (TyVarTy tv)
      = case lookupVarEnv env tv of
          Just role' -> unless (role' `ltRole` role || role' == role) $
                        report_error $ ptext (sLit "type variable") <+> quotes (ppr tv) <+>
                                       ptext (sLit "cannot have role") <+> ppr role <+>
                                       ptext (sLit "because it was assigned role") <+> ppr role'
          Nothing    -> report_error $ ptext (sLit "type variable") <+> quotes (ppr tv) <+>
                                       ptext (sLit "missing in environment")

    check_ty_roles env Representational (TyConApp tc tys)
      = let roles' = tyConRoles tc in
        zipWithM_ (maybe_check_ty_roles env) roles' tys

    check_ty_roles env Nominal (TyConApp _ tys)
      = mapM_ (check_ty_roles env Nominal) tys

    check_ty_roles _   Phantom ty@(TyConApp {})
      = pprPanic "check_ty_roles" (ppr ty)

    check_ty_roles env role (AppTy ty1 ty2)
      =  check_ty_roles env role    ty1
      >> check_ty_roles env Nominal ty2

    check_ty_roles env role (FunTy ty1 ty2)
      =  check_ty_roles env role ty1
      >> check_ty_roles env role ty2

    check_ty_roles env role (ForAllTy tv ty)
      = check_ty_roles (extendVarEnv env tv Nominal) role ty

    check_ty_roles _   _    (LitTy {}) = return ()

    maybe_check_ty_roles env role ty
      = when (role == Nominal || role == Representational) $
        check_ty_roles env role ty

    report_error doc
      = addErrTc $ vcat [ptext (sLit "Internal error in role inference:"),
                         doc,
                         ptext (sLit "Please report this as a GHC bug: http://www.haskell.org/ghc/reportabug")]

\end{code}

%************************************************************************
%*                                                                      *
                Building record selectors
%*                                                                      *
%************************************************************************

\begin{code}
mkDefaultMethodIds :: [TyThing] -> [Id]
-- See Note [Default method Ids and Template Haskell]
mkDefaultMethodIds things
  = [ mkExportedLocalId dm_name (idType sel_id)
    | ATyCon tc <- things
    , Just cls <- [tyConClass_maybe tc]
    , (sel_id, DefMeth dm_name) <- classOpItems cls ]
\end{code}

Note [Default method Ids and Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (Trac #4169):
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

\begin{code}
mkRecSelBinds :: [TyThing] -> HsValBinds Name
-- NB We produce *un-typechecked* bindings, rather like 'deriving'
--    This makes life easier, because the later type checking will add
--    all necessary type abstractions and applications
mkRecSelBinds tycons
  = ValBindsOut [(NonRecursive, b) | b <- binds] sigs
  where
    (sigs, binds) = unzip rec_sels
    rec_sels = map mkRecSelBind [ (tc,fld)
                                | ATyCon tc <- tycons
                                , fld <- tyConFields tc ]

mkRecSelBind :: (TyCon, FieldLabel) -> (LSig Name, LHsBinds Name)
mkRecSelBind (tycon, sel_name)
  = (L loc (IdSig sel_id), unitBag (L loc sel_bind))
  where
    loc    = getSrcSpan sel_name
    sel_id = Var.mkExportedLocalVar rec_details sel_name
                                    sel_ty vanillaIdInfo
    rec_details = RecSelId { sel_tycon = tycon, sel_naughty = is_naughty }

    -- Find a representative constructor, con1
    all_cons     = tyConDataCons tycon
    cons_w_field = [ con | con <- all_cons
                   , sel_name `elem` dataConFieldLabels con ]
    con1 = ASSERT( not (null cons_w_field) ) head cons_w_field

    -- Selector type; Note [Polymorphic selectors]
    field_ty   = dataConFieldType con1 sel_name
    data_ty    = dataConOrigResTy con1
    data_tvs   = tyVarsOfType data_ty
    is_naughty = not (tyVarsOfType field_ty `subVarSet` data_tvs)
    (field_tvs, field_theta, field_tau) = tcSplitSigmaTy field_ty
    sel_ty | is_naughty = unitTy  -- See Note [Naughty record selectors]
           | otherwise  = mkForAllTys (varSetElemsKvsFirst $
                                       data_tvs `extendVarSetList` field_tvs) $
                          mkPhiTy (dataConStupidTheta con1) $   -- Urgh!
                          mkPhiTy field_theta               $   -- Urgh!
                          mkFunTy data_ty field_tau

    -- Make the binding: sel (C2 { fld = x }) = x
    --                   sel (C7 { fld = x }) = x
    --    where cons_w_field = [C2,C7]
    sel_bind = mkTopFunBind Generated sel_lname alts
      where
        alts | is_naughty = [mkSimpleMatch [] unit_rhs]
             | otherwise =  map mk_match cons_w_field ++ deflt
    mk_match con = mkSimpleMatch [L loc (mk_sel_pat con)]
                                 (L loc (HsVar field_var))
    mk_sel_pat con = ConPatIn (L loc (getName con)) (RecCon rec_fields)
    rec_fields = HsRecFields { rec_flds = [rec_field], rec_dotdot = Nothing }
    rec_field  = HsRecField { hsRecFieldId = sel_lname
                            , hsRecFieldArg = L loc (VarPat field_var)
                            , hsRecPun = False }
    sel_lname = L loc sel_name
    field_var = mkInternalName (mkBuiltinUnique 1) (getOccName sel_name) loc

    -- Add catch-all default case unless the case is exhaustive
    -- We do this explicitly so that we get a nice error message that
    -- mentions this particular record selector
    deflt | all dealt_with all_cons = []
          | otherwise = [mkSimpleMatch [L loc (WildPat placeHolderType)]
                            (mkHsApp (L loc (HsVar (getName rEC_SEL_ERROR_ID)))
                                     (L loc (HsLit msg_lit)))]

        -- Do not add a default case unless there are unmatched
        -- constructors.  We must take account of GADTs, else we
        -- get overlap warning messages from the pattern-match checker
        -- NB: we need to pass type args for the *representation* TyCon
        --     to dataConCannotMatch, hence the calculation of inst_tys
        --     This matters in data families
        --              data instance T Int a where
        --                 A :: { fld :: Int } -> T Int Bool
        --                 B :: { fld :: Int } -> T Int Char
    dealt_with con = con `elem` cons_w_field || dataConCannotMatch inst_tys con
    inst_tys = substTyVars (mkTopTvSubst (dataConEqSpec con1)) (dataConUnivTyVars con1)

    unit_rhs = mkLHsTupleExpr []
    msg_lit = HsStringPrim $ unsafeMkByteString $
              occNameString (getOccName sel_name)

---------------
tyConFields :: TyCon -> [FieldLabel]
tyConFields tc
  | isAlgTyCon tc = nub (concatMap dataConFieldLabels (tyConDataCons tc))
  | otherwise     = []
\end{code}

Note [Polymorphic selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a record has a polymorphic field, we pull the foralls out to the front.
   data T = MkT { f :: forall a. [a] -> a }
Then f :: forall a. T -> [a] -> a
NOT  f :: T -> forall a. [a] -> a

This is horrid.  It's only needed in deeply obscure cases, which I hate.
The only case I know is test tc163, which is worth looking at.  It's far
from clear that this test should succeed at all!

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
for naughty selectors, so that the later type-check will add them to the
environment, and they'll be exported.  The function is never called, because
the tyepchecker spots the sel_naughty field.

Note [GADT record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For GADTs, we require that all constructors with a common field 'f' have the same
result type (modulo alpha conversion).  [Checked in TcTyClsDecls.checkValidTyCon]
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
gotten by appying the eq_spec to the univ_tvs of the data con.

%************************************************************************
%*                                                                      *
                Error messages
%*                                                                      *
%************************************************************************

\begin{code}
tcAddDefaultAssocDeclCtxt :: Name -> TcM a -> TcM a
tcAddDefaultAssocDeclCtxt name thing_inside
  = addErrCtxt ctxt thing_inside
  where
     ctxt = hsep [ptext (sLit "In the type synonym instance default declaration for"),
                  quotes (ppr name)]

tcAddTyFamInstCtxt :: TyFamInstDecl Name -> TcM a -> TcM a
tcAddTyFamInstCtxt decl
  = tcAddFamInstCtxt (ptext (sLit "type instance")) (tyFamInstDeclName decl)

tcAddDataFamInstCtxt :: DataFamInstDecl Name -> TcM a -> TcM a
tcAddDataFamInstCtxt decl
  = tcAddFamInstCtxt (pprDataFamInstFlavour decl <+> ptext (sLit "instance"))
                     (unLoc (dfid_tycon decl))

tcAddFamInstCtxt :: SDoc -> Name -> TcM a -> TcM a
tcAddFamInstCtxt flavour tycon thing_inside
  = addErrCtxt ctxt thing_inside
  where
     ctxt = hsep [ptext (sLit "In the") <+> flavour
                  <+> ptext (sLit "declaration for"),
                  quotes (ppr tycon)]

tcAddClosedTypeFamilyDeclCtxt :: TyCon -> TcM a -> TcM a
tcAddClosedTypeFamilyDeclCtxt tc
  = addErrCtxt ctxt
  where
    ctxt = ptext (sLit "In the equations for closed type family") <+>
           quotes (ppr tc)

resultTypeMisMatch :: Name -> DataCon -> DataCon -> SDoc
resultTypeMisMatch field_name con1 con2
  = vcat [sep [ptext (sLit "Constructors") <+> ppr con1 <+> ptext (sLit "and") <+> ppr con2,
                ptext (sLit "have a common field") <+> quotes (ppr field_name) <> comma],
          nest 2 $ ptext (sLit "but have different result types")]

fieldTypeMisMatch :: Name -> DataCon -> DataCon -> SDoc
fieldTypeMisMatch field_name con1 con2
  = sep [ptext (sLit "Constructors") <+> ppr con1 <+> ptext (sLit "and") <+> ppr con2,
         ptext (sLit "give different types for field"), quotes (ppr field_name)]

dataConCtxt :: Outputable a => a -> SDoc
dataConCtxt con = ptext (sLit "In the definition of data constructor") <+> quotes (ppr con)

classOpCtxt :: Var -> Type -> SDoc
classOpCtxt sel_id tau = sep [ptext (sLit "When checking the class method:"),
                              nest 2 (pprPrefixOcc sel_id <+> dcolon <+> ppr tau)]

nullaryClassErr :: Class -> SDoc
nullaryClassErr cls
  = vcat [ptext (sLit "No parameters for class") <+> quotes (ppr cls),
          parens (ptext (sLit "Use NullaryTypeClasses to allow no-parameter classes"))]

classArityErr :: Class -> SDoc
classArityErr cls
  = vcat [ptext (sLit "Too many parameters for class") <+> quotes (ppr cls),
          parens (ptext (sLit "Use MultiParamTypeClasses to allow multi-parameter classes"))]

classFunDepsErr :: Class -> SDoc
classFunDepsErr cls
  = vcat [ptext (sLit "Fundeps in class") <+> quotes (ppr cls),
          parens (ptext (sLit "Use FunctionalDependencies to allow fundeps"))]

noClassTyVarErr :: Class -> Var -> SDoc
noClassTyVarErr clas op
  = sep [ptext (sLit "The class method") <+> quotes (ppr op),
         ptext (sLit "mentions none of the type variables of the class") <+>
                ppr clas <+> hsep (map ppr (classTyVars clas))]

recSynErr :: [LTyClDecl Name] -> TcRn ()
recSynErr syn_decls
  = setSrcSpan (getLoc (head sorted_decls)) $
    addErr (sep [ptext (sLit "Cycle in type synonym declarations:"),
                 nest 2 (vcat (map ppr_decl sorted_decls))])
  where
    sorted_decls = sortLocated syn_decls
    ppr_decl (L loc decl) = ppr loc <> colon <+> ppr decl

recClsErr :: [TyCon] -> TcRn ()
recClsErr cycles
  = addErr (sep [ptext (sLit "Cycle in class declaration (via superclasses):"),
                 nest 2 (hsep (intersperse (text "->") (map ppr cycles)))])

badDataConTyCon :: DataCon -> Type -> Type -> SDoc
badDataConTyCon data_con res_ty_tmpl actual_res_ty
  = hang (ptext (sLit "Data constructor") <+> quotes (ppr data_con) <+>
                ptext (sLit "returns type") <+> quotes (ppr actual_res_ty))
       2 (ptext (sLit "instead of an instance of its parent type") <+> quotes (ppr res_ty_tmpl))

badGadtKindCon :: DataCon -> SDoc
badGadtKindCon data_con
  = hang (ptext (sLit "Data constructor") <+> quotes (ppr data_con)
          <+> ptext (sLit "cannot be GADT-like in its *kind* arguments"))
       2 (ppr data_con <+> dcolon <+> ppr (dataConUserType data_con))

badGadtDecl :: Name -> SDoc
badGadtDecl tc_name
  = vcat [ ptext (sLit "Illegal generalised algebraic data declaration for") <+> quotes (ppr tc_name)
         , nest 2 (parens $ ptext (sLit "Use GADTs to allow GADTs")) ]

badExistential :: DataCon -> SDoc
badExistential con
  = hang (ptext (sLit "Data constructor") <+> quotes (ppr con) <+>
                ptext (sLit "has existential type variables, a context, or a specialised result type"))
       2 (vcat [ ppr con <+> dcolon <+> ppr (dataConUserType con)
               , parens $ ptext (sLit "Use ExistentialQuantification or GADTs to allow this") ])

badStupidTheta :: Name -> SDoc
badStupidTheta tc_name
  = ptext (sLit "A data type declared in GADT style cannot have a context:") <+> quotes (ppr tc_name)

newtypeConError :: Name -> Int -> SDoc
newtypeConError tycon n
  = sep [ptext (sLit "A newtype must have exactly one constructor,"),
         nest 2 $ ptext (sLit "but") <+> quotes (ppr tycon) <+> ptext (sLit "has") <+> speakN n ]

newtypeStrictError :: DataCon -> SDoc
newtypeStrictError con
  = sep [ptext (sLit "A newtype constructor cannot have a strictness annotation,"),
         nest 2 $ ptext (sLit "but") <+> quotes (ppr con) <+> ptext (sLit "does")]

newtypeFieldErr :: DataCon -> Int -> SDoc
newtypeFieldErr con_name n_flds
  = sep [ptext (sLit "The constructor of a newtype must have exactly one field"),
         nest 2 $ ptext (sLit "but") <+> quotes (ppr con_name) <+> ptext (sLit "has") <+> speakN n_flds]

badSigTyDecl :: Name -> SDoc
badSigTyDecl tc_name
  = vcat [ ptext (sLit "Illegal kind signature") <+>
           quotes (ppr tc_name)
         , nest 2 (parens $ ptext (sLit "Use KindSignatures to allow kind signatures")) ]

emptyConDeclsErr :: Name -> SDoc
emptyConDeclsErr tycon
  = sep [quotes (ppr tycon) <+> ptext (sLit "has no constructors"),
         nest 2 $ ptext (sLit "(EmptyDataDecls permits this)")]

wrongKindOfFamily :: TyCon -> SDoc
wrongKindOfFamily family
  = ptext (sLit "Wrong category of family instance; declaration was for a")
    <+> kindOfFamily
  where
    kindOfFamily | isSynTyCon family = ptext (sLit "type synonym")
                 | isAlgTyCon family = ptext (sLit "data type")
                 | otherwise = pprPanic "wrongKindOfFamily" (ppr family)

wrongNumberOfParmsErrTooMany :: Arity -> SDoc
wrongNumberOfParmsErrTooMany max_args
  = ptext (sLit "Number of parameters must match family declaration; expected no more than")
    <+> ppr max_args

wrongNamesInInstGroup :: Name -> Name -> SDoc
wrongNamesInInstGroup first cur
  = ptext (sLit "Mismatched type names in closed type family declaration.") $$
    ptext (sLit "First name was") <+>
    (ppr first) <> (ptext (sLit "; this one is")) <+> (ppr cur)

inaccessibleCoAxBranch :: TyCon -> CoAxBranch -> SDoc
inaccessibleCoAxBranch tc fi
  = ptext (sLit "Inaccessible family instance equation:") $$
      (pprCoAxBranch tc fi)

badRoleAnnot :: Name -> Role -> Role -> SDoc
badRoleAnnot var annot inferred
  = hang (ptext (sLit "Role mismatch on variable") <+> ppr var <> colon)
       2 (sep [ ptext (sLit "Annotation says"), ppr annot
              , ptext (sLit "but role"), ppr inferred
              , ptext (sLit "is required") ])

wrongNumberOfRoles :: [a] -> LRoleAnnotDecl Name -> SDoc
wrongNumberOfRoles tyvars d@(L _ (RoleAnnotDecl _ annots))
  = hang (ptext (sLit "Wrong number of roles listed in role annotation;") $$
          ptext (sLit "Expected") <+> (ppr $ length tyvars) <> comma <+>
          ptext (sLit "got") <+> (ppr $ length annots) <> colon)
       2 (ppr d)

illegalRoleAnnotDecl :: LRoleAnnotDecl Name -> TcM ()
illegalRoleAnnotDecl (L loc (RoleAnnotDecl tycon _))
  = setErrCtxt [] $
    setSrcSpan loc $
    addErrTc (ptext (sLit "Illegal role annotation for") <+> ppr tycon <> char ';' $$
              ptext (sLit "they are allowed only for datatypes and classes."))

needXRoleAnnotations :: TyCon -> SDoc
needXRoleAnnotations tc
  = ptext (sLit "Illegal role annotation for") <+> ppr tc <> char ';' $$
    ptext (sLit "did you intend to use RoleAnnotations?")

incoherentRoles :: SDoc
incoherentRoles = (text "Roles other than" <+> quotes (text "nominal") <+>
                   text "for class parameters can lead to incoherence.") $$
                  (text "Use IncoherentInstances to allow this; bad role found")

addTyThingCtxt :: TyThing -> TcM a -> TcM a
addTyThingCtxt thing
  = addErrCtxt ctxt
  where
    name = getName thing
    flav = case thing of
             ATyCon tc
                | isClassTyCon tc       -> ptext (sLit "class")
                | isSynFamilyTyCon tc   -> ptext (sLit "type family")
                | isDataFamilyTyCon tc  -> ptext (sLit "data family")
                | isTypeSynonymTyCon tc -> ptext (sLit "type")
                | isNewTyCon tc         -> ptext (sLit "newtype")
                | isDataTyCon tc        -> ptext (sLit "data")

             _ -> pprTrace "addTyThingCtxt strange" (ppr thing)
                  empty

    ctxt = hsep [ ptext (sLit "In the"), flav
                , ptext (sLit "declaration for"), quotes (ppr name) ]

addRoleAnnotCtxt :: Name -> TcM a -> TcM a
addRoleAnnotCtxt name
  = addErrCtxt $
    text "while checking a role annotation for" <+> quotes (ppr name)
    
\end{code}
