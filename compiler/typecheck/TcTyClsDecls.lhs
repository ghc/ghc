%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1996-1998
%

TcTyClsDecls: Typecheck type and class declarations

\begin{code}
{-# LANGUAGE TupleSections #-}

module TcTyClsDecls (
        tcTyAndClassDecls, tcAddImplicits,

        -- Functions used by TcInstDcls to check
        -- data/type family instance declarations
        kcDataDefn, tcConDecls, dataDeclChecks, checkValidTyCon,
        tcSynFamInstDecl, tcFamTyPats,
        tcAddTyFamInstCtxt, tcAddDataFamInstCtxt,
        wrongKindOfFamily,
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
import Coercion( mkCoAxBranch )
import Type
import Kind
import Class
import CoAxiom( CoAxBranch(..) )
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
       ; tyclss <- fixM $ \ rec_tyclss -> do
           { let rec_flags = calcRecFlags boot_details rec_tyclss

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
             concatMapM (tcTyClDecl rec_flags) tyclds }

           -- Step 3: Perform the validity chebck
           -- We can do this now because we are done with the recursive knot
           -- Do it before Step 4 (adding implicit things) because the latter
           -- expects well-formed TyCons
       ; tcExtendGlobalEnv tyclss $ do
       { traceTc "Starting validity check" (ppr tyclss)
       ; checkNoErrs $
         mapM_ (recoverM (return ()) . addLocM checkValidTyCl) tyclds
           -- We recover, which allows us to report multiple validity errors
           -- but we then fail if any are wrong.  Lacking the checkNoErrs
           -- we get Trac #7175

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

   1. Make up a kind variable for each parameter of the *data* type,
      and class, decls, and extend the kind environment (which is in
      the TcLclEnv)

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

Type families
~~~~~~~~~~~~~
This treatment of type synonyms only applies to Haskell 98-style synonyms.
General type functions can be recursive, and hence, appear in `alg_decls'.

The kind of a type family is solely determinded by its kind signature;
hence, only kind signatures participate in the construction of the initial
kind environment (as constructed by `getInitialKind').  In fact, we ignore
instances of families altogether in the following.  However, we need to
include the kinds of *associated* families into the construction of the
initial kind environment.  (This is handled by `allDecls').


\begin{code}
kcTyClGroup :: TyClGroup Name -> TcM [(Name,Kind)]
-- Kind check this group, kind generalize, and return the resulting local env
-- This bindds the TyCons and Classes of the group, but not the DataCons
-- See Note [Kind checking for type and class decls]
kcTyClGroup decls
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
        ; initial_kinds <- getInitialKinds TopLevel non_syn_decls
        ; traceTc "kcTyClGroup: initial kinds" (ppr initial_kinds)

        -- Step 2: Set initial envt, kind-check the synonyms
        ; lcl_env <- tcExtendTcTyThingEnv initial_kinds $
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
    generalise :: TcTypeEnv -> Name -> LHsTyVarBndrs Name -> TcM (Name, Kind)
    -- For polymorphic things this is a no-op
    generalise kind_env name tvs
      = do { let kc_kind = case lookupNameEnv kind_env name of
                               Just (AThing k) -> k
                               _ -> pprPanic "kcTyClGroup" (ppr name $$ ppr kind_env)
           ; kvs <- kindGeneralize (tyVarsOfType kc_kind) (hsLTyVarNames tvs)
           ; kc_kind' <- zonkTcKind kc_kind    -- Make sure kc_kind' has the final,
                                               -- skolemised kind variables
           ; traceTc "Generalise kind" (vcat [ ppr name, ppr kc_kind, ppr kvs, ppr kc_kind' ])
           ; return (name, mkForAllTys kvs kc_kind') }

    generaliseTCD :: TcTypeEnv -> LTyClDecl Name -> TcM [(Name, Kind)]
    generaliseTCD kind_env (L _ decl)
      | ClassDecl { tcdLName = (L _ name), tcdTyVars = tyvars, tcdATs = ats } <- decl
      = do { first <- generalise kind_env name tyvars
           ; rest <- mapM ((generaliseFamDecl kind_env) . unLoc) ats
           ; return (first : rest) }

      | FamDecl { tcdFam = fam } <- decl
      = do { res <- generaliseFamDecl kind_env fam
           ; return [res] }

      | ForeignType {} <- decl
      = pprPanic "generaliseTCD" (ppr decl)

      | otherwise
      -- Note: tcdTyVars is safe here because we've eliminated FamDecl and ForeignType
      = do { res <- generalise kind_env (tcdName decl) (tcdTyVars decl)
           ; return [res] }

    generaliseFamDecl :: TcTypeEnv -> FamilyDecl Name -> TcM (Name, Kind)
    generaliseFamDecl kind_env (FamilyDecl { fdLName = L _ name, fdTyVars = tyvars })
      = generalise kind_env name tyvars

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

getInitialKinds :: TopLevelFlag -> [LTyClDecl Name] -> TcM [(Name, TcTyThing)]
getInitialKinds top_lvl decls
  = tcExtendTcTyThingEnv (mk_thing_env decls) $
    concatMapM (addLocM (getInitialKind top_lvl)) decls

getInitialKind :: TopLevelFlag -> TyClDecl Name -> TcM [(Name, TcTyThing)]
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

getInitialKind top_lvl (FamDecl { tcdFam = decl }) = getFamDeclInitialKind top_lvl decl
getInitialKind _ (ClassDecl { tcdLName = L _ name, tcdTyVars = ktvs, tcdATs = ats })
  = kcHsTyVarBndrs False ktvs $ \ arg_kinds ->
    do { inner_prs <- getFamDeclInitialKinds ats
       ; let main_pr = (name, AThing (mkArrowKinds arg_kinds constraintKind))
       ; return (main_pr : inner_prs) }

getInitialKind _top_lvl decl@(SynDecl {}) = pprPanic "getInitialKind" (ppr decl)

getInitialKind top_lvl (DataDecl { tcdLName = L _ name, tcdTyVars = ktvs, tcdDataDefn = defn })
  | HsDataDefn { dd_kindSig = Just ksig, dd_cons = cons } <- defn
  = ASSERT( isTopLevel top_lvl )
    kcHsTyVarBndrs True ktvs $ \ arg_kinds ->
    do { res_k <- tcLHsKind ksig
       ; let body_kind = mkArrowKinds arg_kinds res_k
             kvs       = varSetElems (tyVarsOfType body_kind)
             main_pr   = (name, AThing (mkForAllTys kvs body_kind))
             inner_prs = [(unLoc (con_name con), APromotionErr RecDataConPE) | L _ con <- cons ]
             -- See Note [Recusion and promoting data constructors]
       ; return (main_pr : inner_prs) }

  | HsDataDefn { dd_cons = cons } <- defn
  = kcHsTyVarBndrs False ktvs $ \ arg_kinds ->
    do { let main_pr   = (name, AThing (mkArrowKinds arg_kinds liftedTypeKind))
             inner_prs = [ (unLoc (con_name con), APromotionErr RecDataConPE)
                         | L _ con <- cons ]
             -- See Note [Recusion and promoting data constructors]
       ; return (main_pr : inner_prs) }

getInitialKind _ (ForeignType { tcdLName = L _ name })
  = return [(name, AThing liftedTypeKind)]

getFamDeclInitialKinds :: [LFamilyDecl Name] -> TcM [(Name, TcTyThing)]
getFamDeclInitialKinds decls
  = tcExtendTcTyThingEnv [ (n, APromotionErr TyConPE)
                         | L _ (FamilyDecl { fdLName = L _ n }) <- decls] $
    concatMapM (addLocM (getFamDeclInitialKind NotTopLevel)) decls

getFamDeclInitialKind :: TopLevelFlag
                      -> FamilyDecl Name
                      -> TcM [(Name, TcTyThing)]
getFamDeclInitialKind top_lvl (FamilyDecl { fdLName = L _ name
                                          , fdTyVars = ktvs
                                          , fdKindSig = ksig })
  | isTopLevel top_lvl
  = kcHsTyVarBndrs True ktvs $ \ arg_kinds ->
    do { res_k <- case ksig of
                    Just k  -> tcLHsKind k
                    Nothing -> return liftedTypeKind
       ; let body_kind = mkArrowKinds arg_kinds res_k
             kvs       = varSetElems (tyVarsOfType body_kind)
       ; return [ (name, AThing (mkForAllTys kvs body_kind)) ] }

  | otherwise
  = kcHsTyVarBndrs False ktvs $ \ arg_kinds ->
    do { res_k <- case ksig of
                    Just k  -> tcLHsKind k
                    Nothing -> newMetaKindVar
       ; return [ (name, AThing (mkArrowKinds arg_kinds res_k)) ] }

----------------
kcSynDecls :: [SCC (LTyClDecl Name)] -> TcM TcLclEnv    -- Kind bindings
kcSynDecls [] = getLclEnv
kcSynDecls (group : groups)
  = do  { nk <- kcSynDecl1 group
        ; tcExtendKindEnv [nk] (kcSynDecls groups) }

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
    kcHsTyVarBndrs False hs_tvs $ \ ks ->
    do { traceTc "kcd1" (ppr name <+> brackets (ppr hs_tvs)
                         <+> brackets (ppr ks))
       ; (_, rhs_kind) <- tcLHsType rhs
       ; traceTc "kcd2" (ppr name)
       ; let tc_kind = mkArrowKinds ks rhs_kind
       ; return (name, tc_kind) }
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
    -- hs_tvs and td_kindSig already dealt with in getInitialKind
    -- Ignore the dd_ctxt; heavily deprecated and inconvenient

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
kcTyClDecl (FamDecl {})    = return ()

-------------------
kcConDecl :: ConDecl Name -> TcM ()
kcConDecl (ConDecl { con_name = name, con_qvars = ex_tvs
                   , con_cxt = ex_ctxt, con_details = details, con_res = res })
  = addErrCtxt (dataConCtxt name) $
    kcHsTyVarBndrs False ex_tvs $ \ _ ->
    do { _ <- tcHsContext ex_ctxt
       ; mapM_ (tcHsOpenType . getBangType) (hsConDeclArgTys details)
       ; _ <- tcConRes res
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
tcTyClDecl1 _parent _rec_info
            (SynDecl { tcdLName = L _ tc_name, tcdTyVars = tvs, tcdRhs = rhs })
  = ASSERT( isNoParent _parent )
    tcTyClTyVars tc_name tvs $ \ tvs' kind ->
    tcTySynRhs tc_name tvs' kind rhs

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
               ; let    -- This little knot is just so we can get
                        -- hold of the name of the class TyCon, which we
                        -- need to look up its recursiveness
                    tycon_name = tyConName (classTyCon clas)
                    tc_isrec = rti_is_rec rec_info tycon_name

               ; ctxt' <- tcHsContext ctxt
               ; ctxt' <- zonkTcTypeToTypes emptyZonkEnv ctxt'
                       -- Squeeze out any kind unification variables
               ; fds'  <- mapM (addLocM tc_fundep) fundeps
               ; (sig_stuff, gen_dm_env) <- tcClassSigs class_name sigs meths
               ; at_stuff <- tcClassATs class_name (AssocFamilyTyCon clas) ats at_defs
               ; clas <- buildClass False {- Must include unfoldings for selectors -}
                            class_name tvs' ctxt' fds' at_stuff
                            sig_stuff tc_isrec
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
  = return [ATyCon (mkForeignTyCon tc_name tc_ext_name liftedTypeKind 0)]
\end{code}

\begin{code}
tcFamDecl1 :: TyConParent -> FamilyDecl Name -> TcM [TyThing]
tcFamDecl1 parent
            (FamilyDecl {fdFlavour = TypeFamily, fdLName = L _ tc_name, fdTyVars = tvs})
  = tcTyClTyVars tc_name tvs $ \ tvs' kind -> do
  { traceTc "type family:" (ppr tc_name)
  ; checkFamFlag tc_name
  ; let syn_rhs = SynFamilyTyCon { synf_open = True, synf_injective = False }
  ; tycon <- buildSynTyCon tc_name tvs' syn_rhs kind parent
  ; return [ATyCon tycon] }

tcFamDecl1 parent
           (FamilyDecl {fdFlavour = DataFamily, fdLName = L _ tc_name, fdTyVars = tvs})
  = tcTyClTyVars tc_name tvs $ \ tvs' kind -> do
  { traceTc "data family:" (ppr tc_name)
  ; checkFamFlag tc_name
  ; extra_tvs <- tcDataKindSig kind
  ; let final_tvs = tvs' ++ extra_tvs    -- we may not need these
        tycon = buildAlgTyCon tc_name final_tvs Nothing []
                              DataFamilyTyCon Recursive
                              False   -- Not promotable to the kind level
                              True    -- GADT syntax
                              parent
  ; return [ATyCon tycon] }

tcTySynRhs :: Name
           -> [TyVar] -> Kind
           -> LHsType Name -> TcM [TyThing]
tcTySynRhs tc_name tvs kind hs_ty
  = do { env <- getLclEnv
       ; traceTc "tc-syn" (ppr tc_name $$ ppr (tcl_env env))
       ; rhs_ty <- tcCheckLHsType hs_ty kind
       ; rhs_ty <- zonkTcTypeToType emptyZonkEnv rhs_ty
       ; tycon <- buildSynTyCon tc_name tvs (SynonymTyCon rhs_ty)
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
       ; stupid_theta <- tcHsContext ctxt
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
             ; return (buildAlgTyCon tc_name final_tvs cType stupid_theta tc_rhs
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
                  ; atd <- concatMapM (tcDefaultAssocDecl fam_tc) at_defs
                  ; return (fam_tc, atd) }

-------------------------
tcDefaultAssocDecl :: TyCon                -- ^ Family TyCon
                   -> LTyFamInstDecl Name  -- ^ RHS
                   -> TcM [CoAxBranch]     -- ^ Type checked RHS and free TyVars
tcDefaultAssocDecl fam_tc (L loc decl)
  = setSrcSpan loc $
    tcAddTyFamInstCtxt decl $
    do { traceTc "tcDefaultAssocDecl" (ppr decl)
       ; tcSynFamInstDecl fam_tc decl }
    -- We check for well-formedness and validity later, in checkValidClass

-------------------------
tcSynFamInstDecl :: TyCon -> TyFamInstDecl Name -> TcM [CoAxBranch]
-- Placed here because type family instances appear as
-- default decls in class declarations
tcSynFamInstDecl fam_tc (TyFamInstDecl { tfid_eqns = eqns })
  -- we know the first equation matches the fam_tc because of the lookup logic
  -- now, just check that all other names match the first
  = do { let names = map (tfie_tycon . unLoc) eqns
             first = head names
       ; tcSynFamInstNames first names
       ; checkTc (isSynTyCon fam_tc) (wrongKindOfFamily fam_tc)
       ; mapM (tcTyFamInstEqn fam_tc) eqns }

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

tcTyFamInstEqn :: TyCon -> LTyFamInstEqn Name -> TcM CoAxBranch
tcTyFamInstEqn fam_tc
    (L loc (TyFamInstEqn { tfie_pats = pats, tfie_rhs = hs_ty }))
  = setSrcSpan loc $
    tcFamTyPats fam_tc pats (discardResult . (tcCheckLHsType hs_ty)) $
       \tvs' pats' res_kind ->
    do { rhs_ty <- tcCheckLHsType hs_ty res_kind
       ; rhs_ty <- zonkTcTypeToType emptyZonkEnv rhs_ty
       ; traceTc "tcSynFamInstEqn" (ppr fam_tc <+> (ppr tvs' $$ ppr pats' $$ ppr rhs_ty))
       ; return (mkCoAxBranch tvs' pats' rhs_ty loc) }

kcDataDefn :: HsDataDefn Name -> TcKind -> TcM ()
-- Used for 'data instance' only
-- Ordinary 'data' is handled by kcTyClDec
kcDataDefn (HsDataDefn { dd_ctxt = ctxt, dd_cons = cons, dd_kindSig = mb_kind }) res_k
  = do  { _ <- tcHsContext ctxt
        ; mapM_ (wrapLocM kcConDecl) cons
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

-------------------------
-- Kind check type patterns and kind annotate the embedded type variables.
--     type instance F [a] = rhs
--
-- * Here we check that a type instance matches its kind signature, but we do
--   not check whether there is a pattern for each type index; the latter
--   check is only required for type synonym instances.

-----------------
tcFamTyPats :: TyCon
            -> HsWithBndrs [LHsType Name] -- Patterns
            -> (TcKind -> TcM ())       -- Kind checker for RHS
                                        -- result is ignored
            -> ([TKVar] -> [TcType] -> Kind -> TcM a)
            -> TcM a
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

tcFamTyPats fam_tc (HsWB { hswb_cts = arg_pats, hswb_kvs = kvars, hswb_tvs = tvars })
            kind_checker thing_inside
  = do { -- A family instance must have exactly the same number of type
         -- parameters as the family declaration.  You can't write
         --     type family F a :: * -> *
         --     type instance F Int y = y
         -- because then the type (F Int) would be like (\y.y)
       ; let (fam_kvs, fam_body) = splitForAllTys (tyConKind fam_tc)
             fam_arity = tyConArity fam_tc - length fam_kvs
       ; checkTc (length arg_pats == fam_arity) $
                 wrongNumberOfParmsErr fam_arity

         -- Instantiate with meta kind vars
       ; fam_arg_kinds <- mapM (const newMetaKindVar) fam_kvs
       ; loc <- getSrcSpanM
       ; let (arg_kinds, res_kind)
                 = splitKindFunTysN fam_arity $
                   substKiWith fam_kvs fam_arg_kinds fam_body
             hs_tvs = HsQTvs { hsq_kvs = kvars
                             , hsq_tvs = userHsTyVarBndrs loc tvars }

         -- Kind-check and quantify
         -- See Note [Quantifying over family patterns]
       ; typats <- tcHsTyVarBndrs hs_tvs $ \ _ ->
                   do { kind_checker res_kind
                      ; tcHsArgTys (quotes (ppr fam_tc)) arg_pats arg_kinds }
       ; let all_args = fam_arg_kinds ++ typats

            -- Find free variables (after zonking) and turn
            -- them into skolems, so that we don't subsequently
            -- replace a meta kind var with AnyK
            -- Very like kindGeneralize
       ; tkvs <- zonkTyVarsAndFV (tyVarsOfTypes all_args)
       ; qtkvs <- zonkQuantifiedTyVars (varSetElems tkvs)

            -- Zonk the patterns etc into the Type world
       ; (ze, qtkvs') <- zonkTyBndrsX emptyZonkEnv qtkvs
       ; all_args'    <- zonkTcTypeToTypes ze all_args
       ; res_kind'    <- zonkTcTypeToType  ze res_kind

       ; traceTc "tcFamTyPats" (pprTvBndrs qtkvs' $$ ppr all_args' $$ ppr res_kind')
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
tcConDecls new_or_data rep_tycon res_tmpl cons
  = mapM (addLocM (tcConDecl new_or_data rep_tycon res_tmpl)) cons

tcConDecl :: NewOrData
          -> TyCon              -- Representation tycon
          -> ([TyVar], Type)    -- Return type template (with its template tyvars)
          -> ConDecl Name
          -> TcM DataCon

tcConDecl new_or_data rep_tycon res_tmpl        -- Data types
          (ConDecl { con_name = name
                   , con_qvars = hs_tvs, con_cxt = hs_ctxt
                   , con_details = hs_details, con_res = hs_res_ty })
  = addErrCtxt (dataConCtxt name) $
    do { traceTc "tcConDecl 1" (ppr name)
       ; (tvs, ctxt, arg_tys, res_ty, is_infix, field_lbls, stricts)
           <- tcHsTyVarBndrs hs_tvs $ \ tvs ->
              do { ctxt    <- tcHsContext hs_ctxt
                 ; details <- tcConArgs new_or_data hs_details
                 ; res_ty  <- tcConRes hs_res_ty
                 ; let (is_infix, field_lbls, btys) = details
                       (arg_tys, stricts)           = unzip btys
                 ; return (tvs, ctxt, arg_tys, res_ty, is_infix, field_lbls, stricts) }

       ; let pretend_res_ty = case res_ty of
                                ResTyH98     -> unitTy
                                ResTyGADT ty -> ty
             pretend_con_ty = mkSigmaTy tvs ctxt (mkFunTys arg_tys pretend_res_ty)
                -- This pretend_con_ty stuff is just a convenient way to get the
                -- free kind variables of the type, for kindGeneralize to work on

             -- Generalise the kind variables (returning quantifed TcKindVars)
             -- and quantify the type variables (substiting their kinds)
       ; kvs <- kindGeneralize (tyVarsOfType pretend_con_ty) (map getName tvs)
       ; tvs <- zonkQuantifiedTyVars tvs

             -- Zonk to Types
       ; (ze, qtkvs) <- zonkTyBndrsX emptyZonkEnv (kvs ++ tvs)
       ; arg_tys <- zonkTcTypeToTypes ze arg_tys
       ; ctxt    <- zonkTcTypeToTypes ze ctxt
       ; res_ty  <- case res_ty of
                      ResTyH98     -> return ResTyH98
                      ResTyGADT ty -> ResTyGADT <$> zonkTcTypeToType ze ty

       ; let (univ_tvs, ex_tvs, eq_preds, res_ty')
                = rejigConRes res_tmpl qtkvs res_ty

       ; traceTc "tcConDecl 3" (ppr name)
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

-- Example
--   data instance T (b,c) where
--      TI :: forall e. e -> T (e,e)
--
-- The representation tycon looks like this:
--   data :R7T b c where
--      TI :: forall b1 c1. (b1 ~ c1) => b1 -> :R7T b1 c1
-- In this case orig_res_ty = T (e,e)

rejigConRes :: ([TyVar], Type)  -- Template for result type; e.g.
                                -- data instance T [a] b c = ...
                                --      gives template ([a,b,c], T [a] b c)
             -> [TyVar]         -- where MkT :: forall x y z. ...
             -> ResType Type
             -> ([TyVar],               -- Universal
                 [TyVar],               -- Existential (distinct OccNames from univs)
                 [(TyVar,Type)],        -- Equality predicates
                 Type)          -- Typechecked return type
        -- We don't check that the TyCon given in the ResTy is
        -- the same as the parent tycon, because we are in the middle
        -- of a recursive knot; so it's postponed until checkValidDataCon

rejigConRes (tmpl_tvs, res_ty) dc_tvs ResTyH98
  = (tmpl_tvs, dc_tvs, [], res_ty)
        -- In H98 syntax the dc_tvs are the existential ones
        --      data T a b c = forall d e. MkT ...
        -- The {a,b,c} are tc_tvs, and {d,e} are dc_tvs

rejigConRes (tmpl_tvs, res_tmpl) dc_tvs (ResTyGADT res_ty)
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
                -- checkValidDataCon will complain first. The 'subst'
                -- should not be looked at until after checkValidDataCon
                -- We can't check eagerly because we are in a "knot" in
                -- which 'tycon' is not yet fully defined

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

checkValidDecl :: SDoc -- the context for error checking
               -> Located Name -> TcM ()
checkValidDecl ctxt lname
  = addErrCtxt ctxt $
    do  { traceTc "Validity of 1" (ppr lname)
        ; env <- getGblEnv
        ; traceTc "Validity of 1a" (ppr (tcg_type_env env))
        ; thing <- tcLookupLocatedGlobal lname
        ; traceTc "Validity of 2" (ppr lname)
        ; traceTc "Validity of" (ppr thing)
        ; case thing of
            ATyCon tc -> do
                traceTc "  of kind" (ppr (tyConKind tc))
                checkValidTyCon tc
            AnId _    -> return ()  -- Generic default methods are checked
                                    -- with their parent class
            _         -> panic "checkValidTyCl"
        ; traceTc "Done validity of" (ppr thing)
        }

checkValidTyCl :: TyClDecl Name -> TcM ()
checkValidTyCl decl
  = do { checkValidDecl (tcMkDeclCtxt decl) (tyClDeclLName decl)
       ; case decl of
           ClassDecl { tcdATs = ats } ->
             mapM_ (checkValidFamDecl . unLoc) ats
           _ -> return () }

checkValidFamDecl :: FamilyDecl Name -> TcM ()
checkValidFamDecl (FamilyDecl { fdLName = lname, fdFlavour = flav })
  = checkValidDecl (hsep [ptext (sLit "In the"), ppr flav,
                          ptext (sLit "declaration for"), quotes (ppr lname)])
                   lname

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
      SynFamilyTyCon {} -> return ()
      SynonymTyCon ty   -> checkValidType syn_ctxt ty

  | otherwise
  = do { -- Check the context on the data decl
       ; traceTc "cvtc1" (ppr tc)
       ; checkValidTheta (DataTyCtxt name) (tyConStupidTheta tc)

        -- Check arg types of data constructors
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
    do  { traceTc "Validity of data con" (ppr con)
        ; let tc_tvs = tyConTyVars tc
              res_ty_tmpl = mkFamilyTyConApp tc (mkTyVarTys tc_tvs)
              actual_res_ty = dataConOrigResTy con
        ; traceTc "checkValidDataCon" (ppr con $$ ppr tc $$ ppr tc_tvs $$ ppr res_ty_tmpl)
        ; checkTc (isJust (tcMatchTy (mkVarSet tc_tvs)
                                res_ty_tmpl
                                actual_res_ty))
                  (badDataConTyCon con res_ty_tmpl actual_res_ty)
             -- IA0_TODO: we should also check that kind variables
             -- are only instantiated with kind variables
        ; checkValidMonoType (dataConOrigResTy con)
                -- Disallow MkT :: T (forall a. a->a)
                -- Reason: it's really the argument of an equality constraint
        ; checkValidType ctxt (dataConUserType con)
        ; when (isNewTyCon tc) (checkNewDataCon con)

        ; mapM_ check_bang (zip3 (dataConStrictMarks con) (dataConRepBangs con) [1..])

        ; checkTc (existential_ok || isVanillaDataCon con)
                  (badExistential con)

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
-- Checks for the data constructor of a newtype
checkNewDataCon con
  = do  { checkTc (isSingleton arg_tys) (newtypeFieldErr con (length arg_tys))
                -- One argument
        ; checkTc (null eq_spec) (newtypePredError con)
                -- Return type is (T a b c)
        ; checkTc (null ex_tvs && null theta) (newtypeExError con)
                -- No existentials
        ; checkTc (not (any isBanged (dataConStrictMarks con)))
                  (newtypeStrictError con)
                -- No strictness
    }
  where
    (_univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _res_ty) = dataConFullSig con

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
                 2 (ptext (sLit "Use -XTypeFamilies to allow indexed type families"))
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
    sel_bind | is_naughty = mkTopFunBind sel_lname [mkSimpleMatch [] unit_rhs]
             | otherwise  = mkTopFunBind sel_lname (map mk_match cons_w_field ++ deflt)
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
  | [_] <- tfid_eqns decl
  = tcAddFamInstCtxt (ptext (sLit "type instance")) (tyFamInstDeclName decl)
  | otherwise
  = tcAddFamInstCtxt (ptext (sLit "type instance group")) (tyFamInstDeclName decl)

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
                              nest 2 (ppr sel_id <+> dcolon <+> ppr tau)]

nullaryClassErr :: Class -> SDoc
nullaryClassErr cls
  = vcat [ptext (sLit "No parameters for class") <+> quotes (ppr cls),
          parens (ptext (sLit "Use -XNullaryTypeClasses to allow no-parameter classes"))]

classArityErr :: Class -> SDoc
classArityErr cls
  = vcat [ptext (sLit "Too many parameters for class") <+> quotes (ppr cls),
          parens (ptext (sLit "Use -XMultiParamTypeClasses to allow multi-parameter classes"))]

classFunDepsErr :: Class -> SDoc
classFunDepsErr cls
  = vcat [ptext (sLit "Fundeps in class") <+> quotes (ppr cls),
          parens (ptext (sLit "Use -XFunctionalDependencies to allow fundeps"))]

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
         , nest 2 (parens $ ptext (sLit "Use -XGADTs to allow GADTs")) ]

badExistential :: DataCon -> SDoc
badExistential con_name
  = hang (ptext (sLit "Data constructor") <+> quotes (ppr con_name) <+>
                ptext (sLit "has existential type variables, a context, or a specialised result type"))
       2 (parens $ ptext (sLit "Use -XExistentialQuantification or -XGADTs to allow this"))

badStupidTheta :: Name -> SDoc
badStupidTheta tc_name
  = ptext (sLit "A data type declared in GADT style cannot have a context:") <+> quotes (ppr tc_name)

newtypeConError :: Name -> Int -> SDoc
newtypeConError tycon n
  = sep [ptext (sLit "A newtype must have exactly one constructor,"),
         nest 2 $ ptext (sLit "but") <+> quotes (ppr tycon) <+> ptext (sLit "has") <+> speakN n ]

newtypeExError :: DataCon -> SDoc
newtypeExError con
  = sep [ptext (sLit "A newtype constructor cannot have an existential context,"),
         nest 2 $ ptext (sLit "but") <+> quotes (ppr con) <+> ptext (sLit "does")]

newtypeStrictError :: DataCon -> SDoc
newtypeStrictError con
  = sep [ptext (sLit "A newtype constructor cannot have a strictness annotation,"),
         nest 2 $ ptext (sLit "but") <+> quotes (ppr con) <+> ptext (sLit "does")]

newtypePredError :: DataCon -> SDoc
newtypePredError con
  = sep [ptext (sLit "A newtype constructor must have a return type of form T a1 ... an"),
         nest 2 $ ptext (sLit "but") <+> quotes (ppr con) <+> ptext (sLit "does not")]

newtypeFieldErr :: DataCon -> Int -> SDoc
newtypeFieldErr con_name n_flds
  = sep [ptext (sLit "The constructor of a newtype must have exactly one field"),
         nest 2 $ ptext (sLit "but") <+> quotes (ppr con_name) <+> ptext (sLit "has") <+> speakN n_flds]

badSigTyDecl :: Name -> SDoc
badSigTyDecl tc_name
  = vcat [ ptext (sLit "Illegal kind signature") <+>
           quotes (ppr tc_name)
         , nest 2 (parens $ ptext (sLit "Use -XKindSignatures to allow kind signatures")) ]

emptyConDeclsErr :: Name -> SDoc
emptyConDeclsErr tycon
  = sep [quotes (ppr tycon) <+> ptext (sLit "has no constructors"),
         nest 2 $ ptext (sLit "(-XEmptyDataDecls permits this)")]

wrongNumberOfParmsErr :: Arity -> SDoc
wrongNumberOfParmsErr exp_arity
  = ptext (sLit "Number of parameters must match family declaration; expected")
    <+> ppr exp_arity

wrongKindOfFamily :: TyCon -> SDoc
wrongKindOfFamily family
  = ptext (sLit "Wrong category of family instance; declaration was for a")
    <+> kindOfFamily
  where
    kindOfFamily | isSynTyCon family = ptext (sLit "type synonym")
                 | isAlgTyCon family = ptext (sLit "data type")
                 | otherwise = pprPanic "wrongKindOfFamily" (ppr family)

wrongNamesInInstGroup :: Name -> Name -> SDoc
wrongNamesInInstGroup first cur
  = ptext (sLit "Mismatched family names in instance group.") $$
    ptext (sLit "First name was") <+>
    (ppr first) <> (ptext (sLit "; this one is")) <+> (ppr cur)

\end{code}
