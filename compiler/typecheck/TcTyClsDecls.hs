{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998


TcTyClsDecls: Typecheck type and class declarations
-}

{-# LANGUAGE CPP, TupleSections, MultiWayIf #-}

module TcTyClsDecls (
        tcTyAndClassDecls, tcAddImplicits,

        -- Functions used by TcInstDcls to check
        -- data/type family instance declarations
        kcDataDefn, tcConDecls, dataDeclChecks, checkValidTyCon,
        tcFamTyPats, tcTyFamInstEqn, famTyConShape,
        tcAddTyFamInstCtxt, tcMkDataFamInstCtxt, tcAddDataFamInstCtxt,
        wrongKindOfFamily, dataConCtxt
    ) where

#include "HsVersions.h"

import HsSyn
import HscTypes
import BuildTyCl
import TcRnMonad
import TcEnv
import TcValidity
import TcHsSyn
import TcTyDecls
import TcClassDcl
import {-# SOURCE #-} TcInstDcls( tcInstDecls1 )
import TcDeriv (DerivInfo)
import TcUnify
import TcHsType
import TcMType
import TysWiredIn ( unitTy )
import TcType
import RnEnv( RoleAnnotEnv, mkRoleAnnotEnv, lookupRoleAnnot
            , lookupConstructorFields )
import FamInst
import FamInstEnv
import Coercion
import Type
import TyCoRep   -- for checkValidRoles
import Kind
import Class
import CoAxiom
import TyCon
import DataCon
import Id
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
import DynFlags
import Unique
import BasicTypes
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.List

{-
************************************************************************
*                                                                      *
\subsection{Type checking for type and class declarations}
*                                                                      *
************************************************************************

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
forall (k::*). k -> k. Since it does not depend on anything else, it can be
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
-}

tcTyAndClassDecls :: [TyClGroup Name]       -- Mutually-recursive groups in
                                            -- dependency order
                  -> TcM ( TcGblEnv         -- Input env extended by types and
                                            -- classes
                                            -- and their implicit Ids,DataCons
                         , [InstInfo Name]  -- Source-code instance decls info
                         , [DerivInfo]      -- data family deriving info
                         )
-- Fails if there are any errors
tcTyAndClassDecls tyclds_s
  -- The code recovers internally, but if anything gave rise to
  -- an error we'd better stop now, to avoid a cascade
  -- Type check each group in dependency order folding the global env
  = checkNoErrs $ fold_env [] [] tyclds_s
  where
    fold_env :: [InstInfo Name]
             -> [DerivInfo]
             -> [TyClGroup Name]
             -> TcM (TcGblEnv, [InstInfo Name], [DerivInfo])
    fold_env inst_info deriv_info []
      = do { gbl_env <- getGblEnv
           ; return (gbl_env, inst_info, deriv_info) }
    fold_env inst_info deriv_info (tyclds:tyclds_s)
      = do { (tcg_env, inst_info', deriv_info') <- tcTyClGroup tyclds
           ; setGblEnv tcg_env $
               -- remaining groups are typechecked in the extended global env.
             fold_env (inst_info' ++ inst_info)
                      (deriv_info' ++ deriv_info)
                      tyclds_s }

tcTyClGroup :: TyClGroup Name
            -> TcM (TcGblEnv, [InstInfo Name], [DerivInfo])
-- Typecheck one strongly-connected component of type, class, and instance decls
-- See Note [TyClGroups and dependency analysis] in HsDecls
tcTyClGroup (TyClGroup { group_tyclds = tyclds
                       , group_roles  = roles
                       , group_instds = instds })
  = do { let role_annots = mkRoleAnnotEnv roles

           -- Step 1: Typecheck the type/class declarations
       ; tyclss <- tcTyClDecls tyclds role_annots

           -- Step 1.5: Make sure we don't have any type synonym cycles
       ; traceTc "Starting synonym cycle check" (ppr tyclss)
       ; this_uid <- fmap thisPackage getDynFlags
       ; checkSynCycles this_uid tyclss tyclds
       ; traceTc "Done synonym cycle check" (ppr tyclss)

       ; traceTc "Starting family consistency check" (ppr tyclss)
       ; forM_ tyclss checkRecFamInstConsistency
       ; traceTc "Done family consistency" (ppr tyclss)

           -- Step 2: Perform the validity check on those types/classes
           -- We can do this now because we are done with the recursive knot
           -- Do it before Step 3 (adding implicit things) because the latter
           -- expects well-formed TyCons
       ; traceTc "Starting validity check" (ppr tyclss)
       ; tyclss <- mapM checkValidTyCl tyclss
       ; traceTc "Done validity check" (ppr tyclss)
       ; mapM_ (recoverM (return ()) . checkValidRoleAnnots role_annots) tyclss
           -- See Note [Check role annotations in a second pass]

           -- Step 3: Add the implicit things;
           -- we want them in the environment because
           -- they may be mentioned in interface files
       ; tcExtendTyConEnv tyclss $
    do { gbl_env <- tcAddImplicits tyclss
       ; setGblEnv gbl_env $
    do {
            -- Step 4: check instance declarations
       ; (gbl_env, inst_info, datafam_deriv_info) <- tcInstDecls1 instds

       ; return (gbl_env, inst_info, datafam_deriv_info) } } }

tcTyClDecls :: [LTyClDecl Name] -> RoleAnnotEnv -> TcM [TyCon]
tcTyClDecls tyclds role_annots
  = do {    -- Step 1: kind-check this group and returns the final
            -- (possibly-polymorphic) kind of each TyCon and Class
            -- See Note [Kind checking for type and class decls]
         tc_tycons <- kcTyClGroup tyclds
       ; traceTc "tcTyAndCl generalized kinds" (vcat (map ppr_tc_tycon tc_tycons))

            -- Step 2: type-check all groups together, returning
            -- the final TyCons and Classes
            --
            -- NB: We have to be careful here to NOT eagerly unfold
            -- type synonyms, as we have not tested for type synonym
            -- loops yet and could fall into a black hole.
       ; fixM $ \ ~rec_tyclss -> do
           { is_boot   <- tcIsHsBootOrSig
           ; let roles = inferRoles is_boot role_annots rec_tyclss

                 -- Populate environment with knot-tied ATyCon for TyCons
                 -- NB: if the decls mention any ill-staged data cons
                 -- (see Note [Recusion and promoting data constructors])
                 -- we will have failed already in kcTyClGroup, so no worries here
           ; tcExtendRecEnv (zipRecTyClss tc_tycons rec_tyclss) $

                 -- Also extend the local type envt with bindings giving
                 -- the (polymorphic) kind of each knot-tied TyCon or Class
                 -- See Note [Type checking recursive type and class declarations]
             tcExtendKindEnv2 (map mkTcTyConPair tc_tycons)              $

                 -- Kind and type check declarations for this group
               mapM (tcTyClDecl roles) tyclds
           } }
  where
    ppr_tc_tycon tc = parens (sep [ ppr (tyConName tc) <> comma
                                  , ppr (tyConBinders tc) <> comma
                                  , ppr (tyConResKind tc) ])

zipRecTyClss :: [TcTyCon]
             -> [TyCon]           -- Knot-tied
             -> [(Name,TyThing)]
-- Build a name-TyThing mapping for the TyCons bound by decls
-- being careful not to look at the knot-tied [TyThing]
-- The TyThings in the result list must have a visible ATyCon,
-- because typechecking types (in, say, tcTyClDecl) looks at
-- this outer constructor
zipRecTyClss tc_tycons rec_tycons
  = [ (name, ATyCon (get name)) | tc_tycon <- tc_tycons, let name = getName tc_tycon ]
  where
    rec_tc_env :: NameEnv TyCon
    rec_tc_env = foldr add_tc emptyNameEnv rec_tycons

    add_tc :: TyCon -> NameEnv TyCon -> NameEnv TyCon
    add_tc tc env = foldr add_one_tc env (tc : tyConATs tc)

    add_one_tc :: TyCon -> NameEnv TyCon -> NameEnv TyCon
    add_one_tc tc env = extendNameEnv env (tyConName tc) tc

    get name = case lookupNameEnv rec_tc_env name of
                 Just tc -> tc
                 other   -> pprPanic "zipRecTyClss" (ppr name <+> ppr other)

{-
************************************************************************
*                                                                      *
                Kind checking
*                                                                      *
************************************************************************

Note [Kind checking for type and class decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Kind checking is done thus:

   1. Make up a kind variable for each parameter of the declarations,
      and extend the kind environment (which is in the TcLclEnv)

   2. Kind check the declarations

We need to kind check all types in the mutually recursive group
before we know the kind of the type variables.  For example:

  class C a where
     op :: D b => a -> b -> b

  class D c where
     bop :: (Monad c) => ...

Here, the kind of the locally-polymorphic type variable "b"
depends on *all the uses of class D*.  For example, the use of
Monad c in bop's type signature means that D must have kind Type->Type.

Note: we don't treat type synonyms specially (we used to, in the past);
in particular, even if we have a type synonym cycle, we still kind check
it normally, and test for cycles later (checkSynCycles).  The reason
we can get away with this is because we have more systematic TYPE r
inference, which means that we can do unification between kinds that
aren't lifted (this historically was not true.)

The downside of not directly reading off the kinds off the RHS of
type synonyms in topological order is that we don't transparently
support making synonyms of types with higher-rank kinds.  But
you can always specify a CUSK directly to make this work out.
See tc269 for an example.

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


See also Note [Kind checking recursive type and class declarations]

-}


-- Note [Missed opportunity to retain higher-rank kinds]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In 'kcTyClGroup', there is a missed opportunity to make kind
-- inference work in a few more cases.  The idea is analogous
-- to Note [Single function non-recursive binding special-case]:
--
--      * If we have an SCC with a single decl, which is non-recursive,
--        instead of creating a unification variable representing the
--        kind of the decl and unifying it with the rhs, we can just
--        read the type directly of the rhs.
--
--      * Furthermore, we can update our SCC analysis to ignore
--        dependencies on declarations which have CUSKs: we don't
--        have to kind-check these all at once, since we can use
--        the CUSK to initialize the kind environment.
--
-- Unfortunately this requires reworking a bit of the code in
-- 'kcLTyClDecl' so I've decided to punt unless someone shouts about it.
--
kcTyClGroup :: [LTyClDecl Name] -> TcM [TcTyCon]

-- Kind check this group, kind generalize, and return the resulting local env
-- This binds the TyCons and Classes of the group, but not the DataCons
-- See Note [Kind checking for type and class decls]
-- Third return value is Nothing if the tycon be unsaturated; otherwise,
-- the arity
kcTyClGroup decls
  = do  { mod <- getModule
        ; traceTc "kcTyClGroup" (text "module" <+> ppr mod $$ vcat (map ppr decls))

          -- Kind checking;
          --    1. Bind kind variables for decls
          --    2. Kind-check decls
          --    3. Generalise the inferred kinds
          -- See Note [Kind checking for type and class decls]

        ; lcl_env <- solveEqualities $
          do {
               -- Step 1: Bind kind variables for all decls
               initial_kinds <- getInitialKinds decls
             ; traceTc "kcTyClGroup: initial kinds" $
               vcat (map pp_initial_kind initial_kinds)
             ; tcExtendKindEnv2 initial_kinds $ do {

             -- Step 2: Set extended envt, kind-check the decls
             ; mapM_ kcLTyClDecl decls

             ; getLclEnv } }

             -- Step 4: generalisation
             -- Kind checking done for this group
             -- Now we have to kind generalize the flexis
        ; res <- concatMapM (generaliseTCD (tcl_env lcl_env)) decls

        ; traceTc "kcTyClGroup result" (vcat (map pp_res res))
        ; return res }

  where
    generalise :: TcTypeEnv -> Name -> TcM TcTyCon
    -- For polymorphic things this is a no-op
    generalise kind_env name
      = do { let tc = case lookupNameEnv kind_env name of
                        Just (ATcTyCon tc) -> tc
                        _ -> pprPanic "kcTyClGroup" (ppr name $$ ppr kind_env)
                 kc_binders  = tyConBinders tc
                 kc_res_kind = tyConResKind tc
                 kc_tyvars   = tyConTyVars tc
           ; kvs <- kindGeneralize (mkTyConKind kc_binders kc_res_kind)
           ; let all_binders = mkNamedTyConBinders Inferred kvs ++ kc_binders

           ; (env, all_binders') <- zonkTyVarBindersX emptyZonkEnv all_binders
           ; kc_res_kind'        <- zonkTcTypeToType env kc_res_kind

                      -- Make sure kc_kind' has the final, zonked kind variables
           ; traceTc "Generalise kind" $
             vcat [ ppr name, ppr kc_binders, ppr kvs, ppr all_binders, ppr kc_res_kind
                  , ppr all_binders', ppr kc_res_kind'
                  , ppr kc_tyvars, ppr (tcTyConScopedTyVars tc)]

           ; return (mkTcTyCon name all_binders' kc_res_kind'
                               (mightBeUnsaturatedTyCon tc)
                               (tcTyConScopedTyVars tc)) }

    generaliseTCD :: TcTypeEnv
                  -> LTyClDecl Name -> TcM [TcTyCon]
    generaliseTCD kind_env (L _ decl)
      | ClassDecl { tcdLName = (L _ name), tcdATs = ats } <- decl
      = do { first <- generalise kind_env name
           ; rest <- mapM ((generaliseFamDecl kind_env) . unLoc) ats
           ; return (first : rest) }

      | FamDecl { tcdFam = fam } <- decl
      = do { res <- generaliseFamDecl kind_env fam
           ; return [res] }

      | otherwise
      = do { res <- generalise kind_env (tcdName decl)
           ; return [res] }

    generaliseFamDecl :: TcTypeEnv
                      -> FamilyDecl Name -> TcM TcTyCon
    generaliseFamDecl kind_env (FamilyDecl { fdLName = L _ name })
      = generalise kind_env name

    pp_initial_kind (name, ATcTyCon tc)
      = ppr name <+> dcolon <+> ppr (tyConKind tc)
    pp_initial_kind pair
      = ppr pair

    pp_res tc = ppr (tyConName tc) <+> dcolon <+> ppr (tyConKind tc)

mkTcTyConPair :: TcTyCon -> (Name, TcTyThing)
-- Makes a binding to put in the local envt, binding
-- a name to a TcTyCon
mkTcTyConPair tc
  = (getName tc, ATcTyCon tc)

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

getInitialKind :: TyClDecl Name
               -> TcM [(Name, TcTyThing)]    -- Mixture of ATcTyCon and APromotionErr
-- Allocate a fresh kind variable for each TyCon and Class
-- For each tycon, return   (name, ATcTyCon (TcCyCon with kind k))
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
--    See Note [ARecDataCon: Recursion and promoting data constructors]
--
-- No family instances are passed to getInitialKinds

getInitialKind decl@(ClassDecl { tcdLName = L _ name, tcdTyVars = ktvs, tcdATs = ats })
  = do { (tycon, inner_prs) <-
           kcHsTyVarBndrs name True cusk False True ktvs $
           do { inner_prs <- getFamDeclInitialKinds (Just cusk) ats
              ; return (constraintKind, inner_prs) }
       ; return (mkTcTyConPair tycon : inner_prs) }
  where
    cusk = hsDeclHasCusk decl

getInitialKind decl@(DataDecl { tcdLName = L _ name
                              , tcdTyVars = ktvs
                              , tcdDataDefn = HsDataDefn { dd_kindSig = m_sig
                                                         , dd_cons = cons } })
  = do  { (tycon, _) <-
           kcHsTyVarBndrs name True (hsDeclHasCusk decl) False True ktvs $
           do { res_k <- case m_sig of
                           Just ksig -> tcLHsKind ksig
                           Nothing   -> return liftedTypeKind
              ; return (res_k, ()) }
        ; let inner_prs = [ (unLocEmb con, APromotionErr RecDataConPE)
                          | L _ con' <- cons, con <- getConNames con' ]
        ; return (mkTcTyConPair tycon : inner_prs) }

getInitialKind (FamDecl { tcdFam = decl })
  = getFamDeclInitialKind Nothing decl

getInitialKind decl@(SynDecl { tcdLName = L _ name
                             , tcdTyVars = ktvs
                             , tcdRhs = rhs })
  = do  { (tycon, _) <- kcHsTyVarBndrs name False (hsDeclHasCusk decl)
                            False {- not open -} True ktvs $
            do  { res_k <- case kind_annotation rhs of
                            Nothing -> newMetaKindVar
                            Just ksig -> tcLHsKind ksig
                ; return (res_k, ()) }
        ; return [ mkTcTyConPair tycon ] }
  where
    -- Keep this synchronized with 'hsDeclHasCusk'.
    kind_annotation (L _ ty) = case ty of
        HsParTy lty     -> kind_annotation lty
        HsKindSig _ k   -> Just k
        _               -> Nothing

---------------------------------
getFamDeclInitialKinds :: Maybe Bool  -- if assoc., CUSKness of assoc. class
                       -> [LFamilyDecl Name] -> TcM [(Name, TcTyThing)]
getFamDeclInitialKinds mb_cusk decls
  = tcExtendKindEnv2 [ (n, APromotionErr TyConPE)
                     | L _ (FamilyDecl { fdLName = L _ n }) <- decls] $
    concatMapM (addLocM (getFamDeclInitialKind mb_cusk)) decls

getFamDeclInitialKind :: Maybe Bool  -- if assoc., CUSKness of assoc. class
                      -> FamilyDecl Name
                      -> TcM [(Name, TcTyThing)]
getFamDeclInitialKind mb_cusk decl@(FamilyDecl { fdLName     = L _ name
                                               , fdTyVars    = ktvs
                                               , fdResultSig = L _ resultSig
                                               , fdInfo      = info })
  = do { (tycon, _) <-
           kcHsTyVarBndrs name unsat cusk open True ktvs $
           do { res_k <- case resultSig of
                      KindSig ki                        -> tcLHsKind ki
                      TyVarSig (L _ (KindedTyVar _ ki)) -> tcLHsKind ki
                      _ -- open type families have * return kind by default
                        | open                     -> return liftedTypeKind
                        -- closed type families have their return kind inferred
                        -- by default
                        | otherwise                -> newMetaKindVar
              ; return (res_k, ()) }
       ; return [ mkTcTyConPair tycon ] }
  where
    cusk  = famDeclHasCusk mb_cusk decl
    (open, unsat) = case info of
      DataFamily         -> (True,  True)
      OpenTypeFamily     -> (True,  False)
      ClosedTypeFamily _ -> (False, False)

------------------------------------------------------------------------
kcLTyClDecl :: LTyClDecl Name -> TcM ()
  -- See Note [Kind checking for type and class decls]
kcLTyClDecl (L loc decl)
  = setSrcSpan loc $ tcAddDeclCtxt decl $ kcTyClDecl decl

kcTyClDecl :: TyClDecl Name -> TcM ()
-- This function is used solely for its side effect on kind variables
-- NB kind signatures on the type variables and
--    result kind signature have already been dealt with
--    by getInitialKind, so we can ignore them here.

kcTyClDecl (DataDecl { tcdLName = L _ name, tcdDataDefn = defn })
  | HsDataDefn { dd_cons = cons, dd_kindSig = Just _ } <- defn
  = mapM_ (wrapLocM kcConDecl) cons
    -- hs_tvs and dd_kindSig already dealt with in getInitialKind
    -- If dd_kindSig is Just, this must be a GADT-style decl,
    --        (see invariants of DataDefn declaration)
    -- so (a) we don't need to bring the hs_tvs into scope, because the
    --        ConDecls bind all their own variables
    --    (b) dd_ctxt is not allowed for GADT-style decls, so we can ignore it

  | HsDataDefn { dd_ctxt = ctxt, dd_cons = cons } <- defn
  = kcTyClTyVars name $
    do  { _ <- tcHsContext ctxt
        ; mapM_ (wrapLocM kcConDecl) cons }

kcTyClDecl (SynDecl { tcdLName = L _ name, tcdRhs = lrhs })
  = kcTyClTyVars name $
    do  { syn_tc <- kcLookupTcTyCon name
        -- NB: check against the result kind that we allocated
        -- in getInitialKinds.
        ; discardResult $ tcCheckLHsType lrhs (tyConResKind syn_tc) }

kcTyClDecl (ClassDecl { tcdLName = L _ name
                      , tcdCtxt = ctxt, tcdSigs = sigs })
  = kcTyClTyVars name $
    do  { _ <- tcHsContext ctxt
        ; mapM_ (wrapLocM kc_sig)     sigs }
  where
    kc_sig (ClassOpSig _ nms op_ty) = kcHsSigType (map unLEmb nms) op_ty
    kc_sig _                        = return ()

kcTyClDecl (FamDecl (FamilyDecl { fdLName  = L _ fam_tc_name
                                , fdInfo   = fd_info }))
-- closed type families look at their equations, but other families don't
-- do anything here
  = case fd_info of
      ClosedTypeFamily (Just eqns) ->
        do { fam_tc <- kcLookupTcTyCon fam_tc_name
           ; mapM_ (kcTyFamInstEqn (famTyConShape fam_tc)) eqns }
      _ -> return ()

-------------------
kcConDecl :: ConDecl Name -> TcM ()
kcConDecl (ConDeclH98 { con_name = name, con_qvars = ex_tvs
                      , con_cxt = ex_ctxt, con_details = details })
  = addErrCtxt (dataConCtxtName [name]) $
         -- the 'False' says that the existentials don't have a CUSK, as the
         -- concept doesn't really apply here. We just need to bring the variables
         -- into scope.
    do { _ <- kcHsTyVarBndrs (unLocEmb name) False False False False
                             ((fromMaybe emptyLHsQTvs ex_tvs)) $
              do { _ <- tcHsContext (fromMaybe (noLoc []) ex_ctxt)
                 ; mapM_ (tcHsOpenType . getBangType) (hsConDeclArgTys details)
                 ; return (panic "kcConDecl", ()) }
              -- We don't need to check the telescope here, because that's
              -- done in tcConDecl
       ; return () }

kcConDecl (ConDeclGADT { con_names = names
                       , con_type = ty })
  = addErrCtxt (dataConCtxtName names) $
      do { _ <- tcGadtSigType (ppr names) (unLocEmb $ head names) ty
         ; return () }


{-
Note [Recursion and promoting data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't want to allow promotion in a strongly connected component
when kind checking.

Consider:
  data T f = K (f (K Any))

When kind checking the `data T' declaration the local env contains the
mappings:
  T -> ATcTyCon <some initial kind>
  K -> APromotionErr

APromotionErr is only used for DataCons, and only used during type checking
in tcTyClGroup.


************************************************************************
*                                                                      *
\subsection{Type checking}
*                                                                      *
************************************************************************

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

  *Global* env with T -> ATyCon (the (not yet built) final TyCon for T)
  *Local*  env with T -> ATcTyCon (TcTyCon with the polymorphic kind of T)

Then:

  * During TcHsType.kcTyVar we look in the *local* env, to get the
    known kind for T.

  * But in TcHsType.ds_type (and ds_var_app in particular) we look in
    the *global* env to get the TyCon. But we must be careful not to
    force the TyCon or we'll get a loop.

This fancy footwork (with two bindings for T) is only necessary for the
TyCons or Classes of this recursive group.  Earlier, finished groups,
live in the global env only.

See also Note [Kind checking recursive type and class declarations]

Note [Kind checking recursive type and class declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before we can type-check the decls, we must kind check them. This
is done by establishing an "initial kind", which is a rather uninformed
guess at a tycon's kind (by counting arguments, mainly) and then
using this initial kind for recursive occurrences.

The initial kind is stored in exactly the same way during kind-checking
as it is during type-checking (Note [Type checking recursive type and class
declarations]): in the *local* environment, with ATcTyCon. But we still
must store *something* in the *global* environment. Even though we
discard the result of kind-checking, we sometimes need to produce error
messages. These error messages will want to refer to the tycons being
checked, except that they don't exist yet, and it would be Terribly
Annoying to get the error messages to refer back to HsSyn. So we
create a TcTyCon and put it in the global env. This tycon can
print out its name and knows its kind,
but any other action taken on it will panic. Note
that TcTyCons are *not* knot-tied, unlike the rather valid but
knot-tied ones that occur during type-checking.

Note [Declarations for wired-in things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For wired-in things we simply ignore the declaration
and take the wired-in information.  That avoids complications.
e.g. the need to make the data constructor worker name for
     a constraint tuple match the wired-in one
-}

tcTyClDecl :: RolesInfo -> LTyClDecl Name -> TcM TyCon
tcTyClDecl roles_info (L loc decl)
  | Just thing <- wiredInNameTyThing_maybe (tcdName decl)
  = case thing of -- See Note [Declarations for wired-in things]
      ATyCon tc -> return tc
      _ -> pprPanic "tcTyClDecl" (ppr thing)

  | otherwise
  = setSrcSpan loc $ tcAddDeclCtxt decl $
    do { traceTc "tcTyAndCl-x" (ppr decl)
       ; tcTyClDecl1 Nothing roles_info decl }

  -- "type family" declarations
tcTyClDecl1 :: Maybe Class -> RolesInfo -> TyClDecl Name -> TcM TyCon
tcTyClDecl1 parent _roles_info (FamDecl { tcdFam = fd })
  = tcFamDecl1 parent fd

  -- "type" synonym declaration
tcTyClDecl1 _parent roles_info
            (SynDecl { tcdLName = L _ tc_name, tcdRhs = rhs })
  = ASSERT( isNothing _parent )
    tcTyClTyVars tc_name $ \ binders res_kind ->
    tcTySynRhs roles_info tc_name binders res_kind rhs

  -- "data/newtype" declaration
tcTyClDecl1 _parent roles_info
            (DataDecl { tcdLName = L _ tc_name, tcdDataDefn = defn })
  = ASSERT( isNothing _parent )
    tcTyClTyVars tc_name $ \ tycon_binders res_kind ->
    tcDataDefn roles_info tc_name tycon_binders res_kind defn

tcTyClDecl1 _parent roles_info
            (ClassDecl { tcdLName = L _ class_name
            , tcdCtxt = ctxt, tcdMeths = meths
            , tcdFDs = fundeps, tcdSigs = sigs
            , tcdATs = ats, tcdATDefs = at_defs })
  = ASSERT( isNothing _parent )
    do { clas <- fixM $ \ clas ->
            -- We need the knot because 'clas' is passed into tcClassATs
            tcTyClTyVars class_name $ \ binders res_kind ->
            do { MASSERT( isConstraintKind res_kind )
               ; traceTc "tcClassDecl 1" (ppr class_name $$ ppr binders)
               ; let tycon_name = class_name        -- We use the same name
                     roles = roles_info tycon_name  -- for TyCon and Class

               ; ctxt' <- solveEqualities $ tcHsContext ctxt
               ; ctxt' <- zonkTcTypeToTypes emptyZonkEnv ctxt'
                       -- Squeeze out any kind unification variables
               ; fds'  <- mapM (addLocM tc_fundep) fundeps
               ; sig_stuff <- tcClassSigs class_name sigs meths
               ; at_stuff <- tcClassATs class_name clas ats at_defs
               ; mindef <- tcClassMinimalDef class_name sigs sig_stuff
               ; clas <- buildClass
                            class_name binders roles ctxt'
                            fds' at_stuff
                            sig_stuff mindef
               ; traceTc "tcClassDecl" (ppr fundeps $$ ppr binders $$
                                        ppr fds')
               ; return clas }

         ; return (classTyCon clas) }
  where
    tc_fundep (tvs1, tvs2) = do { tvs1' <- mapM (tcLookupTyVar . unLoc) tvs1 ;
                                ; tvs2' <- mapM (tcLookupTyVar . unLoc) tvs2 ;
                                ; return (tvs1', tvs2') }

tcFamDecl1 :: Maybe Class -> FamilyDecl Name -> TcM TyCon
tcFamDecl1 parent (FamilyDecl { fdInfo = fam_info, fdLName = tc_lname@(L _ tc_name)
                              , fdTyVars = tvs, fdResultSig = L _ sig
                              , fdInjectivityAnn = inj })
  | DataFamily <- fam_info
  = tcTyClTyVars tc_name $ \ binders res_kind -> do
  { traceTc "data family:" (ppr tc_name)
  ; checkFamFlag tc_name
  ; (extra_binders, real_res_kind) <- tcDataKindSig res_kind
  ; tc_rep_name <- newTyConRepName tc_name
  ; let tycon = mkFamilyTyCon tc_name (binders `chkAppend` extra_binders)
                              real_res_kind
                              (resultVariableName sig)
                              (DataFamilyTyCon tc_rep_name)
                              parent NotInjective
  ; return tycon }

  | OpenTypeFamily <- fam_info
  = tcTyClTyVars tc_name $ \ binders res_kind -> do
  { traceTc "open type family:" (ppr tc_name)
  ; checkFamFlag tc_name
  ; inj' <- tcInjectivity binders inj
  ; let tycon = mkFamilyTyCon tc_name binders res_kind
                               (resultVariableName sig) OpenSynFamilyTyCon
                               parent inj'
  ; return tycon }

  | ClosedTypeFamily mb_eqns <- fam_info
  = -- Closed type families are a little tricky, because they contain the definition
    -- of both the type family and the equations for a CoAxiom.
    do { traceTc "Closed type family:" (ppr tc_name)
         -- the variables in the header scope only over the injectivity
         -- declaration but this is not involved here
       ; (inj', binders, res_kind)
            <- tcTyClTyVars tc_name
               $ \ binders res_kind ->
               do { inj' <- tcInjectivity binders inj
                  ; return (inj', binders, res_kind) }

       ; checkFamFlag tc_name -- make sure we have -XTypeFamilies

         -- If Nothing, this is an abstract family in a hs-boot file;
         -- but eqns might be empty in the Just case as well
       ; case mb_eqns of
           Nothing   ->
               return $ mkFamilyTyCon tc_name binders res_kind
                                      (resultVariableName sig)
                                      AbstractClosedSynFamilyTyCon parent
                                      inj'
           Just eqns -> do {

         -- Process the equations, creating CoAxBranches
       ; let fam_tc_shape = (tc_name, length $ hsQTvExplicit tvs, binders, res_kind)

       ; branches <- mapM (tcTyFamInstEqn fam_tc_shape Nothing) eqns
         -- Do not attempt to drop equations dominated by earlier
         -- ones here; in the case of mutual recursion with a data
         -- type, we get a knot-tying failure.  Instead we check
         -- for this afterwards, in TcValidity.checkValidCoAxiom
         -- Example: tc265

         -- Create a CoAxiom, with the correct src location. It is Vitally
         -- Important that we do not pass the branches into
         -- newFamInstAxiomName. They have types that have been zonked inside
         -- the knot and we will die if we look at them. This is OK here
         -- because there will only be one axiom, so we don't need to
         -- differentiate names.
         -- See [Zonking inside the knot] in TcHsType
       ; co_ax_name <- newFamInstAxiomName tc_lname []

       ; let mb_co_ax
              | null eqns = Nothing   -- mkBranchedCoAxiom fails on empty list
              | otherwise = Just (mkBranchedCoAxiom co_ax_name fam_tc branches)

             fam_tc = mkFamilyTyCon tc_name binders res_kind (resultVariableName sig)
                      (ClosedSynFamilyTyCon mb_co_ax) parent inj'

         -- We check for instance validity later, when doing validity
         -- checking for the tycon. Exception: checking equations
         -- overlap done by dropDominatedAxioms
       ; return fam_tc } }

  | otherwise = panic "tcFamInst1"  -- Silence pattern-exhaustiveness checker


-- | Maybe return a list of Bools that say whether a type family was declared
-- injective in the corresponding type arguments. Length of the list is equal to
-- the number of arguments (including implicit kind/coercion arguments).
-- True on position
-- N means that a function is injective in its Nth argument. False means it is
-- not.
tcInjectivity :: [TyConBinder] -> Maybe (LInjectivityAnn Name)
              -> TcM Injectivity
tcInjectivity _ Nothing
  = return NotInjective

  -- User provided an injectivity annotation, so for each tyvar argument we
  -- check whether a type family was declared injective in that argument. We
  -- return a list of Bools, where True means that corresponding type variable
  -- was mentioned in lInjNames (type family is injective in that argument) and
  -- False means that it was not mentioned in lInjNames (type family is not
  -- injective in that type variable). We also extend injectivity information to
  -- kind variables, so if a user declares:
  --
  --   type family F (a :: k1) (b :: k2) = (r :: k3) | r -> a
  --
  -- then we mark both `a` and `k1` as injective.
  -- NB: the return kind is considered to be *input* argument to a type family.
  -- Since injectivity allows to infer input arguments from the result in theory
  -- we should always mark the result kind variable (`k3` in this example) as
  -- injective.  The reason is that result type has always an assigned kind and
  -- therefore we can always infer the result kind if we know the result type.
  -- But this does not seem to be useful in any way so we don't do it.  (Another
  -- reason is that the implementation would not be straightforward.)
tcInjectivity tcbs (Just (L loc (InjectivityAnn _ lInjNames)))
  = setSrcSpan loc $
    do { let tvs = binderVars tcbs
       ; dflags <- getDynFlags
       ; checkTc (xopt LangExt.TypeFamilyDependencies dflags)
                 (text "Illegal injectivity annotation" $$
                  text "Use TypeFamilyDependencies to allow this")
       ; inj_tvs <- mapM (tcLookupTyVar . unLoc) lInjNames
       ; inj_tvs <- mapM zonkTcTyVarToTyVar inj_tvs -- zonk the kinds
       ; let inj_ktvs = filterVarSet isTyVar $  -- no injective coercion vars
                        closeOverKinds (mkVarSet inj_tvs)
       ; let inj_bools = map (`elemVarSet` inj_ktvs) tvs
       ; traceTc "tcInjectivity" (vcat [ ppr tvs, ppr lInjNames, ppr inj_tvs
                                       , ppr inj_ktvs, ppr inj_bools ])
       ; return $ Injective inj_bools }

tcTySynRhs :: RolesInfo
           -> Name
           -> [TyConBinder] -> Kind
           -> LHsType Name -> TcM TyCon
tcTySynRhs roles_info tc_name binders res_kind hs_ty
  = do { env <- getLclEnv
       ; traceTc "tc-syn" (ppr tc_name $$ ppr (tcl_env env))
       ; rhs_ty <- solveEqualities $ tcCheckLHsType hs_ty res_kind
       ; rhs_ty <- zonkTcTypeToType emptyZonkEnv rhs_ty
       ; let roles = roles_info tc_name
             tycon = buildSynTyCon tc_name binders res_kind roles rhs_ty
       ; return tycon }

tcDataDefn :: RolesInfo -> Name
           -> [TyConBinder] -> Kind
           -> HsDataDefn Name -> TcM TyCon
  -- NB: not used for newtype/data instances (whether associated or not)
tcDataDefn roles_info
           tc_name tycon_binders res_kind
         (HsDataDefn { dd_ND = new_or_data, dd_cType = cType
                     , dd_ctxt = ctxt, dd_kindSig = mb_ksig
                     , dd_cons = cons })
 =  do { (extra_bndrs, real_res_kind) <- tcDataKindSig res_kind
       ; let final_bndrs  = tycon_binders `chkAppend` extra_bndrs
             roles        = roles_info tc_name

       ; stupid_tc_theta <- solveEqualities $ tcHsContext ctxt
       ; stupid_theta    <- zonkTcTypeToTypes emptyZonkEnv
                                              stupid_tc_theta
       ; kind_signatures <- xoptM LangExt.KindSignatures
       ; tcg_env         <- getGblEnv
       ; let hsc_src = tcg_src tcg_env

             -- Check that we don't use kind signatures without Glasgow extensions
       ; when (isJust mb_ksig) $
         checkTc (kind_signatures) (badSigTyDecl tc_name)

       ; gadt_syntax <- dataDeclChecks tc_name new_or_data stupid_theta cons

       ; tycon <- fixM $ \ tycon -> do
             { let res_ty = mkTyConApp tycon (mkTyVarTys (binderVars final_bndrs))
             ; data_cons <- tcConDecls tycon (final_bndrs, res_ty) cons
             ; tc_rhs    <- mk_tc_rhs hsc_src tycon data_cons
             ; tc_rep_nm <- newTyConRepName tc_name
             ; return (mkAlgTyCon tc_name
                                  final_bndrs
                                  real_res_kind
                                  roles
                                  (fmap unLoc cType)
                                  stupid_theta tc_rhs
                                  (VanillaAlgTyCon tc_rep_nm)
                                  gadt_syntax) }
       ; traceTc "tcDataDefn" (ppr tc_name $$ ppr tycon_binders $$ ppr extra_bndrs)
       ; return tycon }
  where
    -- In hs-boot, a 'data' declaration with no constructors
    -- indicates an nominally distinct abstract data type.
    mk_tc_rhs HsBootFile _ []
      = return (AbstractTyCon DistinctNominalAbstract)

    -- In hsig, a 'data' declaration indicates a skolem
    -- abstract data type. See 'HowAbstract' and Note
    -- [Skolem abstract data] for more commentary.
    mk_tc_rhs HsigFile _ []
      = return (AbstractTyCon SkolemAbstract)

    mk_tc_rhs _ tycon data_cons
      = case new_or_data of
          DataType -> return (mkDataTyConRhs data_cons)
          NewType  -> ASSERT( not (null data_cons) )
                      mkNewTyConRhs tc_name tycon (head data_cons)

{-
************************************************************************
*                                                                      *
               Typechecking associated types (in class decls)
               (including the associated-type defaults)
*                                                                      *
************************************************************************

Note [Associated type defaults]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following is an example of associated type defaults:
             class C a where
               data D a

               type F a b :: *
               type F a b = [a]        -- Default

Note that we can get default definitions only for type families, not data
families.
-}

tcClassATs :: Name                  -- The class name (not knot-tied)
           -> Class                 -- The class parent of this associated type
           -> [LFamilyDecl Name]    -- Associated types.
           -> [LTyFamDefltEqn Name] -- Associated type defaults.
           -> TcM [ClassATItem]
tcClassATs class_name cls ats at_defs
  = do {  -- Complain about associated type defaults for non associated-types
         sequence_ [ failWithTc (badATErr class_name n)
                   | n <- map at_def_tycon at_defs
                   , not (n `elemNameSet` at_names) ]
       ; mapM tc_at ats }
  where
    at_def_tycon :: LTyFamDefltEqn Name -> Name
    at_def_tycon (L _ eqn) = unLoc (tfe_tycon eqn)

    at_fam_name :: LFamilyDecl Name -> Name
    at_fam_name (L _ decl) = unLoc (fdLName decl)

    at_names = mkNameSet (map at_fam_name ats)

    at_defs_map :: NameEnv [LTyFamDefltEqn Name]
    -- Maps an AT in 'ats' to a list of all its default defs in 'at_defs'
    at_defs_map = foldr (\at_def nenv -> extendNameEnv_C (++) nenv
                                          (at_def_tycon at_def) [at_def])
                        emptyNameEnv at_defs

    tc_at at = do { fam_tc <- addLocM (tcFamDecl1 (Just cls)) at
                  ; let at_defs = lookupNameEnv at_defs_map (at_fam_name at)
                                  `orElse` []
                  ; atd <- tcDefaultAssocDecl fam_tc at_defs
                  ; return (ATI fam_tc atd) }

-------------------------
tcDefaultAssocDecl :: TyCon                    -- ^ Family TyCon (not knot-tied)
                   -> [LTyFamDefltEqn Name]         -- ^ Defaults
                   -> TcM (Maybe (Type, SrcSpan))   -- ^ Type checked RHS
tcDefaultAssocDecl _ []
  = return Nothing  -- No default declaration

tcDefaultAssocDecl _ (d1:_:_)
  = failWithTc (text "More than one default declaration for"
                <+> ppr (tfe_tycon (unLoc d1)))

tcDefaultAssocDecl fam_tc [L loc (TyFamEqn { tfe_tycon = L _ tc_name
                                           , tfe_pats = hs_tvs
                                           , tfe_rhs = rhs })]
  | HsQTvs { hsq_implicit = imp_vars, hsq_explicit = exp_vars } <- hs_tvs
  = -- See Note [Type-checking default assoc decls]
    setSrcSpan loc $
    tcAddFamInstCtxt (text "default type instance") tc_name $
    do { traceTc "tcDefaultAssocDecl" (ppr tc_name)
       ; let shape@(fam_tc_name, fam_arity, _, _) = famTyConShape fam_tc

       -- Kind of family check
       ; ASSERT( fam_tc_name == tc_name )
         checkTc (isTypeFamilyTyCon fam_tc) (wrongKindOfFamily fam_tc)

       -- Arity check
       ; checkTc (length exp_vars == fam_arity)
                 (wrongNumberOfParmsErr fam_arity)

       -- Typecheck RHS
       ; let pats = HsIB { hsib_vars = imp_vars ++ map hsLTyVarName exp_vars
                         , hsib_body = map hsLTyVarBndrToType exp_vars }
          -- NB: Use tcFamTyPats, not tcTyClTyVars. The latter expects to get
          -- the LHsQTyVars used for declaring a tycon, but the names here
          -- are different.
       ; (pats', rhs_ty)
           <- tcFamTyPats shape Nothing pats
              (discardResult . tcCheckLHsType rhs) $ \tvs pats rhs_kind ->
              do { rhs_ty <- solveEqualities $
                             tcCheckLHsType rhs rhs_kind

                     -- Zonk the patterns etc into the Type world
                 ; (ze, _) <- zonkTyBndrsX emptyZonkEnv tvs
                 ; pats'   <- zonkTcTypeToTypes ze pats
                 ; rhs_ty'  <- zonkTcTypeToType ze rhs_ty
                 ; return (pats', rhs_ty') }

         -- See Note [Type-checking default assoc decls]
       ; case tcMatchTys pats' (mkTyVarTys (tyConTyVars fam_tc)) of
           Just subst -> return (Just (substTyUnchecked subst rhs_ty, loc) )
           Nothing    -> failWithTc (defaultAssocKindErr fam_tc)
           -- We check for well-formedness and validity later,
           -- in checkValidClass
     }

{- Note [Type-checking default assoc decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this default declaration for an associated type

   class C a where
      type F (a :: k) b :: *
      type F x y = Proxy x -> y

Note that the class variable 'a' doesn't scope over the default assoc
decl (rather oddly I think), and (less oddly) neither does the second
argument 'b' of the associated type 'F', or the kind variable 'k'.
Instead, the default decl is treated more like a top-level type
instance.

However we store the default rhs (Proxy x -> y) in F's TyCon, using
F's own type variables, so we need to convert it to (Proxy a -> b).
We do this by calling tcMatchTys to match them up.  This also ensures
that x's kind matches a's and similarly for y and b.  The error
message isn't great, mind you.  (Trac #11361 was caused by not doing a
proper tcMatchTys here.)  -}

-------------------------
kcTyFamInstEqn :: FamTyConShape -> LTyFamInstEqn Name -> TcM ()
kcTyFamInstEqn fam_tc_shape@(fam_tc_name,_,_,_)
    (L loc (TyFamEqn { tfe_tycon = L _ eqn_tc_name
                     , tfe_pats  = pats
                     , tfe_rhs   = hs_ty }))
  = setSrcSpan loc $
    do { checkTc (fam_tc_name == eqn_tc_name)
                 (wrongTyFamName fam_tc_name eqn_tc_name)
       ; discardResult $
         tc_fam_ty_pats fam_tc_shape Nothing -- not an associated type
                        pats (discardResult . (tcCheckLHsType hs_ty)) }

tcTyFamInstEqn :: FamTyConShape -> Maybe ClsInstInfo -> LTyFamInstEqn Name -> TcM CoAxBranch
-- Needs to be here, not in TcInstDcls, because closed families
-- (typechecked here) have TyFamInstEqns
tcTyFamInstEqn fam_tc_shape@(fam_tc_name,_,_,_) mb_clsinfo
    (L loc (TyFamEqn { tfe_tycon = L _ eqn_tc_name
                     , tfe_pats  = pats
                     , tfe_rhs   = hs_ty }))
  = ASSERT( fam_tc_name == eqn_tc_name )
    setSrcSpan loc $
    tcFamTyPats fam_tc_shape mb_clsinfo pats
                (discardResult . (tcCheckLHsType hs_ty)) $
                    \tvs pats res_kind ->
    do { rhs_ty <- solveEqualities $ tcCheckLHsType hs_ty res_kind

       ; (ze, tvs') <- zonkTyBndrsX emptyZonkEnv tvs
       ; pats'      <- zonkTcTypeToTypes ze pats
       ; rhs_ty'    <- zonkTcTypeToType ze rhs_ty
       ; traceTc "tcTyFamInstEqn" (ppr fam_tc_name <+> pprTyVars tvs')
          -- don't print out the pats here, as they might be zonked inside the knot
       ; return (mkCoAxBranch tvs' [] pats' rhs_ty'
                              (map (const Nominal) tvs')
                              loc) }

kcDataDefn :: Name                -- ^ the family name, for error msgs only
           -> HsTyPats Name       -- ^ the patterns, for error msgs only
           -> HsDataDefn Name     -- ^ the RHS
           -> TcKind              -- ^ the expected kind
           -> TcM ()
-- Used for 'data instance' only
-- Ordinary 'data' is handled by kcTyClDec
kcDataDefn fam_name (HsIB { hsib_body = pats })
           (HsDataDefn { dd_ctxt = ctxt, dd_cons = cons, dd_kindSig = mb_kind }) res_k
  = do  { _ <- tcHsContext ctxt
        ; checkNoErrs $ mapM_ (wrapLocM kcConDecl) cons
          -- See Note [Failing early in kcDataDefn]
        ; discardResult $
          case mb_kind of
            Nothing -> unifyKind (Just hs_ty_pats) res_k liftedTypeKind
            Just k  -> do { k' <- tcLHsKind k
                          ; unifyKind (Just hs_ty_pats) res_k k' } }
  where
    hs_ty_pats = mkHsAppTys (noLoc $ HsTyVar NotPromoted (noEmb fam_name)) pats

{-
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

Note [Type-checking type patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When typechecking the patterns of a family instance declaration, we can't
rely on using the family TyCon, because this is sometimes called
from within a type-checking knot. (Specifically for closed type families.)
The type FamTyConShape gives just enough information to do the job.

See also Note [tc_fam_ty_pats vs tcFamTyPats]

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
-}

-----------------
type FamTyConShape = (Name, Arity, [TyConBinder], Kind)
  -- See Note [Type-checking type patterns]

famTyConShape :: TyCon -> FamTyConShape
famTyConShape fam_tc
  = ( tyConName fam_tc
    , length $ filterOutInvisibleTyVars fam_tc (tyConTyVars fam_tc)
    , tyConBinders fam_tc
    , tyConResKind fam_tc )

tc_fam_ty_pats :: FamTyConShape
               -> Maybe ClsInstInfo
               -> HsTyPats Name       -- Patterns
               -> (TcKind -> TcM ())  -- Kind checker for RHS
                                      -- result is ignored
               -> TcM ([Type], Kind)
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

tc_fam_ty_pats (name, _, binders, res_kind) mb_clsinfo
               (HsIB { hsib_body = arg_pats, hsib_vars = tv_names })
               kind_checker
  = do { -- Kind-check and quantify
         -- See Note [Quantifying over family patterns]
         (_, (insted_res_kind, typats)) <- tcImplicitTKBndrs tv_names $
         do { (insting_subst, _leftover_binders, args, leftovers, n)
                <- tcInferArgs name binders (thdOf3 <$> mb_clsinfo) arg_pats
            ; case leftovers of
                hs_ty:_ -> addErrTc $ too_many_args hs_ty n
                _       -> return ()
              -- don't worry about leftover_binders; TcValidity catches them

            ; let insted_res_kind = substTyUnchecked insting_subst res_kind
            ; kind_checker insted_res_kind
            ; return ((insted_res_kind, args), emptyVarSet) }

       ; return (typats, insted_res_kind) }
  where
    too_many_args hs_ty n
      = hang (text "Too many parameters to" <+> ppr name <> colon)
           2 (vcat [ ppr hs_ty <+> text "is unexpected;"
                   , text (if n == 1 then "expected" else "expected only") <+>
                     speakNOf (n-1) (text "parameter") ])

-- See Note [tc_fam_ty_pats vs tcFamTyPats]
tcFamTyPats :: FamTyConShape
            -> Maybe ClsInstInfo
            -> HsTyPats Name         -- patterns
            -> (TcKind -> TcM ())    -- kind-checker for RHS
            -> (   [TcTyVar]         -- Kind and type variables
                -> [TcType]          -- Kind and type arguments
                -> TcKind
                -> TcM a)            -- NB: You can use solveEqualities here.
            -> TcM a
tcFamTyPats fam_shape@(name,_,_,_) mb_clsinfo pats kind_checker thing_inside
  = do { (typats, res_kind)
            <- solveEqualities $  -- See Note [Constraints in patterns]
               tc_fam_ty_pats fam_shape mb_clsinfo pats kind_checker

          {- TODO (RAE): This should be cleverer. Consider this:

                 type family F a

                 data G a where
                   MkG :: F a ~ Bool => G a

                 type family Foo (x :: G a) :: F a
                 type instance Foo MkG = False

             This should probably be accepted. Yet the solveEqualities
             will fail, unable to solve (F a ~ Bool)
             We want to quantify over that proof.
             But see Note [Constraints in patterns]
             below, which is missing this piece. -}


            -- Find free variables (after zonking) and turn
            -- them into skolems, so that we don't subsequently
            -- replace a meta kind var with (Any *)
            -- Very like kindGeneralize
       ; vars  <- zonkTcTypesAndSplitDepVars typats
       ; qtkvs <- quantifyZonkedTyVars emptyVarSet vars

       ; MASSERT( isEmptyVarSet $ coVarsOfTypes typats )
           -- This should be the case, because otherwise the solveEqualities
           -- above would fail. TODO (RAE): Update once the solveEqualities
           -- bit is cleverer.

       ; traceTc "tcFamTyPats" (ppr name $$ ppr typats $$ ppr qtkvs)
            -- Don't print out too much, as we might be in the knot

       ; tcExtendTyVarEnv qtkvs $
            -- Extend envt with TcTyVars not TyVars, because the
            -- kind checking etc done by thing_inside does not expect
            -- to encounter TyVars; it expects TcTyVars
         thing_inside qtkvs typats res_kind }

{-
Note [Constraints in patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NB: This isn't the whole story. See comment in tcFamTyPats.

At first glance, it seems there is a complicated story to tell in tcFamTyPats
around constraint solving. After all, type family patterns can now do
GADT pattern-matching, which is jolly complicated. But, there's a key fact
which makes this all simple: everything is at top level! There cannot
be untouchable type variables. There can't be weird interaction between
case branches. There can't be global skolems.

This means that the semantics of type-level GADT matching is a little
different than term level. If we have

  data G a where
    MkGBool :: G Bool

And then

  type family F (a :: G k) :: k
  type instance F MkGBool = True

we get

  axF : F Bool (MkGBool <Bool>) ~ True

Simple! No casting on the RHS, because we can affect the kind parameter
to F.

If we ever introduce local type families, this all gets a lot more
complicated, and will end up looking awfully like term-level GADT
pattern-matching.


** The new story **

Here is really what we want:

The matcher really can't deal with covars in arbitrary spots in coercions.
But it can deal with covars that are arguments to GADT data constructors.
So we somehow want to allow covars only in precisely those spots, then use
them as givens when checking the RHS. TODO (RAE): Implement plan.


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
which we pick up using the (tyCoVarsOfTypes typats) in the result of
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
  * Bring into scope [ "k" -> k:*, "a" -> a:k ]
  * Kind-check the RHS
  * Quantify the type instance over k and k', as well as a,b, thus
       type instance [k, k', a:Maybe k, b:k']
                     KindFam (Maybe k) k' a b = T k k' a b -> Int

Notice that in the third step we quantify over all the visibly-mentioned
type variables (a,b), but also over the implicitly mentioned kind variables
(k, k').  In this case one is bound explicitly but often there will be
none. The role of the kind signature (a :: Maybe k) is to add a constraint
that 'a' must have that kind, and to bring 'k' into scope.



************************************************************************
*                                                                      *
               Data types
*                                                                      *
************************************************************************
-}

dataDeclChecks :: Name -> NewOrData -> ThetaType -> [LConDecl Name] -> TcM Bool
dataDeclChecks tc_name new_or_data stupid_theta cons
  = do {   -- Check that we don't use GADT syntax in H98 world
         gadtSyntax_ok <- xoptM LangExt.GADTSyntax
       ; let gadt_syntax = consUseGadtSyntax cons
       ; checkTc (gadtSyntax_ok || not gadt_syntax) (badGadtDecl tc_name)

           -- Check that the stupid theta is empty for a GADT-style declaration
       ; checkTc (null stupid_theta || not gadt_syntax) (badStupidTheta tc_name)

         -- Check that a newtype has exactly one constructor
         -- Do this before checking for empty data decls, so that
         -- we don't suggest -XEmptyDataDecls for newtypes
       ; checkTc (new_or_data == DataType || isSingleton cons)
                (newtypeConError tc_name (length cons))

                -- Check that there's at least one condecl,
         -- or else we're reading an hs-boot file, or -XEmptyDataDecls
       ; empty_data_decls <- xoptM LangExt.EmptyDataDecls
       ; is_boot <- tcIsHsBootOrSig  -- Are we compiling an hs-boot file?
       ; checkTc (not (null cons) || empty_data_decls || is_boot)
                 (emptyConDeclsErr tc_name)
       ; return gadt_syntax }


-----------------------------------
consUseGadtSyntax :: [LConDecl a] -> Bool
consUseGadtSyntax (L _ (ConDeclGADT { }) : _) = True
consUseGadtSyntax _                           = False
                 -- All constructors have same shape

-----------------------------------
tcConDecls :: TyCon -> ([TyConBinder], Type)
           -> [LConDecl Name] -> TcM [DataCon]
  -- Why both the tycon tyvars and binders? Because the tyvars
  -- have all the names and the binders have the visibilities.
tcConDecls rep_tycon (tmpl_bndrs, res_tmpl)
  = concatMapM $ addLocM $
    tcConDecl rep_tycon tmpl_bndrs res_tmpl

tcConDecl :: TyCon             -- Representation tycon. Knot-tied!
          -> [TyConBinder] -> Type
                 -- Return type template (with its template tyvars)
                 --    (tvs, T tys), where T is the family TyCon
          -> ConDecl Name
          -> TcM [DataCon]

tcConDecl rep_tycon tmpl_bndrs res_tmpl
          (ConDeclH98 { con_name = name
                      , con_qvars = hs_qvars, con_cxt = hs_ctxt
                      , con_details = hs_details })
  = addErrCtxt (dataConCtxtName [name]) $
    do { traceTc "tcConDecl 1" (ppr name)
       ; let (hs_kvs, hs_tvs) = case hs_qvars of
               Nothing -> ([], [])
               Just (HsQTvs { hsq_implicit = kvs, hsq_explicit = tvs })
                       -> (kvs, tvs)
       ; (imp_tvs, (exp_tvs, ctxt, arg_tys, field_lbls, stricts))
           <- solveEqualities $
              tcImplicitTKBndrs hs_kvs $
              tcExplicitTKBndrs hs_tvs $ \ exp_tvs ->
              do { traceTc "tcConDecl" (ppr name <+> text "tvs:" <+> ppr hs_tvs)
                 ; ctxt <- tcHsContext (fromMaybe (noLoc []) hs_ctxt)
                 ; btys <- tcConArgs hs_details
                 ; field_lbls <- lookupConstructorFields (unLocEmb name)
                 ; let (arg_tys, stricts) = unzip btys
                       bound_vars  = allBoundVariabless ctxt `unionVarSet`
                                     allBoundVariabless arg_tys
                 ; return ((exp_tvs, ctxt, arg_tys, field_lbls, stricts), bound_vars)
                 }
         -- imp_tvs are user-written kind variables, without an explicit binding site
         -- exp_tvs have binding sites
         -- the kvs below are those kind variables entirely unmentioned by the user
         --   and discovered only by generalization

             -- Kind generalisation
       ; let all_user_tvs = imp_tvs ++ exp_tvs
       ; vars <- zonkTcTypeAndSplitDepVars (mkSpecForAllTys all_user_tvs $
                                            mkFunTys ctxt $
                                            mkFunTys arg_tys $
                                            unitTy)
                 -- That type is a lie, of course. (It shouldn't end in ()!)
                 -- And we could construct a proper result type from the info
                 -- at hand. But the result would mention only the tmpl_tvs,
                 -- and so it just creates more work to do it right. Really,
                 -- we're doing this to get the right behavior around removing
                 -- any vars bound in exp_binders.

       ; kvs <- quantifyZonkedTyVars (mkVarSet (binderVars tmpl_bndrs)) vars

             -- Zonk to Types
       ; (ze, qkvs)      <- zonkTyBndrsX emptyZonkEnv kvs
       ; (ze, user_qtvs) <- zonkTyBndrsX ze all_user_tvs
       ; arg_tys         <- zonkTcTypeToTypes ze arg_tys
       ; ctxt            <- zonkTcTypeToTypes ze ctxt

       ; fam_envs <- tcGetFamInstEnvs

       -- Can't print univ_tvs, arg_tys etc, because we are inside the knot here
       ; traceTc "tcConDecl 2" (ppr name $$ ppr field_lbls)
       ; let
           ex_tvs = mkTyVarBinders Inferred qkvs ++
                    mkTyVarBinders Specified user_qtvs
           buildOneDataCon (L _ name) = do
             { is_infix <- tcConIsInfixH98 (unEmb name) hs_details
             ; rep_nm   <- newTyConRepName $ unEmb name

             ; buildDataCon fam_envs (unEmb name) is_infix rep_nm
                            stricts Nothing field_lbls
                            (mkDataConUnivTyVarBinders tmpl_bndrs)
                            ex_tvs
                            [{- no eq_preds -}] ctxt arg_tys
                            res_tmpl rep_tycon
                  -- NB:  we put data_tc, the type constructor gotten from the
                  --      constructor type signature into the data constructor;
                  --      that way checkValidDataCon can complain if it's wrong.
             }
       ; traceTc "tcConDecl 2" (ppr name)
       ; mapM buildOneDataCon [name]
       }

tcConDecl rep_tycon tmpl_bndrs res_tmpl
          (ConDeclGADT { con_names = names, con_type = ty })
  = addErrCtxt (dataConCtxtName names) $
    do { traceTc "tcConDecl 1" (ppr names)
       ; (user_tvs, ctxt, stricts, field_lbls, arg_tys, res_ty,hs_details)
           <- tcGadtSigType (ppr names) (unLocEmb $ head names) ty

       ; vars <- zonkTcTypeAndSplitDepVars (mkSpecForAllTys user_tvs $
                                            mkFunTys ctxt $
                                            mkFunTys arg_tys $
                                            res_ty)
       ; tkvs <- quantifyZonkedTyVars emptyVarSet vars

             -- Zonk to Types
       ; (ze, qtkvs) <- zonkTyBndrsX emptyZonkEnv (tkvs ++ user_tvs)
       ; arg_tys <- zonkTcTypeToTypes ze arg_tys
       ; ctxt    <- zonkTcTypeToTypes ze ctxt
       ; res_ty  <- zonkTcTypeToType ze res_ty

       ; let (univ_tvs, ex_tvs, eq_preds, res_ty', arg_subst)
               = rejigConRes tmpl_bndrs res_tmpl qtkvs res_ty
             -- NB: this is a /lazy/ binding, so we pass five thunks to buildDataCon
             --     without yet forcing the guards in rejigConRes
             -- See Note [Checking GADT return types]

             -- See Note [Wrong visibility for GADTs]
             univ_bndrs = mkTyVarBinders Specified univ_tvs
             ex_bndrs   = mkTyVarBinders Specified ex_tvs

       ; fam_envs <- tcGetFamInstEnvs

       -- Can't print univ_tvs, arg_tys etc, because we are inside the knot here
       ; traceTc "tcConDecl 2" (ppr names $$ ppr field_lbls)
       ; let
           buildOneDataCon (L _ name) = do
             { is_infix <- tcConIsInfixGADT (unEmb name) hs_details
             ; rep_nm   <- newTyConRepName $ unEmb name

             ; buildDataCon fam_envs (unEmb name) is_infix
                            rep_nm
                            stricts Nothing field_lbls
                            univ_bndrs ex_bndrs eq_preds
                            (substTys arg_subst ctxt)
                            (substTys arg_subst arg_tys)
                            (substTy  arg_subst res_ty')
                            rep_tycon
                  -- NB:  we put data_tc, the type constructor gotten from the
                  --      constructor type signature into the data constructor;
                  --      that way checkValidDataCon can complain if it's wrong.
             }
       ; traceTc "tcConDecl 2" (ppr names)
       ; mapM buildOneDataCon names
       }


tcGadtSigType :: SDoc -> Name -> LHsSigType Name
              -> TcM ( [TcTyVar], [PredType],[HsSrcBang], [FieldLabel], [Type], Type
                     , HsConDetails (LHsType Name)
                                    (Located [LConDeclField Name]) )
tcGadtSigType doc name ty@(HsIB { hsib_vars = vars })
  = do { let (hs_details', res_ty', cxt, gtvs) = gadtDeclDetails ty
       ; (hs_details, res_ty) <- updateGadtResult failWithTc doc hs_details' res_ty'
       ; (imp_tvs, (exp_tvs, ctxt, arg_tys, res_ty, field_lbls, stricts))
           <- solveEqualities $
              tcImplicitTKBndrs vars $
              tcExplicitTKBndrs gtvs $ \ exp_tvs ->
              do { ctxt <- tcHsContext cxt
                 ; btys <- tcConArgs hs_details
                 ; ty' <- tcHsLiftedType res_ty
                 ; field_lbls <- lookupConstructorFields name
                 ; let (arg_tys, stricts) = unzip btys
                       bound_vars = allBoundVariabless ctxt `unionVarSet`
                                    allBoundVariabless arg_tys

                 ; return ((exp_tvs, ctxt, arg_tys, ty', field_lbls, stricts), bound_vars)
                 }
       ; return (imp_tvs ++ exp_tvs, ctxt, stricts, field_lbls, arg_tys, res_ty, hs_details)
       }

tcConIsInfixH98 :: Name
             -> HsConDetails (LHsType Name) (Located [LConDeclField Name])
             -> TcM Bool
tcConIsInfixH98 _   details
  = case details of
           InfixCon {}  -> return True
           _            -> return False

tcConIsInfixGADT :: Name
             -> HsConDetails (LHsType Name) (Located [LConDeclField Name])
             -> TcM Bool
tcConIsInfixGADT con details
  = case details of
           InfixCon {}  -> return True
           RecCon {}    -> return False
           PrefixCon arg_tys           -- See Note [Infix GADT constructors]
               | isSymOcc (getOccName con)
               , [_ty1,_ty2] <- arg_tys
                  -> do { fix_env <- getFixityEnv
                        ; return (con `elemNameEnv` fix_env) }
               | otherwise -> return False

tcConArgs :: HsConDeclDetails Name
          -> TcM [(TcType, HsSrcBang)]
tcConArgs (PrefixCon btys)
  = mapM tcConArg btys
tcConArgs (InfixCon bty1 bty2)
  = do { bty1' <- tcConArg bty1
       ; bty2' <- tcConArg bty2
       ; return [bty1', bty2'] }
tcConArgs (RecCon fields)
  = mapM tcConArg btys
  where
    -- We need a one-to-one mapping from field_names to btys
    combined = map (\(L _ f) -> (cd_fld_names f,cd_fld_type f)) (unLoc fields)
    explode (ns,ty) = zip ns (repeat ty)
    exploded = concatMap explode combined
    (_,btys) = unzip exploded


tcConArg :: LHsType Name -> TcM (TcType, HsSrcBang)
tcConArg bty
  = do  { traceTc "tcConArg 1" (ppr bty)
        ; arg_ty <- tcHsOpenType (getBangType bty)
             -- Newtypes can't have unboxed types, but we check
             -- that in checkValidDataCon; this tcConArg stuff
             -- doesn't happen for GADT-style declarations
        ; traceTc "tcConArg 2" (ppr bty)
        ; return (arg_ty, getBangStrictness bty) }

{-
Note [Wrong visibility for GADTs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GADT tyvars shouldn't all be specified, but it's hard to do much better, as
described in #11721, which is duplicated here for convenience:

Consider

  data X a where
    MkX :: b -> Proxy a -> X a

According to the rules around specified vs. generalized variables around
TypeApplications, the type of MkX should be

  MkX :: forall {k} (b :: *) (a :: k). b -> Proxy a -> X a

A few things to note:

  * The k isn't available for TypeApplications (that's why it's in braces),
    because it is not user-written.

  * The b is quantified before the a, because b comes before a in the
    user-written type signature for MkX.

Both of these bullets are currently violated. GHCi reports MkX's type as

  MkX :: forall k (a :: k) b. b -> Proxy a -> X a

It turns out that this is a hard to fix. The problem is that GHC expects data
constructors to have their universal variables followed by their existential
variables, always. And yet that's violated in the desired type for MkX.
Furthermore, given the way that GHC deals with GADT return types ("rejigging",
in technical parlance), it's inconvenient to get the specified/generalized
distinction correct.

Given time constraints, I'm afraid fixing this all won't make it for 8.0.

Happily, there is are easy-to-articulate rules governing GHC's current (wrong)
behavior. In a GADT-syntax data constructor:

  * All kind and type variables are considered specified and available for
    visible type application.

  * Universal variables always come first, in precisely the order they appear
    in the tycon. Note that universals that are constrained by a GADT return
    type are missing from the datacon.

  * Existential variables come next. Their order is determined by a
    user-written forall; or, if there is none, by taking the left-to-right
    order in the datacon's type and doing a stable topological sort. (This
    stable topological sort step is the same as for other user-written type
    signatures.)

Note [Infix GADT constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not currently have syntax to declare an infix constructor in GADT syntax,
but it makes a (small) difference to the Show instance.  So as a slightly
ad-hoc solution, we regard a GADT data constructor as infix if
  a) it is an operator symbol
  b) it has two arguments
  c) there is a fixity declaration for it
For example:
   infix 6 (:--:)
   data T a where
     (:--:) :: t1 -> t2 -> T Int


Note [Checking GADT return types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is a delicacy around checking the return types of a datacon. The
central problem is dealing with a declaration like

  data T a where
    MkT :: T a -> Q a

Note that the return type of MkT is totally bogus. When creating the T
tycon, we also need to create the MkT datacon, which must have a "rejigged"
return type. That is, the MkT datacon's type must be transformed to have
a uniform return type with explicit coercions for GADT-like type parameters.
This rejigging is what rejigConRes does. The problem is, though, that checking
that the return type is appropriate is much easier when done over *Type*,
not *HsType*, and doing a call to tcMatchTy will loop because T isn't fully
defined yet.

So, we want to make rejigConRes lazy and then check the validity of
the return type in checkValidDataCon.  To do this we /always/ return a
5-tuple from rejigConRes (so that we can extract ret_ty from it, which
checkValidDataCon needs), but the first four fields may be bogus if
the return type isn't valid (the last equation for rejigConRes).

This is better than an earlier solution which reduced the number of
errors reported in one pass.  See Trac #7175, and #10836.
-}

-- Example
--   data instance T (b,c) where
--      TI :: forall e. e -> T (e,e)
--
-- The representation tycon looks like this:
--   data :R7T b c where
--      TI :: forall b1 c1. (b1 ~ c1) => b1 -> :R7T b1 c1
-- In this case orig_res_ty = T (e,e)

rejigConRes :: [TyConBinder] -> Type    -- Template for result type; e.g.
                                  -- data instance T [a] b c = ...
                                  --      gives template ([a,b,c], T [a] b c)
                                  -- Type must be of kind *!
            -> [TyVar]            -- where MkT :: forall x y z. ...
            -> Type               -- res_ty type must be of kind *
            -> ([TyVar],          -- Universal
                [TyVar],          -- Existential (distinct OccNames from univs)
                [EqSpec],      -- Equality predicates
                Type,          -- Typechecked return type
                TCvSubst)      -- Substitution to apply to argument types
        -- We don't check that the TyCon given in the ResTy is
        -- the same as the parent tycon, because checkValidDataCon will do it

rejigConRes tmpl_bndrs res_tmpl dc_tvs res_ty
        -- E.g.  data T [a] b c where
        --         MkT :: forall x y z. T [(x,y)] z z
        -- The {a,b,c} are the tmpl_tvs, and the {x,y,z} are the dc_tvs
        --     (NB: unlike the H98 case, the dc_tvs are not all existential)
        -- Then we generate
        --      Univ tyvars     Eq-spec
        --          a              a~(x,y)
        --          b              b~z
        --          z
        -- Existentials are the leftover type vars: [x,y]
        -- So we return ([a,b,z], [x,y], [a~(x,y),b~z], T [(x,y)] z z)
  | Just subst <- ASSERT( isLiftedTypeKind (typeKind res_ty) )
                  ASSERT( isLiftedTypeKind (typeKind res_tmpl) )
                  tcMatchTy res_tmpl res_ty
  = let (univ_tvs, raw_eqs, kind_subst) = mkGADTVars tmpl_tvs dc_tvs subst
        raw_ex_tvs = dc_tvs `minusList` univ_tvs
        (arg_subst, substed_ex_tvs)
          = mapAccumL substTyVarBndr kind_subst raw_ex_tvs

        substed_eqs = map (substEqSpec arg_subst) raw_eqs
    in
    (univ_tvs, substed_ex_tvs, substed_eqs, res_ty, arg_subst)

  | otherwise
        -- If the return type of the data constructor doesn't match the parent
        -- type constructor, or the arity is wrong, the tcMatchTy will fail
        --    e.g   data T a b where
        --            T1 :: Maybe a   -- Wrong tycon
        --            T2 :: T [a]     -- Wrong arity
        -- We are detect that later, in checkValidDataCon, but meanwhile
        -- we must do *something*, not just crash.  So we do something simple
        -- albeit bogus, relying on checkValidDataCon to check the
        --  bad-result-type error before seeing that the other fields look odd
        -- See Note [Checking GADT return types]
  = (tmpl_tvs, dc_tvs `minusList` tmpl_tvs, [], res_ty, emptyTCvSubst)
  where
    tmpl_tvs = binderVars tmpl_bndrs

{-
Note [mkGADTVars]
~~~~~~~~~~~~~~~~~

Running example:

data T (k1 :: *) (k2 :: *) (a :: k2) (b :: k2) where
  MkT :: T x1 * (Proxy (y :: x1), z) z

We need the rejigged type to be

  MkT :: forall (x1 :: *) (k2 :: *) (a :: k2) (b :: k2).
         forall (y :: x1) (z :: *).
         (k2 ~ *, a ~ (Proxy x1 y, z), b ~ z)
      => T x1 k2 a b

You might naively expect that z should become a universal tyvar,
not an existential. (After all, x1 becomes a universal tyvar.)
The problem is that the universal tyvars must have exactly the
same kinds as the tyConTyVars. z has kind * while b has kind k2.
So we need an existential tyvar and a heterogeneous equality
constraint. (The b ~ z is a bit redundant with the k2 ~ * that
comes before in that b ~ z implies k2 ~ *. I'm sure we could do
some analysis that could eliminate k2 ~ *. But we don't do this
yet.)

The HsTypes have already been desugared to proper Types:

  T x1 * (Proxy (y :: x1), z) z
becomes
  [x1 :: *, y :: x1, z :: *]. T x1 * (Proxy x1 y, z) z

We start off by matching (T k1 k2 a b) with (T x1 * (Proxy x1 y, z) z). We
know this match will succeed because of the validity check (actually done
later, but laziness saves us -- see Note [Checking GADT return types]).
Thus, we get

  subst := { k1 |-> x1, k2 |-> *, a |-> (Proxy x1 y, z), b |-> z }

Now, we need to figure out what the GADT equalities should be. In this case,
we *don't* want (k1 ~ x1) to be a GADT equality: it should just be a
renaming. The others should be GADT equalities. We also need to make
sure that the universally-quantified variables of the datacon match up
with the tyvars of the tycon, as required for Core context well-formedness.
(This last bit is why we have to rejig at all!)

`choose` walks down the tycon tyvars, figuring out what to do with each one.
It carries two substitutions:
  - t_sub's domain is *template* or *tycon* tyvars, mapping them to variables
    mentioned in the datacon signature.
  - r_sub's domain is *result* tyvars, names written by the programmer in
    the datacon signature. The final rejigged type will use these names, but
    the subst is still needed because sometimes the printed name of these variables
    is different. (See choose_tv_name, below.)

Before explaining the details of `choose`, let's just look at its operation
on our example:

  choose [] [] {} {} [k1, k2, a, b]
  -->          -- first branch of `case` statement
  choose
    univs:    [x1 :: *]
    eq_spec:  []
    t_sub:    {k1 |-> x1}
    r_sub:    {x1 |-> x1}
    t_tvs:    [k2, a, b]
  -->          -- second branch of `case` statement
  choose
    univs:    [k2 :: *, x1 :: *]
    eq_spec:  [k2 ~ *]
    t_sub:    {k1 |-> x1, k2 |-> k2}
    r_sub:    {x1 |-> x1}
    t_tvs:    [a, b]
  -->          -- second branch of `case` statement
  choose
    univs:    [a :: k2, k2 :: *, x1 :: *]
    eq_spec:  [ a ~ (Proxy x1 y, z)
              , k2 ~ * ]
    t_sub:    {k1 |-> x1, k2 |-> k2, a |-> a}
    r_sub:    {x1 |-> x1}
    t_tvs:    [b]
  -->          -- second branch of `case` statement
  choose
    univs:    [b :: k2, a :: k2, k2 :: *, x1 :: *]
    eq_spec:  [ b ~ z
              , a ~ (Proxy x1 y, z)
              , k2 ~ * ]
    t_sub:    {k1 |-> x1, k2 |-> k2, a |-> a, b |-> z}
    r_sub:    {x1 |-> x1}
    t_tvs:    []
  -->          -- end of recursion
  ( [x1 :: *, k2 :: *, a :: k2, b :: k2]
  , [k2 ~ *, a ~ (Proxy x1 y, z), b ~ z]
  , {x1 |-> x1} )

`choose` looks up each tycon tyvar in the matching (it *must* be matched!). If
it finds a bare result tyvar (the first branch of the `case` statement), it
checks to make sure that the result tyvar isn't yet in the list of univ_tvs.
If it is in that list, then we have a repeated variable in the return type,
and we in fact need a GADT equality. We then check to make sure that the
kind of the result tyvar matches the kind of the template tyvar. This
check is what forces `z` to be existential, as it should be, explained above.
Assuming no repeated variables or kind-changing, we wish
to use the variable name given in the datacon signature (that is, `x1` not
`k1`), not the tycon signature (which may have been made up by
GHC). So, we add a mapping from the tycon tyvar to the result tyvar to t_sub.

If we discover that a mapping in `subst` gives us a non-tyvar (the second
branch of the `case` statement), then we have a GADT equality to create.
We create a fresh equality, but we don't extend any substitutions. The
template variable substitution is meant for use in universal tyvar kinds,
and these shouldn't be affected by any GADT equalities.

This whole algorithm is quite delicate, indeed. I (Richard E.) see two ways
of simplifying it:

1) The first branch of the `case` statement is really an optimization, used
in order to get fewer GADT equalities. It might be possible to make a GADT
equality for *every* univ. tyvar, even if the equality is trivial, and then
either deal with the bigger type or somehow reduce it later.

2) This algorithm strives to use the names for type variables as specified
by the user in the datacon signature. If we always used the tycon tyvar
names, for example, this would be simplified. This change would almost
certainly degrade error messages a bit, though.
-}

-- ^ From information about a source datacon definition, extract out
-- what the universal variables and the GADT equalities should be.
-- See Note [mkGADTVars].
mkGADTVars :: [TyVar]    -- ^ The tycon vars
           -> [TyVar]    -- ^ The datacon vars
           -> TCvSubst   -- ^ The matching between the template result type
                         -- and the actual result type
           -> ( [TyVar]
              , [EqSpec]
              , TCvSubst ) -- ^ The univ. variables, the GADT equalities,
                           -- and a subst to apply to the GADT equalities
                           -- and existentials.
mkGADTVars tmpl_tvs dc_tvs subst
  = choose [] [] empty_subst empty_subst tmpl_tvs
  where
    in_scope = mkInScopeSet (mkVarSet tmpl_tvs `unionVarSet` mkVarSet dc_tvs)
               `unionInScope` getTCvInScope subst
    empty_subst = mkEmptyTCvSubst in_scope

    choose :: [TyVar]           -- accumulator of univ tvs, reversed
           -> [EqSpec]          -- accumulator of GADT equalities, reversed
           -> TCvSubst          -- template substutition
           -> TCvSubst          -- res. substitution
           -> [TyVar]           -- template tvs (the univ tvs passed in)
           -> ( [TyVar]         -- the univ_tvs
              , [EqSpec]        -- GADT equalities
              , TCvSubst )       -- a substitution to fix kinds in ex_tvs

    choose univs eqs _t_sub r_sub []
      = (reverse univs, reverse eqs, r_sub)
    choose univs eqs t_sub r_sub (t_tv:t_tvs)
      | Just r_ty <- lookupTyVar subst t_tv
      = case getTyVar_maybe r_ty of
          Just r_tv
            |  not (r_tv `elem` univs)
            ,  tyVarKind r_tv `eqType` (substTy t_sub (tyVarKind t_tv))
            -> -- simple, well-kinded variable substitution.
               choose (r_tv:univs) eqs
                      (extendTvSubst t_sub t_tv r_ty')
                      (extendTvSubst r_sub r_tv r_ty')
                      t_tvs
            where
              r_tv1  = setTyVarName r_tv (choose_tv_name r_tv t_tv)
              r_ty'  = mkTyVarTy r_tv1

               -- not a simple substitution. make an equality predicate
          _ -> choose (t_tv':univs) (mkEqSpec t_tv' r_ty : eqs)
                      t_sub r_sub t_tvs
            where t_tv' = updateTyVarKind (substTy t_sub) t_tv

      | otherwise
      = pprPanic "mkGADTVars" (ppr tmpl_tvs $$ ppr subst)

      -- choose an appropriate name for a univ tyvar.
      -- This *must* preserve the Unique of the result tv, so that we
      -- can detect repeated variables. It prefers user-specified names
      -- over system names. A result variable with a system name can
      -- happen with GHC-generated implicit kind variables.
    choose_tv_name :: TyVar -> TyVar -> Name
    choose_tv_name r_tv t_tv
      | isSystemName r_tv_name
      = setNameUnique t_tv_name (getUnique r_tv_name)

      | otherwise
      = r_tv_name

      where
        r_tv_name = getName r_tv
        t_tv_name = getName t_tv

{-
Note [Substitution in template variables kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data G (a :: Maybe k) where
  MkG :: G Nothing

With explicit kind variables

data G k (a :: Maybe k) where
  MkG :: G k1 (Nothing k1)

Note how k1 is distinct from k. So, when we match the template
`G k a` against `G k1 (Nothing k1)`, we get a subst
[ k |-> k1, a |-> Nothing k1 ]. Even though this subst has two
mappings, we surely don't want to add (k, k1) to the list of
GADT equalities -- that would be overly complex and would create
more untouchable variables than we need. So, when figuring out
which tyvars are GADT-like and which aren't (the fundamental
job of `choose`), we want to treat `k` as *not* GADT-like.
Instead, we wish to substitute in `a`'s kind, to get (a :: Maybe k1)
instead of (a :: Maybe k). This is the reason for dealing
with a substitution in here.

However, we do not *always* want to substitute. Consider

data H (a :: k) where
  MkH :: H Int

With explicit kind variables:

data H k (a :: k) where
  MkH :: H * Int

Here, we have a kind-indexed GADT. The subst in question is
[ k |-> *, a |-> Int ]. Now, we *don't* want to substitute in `a`'s
kind, because that would give a constructor with the type

MkH :: forall (k :: *) (a :: *). (k ~ *) -> (a ~ Int) -> H k a

The problem here is that a's kind is wrong -- it needs to be k, not *!
So, if the matching for a variable is anything but another bare variable,
we drop the mapping from the substitution before proceeding. This
was not an issue before kind-indexed GADTs because this case could
never happen.

************************************************************************
*                                                                      *
                Validity checking
*                                                                      *
************************************************************************

Validity checking is done once the mutually-recursive knot has been
tied, so we can look at things freely.
-}

checkValidTyCl :: TyCon -> TcM TyCon
checkValidTyCl tc
  = setSrcSpan (getSrcSpan tc) $
    addTyConCtxt tc $
    recoverM recovery_code
             (do { traceTc "Starting validity for tycon" (ppr tc)
                 ; checkValidTyCon tc
                 ; traceTc "Done validity for tycon" (ppr tc)
                 ; return tc })
  where
    recovery_code -- See Note [Recover from validity error]
      = do { traceTc "Aborted validity for tycon" (ppr tc)
           ; return fake_tc }
    fake_tc | isFamilyTyCon tc || isTypeSynonymTyCon tc
            = makeTyConAbstract tc
            | otherwise
            = tc

{- Note [Recover from validity error]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We recover from a validity error in a type or class, which allows us
to report multiple validity errors. In the failure case we return a
TyCon of the right kind, but with no interesting behaviour
(makeTyConAbstract). Why?  Suppose we have
   type T a = Fun
where Fun is a type family of arity 1.  The RHS is invalid, but we
want to go on checking validity of subsequent type declarations.
So we replace T with an abstract TyCon which will do no harm.
See indexed-types/should_fail/BadSock and Trac #10896

Painfully, though, we *don't* want to do this for classes.
Consider tcfail041:
   class (?x::Int) => C a where ...
   instance C Int
The class is invalid because of the superclass constraint.  But
we still want it to look like a /class/, else the instance bleats
that the instance is mal-formed because it hasn't got a class in
the head.
-}

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
  | isPrimTyCon tc   -- Happens when Haddock'ing GHC.Prim
  = return ()

  | otherwise
  = do { traceTc "checkValidTyCon" (ppr tc $$ ppr (tyConClass_maybe tc))
       ; checkValidTyConTyVars tc
       ; if | Just cl <- tyConClass_maybe tc
              -> checkValidClass cl

            | Just syn_rhs <- synTyConRhs_maybe tc
              -> do { checkValidType syn_ctxt syn_rhs
                    ; checkTySynRhs syn_ctxt syn_rhs }

            | Just fam_flav <- famTyConFlav_maybe tc
              -> case fam_flav of
               { ClosedSynFamilyTyCon (Just ax)
                   -> tcAddClosedTypeFamilyDeclCtxt tc $
                      checkValidCoAxiom ax
               ; ClosedSynFamilyTyCon Nothing   -> return ()
               ; AbstractClosedSynFamilyTyCon ->
                 do { hsBoot <- tcIsHsBootOrSig
                    ; checkTc hsBoot $
                      text "You may define an abstract closed type family" $$
                      text "only in a .hs-boot file" }
               ; DataFamilyTyCon {}           -> return ()
               ; OpenSynFamilyTyCon           -> return ()
               ; BuiltInSynFamTyCon _         -> return () }

             | otherwise -> do
               { -- Check the context on the data decl
                 traceTc "cvtc1" (ppr tc)
               ; checkValidTheta (DataTyCtxt name) (tyConStupidTheta tc)

               ; traceTc "cvtc2" (ppr tc)

               ; dflags          <- getDynFlags
               ; existential_ok  <- xoptM LangExt.ExistentialQuantification
               ; gadt_ok         <- xoptM LangExt.GADTs
               ; let ex_ok = existential_ok || gadt_ok
                     -- Data cons can have existential context
               ; mapM_ (checkValidDataCon dflags ex_ok tc) data_cons

                -- Check that fields with the same name share a type
               ; mapM_ check_fields groups }}
  where
    syn_ctxt  = TySynCtxt name
    name      = tyConName tc
    data_cons = tyConDataCons tc

    groups = equivClasses cmp_fld (concatMap get_fields data_cons)
    cmp_fld (f1,_) (f2,_) = flLabel f1 `compare` flLabel f2
    get_fields con = dataConFieldLabels con `zip` repeat con
        -- dataConFieldLabels may return the empty list, which is fine

    -- See Note [GADT record selectors] in TcTyDecls
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
        (_, _, _, res1) = dataConSig con1
        fty1 = dataConFieldType con1 lbl
        lbl = flLabel label

        checkOne (_, con2)    -- Do it bothways to ensure they are structurally identical
            = do { checkFieldCompat lbl con1 con2 res1 res2 fty1 fty2
                 ; checkFieldCompat lbl con2 con1 res2 res1 fty2 fty1 }
            where
                (_, _, _, res2) = dataConSig con2
                fty2 = dataConFieldType con2 lbl
    check_fields [] = panic "checkValidTyCon/check_fields []"

checkFieldCompat :: FieldLabelString -> DataCon -> DataCon
                 -> Type -> Type -> Type -> Type -> TcM ()
checkFieldCompat fld con1 con2 res1 res2 fty1 fty2
  = do  { checkTc (isJust mb_subst1) (resultTypeMisMatch fld con1 con2)
        ; checkTc (isJust mb_subst2) (fieldTypeMisMatch fld con1 con2) }
  where
    mb_subst1 = tcMatchTy res1 res2
    mb_subst2 = tcMatchTyX (expectJust "checkFieldCompat" mb_subst1) fty1 fty2

-------------------------------
-- | Check for ill-scoped telescopes in a tycon.
-- For example:
--
-- > data SameKind :: k -> k -> *   -- this is OK
-- > data Bad a (c :: Proxy b) (d :: Proxy a) (x :: SameKind b d)
--
-- The problem is that @b@ should be bound (implicitly) at the beginning,
-- but its kind mentions @a@, which is not yet in scope. Kind generalization
-- makes a mess of this, and ends up including @a@ twice in the final
-- tyvars. So this function checks for duplicates and, if there are any,
-- produces the appropriate error message.
checkValidTyConTyVars :: TyCon -> TcM ()
checkValidTyConTyVars tc
  = do { -- strip off the duplicates and look for ill-scoped things
         -- but keep the *last* occurrence of each variable, as it's
         -- most likely the one the user wrote
         let stripped_tvs | duplicate_vars
                          = reverse $ nub $ reverse tvs
                          | otherwise
                          = tvs
             vis_tvs      = filterOutInvisibleTyVars tc tvs
             extra | not (vis_tvs `equalLength` stripped_tvs)
                   = text "NB: Implicitly declared kind variables are put first."
                   | otherwise
                   = empty
       ; checkValidTelescope (pprTyVars vis_tvs) stripped_tvs extra
         `and_if_that_doesn't_error`
           -- This triggers on test case dependent/should_fail/InferDependency
           -- It reports errors around Note [Dependent LHsQTyVars] in TcHsType
         when duplicate_vars (
          addErr (vcat [ text "Invalid declaration for" <+>
                         quotes (ppr tc) <> semi <+> text "you must explicitly"
                       , text "declare which variables are dependent on which others."
                       , hang (text "Inferred variable kinds:")
                         2 (vcat (map pp_tv stripped_tvs)) ])) }
  where
    tvs = tyConTyVars tc
    duplicate_vars = sizeVarSet (mkVarSet tvs) < length tvs

    pp_tv tv = ppr tv <+> dcolon <+> ppr (tyVarKind tv)

     -- only run try_second if the first reports no errors
    and_if_that_doesn't_error :: TcM () -> TcM () -> TcM ()
    try_first `and_if_that_doesn't_error` try_second
      = recoverM (return ()) $
        do { checkNoErrs try_first
           ; try_second }

-------------------------------
checkValidDataCon :: DynFlags -> Bool -> TyCon -> DataCon -> TcM ()
checkValidDataCon dflags existential_ok tc con
  = setSrcSpan (srcLocSpan (getSrcLoc con))     $
    addErrCtxt (dataConCtxt con)                $
    do  { -- Check that the return type of the data constructor
          -- matches the type constructor; eg reject this:
          --   data T a where { MkT :: Bogus a }
          -- It's important to do this first:
          --  see Note [Checking GADT return types]
          --  and c.f. Note [Check role annotations in a second pass]
          let tc_tvs      = tyConTyVars tc
              res_ty_tmpl = mkFamilyTyConApp tc (mkTyVarTys tc_tvs)
              orig_res_ty = dataConOrigResTy con
        ; traceTc "checkValidDataCon" (vcat
              [ ppr con, ppr tc, ppr tc_tvs
              , ppr res_ty_tmpl <+> dcolon <+> ppr (typeKind res_ty_tmpl)
              , ppr orig_res_ty <+> dcolon <+> ppr (typeKind orig_res_ty)])


        ; checkTc (isJust (tcMatchTy res_ty_tmpl
                                     orig_res_ty))
                  (badDataConTyCon con res_ty_tmpl orig_res_ty)
            -- Note that checkTc aborts if it finds an error. This is
            -- critical to avoid panicking when we call dataConUserType
            -- on an un-rejiggable datacon!

        ; traceTc "checkValidDataCon 2" (ppr (dataConUserType con))

          -- Check that the result type is a *monotype*
          --  e.g. reject this:   MkT :: T (forall a. a->a)
          -- Reason: it's really the argument of an equality constraint
        ; checkValidMonoType orig_res_ty

          -- Check all argument types for validity
        ; checkValidType ctxt (dataConUserType con)
        ; mapM_ (checkForLevPoly empty)
                (dataConOrigArgTys con)

          -- Extra checks for newtype data constructors
        ; when (isNewTyCon tc) (checkNewDataCon con)

          -- Check that existentials are allowed if they are used
        ; checkTc (existential_ok || isVanillaDataCon con)
                  (badExistential con)

          -- Check that UNPACK pragmas and bangs work out
          -- E.g.  reject   data T = MkT {-# UNPACK #-} Int     -- No "!"
          --                data T = MkT {-# UNPACK #-} !a      -- Can't unpack
        ; zipWith3M_ check_bang (dataConSrcBangs con) (dataConImplBangs con) [1..]

        ; traceTc "Done validity of data con" (ppr con <+> ppr (dataConRepType con))
    }
  where
    ctxt = ConArgCtxt (dataConName con)

    check_bang :: HsSrcBang -> HsImplBang -> Int -> TcM ()
    check_bang (HsSrcBang _ _ SrcLazy) _ n
      | not (xopt LangExt.StrictData dflags)
      = addErrTc
          (bad_bang n (text "Lazy annotation (~) without StrictData"))
    check_bang (HsSrcBang _ want_unpack strict_mark) rep_bang n
      | isSrcUnpacked want_unpack, not is_strict
      = addWarnTc NoReason (bad_bang n (text "UNPACK pragma lacks '!'"))
      | isSrcUnpacked want_unpack
      , case rep_bang of { HsUnpack {} -> False; _ -> True }
      , not (gopt Opt_OmitInterfacePragmas dflags)
           -- If not optimising, se don't unpack, so don't complain!
           -- See MkId.dataConArgRep, the (HsBang True) case
      = addWarnTc NoReason (bad_bang n (text "Ignoring unusable UNPACK pragma"))
      where
        is_strict = case strict_mark of
                      NoSrcStrict -> xopt LangExt.StrictData dflags
                      bang        -> isSrcStrict bang

    check_bang _ _ _
      = return ()

    bad_bang n herald
      = hang herald 2 (text "on the" <+> speakNth n
                       <+> text "argument of" <+> quotes (ppr con))
-------------------------------
checkNewDataCon :: DataCon -> TcM ()
-- Further checks for the data constructor of a newtype
checkNewDataCon con
  = do  { checkTc (isSingleton arg_tys) (newtypeFieldErr con (length arg_tys))
              -- One argument

        ; checkTc (not (isUnliftedType arg_ty1)) $
          text "A newtype cannot have an unlifted argument type"

        ; check_con (null eq_spec) $
          text "A newtype constructor must have a return type of form T a1 ... an"
                -- Return type is (T a b c)

        ; check_con (null theta) $
          text "A newtype constructor cannot have a context in its type"

        ; check_con (null ex_tvs) $
          text "A newtype constructor cannot have existential type variables"
                -- No existentials

        ; checkTc (all ok_bang (dataConSrcBangs con))
                  (newtypeStrictError con)
                -- No strictness annotations
    }
  where
    (_univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _res_ty)
      = dataConFullSig con
    check_con what msg
       = checkTc what (msg $$ ppr con <+> dcolon <+> ppr (dataConUserType con))

    (arg_ty1 : _) = arg_tys

    ok_bang (HsSrcBang _ _ SrcStrict) = False
    ok_bang (HsSrcBang _ _ SrcLazy)   = False
    ok_bang _                         = True

-------------------------------
checkValidClass :: Class -> TcM ()
checkValidClass cls
  = do  { constrained_class_methods <- xoptM LangExt.ConstrainedClassMethods
        ; multi_param_type_classes  <- xoptM LangExt.MultiParamTypeClasses
        ; nullary_type_classes      <- xoptM LangExt.NullaryTypeClasses
        ; fundep_classes            <- xoptM LangExt.FunctionalDependencies
        ; undecidable_super_classes <- xoptM LangExt.UndecidableSuperClasses

        -- Check that the class is unary, unless multiparameter type classes
        -- are enabled; also recognize deprecated nullary type classes
        -- extension (subsumed by multiparameter type classes, Trac #8993)
        ; checkTc (multi_param_type_classes || cls_arity == 1 ||
                    (nullary_type_classes && cls_arity == 0))
                  (classArityErr cls_arity cls)
        ; checkTc (fundep_classes || null fundeps) (classFunDepsErr cls)

        -- Check the super-classes
        ; checkValidTheta (ClassSCCtxt (className cls)) theta

          -- Now check for cyclic superclasses
          -- If there are superclass cycles, checkClassCycleErrs bails.
        ; unless undecidable_super_classes $
          case checkClassCycles cls of
             Just err -> setSrcSpan (getSrcSpan cls) $
                         addErrTc err
             Nothing  -> return ()

        -- Check the class operations.
        -- But only if there have been no earlier errors
        -- See Note [Abort when superclass cycle is detected]
        ; whenNoErrs $
          mapM_ (check_op constrained_class_methods) op_stuff

        -- Check the associated type defaults are well-formed and instantiated
        ; mapM_ check_at at_stuff  }
  where
    (tyvars, fundeps, theta, _, at_stuff, op_stuff) = classExtraBigSig cls
    cls_arity = length $ filterOutInvisibleTyVars (classTyCon cls) tyvars
       -- Ignore invisible variables
    cls_tv_set = mkVarSet tyvars
    mini_env   = zipVarEnv tyvars (mkTyVarTys tyvars)
    mb_cls     = Just (cls, tyvars, mini_env)

    check_op constrained_class_methods (sel_id, dm)
      = setSrcSpan (getSrcSpan sel_id) $
        addErrCtxt (classOpCtxt sel_id op_ty) $ do
        { traceTc "class op type" (ppr op_ty)
        ; checkValidType ctxt op_ty
                -- This implements the ambiguity check, among other things
                -- Example: tc223
                --   class Error e => Game b mv e | b -> mv e where
                --      newBoard :: MonadState b m => m ()
                -- Here, MonadState has a fundep m->b, so newBoard is fine

           -- a method cannot be levity polymorphic, as we have to store the
           -- method in a dictionary
           -- example of what this prevents:
           --   class BoundedX (a :: TYPE r) where minBound :: a
           -- See Note [Levity polymorphism checking] in DsMonad
        ; checkForLevPoly empty tau1

        ; unless constrained_class_methods $
          mapM_ check_constraint (tail (cls_pred:op_theta))

        ; check_dm ctxt sel_id cls_pred tau2 dm
        }
        where
          ctxt    = FunSigCtxt op_name True -- Report redundant class constraints
          op_name = idName sel_id
          op_ty   = idType sel_id
          (_,cls_pred,tau1) = tcSplitMethodTy op_ty
          -- See Note [Splitting nested sigma types]
          (_,op_theta,tau2) = tcSplitNestedSigmaTys tau1

          check_constraint :: TcPredType -> TcM ()
          check_constraint pred -- See Note [Class method constraints]
            = when (not (isEmptyVarSet pred_tvs) &&
                    pred_tvs `subVarSet` cls_tv_set)
                   (addErrTc (badMethPred sel_id pred))
            where
              pred_tvs = tyCoVarsOfType pred

    check_at (ATI fam_tc m_dflt_rhs)
      = do { checkTc (cls_arity == 0 || any (`elemVarSet` cls_tv_set) fam_tvs)
                     (noClassTyVarErr cls fam_tc)
                        -- Check that the associated type mentions at least
                        -- one of the class type variables
                        -- The check is disabled for nullary type classes,
                        -- since there is no possible ambiguity (Trac #10020)

             -- Check that any default declarations for associated types are valid
           ; whenIsJust m_dflt_rhs $ \ (rhs, loc) ->
             checkValidTyFamEqn mb_cls fam_tc
                                fam_tvs [] (mkTyVarTys fam_tvs) rhs loc }
        where
          fam_tvs = tyConTyVars fam_tc

    check_dm :: UserTypeCtxt -> Id -> PredType -> Type -> DefMethInfo -> TcM ()
    -- Check validity of the /top-level/ generic-default type
    -- E.g for   class C a where
    --             default op :: forall b. (a~b) => blah
    -- we do not want to do an ambiguity check on a type with
    -- a free TyVar 'a' (Trac #11608).  See TcType
    -- Note [TyVars and TcTyVars during type checking] in TcType
    -- Hence the mkDefaultMethodType to close the type.
    check_dm ctxt sel_id vanilla_cls_pred vanilla_tau
             (Just (dm_name, dm_spec@(GenericDM dm_ty)))
      = setSrcSpan (getSrcSpan dm_name) $ do
            -- We have carefully set the SrcSpan on the generic
            -- default-method Name to be that of the generic
            -- default type signature

          -- First, we check that that the method's default type signature
          -- aligns with the non-default type signature.
          -- See Note [Default method type signatures must align]
          let cls_pred = mkClassPred cls $ mkTyVarTys $ classTyVars cls
              -- Note that the second field of this tuple contains the context
              -- of the default type signature, making it apparent that we
              -- ignore method contexts completely when validity-checking
              -- default type signatures. See the end of
              -- Note [Default method type signatures must align]
              -- to learn why this is OK.
              --
              -- See also Note [Splitting nested sigma types]
              -- for an explanation of why we don't use tcSplitSigmaTy here.
              (_, _, dm_tau) = tcSplitNestedSigmaTys dm_ty

              -- Given this class definition:
              --
              --  class C a b where
              --    op         :: forall p q. (Ord a, D p q)
              --               => a -> b -> p -> (a, b)
              --    default op :: forall r s. E r
              --               => a -> b -> s -> (a, b)
              --
              -- We want to match up two types of the form:
              --
              --   Vanilla type sig: C aa bb => aa -> bb -> p -> (aa, bb)
              --   Default type sig: C a  b  => a  -> b  -> s -> (a,  b)
              --
              -- Notice that the two type signatures can be quantified over
              -- different class type variables! Therefore, it's important that
              -- we include the class predicate parts to match up a with aa and
              -- b with bb.
              vanilla_phi_ty = mkPhiTy [vanilla_cls_pred] vanilla_tau
              dm_phi_ty      = mkPhiTy [cls_pred] dm_tau

          traceTc "check_dm" $ vcat
              [ text "vanilla_phi_ty" <+> ppr vanilla_phi_ty
              , text "dm_phi_ty"      <+> ppr dm_phi_ty ]

          -- Actually checking that the types align is done with a call to
          -- tcMatchTys. We need to get a match in both directions to rule
          -- out degenerate cases like these:
          --
          --  class Foo a where
          --    foo1         :: a -> b
          --    default foo1 :: a -> Int
          --
          --    foo2         :: a -> Int
          --    default foo2 :: a -> b
          unless (isJust $ tcMatchTys [dm_phi_ty, vanilla_phi_ty]
                                      [vanilla_phi_ty, dm_phi_ty]) $ addErrTc $
               hang (text "The default type signature for"
                     <+> ppr sel_id <> colon)
                 2 (ppr dm_ty)
            $$ (text "does not match its corresponding"
                <+> text "non-default type signature")

          -- Now do an ambiguity check on the default type signature.
          checkValidType ctxt (mkDefaultMethodType cls sel_id dm_spec)
    check_dm _ _ _ _ _ = return ()

checkFamFlag :: Name -> TcM ()
-- Check that we don't use families without -XTypeFamilies
-- The parser won't even parse them, but I suppose a GHC API
-- client might have a go!
checkFamFlag tc_name
  = do { idx_tys <- xoptM LangExt.TypeFamilies
       ; checkTc idx_tys err_msg }
  where
    err_msg = hang (text "Illegal family declaration for" <+> quotes (ppr tc_name))
                 2 (text "Use TypeFamilies to allow indexed type families")

{- Note [Class method constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Haskell 2010 is supposed to reject
  class C a where
    op :: Eq a => a -> a
where the method type costrains only the class variable(s).  (The extension
-XConstrainedClassMethods switches off this check.)  But regardless
we should not reject
  class C a where
    op :: (?x::Int) => a -> a
as pointed out in Trac #11793. So the test here rejects the program if
  * -XConstrainedClassMethods is off
  * the tyvars of the constraint are non-empty
  * all the tyvars are class tyvars, none are locally quantified

Note [Abort when superclass cycle is detected]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must avoid doing the ambiguity check for the methods (in
checkValidClass.check_op) when there are already errors accumulated.
This is because one of the errors may be a superclass cycle, and
superclass cycles cause canonicalization to loop. Here is a
representative example:

  class D a => C a where
    meth :: D a => ()
  class C a => D a

This fixes Trac #9415, #9739

Note [Default method type signatures must align]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC enforces the invariant that a class method's default type signature
must "align" with that of the method's non-default type signature, as per
GHC Trac #12918. For instance, if you have:

  class Foo a where
    bar :: forall b. Context => a -> b

Then a default type signature for bar must be alpha equivalent to
(forall b. a -> b). That is, the types must be the same modulo differences in
contexts. So the following would be acceptable default type signatures:

    default bar :: forall b. Context1 => a -> b
    default bar :: forall x. Context2 => a -> x

But the following are NOT acceptable default type signatures:

    default bar :: forall b. b -> a
    default bar :: forall x. x
    default bar :: a -> Int

Note that a is bound by the class declaration for Foo itself, so it is
not allowed to differ in the default type signature.

The default type signature (default bar :: a -> Int) deserves special mention,
since (a -> Int) is a straightforward instantiation of (forall b. a -> b). To
write this, you need to declare the default type signature like so:

    default bar :: forall b. (b ~ Int). a -> b

As noted in #12918, there are several reasons to do this:

1. It would make no sense to have a type that was flat-out incompatible with
   the non-default type signature. For instance, if you had:

     class Foo a where
       bar :: a -> Int
       default bar :: a -> Bool

   Then that would always fail in an instance declaration. So this check
   nips such cases in the bud before they have the chance to produce
   confusing error messages.

2. Internally, GHC uses TypeApplications to instantiate the default method in
   an instance. See Note [Default methods in instances] in TcInstDcls.
   Thus, GHC needs to know exactly what the universally quantified type
   variables are, and when instantiated that way, the default method's type
   must match the expected type.

3. Aesthetically, by only allowing the default type signature to differ in its
   context, we are making it more explicit the ways in which the default type
   signature is less polymorphic than the non-default type signature.

You might be wondering: why are the contexts allowed to be different, but not
the rest of the type signature? That's because default implementations often
rely on assumptions that the more general, non-default type signatures do not.
For instance, in the Enum class declaration:

    class Enum a where
      enum :: [a]
      default enum :: (Generic a, GEnum (Rep a)) => [a]
      enum = map to genum

    class GEnum f where
      genum :: [f a]

The default implementation for enum only works for types that are instances of
Generic, and for which their generic Rep type is an instance of GEnum. But
clearly enum doesn't _have_ to use this implementation, so naturally, the
context for enum is allowed to be different to accomodate this. As a result,
when we validity-check default type signatures, we ignore contexts completely.

Note that when checking whether two type signatures match, we must take care to
split as many foralls as it takes to retrieve the tau types we which to check.
See Note [Splitting nested sigma types].

Note [Splitting nested sigma types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this type synonym and class definition:

  type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

  class Each s t a b where
    each         ::                                      Traversal s t a b
    default each :: (Traversable g, s ~ g a, t ~ g b) => Traversal s t a b

It might seem obvious that the tau types in both type signatures for `each`
are the same, but actually getting GHC to conclude this is surprisingly tricky.
That is because in general, the form of a class method's non-default type
signature is:

  forall a. C a => forall d. D d => E a b

And the general form of a default type signature is:

  forall f. F f => E a f -- The variable `a` comes from the class

So it you want to get the tau types in each type signature, you might find it
reasonable to call tcSplitSigmaTy twice on the non-default type signature, and
call it once on the default type signature. For most classes and methods, this
will work, but Each is a bit of an exceptional case. The way `each` is written,
it doesn't quantify any additional type variables besides those of the Each
class itself, so the non-default type signature for `each` is actually this:

  forall s t a b. Each s t a b => Traversal s t a b

Notice that there _appears_ to only be one forall. But there's actually another
forall lurking in the Traversal type synonym, so if you call tcSplitSigmaTy
twice, you'll also go under the forall in Traversal! That is, you'll end up
with:

  (a -> f b) -> s -> f t

A problem arises because you only call tcSplitSigmaTy once on the default type
signature for `each`, which gives you

  Traversal s t a b

Or, equivalently:

  forall f. Applicative f => (a -> f b) -> s -> f t

This is _not_ the same thing as (a -> f b) -> s -> f t! So now tcMatchTy will
say that the tau types for `each` are not equal.

A solution to this problem is to use tcSplitNestedSigmaTys instead of
tcSplitSigmaTy. tcSplitNestedSigmaTys will always split any foralls that it
sees until it can't go any further, so if you called it on the default type
signature for `each`, it would return (a -> f b) -> s -> f t like we desired.

************************************************************************
*                                                                      *
                Checking role validity
*                                                                      *
************************************************************************
-}

checkValidRoleAnnots :: RoleAnnotEnv -> TyCon -> TcM ()
checkValidRoleAnnots role_annots tc
  | isTypeSynonymTyCon tc = check_no_roles
  | isFamilyTyCon tc      = check_no_roles
  | isAlgTyCon tc         = check_roles
  | otherwise             = return ()
  where
    -- Role annotations are given only on *explicit* variables,
    -- but a tycon stores roles for all variables.
    -- So, we drop the implicit roles (which are all Nominal, anyway).
    name                   = tyConName tc
    tyvars                 = tyConTyVars tc
    roles                  = tyConRoles tc
    (vis_roles, vis_vars)  = unzip $ snd $
                             partitionInvisibles tc (mkTyVarTy . snd) $
                             zip roles tyvars
    role_annot_decl_maybe  = lookupRoleAnnot role_annots name

    check_roles
      = whenIsJust role_annot_decl_maybe $
          \decl@(L loc (RoleAnnotDecl _ the_role_annots)) ->
          addRoleAnnotCtxt name $
          setSrcSpan loc $ do
          { role_annots_ok <- xoptM LangExt.RoleAnnotations
          ; checkTc role_annots_ok $ needXRoleAnnotations tc
          ; checkTc (vis_vars `equalLength` the_role_annots)
                    (wrongNumberOfRoles vis_vars decl)
          ; _ <- zipWith3M checkRoleAnnot vis_vars the_role_annots vis_roles
          -- Representational or phantom roles for class parameters
          -- quickly lead to incoherence. So, we require
          -- IncoherentInstances to have them. See #8773.
          ; incoherent_roles_ok <- xoptM LangExt.IncoherentInstances
          ; checkTc (  incoherent_roles_ok
                    || (not $ isClassTyCon tc)
                    || (all (== Nominal) vis_roles))
                    incoherentRoles

          ; lint <- goptM Opt_DoCoreLinting
          ; when lint $ checkValidRoles tc }

    check_no_roles
      = whenIsJust role_annot_decl_maybe illegalRoleAnnotDecl

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
  | Just rhs <- synTyConRhs_maybe tc
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
        (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _res_ty)
          = dataConFullSig datacon
        univ_roles = zipVarEnv univ_tvs (tyConRoles tc)
              -- zipVarEnv uses zipEqual, but we don't want that for ex_tvs
        ex_roles   = mkVarEnv (map (, Nominal) ex_tvs)
        role_env   = univ_roles `plusVarEnv` ex_roles

    check_ty_roles env role (TyVarTy tv)
      = case lookupVarEnv env tv of
          Just role' -> unless (role' `ltRole` role || role' == role) $
                        report_error $ text "type variable" <+> quotes (ppr tv) <+>
                                       text "cannot have role" <+> ppr role <+>
                                       text "because it was assigned role" <+> ppr role'
          Nothing    -> report_error $ text "type variable" <+> quotes (ppr tv) <+>
                                       text "missing in environment"

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

    check_ty_roles env role (ForAllTy (TvBndr tv _) ty)
      =  check_ty_roles env Nominal (tyVarKind tv)
      >> check_ty_roles (extendVarEnv env tv Nominal) role ty

    check_ty_roles _   _    (LitTy {}) = return ()

    check_ty_roles env role (CastTy t _)
      = check_ty_roles env role t

    check_ty_roles _   role (CoercionTy co)
      = unless (role == Phantom) $
        report_error $ text "coercion" <+> ppr co <+> text "has bad role" <+> ppr role

    maybe_check_ty_roles env role ty
      = when (role == Nominal || role == Representational) $
        check_ty_roles env role ty

    report_error doc
      = addErrTc $ vcat [text "Internal error in role inference:",
                         doc,
                         text "Please report this as a GHC bug: http://www.haskell.org/ghc/reportabug"]

{-
************************************************************************
*                                                                      *
                Error messages
*                                                                      *
************************************************************************
-}

tcAddTyFamInstCtxt :: TyFamInstDecl Name -> TcM a -> TcM a
tcAddTyFamInstCtxt decl
  = tcAddFamInstCtxt (text "type instance") (tyFamInstDeclName decl)

tcMkDataFamInstCtxt :: DataFamInstDecl Name -> SDoc
tcMkDataFamInstCtxt decl
  = tcMkFamInstCtxt (pprDataFamInstFlavour decl <+> text "instance")
                    (unLoc (dfid_tycon decl))

tcAddDataFamInstCtxt :: DataFamInstDecl Name -> TcM a -> TcM a
tcAddDataFamInstCtxt decl
  = addErrCtxt (tcMkDataFamInstCtxt decl)

tcMkFamInstCtxt :: SDoc -> Name -> SDoc
tcMkFamInstCtxt flavour tycon
  = hsep [ text "In the" <+> flavour <+> text "declaration for"
         , quotes (ppr tycon) ]

tcAddFamInstCtxt :: SDoc -> Name -> TcM a -> TcM a
tcAddFamInstCtxt flavour tycon thing_inside
  = addErrCtxt (tcMkFamInstCtxt flavour tycon) thing_inside

tcAddClosedTypeFamilyDeclCtxt :: TyCon -> TcM a -> TcM a
tcAddClosedTypeFamilyDeclCtxt tc
  = addErrCtxt ctxt
  where
    ctxt = text "In the equations for closed type family" <+>
           quotes (ppr tc)

resultTypeMisMatch :: FieldLabelString -> DataCon -> DataCon -> SDoc
resultTypeMisMatch field_name con1 con2
  = vcat [sep [text "Constructors" <+> ppr con1 <+> text "and" <+> ppr con2,
                text "have a common field" <+> quotes (ppr field_name) <> comma],
          nest 2 $ text "but have different result types"]

fieldTypeMisMatch :: FieldLabelString -> DataCon -> DataCon -> SDoc
fieldTypeMisMatch field_name con1 con2
  = sep [text "Constructors" <+> ppr con1 <+> text "and" <+> ppr con2,
         text "give different types for field", quotes (ppr field_name)]

dataConCtxtName :: [LEmbellished Name] -> SDoc
dataConCtxtName [con]
   = text "In the definition of data constructor" <+> quotes (ppr con)
dataConCtxtName con
   = text "In the definition of data constructors" <+> interpp'SP con

dataConCtxt :: Outputable a => a -> SDoc
dataConCtxt con = text "In the definition of data constructor" <+> quotes (ppr con)

classOpCtxt :: Var -> Type -> SDoc
classOpCtxt sel_id tau = sep [text "When checking the class method:",
                              nest 2 (pprPrefixOcc sel_id <+> dcolon <+> ppr tau)]

classArityErr :: Int -> Class -> SDoc
classArityErr n cls
    | n == 0 = mkErr "No" "no-parameter"
    | otherwise = mkErr "Too many" "multi-parameter"
  where
    mkErr howMany allowWhat =
        vcat [text (howMany ++ " parameters for class") <+> quotes (ppr cls),
              parens (text ("Use MultiParamTypeClasses to allow "
                                    ++ allowWhat ++ " classes"))]

classFunDepsErr :: Class -> SDoc
classFunDepsErr cls
  = vcat [text "Fundeps in class" <+> quotes (ppr cls),
          parens (text "Use FunctionalDependencies to allow fundeps")]

badMethPred :: Id -> TcPredType -> SDoc
badMethPred sel_id pred
  = vcat [ hang (text "Constraint" <+> quotes (ppr pred)
                 <+> text "in the type of" <+> quotes (ppr sel_id))
              2 (text "constrains only the class type variables")
         , text "Use ConstrainedClassMethods to allow it" ]

noClassTyVarErr :: Class -> TyCon -> SDoc
noClassTyVarErr clas fam_tc
  = sep [ text "The associated type" <+> quotes (ppr fam_tc)
        , text "mentions none of the type or kind variables of the class" <+>
                quotes (ppr clas <+> hsep (map ppr (classTyVars clas)))]

badDataConTyCon :: DataCon -> Type -> Type -> SDoc
badDataConTyCon data_con res_ty_tmpl actual_res_ty
  = hang (text "Data constructor" <+> quotes (ppr data_con) <+>
                text "returns type" <+> quotes (ppr actual_res_ty))
       2 (text "instead of an instance of its parent type" <+> quotes (ppr res_ty_tmpl))

badGadtDecl :: Name -> SDoc
badGadtDecl tc_name
  = vcat [ text "Illegal generalised algebraic data declaration for" <+> quotes (ppr tc_name)
         , nest 2 (parens $ text "Use GADTs to allow GADTs") ]

badExistential :: DataCon -> SDoc
badExistential con
  = hang (text "Data constructor" <+> quotes (ppr con) <+>
                text "has existential type variables, a context, or a specialised result type")
       2 (vcat [ ppr con <+> dcolon <+> ppr (dataConUserType con)
               , parens $ text "Use ExistentialQuantification or GADTs to allow this" ])

badStupidTheta :: Name -> SDoc
badStupidTheta tc_name
  = text "A data type declared in GADT style cannot have a context:" <+> quotes (ppr tc_name)

newtypeConError :: Name -> Int -> SDoc
newtypeConError tycon n
  = sep [text "A newtype must have exactly one constructor,",
         nest 2 $ text "but" <+> quotes (ppr tycon) <+> text "has" <+> speakN n ]

newtypeStrictError :: DataCon -> SDoc
newtypeStrictError con
  = sep [text "A newtype constructor cannot have a strictness annotation,",
         nest 2 $ text "but" <+> quotes (ppr con) <+> text "does"]

newtypeFieldErr :: DataCon -> Int -> SDoc
newtypeFieldErr con_name n_flds
  = sep [text "The constructor of a newtype must have exactly one field",
         nest 2 $ text "but" <+> quotes (ppr con_name) <+> text "has" <+> speakN n_flds]

badSigTyDecl :: Name -> SDoc
badSigTyDecl tc_name
  = vcat [ text "Illegal kind signature" <+>
           quotes (ppr tc_name)
         , nest 2 (parens $ text "Use KindSignatures to allow kind signatures") ]

emptyConDeclsErr :: Name -> SDoc
emptyConDeclsErr tycon
  = sep [quotes (ppr tycon) <+> text "has no constructors",
         nest 2 $ text "(EmptyDataDecls permits this)"]

wrongKindOfFamily :: TyCon -> SDoc
wrongKindOfFamily family
  = text "Wrong category of family instance; declaration was for a"
    <+> kindOfFamily
  where
    kindOfFamily | isTypeFamilyTyCon family = text "type family"
                 | isDataFamilyTyCon family = text "data family"
                 | otherwise = pprPanic "wrongKindOfFamily" (ppr family)

wrongNumberOfParmsErr :: Arity -> SDoc
wrongNumberOfParmsErr max_args
  = text "Number of parameters must match family declaration; expected"
    <+> ppr max_args

defaultAssocKindErr :: TyCon -> SDoc
defaultAssocKindErr fam_tc
  = text "Kind mis-match on LHS of default declaration for"
    <+> quotes (ppr fam_tc)

wrongTyFamName :: Name -> Name -> SDoc
wrongTyFamName fam_tc_name eqn_tc_name
  = hang (text "Mismatched type name in type family instance.")
       2 (vcat [ text "Expected:" <+> ppr fam_tc_name
               , text "  Actual:" <+> ppr eqn_tc_name ])

badRoleAnnot :: Name -> Role -> Role -> SDoc
badRoleAnnot var annot inferred
  = hang (text "Role mismatch on variable" <+> ppr var <> colon)
       2 (sep [ text "Annotation says", ppr annot
              , text "but role", ppr inferred
              , text "is required" ])

wrongNumberOfRoles :: [a] -> LRoleAnnotDecl Name -> SDoc
wrongNumberOfRoles tyvars d@(L _ (RoleAnnotDecl _ annots))
  = hang (text "Wrong number of roles listed in role annotation;" $$
          text "Expected" <+> (ppr $ length tyvars) <> comma <+>
          text "got" <+> (ppr $ length annots) <> colon)
       2 (ppr d)

illegalRoleAnnotDecl :: LRoleAnnotDecl Name -> TcM ()
illegalRoleAnnotDecl (L loc (RoleAnnotDecl tycon _))
  = setErrCtxt [] $
    setSrcSpan loc $
    addErrTc (text "Illegal role annotation for" <+> ppr tycon <> char ';' $$
              text "they are allowed only for datatypes and classes.")

needXRoleAnnotations :: TyCon -> SDoc
needXRoleAnnotations tc
  = text "Illegal role annotation for" <+> ppr tc <> char ';' $$
    text "did you intend to use RoleAnnotations?"

incoherentRoles :: SDoc
incoherentRoles = (text "Roles other than" <+> quotes (text "nominal") <+>
                   text "for class parameters can lead to incoherence.") $$
                  (text "Use IncoherentInstances to allow this; bad role found")

addTyConCtxt :: TyCon -> TcM a -> TcM a
addTyConCtxt tc
  = addErrCtxt ctxt
  where
    name = getName tc
    flav = text (tyConFlavour tc)
    ctxt = hsep [ text "In the", flav
                , text "declaration for", quotes (ppr name) ]

addRoleAnnotCtxt :: Name -> TcM a -> TcM a
addRoleAnnotCtxt name
  = addErrCtxt $
    text "while checking a role annotation for" <+> quotes (ppr name)
