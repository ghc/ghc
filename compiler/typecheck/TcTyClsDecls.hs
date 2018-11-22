{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998


TcTyClsDecls: Typecheck type and class declarations
-}

{-# LANGUAGE CPP, TupleSections, MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module TcTyClsDecls (
        tcTyAndClassDecls,

        -- Functions used by TcInstDcls to check
        -- data/type family instance declarations
        kcDataDefn, tcConDecls, dataDeclChecks, checkValidTyCon,
        tcFamTyPats, tcTyFamInstEqn,
        tcAddTyFamInstCtxt, tcMkDataFamInstCtxt, tcAddDataFamInstCtxt,
        wrongKindOfFamily, dataConCtxt
    ) where

#include "HsVersions.h"

import GhcPrelude

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
import TcEvidence  ( tcCoercionKind, isEmptyTcEvBinds )
import TcUnify     ( checkConstraints )
import TcHsType
import TcMType
import TysWiredIn ( unitTy )
import TcType
import RnEnv( lookupConstructorFields )
import FamInst
import FamInstEnv
import Coercion
import Type
import TyCoRep   -- for checkValidRoles
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
import Pair
import SrcLoc
import ListSetOps
import DynFlags
import Unique
import ConLike( ConLike(..) )
import BasicTypes
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.List
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.Set as Set


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

tcTyAndClassDecls :: [TyClGroup GhcRn]      -- Mutually-recursive groups in
                                            -- dependency order
                  -> TcM ( TcGblEnv         -- Input env extended by types and
                                            -- classes
                                            -- and their implicit Ids,DataCons
                         , [InstInfo GhcRn] -- Source-code instance decls info
                         , [DerivInfo]      -- data family deriving info
                         )
-- Fails if there are any errors
tcTyAndClassDecls tyclds_s
  -- The code recovers internally, but if anything gave rise to
  -- an error we'd better stop now, to avoid a cascade
  -- Type check each group in dependency order folding the global env
  = checkNoErrs $ fold_env [] [] tyclds_s
  where
    fold_env :: [InstInfo GhcRn]
             -> [DerivInfo]
             -> [TyClGroup GhcRn]
             -> TcM (TcGblEnv, [InstInfo GhcRn], [DerivInfo])
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

tcTyClGroup :: TyClGroup GhcRn
            -> TcM (TcGblEnv, [InstInfo GhcRn], [DerivInfo])
-- Typecheck one strongly-connected component of type, class, and instance decls
-- See Note [TyClGroups and dependency analysis] in HsDecls
tcTyClGroup (TyClGroup { group_tyclds = tyclds
                       , group_roles  = roles
                       , group_instds = instds })
  = do { let role_annots = mkRoleAnnotEnv roles

           -- Step 1: Typecheck the type/class declarations
       ; traceTc "---- tcTyClGroup ---- {" empty
       ; traceTc "Decls for" (ppr (map (tcdName . unLoc) tyclds))
       ; tyclss <- tcTyClDecls tyclds role_annots

           -- Step 1.5: Make sure we don't have any type synonym cycles
       ; traceTc "Starting synonym cycle check" (ppr tyclss)
       ; this_uid <- fmap thisPackage getDynFlags
       ; checkSynCycles this_uid tyclss tyclds
       ; traceTc "Done synonym cycle check" (ppr tyclss)

           -- Step 2: Perform the validity check on those types/classes
           -- We can do this now because we are done with the recursive knot
           -- Do it before Step 3 (adding implicit things) because the latter
           -- expects well-formed TyCons
       ; traceTc "Starting validity check" (ppr tyclss)
       ; tyclss <- concatMapM checkValidTyCl tyclss
       ; traceTc "Done validity check" (ppr tyclss)
       ; mapM_ (recoverM (return ()) . checkValidRoleAnnots role_annots) tyclss
           -- See Note [Check role annotations in a second pass]

       ; traceTc "---- end tcTyClGroup ---- }" empty

           -- Step 3: Add the implicit things;
           -- we want them in the environment because
           -- they may be mentioned in interface files
       ; gbl_env <- addTyConsToGblEnv tyclss

           -- Step 4: check instance declarations
       ; setGblEnv gbl_env $
         tcInstDecls1 instds }

tcTyClGroup (XTyClGroup _) = panic "tcTyClGroup"

tcTyClDecls :: [LTyClDecl GhcRn] -> RoleAnnotEnv -> TcM [TyCon]
tcTyClDecls tyclds role_annots
  = tcExtendKindEnv promotion_err_env $   --- See Note [Type environment evolution]
    do {    -- Step 1: kind-check this group and returns the final
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
           { tcg_env <- getGblEnv
           ; let roles = inferRoles (tcg_src tcg_env) role_annots rec_tyclss

                 -- Populate environment with knot-tied ATyCon for TyCons
                 -- NB: if the decls mention any ill-staged data cons
                 -- (see Note [Recursion and promoting data constructors])
                 -- we will have failed already in kcTyClGroup, so no worries here
           ; tcExtendRecEnv (zipRecTyClss tc_tycons rec_tyclss) $

                 -- Also extend the local type envt with bindings giving
                 -- a TcTyCon for each each knot-tied TyCon or Class
                 -- See Note [Type checking recursive type and class declarations]
                 -- and Note [Type environment evolution]
             tcExtendKindEnvWithTyCons tc_tycons $

                 -- Kind and type check declarations for this group
               mapM (tcTyClDecl roles) tyclds
           } }
  where
    promotion_err_env = mkPromotionErrorEnv tyclds
    ppr_tc_tycon tc = parens (sep [ ppr (tyConName tc) <> comma
                                  , ppr (tyConBinders tc) <> comma
                                  , ppr (tyConResKind tc)
                                  , ppr (isTcTyCon tc) ])

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

Note [Skip decls with CUSKs in kcLTyClDecl]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

    data T (a :: *) = MkT (S a)   -- Has CUSK
    data S a = MkS (T Int) (S a)  -- No CUSK

Via getInitialKinds we get
  T :: * -> *
  S :: kappa -> *

Then we call kcTyClDecl on each decl in the group, to constrain the
kind unification variables.  BUT we /skip/ the RHS of any decl with
a CUSK.  Here we skip the RHS of T, so we eventually get
  S :: forall k. k -> *

This gets us more polymorphism than we would otherwise get, similar
(but implemented strangely differently from) the treatment of type
signatures in value declarations.

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

Note [How TcTyCons work]
~~~~~~~~~~~~~~~~~~~~~~~~
TcTyCons are used for two distinct purposes

1.  When recovering from a type error in a type declaration,
    we want to put the erroneous TyCon in the environment in a
    way that won't lead to more errors.  We use a TcTyCon for this;
    see makeRecoveryTyCon.

2.  When checking a type/class declaration (in module TcTyClsDecls), we come
    upon knowledge of the eventual tycon in bits and pieces.

      S1) First, we use getInitialKinds to look over the user-provided
          kind signature of a tycon (including, for example, the number
          of parameters written to the tycon) to get an initial shape of
          the tycon's kind.  We record that shape in a TcTyCon.

          For CUSK tycons, the TcTyCon has the final, generalised kind.
          For non-CUSK tycons, the TcTyCon has as its tyConBinders only
          the explicit arguments given -- no kind variables, etc.

      S2) Then, using these initial kinds, we kind-check the body of the
          tycon (class methods, data constructors, etc.), filling in the
          metavariables in the tycon's initial kind.

      S3) We then generalize to get the (non-CUSK) tycon's final, fixed
          kind. Finally, once this has happened for all tycons in a
          mutually recursive group, we can desugar the lot.

    For convenience, we store partially-known tycons in TcTyCons, which
    might store meta-variables. These TcTyCons are stored in the local
    environment in TcTyClsDecls, until the real full TyCons can be created
    during desugaring. A desugared program should never have a TcTyCon.

3.  In a TcTyCon, everything is zonked after the kind-checking pass (S2).

4.  tyConScopedTyVars.  A challenging piece in all of this is that we
    end up taking three separate passes over every declaration:
      - one in getInitialKind (this pass look only at the head, not the body)
      - one in kcTyClDecls (to kind-check the body)
      - a final one in tcTyClDecls (to desugar)

    In the latter two passes, we need to connect the user-written type
    variables in an LHsQTyVars with the variables in the tycon's
    inferred kind. Because the tycon might not have a CUSK, this
    matching up is, in general, quite hard to do.  (Look through the
    git history between Dec 2015 and Apr 2016 for
    TcHsType.splitTelescopeTvs!)

    Instead of trying, we just store the list of type variables to
    bring into scope, in the tyConScopedTyVars field of the TcTyCon.
    These tyvars are brought into scope in kcTyClTyVars and
    tcTyClTyVars, both in TcHsType.

    In a TcTyCon, why is tyConScopedTyVars :: [(Name,TcTyVar)] rather
    than just [TcTyVar]?  Consider these mutually-recursive decls
       data T (a :: k1) b = MkT (S a b)
       data S (c :: k2) d = MkS (T c d)
    We start with k1 bound to kappa1, and k2 to kappa2; so initially
    in the (Name,TcTyVar) pairs the Name is that of the TcTyVar. But
    then kappa1 and kappa2 get unified; so after the zonking in
    'generalise' in 'kcTyClGroup' the Name and TcTyVar may differ.

See also Note [Type checking recursive type and class declarations].

Note [Type environment evolution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As we typecheck a group of declarations the type environment evolves.
Consider for example:
  data B (a :: Type) = MkB (Proxy 'MkB)

We do the following steps:

  1. Start of tcTyClDecls: use mkPromotionErrorEnv to initialise the
     type env with promotion errors
            B   :-> TyConPE
            MkB :-> DataConPE

  2. kcTyCLGruup
      - Do getInitialKinds, which will signal a promotion
        error if B is used in any of the kinds needed to initialse
        B's kind (e.g. (a :: Type)) here

      - Extend the type env with these initial kinds (monomorphic for
        decls that lack a CUSK)
            B :-> TcTyCon <initial kind>
        (thereby overriding the B :-> TyConPE binding)
        and do kcLTyClDecl on each decl to get equality constraints on
        all those inital kinds

      - Generalise the inital kind, making a poly-kinded TcTyCon

  3. Back in tcTyDecls, extend the envt with bindings of the poly-kinded
     TcTyCons, again overriding the promotion-error bindings.

     But note that the data constructor promotion errors are still in place
     so that (in our example) a use of MkB will sitll be signalled as
     an error.

  4. Typecheck the decls.

  5. In tcTyClGroup, extend the envt with bindings for TyCon and DataCons


Note [Missed opportunity to retain higher-rank kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In 'kcTyClGroup', there is a missed opportunity to make kind
inference work in a few more cases.  The idea is analogous
to Note [Single function non-recursive binding special-case]:

     * If we have an SCC with a single decl, which is non-recursive,
       instead of creating a unification variable representing the
       kind of the decl and unifying it with the rhs, we can just
       read the type directly of the rhs.

     * Furthermore, we can update our SCC analysis to ignore
       dependencies on declarations which have CUSKs: we don't
       have to kind-check these all at once, since we can use
       the CUSK to initialize the kind environment.

Unfortunately this requires reworking a bit of the code in
'kcLTyClDecl' so I've decided to punt unless someone shouts about it.

Note [Don't process associated types in kcLHsQTyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Previously, we processed associated types in the thing_inside in kcLHsQTyVars,
but this was wrong -- we want to do ATs sepearately.
The consequence for not doing it this way is #15142:

  class ListTuple (tuple :: Type) (as :: [(k, Type)]) where
    type ListToTuple as :: Type

We assign k a kind kappa[1]. When checking the tuple (k, Type), we try to unify
kappa ~ Type, but this gets deferred because we bumped the TcLevel as we bring
`tuple` into scope. Thus, when we check ListToTuple, kappa[1] still hasn't
unified with Type. And then, when we generalize the kind of ListToTuple (which
indeed has a CUSK, according to the rules), we skolemize the free metavariable
kappa. Note that we wouldn't skolemize kappa when generalizing the kind of ListTuple,
because the solveEqualities in kcLHsQTyVars is at TcLevel 1 and so kappa[1]
will unify with Type.

Bottom line: as associated types should have no effect on a CUSK enclosing class,
we move processing them to a separate action, run after the outer kind has
been generalized.

-}

kcTyClGroup :: [LTyClDecl GhcRn] -> TcM [TcTyCon]

-- Kind check this group, kind generalize, and return the resulting local env
-- This binds the TyCons and Classes of the group, but not the DataCons
-- See Note [Kind checking for type and class decls]
-- Third return value is Nothing if the tycon be unsaturated; otherwise,
-- the arity
kcTyClGroup decls
  = do  { mod <- getModule
        ; traceTc "---- kcTyClGroup ---- {"
            (text "module" <+> ppr mod $$ vcat (map ppr decls))

          -- Kind checking;
          --    1. Bind kind variables for decls
          --    2. Kind-check decls
          --    3. Generalise the inferred kinds
          -- See Note [Kind checking for type and class decls]

          -- Step 1: Bind kind variables for all decls
        ; initial_tcs <- getInitialKinds decls
        ; traceTc "kcTyClGroup: initial kinds" $
          ppr_tc_kinds initial_tcs

         -- Step 2: Set extended envt, kind-check the decls
         -- NB: the environment extension overrides the tycon
         --     promotion-errors bindings
         --     See Note [Type environment evolution]

        ; solveEqualities $
          tcExtendKindEnvWithTyCons initial_tcs $
          mapM_ kcLTyClDecl decls

        -- Step 3: skolemisation
        -- Kind checking done for this group
        -- Now we have to kind skolemise the flexis
        ; candidates <- gather_quant_candidates initial_tcs
        ; _ <- quantifyTyVars emptyVarSet candidates
           -- We'll get the actual vars to quantify over later.

        -- Step 4: generalisation
        -- Finally, go through each tycon and give it its final kind,
        -- with all the required, specified, and inferred variables
        -- in order.
        ; poly_tcs <- mapAndReportM generalise initial_tcs

        ; traceTc "---- kcTyClGroup end ---- }" (ppr_tc_kinds poly_tcs)
        ; return poly_tcs }

  where
    ppr_tc_kinds tcs = vcat (map pp_tc tcs)
    pp_tc tc = ppr (tyConName tc) <+> dcolon <+> ppr (tyConKind tc)

    gather_quant_candidates :: [TcTyCon] -> TcM CandidatesQTvs
    gather_quant_candidates tcs = mconcat <$> mapM gather1 tcs

    gather1 :: TcTyCon -> TcM CandidatesQTvs
    gather1 tc
      | tcTyConIsPoly tc  -- these don't need generalisation
      = return mempty

      | otherwise
      = do { tc_binders  <- zonkTyConBinders (tyConBinders tc)
           ; tc_res_kind <- zonkTcType (tyConResKind tc)

           ; let tvs = mkDVarSet $ map binderVar tc_binders
                 kvs = tyCoVarsOfTypesDSet (tc_res_kind : map binderType tc_binders)
                       `minusDVarSet` tvs

           ; return (mempty { dv_kvs = kvs, dv_tvs = tvs }) }

    generalise :: TcTyCon -> TcM TcTyCon
    generalise tc
      | tcTyConIsPoly tc
      = return tc  -- nothing to do here; we already have the final kind
                   -- This is just an optimization; generalising is a no-op

      | otherwise
        -- See Note [Required, Specified, and Inferred for types]
      = do {  -- Step 0: get the tyvars from the enclosing class (if any)
             (all_class_tctvs, class_scoped_tvs) <- get_class_tvs tc

              -- Step 1: gather all the free variables
           ; tc_tvs          <- mapM zonkTcTyCoVarBndr (map binderVar (tyConBinders tc))
           ; tc_res_kind     <- zonkTcType (tyConResKind tc)
           ; scoped_tv_pairs <- zonkTyVarTyVarPairs (tcTyConScopedTyVars tc)

           ; let all_fvs    = tyCoVarsOfTypesDSet (tc_res_kind : map tyVarKind tc_tvs)
                 scoped_tvs = map snd scoped_tv_pairs

           ; MASSERT( all ((== Required) . tyConBinderArgFlag) (tyConBinders tc) )

             -- Step 2: Select out the Required arguments; that is, the tc_binders
           ; let no_req_fvs = all_fvs `delDVarSetList` tc_tvs

             -- Step 3: partition remaining variables into class variables and
             -- local variables (matters only for associated types)
                 (class_fvs, local_fvs)
                   = partitionDVarSet (`elemDVarSet` all_class_tctvs) no_req_fvs

             -- Step 4: For each set so far, use the set to select the scoped_tvs.
             -- We take from the scoped_tvs to preserve order. These tvs will become
             -- the Specified ones.
                 class_specified = filter (`elemDVarSet` class_fvs) class_scoped_tvs
                 local_specified = filter (`elemDVarSet` local_fvs) scoped_tvs

             -- Step 5: Order the specified variables by ScopedSort
             -- See Note [ScopedSort] in Type
                 class_specified_sorted = scopedSort class_specified
                 local_specified_sorted = scopedSort local_specified

             -- Step 6: Remove the Specified ones from the fv sets. These are the
             -- Inferred ones.
                 class_inferred_set = class_fvs `delDVarSetList` class_specified_sorted
                 local_inferred_set = local_fvs `delDVarSetList` local_specified_sorted

                 class_inferred = dVarSetElemsWellScoped class_inferred_set
                 local_inferred = dVarSetElemsWellScoped local_inferred_set

             -- Step 7: Make the TyConBinders.
                 class_inferred_tcbs  = mkNamedTyConBinders Inferred class_inferred
                 class_specified_tcbs = mkNamedTyConBinders Specified class_specified_sorted
                 local_inferred_tcbs  = mkNamedTyConBinders Inferred local_inferred
                 local_specified_tcbs = mkNamedTyConBinders Specified local_specified_sorted

                 mk_req_tcb tv
                   | tv `elemDVarSet` all_fvs = mkNamedTyConBinder Required tv
                   | otherwise                = mkAnonTyConBinder tv

                 required_tcbs = map mk_req_tcb tc_tvs

             -- Step 8: Assemble the final list.
                 final_tcbs = concat [ class_inferred_tcbs
                                     , class_specified_tcbs
                                     , local_inferred_tcbs
                                     , local_specified_tcbs
                                     , required_tcbs ]

             -- Step 9: Check for validity. We do this here because we're about to
             -- put the tycon into the environment, and we don't want anything malformed
             -- in the environment.
           ; let user_tyvars = tcTyConUserTyVars tc
           ; setSrcSpan (getSrcSpan tc) $
             addTyConCtxt tc $
             checkValidTelescope final_tcbs user_tyvars

             -- Step 10: Make the result TcTyCon
           ; let name = tyConName tc
           ; traceTc "Generalise kind" $
             vcat [ text "name =" <+> ppr name
                  , text "all_class_tctvs =" <+> ppr all_class_tctvs
                  , text "class_scoped_tvs =" <+> ppr class_scoped_tvs
                  , text "tc_tvs =" <+> ppr tc_tvs
                  , text "tc_res_kind =" <+> ppr tc_res_kind
                  , text "scoped_tvs =" <+> ppr scoped_tvs
                  , text "class_inferred_tcbs =" <+> ppr class_inferred_tcbs
                  , text "class_specified_tcbs =" <+> ppr class_specified_tcbs
                  , text "local_inferred_tcbs =" <+> ppr local_inferred_tcbs
                  , text "local_specified_tcbs =" <+> ppr local_specified_tcbs
                  , text "required_tcbs =" <+> ppr required_tcbs ]
           ; return $ mkTcTyCon name user_tyvars final_tcbs tc_res_kind scoped_tv_pairs
                                True {- it's generalised now -} (tyConFlavour tc) }

    get_class_tvs :: TcTyCon -> TcM (DTyCoVarSet, [TcTyVar])
        -- returns all tyConTyVars of the enclosing class, as well as its
        -- scoped type variables. Both are zonked.
    get_class_tvs at_tc
      | Just class_tc <- tyConAssoc_maybe at_tc
      = do { -- We can't just call tyConTyVars, because the enclosing class
             -- hasn't been generalised yet
             tc_binders  <- zonkTyConBinders (tyConBinders class_tc)
           ; tc_res_kind <- zonkTcType (tyConResKind class_tc)
           ; scoped_tvs  <- mapM zonkTcTyVarToTyVar (map snd (tcTyConScopedTyVars class_tc))

           ; return ( tyCoVarsOfTypesDSet (tc_res_kind : map binderType tc_binders)
                      `extendDVarSetList` tyConTyVars class_tc
                    , scoped_tvs ) }

      | otherwise
      = return (emptyDVarSet, [])

{- Note [Required, Specified, and Inferred for types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have some design choices in how we classify the tyvars bound
in a type declaration. (Here, I use "type" to refer to any TyClDecl.)
Much of the debate is memorialized in #15743. This Note documents
the final conclusion.

First, a reminder:
  * a Required argument is one that must be provided at every call site
  * a Specified argument is one that can be inferred at call sites, but
    may be instantiated with visible type application
  * an Inferred argument is one that must be inferred at call sites; it
    is unavailable for use with visible type application.

Why have Inferred at all? Because we just can't make user-facing promises
about the ordering of some variables. These might swizzle around even between
minor released. By forbidding visible type application, we ensure users
aren't caught unawares. See also
Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility] in TyCoRep.

When inferring the ordering of variables (that is, for those
variables that he user has not specified the order with an explicit `forall`)
we use the following order:

 1. Inferred variables from an enclosing class (associated types only)
 2. Specified variables from an enclosing class (associated types only)
 3. Inferred variables not from an enclosing class
 4. Specified variables not from an enclosing class
 5. Required variables before a top-level ::
 6. All variables after a top-level ::

If this ordering does not make a valid telescope, we reject the definition.

This idea is implemented in the generalise function within kcTyClGroup (for
declarations without CUSKs), and in kcLHsQTyVars (for declarations with
CUSKs). Note that neither definition worries about point (6) above, as this
is nicely handled by not mangling the res_kind. (Mangling res_kinds is done
*after* all this stuff, in tcDataDefn's call to tcDataKindSig.) We can
easily tell Inferred apart from Specified by looking at the scoped tyvars;
Specified are always included there.

One other small open question here: how to classify variables from an
enclosing class? Here is an example:

  class C (a :: k) where
    type F a

In the kind of F, should k be Inferred or Specified? Currently, we mark
it as Specified, as we can commit to an ordering, based on the ordering
of class variables in the enclosing class declaration. If k were not mentioned
in the class head, then it would be Inferred. The alternative to this
approach is to make the Inferred/Specified distinction locally, by just
looking at the declaration for F. This lowers the availability of type
application, but makes the reasoning more local. However, this alternative
also disagrees with the treatment for methods, where all class variables
are Specified, regardless of whether or not the variable is mentioned in the
method type.

A few points of motivation for the ordering above:

* We put the class variables before the local variables in a nod to the
  treatment for class methods, where class variables (and the class constraint)
  come first. While this is an unforced design decision, it never rejects
  more declarations, as class variables can never depend on local variables.

* We rigidly require the ordering above, even though we could be much more
  permissive. Relevant musings are at
  https://ghc.haskell.org/trac/ghc/ticket/15743#comment:7
  The bottom line conclusion is that, if the user wants a different ordering,
  then can specify it themselves, and it is better to be predictable and dumb
  than clever and capricious.

  I (Richard) conjecture we could be fully permissive, allowing all classes
  of variables to intermix. We would have to augment ScopedSort to refuse to
  reorder Required variables (or check that it wouldn't have). But this would
  allow more programs. See #15743 for examples. Interestingly, Idris seems
  to allow this intermixing. The intermixing would be fully specified, in that
  we can be sure that inference wouldn't change between versions. However,
  would users be able to predict it? That I cannot answer.

Test cases (and tickets) relevant to these design decisions:
  T15591*
  T15592*
  T15743*

-}

--------------
tcExtendKindEnvWithTyCons :: [TcTyCon] -> TcM a -> TcM a
tcExtendKindEnvWithTyCons tcs
  = tcExtendKindEnvList [ (tyConName tc, ATcTyCon tc) | tc <- tcs ]

--------------
mkPromotionErrorEnv :: [LTyClDecl GhcRn] -> TcTypeEnv
-- Maps each tycon/datacon to a suitable promotion error
--    tc :-> APromotionErr TyConPE
--    dc :-> APromotionErr RecDataConPE
--    See Note [Recursion and promoting data constructors]

mkPromotionErrorEnv decls
  = foldr (plusNameEnv . mk_prom_err_env . unLoc)
          emptyNameEnv decls

mk_prom_err_env :: TyClDecl GhcRn -> TcTypeEnv
mk_prom_err_env (ClassDecl { tcdLName = L _ nm, tcdATs = ats })
  = unitNameEnv nm (APromotionErr ClassPE)
    `plusNameEnv`
    mkNameEnv [ (name, APromotionErr TyConPE)
              | (dL->L _ (FamilyDecl { fdLName = (dL->L _ name) })) <- ats ]

mk_prom_err_env (DataDecl { tcdLName = (dL->L _ name)
                          , tcdDataDefn = HsDataDefn { dd_cons = cons } })
  = unitNameEnv name (APromotionErr TyConPE)
    `plusNameEnv`
    mkNameEnv [ (con, APromotionErr RecDataConPE)
              | (dL->L _ con') <- cons
              , (dL->L _ con)  <- getConNames con' ]

mk_prom_err_env decl
  = unitNameEnv (tcdName decl) (APromotionErr TyConPE)
    -- Works for family declarations too

--------------
getInitialKinds :: [LTyClDecl GhcRn] -> TcM [TcTyCon]
-- Returns a TcTyCon for each TyCon bound by the decls,
-- each with its initial kind

getInitialKinds decls = concatMapM (addLocM getInitialKind) decls

getInitialKind :: TyClDecl GhcRn -> TcM [TcTyCon]
-- Allocate a fresh kind variable for each TyCon and Class
-- For each tycon, return a TcTyCon with kind k
-- where k is the kind of tc, derived from the LHS
--         of the definition (and probably including
--         kind unification variables)
--      Example: data T a b = ...
--      return (T, kv1 -> kv2 -> kv3)
--
-- This pass deals with (ie incorporates into the kind it produces)
--   * The kind signatures on type-variable binders
--   * The result kinds signature on a TyClDecl
--
-- No family instances are passed to getInitialKinds

getInitialKind decl@(ClassDecl { tcdLName = (dL->L _ name)
                               , tcdTyVars = ktvs
                               , tcdATs = ats })
  = do { let cusk = hsDeclHasCusk decl
       ; tycon <- kcLHsQTyVars name ClassFlavour cusk ktvs $
                  return constraintKind
       ; let parent_tv_prs = tcTyConScopedTyVars tycon
            -- See Note [Don't process associated types in kcLHsQTyVars]
       ; inner_tcs <- tcExtendNameTyVarEnv parent_tv_prs $
                      getFamDeclInitialKinds (Just tycon) ats
       ; return (tycon : inner_tcs) }

getInitialKind decl@(DataDecl { tcdLName = (dL->L _ name)
                              , tcdTyVars = ktvs
                              , tcdDataDefn = HsDataDefn { dd_kindSig = m_sig
                                                         , dd_ND = new_or_data } })
  = do  { tycon <-
           kcLHsQTyVars name (newOrDataToFlavour new_or_data)
                        (hsDeclHasCusk decl) ktvs $
           case m_sig of
             Just ksig -> tcLHsKindSig (DataKindCtxt name) ksig
             Nothing   -> return liftedTypeKind
        ; return [tycon] }

getInitialKind (FamDecl { tcdFam = decl })
  = do { tc <- getFamDeclInitialKind Nothing decl
       ; return [tc] }

getInitialKind decl@(SynDecl { tcdLName = (dL->L _ name)
                             , tcdTyVars = ktvs
                             , tcdRhs = rhs })
  = do  { tycon <- kcLHsQTyVars name TypeSynonymFlavour (hsDeclHasCusk decl)
                                ktvs $
            case kind_annotation rhs of
              Nothing -> newMetaKindVar
              Just ksig -> tcLHsKindSig (TySynKindCtxt name) ksig
        ; return [tycon] }
  where
    -- Keep this synchronized with 'hsDeclHasCusk'.
    kind_annotation (dL->L _ ty) = case ty of
        HsParTy _ lty     -> kind_annotation lty
        HsKindSig _ _ k   -> Just k
        _                 -> Nothing

getInitialKind (DataDecl _ _ _ _ (XHsDataDefn _)) = panic "getInitialKind"
getInitialKind (XTyClDecl _) = panic "getInitialKind"

---------------------------------
getFamDeclInitialKinds
  :: Maybe TcTyCon -- ^ Enclosing class TcTyCon, if any
  -> [LFamilyDecl GhcRn]
  -> TcM [TcTyCon]
getFamDeclInitialKinds mb_parent_tycon decls
  = mapM (addLocM (getFamDeclInitialKind mb_parent_tycon)) decls

getFamDeclInitialKind
  :: Maybe TcTyCon -- ^ Enclosing class TcTyCon, if any
  -> FamilyDecl GhcRn
  -> TcM TcTyCon
getFamDeclInitialKind mb_parent_tycon
    decl@(FamilyDecl { fdLName     = (dL->L _ name)
                     , fdTyVars    = ktvs
                     , fdResultSig = (dL->L _ resultSig)
                     , fdInfo      = info })
  = kcLHsQTyVars name flav cusk ktvs $
    case resultSig of
      KindSig _ ki                              -> tcLHsKindSig ctxt ki
      TyVarSig _ (dL->L _ (KindedTyVar _ _ ki)) -> tcLHsKindSig ctxt ki
      _ -- open type families have * return kind by default
        | tcFlavourIsOpen flav              -> return liftedTypeKind
               -- closed type families have their return kind inferred
               -- by default
        | otherwise                         -> newMetaKindVar
  where
    mb_cusk = tcTyConIsPoly <$> mb_parent_tycon
    cusk    = famDeclHasCusk mb_cusk decl
    flav  = case info of
      DataFamily         -> DataFamilyFlavour mb_parent_tycon
      OpenTypeFamily     -> OpenTypeFamilyFlavour mb_parent_tycon
      ClosedTypeFamily _ -> ASSERT( isNothing mb_parent_tycon )
                            ClosedTypeFamilyFlavour
    ctxt  = TyFamResKindCtxt name
getFamDeclInitialKind _ (XFamilyDecl _) = panic "getFamDeclInitialKind"

------------------------------------------------------------------------
kcLTyClDecl :: LTyClDecl GhcRn -> TcM ()
  -- See Note [Kind checking for type and class decls]
kcLTyClDecl (dL->L loc decl)
  | hsDeclHasCusk decl  -- See Note [Skip decls with CUSKs in kcLTyClDecl]
  = traceTc "kcTyClDecl skipped due to cusk:" (ppr tc_name)

  | otherwise
  = setSrcSpan loc $
    tcAddDeclCtxt decl $
    do { traceTc "kcTyClDecl {" (ppr tc_name)
       ; kcTyClDecl decl
       ; traceTc "kcTyClDecl done }" (ppr tc_name) }
  where
    tc_name = tyClDeclLName decl

kcTyClDecl :: TyClDecl GhcRn -> TcM ()
-- This function is used solely for its side effect on kind variables
-- NB kind signatures on the type variables and
--    result kind signature have already been dealt with
--    by getInitialKind, so we can ignore them here.

kcTyClDecl (DataDecl { tcdLName    = (dL->L _ name)
                     , tcdDataDefn = defn })
  | HsDataDefn { dd_cons = cons@((dL->L _ (ConDeclGADT {})) : _)
               , dd_ctxt = (dL->L _ []) } <- defn
  = mapM_ (wrapLocM_ kcConDecl) cons
    -- hs_tvs and dd_kindSig already dealt with in getInitialKind
    -- This must be a GADT-style decl,
    --        (see invariants of DataDefn declaration)
    -- so (a) we don't need to bring the hs_tvs into scope, because the
    --        ConDecls bind all their own variables
    --    (b) dd_ctxt is not allowed for GADT-style decls, so we can ignore it

  | HsDataDefn { dd_ctxt = ctxt, dd_cons = cons } <- defn
  = kcTyClTyVars name $
    do  { _ <- tcHsContext ctxt
        ; mapM_ (wrapLocM_ kcConDecl) cons }

kcTyClDecl (SynDecl { tcdLName = (dL->L _ name)
                    , tcdRhs = lrhs })
  = kcTyClTyVars name $
    do  { syn_tc <- kcLookupTcTyCon name
        -- NB: check against the result kind that we allocated
        -- in getInitialKinds.
        ; discardResult $ tcCheckLHsType lrhs (tyConResKind syn_tc) }

kcTyClDecl (ClassDecl { tcdLName = (dL->L _ name)
                      , tcdCtxt = ctxt, tcdSigs = sigs })
  = kcTyClTyVars name $
    do  { _ <- tcHsContext ctxt
        ; mapM_ (wrapLocM_ kc_sig)     sigs }
  where
    kc_sig (ClassOpSig _ _ nms op_ty)
             = kcHsSigType (TyConSkol ClassFlavour name) nms op_ty
    kc_sig _ = return ()

kcTyClDecl (FamDecl _ (FamilyDecl { fdLName  = (dL->L _ fam_tc_name)
                                  , fdInfo   = fd_info }))
-- closed type families look at their equations, but other families don't
-- do anything here
  = case fd_info of
      ClosedTypeFamily (Just eqns) ->
        do { fam_tc <- kcLookupTcTyCon fam_tc_name
           ; mapM_ (kcTyFamInstEqn fam_tc) eqns }
      _ -> return ()
kcTyClDecl (FamDecl _ (XFamilyDecl _))              = panic "kcTyClDecl"
kcTyClDecl (DataDecl _ _ _ _ (XHsDataDefn _)) = panic "kcTyClDecl"
kcTyClDecl (XTyClDecl _)                            = panic "kcTyClDecl"

-------------------
kcConDecl :: ConDecl GhcRn -> TcM ()
kcConDecl (ConDeclH98 { con_name = name, con_ex_tvs = ex_tvs
                      , con_mb_cxt = ex_ctxt, con_args = args })
  = addErrCtxt (dataConCtxtName [name]) $
      -- See Note [Use TyVarTvs in kind-checking pass]
    kcExplicitTKBndrs ex_tvs $
    do { _ <- tcHsMbContext ex_ctxt
       ; mapM_ (tcHsOpenType . getBangType) (hsConDeclArgTys args) }
              -- We don't need to check the telescope here, because that's
              -- done in tcConDecl

kcConDecl (ConDeclGADT { con_names = names
                       , con_qvars = qtvs, con_mb_cxt = cxt
                       , con_args = args, con_res_ty = res_ty })
  | HsQTvs { hsq_ext = HsQTvsRn { hsq_implicit = implicit_tkv_nms }
           , hsq_explicit = explicit_tkv_nms } <- qtvs
  = -- Even though the data constructor's type is closed, we
    -- must still kind-check the type, because that may influence
    -- the inferred kind of the /type/ constructor.  Example:
    --    data T f a where
    --      MkT :: f a -> T f a
    -- If we don't look at MkT we won't get the correct kind
    -- for the type constructor T
    addErrCtxt (dataConCtxtName names) $
    discardResult $
    kcImplicitTKBndrs implicit_tkv_nms $
    kcExplicitTKBndrs explicit_tkv_nms $
    do { _ <- tcHsMbContext cxt
       ; mapM_ (tcHsOpenType . getBangType) (hsConDeclArgTys args)
       ; _ <- tcHsOpenType res_ty
       ; return () }
kcConDecl (XConDecl _) = panic "kcConDecl"
kcConDecl (ConDeclGADT _ _ _ (XLHsQTyVars _) _ _ _ _) = panic "kcConDecl"

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

Note [Use TyVarTvs in kind-checking pass]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  data Proxy a where
    MkProxy1 :: forall k (b :: k). Proxy b
    MkProxy2 :: forall j (c :: j). Proxy c

It seems reasonable that this should be accepted. But something very strange
is going on here: when we're kind-checking this declaration, we need to unify
the kind of `a` with k and j -- even though k and j's scopes are local to the type of
MkProxy{1,2}. The best approach we've come up with is to use TyVarTvs during
the kind-checking pass. First off, note that it's OK if the kind-checking pass
is too permissive: we'll snag the problems in the type-checking pass later.
(This extra permissiveness might happen with something like

  data SameKind :: k -> k -> Type
  data Bad a where
    MkBad :: forall k1 k2 (a :: k1) (b :: k2). Bad (SameKind a b)

which would be accepted if k1 and k2 were TyVarTvs. This is correctly rejected
in the second pass, though. Test case: polykinds/TyVarTvKinds3)
Recall that the kind-checking pass exists solely to collect constraints
on the kinds and to power unification.

To achieve the use of TyVarTvs, we must be careful to use specialized functions
that produce TyVarTvs, not ordinary skolems. This is why we need
kcExplicitTKBndrs and kcImplicitTKBndrs in TcHsType, separate from their
tc... variants.

The drawback of this approach is sometimes it will accept a definition that
a (hypothetical) declarative specification would likely reject. As a general
rule, we don't want to allow polymorphic recursion without a CUSK. Indeed,
the whole point of CUSKs is to allow polymorphic recursion. Yet, the TyVarTvs
approach allows a limited form of polymorphic recursion *without* a CUSK.

To wit:
  data T a = forall k (b :: k). MkT (T b) Int
  (test case: dependent/should_compile/T14066a)

Note that this is polymorphically recursive, with the recursive occurrence
of T used at a kind other than a's kind. The approach outlined here accepts
this definition, because this kind is still a kind variable (and so the
TyVarTvs unify). Stepping back, I (Richard) have a hard time envisioning a
way to describe exactly what declarations will be accepted and which will
be rejected (without a CUSK). However, the accepted definitions are indeed
well-kinded and any rejected definitions would be accepted with a CUSK,
and so this wrinkle need not cause anyone to lose sleep.

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

  * During TcHsType.tcTyVar we look in the *local* env, to get the
    fully-known, not knot-tied TcTyCon for T.

  * Then, in TcHsSyn.zonkTcTypeToType (and zonkTcTyCon in particular)
    we look in the *global* env to get the TyCon.

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

The initial kind is stored in exactly the same way during
kind-checking as it is during type-checking (Note [Type checking
recursive type and class declarations]): in the *local* environment,
with ATcTyCon. But we still must store *something* in the *global*
environment. Even though we discard the result of kind-checking, we
sometimes need to produce error messages. These error messages will
want to refer to the tycons being checked, except that they don't
exist yet, and it would be Terribly Annoying to get the error messages
to refer back to HsSyn. So we create a TcTyCon and put it in the
global env. This tycon can print out its name and knows its kind, but
any other action taken on it will panic. Note that TcTyCons are *not*
knot-tied, unlike the rather valid but knot-tied ones that occur
during type-checking.

Note [Declarations for wired-in things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For wired-in things we simply ignore the declaration
and take the wired-in information.  That avoids complications.
e.g. the need to make the data constructor worker name for
     a constraint tuple match the wired-in one
-}

tcTyClDecl :: RolesInfo -> LTyClDecl GhcRn -> TcM TyCon
tcTyClDecl roles_info (dL->L loc decl)
  | Just thing <- wiredInNameTyThing_maybe (tcdName decl)
  = case thing of -- See Note [Declarations for wired-in things]
      ATyCon tc -> return tc
      _ -> pprPanic "tcTyClDecl" (ppr thing)

  | otherwise
  = setSrcSpan loc $ tcAddDeclCtxt decl $
    do { traceTc "---- tcTyClDecl ---- {" (ppr decl)
       ; tc <- tcTyClDecl1 Nothing roles_info decl
       ; traceTc "---- tcTyClDecl end ---- }" (ppr tc)
       ; return tc }

  -- "type family" declarations
tcTyClDecl1 :: Maybe Class -> RolesInfo -> TyClDecl GhcRn -> TcM TyCon
tcTyClDecl1 parent _roles_info (FamDecl { tcdFam = fd })
  = tcFamDecl1 parent fd

  -- "type" synonym declaration
tcTyClDecl1 _parent roles_info
            (SynDecl { tcdLName = (dL->L _ tc_name)
                     , tcdRhs   = rhs })
  = ASSERT( isNothing _parent )
    tcTyClTyVars tc_name $ \ binders res_kind ->
    tcTySynRhs roles_info tc_name binders res_kind rhs

  -- "data/newtype" declaration
tcTyClDecl1 _parent roles_info
            (DataDecl { tcdLName = (dL->L _ tc_name)
                      , tcdDataDefn = defn })
  = ASSERT( isNothing _parent )
    tcTyClTyVars tc_name $ \ tycon_binders res_kind ->
    tcDataDefn roles_info tc_name tycon_binders res_kind defn

tcTyClDecl1 _parent roles_info
            (ClassDecl { tcdLName = (dL->L _ class_name)
                       , tcdCtxt = hs_ctxt
                       , tcdMeths = meths
                       , tcdFDs = fundeps
                       , tcdSigs = sigs
                       , tcdATs = ats
                       , tcdATDefs = at_defs })
  = ASSERT( isNothing _parent )
    do { clas <- tcClassDecl1 roles_info class_name hs_ctxt
                              meths fundeps sigs ats at_defs
       ; return (classTyCon clas) }

tcTyClDecl1 _ _ (XTyClDecl _) = panic "tcTyClDecl1"


{- *********************************************************************
*                                                                      *
          Class declarations
*                                                                      *
********************************************************************* -}

tcClassDecl1 :: RolesInfo -> Name -> LHsContext GhcRn
             -> LHsBinds GhcRn -> [LHsFunDep GhcRn] -> [LSig GhcRn]
             -> [LFamilyDecl GhcRn] -> [LTyFamDefltEqn GhcRn]
             -> TcM Class
tcClassDecl1 roles_info class_name hs_ctxt meths fundeps sigs ats at_defs
  = fixM $ \ clas ->
    -- We need the knot because 'clas' is passed into tcClassATs
    tcTyClTyVars class_name $ \ binders res_kind ->
    do { MASSERT2( tcIsConstraintKind res_kind
                 , ppr class_name $$ ppr res_kind )
       ; traceTc "tcClassDecl 1" (ppr class_name $$ ppr binders)
       ; let tycon_name = class_name        -- We use the same name
             roles = roles_info tycon_name  -- for TyCon and Class

       ; (ctxt, fds, sig_stuff, at_stuff)
            <- solveEqualities $
               do { ctxt <- tcHsContext hs_ctxt
                  ; fds  <- mapM (addLocM tc_fundep) fundeps
                  ; sig_stuff <- tcClassSigs class_name sigs meths
                  ; at_stuff  <- tcClassATs class_name clas ats at_defs
                  ; return (ctxt, fds, sig_stuff, at_stuff) }

       -- The solveEqualities will report errors for any
       -- unsolved equalities, so these zonks should not encounter
       -- any unfilled coercion variables unless there is such an error
       -- The zonk also squeeze out the TcTyCons, and converts
       -- Skolems to tyvars.
       ; ze        <- emptyZonkEnv
       ; ctxt      <- zonkTcTypesToTypesX ze ctxt
       ; sig_stuff <- mapM (zonkTcMethInfoToMethInfoX ze) sig_stuff
         -- ToDo: do we need to zonk at_stuff?

       -- TODO: Allow us to distinguish between abstract class,
       -- and concrete class with no methods (maybe by
       -- specifying a trailing where or not

       ; mindef <- tcClassMinimalDef class_name sigs sig_stuff
       ; is_boot <- tcIsHsBootOrSig
       ; let body | is_boot, null ctxt, null at_stuff, null sig_stuff
                  = Nothing
                  | otherwise
                  = Just (ctxt, at_stuff, sig_stuff, mindef)

       ; clas <- buildClass class_name binders roles fds body
       ; traceTc "tcClassDecl" (ppr fundeps $$ ppr binders $$
                                ppr fds)
       ; return clas }
  where
    tc_fundep (tvs1, tvs2) = do { tvs1' <- mapM (tcLookupTyVar . unLoc) tvs1 ;
                                ; tvs2' <- mapM (tcLookupTyVar . unLoc) tvs2 ;
                                ; return (tvs1', tvs2') }


{- Note [Associated type defaults]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following is an example of associated type defaults:
             class C a where
               data D a

               type F a b :: *
               type F a b = [a]        -- Default

Note that we can get default definitions only for type families, not data
families.
-}

tcClassATs :: Name                   -- The class name (not knot-tied)
           -> Class                  -- The class parent of this associated type
           -> [LFamilyDecl GhcRn]    -- Associated types.
           -> [LTyFamDefltEqn GhcRn] -- Associated type defaults.
           -> TcM [ClassATItem]
tcClassATs class_name cls ats at_defs
  = do {  -- Complain about associated type defaults for non associated-types
         sequence_ [ failWithTc (badATErr class_name n)
                   | n <- map at_def_tycon at_defs
                   , not (n `elemNameSet` at_names) ]
       ; mapM tc_at ats }
  where
    at_def_tycon :: LTyFamDefltEqn GhcRn -> Name
    at_def_tycon (dL->L _ eqn) = unLoc (feqn_tycon eqn)

    at_fam_name :: LFamilyDecl GhcRn -> Name
    at_fam_name (dL->L _ decl) = unLoc (fdLName decl)

    at_names = mkNameSet (map at_fam_name ats)

    at_defs_map :: NameEnv [LTyFamDefltEqn GhcRn]
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
                   -> [LTyFamDefltEqn GhcRn]        -- ^ Defaults
                   -> TcM (Maybe (KnotTied Type, SrcSpan))   -- ^ Type checked RHS
tcDefaultAssocDecl _ []
  = return Nothing  -- No default declaration

tcDefaultAssocDecl _ (d1:_:_)
  = failWithTc (text "More than one default declaration for"
                <+> ppr (feqn_tycon (unLoc d1)))

tcDefaultAssocDecl fam_tc [dL->L loc (FamEqn { feqn_tycon = (dL->L _ tc_name)
                                             , feqn_pats = hs_tvs
                                             , feqn_rhs = rhs })]
  | HsQTvs { hsq_ext = HsQTvsRn { hsq_implicit = imp_vars}
           , hsq_explicit = exp_vars } <- hs_tvs
  = -- See Note [Type-checking default assoc decls]
    setSrcSpan loc $
    tcAddFamInstCtxt (text "default type instance") tc_name $
    do { traceTc "tcDefaultAssocDecl" (ppr tc_name)
       ; let fam_tc_name = tyConName fam_tc
             fam_arity = length (tyConVisibleTyVars fam_tc)

       -- Kind of family check
       ; ASSERT( fam_tc_name == tc_name )
         checkTc (isTypeFamilyTyCon fam_tc) (wrongKindOfFamily fam_tc)

       -- Arity check
       ; checkTc (exp_vars `lengthIs` fam_arity)
                 (wrongNumberOfParmsErr fam_arity)

       -- Typecheck RHS
       ; let all_vars = imp_vars ++ map hsLTyVarName exp_vars
             pats     = map hsLTyVarBndrToType exp_vars

          -- NB: Use tcFamTyPats, not tcTyClTyVars. The latter expects to get
          -- the LHsQTyVars used for declaring a tycon, but the names here
          -- are different.

          -- You might think we should pass in some ClsInstInfo, as we're looking
          -- at an associated type. But this would be wrong, because an associated
          -- type default LHS can mention *different* type variables than the
          -- enclosing class. So it's treated more as a freestanding beast.
       ; (pats', rhs_ty)
           <- tcFamTyPats fam_tc Nothing all_vars Nothing pats
              (kcTyFamEqnRhs Nothing rhs) $
              \tvs pats rhs_kind ->
              do { rhs_ty <- solveEqualities $
                             tcCheckLHsType rhs rhs_kind

                     -- Zonk the patterns etc into the Type world
                 ; (ze, _) <- zonkTyBndrs tvs
                 ; pats'   <- zonkTcTypesToTypesX ze pats
                 ; rhs_ty'  <- zonkTcTypeToTypeX ze rhs_ty
                 ; return (pats', rhs_ty') }

         -- See Note [Type-checking default assoc decls]
       ; case tcMatchTys pats' (mkTyVarTys (tyConTyVars fam_tc)) of
           Just subst -> return (Just (substTyUnchecked subst rhs_ty, loc) )
           Nothing    -> failWithTc (defaultAssocKindErr fam_tc)
           -- We check for well-formedness and validity later,
           -- in checkValidClass
     }
tcDefaultAssocDecl _ [dL->L _ (XFamEqn _)] = panic "tcDefaultAssocDecl"
tcDefaultAssocDecl _ [dL->L _ (FamEqn _ _ _ (XLHsQTyVars _) _ _)]
  = panic "tcDefaultAssocDecl"
tcDefaultAssocDecl _ [_]
  = panic "tcDefaultAssocDecl: Impossible Match" -- due to #15884


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
proper tcMatchTys here.)

Recall also that the left-hand side of an associated type family
default is always just variables -- no tycons here. Accordingly,
the patterns used in the tcMatchTys won't actually be knot-tied,
even though we're in the knot. This is too delicate for my taste,
but it works.

-}

{- *********************************************************************
*                                                                      *
          Type family declarations
*                                                                      *
********************************************************************* -}

tcFamDecl1 :: Maybe Class -> FamilyDecl GhcRn -> TcM TyCon
tcFamDecl1 parent (FamilyDecl { fdInfo = fam_info
                              , fdLName = tc_lname@(dL->L _ tc_name)
                              , fdResultSig = (dL->L _ sig)
                              , fdTyVars = user_tyvars
                              , fdInjectivityAnn = inj })
  | DataFamily <- fam_info
  = tcTyClTyVars tc_name $ \ binders res_kind -> do
  { traceTc "data family:" (ppr tc_name)
  ; checkFamFlag tc_name

  -- Check the kind signature, if any.
  -- Data families might have a variable return kind.
  -- See See Note [Arity of data families] in FamInstEnv.
  ; (extra_binders, final_res_kind) <- tcDataKindSig binders res_kind
  ; checkTc (tcIsLiftedTypeKind final_res_kind
             || isJust (tcGetCastedTyVar_maybe final_res_kind))
            (badKindSig False res_kind)

  ; tc_rep_name <- newTyConRepName tc_name
  ; let tycon = mkFamilyTyCon tc_name (binders `chkAppend` extra_binders)
                              final_res_kind
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
       ; let tc_fam_tc = mkTcTyCon tc_name (ppr user_tyvars) binders res_kind
                                   [] False {- this doesn't matter here -}
                                   ClosedTypeFamilyFlavour

       ; branches <- mapAndReportM (tcTyFamInstEqn tc_fam_tc Nothing) eqns
         -- Do not attempt to drop equations dominated by earlier
         -- ones here; in the case of mutual recursion with a data
         -- type, we get a knot-tying failure.  Instead we check
         -- for this afterwards, in TcValidity.checkValidCoAxiom
         -- Example: tc265

         -- Create a CoAxiom, with the correct src location.
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
tcFamDecl1 _ (XFamilyDecl _) = panic "tcFamDecl1"

-- | Maybe return a list of Bools that say whether a type family was declared
-- injective in the corresponding type arguments. Length of the list is equal to
-- the number of arguments (including implicit kind/coercion arguments).
-- True on position
-- N means that a function is injective in its Nth argument. False means it is
-- not.
tcInjectivity :: [TyConBinder] -> Maybe (LInjectivityAnn GhcRn)
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
tcInjectivity tcbs (Just (dL->L loc (InjectivityAnn _ lInjNames)))
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
           -> LHsType GhcRn -> TcM TyCon
tcTySynRhs roles_info tc_name binders res_kind hs_ty
  = do { env <- getLclEnv
       ; traceTc "tc-syn" (ppr tc_name $$ ppr (tcl_env env))
       ; rhs_ty <- solveEqualities $ tcCheckLHsType hs_ty res_kind
       ; rhs_ty <- zonkTcTypeToType rhs_ty
       ; let roles = roles_info tc_name
             tycon = buildSynTyCon tc_name binders res_kind roles rhs_ty
       ; return tycon }

tcDataDefn :: RolesInfo -> Name
           -> [TyConBinder] -> Kind
           -> HsDataDefn GhcRn -> TcM TyCon
  -- NB: not used for newtype/data instances (whether associated or not)
tcDataDefn roles_info
           tc_name tycon_binders res_kind
         (HsDataDefn { dd_ND = new_or_data, dd_cType = cType
                     , dd_ctxt = ctxt, dd_kindSig = mb_ksig
                     , dd_cons = cons })
 =  do { tcg_env         <- getGblEnv
       ; let hsc_src = tcg_src tcg_env
       ; (extra_bndrs, final_res_kind) <- tcDataKindSig tycon_binders res_kind
       ; unless (mk_permissive_kind hsc_src cons) $
         checkTc (tcIsLiftedTypeKind final_res_kind) (badKindSig True res_kind)

       ; let final_bndrs  = tycon_binders `chkAppend` extra_bndrs
             roles        = roles_info tc_name

       ; stupid_tc_theta <- solveEqualities $ tcHsContext ctxt
       ; stupid_theta    <- zonkTcTypesToTypes stupid_tc_theta
       ; kind_signatures <- xoptM LangExt.KindSignatures

             -- Check that we don't use kind signatures without Glasgow extensions
       ; when (isJust mb_ksig) $
         checkTc (kind_signatures) (badSigTyDecl tc_name)

       ; gadt_syntax <- dataDeclChecks tc_name new_or_data stupid_theta cons

       ; tycon <- fixM $ \ tycon -> do
             { let res_ty = mkTyConApp tycon (mkTyVarTys (binderVars final_bndrs))
             ; data_cons <- tcConDecls tycon final_bndrs res_ty cons
             ; tc_rhs    <- mk_tc_rhs hsc_src tycon data_cons
             ; tc_rep_nm <- newTyConRepName tc_name
             ; return (mkAlgTyCon tc_name
                                  final_bndrs
                                  final_res_kind
                                  roles
                                  (fmap unLoc cType)
                                  stupid_theta tc_rhs
                                  (VanillaAlgTyCon tc_rep_nm)
                                  gadt_syntax) }
       ; traceTc "tcDataDefn" (ppr tc_name $$ ppr tycon_binders $$ ppr extra_bndrs)
       ; return tycon }
  where
    -- Abstract data types in hsig files can have arbitrary kinds,
    -- because they may be implemented by type synonyms
    -- (which themselves can have arbitrary kinds, not just *)
    mk_permissive_kind HsigFile [] = True
    mk_permissive_kind _ _ = False

    -- In hs-boot, a 'data' declaration with no constructors
    -- indicates a nominally distinct abstract data type.
    mk_tc_rhs HsBootFile _ []
      = return AbstractTyCon

    mk_tc_rhs HsigFile _ [] -- ditto
      = return AbstractTyCon

    mk_tc_rhs _ tycon data_cons
      = case new_or_data of
          DataType -> return (mkDataTyConRhs data_cons)
          NewType  -> ASSERT( not (null data_cons) )
                      mkNewTyConRhs tc_name tycon (head data_cons)
tcDataDefn _ _ _ _ (XHsDataDefn _) = panic "tcDataDefn"


-------------------------
kcTyFamInstEqn :: TcTyCon -> LTyFamInstEqn GhcRn -> TcM ()
kcTyFamInstEqn tc_fam_tc
    (dL->L loc
           (HsIB { hsib_ext = imp_vars
                 , hsib_body = FamEqn { feqn_tycon  = (dL->L _ eqn_tc_name)
                                      , feqn_bndrs  = mb_expl_bndrs
                                      , feqn_pats   = pats
                                      , feqn_rhs    = hs_ty }}))
  = setSrcSpan loc $
    do { traceTc "kcTyFamInstEqn" (vcat
           [ text "tc_name =" <+> ppr eqn_tc_name
           , text "fam_tc =" <+> ppr tc_fam_tc <+> dcolon <+> ppr (tyConKind tc_fam_tc)
           , text "hsib_vars =" <+> ppr imp_vars
           , text "feqn_bndrs =" <+> ppr mb_expl_bndrs
           , text "feqn_pats =" <+> ppr pats ])
       ; checkTc (fam_name == eqn_tc_name)
                 (wrongTyFamName fam_name eqn_tc_name)
          -- this check reports an arity error instead of a kind error; easier for user
       ; checkTc (pats `lengthIs` vis_arity) $
                  wrongNumberOfParmsErr vis_arity
       ; kcFamTyPats tc_fam_tc imp_vars mb_expl_bndrs pats $ \ rhs_kind ->
         discardResult $ kcTyFamEqnRhs Nothing hs_ty rhs_kind }
  where
    fam_name = tyConName tc_fam_tc
    vis_arity = length (tyConVisibleTyVars tc_fam_tc)
kcTyFamInstEqn _ (dL->L _ (XHsImplicitBndrs _)) = panic "kcTyFamInstEqn"
kcTyFamInstEqn _ (dL->L _ (HsIB _ (XFamEqn _))) = panic "kcTyFamInstEqn"
kcTyFamInstEqn _ _ = panic "kcTyFamInstEqn: Impossible Match" -- due to #15884

-- Infer the kind of the type on the RHS of a type family eqn. Then use
-- this kind to check the kind of the LHS of the equation. This is useful
-- as the callback to tcFamTyPats.
kcTyFamEqnRhs :: Maybe ClsInstInfo
              -> LHsType GhcRn        -- ^ Eqn RHS
              -> TcKind               -- ^ Inferred kind of left-hand side
              -> TcM ([TcTyVar], [TcType], TcKind)
                -- ^ New pattern skolems, New pats, inst'ed kind of left-hand side
kcTyFamEqnRhs mb_clsinfo rhs_hs_ty lhs_ki
  = do { -- It's still possible the lhs_ki has some foralls. Instantiate these away.
         (new_pats, insted_lhs_ki)
           <- instantiateTyUntilN mb_kind_env 0 lhs_ki

       ; traceTc "kcTyFamEqnRhs" (vcat
           [ text "rhs_hs_ty =" <+> ppr rhs_hs_ty
           , text "lhs_ki =" <+> ppr lhs_ki
           , text "insted_lhs_ki =" <+> ppr insted_lhs_ki
           , text "new_pats =" <+> ppr new_pats
           ])

       ; _ <- tcCheckLHsType rhs_hs_ty insted_lhs_ki

       ; return ([], new_pats, insted_lhs_ki) }
        -- we never introduce new skolems here
  where
    mb_kind_env = thdOf3 <$> mb_clsinfo

tcTyFamInstEqn :: TcTyCon -> Maybe ClsInstInfo -> LTyFamInstEqn GhcRn
               -> TcM (KnotTied CoAxBranch)
-- Needs to be here, not in TcInstDcls, because closed families
-- (typechecked here) have TyFamInstEqns
tcTyFamInstEqn fam_tc mb_clsinfo
    (dL->L loc (HsIB { hsib_ext = imp_vars
                     , hsib_body = FamEqn { feqn_tycon  = (dL->L _ eqn_tc_name)
                                          , feqn_bndrs  = mb_expl_bndrs
                                          , feqn_pats   = pats
                                          , feqn_rhs    = hs_ty }}))
  = ASSERT( getName fam_tc == eqn_tc_name )
    setSrcSpan loc $
    tcFamTyPats fam_tc mb_clsinfo imp_vars mb_expl_bndrs pats
                (kcTyFamEqnRhs mb_clsinfo hs_ty) $
                    \tvs pats res_kind ->
    do { traceTc "tcTyFamInstEqn {" (ppr eqn_tc_name <+> ppr pats)
       ; rhs_ty <- solveEqualities $ tcCheckLHsType hs_ty res_kind
       ; traceTc "tcTyFamInstEqn 1" (ppr eqn_tc_name <+> ppr pats)
       ; (ze, tvs') <- zonkTyBndrs tvs
       ; traceTc "tcTyFamInstEqn 2" (ppr eqn_tc_name <+> ppr pats)
       ; pats'      <- zonkTcTypesToTypesX ze pats
       ; traceTc "tcTyFamInstEqn 3" (ppr eqn_tc_name <+> ppr pats $$ ppr rhs_ty)
       ; rhs_ty'    <- zonkTcTypeToTypeX ze rhs_ty
       ; traceTc "tcTyFamInstEqn 4 }" (ppr fam_tc <+> pprTyVars tvs')
       ; return (mkCoAxBranch tvs' [] pats' rhs_ty'
                              (map (const Nominal) tvs')
                              loc) }
tcTyFamInstEqn _ _ (dL->L _ (XHsImplicitBndrs _)) = panic "tcTyFamInstEqn"
tcTyFamInstEqn _ _ (dL->L _ (HsIB _ (XFamEqn _))) = panic "tcTyFamInstEqn"
tcTyFamInstEqn _ _ _ = panic "tcTyFamInstEqn: Impossible Match" -- due to #15884

kcDataDefn :: Maybe (VarEnv Kind) -- ^ Possibly, instantiations for vars
                                  -- (associated types only)
           -> DataFamInstDecl GhcRn
           -> TcKind              -- ^ the kind of the tycon applied to pats
           -> TcM ([TcTyVar], [TcType], TcKind)
             -- ^ the kind signature might force instantiation
             -- of the tycon; this returns any extra skolems, args and the inst'ed kind
             -- See Note [Instantiating a family tycon]
-- Used for 'data instance' only
-- Ordinary 'data' is handled by kcTyClDecl
kcDataDefn mb_kind_env
           (DataFamInstDecl { dfid_eqn = HsIB { hsib_body =
              FamEqn { feqn_tycon  = fam_name
                     , feqn_bndrs  = mb_bndrs
                     , feqn_pats   = pats
                     , feqn_fixity = fixity
                     , feqn_rhs    = HsDataDefn { dd_ctxt = ctxt
                                                , dd_cons = cons
                                                , dd_kindSig = mb_kind } }}})
           res_k
  = do  { _ <- tcHsContext ctxt
        ; checkNoErrs $ mapM_ (wrapLocM_ kcConDecl) cons
          -- See Note [Failing early in kcDataDefn]
        ; exp_res_kind <- case mb_kind of
            Nothing -> return liftedTypeKind
            Just k  -> tcLHsKindSig (DataKindCtxt (unLoc fam_name)) k

          -- The expected type might have a forall at the type. Normally, we
          -- can't skolemise in kinds because we don't have type-level lambda.
          -- But here, we're at the top-level of an instance declaration, so
          -- we actually have a place to put the regeneralised variables.
          -- Thus: skolemise away. cf. Inst.deeplySkolemise and TcUnify.tcSkolemise
          -- Examples in indexed-types/should_compile/T12369
        ; let (tvs_to_skolemise, inner_res_kind) = tcSplitForAllTys exp_res_kind

        ; (skol_subst, tvs') <- tcInstSkolTyVars tvs_to_skolemise

        ; let inner_res_kind' = substTyAddInScope skol_subst inner_res_kind
              tv_prs          = zip (map tyVarName tvs_to_skolemise) tvs'
              skol_info       = SigSkol (InstDeclCtxt False) exp_res_kind tv_prs

        ; (ev_binds, (_, new_args, co))
            <- solveEqualities $
               checkConstraints skol_info tvs' [] $
               checkExpectedKindX mb_kind_env pp_fam_app
                                  bogus_ty res_k inner_res_kind'

        ; let Pair lhs_ki rhs_ki = tcCoercionKind co

        ; when debugIsOn $
          do { (_, ev_binds) <- initZonkEnv zonkTcEvBinds ev_binds
             ; MASSERT( isEmptyTcEvBinds ev_binds )
             ; lhs_ki <- zonkTcType lhs_ki
             ; rhs_ki <- zonkTcType rhs_ki
             ; MASSERT( lhs_ki `tcEqType` rhs_ki ) }

        ; return (tvs', new_args, lhs_ki) }
  where
    bogus_ty   = pprPanic "kcDataDefn" (ppr fam_name <+> ppr pats)
    pp_fam_app = pprFamInstLHS fam_name mb_bndrs pats fixity (unLoc ctxt) mb_kind
kcDataDefn _ (DataFamInstDecl (XHsImplicitBndrs _)) _
  = panic "kcDataDefn"
kcDataDefn _ (DataFamInstDecl (HsIB _ (FamEqn _ _ _ _ _ (XHsDataDefn _)))) _
  = panic "kcDataDefn"
kcDataDefn _ (DataFamInstDecl (HsIB _ (XFamEqn _))) _
  = panic "kcDataDefn"

{-
Kind check type patterns and kind annotate the embedded type variables.
     type instance F [a] = rhs

 * Here we check that a type instance matches its kind signature, but we do
   not check whether there is a pattern for each type index; the latter
   check is only required for type synonym instances.

Note [Instantiating a family tycon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's possible that kind-checking the result of a family tycon applied to
its patterns will instantiate the tycon further. For example, we might
have

  type family F :: k where
    F = Int
    F = Maybe

After checking (F :: forall k. k) (with no visible patterns), we still need
to instantiate the k. With data family instances, this problem can be even
more intricate, due to Note [Arity of data families] in FamInstEnv. See
indexed-types/should_compile/T12369 for an example.

So, the kind-checker must return the new skolems and args (that is, Type
or (Type -> Type) for the equations above) and the instantiated kind.

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
kcFamTyPats :: TcTyCon
            -> [Name]
            -> Maybe [LHsTyVarBndr GhcRn]
            -> HsTyPats GhcRn
            -> (TcKind -> TcM ())
            -> TcM ()
kcFamTyPats tc_fam_tc imp_vars mb_expl_bndrs arg_pats kind_checker
  = discardResult $
    kcImplicitTKBndrs imp_vars $
    kcExplicitTKBndrs (fromMaybe [] mb_expl_bndrs) $
    do { let name     = tyConName tc_fam_tc
             loc      = nameSrcSpan name
             lhs_fun  = cL loc (HsTyVar noExt NotPromoted (cL loc name))
                        -- lhs_fun is for error messages only
             no_fun   = pprPanic "kcFamTyPats" (ppr name)
             fun_kind = tyConKind tc_fam_tc

       ; (_, _, res_kind_out) <- tcInferApps typeLevelMode Nothing lhs_fun no_fun
                                             fun_kind arg_pats
       ; traceTc "kcFamTyPats" (vcat [ ppr tc_fam_tc, ppr arg_pats, ppr res_kind_out ])
       ; kind_checker res_kind_out }

tcFamTyPats :: TyCon
            -> Maybe ClsInstInfo
            -> [Name]          -- Implicitly bound kind/type variable names
            -> Maybe [LHsTyVarBndr GhcRn]
            -> HsTyPats GhcRn  -- Type patterns
            -> (TcKind -> TcM ([TcTyVar], [TcType], TcKind))
                -- kind-checker for RHS
                -- See Note [Instantiating a family tycon]
            -> (   [TcTyVar]         -- Kind and type variables
                -> [TcType]          -- Kind and type arguments
                -> TcKind
                -> TcM a)            -- NB: You can use solveEqualities here.
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
tcFamTyPats fam_tc mb_clsinfo
            imp_vars mb_expl_bndrs arg_pats kind_checker thing_inside
  = do { -- First, check the arity.
         -- If we wait until validity checking, we'll get kind
         -- errors below when an arity error will be much easier to
         -- understand.
         let should_check_arity
               | DataFamilyFlavour _ <- flav = False
                  -- why not check data families? See [Arity of data families] in FamInstEnv
               | otherwise                   = True

       ; when should_check_arity $
         checkTc (arg_pats `lengthIs` vis_arity) $
         wrongNumberOfParmsErr vis_arity
                      -- report only explicit arguments

       ; (imp_tvs, (exp_tvs, (typats, (more_tyvars, more_typats, res_kind))))
            <- solveEqualities $  -- See Note [Constraints in patterns]
               tcImplicitQTKBndrs FamInstSkol imp_vars $
               tcExplicitTKBndrs FamInstSkol (fromMaybe [] mb_expl_bndrs) $
               do { let loc = nameSrcSpan fam_name
                        lhs_fun = cL loc (HsTyVar noExt NotPromoted
                                            (cL loc fam_name))
                        fun_ty = mkTyConApp fam_tc []
                        fun_kind = tyConKind fam_tc
                        mb_kind_env = thdOf3 <$> mb_clsinfo

                  ; (_, args, res_kind_out)
                      <- tcInferApps typeLevelMode mb_kind_env
                                     lhs_fun fun_ty fun_kind arg_pats

                  ; traceTc "tcFamTyPats 1" (vcat [ ppr fam_tc
                                                  , ppr arg_pats
                                                  , ppr res_kind_out ])

                  ; stuff <- kind_checker res_kind_out
                  ; return (args, stuff) }

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
       ; let all_pats  = typats `chkAppend` more_typats
             fam_app   = mkTyConApp fam_tc all_pats

             user_tvs  = exp_tvs ++ imp_tvs `chkAppend` more_tyvars

                -- the user_tvs might have quantified kind variables from
                -- an enclosing class/instance; make sure to bring these into scope
             extra_tvs = case mb_clsinfo of
               Nothing -> []
               Just (_, inst_tvs, _) ->
                 filter (`elemVarSet` tyCoVarsOfType (mkSpecForAllTys user_tvs fam_app))
                        inst_tvs

             all_tvs = extra_tvs ++ user_tvs

          -- the user_tvs are already bound in the pats; don't quantify over these again.
       ; vars  <- candidateQTyVarsOfType emptyVarSet $
                  mkSpecForAllTys all_tvs fam_app
       ; qtkvs <- quantifyTyVars emptyVarSet vars
       ; let all_qtkvs = qtkvs ++ all_tvs

       ; when debugIsOn $
         do { all_pats <- mapM zonkTcType all_pats
            ; MASSERT2( isEmptyVarSet $ coVarsOfTypes all_pats, ppr all_pats ) }
           -- This should be the case, because otherwise the solveEqualities
           -- above would fail. TODO (RAE): Update once the solveEqualities
           -- bit is cleverer.

       ; traceTc "tcFamTyPats" (ppr (getName fam_tc)
                                $$ ppr mb_expl_bndrs
                                $$ ppr all_pats $$ ppr qtkvs)

           -- See Note [Free-floating kind vars] in TcHsType
       ; lhs_tvs <- zonkTcTypeAndFV fam_app
       ; let unmentioned_tvs   = filterOut (`elemDVarSet` lhs_tvs) imp_tvs
                                   -- If there are tyvars left over, we can
                                   -- assume they're free-floating, since they
                                   -- aren't bound by a type pattern
                                   -- Recall that user are those lexically
                                   -- used in the equation. As skolems, they
                                   -- don't need zonking.
       ; checkNoErrs $ reportFloatingKvs fam_name flav
                                         (dVarSetElemsWellScoped lhs_tvs) unmentioned_tvs

            -- Error if exp_tvs contains anything that is still unused.
            -- See Note [Unused explicitly bound variables in a family pattern]
       ; let unmentioned_exp_tvs = filterOut (`elemDVarSet` lhs_tvs) exp_tvs
       ; checkNoErrs $ mapM_ (unusedExplicitForAllErr . Var.varName) unmentioned_exp_tvs

       ; scopeTyVars FamInstSkol all_qtkvs $
            -- Extend envt with TcTyVars not TyVars, because the
            -- kind checking etc done by thing_inside does not expect
            -- to encounter TyVars; it expects TcTyVars
         thing_inside all_qtkvs all_pats res_kind }
  where
    fam_name  = tyConName fam_tc
    flav      = tyConFlavour fam_tc
    vis_arity = length (tyConVisibleTyVars fam_tc)

unusedExplicitForAllErr :: Name -> RnM ()
unusedExplicitForAllErr n = addErrAt (nameSrcSpan n) $
  text "Explicitly quantified but not used in LHS pattern: type variable"
  <+> quotes (ppr n)

{-

Note [Unused explicitly bound variables in a family pattern]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Why is 'unusedExplicitForAllErr' not just a warning?

Consider the following examples:

  type instance F a = Maybe b
  type instance forall b. F a = Bool
  type instance forall b. F a = Maybe b

In every case, b is a type variable not determined by the LHS pattern. The
first is caught by the renamer, but we catch the last two here. Perhaps one
could argue that the second should be accepted, albeit with a warning, but
consider the fact that in a type family instance, there is no way to interact
with such a varable. At least with @x :: forall a. Int@ we can use visibile
type application, like @x \@Bool 1@. (Of course it does nothing, but it is
permissible.) In the type family case, the only sensible explanation is that
the user has made a mistake -- thus we throw an error.


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

dataDeclChecks :: Name -> NewOrData -> ThetaType -> [LConDecl GhcRn] -> TcM Bool
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
consUseGadtSyntax ((dL->L _ (ConDeclGADT {})) : _) = True
consUseGadtSyntax _                                = False
                 -- All constructors have same shape

-----------------------------------
tcConDecls :: KnotTied TyCon -> [KnotTied TyConBinder] -> KnotTied Type
           -> [LConDecl GhcRn] -> TcM [DataCon]
  -- Why both the tycon tyvars and binders? Because the tyvars
  -- have all the names and the binders have the visibilities.
tcConDecls rep_tycon tmpl_bndrs res_tmpl
  = concatMapM $ addLocM $
    tcConDecl rep_tycon (mkTyConTagMap rep_tycon) tmpl_bndrs res_tmpl
    -- It's important that we pay for tag allocation here, once per TyCon,
    -- See Note [Constructor tag allocation], fixes #14657

tcConDecl :: KnotTied TyCon          -- Representation tycon. Knot-tied!
          -> NameEnv ConTag
          -> [KnotTied TyConBinder] -> KnotTied Type
                 -- Return type template (with its template tyvars)
                 --    (tvs, T tys), where T is the family TyCon
          -> ConDecl GhcRn
          -> TcM [DataCon]

tcConDecl rep_tycon tag_map tmpl_bndrs res_tmpl
          (ConDeclH98 { con_name = name
                      , con_ex_tvs = explicit_tkv_nms
                      , con_mb_cxt = hs_ctxt
                      , con_args = hs_args })
  = addErrCtxt (dataConCtxtName [name]) $
    do { -- Get hold of the existential type variables
         -- e.g. data T a = forall k (b::k) f. MkT a (f b)
         -- Here tmpl_bndrs = {a}
         --      hs_qvars = HsQTvs { hsq_implicit = {k}
         --                        , hsq_explicit = {f,b} }

       ; traceTc "tcConDecl 1" (vcat [ ppr name, ppr explicit_tkv_nms ])

       ; (exp_tvs, (ctxt, arg_tys, field_lbls, stricts))
           <- solveEqualities $
              tcExplicitTKBndrs skol_info explicit_tkv_nms $
              do { ctxt <- tcHsMbContext hs_ctxt
                 ; btys <- tcConArgs hs_args
                 ; field_lbls <- lookupConstructorFields (unLoc name)
                 ; let (arg_tys, stricts) = unzip btys
                 ; return (ctxt, arg_tys, field_lbls, stricts)
                 }

         -- exp_tvs have explicit, user-written binding sites
         -- the kvs below are those kind variables entirely unmentioned by the user
         --   and discovered only by generalization

       ; kvs <- quantifyConDecl (mkVarSet (binderVars tmpl_bndrs))
                                (mkSpecForAllTys exp_tvs $
                                 mkFunTys ctxt $
                                 mkFunTys arg_tys $
                                 unitTy)
                 -- That type is a lie, of course. (It shouldn't end in ()!)
                 -- And we could construct a proper result type from the info
                 -- at hand. But the result would mention only the tmpl_tvs,
                 -- and so it just creates more work to do it right. Really,
                 -- we're doing this to get the right behavior around removing
                 -- any vars bound in exp_binders.

             -- Zonk to Types
       ; (ze, qkvs)      <- zonkTyBndrs kvs
       ; (ze, user_qtvs) <- zonkTyBndrsX ze exp_tvs
       ; arg_tys         <- zonkTcTypesToTypesX ze arg_tys
       ; ctxt            <- zonkTcTypesToTypesX ze ctxt

       ; fam_envs <- tcGetFamInstEnvs

       -- Can't print univ_tvs, arg_tys etc, because we are inside the knot here
       ; traceTc "tcConDecl 2" (ppr name $$ ppr field_lbls)
       ; let
           univ_tvbs = tyConTyVarBinders tmpl_bndrs
           univ_tvs  = binderVars univ_tvbs
           ex_tvbs   = mkTyVarBinders Inferred qkvs ++
                       mkTyVarBinders Specified user_qtvs
           ex_tvs    = qkvs ++ user_qtvs
           -- For H98 datatypes, the user-written tyvar binders are precisely
           -- the universals followed by the existentials.
           -- See Note [DataCon user type variable binders] in DataCon.
           user_tvbs = univ_tvbs ++ ex_tvbs
           buildOneDataCon (dL->L _ name) = do
             { is_infix <- tcConIsInfixH98 name hs_args
             ; rep_nm   <- newTyConRepName name

             ; buildDataCon fam_envs name is_infix rep_nm
                            stricts Nothing field_lbls
                            univ_tvs ex_tvs user_tvbs
                            [{- no eq_preds -}] ctxt arg_tys
                            res_tmpl rep_tycon tag_map
                  -- NB:  we put data_tc, the type constructor gotten from the
                  --      constructor type signature into the data constructor;
                  --      that way checkValidDataCon can complain if it's wrong.
             }
       ; traceTc "tcConDecl 2" (ppr name)
       ; mapM buildOneDataCon [name]
       }
  where
    skol_info = SigTypeSkol (ConArgCtxt (unLoc name))

tcConDecl rep_tycon tag_map tmpl_bndrs res_tmpl
          (ConDeclGADT { con_names = names
                       , con_qvars = qtvs
                       , con_mb_cxt = cxt, con_args = hs_args
                       , con_res_ty = res_ty })
  | HsQTvs { hsq_ext = HsQTvsRn { hsq_implicit = implicit_tkv_nms }
           , hsq_explicit = explicit_tkv_nms } <- qtvs
  = addErrCtxt (dataConCtxtName names) $
    do { traceTc "tcConDecl 1" (ppr names)
       ; let ((dL->L _ name) : _) = names
             skol_info            = DataConSkol name

       ; (imp_tvs, (exp_tvs, (ctxt, arg_tys, res_ty, field_lbls, stricts)))
           <- failIfEmitsConstraints $  -- we won't get another crack, and we don't
                                        -- want an error cascade
              tcImplicitTKBndrs skol_info implicit_tkv_nms $
              tcExplicitTKBndrs skol_info explicit_tkv_nms $
              do { ctxt <- tcHsMbContext cxt
                 ; btys <- tcConArgs hs_args
                 ; res_ty' <- tcHsLiftedType res_ty
                 ; field_lbls <- lookupConstructorFields name
                 ; let (arg_tys, stricts) = unzip btys
                 ; return (ctxt, arg_tys, res_ty', field_lbls, stricts)
                 }
       ; let user_tvs = imp_tvs ++ exp_tvs

       ; tkvs <- quantifyConDecl emptyVarSet (mkSpecForAllTys user_tvs $
                                              mkFunTys ctxt $
                                              mkFunTys arg_tys $
                                              res_ty)

             -- Zonk to Types
       ; (ze, tkvs)     <- zonkTyBndrs tkvs
       ; (ze, user_tvs) <- zonkTyBndrsX ze user_tvs
       ; arg_tys <- zonkTcTypesToTypesX ze arg_tys
       ; ctxt    <- zonkTcTypesToTypesX ze ctxt
       ; res_ty  <- zonkTcTypeToTypeX   ze res_ty

       ; let (univ_tvs, ex_tvs, tkvs', user_tvs', eq_preds, arg_subst)
               = rejigConRes tmpl_bndrs res_tmpl tkvs user_tvs res_ty
             -- NB: this is a /lazy/ binding, so we pass six thunks to
             --     buildDataCon without yet forcing the guards in rejigConRes
             -- See Note [Checking GADT return types]

             -- Compute the user-written tyvar binders. These have the same
             -- tyvars as univ_tvs/ex_tvs, but perhaps in a different order.
             -- See Note [DataCon user type variable binders] in DataCon.
             tkv_bndrs      = mkTyVarBinders Inferred  tkvs'
             user_tv_bndrs  = mkTyVarBinders Specified user_tvs'
             all_user_bndrs = tkv_bndrs ++ user_tv_bndrs

             ctxt'      = substTys arg_subst ctxt
             arg_tys'   = substTys arg_subst arg_tys
             res_ty'    = substTy  arg_subst res_ty


       ; fam_envs <- tcGetFamInstEnvs

       -- Can't print univ_tvs, arg_tys etc, because we are inside the knot here
       ; traceTc "tcConDecl 2" (ppr names $$ ppr field_lbls)
       ; let
           buildOneDataCon (dL->L _ name) = do
             { is_infix <- tcConIsInfixGADT name hs_args
             ; rep_nm   <- newTyConRepName name

             ; buildDataCon fam_envs name is_infix
                            rep_nm
                            stricts Nothing field_lbls
                            univ_tvs ex_tvs all_user_bndrs eq_preds
                            ctxt' arg_tys' res_ty' rep_tycon tag_map
                  -- NB:  we put data_tc, the type constructor gotten from the
                  --      constructor type signature into the data constructor;
                  --      that way checkValidDataCon can complain if it's wrong.
             }
       ; traceTc "tcConDecl 2" (ppr names)
       ; mapM buildOneDataCon names
       }
tcConDecl _ _ _ _ (ConDeclGADT _ _ _ (XLHsQTyVars _) _ _ _ _)
  = panic "tcConDecl"
tcConDecl _ _ _ _ (XConDecl _) = panic "tcConDecl"

-- | Produce the telescope of kind variables that this datacon is
-- implicitly quantified over. Incoming type need not be zonked.
quantifyConDecl :: TcTyCoVarSet  -- outer tvs, not to be quantified over; zonked
                -> TcType -> TcM [TcTyVar]
quantifyConDecl gbl_tvs ty
  = do { ty <- zonkTcType ty
       ; fvs <- candidateQTyVarsOfType gbl_tvs ty
       ; quantifyTyVars gbl_tvs fvs }

tcConIsInfixH98 :: Name
             -> HsConDetails (LHsType GhcRn) (Located [LConDeclField GhcRn])
             -> TcM Bool
tcConIsInfixH98 _   details
  = case details of
           InfixCon {}  -> return True
           _            -> return False

tcConIsInfixGADT :: Name
             -> HsConDetails (LHsType GhcRn) (Located [LConDeclField GhcRn])
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

tcConArgs :: HsConDeclDetails GhcRn
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
    combined = map (\(dL->L _ f) -> (cd_fld_names f,cd_fld_type f))
                   (unLoc fields)
    explode (ns,ty) = zip ns (repeat ty)
    exploded = concatMap explode combined
    (_,btys) = unzip exploded


tcConArg :: LHsType GhcRn -> TcM (TcType, HsSrcBang)
tcConArg bty
  = do  { traceTc "tcConArg 1" (ppr bty)
        ; arg_ty <- tcHsOpenType (getBangType bty)
             -- Newtypes can't have unboxed types, but we check
             -- that in checkValidDataCon; this tcConArg stuff
             -- doesn't happen for GADT-style declarations
        ; traceTc "tcConArg 2" (ppr bty)
        ; return (arg_ty, getBangStrictness bty) }

{-
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
6-tuple from rejigConRes (so that we can compute the return type from it, which
checkValidDataCon needs), but the first three fields may be bogus if
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

rejigConRes :: [KnotTied TyConBinder] -> KnotTied Type    -- Template for result type; e.g.
                                  -- data instance T [a] b c ...
                                  --      gives template ([a,b,c], T [a] b c)
                                  -- Type must be of kind *!
            -> [TyVar]            -- The constructor's inferred type variables
            -> [TyVar]            -- The constructor's user-written, specified
                                  -- type variables
            -> KnotTied Type      -- res_ty type must be of kind *
            -> ([TyVar],          -- Universal
                [TyVar],          -- Existential (distinct OccNames from univs)
                [TyVar],          -- The constructor's rejigged, user-written,
                                  -- inferred type variables
                [TyVar],          -- The constructor's rejigged, user-written,
                                  -- specified type variables
                [EqSpec],      -- Equality predicates
                TCvSubst)      -- Substitution to apply to argument types
        -- We don't check that the TyCon given in the ResTy is
        -- the same as the parent tycon, because checkValidDataCon will do it
-- NB: All arguments may potentially be knot-tied
rejigConRes tmpl_bndrs res_tmpl dc_inferred_tvs dc_specified_tvs res_ty
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
        -- The user-written type variables are what is listed in the forall:
        --   [x, y, z] (all specified). We must rejig these as well.
        --   See Note [DataCon user type variable binders] in DataCon.
        -- So we return ( [a,b,z], [x,y]
        --              , [], [x,y,z]
        --              , [a~(x,y),b~z], <arg-subst> )
  | Just subst <- ASSERT( isLiftedTypeKind (typeKind res_ty) )
                  ASSERT( isLiftedTypeKind (typeKind res_tmpl) )
                  tcMatchTy res_tmpl res_ty
  = let (univ_tvs, raw_eqs, kind_subst) = mkGADTVars tmpl_tvs dc_tvs subst
        raw_ex_tvs = dc_tvs `minusList` univ_tvs
        (arg_subst, substed_ex_tvs) = substTyVarBndrs kind_subst raw_ex_tvs

        -- After rejigging the existential tyvars, the resulting substitution
        -- gives us exactly what we need to rejig the user-written tyvars,
        -- since the dcUserTyVarBinders invariant guarantees that the
        -- substitution has *all* the tyvars in its domain.
        -- See Note [DataCon user type variable binders] in DataCon.
        subst_user_tvs = map (getTyVar "rejigConRes" . substTyVar arg_subst)
        substed_inferred_tvs  = subst_user_tvs dc_inferred_tvs
        substed_specified_tvs = subst_user_tvs dc_specified_tvs

        substed_eqs = map (substEqSpec arg_subst) raw_eqs
    in
    (univ_tvs, substed_ex_tvs, substed_inferred_tvs, substed_specified_tvs,
     substed_eqs, arg_subst)

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
  = (tmpl_tvs, dc_tvs `minusList` tmpl_tvs, dc_inferred_tvs, dc_specified_tvs,
     [], emptyTCvSubst)
  where
    dc_tvs   = dc_inferred_tvs ++ dc_specified_tvs
    tmpl_tvs = binderVars tmpl_bndrs

{- Note [mkGADTVars]
~~~~~~~~~~~~~~~~~~~~
Running example:

data T (k1 :: *) (k2 :: *) (a :: k2) (b :: k2) where
  MkT :: forall (x1 : *) (y :: x1) (z :: *).
         T x1 * (Proxy (y :: x1), z) z

We need the rejigged type to be

  MkT :: forall (x1 :: *) (k2 :: *) (a :: k2) (b :: k2).
         forall (y :: x1) (z :: *).
         (k2 ~ *, a ~ (Proxy x1 y, z), b ~ z)
      => T x1 k2 a b

You might naively expect that z should become a universal tyvar,
not an existential. (After all, x1 becomes a universal tyvar.)
But z has kind * while b has kind k2, so the return type
   T x1 k2 a z
is ill-kinded.  Another way to say it is this: the universal
tyvars must have exactly the same kinds as the tyConTyVars.

So we need an existential tyvar and a heterogeneous equality
constraint. (The b ~ z is a bit redundant with the k2 ~ * that
comes before in that b ~ z implies k2 ~ *. I'm sure we could do
some analysis that could eliminate k2 ~ *. But we don't do this
yet.)

The data con signature has already been fully kind-checked.
The return type

  T x1 * (Proxy (y :: x1), z) z
becomes
  qtkvs    = [x1 :: *, y :: x1, z :: *]
  res_tmpl = T x1 * (Proxy x1 y, z) z

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

`choose` looks up each tycon tyvar in the matching (it *must* be matched!).

* If it finds a bare result tyvar (the first branch of the `case`
  statement), it checks to make sure that the result tyvar isn't yet
  in the list of univ_tvs.  If it is in that list, then we have a
  repeated variable in the return type, and we in fact need a GADT
  equality.

* It then checks to make sure that the kind of the result tyvar
  matches the kind of the template tyvar. This check is what forces
  `z` to be existential, as it should be, explained above.

* Assuming no repeated variables or kind-changing, we wish to use the
  variable name given in the datacon signature (that is, `x1` not
  `k1`), not the tycon signature (which may have been made up by
  GHC). So, we add a mapping from the tycon tyvar to the result tyvar
  to t_sub.

* If we discover that a mapping in `subst` gives us a non-tyvar (the
  second branch of the `case` statement), then we have a GADT equality
  to create.  We create a fresh equality, but we don't extend any
  substitutions. The template variable substitution is meant for use
  in universal tyvar kinds, and these shouldn't be affected by any
  GADT equalities.

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
           -> TCvSubst          -- template substitution
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

               -- Not a simple substitution: make an equality predicate
          _ -> choose (t_tv':univs) (mkEqSpec t_tv' r_ty : eqs)
                      (extendTvSubst t_sub t_tv (mkTyVarTy t_tv'))
                         -- We've updated the kind of t_tv,
                         -- so add it to t_sub (Trac #14162)
                      r_sub t_tvs
            where
              t_tv' = updateTyVarKind (substTy t_sub) t_tv

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

checkValidTyCl :: TyCon -> TcM [TyCon]
-- The returned list is either a singleton (if valid)
-- or a list of "fake tycons" (if not); the fake tycons
-- include any implicits, like promoted data constructors
-- See Note [Recover from validity error]
checkValidTyCl tc
  = setSrcSpan (getSrcSpan tc) $
    addTyConCtxt tc $
    recoverM recovery_code
             (do { traceTc "Starting validity for tycon" (ppr tc)
                 ; checkValidTyCon tc
                 ; traceTc "Done validity for tycon" (ppr tc)
                 ; return [tc] })
  where
    recovery_code -- See Note [Recover from validity error]
      = do { traceTc "Aborted validity for tycon" (ppr tc)
           ; return (concatMap mk_fake_tc $
                     ATyCon tc : implicitTyConThings tc) }

    mk_fake_tc (ATyCon tc)
      | isClassTyCon tc = [tc]   -- Ugh! Note [Recover from validity error]
      | otherwise       = [makeRecoveryTyCon tc]
    mk_fake_tc (AConLike (RealDataCon dc))
                        = [makeRecoveryTyCon (promoteDataCon dc)]
    mk_fake_tc _        = []

{- Note [Recover from validity error]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We recover from a validity error in a type or class, which allows us
to report multiple validity errors. In the failure case we return a
TyCon of the right kind, but with no interesting behaviour
(makeRecoveryTyCon). Why?  Suppose we have
   type T a = Fun
where Fun is a type family of arity 1.  The RHS is invalid, but we
want to go on checking validity of subsequent type declarations.
So we replace T with an abstract TyCon which will do no harm.
See indexed-types/should_fail/BadSock and Trac #10896

Some notes:

* We must make fakes for promoted DataCons too. Consider (Trac #15215)
      data T a = MkT ...
      data S a = ...T...MkT....
  If there is an error in the definition of 'T' we add a "fake type
  constructor" to the type environment, so that we can continue to
  typecheck 'S'.  But we /were not/ adding a fake anything for 'MkT'
  and so there was an internal error when we met 'MkT' in the body of
  'S'.

* Painfully, we *don't* want to do this for classes.
  Consider tcfail041:
     class (?x::Int) => C a where ...
     instance C Int
  The class is invalid because of the superclass constraint.  But
  we still want it to look like a /class/, else the instance bleats
  that the instance is mal-formed because it hasn't got a class in
  the head.

  This is really bogus; now we have in scope a Class that is invalid
  in some way, with unknown downstream consequences.  A better
  alterantive might be to make a fake class TyCon.  A job for another day.
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
               ; mapM_ (checkPartialRecordField data_cons) (tyConFieldLabels tc)

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
    check_fields ((label, con1) :| other_fields)
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

        checkOne (_, con2)    -- Do it both ways to ensure they are structurally identical
            = do { checkFieldCompat lbl con1 con2 res1 res2 fty1 fty2
                 ; checkFieldCompat lbl con2 con1 res2 res1 fty2 fty1 }
            where
                (_, _, _, res2) = dataConSig con2
                fty2 = dataConFieldType con2 lbl

checkPartialRecordField :: [DataCon] -> FieldLabel -> TcM ()
-- Checks the partial record field selector, and warns.
-- See Note [Checking partial record field]
checkPartialRecordField all_cons fld
  = setSrcSpan loc $
      warnIfFlag Opt_WarnPartialFields
        (not is_exhaustive && not (startsWithUnderscore occ_name))
        (sep [text "Use of partial record field selector" <> colon,
              nest 2 $ quotes (ppr occ_name)])
  where
    sel_name = flSelector fld
    loc    = getSrcSpan sel_name
    occ_name = getOccName sel_name

    (cons_with_field, cons_without_field) = partition has_field all_cons
    has_field con = fld `elem` (dataConFieldLabels con)
    is_exhaustive = all (dataConCannotMatch inst_tys) cons_without_field

    con1 = ASSERT( not (null cons_with_field) ) head cons_with_field
    (univ_tvs, _, eq_spec, _, _, _) = dataConFullSig con1
    eq_subst = mkTvSubstPrs (map eqSpecPair eq_spec)
    inst_tys = substTyVars eq_subst univ_tvs

checkFieldCompat :: FieldLabelString -> DataCon -> DataCon
                 -> Type -> Type -> Type -> Type -> TcM ()
checkFieldCompat fld con1 con2 res1 res2 fty1 fty2
  = do  { checkTc (isJust mb_subst1) (resultTypeMisMatch fld con1 con2)
        ; checkTc (isJust mb_subst2) (fieldTypeMisMatch fld con1 con2) }
  where
    mb_subst1 = tcMatchTy res1 res2
    mb_subst2 = tcMatchTyX (expectJust "checkFieldCompat" mb_subst1) fty1 fty2

-------------------------------
checkValidDataCon :: DynFlags -> Bool -> TyCon -> DataCon -> TcM ()
checkValidDataCon dflags existential_ok tc con
  = setSrcSpan (getSrcSpan con)  $
    addErrCtxt (dataConCtxt con) $
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

          -- Check the dcUserTyVarBinders invariant
          -- See Note [DataCon user type variable binders] in DataCon
          -- checked here because we sometimes build invalid DataCons before
          -- erroring above here
        ; when debugIsOn $
          do { let (univs, exs, eq_spec, _, _, _) = dataConFullSig con
                   user_tvs                       = dataConUserTyVars con
                   user_tvbs_invariant
                     =    Set.fromList (filterEqSpec eq_spec univs ++ exs)
                       == Set.fromList user_tvs
             ; MASSERT2( user_tvbs_invariant
                       , vcat ([ ppr con
                               , ppr univs
                               , ppr exs
                               , ppr eq_spec
                               , ppr user_tvs ])) }

        ; traceTc "Done validity of data con" $
          vcat [ ppr con
               , text "Datacon user type:" <+> ppr (dataConUserType con)
               , text "Datacon rep type:" <+> ppr (dataConRepType con)
               , text "Rep typcon binders:" <+> ppr (tyConBinders (dataConTyCon con))
               , case tyConFamInst_maybe (dataConTyCon con) of
                   Nothing -> text "not family"
                   Just (f, _) -> ppr (tyConBinders f) ]
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
      -- If not optimising, we don't unpack (rep_bang is never
      -- HsUnpack), so don't complain!  This happens, e.g., in Haddock.
      -- See dataConSrcToImplBang.
      , not (gopt Opt_OmitInterfacePragmas dflags)
      -- When typechecking an indefinite package in Backpack, we
      -- may attempt to UNPACK an abstract type.  The test here will
      -- conclude that this is unusable, but it might become usable
      -- when we actually fill in the abstract type.  As such, don't
      -- warn in this case (it gives users the wrong idea about whether
      -- or not UNPACK on abstract types is supported; it is!)
      , unitIdIsDefinite (thisPackage dflags)
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
    cls_arity = length (tyConVisibleTyVars (classTyCon cls))
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
          -- See Note [Splitting nested sigma types in class type signatures]
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
                                fam_tvs [] (mkTyVarTys fam_tvs) rhs pp_lhs loc }
        where
          fam_tvs = tyConTyVars fam_tc
          pp_lhs  = ppr (mkTyConApp fam_tc (mkTyVarTys fam_tvs))

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
              -- See also
              -- Note [Splitting nested sigma types in class type signatures]
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
                 2 (text "Enable TypeFamilies to allow indexed type families")

{- Note [Class method constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Haskell 2010 is supposed to reject
  class C a where
    op :: Eq a => a -> a
where the method type constrains only the class variable(s).  (The extension
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
See Note [Splitting nested sigma types in class type signatures].

Note [Splitting nested sigma types in class type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Note [Checking partial record field]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This check checks the partial record field selector, and warns (Trac #7169).

For example:

  data T a = A { m1 :: a, m2 :: a } | B { m1 :: a }

The function 'm2' is partial record field, and will fail when it is applied to
'B'. The warning identifies such partial fields. The check is performed at the
declaration of T, not at the call-sites of m2.

The warning can be suppressed by prefixing the field-name with an underscore.
For example:

  data T a = A { m1 :: a, _m2 :: a } | B { m1 :: a }

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
    roles                  = tyConRoles tc
    (vis_roles, vis_vars)  = unzip $ mapMaybe pick_vis $
                             zip roles (tyConBinders tc)
    role_annot_decl_maybe  = lookupRoleAnnot role_annots name

    pick_vis :: (Role, TyConBinder) -> Maybe (Role, TyVar)
    pick_vis (role, tvb)
      | isVisibleTyConBinder tvb = Just (role, binderVar tvb)
      | otherwise                = Nothing

    check_roles
      = whenIsJust role_annot_decl_maybe $
          \decl@(dL->L loc (RoleAnnotDecl _ _ the_role_annots)) ->
          addRoleAnnotCtxt name $
          setSrcSpan loc $ do
          { role_annots_ok <- xoptM LangExt.RoleAnnotations
          ; checkTc role_annots_ok $ needXRoleAnnotations tc
          ; checkTc (vis_vars `equalLength` the_role_annots)
                    (wrongNumberOfRoles vis_vars decl)
          ; _ <- zipWith3M checkRoleAnnot vis_vars the_role_annots vis_roles
          -- Representational or phantom roles for class parameters
          -- quickly lead to incoherence. So, we require
          -- IncoherentInstances to have them. See #8773, #14292
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
checkRoleAnnot _  (dL->L _ Nothing)   _  = return ()
checkRoleAnnot tv (dL->L _ (Just r1)) r2
  = when (r1 /= r2) $
    addErrTc $ badRoleAnnot (tyVarName tv) r1 r2
checkRoleAnnot _ _ _ = panic "checkRoleAnnot: Impossible Match" -- due to #15884

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

    check_ty_roles env role ty
      | Just ty' <- coreView ty -- #14101
      = check_ty_roles env role ty'

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

    check_ty_roles env role (ForAllTy (Bndr tv _) ty)
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

tcAddTyFamInstCtxt :: TyFamInstDecl GhcRn -> TcM a -> TcM a
tcAddTyFamInstCtxt decl
  = tcAddFamInstCtxt (text "type instance") (tyFamInstDeclName decl)

tcMkDataFamInstCtxt :: DataFamInstDecl GhcRn -> SDoc
tcMkDataFamInstCtxt decl@(DataFamInstDecl { dfid_eqn =
                            HsIB { hsib_body = eqn }})
  = tcMkFamInstCtxt (pprDataFamInstFlavour decl <+> text "instance")
                    (unLoc (feqn_tycon eqn))
tcMkDataFamInstCtxt (DataFamInstDecl (XHsImplicitBndrs _))
  = panic "tcMkDataFamInstCtxt"

tcAddDataFamInstCtxt :: DataFamInstDecl GhcRn -> TcM a -> TcM a
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

dataConCtxtName :: [Located Name] -> SDoc
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
              parens (text ("Enable MultiParamTypeClasses to allow "
                                    ++ allowWhat ++ " classes"))]

classFunDepsErr :: Class -> SDoc
classFunDepsErr cls
  = vcat [text "Fundeps in class" <+> quotes (ppr cls),
          parens (text "Enable FunctionalDependencies to allow fundeps")]

badMethPred :: Id -> TcPredType -> SDoc
badMethPred sel_id pred
  = vcat [ hang (text "Constraint" <+> quotes (ppr pred)
                 <+> text "in the type of" <+> quotes (ppr sel_id))
              2 (text "constrains only the class type variables")
         , text "Enable ConstrainedClassMethods to allow it" ]

noClassTyVarErr :: Class -> TyCon -> SDoc
noClassTyVarErr clas fam_tc
  = sep [ text "The associated type" <+> quotes (ppr fam_tc)
        , text "mentions none of the type or kind variables of the class" <+>
                quotes (ppr clas <+> hsep (map ppr (classTyVars clas)))]

badDataConTyCon :: DataCon -> Type -> Type -> SDoc
badDataConTyCon data_con res_ty_tmpl actual_res_ty
  | ASSERT( all isTyVar actual_ex_tvs )
    tcIsForAllTy actual_res_ty
  = nested_foralls_contexts_suggestion
  | isJust (tcSplitPredFunTy_maybe actual_res_ty)
  = nested_foralls_contexts_suggestion
  | otherwise
  = hang (text "Data constructor" <+> quotes (ppr data_con) <+>
                text "returns type" <+> quotes (ppr actual_res_ty))
       2 (text "instead of an instance of its parent type" <+> quotes (ppr res_ty_tmpl))
  where
    -- This suggestion is useful for suggesting how to correct code like what
    -- was reported in Trac #12087:
    --
    --   data F a where
    --     MkF :: Ord a => Eq a => a -> F a
    --
    -- Although nested foralls or contexts are allowed in function type
    -- signatures, it is much more difficult to engineer GADT constructor type
    -- signatures to allow something similar, so we error in the latter case.
    -- Nevertheless, we can at least suggest how a user might reshuffle their
    -- exotic GADT constructor type signature so that GHC will accept.
    nested_foralls_contexts_suggestion =
      text "GADT constructor type signature cannot contain nested"
      <+> quotes forAllLit <> text "s or contexts"
      $+$ hang (text "Suggestion: instead use this type signature:")
             2 (ppr (dataConName data_con) <+> dcolon <+> ppr suggested_ty)

    -- To construct a type that GHC would accept (suggested_ty), we:
    --
    -- 1) Find the existentially quantified type variables and the class
    --    predicates from the datacon. (NB: We don't need the universally
    --    quantified type variables, since rejigConRes won't substitute them in
    --    the result type if it fails, as in this scenario.)
    -- 2) Split apart the return type (which is headed by a forall or a
    --    context) using tcSplitNestedSigmaTys, collecting the type variables
    --    and class predicates we find, as well as the rho type lurking
    --    underneath the nested foralls and contexts.
    -- 3) Smash together the type variables and class predicates from 1) and
    --    2), and prepend them to the rho type from 2).
    actual_ex_tvs = dataConExTyCoVars data_con
    actual_theta  = dataConTheta data_con
    (actual_res_tvs, actual_res_theta, actual_res_rho)
      = tcSplitNestedSigmaTys actual_res_ty
    suggested_ty = mkSpecForAllTys (actual_ex_tvs ++ actual_res_tvs) $
                   mkFunTys (actual_theta ++ actual_res_theta)
                   actual_res_rho

badGadtDecl :: Name -> SDoc
badGadtDecl tc_name
  = vcat [ text "Illegal generalised algebraic data declaration for" <+> quotes (ppr tc_name)
         , nest 2 (parens $ text "Enable the GADTs extension to allow this") ]

badExistential :: DataCon -> SDoc
badExistential con
  = hang (text "Data constructor" <+> quotes (ppr con) <+>
                text "has existential type variables, a context, or a specialised result type")
       2 (vcat [ ppr con <+> dcolon <+> ppr (dataConUserType con)
               , parens $ text "Enable ExistentialQuantification or GADTs to allow this" ])

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

wrongNumberOfRoles :: [a] -> LRoleAnnotDecl GhcRn -> SDoc
wrongNumberOfRoles tyvars d@(dL->L _ (RoleAnnotDecl _ _ annots))
  = hang (text "Wrong number of roles listed in role annotation;" $$
          text "Expected" <+> (ppr $ length tyvars) <> comma <+>
          text "got" <+> (ppr $ length annots) <> colon)
       2 (ppr d)
wrongNumberOfRoles _ (dL->L _ (XRoleAnnotDecl _)) = panic "wrongNumberOfRoles"
wrongNumberOfRoles _ _ = panic "wrongNumberOfRoles: Impossible Match"
                         -- due to #15884


illegalRoleAnnotDecl :: LRoleAnnotDecl GhcRn -> TcM ()
illegalRoleAnnotDecl (dL->L loc (RoleAnnotDecl _ tycon _))
  = setErrCtxt [] $
    setSrcSpan loc $
    addErrTc (text "Illegal role annotation for" <+> ppr tycon <> char ';' $$
              text "they are allowed only for datatypes and classes.")
illegalRoleAnnotDecl (dL->L _ (XRoleAnnotDecl _)) = panic "illegalRoleAnnotDecl"
illegalRoleAnnotDecl _ = panic "illegalRoleAnnotDecl: Impossible Match"
                         -- due to #15884

needXRoleAnnotations :: TyCon -> SDoc
needXRoleAnnotations tc
  = text "Illegal role annotation for" <+> ppr tc <> char ';' $$
    text "did you intend to use RoleAnnotations?"

incoherentRoles :: SDoc
incoherentRoles = (text "Roles other than" <+> quotes (text "nominal") <+>
                   text "for class parameters can lead to incoherence.") $$
                  (text "Use IncoherentInstances to allow this; bad role found")

addTyConCtxt :: TyCon -> TcM a -> TcM a
addTyConCtxt tc = addTyConFlavCtxt name flav
  where
    name = getName tc
    flav = tyConFlavour tc

addRoleAnnotCtxt :: Name -> TcM a -> TcM a
addRoleAnnotCtxt name
  = addErrCtxt $
    text "while checking a role annotation for" <+> quotes (ppr name)
