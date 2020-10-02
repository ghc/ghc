{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998

-}

{-# LANGUAGE CPP, TupleSections, ScopedTypeVariables, MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Typecheck type and class declarations
module GHC.Tc.TyCl (
        tcTyAndClassDecls,

        -- Functions used by GHC.Tc.TyCl.Instance to check
        -- data/type family instance declarations
        kcConDecls, tcConDecls, DataDeclInfo(..),
        dataDeclChecks, checkValidTyCon,
        tcFamTyPats, tcTyFamInstEqn,
        tcAddTyFamInstCtxt, tcMkDataFamInstCtxt, tcAddDataFamInstCtxt,
        unravelFamInstPats, addConsistencyConstraints,
        wrongKindOfFamily
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Session

import GHC.Hs

import GHC.Tc.TyCl.Build
import GHC.Tc.Solver( pushLevelAndSolveEqualities, pushLevelAndSolveEqualitiesX
                    , reportUnsolvedEqualities )
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Unify( unifyType, emitResidualTvConstraint )
import GHC.Tc.Types.Constraint( emptyWC )
import GHC.Tc.Validity
import GHC.Tc.Utils.Zonk
import GHC.Tc.TyCl.Utils
import GHC.Tc.TyCl.Class
import {-# SOURCE #-} GHC.Tc.TyCl.Instance( tcInstDecls1 )
import GHC.Tc.Deriv (DerivInfo(..))
import GHC.Tc.Gen.HsType
import GHC.Tc.Instance.Class( AssocInstInfo(..) )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.TcType
import GHC.Tc.Instance.Family
import GHC.Tc.Types.Origin

import GHC.Builtin.Types (oneDataConTy,  unitTy, makeRecoveryTyCon )

import GHC.Rename.Env( lookupConstructorFields )

import GHC.Core.Multiplicity
import GHC.Core.FamInstEnv
import GHC.Core.Coercion
import GHC.Core.Type
import GHC.Core.TyCo.Rep   -- for checkValidRoles
import GHC.Core.TyCo.Ppr( pprTyVars )
import GHC.Core.Class
import GHC.Core.Coercion.Axiom
import GHC.Core.TyCon
import GHC.Core.DataCon
import GHC.Core.Unify

import GHC.Types.Id
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Types.SrcLoc
import GHC.Types.SourceFile
import GHC.Types.Unique
import GHC.Types.Basic
import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.FastString
import GHC.Data.Maybe
import GHC.Data.List.SetOps

import GHC.Unit

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

import Control.Monad
import Data.Function ( on )
import Data.Functor.Identity
import Data.List
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.Set as Set
import Data.Tuple( swap )

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
be ill-formed (see #7175 and Note [rejigConRes]) we must check
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
                         , [DerivInfo]      -- Deriving info
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
-- See Note [TyClGroups and dependency analysis] in GHC.Hs.Decls
tcTyClGroup (TyClGroup { group_tyclds = tyclds
                       , group_roles  = roles
                       , group_kisigs = kisigs
                       , group_instds = instds })
  = do { let role_annots = mkRoleAnnotEnv roles

           -- Step 1: Typecheck the standalone kind signatures and type/class declarations
       ; traceTc "---- tcTyClGroup ---- {" empty
       ; traceTc "Decls for" (ppr (map (tcdName . unLoc) tyclds))
       ; (tyclss, data_deriv_info) <-
           tcExtendKindEnv (mkPromotionErrorEnv tyclds) $ -- See Note [Type environment evolution]
           do { kisig_env <- mkNameEnv <$> traverse tcStandaloneKindSig kisigs
              ; tcTyClDecls tyclds kisig_env role_annots }

           -- Step 1.5: Make sure we don't have any type synonym cycles
       ; traceTc "Starting synonym cycle check" (ppr tyclss)
       ; home_unit <- hsc_home_unit <$> getTopEnv
       ; checkSynCycles (homeUnitAsUnit home_unit) tyclss tyclds
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
       ; (gbl_env', inst_info, datafam_deriv_info) <-
         setGblEnv gbl_env $
         tcInstDecls1 instds

       ; let deriv_info = datafam_deriv_info ++ data_deriv_info
       ; return (gbl_env', inst_info, deriv_info) }

-- Gives the kind for every TyCon that has a standalone kind signature
type KindSigEnv = NameEnv Kind

tcTyClDecls
  :: [LTyClDecl GhcRn]
  -> KindSigEnv
  -> RoleAnnotEnv
  -> TcM ([TyCon], [DerivInfo])
tcTyClDecls tyclds kisig_env role_annots
  = do {    -- Step 1: kind-check this group and returns the final
            -- (possibly-polymorphic) kind of each TyCon and Class
            -- See Note [Kind checking for type and class decls]
         tc_tycons <- kcTyClGroup kisig_env tyclds
       ; traceTc "tcTyAndCl generalized kinds" (vcat (map ppr_tc_tycon tc_tycons))

            -- Step 2: type-check all groups together, returning
            -- the final TyCons and Classes
            --
            -- NB: We have to be careful here to NOT eagerly unfold
            -- type synonyms, as we have not tested for type synonym
            -- loops yet and could fall into a black hole.
       ; fixM $ \ ~(rec_tyclss, _) -> do
           { tcg_env <- getGblEnv
           ; let roles = inferRoles (tcg_src tcg_env) role_annots rec_tyclss

                 -- Populate environment with knot-tied ATyCon for TyCons
                 -- NB: if the decls mention any ill-staged data cons
                 -- (see Note [Recursion and promoting data constructors])
                 -- we will have failed already in kcTyClGroup, so no worries here
           ; (tycons, data_deriv_infos) <-
             tcExtendRecEnv (zipRecTyClss tc_tycons rec_tyclss) $

                 -- Also extend the local type envt with bindings giving
                 -- a TcTyCon for each knot-tied TyCon or Class
                 -- See Note [Type checking recursive type and class declarations]
                 -- and Note [Type environment evolution]
             tcExtendKindEnvWithTyCons tc_tycons $

                 -- Kind and type check declarations for this group
               mapAndUnzipM (tcTyClDecl roles) tyclds
           ; return (tycons, concat data_deriv_infos)
           } }
  where
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

The downside of not directly reading off the kinds of the RHS of
type synonyms in topological order is that we don't transparently
support making synonyms of types with higher-rank kinds.  But
you can always specify a CUSK directly to make this work out.
See tc269 for an example.

Note [CUSKs and PolyKinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

    data T (a :: *) = MkT (S a)   -- Has CUSK
    data S a = MkS (T Int) (S a)  -- No CUSK

Via inferInitialKinds we get
  T :: * -> *
  S :: kappa -> *

Then we call kcTyClDecl on each decl in the group, to constrain the
kind unification variables.  BUT we /skip/ the RHS of any decl with
a CUSK.  Here we skip the RHS of T, so we eventually get
  S :: forall k. k -> *

This gets us more polymorphism than we would otherwise get, similar
(but implemented strangely differently from) the treatment of type
signatures in value declarations.

However, we only want to do so when we have PolyKinds.
When we have NoPolyKinds, we don't skip those decls, because we have defaulting
(#16609). Skipping won't bring us more polymorphism when we have defaulting.
Consider

  data T1 a = MkT1 T2        -- No CUSK
  data T2 = MkT2 (T1 Maybe)  -- Has CUSK

If we skip the rhs of T2 during kind-checking, the kind of a remains unsolved.
With PolyKinds, we do generalization to get T1 :: forall a. a -> *. And the
program type-checks.
But with NoPolyKinds, we do defaulting to get T1 :: * -> *. Defaulting happens
in quantifyTyVars, which is called from generaliseTcTyCon. Then type-checking
(T1 Maybe) will throw a type error.

Summary: with PolyKinds, we must skip; with NoPolyKinds, we must /not/ skip.

Open type families
~~~~~~~~~~~~~~~~~~
This treatment of type synonyms only applies to Haskell 98-style synonyms.
General type functions can be recursive, and hence, appear in `alg_decls'.

The kind of an open type family is solely determinded by its kind signature;
hence, only kind signatures participate in the construction of the initial
kind environment (as constructed by `inferInitialKind'). In fact, we ignore
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

2.  When checking a type/class declaration (in module GHC.Tc.TyCl), we come
    upon knowledge of the eventual tycon in bits and pieces.

      S1) First, we use inferInitialKinds to look over the user-provided
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
    environment in GHC.Tc.TyCl, until the real full TyCons can be created
    during desugaring. A desugared program should never have a TcTyCon.

3.  In a TcTyCon, everything is zonked after the kind-checking pass (S2).

4.  tyConScopedTyVars.  A challenging piece in all of this is that we
    end up taking three separate passes over every declaration:
      - one in inferInitialKind (this pass look only at the head, not the body)
      - one in kcTyClDecls (to kind-check the body)
      - a final one in tcTyClDecls (to desugar)

    In the latter two passes, we need to connect the user-written type
    variables in an LHsQTyVars with the variables in the tycon's
    inferred kind. Because the tycon might not have a CUSK, this
    matching up is, in general, quite hard to do.  (Look through the
    git history between Dec 2015 and Apr 2016 for
    GHC.Tc.Gen.HsType.splitTelescopeTvs!)

    Instead of trying, we just store the list of type variables to
    bring into scope, in the tyConScopedTyVars field of the TcTyCon.
    These tyvars are brought into scope in GHC.Tc.Gen.HsType.bindTyClTyVars.

    In a TcTyCon, why is tyConScopedTyVars :: [(Name,TcTyVar)] rather
    than just [TcTyVar]?  Consider these mutually-recursive decls
       data T (a :: k1) b = MkT (S a b)
       data S (c :: k2) d = MkS (T c d)
    We start with k1 bound to kappa1, and k2 to kappa2; so initially
    in the (Name,TcTyVar) pairs the Name is that of the TcTyVar. But
    then kappa1 and kappa2 get unified; so after the zonking in
    'generalise' in 'kcTyClGroup' the Name and TcTyVar may differ.

See also Note [Type checking recursive type and class declarations].

Note [Swizzling the tyvars before generaliseTcTyCon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note only applies when /inferring/ the kind of a TyCon.
If there is a separate kind signature, or a CUSK, we take an entirely
different code path.

For inference, consider
   class C (f :: k) x where
      type T f
      op :: D f => blah
   class D (g :: j) y where
      op :: C g => y -> blah

Here C and D are considered mutually recursive.  Neither has a CUSK.
Just before generalisation we have the (un-quantified) kinds
   C :: k1 -> k2 -> Constraint
   T :: k1 -> Type
   D :: k1 -> Type -> Constraint
Notice that f's kind and g's kind have been unified to 'k1'. We say
that k1 is the "representative" of k in C's decl, and of j in D's decl.

Now when quantifying, we'd like to end up with
   C :: forall {k2}. forall k. k -> k2 -> Constraint
   T :: forall k. k -> Type
   D :: forall j. j -> Type -> Constraint

That is, we want to swizzle the representative to have the Name given
by the user. Partly this is to improve error messages and the output of
:info in GHCi.  But it is /also/ important because the code for a
default method may mention the class variable(s), but at that point
(tcClassDecl2), we only have the final class tyvars available.
(Alternatively, we could record the scoped type variables in the
TyCon, but it's a nuisance to do so.)

Notes:

* On the input to generaliseTyClDecl, the mapping between the
  user-specified Name and the representative TyVar is recorded in the
  tyConScopedTyVars of the TcTyCon.  NB: you first need to zonk to see
  this representative TyVar.

* The swizzling is actually performed by swizzleTcTyConBndrs

* We must do the swizzling across the whole class decl. Consider
     class C f where
       type S (f :: k)
       type T f
  Here f's kind k is a parameter of C, and its identity is shared
  with S and T.  So if we swizzle the representative k at all, we
  must do so consistently for the entire declaration.

  Hence the call to check_duplicate_tc_binders is in generaliseTyClDecl,
  rather than in generaliseTcTyCon.

There are errors to catch here.  Suppose we had
   class E (f :: j) (g :: k) where
     op :: SameKind f g -> blah

Then, just before generalisation we will have the (unquantified)
   E :: k1 -> k1 -> Constraint

That's bad!  Two distinctly-named tyvars (j and k) have ended up with
the same representative k1.  So when swizzling, we check (in
check_duplicate_tc_binders) that two distinct source names map
to the same representative.

Here's an interesting case:
    class C1 f where
      type S (f :: k1)
      type T (f :: k2)
Here k1 and k2 are different Names, but they end up mapped to the
same representative TyVar.  To make the swizzling consistent (remember
we must have a single k across C1, S and T) we reject the program.

Another interesting case
    class C2 f where
      type S (f :: k) (p::Type)
      type T (f :: k) (p::Type->Type)

Here the two k's (and the two p's) get distinct Uniques, because they
are seen by the renamer as locally bound in S and T resp.  But again
the two (distinct) k's end up bound to the same representative TyVar.
You might argue that this should be accepted, but it's definitely
rejected (via an entirely different code path) if you add a kind sig:
    type C2' :: j -> Constraint
    class C2' f where
      type S (f :: k) (p::Type)
We get
    • Expected kind ‘j’, but ‘f’ has kind ‘k’
    • In the associated type family declaration for ‘S’

So we reject C2 too, even without the kind signature.  We have
to do a bit of work to get a good error message, since both k's
look the same to the user.

Another case
    class C3 (f :: k1) where
      type S (f :: k2)

This will be rejected too.


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

  2. kcTyCLGroup
      - Do inferInitialKinds, which will signal a promotion
        error if B is used in any of the kinds needed to initialise
        B's kind (e.g. (a :: Type)) here

      - Extend the type env with these initial kinds (monomorphic for
        decls that lack a CUSK)
            B :-> TcTyCon <initial kind>
        (thereby overriding the B :-> TyConPE binding)
        and do kcLTyClDecl on each decl to get equality constraints on
        all those initial kinds

      - Generalise the initial kind, making a poly-kinded TcTyCon

  3. Back in tcTyDecls, extend the envt with bindings of the poly-kinded
     TcTyCons, again overriding the promotion-error bindings.

     But note that the data constructor promotion errors are still in place
     so that (in our example) a use of MkB will still be signalled as
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

Note [Don't process associated types in getInitialKind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Previously, we processed associated types in the thing_inside in getInitialKind,
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
because the solveEqualities in kcInferDeclHeader is at TcLevel 1 and so kappa[1]
will unify with Type.

Bottom line: as associated types should have no effect on a CUSK enclosing class,
we move processing them to a separate action, run after the outer kind has
been generalized.

-}

kcTyClGroup :: KindSigEnv -> [LTyClDecl GhcRn] -> TcM [TcTyCon]

-- Kind check this group, kind generalize, and return the resulting local env
-- This binds the TyCons and Classes of the group, but not the DataCons
-- See Note [Kind checking for type and class decls]
-- and Note [Inferring kinds for type declarations]
kcTyClGroup kisig_env decls
  = do  { mod <- getModule
        ; traceTc "---- kcTyClGroup ---- {"
                  (text "module" <+> ppr mod $$ vcat (map ppr decls))

          -- Kind checking;
          --    1. Bind kind variables for decls
          --    2. Kind-check decls
          --    3. Generalise the inferred kinds
          -- See Note [Kind checking for type and class decls]

        ; cusks_enabled <- xoptM LangExt.CUSKs <&&> xoptM LangExt.PolyKinds
                    -- See Note [CUSKs and PolyKinds]
        ; let (kindless_decls, kinded_decls) = partitionWith get_kind decls

              get_kind d
                | Just ki <- lookupNameEnv kisig_env (tcdName (unLoc d))
                = Right (d, SAKS ki)

                | cusks_enabled && hsDeclHasCusk (unLoc d)
                = Right (d, CUSK)

                | otherwise = Left d

        ; checked_tcs <- checkNoErrs $
                         checkInitialKinds kinded_decls
                         -- checkNoErrs because we are about to extend
                         -- the envt with these tycons, and we get
                         -- knock-on errors if we have tycons with
                         -- malformed kinds

        ; inferred_tcs
            <- tcExtendKindEnvWithTyCons checked_tcs  $
               pushLevelAndSolveEqualities UnkSkol [] $
                     -- We are going to kind-generalise, so unification
                     -- variables in here must be one level in
               do {  -- Step 1: Bind kind variables for all decls
                    mono_tcs <- inferInitialKinds kindless_decls

                  ; traceTc "kcTyClGroup: initial kinds" $
                    ppr_tc_kinds mono_tcs

                    -- Step 2: Set extended envt, kind-check the decls
                    -- NB: the environment extension overrides the tycon
                    --     promotion-errors bindings
                    --     See Note [Type environment evolution]
                  ; checkNoErrs $
                    tcExtendKindEnvWithTyCons mono_tcs $
                    mapM_ kcLTyClDecl kindless_decls

                  ; return mono_tcs }

        -- Step 3: generalisation
        -- Finally, go through each tycon and give it its final kind,
        -- with all the required, specified, and inferred variables
        -- in order.
        ; let inferred_tc_env = mkNameEnv $
                                map (\tc -> (tyConName tc, tc)) inferred_tcs
        ; generalized_tcs <- concatMapM (generaliseTyClDecl inferred_tc_env)
                                        kindless_decls

        ; let poly_tcs = checked_tcs ++ generalized_tcs
        ; traceTc "---- kcTyClGroup end ---- }" (ppr_tc_kinds poly_tcs)
        ; return poly_tcs }
  where
    ppr_tc_kinds tcs = vcat (map pp_tc tcs)
    pp_tc tc = ppr (tyConName tc) <+> dcolon <+> ppr (tyConKind tc)

type ScopedPairs = [(Name, TcTyVar)]
  -- The ScopedPairs for a TcTyCon are precisely
  --    specified-tvs ++ required-tvs
  -- You can distinguish them because there are tyConArity required-tvs

generaliseTyClDecl :: NameEnv TcTyCon -> LTyClDecl GhcRn -> TcM [TcTyCon]
-- See Note [Swizzling the tyvars before generaliseTcTyCon]
generaliseTyClDecl inferred_tc_env (L _ decl)
  = do { let names_in_this_decl :: [Name]
             names_in_this_decl = tycld_names decl

       -- Extract the specified/required binders and skolemise them
       ; tc_with_tvs  <- mapM skolemise_tc_tycon names_in_this_decl

       -- Zonk, to manifest the side-effects of skolemisation to the swizzler
       -- NB: it's important to skolemise them all before this step. E.g.
       --         class C f where { type T (f :: k) }
       --     We only skolemise k when looking at T's binders,
       --     but k appears in f's kind in C's binders.
       ; tc_infos <- mapM zonk_tc_tycon tc_with_tvs

       -- Swizzle
       ; swizzled_infos <- tcAddDeclCtxt decl (swizzleTcTyConBndrs tc_infos)

       -- And finally generalise
       ; mapAndReportM generaliseTcTyCon swizzled_infos }
  where
    tycld_names :: TyClDecl GhcRn -> [Name]
    tycld_names decl = tcdName decl : at_names decl

    at_names :: TyClDecl GhcRn -> [Name]
    at_names (ClassDecl { tcdATs = ats }) = map (familyDeclName . unLoc) ats
    at_names _ = []  -- Only class decls have associated types

    skolemise_tc_tycon :: Name -> TcM (TcTyCon, ScopedPairs)
    -- Zonk and skolemise the Specified and Required binders
    skolemise_tc_tycon tc_name
      = do { let tc = lookupNameEnv_NF inferred_tc_env tc_name
                      -- This lookup should not fail
           ; scoped_prs <- mapSndM zonkAndSkolemise (tcTyConScopedTyVars tc)
           ; return (tc, scoped_prs) }

    zonk_tc_tycon :: (TcTyCon, ScopedPairs) -> TcM (TcTyCon, ScopedPairs, TcKind)
    zonk_tc_tycon (tc, scoped_prs)
      = do { scoped_prs <- mapSndM zonkTcTyVarToTyVar scoped_prs
                           -- We really have to do this again, even though
                           -- we have just done zonkAndSkolemise
           ; res_kind   <- zonkTcType (tyConResKind tc)
           ; return (tc, scoped_prs, res_kind) }

swizzleTcTyConBndrs :: [(TcTyCon, ScopedPairs, TcKind)]
                -> TcM [(TcTyCon, ScopedPairs, TcKind)]
swizzleTcTyConBndrs tc_infos
  | all no_swizzle swizzle_prs
    -- This fast path happens almost all the time
    -- See Note [Cloning for type variable binders] in GHC.Tc.Gen.HsType
    -- "Almost all the time" means not the case of mutual recursion with
    -- polymorphic kinds.
  = do { traceTc "Skipping swizzleTcTyConBndrs for" (ppr (map fstOf3 tc_infos))
       ; return tc_infos }

  | otherwise
  = do { check_duplicate_tc_binders

       ; traceTc "swizzleTcTyConBndrs" $
         vcat [ text "before" <+> ppr_infos tc_infos
              , text "swizzle_prs" <+> ppr swizzle_prs
              , text "after" <+> ppr_infos swizzled_infos ]

       ; return swizzled_infos }

  where
    swizzled_infos =  [ (tc, mapSnd swizzle_var scoped_prs, swizzle_ty kind)
                      | (tc, scoped_prs, kind) <- tc_infos ]

    swizzle_prs :: [(Name,TyVar)]
    -- Pairs the user-specifed Name with its representative TyVar
    -- See Note [Swizzling the tyvars before generaliseTcTyCon]
    swizzle_prs = [ pr | (_, prs, _) <- tc_infos, pr <- prs ]

    no_swizzle :: (Name,TyVar) -> Bool
    no_swizzle (nm, tv) = nm == tyVarName tv

    ppr_infos infos = vcat [ ppr tc <+> pprTyVars (map snd prs)
                           | (tc, prs, _) <- infos ]

    -- Check for duplicates
    -- E.g. data SameKind (a::k) (b::k)
    --      data T (a::k1) (b::k2) = MkT (SameKind a b)
    -- Here k1 and k2 start as TyVarTvs, and get unified with each other
    -- If this happens, things get very confused later, so fail fast
    check_duplicate_tc_binders :: TcM ()
    check_duplicate_tc_binders = unless (null err_prs) $
                                 do { mapM_ report_dup err_prs; failM }

    -------------- Error reporting ------------
    err_prs :: [(Name,Name)]
    err_prs = [ (n1,n2)
              | pr :| prs <- findDupsEq ((==) `on` snd) swizzle_prs
              , (n1,_):(n2,_):_ <- [nubBy ((==) `on` fst) (pr:prs)] ]
              -- This nubBy avoids bogus error reports when we have
              --    [("f", f), ..., ("f",f)....] in swizzle_prs
              -- which happens with  class C f where { type T f }

    report_dup :: (Name,Name) -> TcM ()
    report_dup (n1,n2)
      = setSrcSpan (getSrcSpan n2) $ addErrTc $
        hang (text "Different names for the same type variable:") 2 info
      where
        info | nameOccName n1 /= nameOccName n2
             = quotes (ppr n1) <+> text "and" <+> quotes (ppr n2)
             | otherwise -- Same OccNames! See C2 in
                         -- Note [Swizzling the tyvars before generaliseTcTyCon]
             = vcat [ quotes (ppr n1) <+> text "bound at" <+> ppr (getSrcLoc n1)
                    , quotes (ppr n2) <+> text "bound at" <+> ppr (getSrcLoc n2) ]

    -------------- The swizzler ------------
    -- This does a deep traverse, simply doing a
    -- Name-to-Name change, governed by swizzle_env
    -- The 'swap' is what gets from the representative TyVar
    -- back to the original user-specified Name
    swizzle_env = mkVarEnv (map swap swizzle_prs)

    swizzleMapper :: TyCoMapper () Identity
    swizzleMapper = TyCoMapper { tcm_tyvar = swizzle_tv
                               , tcm_covar = swizzle_cv
                               , tcm_hole  = swizzle_hole
                               , tcm_tycobinder = swizzle_bndr
                               , tcm_tycon      = swizzle_tycon }
    swizzle_hole  _ hole = pprPanic "swizzle_hole" (ppr hole)
       -- These types are pre-zonked
    swizzle_tycon tc = pprPanic "swizzle_tc" (ppr tc)
       -- TcTyCons can't appear in kinds (yet)
    swizzle_tv _ tv = return (mkTyVarTy (swizzle_var tv))
    swizzle_cv _ cv = return (mkCoVarCo (swizzle_var cv))

    swizzle_bndr _ tcv _
      = return ((), swizzle_var tcv)

    swizzle_var :: Var -> Var
    swizzle_var v
      | Just nm <- lookupVarEnv swizzle_env v
      = updateVarType swizzle_ty (v `setVarName` nm)
      | otherwise
      = updateVarType swizzle_ty v

    (map_type, _, _, _) = mapTyCo swizzleMapper
    swizzle_ty ty = runIdentity (map_type ty)


generaliseTcTyCon :: (TcTyCon, ScopedPairs, TcKind) -> TcM TcTyCon
generaliseTcTyCon (tc, scoped_prs, tc_res_kind)
  -- See Note [Required, Specified, and Inferred for types]
  = setSrcSpan (getSrcSpan tc) $
    addTyConCtxt tc $
    do { -- Step 1: Separate Specified from Required variables
         -- NB: spec_req_tvs = spec_tvs ++ req_tvs
         --     And req_tvs is 1-1 with tyConTyVars
         --     See Note [Scoped tyvars in a TcTyCon] in GHC.Core.TyCon
       ; let spec_req_tvs        = map snd scoped_prs
             n_spec              = length spec_req_tvs - tyConArity tc
             (spec_tvs, req_tvs) = splitAt n_spec spec_req_tvs
             sorted_spec_tvs     = scopedSort spec_tvs
                 -- NB: We can't do the sort until we've zonked
                 --     Maintain the L-R order of scoped_tvs

       -- Step 2a: find all the Inferred variables we want to quantify over
       ; dvs1 <- candidateQTyVarsOfKinds $
                 (tc_res_kind : map tyVarKind spec_req_tvs)
       ; let dvs2 = dvs1 `delCandidates` spec_req_tvs

       -- Step 2b: quantify, mainly meaning skolemise the free variables
       -- Returned 'inferred' are scope-sorted and skolemised
       ; inferred <- quantifyTyVars dvs2

       ; traceTc "generaliseTcTyCon: pre zonk"
           (vcat [ text "tycon =" <+> ppr tc
                 , text "spec_req_tvs =" <+> pprTyVars spec_req_tvs
                 , text "tc_res_kind =" <+> ppr tc_res_kind
                 , text "dvs1 =" <+> ppr dvs1
                 , text "inferred =" <+> pprTyVars inferred ])

       -- Step 3: Final zonk (following kind generalisation)
       -- See Note [Swizzling the tyvars before generaliseTcTyCon]
       ; ze <- emptyZonkEnv
       ; (ze, inferred)        <- zonkTyBndrsX ze inferred
       ; (ze, sorted_spec_tvs) <- zonkTyBndrsX ze sorted_spec_tvs
       ; (ze, req_tvs)         <- zonkTyBndrsX ze req_tvs
       ; tc_res_kind           <- zonkTcTypeToTypeX ze tc_res_kind

       ; traceTc "generaliseTcTyCon: post zonk" $
         vcat [ text "tycon =" <+> ppr tc
              , text "inferred =" <+> pprTyVars inferred
              , text "spec_req_tvs =" <+> pprTyVars spec_req_tvs
              , text "sorted_spec_tvs =" <+> pprTyVars sorted_spec_tvs
              , text "req_tvs =" <+> ppr req_tvs
              , text "zonk-env =" <+> ppr ze ]

       -- Step 4: Make the TyConBinders.
       ; let dep_fv_set     = candidateKindVars dvs1
             inferred_tcbs  = mkNamedTyConBinders Inferred inferred
             specified_tcbs = mkNamedTyConBinders Specified sorted_spec_tvs
             required_tcbs  = map (mkRequiredTyConBinder dep_fv_set) req_tvs

       -- Step 5: Assemble the final list.
             final_tcbs = concat [ inferred_tcbs
                                 , specified_tcbs
                                 , required_tcbs ]

       -- Step 6: Make the result TcTyCon
             tycon = mkTcTyCon (tyConName tc) final_tcbs tc_res_kind
                            (mkTyVarNamePairs (sorted_spec_tvs ++ req_tvs))
                            True {- it's generalised now -}
                            (tyConFlavour tc)

       ; traceTc "generaliseTcTyCon done" $
         vcat [ text "tycon =" <+> ppr tc
              , text "tc_res_kind =" <+> ppr tc_res_kind
              , text "dep_fv_set =" <+> ppr dep_fv_set
              , text "inferred_tcbs =" <+> ppr inferred_tcbs
              , text "specified_tcbs =" <+> ppr specified_tcbs
              , text "required_tcbs =" <+> ppr required_tcbs
              , text "final_tcbs =" <+> ppr final_tcbs ]

       -- Step 7: Check for validity.
       -- We do this here because we're about to put the tycon into the
       -- the environment, and we don't want anything malformed there
       ; checkTyConTelescope tycon

       ; return tycon }

{- Note [Required, Specified, and Inferred for types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each forall'd type variable in a type or kind is one of

  * Required: an argument must be provided at every call site

  * Specified: the argument can be inferred at call sites, but
    may be instantiated with visible type/kind application

  * Inferred: the must be inferred at call sites; it
    is unavailable for use with visible type/kind application.

Why have Inferred at all? Because we just can't make user-facing
promises about the ordering of some variables. These might swizzle
around even between minor released. By forbidding visible type
application, we ensure users aren't caught unawares.

Go read Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility] in GHC.Core.TyCo.Rep.

The question for this Note is this:
   given a TyClDecl, how are its quantified type variables classified?
Much of the debate is memorialized in #15743.

Here is our design choice. When inferring the ordering of variables
for a TyCl declaration (that is, for those variables that the user
has not specified the order with an explicit `forall`), we use the
following order:

 1. Inferred variables
 2. Specified variables; in the left-to-right order in which
    the user wrote them, modified by scopedSort (see below)
    to put them in depdendency order.
 3. Required variables before a top-level ::
 4. All variables after a top-level ::

If this ordering does not make a valid telescope, we reject the definition.

Example:
  data SameKind :: k -> k -> *
  data Bad a (c :: Proxy b) (d :: Proxy a) (x :: SameKind b d)

For Bad:
  - a, c, d, x are Required; they are explicitly listed by the user
    as the positional arguments of Bad
  - b is Specified; it appears explicitly in a kind signature
  - k, the kind of a, is Inferred; it is not mentioned explicitly at all

Putting variables in the order Inferred, Specified, Required
gives us this telescope:
  Inferred:  k
  Specified: b : Proxy a
  Required : (a : k) (c : Proxy b) (d : Proxy a) (x : SameKind b d)

But this order is ill-scoped, because b's kind mentions a, which occurs
after b in the telescope. So we reject Bad.

Associated types
~~~~~~~~~~~~~~~~
For associated types everything above is determined by the
associated-type declaration alone, ignoring the class header.
Here is an example (#15592)
  class C (a :: k) b where
    type F (x :: b a)

In the kind of C, 'k' is Specified.  But what about F?
In the kind of F,

 * Should k be Inferred or Specified?  It's Specified for C,
   but not mentioned in F's declaration.

 * In which order should the Specified variables a and b occur?
   It's clearly 'a' then 'b' in C's declaration, but the L-R ordering
   in F's declaration is 'b' then 'a'.

In both cases we make the choice by looking at F's declaration alone,
so it gets the kind
   F :: forall {k}. forall b a. b a -> Type

How it works
~~~~~~~~~~~~
These design choices are implemented by two completely different code
paths for

  * Declarations with a standalone kind signature or a complete user-specified
    kind signature (CUSK). Handled by the kcCheckDeclHeader.

  * Declarations without a kind signature (standalone or CUSK) are handled by
    kcInferDeclHeader; see Note [Inferring kinds for type declarations].

Note that neither code path worries about point (4) above, as this
is nicely handled by not mangling the res_kind. (Mangling res_kinds is done
*after* all this stuff, in tcDataDefn's call to etaExpandAlgTyCon.)

We can tell Inferred apart from Specified by looking at the scoped
tyvars; Specified are always included there.

Design alternatives
~~~~~~~~~~~~~~~~~~~
* For associated types we considered putting the class variables
  before the local variables, in a nod to the treatment for class
  methods. But it got too compilicated; see #15592, comment:21ff.

* We rigidly require the ordering above, even though we could be much more
  permissive. Relevant musings are at
  https://gitlab.haskell.org/ghc/ghc/issues/15743#note_161623
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

Test cases (and tickets) relevant to these design decisions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  T15591*
  T15592*
  T15743*

Note [Inferring kinds for type declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This note deals with /inference/ for type declarations
that do not have a CUSK.  Consider
  data T (a :: k1) k2 (x :: k2) = MkT (S a k2 x)
  data S (b :: k3) k4 (y :: k4) = MkS (T b k4 y)

We do kind inference as follows:

* Step 1: inferInitialKinds, and in particular kcInferDeclHeader.
  Make a unification variable for each of the Required and Specified
  type variables in the header.

  Record the connection between the Names the user wrote and the
  fresh unification variables in the tcTyConScopedTyVars field
  of the TcTyCon we are making
      [ (a,  aa)
      , (k1, kk1)
      , (k2, kk2)
      , (x,  xx) ]
  (I'm using the convention that double letter like 'aa' or 'kk'
  mean a unification variable.)

  These unification variables
    - Are TyVarTvs: that is, unification variables that can
      unify only with other type variables.
      See Note [Signature skolems] in GHC.Tc.Utils.TcType

    - Have complete fresh Names; see GHC.Tc.Utils.TcMType
      Note [Unification variables need fresh Names]

  Assign initial monomorphic kinds to S, T
          T :: kk1 -> * -> kk2 -> *
          S :: kk3 -> * -> kk4 -> *

* Step 2: kcTyClDecl. Extend the environment with a TcTyCon for S and
  T, with these monomorphic kinds.  Now kind-check the declarations,
  and solve the resulting equalities.  The goal here is to discover
  constraints on all these unification variables.

  Here we find that kk1 := kk3, and kk2 := kk4.

  This is why we can't use skolems for kk1 etc; they have to
  unify with each other.

* Step 3: generaliseTcTyCon. Generalise each TyCon in turn.
  We find the free variables of the kind, skolemise them,
  sort them out into Inferred/Required/Specified (see the above
  Note [Required, Specified, and Inferred for types]),
  and perform some validity checks.

  This makes the utterly-final TyConBinders for the TyCon.

  All this is very similar at the level of terms: see GHC.Tc.Gen.Bind
  Note [Quantified variables in partial type signatures]

  But there some tricky corners: Note [Tricky scoping in generaliseTcTyCon]

* Step 4.  Extend the type environment with a TcTyCon for S and T, now
  with their utterly-final polymorphic kinds (needed for recursive
  occurrences of S, T).  Now typecheck the declarations, and build the
  final AlgTyCon for S and T resp.

The first three steps are in kcTyClGroup; the fourth is in
tcTyClDecls.

There are some wrinkles

* Do not default TyVarTvs.  We always want to kind-generalise over
  TyVarTvs, and /not/ default them to Type. By definition a TyVarTv is
  not allowed to unify with a type; it must stand for a type
  variable. Hence the check in GHC.Tc.Solver.defaultTyVarTcS, and
  GHC.Tc.Utils.TcMType.defaultTyVar.  Here's another example (#14555):
     data Exp :: [TYPE rep] -> TYPE rep -> Type where
        Lam :: Exp (a:xs) b -> Exp xs (a -> b)
  We want to kind-generalise over the 'rep' variable.
  #14563 is another example.

* Duplicate type variables. Consider #11203
    data SameKind :: k -> k -> *
    data Q (a :: k1) (b :: k2) c = MkQ (SameKind a b)
  Here we will unify k1 with k2, but this time doing so is an error,
  because k1 and k2 are bound in the same declaration.

  We spot this during validity checking (findDupTyVarTvs),
  in generaliseTcTyCon.

* Required arguments.  Even the Required arguments should be made
  into TyVarTvs, not skolems.  Consider
    data T k (a :: k)
  Here, k is a Required, dependent variable. For uniformity, it is helpful
  to have k be a TyVarTv, in parallel with other dependent variables.

* Duplicate skolemisation is expected.  When generalising in Step 3,
  we may find that one of the variables we want to quantify has
  already been skolemised.  For example, suppose we have already
  generalise S. When we come to T we'll find that kk1 (now the same as
  kk3) has already been skolemised.

  That's fine -- but it means that
    a) when collecting quantification candidates, in
       candidateQTyVarsOfKind, we must collect skolems
    b) quantifyTyVars should be a no-op on such a skolem

Note [Tricky scoping in generaliseTcTyCon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider #16342
  class C (a::ka) x where
    cop :: D a x => x -> Proxy a -> Proxy a
    cop _ x = x :: Proxy (a::ka)

  class D (b::kb) y where
    dop :: C b y => y -> Proxy b -> Proxy b
    dop _ x = x :: Proxy (b::kb)

C and D are mutually recursive, by the time we get to
generaliseTcTyCon we'll have unified kka := kkb.

But when typechecking the default declarations for 'cop' and 'dop' in
tcDlassDecl2 we need {a, ka} and {b, kb} respectively to be in scope.
But at that point all we have is the utterly-final Class itself.

Conclusion: the classTyVars of a class must have the same Name as
that originally assigned by the user.  In our example, C must have
classTyVars {a, ka, x} while D has classTyVars {a, kb, y}.  Despite
the fact that kka and kkb got unified!

We achieve this sleight of hand in generaliseTcTyCon, using
the specialised function zonkRecTyVarBndrs.  We make the call
   zonkRecTyVarBndrs [ka,a,x] [kkb,aa,xxx]
where the [ka,a,x] are the Names originally assigned by the user, and
[kkb,aa,xx] are the corresponding (post-zonking, skolemised) TcTyVars.
zonkRecTyVarBndrs builds a recursive ZonkEnv that binds
   kkb :-> (ka :: <zonked kind of kkb>)
   aa  :-> (a  :: <konked kind of aa>)
   etc
That is, it maps each skolemised TcTyVars to the utterly-final
TyVar to put in the class, with its correct user-specified name.
When generalising D we'll do the same thing, but the ZonkEnv will map
   kkb :-> (kb :: <zonked kind of kkb>)
   bb  :-> (b  :: <konked kind of bb>)
   etc
Note that 'kkb' again appears in the domain of the mapping, but this
time mapped to 'kb'.  That's how C and D end up with differently-named
final TyVars despite the fact that we unified kka:=kkb

zonkRecTyVarBndrs we need to do knot-tying because of the need to
apply this same substitution to the kind of each.

Note [Inferring visible dependent quantification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  data T k :: k -> Type where
    MkT1 :: T Type Int
    MkT2 :: T (Type -> Type) Maybe

This looks like it should work. However, it is polymorphically recursive,
as the uses of T in the constructor types specialize the k in the kind
of T. This trips up our dear users (#17131, #17541), and so we add
a "landmark" context (which cannot be suppressed) whenever we
spot inferred visible dependent quantification (VDQ).

It's hard to know when we've actually been tripped up by polymorphic recursion
specifically, so we just include a note to users whenever we infer VDQ. The
testsuite did not show up a single spurious inclusion of this message.

The context is added in addVDQNote, which looks for a visible TyConBinder
that also appears in the TyCon's kind. (I first looked at the kind for
a visible, dependent quantifier, but Note [No polymorphic recursion] in
GHC.Tc.Gen.HsType defeats that approach.) addVDQNote is used in kcTyClDecl,
which is used only when inferring the kind of a tycon (never with a CUSK or
SAK).

Once upon a time, I (Richard E) thought that the tycon-kind could
not be a forall-type. But this is wrong: data T :: forall k. k -> Type
(with -XNoCUSKs) could end up here. And this is all OK.


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
    mkNameEnv [ (familyDeclName at, APromotionErr TyConPE)
              | L _ at <- ats ]

mk_prom_err_env (DataDecl { tcdLName = L _ name
                          , tcdDataDefn = HsDataDefn { dd_cons = cons } })
  = unitNameEnv name (APromotionErr TyConPE)
    `plusNameEnv`
    mkNameEnv [ (con, APromotionErr RecDataConPE)
              | L _ con' <- cons
              , L _ con  <- getConNames con' ]

mk_prom_err_env decl
  = unitNameEnv (tcdName decl) (APromotionErr TyConPE)
    -- Works for family declarations too

--------------
inferInitialKinds :: [LTyClDecl GhcRn] -> TcM [TcTyCon]
-- Returns a TcTyCon for each TyCon bound by the decls,
-- each with its initial kind

inferInitialKinds decls
  = do { traceTc "inferInitialKinds {" $ ppr (map (tcdName . unLoc) decls)
       ; tcs <- concatMapM infer_initial_kind decls
       ; traceTc "inferInitialKinds done }" empty
       ; return tcs }
  where
    infer_initial_kind = addLocM (getInitialKind InitialKindInfer)

-- Check type/class declarations against their standalone kind signatures or
-- CUSKs, producing a generalized TcTyCon for each.
checkInitialKinds :: [(LTyClDecl GhcRn, SAKS_or_CUSK)] -> TcM [TcTyCon]
checkInitialKinds decls
  = do { traceTc "checkInitialKinds {" $ ppr (mapFst (tcdName . unLoc) decls)
       ; tcs <- concatMapM check_initial_kind decls
       ; traceTc "checkInitialKinds done }" empty
       ; return tcs }
  where
    check_initial_kind (ldecl, msig) =
      addLocM (getInitialKind (InitialKindCheck msig)) ldecl

-- | Get the initial kind of a TyClDecl, either generalized or non-generalized,
-- depending on the 'InitialKindStrategy'.
getInitialKind :: InitialKindStrategy -> TyClDecl GhcRn -> TcM [TcTyCon]

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
-- No family instances are passed to checkInitialKinds/inferInitialKinds
getInitialKind strategy
    (ClassDecl { tcdLName = L _ name
               , tcdTyVars = ktvs
               , tcdATs = ats })
  = do { cls <- kcDeclHeader strategy name ClassFlavour ktvs $
                return (TheKind constraintKind)
       ; let parent_tv_prs = tcTyConScopedTyVars cls
            -- See Note [Don't process associated types in getInitialKind]
       ; inner_tcs <-
           tcExtendNameTyVarEnv parent_tv_prs $
           mapM (addLocM (getAssocFamInitialKind cls)) ats
       ; return (cls : inner_tcs) }
  where
    getAssocFamInitialKind cls =
      case strategy of
        InitialKindInfer -> get_fam_decl_initial_kind (Just cls)
        InitialKindCheck _ -> check_initial_kind_assoc_fam cls

getInitialKind strategy
    (DataDecl { tcdLName = L _ name
              , tcdTyVars = ktvs
              , tcdDataDefn = HsDataDefn { dd_kindSig = m_sig
                                         , dd_ND = new_or_data } })
  = do  { let flav = newOrDataToFlavour new_or_data
              ctxt = DataKindCtxt name
        ; tc <- kcDeclHeader strategy name flav ktvs $
                case m_sig of
                  Just ksig -> TheKind <$> tcLHsKindSig ctxt ksig
                  Nothing -> return $ dataDeclDefaultResultKind new_or_data
        ; return [tc] }

getInitialKind InitialKindInfer (FamDecl { tcdFam = decl })
  = do { tc <- get_fam_decl_initial_kind Nothing decl
       ; return [tc] }

getInitialKind (InitialKindCheck msig) (FamDecl { tcdFam =
  FamilyDecl { fdLName     = unLoc -> name
             , fdTyVars    = ktvs
             , fdResultSig = unLoc -> resultSig
             , fdInfo      = info } } )
  = do { let flav = getFamFlav Nothing info
             ctxt = TyFamResKindCtxt name
       ; tc <- kcDeclHeader (InitialKindCheck msig) name flav ktvs $
               case famResultKindSignature resultSig of
                 Just ksig -> TheKind <$> tcLHsKindSig ctxt ksig
                 Nothing ->
                   case msig of
                     CUSK -> return (TheKind liftedTypeKind)
                     SAKS _ -> return AnyKind
       ; return [tc] }

getInitialKind strategy
    (SynDecl { tcdLName = L _ name
             , tcdTyVars = ktvs
             , tcdRhs = rhs })
  = do { let ctxt = TySynKindCtxt name
       ; tc <- kcDeclHeader strategy name TypeSynonymFlavour ktvs $
               case hsTyKindSig rhs of
                 Just rhs_sig -> TheKind <$> tcLHsKindSig ctxt rhs_sig
                 Nothing -> return AnyKind
       ; return [tc] }

get_fam_decl_initial_kind
  :: Maybe TcTyCon -- ^ Just cls <=> this is an associated family of class cls
  -> FamilyDecl GhcRn
  -> TcM TcTyCon
get_fam_decl_initial_kind mb_parent_tycon
    FamilyDecl { fdLName     = L _ name
               , fdTyVars    = ktvs
               , fdResultSig = L _ resultSig
               , fdInfo      = info }
  = kcDeclHeader InitialKindInfer name flav ktvs $
    case resultSig of
      KindSig _ ki                            -> TheKind <$> tcLHsKindSig ctxt ki
      TyVarSig _ (L _ (KindedTyVar _ _ _ ki)) -> TheKind <$> tcLHsKindSig ctxt ki
      _ -- open type families have * return kind by default
        | tcFlavourIsOpen flav              -> return (TheKind liftedTypeKind)
               -- closed type families have their return kind inferred
               -- by default
        | otherwise                         -> return AnyKind
  where
    flav = getFamFlav mb_parent_tycon info
    ctxt = TyFamResKindCtxt name

-- See Note [Standalone kind signatures for associated types]
check_initial_kind_assoc_fam
  :: TcTyCon -- parent class
  -> FamilyDecl GhcRn
  -> TcM TcTyCon
check_initial_kind_assoc_fam cls
  FamilyDecl
    { fdLName     = unLoc -> name
    , fdTyVars    = ktvs
    , fdResultSig = unLoc -> resultSig
    , fdInfo      = info }
  = kcDeclHeader (InitialKindCheck CUSK) name flav ktvs $
    case famResultKindSignature resultSig of
      Just ksig -> TheKind <$> tcLHsKindSig ctxt ksig
      Nothing -> return (TheKind liftedTypeKind)
  where
    ctxt = TyFamResKindCtxt name
    flav = getFamFlav (Just cls) info

{- Note [Standalone kind signatures for associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If associated types had standalone kind signatures, would they wear them

---------------------------+------------------------------
  like this? (OUT)         |   or like this? (IN)
---------------------------+------------------------------
  type T :: Type -> Type   |   class C a where
  class C a where          |     type T :: Type -> Type
    type T a               |     type T a

The (IN) variant is syntactically ambiguous:

  class C a where
    type T :: a   -- standalone kind signature?
    type T :: a   -- declaration header?

The (OUT) variant does not suffer from this issue, but it might not be the
direction in which we want to take Haskell: we seek to unify type families and
functions, and, by extension, associated types with class methods. And yet we
give class methods their signatures inside the class, not outside. Neither do
we have the counterpart of InstanceSigs for StandaloneKindSignatures.

For now, we dodge the question by using CUSKs for associated types instead of
standalone kind signatures. This is a simple addition to the rule we used to
have before standalone kind signatures:

  old rule:  associated type has a CUSK iff its parent class has a CUSK
  new rule:  associated type has a CUSK iff its parent class has a CUSK or a standalone kind signature

-}

-- See Note [Data declaration default result kind]
dataDeclDefaultResultKind :: NewOrData -> ContextKind
dataDeclDefaultResultKind NewType  = OpenKind
  -- See Note [Implementation of UnliftedNewtypes], point <Error Messages>.
dataDeclDefaultResultKind DataType = TheKind liftedTypeKind

{- Note [Data declaration default result kind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When the user has not written an inline result kind annotation on a data
declaration, we assume it to be 'Type'. That is, the following declarations
D1 and D2 are considered equivalent:

  data D1         where ...
  data D2 :: Type where ...

The consequence of this assumption is that we reject D3 even though we
accept D4:

  data D3 where
    MkD3 :: ... -> D3 param

  data D4 :: Type -> Type where
    MkD4 :: ... -> D4 param

However, there's a twist: for newtypes, we must relax
the assumed result kind to (TYPE r):

  newtype D5 where
    MkD5 :: Int# -> D5

See Note [Implementation of UnliftedNewtypes], STEP 1 and it's sub-note
<Error Messages>.
-}

---------------------------------
getFamFlav
  :: Maybe TcTyCon    -- ^ Just cls <=> this is an associated family of class cls
  -> FamilyInfo pass
  -> TyConFlavour
getFamFlav mb_parent_tycon info =
  case info of
    DataFamily         -> DataFamilyFlavour mb_parent_tycon
    OpenTypeFamily     -> OpenTypeFamilyFlavour mb_parent_tycon
    ClosedTypeFamily _ -> ASSERT( isNothing mb_parent_tycon ) -- See Note [Closed type family mb_parent_tycon]
                          ClosedTypeFamilyFlavour

{- Note [Closed type family mb_parent_tycon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's no way to write a closed type family inside a class declaration:

  class C a where
    type family F a where  -- error: parse error on input ‘where’

In fact, it is not clear what the meaning of such a declaration would be.
Therefore, 'mb_parent_tycon' of any closed type family has to be Nothing.
-}

------------------------------------------------------------------------
kcLTyClDecl :: LTyClDecl GhcRn -> TcM ()
  -- See Note [Kind checking for type and class decls]
  -- Called only for declarations without a signature (no CUSKs or SAKs here)
kcLTyClDecl (L loc decl)
  = setSrcSpan loc $
    do { tycon <- tcLookupTcTyCon tc_name
       ; traceTc "kcTyClDecl {" (ppr tc_name)
       ; addVDQNote tycon $   -- See Note [Inferring visible dependent quantification]
         addErrCtxt (tcMkDeclCtxt decl) $
         kcTyClDecl decl tycon
       ; traceTc "kcTyClDecl done }" (ppr tc_name) }
  where
    tc_name = tcdName decl

kcTyClDecl :: TyClDecl GhcRn -> TcTyCon -> TcM ()
-- This function is used solely for its side effect on kind variables
-- NB kind signatures on the type variables and
--    result kind signature have already been dealt with
--    by inferInitialKind, so we can ignore them here.

kcTyClDecl (DataDecl { tcdLName    = (L _ name), tcdDataDefn = defn }) tycon
  | HsDataDefn { dd_ctxt = ctxt, dd_cons = cons, dd_ND = new_or_data } <- defn
  = bindTyClTyVars name $ \ _ _ _ ->
       -- NB: binding these tyvars isn't necessary for GADTs, but it does no
       -- harm.  For GADTs, each data con brings its own tyvars into scope,
       -- and the ones from this bindTyClTyVars are either not mentioned or
       -- (conceivably) shadowed.
    do { traceTc "kcTyClDecl" (ppr tycon $$ ppr (tyConTyVars tycon) $$ ppr (tyConResKind tycon))
       ; _ <- tcHsContext ctxt
       ; kcConDecls new_or_data (tyConResKind tycon) cons
       }

kcTyClDecl (SynDecl { tcdLName = L _ name, tcdRhs = rhs }) _tycon
  = bindTyClTyVars name $ \ _ _ res_kind ->
    discardResult $ tcCheckLHsType rhs (TheKind res_kind)
        -- NB: check against the result kind that we allocated
        -- in inferInitialKinds.

kcTyClDecl (ClassDecl { tcdLName = L _ name
                      , tcdCtxt = ctxt, tcdSigs = sigs }) _tycon
  = bindTyClTyVars name $ \ _ _ _ ->
    do  { _ <- tcHsContext ctxt
        ; mapM_ (wrapLocM_ kc_sig) sigs }
  where
    kc_sig (ClassOpSig _ _ nms op_ty) = kcClassSigType nms op_ty
    kc_sig _                          = return ()

kcTyClDecl (FamDecl _ (FamilyDecl { fdInfo   = fd_info })) fam_tc
-- closed type families look at their equations, but other families don't
-- do anything here
  = case fd_info of
      ClosedTypeFamily (Just eqns) -> mapM_ (kcTyFamInstEqn fam_tc) eqns
      _ -> return ()

-------------------

-- Kind-check the types of the arguments to a data constructor.
-- This includes doing kind unification if the type is a newtype.
-- See Note [Implementation of UnliftedNewtypes] for why we need
-- the first two arguments.
kcConArgTys :: NewOrData -> Kind -> [HsScaled GhcRn (LHsType GhcRn)] -> TcM ()
kcConArgTys new_or_data res_kind arg_tys = do
  { let exp_kind = getArgExpKind new_or_data res_kind
  ; forM_ arg_tys (\(HsScaled mult ty) -> do _ <- tcCheckLHsType (getBangType ty) exp_kind
                                             tcMult mult)
    -- See Note [Implementation of UnliftedNewtypes], STEP 2
  }

-- Kind-check the types of arguments to a Haskell98 data constructor.
kcConH98Args :: NewOrData -> Kind -> HsConDeclH98Details GhcRn -> TcM ()
kcConH98Args new_or_data res_kind con_args = case con_args of
  PrefixCon _ tys   -> kcConArgTys new_or_data res_kind tys
  InfixCon ty1 ty2  -> kcConArgTys new_or_data res_kind [ty1, ty2]
  RecCon (L _ flds) -> kcConArgTys new_or_data res_kind $
                       map (hsLinear . cd_fld_type . unLoc) flds

-- Kind-check the types of arguments to a GADT data constructor.
kcConGADTArgs :: NewOrData -> Kind -> HsConDeclGADTDetails GhcRn -> TcM ()
kcConGADTArgs new_or_data res_kind con_args = case con_args of
  PrefixConGADT tys     -> kcConArgTys new_or_data res_kind tys
  RecConGADT (L _ flds) -> kcConArgTys new_or_data res_kind $
                           map (hsLinear . cd_fld_type . unLoc) flds

kcConDecls :: NewOrData
           -> Kind             -- The result kind signature
                               --   Used only in H98 case
           -> [LConDecl GhcRn] -- The data constructors
           -> TcM ()
-- See Note [kcConDecls: kind-checking data type decls]
kcConDecls new_or_data tc_res_kind cons
  = mapM_ (wrapLocM_ (kcConDecl new_or_data tc_res_kind)) cons

-- Kind check a data constructor. In additional to the data constructor,
-- we also need to know about whether or not its corresponding type was
-- declared with data or newtype, and we need to know the result kind of
-- this type. See Note [Implementation of UnliftedNewtypes] for why
-- we need the first two arguments.
kcConDecl :: NewOrData
          -> Kind  -- Result kind of the type constructor
                   -- Usually Type but can be TYPE UnliftedRep
                   -- or even TYPE r, in the case of unlifted newtype
                   -- Used only in H98 case
          -> ConDecl GhcRn
          -> TcM ()
kcConDecl new_or_data tc_res_kind (ConDeclH98
  { con_name = name, con_ex_tvs = ex_tvs
  , con_mb_cxt = ex_ctxt, con_args = args })
  = addErrCtxt (dataConCtxt [name]) $
    discardResult                   $
    bindExplicitTKBndrs_Tv ex_tvs $
    do { _ <- tcHsMbContext ex_ctxt
       ; kcConH98Args new_or_data tc_res_kind args
         -- We don't need to check the telescope here,
         -- because that's done in tcConDecl
       }

kcConDecl new_or_data
          _tc_res_kind   -- Not used in GADT case (and doesn't make sense)
          (ConDeclGADT
    { con_names = names, con_bndrs = L _ outer_bndrs, con_mb_cxt = cxt
    , con_g_args = args, con_res_ty = res_ty })
  = -- See Note [kcConDecls: kind-checking data type decls]
    addErrCtxt (dataConCtxt names) $
    discardResult                      $
    bindOuterSigTKBndrs_Tv outer_bndrs $
        -- Why "_Tv"?  See Note [Using TyVarTvs for kind-checking GADTs]
    do { _ <- tcHsMbContext cxt
       ; traceTc "kcConDecl:GADT {" (ppr names $$ ppr res_ty)
       ; con_res_kind <- newOpenTypeKind
       ; _ <- tcCheckLHsType res_ty (TheKind con_res_kind)
       ; kcConGADTArgs new_or_data con_res_kind args
       ; traceTc "kcConDecl:GADT }" (ppr names $$ ppr con_res_kind)
       ; return () }

{- Note [kcConDecls: kind-checking data type decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kcConDecls is used when we are inferring the kind of the type
constructor in a data type declaration.  E.g.
    data T f a = MkT (f a)
we want to infer the kind of 'f' and 'a'. The basic plan is described
in Note [Inferring kinds for type declarations]; here we are doing Step 2.

In the GADT case we may have this:
   data T f a where
      MkT :: forall g b. g b -> T g b

Notice that the variables f,a, and g,b are quite distinct.
Nevertheless, the type signature for MkT must still influence the kind
T which is (remember Step 1) something like
  T :: kappa1 -> kappa2 -> Type
Otherwise we'd infer the bogus kind
  T :: forall k1 k2. k1 -> k2 -> Type.

The type signature for MkT influences the kind of T simply by
kind-checking the result type (T g b), which will force 'f' and 'g' to
have the same kinds. This is the call to
    tcCheckLHsType res_ty (TheKind con_res_kind)
Because this is the result type of an arrow, we know the kind must be
of form (TYPE rr), and we get better error messages if we enforce that
here (e.g. test gadt10).

For unlifted newtypes only, we must ensure that the argument kind
and result kind are the same:
* In the H98 case, we need the result kind of the TyCon, to unify with
  the argument kind.

* In GADT syntax, this unification happens via the result kind passed
  to kcConGADTArgs. The tycon's result kind is not used at all in the
  GADT case.

Note [Using TyVarTvs for kind-checking GADTs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  data Proxy a where
    MkProxy1 :: forall k (b :: k). Proxy b
    MkProxy2 :: forall j (c :: j). Proxy c

It seems reasonable that this should be accepted. But something very strange
is going on here: when we're kind-checking this declaration, we need to unify
the kind of `a` with k and j -- even though k and j's scopes are local to the type of
MkProxy{1,2}.

In effect, we are simply gathering constraints on the shape of Proxy's
kind, with no skolemisation or implication constraints involved at all.

The best approach we've come up with is to use TyVarTvs during the
kind-checking pass, rather than ordinary skolems. This is why we use
the "_Tv" variant, bindOuterSigTKBndrs_Tv.

Our only goal is to gather constraints on the kind of the type constructor;
we do not certify that the data declaration is well-kinded. For example:

  data SameKind :: k -> k -> Type
  data Bad a where
    MkBad :: forall k1 k2 (a :: k1) (b :: k2). Bad (SameKind a b)

which would be accepted by kcConDecl because k1 and k2 are
TyVarTvs. It is correctly rejected in the second pass, tcConDecl.
(Test case: polykinds/TyVarTvKinds3)

One drawback of this approach is sometimes it will accept a definition that
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

  * During GHC.Tc.Gen.HsType.tcTyVar we look in the *local* env, to get the
    fully-known, not knot-tied TcTyCon for T.

  * Then, in GHC.Tc.Utils.Zonk.zonkTcTypeToType (and zonkTcTyCon in particular)
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

Note [Datatype return kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are several poorly lit corners around datatype/newtype return kinds.
This Note explains these.  We cover data/newtype families and instances
in Note [Data family/instance return kinds].

data    T a :: <kind> where ...   -- See Point DT4
newtype T a :: <kind> where ...   -- See Point DT5

DT1 Where this applies: Only GADT syntax for data/newtype/instance declarations
    can have declared return kinds. This Note does not apply to Haskell98
    syntax.

DT2 Where these kinds come from: The return kind is part of the TyCon kind, gotten either
     by checkInitialKind (standalone kind signature / CUSK) or
     inferInitialKind. It is extracted by bindTyClTyVars in tcTyClDecl1. It is
     then passed to tcDataDefn.

DT3 Eta-expansion: Any forall-bound variables and function arguments in a result kind
    become parameters to the type. That is, when we say

     data T a :: Type -> Type where ...

    we really mean for T to have two parameters. The second parameter
    is produced by processing the return kind in etaExpandAlgTyCon,
    called in tcDataDefn.

    See also Note [TyConBinders for the result kind signatures of a data type]
    in GHC.Tc.Gen.HsType.

DT4 Datatype return kind restriction: A data type return kind must end
    in a type that, after type-synonym expansion, yields `TYPE LiftedRep`. By
    "end in", we mean we strip any foralls and function arguments off before
    checking.

    Examples:
      data T1 :: Type                          -- good
      data T2 :: Bool -> Type                  -- good
      data T3 :: Bool -> forall k. Type        -- strange, but still accepted
      data T4 :: forall k. k -> Type           -- good
      data T5 :: Bool                          -- bad
      data T6 :: Type -> Bool                  -- bad

    Exactly the same applies to data instance (but not data family)
    declarations.  Examples
      data instance D1 :: Type                 -- good
      data instance D2 :: Bool -> Type         -- good

    We can "look through" type synonyms
      type Star = Type
      data T7 :: Bool -> Star                  -- good (synonym expansion ok)
      type Arrow = (->)
      data T8 :: Arrow Bool Type               -- good (ditto)

    But we specifically do *not* do type family reduction here.
      type family ARROW where
        ARROW = (->)
      data T9 :: ARROW Bool Type               -- bad

      type family F a where
        F Int  = Bool
        F Bool = Type
      data T10 :: Bool -> F Bool               -- bad

    The /principle/ here is that in the TyCon for a data type or data instance,
    we must be able to lay out all the type-variable binders, one by one, until
    we reach (TYPE xx).  There is no place for a cast here.  We could add one,
    but let's not!

    This check is done in checkDataKindSig. For data declarations, this
    call is in tcDataDefn; for data instances, this call is in tcDataFamInstDecl.

DT5 Newtype return kind restriction.
    If -XUnliftedNewtypes is not on, then newtypes are treated just
    like datatypes --- see (4) above.

    If -XUnliftedNewtypes is on, then a newtype return kind must end in
    TYPE xyz, for some xyz (after type synonym expansion). The "xyz"
    may include type families, but the TYPE part must be visible
    /without/ expanding type families (only synonyms).

    This kind is unified with the kind of the representation type (the
    type of the one argument to the one constructor). See also steps
    (2) and (3) of Note [Implementation of UnliftedNewtypes].

    The checks are done in the same places as for datatypes.
    Examples (assume -XUnliftedNewtypes):

      newtype N1 :: Type                       -- good
      newtype N2 :: Bool -> Type               -- good
      newtype N3 :: forall r. Bool -> TYPE r   -- good

      type family F (t :: Type) :: RuntimeRep
      newtype N4 :: forall t -> TYPE (F t)     -- good

      type family STAR where
        STAR = Type
      newtype N5 :: Bool -> STAR               -- bad

Note [Data family/instance return kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Within this note, understand "instance" to mean data or newtype
instance, and understand "family" to mean data family. No type
families or classes here. Some examples:

data family T a :: <kind>          -- See Point DF56

data    instance T [a] :: <kind> where ...   -- See Point DF2
newtype instance T [a] :: <kind> where ...   -- See Point DF2

Here is the Plan for Data Families:

DF0 Where these kinds come from:

    Families: The return kind is either written in a standalone signature
     or extracted from a family declaration in getInitialKind.
     If a family declaration is missing a result kind, it is assumed to be
     Type. This assumption is in getInitialKind for CUSKs or
     get_fam_decl_initial_kind for non-signature & non-CUSK cases.

   Instances: The data family already has a known kind. The return kind
     of an instance is then calculated by applying the data family tycon
     to the patterns provided, as computed by the typeKind lhs_ty in the
     end of tcDataFamInstHeader. In the case of an instance written in GADT
     syntax, there are potentially *two* return kinds: the one computed from
     applying the data family tycon to the patterns, and the one given by
     the user. This second kind is checked by the tc_kind_sig function within
     tcDataFamInstHeader. See also DF3, below.

DF1 In a data/newtype instance, we treat the kind of the /data family/,
    once instantiated, as the "master kind" for the representation
    TyCon.  For example:
        data family T1 :: Type -> Type -> Type
        data instance T1 Int :: F Bool -> Type where ...
    The "master kind" for the representation TyCon R:T1Int comes
    from T1, not from the signature on the data instance.  It is as
    if we declared
        data R:T1Int :: Type -> Type where ...
     See Note [Liberalising data family return kinds] for an alternative
     plan.  But this current plan is simple, and ensures that all instances
     are simple instantiations of the master, without strange casts.

     An example with non-trivial instantiation:
        data family T2 :: forall k. Type -> k
        data instance T2 :: Type -> Type -> Type where ...
     Here 'k' gets instantiated with (Type -> Type), driven by
     the signature on the 'data instance'. (See also DT3 of
     Note [Datatype return kinds] about eta-expansion, which applies here,
     too; see tcDataFamInstDecl's call of etaExpandAlgTyCon.)

     A newtype example:

       data Color = Red | Blue
       type family Interpret (x :: Color) :: RuntimeRep where
         Interpret 'Red = 'IntRep
         Interpret 'Blue = 'WordRep
       data family Foo (x :: Color) :: TYPE (Interpret x)
       newtype instance Foo 'Red :: TYPE IntRep where
         FooRedC :: Int# -> Foo 'Red

    Here we get that Foo 'Red :: TYPE (Interpret Red), and our
    representation newtype looks like
         newtype R:FooRed :: TYPE (Interpret Red) where
            FooRedC :: Int# -> R:FooRed
    Remember: the master kind comes from the /family/ tycon.

DF2 /After/ this instantiation, the return kind of the master kind
    must obey the usual rules for data/newtype return kinds (DT4, DT5)
    of Note [Datatype return kinds].  Examples:
        data family T3 k :: k
        data instance T3 Type where ...          -- OK
        data instance T3 (Type->Type) where ...  -- OK
        data instance T3 (F Int) where ...       -- Not OK

DF3 Any kind signatures on the data/newtype instance are checked for
    equality with the master kind (and hence may guide instantiation)
    but are otherwise ignored. So in the T1 example above, we check
    that (F Int ~ Type) by unification; but otherwise ignore the
    user-supplied signature from the /family/ not the /instance/.

    We must be sure to instantiate any trailing invisible binders
    before doing this unification.  See the call to tcInstInvisibleBinders
    in tcDataFamInstHeader. For example:
       data family D :: forall k. k
       data instance D :: Type               -- forall k. k   <:  Type
       data instance D :: Type -> Type       -- forall k. k   <:  Type -> Type
         -- NB: these do not overlap
    we must instantiate D before unifying with the signature in the
    data instance declaration

DF4 We also (redundantly) check that any user-specified return kind
    signature in the data instance also obeys DT4/DT5.  For example we
    reject
        data family T1 :: Type -> Type -> Type
        data instance T1 Int :: Type -> F Int
    even if (F Int ~ Type).  We could omit this check, because we
    use the master kind; but it seems more uniform to check it, again
    with checkDataKindSig.

DF5 Data /family/ return kind restrictions. Consider
       data family D8 a :: F a
    where F is a type family.  No data/newtype instance can instantiate
    this so that it obeys the rules of DT4 or DT5.  So GHC proactively
    rejects the data /family/ declaration if it can never satisfy (DT4)/(DT5).
    Remember that a data family supports both data and newtype instances.

    More precisely, the return kind of a data family must be either
        * TYPE xyz (for some type xyz) or
        * a kind variable
    Only in these cases can a data/newtype instance possibly satisfy (DT4)/(DT5).
    This is checked by the call to checkDataKindSig in tcFamDecl1.  Examples:

      data family D1 :: Type              -- good
      data family D2 :: Bool -> Type      -- good
      data family D3 k :: k               -- good
      data family D4 :: forall k -> k     -- good
      data family D5 :: forall k. k -> k  -- good
      data family D6 :: forall r. TYPE r  -- good
      data family D7 :: Bool -> STAR      -- bad (see STAR from point 5)

DF6 Two return kinds for instances: If an instance has two return kinds,
    one from the family declaration and one from the instance declaration
    (see point DF3 above), they are unified. More accurately, we make sure
    that the kind of the applied data family is a subkind of the user-written
    kind. GHC.Tc.Gen.HsType.checkExpectedKind normally does this check for types, but
    that's overkill for our needs here. Instead, we just instantiate any
    invisible binders in the (instantiated) kind of the data family
    (called lhs_kind in tcDataFamInstHeader) with tcInstInvisibleTyBinders
    and then unify the resulting kind with the kind written by the user.
    This unification naturally produces a coercion, which we can drop, as
    the kind annotation on the instance is redundant (except perhaps for
    effects of unification).

    This all is Wrinkle (3) in Note [Implementation of UnliftedNewtypes].

Note [Liberalising data family return kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Could we allow this?
   type family F a where { F Int = Type }
   data family T a :: F a
   data instance T Int where
      MkT :: T Int

In the 'data instance', T Int :: F Int, and F Int = Type, so all seems
well.  But there are lots of complications:

* The representation constructor R:TInt presumably has kind Type.
  So the axiom connecting the two would have to look like
       axTInt :: T Int ~ R:TInt |> sym axFInt
  and that doesn't match expectation in DataFamInstTyCon
  in AlgTyConFlav

* The wrapper can't have type
     $WMkT :: Int -> T Int
  because T Int has the wrong kind.  It would have to be
     $WMkT :: Int -> (T Int) |> axFInt

* The code for $WMkT would also be more complicated, needing
  two coherence coercions. Try it!

* Code for pattern matching would be complicated in an
  exactly dual way.

So yes, we could allow this, but we currently do not. That's
why we have DF2 in Note [Data family/instance return kinds].

Note [Implementation of UnliftedNewtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Expected behavior of UnliftedNewtypes:

* Proposal: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0013-unlifted-newtypes.rst
* Discussion: https://github.com/ghc-proposals/ghc-proposals/pull/98

What follows is a high-level overview of the implementation of the
proposal.

STEP 1: Getting the initial kind, as done by inferInitialKind. We have
two sub-cases:

* With a SAK/CUSK: no change in kind-checking; the tycon is given the kind
  the user writes, whatever it may be.

* Without a SAK/CUSK: If there is no kind signature, the tycon is given
  a kind `TYPE r`, for a fresh unification variable `r`. We do this even
  when -XUnliftedNewtypes is not on; see <Error Messages>, below.

STEP 2: Kind-checking, as done by kcTyClDecl. This step is skipped for CUSKs.
The key function here is kcConDecl, which looks at an individual constructor
declaration. When we are processing a newtype (but whether or not -XUnliftedNewtypes
is enabled; see <Error Messages>, below), we generate a correct ContextKind
for the checking argument types: see getArgExpKind.

Examples of newtypes affected by STEP 2, assuming -XUnliftedNewtypes is
enabled (we use r0 to denote a unification variable):

newtype Foo rep = MkFoo (forall (a :: TYPE rep). a)
+ kcConDecl unifies (TYPE r0) with (TYPE rep), where (TYPE r0)
  is the kind that inferInitialKind invented for (Foo rep).

data Color = Red | Blue
type family Interpret (x :: Color) :: RuntimeRep where
  Interpret 'Red = 'IntRep
  Interpret 'Blue = 'WordRep
data family Foo (x :: Color) :: TYPE (Interpret x)
newtype instance Foo 'Red = FooRedC Int#
+ kcConDecl unifies TYPE (Interpret 'Red) with TYPE 'IntRep

Note that, in the GADT case, we might have a kind signature with arrows
(newtype XYZ a b :: Type -> Type where ...). We want only the final
component of the kind for checking in kcConDecl, so we call etaExpandAlgTyCon
in kcTyClDecl.

STEP 3: Type-checking (desugaring), as done by tcTyClDecl. The key function
here is tcConDecl. Once again, we must use getArgExpKind to ensure that the
representation type's kind matches that of the newtype, for two reasons:

  A. It is possible that a GADT has a CUSK. (Note that this is *not*
     possible for H98 types.) Recall that CUSK types don't go through
     kcTyClDecl, so we might not have done this kind check.
  B. We need to produce the coercion to put on the argument type
     if the kinds are different (for both H98 and GADT).

Example of (B):

type family F a where
  F Int = LiftedRep

newtype N :: TYPE (F Int) where
  MkN :: Int -> N

We really need to have the argument to MkN be (Int |> TYPE (sym axF)), where
axF :: F Int ~ LiftedRep. That way, the argument kind is the same as the
newtype kind, which is the principal correctness condition for newtypes.

Wrinkle: Consider (#17021, typecheck/should_fail/T17021)

    type family Id (x :: a) :: a where
      Id x = x

    newtype T :: TYPE (Id LiftedRep) where
      MkT :: Int -> T

  In the type of MkT, we must end with (Int |> TYPE (sym axId)) -> T,
  never Int -> (T |> TYPE axId); otherwise, the result type of the
  constructor wouldn't match the datatype. However, type-checking the
  HsType T might reasonably result in (T |> hole). We thus must ensure
  that this cast is dropped, forcing the type-checker to add one to
  the Int instead.

  Why is it always safe to drop the cast? This result type is type-checked by
  tcHsOpenType, so its kind definitely looks like TYPE r, for some r. It is
  important that even after dropping the cast, the type's kind has the form
  TYPE r. This is guaranteed by restrictions on the kinds of datatypes.
  For example, a declaration like `newtype T :: Id Type` is rejected: a
  newtype's final kind always has the form TYPE r, just as we want.

Note that this is possible in the H98 case only for a data family, because
the H98 syntax doesn't permit a kind signature on the newtype itself.

There are also some changes for dealing with families:

1. In tcFamDecl1, we suppress a tcIsLiftedTypeKind check if
   UnliftedNewtypes is on. This allows us to write things like:
     data family Foo :: TYPE 'IntRep

2. In a newtype instance (with -XUnliftedNewtypes), if the user does
   not write a kind signature, we want to allow the possibility that
   the kind is not Type, so we use newOpenTypeKind instead of liftedTypeKind.
   This is done in tcDataFamInstHeader in GHC.Tc.TyCl.Instance. Example:

       data family Bar (a :: RuntimeRep) :: TYPE a
       newtype instance Bar 'IntRep = BarIntC Int#
       newtype instance Bar 'WordRep :: TYPE 'WordRep where
         BarWordC :: Word# -> Bar 'WordRep

   The data instance corresponding to IntRep does not specify a kind signature,
   so tc_kind_sig just returns `TYPE r0` (where `r0` is a fresh metavariable).
   The data instance corresponding to WordRep does have a kind signature, so
   we use that kind signature.

3. A data family and its newtype instance may be declared with slightly
   different kinds. See point DF6 in Note [Data family/instance return kinds]

There's also a change in the renamer:

* In GHC.RenameSource.rnTyClDecl, enabling UnliftedNewtypes changes what is means
  for a newtype to have a CUSK. This is necessary since UnliftedNewtypes
  means that, for newtypes without kind signatures, we must use the field
  inside the data constructor to determine the result kind.
  See Note [Unlifted Newtypes and CUSKs] for more detail.

For completeness, it was also necessary to make coerce work on
unlifted types, resolving #13595.

<Error Messages>: It's tempting to think that the expected kind for a newtype
constructor argument when -XUnliftedNewtypes is *not* enabled should just be Type.
But this leads to difficulty in suggesting to enable UnliftedNewtypes. Here is
an example:

  newtype A = MkA Int#

If we expect the argument to MkA to have kind Type, then we get a kind-mismatch
error. The problem is that there is no way to connect this mismatch error to
-XUnliftedNewtypes, and suggest enabling the extension. So, instead, we allow
the A to type-check, but then find the problem when doing validity checking (and
where we get make a suitable error message). One potential worry is

  {-# LANGUAGE PolyKinds #-}
  newtype B a = MkB a

This turns out OK, because unconstrained RuntimeReps default to LiftedRep, just
as we would like. Another potential problem comes in a case like

  -- no UnliftedNewtypes

  data family D :: k
  newtype instance D = MkD Any

Here, we want inference to tell us that k should be instantiated to Type in
the instance. With the approach described here (checking for Type only in
the validity checker), that will not happen. But I cannot think of a non-contrived
example that will notice this lack of inference, so it seems better to improve
error messages than be able to infer this instantiation.

-}

tcTyClDecl :: RolesInfo -> LTyClDecl GhcRn -> TcM (TyCon, [DerivInfo])
tcTyClDecl roles_info (L loc decl)
  | Just thing <- wiredInNameTyThing_maybe (tcdName decl)
  = case thing of -- See Note [Declarations for wired-in things]
      ATyCon tc -> return (tc, wiredInDerivInfo tc decl)
      _ -> pprPanic "tcTyClDecl" (ppr thing)

  | otherwise
  = setSrcSpan loc $ tcAddDeclCtxt decl $
    do { traceTc "---- tcTyClDecl ---- {" (ppr decl)
       ; (tc, deriv_infos) <- tcTyClDecl1 Nothing roles_info decl
       ; traceTc "---- tcTyClDecl end ---- }" (ppr tc)
       ; return (tc, deriv_infos) }

noDerivInfos :: a -> (a, [DerivInfo])
noDerivInfos a = (a, [])

wiredInDerivInfo :: TyCon -> TyClDecl GhcRn -> [DerivInfo]
wiredInDerivInfo tycon decl
  | DataDecl { tcdDataDefn = dataDefn } <- decl
  , HsDataDefn { dd_derivs = derivs } <- dataDefn
  = [ DerivInfo { di_rep_tc = tycon
                , di_scoped_tvs =
                    if isFunTyCon tycon || isPrimTyCon tycon
                       then []  -- no tyConTyVars
                       else mkTyVarNamePairs (tyConTyVars tycon)
                , di_clauses = unLoc derivs
                , di_ctxt = tcMkDeclCtxt decl } ]
wiredInDerivInfo _ _ = []

  -- "type family" declarations
tcTyClDecl1 :: Maybe Class -> RolesInfo -> TyClDecl GhcRn -> TcM (TyCon, [DerivInfo])
tcTyClDecl1 parent _roles_info (FamDecl { tcdFam = fd })
  = fmap noDerivInfos $
    tcFamDecl1 parent fd

  -- "type" synonym declaration
tcTyClDecl1 _parent roles_info
            (SynDecl { tcdLName = L _ tc_name
                     , tcdRhs   = rhs })
  = ASSERT( isNothing _parent )
    fmap noDerivInfos $
    tcTySynRhs roles_info tc_name rhs

  -- "data/newtype" declaration
tcTyClDecl1 _parent roles_info
            decl@(DataDecl { tcdLName = L _ tc_name
                           , tcdDataDefn = defn })
  = ASSERT( isNothing _parent )
    tcDataDefn (tcMkDeclCtxt decl) roles_info tc_name defn

tcTyClDecl1 _parent roles_info
            (ClassDecl { tcdLName = L _ class_name
                       , tcdCtxt = hs_ctxt
                       , tcdMeths = meths
                       , tcdFDs = fundeps
                       , tcdSigs = sigs
                       , tcdATs = ats
                       , tcdATDefs = at_defs })
  = ASSERT( isNothing _parent )
    do { clas <- tcClassDecl1 roles_info class_name hs_ctxt
                              meths fundeps sigs ats at_defs
       ; return (noDerivInfos (classTyCon clas)) }


{- *********************************************************************
*                                                                      *
          Class declarations
*                                                                      *
********************************************************************* -}

tcClassDecl1 :: RolesInfo -> Name -> LHsContext GhcRn
             -> LHsBinds GhcRn -> [LHsFunDep GhcRn] -> [LSig GhcRn]
             -> [LFamilyDecl GhcRn] -> [LTyFamDefltDecl GhcRn]
             -> TcM Class
tcClassDecl1 roles_info class_name hs_ctxt meths fundeps sigs ats at_defs
  = fixM $ \ clas ->
    -- We need the knot because 'clas' is passed into tcClassATs
    bindTyClTyVars class_name $ \ _ binders res_kind ->
    do { checkClassKindSig res_kind
       ; traceTc "tcClassDecl 1" (ppr class_name $$ ppr binders)
       ; let tycon_name = class_name        -- We use the same name
             roles = roles_info tycon_name  -- for TyCon and Class

       ; (ctxt, fds, sig_stuff, at_stuff)
            <- pushLevelAndSolveEqualities skol_info (binderVars binders) $
               -- The (binderVars binders) is needed bring into scope the
               -- skolems bound by the class decl header (#17841)
               do { ctxt <- tcHsContext hs_ctxt
                  ; fds  <- mapM (addLocM tc_fundep) fundeps
                  ; sig_stuff <- tcClassSigs class_name sigs meths
                  ; at_stuff  <- tcClassATs class_name clas ats at_defs
                  ; return (ctxt, fds, sig_stuff, at_stuff) }


       -- The pushLevelAndSolveEqualities will report errors for any
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
    skol_info = TyConSkol ClassFlavour class_name
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

tcClassATs :: Name                    -- The class name (not knot-tied)
           -> Class                   -- The class parent of this associated type
           -> [LFamilyDecl GhcRn]     -- Associated types.
           -> [LTyFamDefltDecl GhcRn] -- Associated type defaults.
           -> TcM [ClassATItem]
tcClassATs class_name cls ats at_defs
  = do {  -- Complain about associated type defaults for non associated-types
         sequence_ [ failWithTc (badATErr class_name n)
                   | n <- map at_def_tycon at_defs
                   , not (n `elemNameSet` at_names) ]
       ; mapM tc_at ats }
  where
    at_def_tycon :: LTyFamDefltDecl GhcRn -> Name
    at_def_tycon = tyFamInstDeclName . unLoc

    at_fam_name :: LFamilyDecl GhcRn -> Name
    at_fam_name = familyDeclName . unLoc

    at_names = mkNameSet (map at_fam_name ats)

    at_defs_map :: NameEnv [LTyFamDefltDecl GhcRn]
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
tcDefaultAssocDecl ::
     TyCon                                       -- ^ Family TyCon (not knot-tied)
  -> [LTyFamDefltDecl GhcRn]                     -- ^ Defaults
  -> TcM (Maybe (KnotTied Type, ATValidityInfo)) -- ^ Type checked RHS
tcDefaultAssocDecl _ []
  = return Nothing  -- No default declaration

tcDefaultAssocDecl _ (d1:_:_)
  = failWithTc (text "More than one default declaration for"
                <+> ppr (tyFamInstDeclName (unLoc d1)))

tcDefaultAssocDecl fam_tc
  [L loc (TyFamInstDecl { tfid_eqn =
                            FamEqn { feqn_tycon = L _ tc_name
                                   , feqn_bndrs = outer_bndrs
                                   , feqn_pats  = hs_pats
                                   , feqn_rhs   = hs_rhs_ty }})]
  = -- See Note [Type-checking default assoc decls]
    setSrcSpan loc $
    tcAddFamInstCtxt (text "default type instance") tc_name $
    do { traceTc "tcDefaultAssocDecl 1" (ppr tc_name)
       ; let fam_tc_name = tyConName fam_tc
             vis_arity = length (tyConVisibleTyVars fam_tc)
             vis_pats  = numVisibleArgs hs_pats

       -- Kind of family check
       ; ASSERT( fam_tc_name == tc_name )
         checkTc (isTypeFamilyTyCon fam_tc) (wrongKindOfFamily fam_tc)

       -- Arity check
       ; checkTc (vis_pats == vis_arity)
                 (wrongNumberOfParmsErr vis_arity)

       -- Typecheck RHS
       --
       -- You might think we should pass in some AssocInstInfo, as we're looking
       -- at an associated type. But this would be wrong, because an associated
       -- type default LHS can mention *different* type variables than the
       -- enclosing class. So it's treated more as a freestanding beast.
       ; (qtvs, pats, rhs_ty) <- tcTyFamInstEqnGuts fam_tc NotAssociated
                                      outer_bndrs hs_pats hs_rhs_ty

       ; let fam_tvs = tyConTyVars fam_tc
       ; traceTc "tcDefaultAssocDecl 2" (vcat
           [ text "hs_pats"   <+> ppr hs_pats
           , text "hs_rhs_ty" <+> ppr hs_rhs_ty
           , text "fam_tvs" <+> ppr fam_tvs
           , text "qtvs"    <+> ppr qtvs
             -- NB: Do *not* print `pats` or rhs_ty here, as they can mention
             -- knot-tied TyCons. See #18648.
           ])
       ; let subst = case traverse getTyVar_maybe pats of
                       Just cpt_tvs -> zipTvSubst cpt_tvs (mkTyVarTys fam_tvs)
                       Nothing      -> emptyTCvSubst
                       -- The Nothing case can only be reached in invalid
                       -- associated type family defaults. In such cases, we
                       -- simply create an empty substitution and let GHC fall
                       -- over later, in GHC.Tc.Validity.checkValidAssocTyFamDeflt.
                       -- See Note [Type-checking default assoc decls].
       ; pure $ Just (substTyUnchecked subst rhs_ty, ATVI loc pats)
           -- We perform checks for well-formedness and validity later, in
           -- GHC.Tc.Validity.checkValidAssocTyFamDeflt.
     }

{- Note [Type-checking default assoc decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this default declaration for an associated type

   class C a where
      type F (a :: k) b :: Type
      type F (x :: j) y = Proxy x -> y

Note that the class variable 'a' doesn't scope over the default assoc
decl, nor do the type variables `k` and `b`. Instead, the default decl is
treated more like a top-level type instance. However, we store the default rhs
(Proxy x -> y) in F's TyCon, using F's own type variables, so we need to
convert it to (Proxy a -> b). We do this in the tcDefaultAssocDecl function by
creating a substitution [j |-> k, x |-> a, b |-> y] and applying this
substitution to the RHS.

In order to create this substitution, we must first ensure that all of
the arguments in the default instance consist of distinct type variables.
Checking for this property proves surprisingly tricky. Three potential places
where GHC could check for this property include:

1. Before typechecking (in the parser or renamer)
2. During typechecking (in tcDefaultAssocDecl)
3. After typechecking (using GHC.Tc.Validity)

Currently, GHC picks option (3) and implements this check using
GHC.Tc.Validity.checkValidAssocTyFamDeflt. GHC previously used options (1) and
(2), but neither option quite worked out for reasons that we will explain
shortly.

The first thing that checkValidAssocTyFamDeflt does is check that all arguments
in an associated type family default are type variables. As a motivating
example, consider this erroneous program (inspired by #11361):

   class C a where
      type F (a :: k) b :: Type
      type F x        b = x

If you squint, you'll notice that the kind of `x` is actually Type. However,
we cannot substitute from [Type |-> k], so we reject this default. This also
explains why GHC no longer implements option (1) above, since figuring out that
`x`'s kind is Type would be much more difficult without the knowledge that the
typechecker provides.

Next, checkValidAssocTyFamDeflt checks that all arguments are distinct. Here is
another offending example, this time taken from #13971:

   class C2 (a :: j) where
      type F2 (a :: j) (b :: k)
      type F2 (x :: z) y = SameKind x y
   data SameKind :: k -> k -> Type

All of the arguments in the default equation for `F2` are type variables, so
that passes the first check. However, if we were to build this substitution,
then both `j` and `k` map to `z`! In terms of visible kind application, it's as
if we had written `type F2 @z @z x y = SameKind @z x y`, which makes it clear
that we have duplicated a use of `z` on the LHS. Therefore, `F2`'s default is
also rejected.

There is one more design consideration in play here: what error message should
checkValidAssocTyFamDeflt produce if one of its checks fails? Ideally, it would
be something like this:

  Illegal duplicate variable ‘z’ in:
    ‘type F2 @z @z x y = ...’
    The arguments to ‘F2’ must all be distinct type variables

This requires printing out the arguments to the associated type family. This
can be dangerous, however. Consider this example, adapted from #18648:

  class C3 a where
     type F3 a
     type F3 (F3 a) = a

F3's default is illegal, since its argument is not a bare type variable. But
note that when we typecheck F3's default, the F3 type constructor is knot-tied.
Therefore, if we print the type `F3 a` in an error message, GHC will diverge!
This is the reason why GHC no longer implements option (2) above and instead
waits until /after/ typechecking has finished, at which point the typechecker
knot has been worked out.

As one final point, one might worry that the typechecker knot could cause the
substitution that tcDefaultAssocDecl creates to diverge, but this is not the
case. Since the LHS of a valid associated type family default is always just
variables, it won't contain any tycons. Accordingly, the patterns used in the
substitution won't actually be knot-tied, even though we're in the knot. (This
is too delicate for my taste, but it works.) If we're dealing with /invalid/
default, such as F3's above, then we simply create an empty substitution and
rely on checkValidAssocTyFamDeflt throwing an error message afterwards before
any damage is done.
-}

{- *********************************************************************
*                                                                      *
          Type family declarations
*                                                                      *
********************************************************************* -}

tcFamDecl1 :: Maybe Class -> FamilyDecl GhcRn -> TcM TyCon
tcFamDecl1 parent (FamilyDecl { fdInfo = fam_info
                              , fdLName = tc_lname@(L _ tc_name)
                              , fdResultSig = L _ sig
                              , fdInjectivityAnn = inj })
  | DataFamily <- fam_info
  = bindTyClTyVars tc_name $ \ _ binders res_kind -> do
  { traceTc "data family:" (ppr tc_name)
  ; checkFamFlag tc_name

  -- Check that the result kind is OK
  -- We allow things like
  --   data family T (a :: Type) :: forall k. k -> Type
  -- We treat T as having arity 1, but result kind forall k. k -> Type
  -- But we want to check that the result kind finishes in
  --   Type or a kind-variable
  -- For the latter, consider
  --   data family D a :: forall k. Type -> k
  -- When UnliftedNewtypes is enabled, we loosen this restriction
  -- on the return kind. See Note [Implementation of UnliftedNewtypes], wrinkle (1).
  -- See also Note [Datatype return kinds]
  ; checkDataKindSig DataFamilySort res_kind
  ; tc_rep_name <- newTyConRepName tc_name
  ; let inj   = Injective $ replicate (length binders) True
        tycon = mkFamilyTyCon tc_name binders
                              res_kind
                              (resultVariableName sig)
                              (DataFamilyTyCon tc_rep_name)
                              parent inj
  ; return tycon }

  | OpenTypeFamily <- fam_info
  = bindTyClTyVars tc_name $ \ _ binders res_kind -> do
  { traceTc "open type family:" (ppr tc_name)
  ; checkFamFlag tc_name
  ; inj' <- tcInjectivity binders inj
  ; checkResultSigFlag tc_name sig  -- check after injectivity for better errors
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
            <- bindTyClTyVars tc_name $ \ _ binders res_kind ->
               do { inj' <- tcInjectivity binders inj
                  ; return (inj', binders, res_kind) }

       ; checkFamFlag tc_name -- make sure we have -XTypeFamilies
       ; checkResultSigFlag tc_name sig

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
       ; let tc_fam_tc = mkTcTyCon tc_name binders res_kind
                                   noTcTyConScopedTyVars
                                   False {- this doesn't matter here -}
                                   ClosedTypeFamilyFlavour

       ; branches <- mapAndReportM (tcTyFamInstEqn tc_fam_tc NotAssociated) eqns
         -- Do not attempt to drop equations dominated by earlier
         -- ones here; in the case of mutual recursion with a data
         -- type, we get a knot-tying failure.  Instead we check
         -- for this afterwards, in GHC.Tc.Validity.checkValidCoAxiom
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

#if __GLASGOW_HASKELL__ <= 810
  | otherwise = panic "tcFamInst1"  -- Silence pattern-exhaustiveness checker
#endif

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

tcTySynRhs :: RolesInfo -> Name
           -> LHsType GhcRn -> TcM TyCon
tcTySynRhs roles_info tc_name hs_ty
  = bindTyClTyVars tc_name $ \ _ binders res_kind ->
    do { env <- getLclEnv
       ; traceTc "tc-syn" (ppr tc_name $$ ppr (tcl_env env))
       ; rhs_ty <- pushLevelAndSolveEqualities skol_info (binderVars binders) $
                   tcCheckLHsType hs_ty (TheKind res_kind)
       ; rhs_ty <- zonkTcTypeToType rhs_ty
       ; let roles = roles_info tc_name
       ; return (buildSynTyCon tc_name binders res_kind roles rhs_ty) }
  where
    skol_info = TyConSkol TypeSynonymFlavour tc_name

tcDataDefn :: SDoc -> RolesInfo -> Name
           -> HsDataDefn GhcRn -> TcM (TyCon, [DerivInfo])
  -- NB: not used for newtype/data instances (whether associated or not)
tcDataDefn err_ctxt roles_info tc_name
           (HsDataDefn { dd_ND = new_or_data, dd_cType = cType
                       , dd_ctxt = ctxt
                       , dd_kindSig = mb_ksig  -- Already in tc's kind
                                               -- via inferInitialKinds
                       , dd_cons = cons
                       , dd_derivs = derivs })
  = bindTyClTyVars tc_name $ \ tctc tycon_binders res_kind ->
       -- 'tctc' is a 'TcTyCon' and has the 'tcTyConScopedTyVars' that we need
       -- unlike the finalized 'tycon' defined above which is an 'AlgTyCon'
       --
       -- The TyCon tyvars must scope over
       --    - the stupid theta (dd_ctxt)
       --    - for H98 constructors only, the ConDecl
       -- But it does no harm to bring them into scope
       -- over GADT ConDecls as well; and it's awkward not to
    do { gadt_syntax <- dataDeclChecks tc_name new_or_data ctxt cons
         -- see Note [Datatype return kinds]
       ; (extra_bndrs, final_res_kind) <- etaExpandAlgTyCon tycon_binders res_kind

       ; tcg_env <- getGblEnv
       ; let hsc_src = tcg_src tcg_env
       ; unless (mk_permissive_kind hsc_src cons) $
         checkDataKindSig (DataDeclSort new_or_data) final_res_kind

       ; let skol_tvs = binderVars tycon_binders
       ; stupid_tc_theta <- pushLevelAndSolveEqualities skol_info skol_tvs $
                            tcHsContext ctxt
       ; stupid_theta    <- zonkTcTypesToTypes stupid_tc_theta
       ; kind_signatures <- xoptM LangExt.KindSignatures

             -- Check that we don't use kind signatures without Glasgow extensions
       ; when (isJust mb_ksig) $
         checkTc (kind_signatures) (badSigTyDecl tc_name)

       ; tycon <- fixM $ \ rec_tycon -> do
             { let final_bndrs = tycon_binders `chkAppend` extra_bndrs
                   roles       = roles_info tc_name
             ; data_cons <- tcConDecls
                              new_or_data DDataType
                              rec_tycon final_bndrs final_res_kind
                              cons
             ; tc_rhs    <- mk_tc_rhs hsc_src rec_tycon data_cons
             ; tc_rep_nm <- newTyConRepName tc_name
             ; return (mkAlgTyCon tc_name
                                  final_bndrs
                                  final_res_kind
                                  roles
                                  (fmap unLoc cType)
                                  stupid_theta tc_rhs
                                  (VanillaAlgTyCon tc_rep_nm)
                                  gadt_syntax) }
       ; let deriv_info = DerivInfo { di_rep_tc = tycon
                                    , di_scoped_tvs = tcTyConScopedTyVars tctc
                                    , di_clauses = unLoc derivs
                                    , di_ctxt = err_ctxt }
       ; traceTc "tcDataDefn" (ppr tc_name $$ ppr tycon_binders $$ ppr extra_bndrs)
       ; return (tycon, [deriv_info]) }
  where
    skol_info = TyConSkol flav tc_name
    flav = newOrDataToFlavour new_or_data

    -- Abstract data types in hsig files can have arbitrary kinds,
    -- because they may be implemented by type synonyms
    -- (which themselves can have arbitrary kinds, not just *). See #13955.
    --
    -- Note that this is only a property that data type declarations possess,
    -- so one could not have, say, a data family instance in an hsig file that
    -- has kind `Bool`. Therefore, this check need only occur in the code that
    -- typechecks data type declarations.
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


-------------------------
kcTyFamInstEqn :: TcTyCon -> LTyFamInstEqn GhcRn -> TcM ()
-- Used for the equations of a closed type family only
-- Not used for data/type instances
kcTyFamInstEqn tc_fam_tc
    (L loc (FamEqn { feqn_tycon = L _ eqn_tc_name
                   , feqn_bndrs = outer_bndrs
                   , feqn_pats  = hs_pats
                   , feqn_rhs   = hs_rhs_ty }))
  = setSrcSpan loc $
    do { traceTc "kcTyFamInstEqn" (vcat
           [ text "tc_name ="    <+> ppr eqn_tc_name
           , text "fam_tc ="     <+> ppr tc_fam_tc <+> dcolon <+> ppr (tyConKind tc_fam_tc)
           , text "feqn_bndrs =" <+> ppr outer_bndrs
           , text "feqn_pats ="  <+> ppr hs_pats ])
          -- this check reports an arity error instead of a kind error; easier for user
       ; let vis_pats = numVisibleArgs hs_pats

       -- First, check if we're dealing with a closed type family equation, and
       -- if so, ensure that each equation's type constructor is for the right
       -- type family.  E.g. barf on
       --    type family F a where { G Int = Bool }
       ; checkTc (tc_fam_tc_name == eqn_tc_name) $
         wrongTyFamName tc_fam_tc_name eqn_tc_name

       ; checkTc (vis_pats == vis_arity) $
                  wrongNumberOfParmsErr vis_arity

       ; discardResult $
         bindOuterFamEqnTKBndrs_Q_Tv outer_bndrs $
         do { (_fam_app, res_kind) <- tcFamTyPats tc_fam_tc hs_pats
            ; tcCheckLHsType hs_rhs_ty (TheKind res_kind) }
             -- Why "_Tv" here?  Consider (#14066
             --  type family Bar x y where
             --      Bar (x :: a) (y :: b) = Int
             --      Bar (x :: c) (y :: d) = Bool
             -- During kind-checking, a,b,c,d should be TyVarTvs and unify appropriately
    }
  where
    vis_arity = length (tyConVisibleTyVars tc_fam_tc)
    tc_fam_tc_name = getName tc_fam_tc

--------------------------
tcTyFamInstEqn :: TcTyCon -> AssocInstInfo -> LTyFamInstEqn GhcRn
               -> TcM (KnotTied CoAxBranch)
-- Needs to be here, not in GHC.Tc.TyCl.Instance, because closed families
-- (typechecked here) have TyFamInstEqns

tcTyFamInstEqn fam_tc mb_clsinfo
    (L loc (FamEqn { feqn_bndrs  = outer_bndrs
                   , feqn_pats   = hs_pats
                   , feqn_rhs    = hs_rhs_ty }))
  = setSrcSpan loc $
    do { traceTc "tcTyFamInstEqn" $
         vcat [ ppr loc, ppr fam_tc <+> ppr hs_pats
              , text "fam tc bndrs" <+> pprTyVars (tyConTyVars fam_tc)
              , case mb_clsinfo of
                  NotAssociated {} -> empty
                  InClsInst { ai_class = cls } -> text "class" <+> ppr cls <+> pprTyVars (classTyVars cls) ]

       -- First, check the arity of visible arguments
       -- If we wait until validity checking, we'll get kind errors
       -- below when an arity error will be much easier to understand.
       -- Note that for closed type families, kcTyFamInstEqn has already
       -- checked the arity previously.
       ; let vis_arity = length (tyConVisibleTyVars fam_tc)
             vis_pats  = numVisibleArgs hs_pats
       ; checkTc (vis_pats == vis_arity) $
         wrongNumberOfParmsErr vis_arity
       ; (qtvs, pats, rhs_ty) <- tcTyFamInstEqnGuts fam_tc mb_clsinfo
                                      outer_bndrs hs_pats hs_rhs_ty
       -- Don't print results they may be knot-tied
       -- (tcFamInstEqnGuts zonks to Type)
       ; return (mkCoAxBranch qtvs [] [] pats rhs_ty
                              (map (const Nominal) qtvs)
                              loc) }

{- Note [Instantiating a family tycon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's possible that kind-checking the result of a family tycon applied to
its patterns will instantiate the tycon further. For example, we might
have

  type family F :: k where
    F = Int
    F = Maybe

After checking (F :: forall k. k) (with no visible patterns), we still need
to instantiate the k. With data family instances, this problem can be even
more intricate, due to Note [Arity of data families] in GHC.Core.FamInstEnv. See
indexed-types/should_compile/T12369 for an example.

So, the kind-checker must return the new skolems and args (that is, Type
or (Type -> Type) for the equations above) and the instantiated kind.

Note [Generalising in tcTyFamInstEqnGuts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have something like
  type instance forall (a::k) b. F (Proxy t1) _ = rhs

Then  imp_vars = [k], exp_bndrs = [a::k, b]

We want to quantify over all the free vars of the LHS including
  * any invisible kind variables arising from instantiating tycons,
    such as Proxy
  * wildcards such as '_' above

The wildcards are particularly awkward: they may need to be quantified
  - before the explicit variables k,a,b
  - after them
  - or even interleaved with them
  c.f. Note [Naughty quantification candidates] in GHC.Tc.Utils.TcMType

So, we use bindOuterFamEqnTKBndrs (which does not create an implication for
the telescope), and generalise over /all/ the variables in the LHS,
without treating the explicitly-quanfitifed ones specially. Wrinkles:

 - When generalising, include the explicit user-specified forall'd
   variables, so that we get an error from Validity.checkFamPatBinders
   if a forall'd variable is not bound on the LHS

 - We still want to complain about a bad telescope among the user-specified
   variables.  So in checkFamTelescope we emit an implication constraint
   quantifying only over them, purely so that we get a good telescope error.

  - Note that, unlike a type signature like
       f :: forall (a::k). blah
    we do /not/ care about the Inferred/Specified designation or order for
    the final quantified tyvars.  Type-family instances are not invoked
    directly in Haskell source code, so visible type application etc plays
    no role.

See also Note [Re-quantify type variables in rules] in
GHC.Tc.Gen.Rule, which explains a /very/ similar design when
generalising over the type of a rewrite rule.

-}

--------------------------
tcTyFamInstEqnGuts :: TyCon -> AssocInstInfo
                   -> HsOuterFamEqnTyVarBndrs GhcRn     -- Implicit and explicit binders
                   -> HsTyPats GhcRn                    -- Patterns
                   -> LHsType GhcRn                     -- RHS
                   -> TcM ([TyVar], [TcType], TcType)   -- (tyvars, pats, rhs)
-- Used only for type families, not data families
tcTyFamInstEqnGuts fam_tc mb_clsinfo outer_hs_bndrs hs_pats hs_rhs_ty
  = do { traceTc "tcTyFamInstEqnGuts {" (ppr fam_tc)

       -- By now, for type families (but not data families) we should
       -- have checked that the number of patterns matches tyConArity

       -- This code is closely related to the code
       -- in GHC.Tc.Gen.HsType.kcCheckDeclHeader_cusk
       ; (tclvl, wanted, (outer_tvs, (lhs_ty, rhs_ty)))
               <- pushLevelAndSolveEqualitiesX "tcTyFamInstEqnGuts" $
                  bindOuterFamEqnTKBndrs outer_hs_bndrs             $
                  do { (lhs_ty, rhs_kind) <- tcFamTyPats fam_tc hs_pats
                       -- Ensure that the instance is consistent with its
                       -- parent class (#16008)
                     ; addConsistencyConstraints mb_clsinfo lhs_ty
                     ; rhs_ty <- tcCheckLHsType hs_rhs_ty (TheKind rhs_kind)
                     ; return (lhs_ty, rhs_ty) }

       -- This code (and the stuff immediately above) is very similar
       -- to that in tcDataFamInstHeader.  Maybe we should abstract the
       -- common code; but for the moment I concluded that it's
       -- clearer to duplicate it.  Still, if you fix a bug here,
       -- check there too!

       -- See Note [Generalising in tcTyFamInstEqnGuts]
       ; dvs  <- candidateQTyVarsOfTypes (lhs_ty : mkTyVarTys outer_tvs)
       ; qtvs <- quantifyTyVars dvs
       ; reportUnsolvedEqualities FamInstSkol qtvs tclvl wanted
       ; checkFamTelescope tclvl outer_hs_bndrs outer_tvs

       ; traceTc "tcTyFamInstEqnGuts 2" $
         vcat [ ppr fam_tc
              , text "lhs_ty"     <+> ppr lhs_ty
              , text "qtvs"       <+> pprTyVars qtvs ]

       ; (ze, qtvs) <- zonkTyBndrs qtvs
       ; lhs_ty     <- zonkTcTypeToTypeX ze lhs_ty
       ; rhs_ty     <- zonkTcTypeToTypeX ze rhs_ty

       ; let pats = unravelFamInstPats lhs_ty
             -- Note that we do this after solveEqualities
             -- so that any strange coercions inside lhs_ty
             -- have been solved before we attempt to unravel it
       ; traceTc "tcTyFamInstEqnGuts }" (ppr fam_tc <+> pprTyVars qtvs)
       ; return (qtvs, pats, rhs_ty) }


checkFamTelescope :: TcLevel -> HsOuterFamEqnTyVarBndrs GhcRn
                  -> [TcTyVar] -> TcM ()
-- Emit a constraint (forall a b c. <empty>), so that
-- we will do telescope-checking on a,b,c
-- See Note [Generalising in tcTyFamInstEqnGuts]
checkFamTelescope tclvl hs_outer_bndrs outer_tvs
  | HsOuterExplicit { hso_bndrs = bndrs } <- hs_outer_bndrs
  , (b_first : _) <- bndrs
  , let b_last    = last bndrs
        skol_info = ForAllSkol (fsep (map ppr bndrs))
  = setSrcSpan (combineSrcSpans (getLoc b_first) (getLoc b_last)) $
    emitResidualTvConstraint skol_info outer_tvs tclvl emptyWC
  | otherwise
  = return ()

-----------------
unravelFamInstPats :: TcType -> [TcType]
-- Decompose fam_app to get the argument patterns
--
-- We expect fam_app to look like (F t1 .. tn)
-- tcFamTyPats is capable of returning ((F ty1 |> co) ty2),
-- but that can't happen here because we already checked the
-- arity of F matches the number of pattern
unravelFamInstPats fam_app
  = case splitTyConApp_maybe fam_app of
      Just (_, pats) -> pats
      Nothing -> panic "unravelFamInstPats: Ill-typed LHS of family instance"
        -- The Nothing case cannot happen for type families, because
        -- we don't call unravelFamInstPats until we've solved the
        -- equalities. For data families, it shouldn't happen either,
        -- we need to fail hard and early if it does. See trac issue #15905
        -- for an example of this happening.

addConsistencyConstraints :: AssocInstInfo -> TcType -> TcM ()
-- In the corresponding positions of the class and type-family,
-- ensure the family argument is the same as the class argument
--   E.g    class C a b c d where
--             F c x y a :: Type
-- Here the first  arg of F should be the same as the third of C
--  and the fourth arg of F should be the same as the first of C
--
-- We emit /Derived/ constraints (a bit like fundeps) to encourage
-- unification to happen, but without actually reporting errors.
-- If, despite the efforts, corresponding positions do not match,
-- checkConsistentFamInst will complain
addConsistencyConstraints mb_clsinfo fam_app
  | InClsInst { ai_inst_env = inst_env } <- mb_clsinfo
  , Just (fam_tc, pats) <- tcSplitTyConApp_maybe fam_app
  = do { let eqs = [ (cls_ty, pat)
                   | (fam_tc_tv, pat) <- tyConTyVars fam_tc `zip` pats
                   , Just cls_ty <- [lookupVarEnv inst_env fam_tc_tv] ]
       ; traceTc "addConsistencyConstraints" (ppr eqs)
       ; emitDerivedEqs AssocFamPatOrigin eqs }
    -- Improve inference
    -- Any mis-match is reports by checkConsistentFamInst
  | otherwise
  = return ()

{- Note [Constraints in patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

dataDeclChecks :: Name -> NewOrData
               -> LHsContext GhcRn -> [LConDecl GhcRn]
               -> TcM Bool
dataDeclChecks tc_name new_or_data (L _ stupid_theta) cons
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
consUseGadtSyntax :: [LConDecl GhcRn] -> Bool
consUseGadtSyntax (L _ (ConDeclGADT {}) : _) = True
consUseGadtSyntax _                          = False
                 -- All constructors have same shape

-----------------------------------
data DataDeclInfo
  = DDataType      -- data T a b = T1 a | T2 b
  | DDataInstance  -- data instance D [a] = D1 a | D2
       Type        --   The header D [a]

mkDDHeaderTy :: DataDeclInfo -> TyCon -> [TyConBinder] -> Type
mkDDHeaderTy dd_info rep_tycon tc_bndrs
  = case dd_info of
      DDataType -> mkTyConApp rep_tycon $
                   mkTyVarTys (binderVars tc_bndrs)
      DDataInstance header_ty -> header_ty

tcConDecls :: NewOrData
           -> DataDeclInfo
           -> KnotTied TyCon            -- Representation TyCon
           -> [TyConBinder]             -- Binders of representation TyCon
           -> TcKind                    -- Result kind
           -> [LConDecl GhcRn] -> TcM [DataCon]
tcConDecls new_or_data dd_info rep_tycon tmpl_bndrs res_kind
  = concatMapM $ addLocM $
    tcConDecl new_or_data dd_info rep_tycon tmpl_bndrs res_kind
              (mkTyConTagMap rep_tycon)
    -- mkTyConTagMap: it's important that we pay for tag allocation here,
    -- once per TyCon. See Note [Constructor tag allocation], fixes #14657

tcConDecl :: NewOrData
          -> DataDeclInfo
          -> KnotTied TyCon   -- Representation tycon. Knot-tied!
          -> [TyConBinder]    -- Binders of representation TyCon
          -> TcKind           -- Result kind
          -> NameEnv ConTag
          -> ConDecl GhcRn
          -> TcM [DataCon]

tcConDecl new_or_data dd_info rep_tycon tc_bndrs res_kind tag_map
          (ConDeclH98 { con_name = lname@(L _ name)
                      , con_ex_tvs = explicit_tkv_nms
                      , con_mb_cxt = hs_ctxt
                      , con_args = hs_args })
  = addErrCtxt (dataConCtxt [lname]) $
    do { -- NB: the tyvars from the declaration header are in scope

         -- Get hold of the existential type variables
         -- e.g. data T a = forall k (b::k) f. MkT a (f b)
         -- Here tc_bndrs = {a}
         --      hs_qvars = HsQTvs { hsq_implicit = {k}
         --                        , hsq_explicit = {f,b} }

       ; traceTc "tcConDecl 1" (vcat [ ppr name, ppr explicit_tkv_nms ])

       ; (tclvl, wanted, (exp_tvbndrs, (ctxt, arg_tys, field_lbls, stricts)))
           <- pushLevelAndSolveEqualitiesX "tcConDecl:H98"  $
              tcExplicitTKBndrs explicit_tkv_nms            $
              do { ctxt <- tcHsMbContext hs_ctxt
                 ; let exp_kind = getArgExpKind new_or_data res_kind
                 ; btys <- tcConH98Args exp_kind hs_args
                 ; field_lbls <- lookupConstructorFields name
                 ; let (arg_tys, stricts) = unzip btys
                 ; return (ctxt, arg_tys, field_lbls, stricts)
                 }


       ; let tc_tvs   = binderVars tc_bndrs
             fake_ty  = mkSpecForAllTys  tc_tvs      $
                        mkInvisForAllTys exp_tvbndrs $
                        mkPhiTy ctxt $
                        mkVisFunTys arg_tys $
                        unitTy
             -- That type is a lie, of course. (It shouldn't end in ()!)
             -- And we could construct a proper result type from the info
             -- at hand. But the result would mention only the univ_tvs,
             -- and so it just creates more work to do it right. Really,
             -- we're only doing this to find the right kind variables to
             -- quantify over, and this type is fine for that purpose.

         -- exp_tvbndrs have explicit, user-written binding sites
         -- the kvs below are those kind variables entirely unmentioned by the user
         --   and discovered only by generalization

       ; kvs <- kindGeneralizeAll fake_ty

       ; let skol_tvs = tc_tvs ++ kvs ++ binderVars exp_tvbndrs
       ; reportUnsolvedEqualities skol_info skol_tvs tclvl wanted
             -- The skol_info claims that all the variables are bound
             -- by the data constructor decl, whereas actually the
             -- univ_tvs are bound by the data type decl itself.  It
             -- would be better to have a doubly-nested implication.
             -- But that just doesn't seem worth it.
             -- See test dependent/should_fail/T13780a

       -- Zonk to Types
       ; (ze, qkvs)          <- zonkTyBndrs kvs
       ; (ze, user_qtvbndrs) <- zonkTyVarBindersX ze exp_tvbndrs
       ; arg_tys             <- zonkScaledTcTypesToTypesX ze arg_tys
       ; ctxt                <- zonkTcTypesToTypesX ze ctxt

       -- Can't print univ_tvs, arg_tys etc, because we are inside the knot here
       ; traceTc "tcConDecl 2" (ppr name $$ ppr field_lbls)
       ; let univ_tvbs = tyConInvisTVBinders tc_bndrs
             ex_tvbs   = mkTyVarBinders InferredSpec qkvs ++ user_qtvbndrs
             ex_tvs    = binderVars ex_tvbs
                -- For H98 datatypes, the user-written tyvar binders are precisely
                -- the universals followed by the existentials.
                -- See Note [DataCon user type variable binders] in GHC.Core.DataCon.
             user_tvbs = univ_tvbs ++ ex_tvbs
             user_res_ty = mkDDHeaderTy dd_info rep_tycon tc_bndrs

       ; traceTc "tcConDecl 2" (ppr name)
       ; is_infix <- tcConIsInfixH98 name hs_args
       ; rep_nm   <- newTyConRepName name
       ; fam_envs <- tcGetFamInstEnvs
       ; dc <- buildDataCon fam_envs name is_infix rep_nm
                            stricts Nothing field_lbls
                            tc_tvs ex_tvs user_tvbs
                            [{- no eq_preds -}] ctxt arg_tys
                            user_res_ty rep_tycon tag_map
                  -- NB:  we put data_tc, the type constructor gotten from the
                  --      constructor type signature into the data constructor;
                  --      that way checkValidDataCon can complain if it's wrong.

       ; return [dc] }
  where
    skol_info = DataConSkol name

tcConDecl new_or_data dd_info rep_tycon tc_bndrs _res_kind tag_map
  -- NB: don't use res_kind here, as it's ill-scoped. Instead,
  -- we get the res_kind by typechecking the result type.
          (ConDeclGADT { con_names = names
                       , con_bndrs = L _ outer_hs_bndrs
                       , con_mb_cxt = cxt, con_g_args = hs_args
                       , con_res_ty = hs_res_ty })
  = addErrCtxt (dataConCtxt names) $
    do { traceTc "tcConDecl 1 gadt" (ppr names)
       ; let (L _ name : _) = names

       ; (tclvl, wanted, (outer_bndrs, (ctxt, arg_tys, res_ty, field_lbls, stricts)))
           <- pushLevelAndSolveEqualitiesX "tcConDecl:GADT" $
              tcOuterTKBndrs skol_info outer_hs_bndrs       $
              do { ctxt <- tcHsMbContext cxt
                 ; (res_ty, res_kind) <- tcInferLHsTypeKind hs_res_ty
                         -- See Note [GADT return kinds]

                 -- For data instances (only), ensure that the return type,
                 -- res_ty, is a substitution instance of the header.
                 -- See Note [GADT return types]
                 ; case dd_info of
                      DDataType -> return ()
                      DDataInstance hdr_ty ->
                        do { (subst, _meta_tvs) <- newMetaTyVars (binderVars tc_bndrs)
                           ; let head_shape = substTy subst hdr_ty
                           ; discardResult $
                             popErrCtxt $  -- Drop dataConCtxt
                             addErrCtxt (dataConResCtxt names) $
                             unifyType Nothing res_ty head_shape }

                   -- See Note [Datatype return kinds]
                 ; let exp_kind = getArgExpKind new_or_data res_kind
                 ; btys <- tcConGADTArgs exp_kind hs_args

                 ; let (arg_tys, stricts) = unzip btys
                 ; field_lbls <- lookupConstructorFields name
                 ; return (ctxt, arg_tys, res_ty, field_lbls, stricts)
                 }

       ; outer_tv_bndrs <- scopedSortOuter outer_bndrs

       ; tkvs <- kindGeneralizeAll (mkInvisForAllTys outer_tv_bndrs $
                                    mkPhiTy ctxt $
                                    mkVisFunTys arg_tys $
                                    res_ty)
       ; traceTc "tcConDecl:GADT" (ppr names $$ ppr res_ty $$ ppr tkvs)
       ; reportUnsolvedEqualities skol_info tkvs tclvl wanted

       ; let tvbndrs =  mkTyVarBinders InferredSpec tkvs ++ outer_tv_bndrs

             -- Zonk to Types
       ; (ze, tvbndrs) <- zonkTyVarBinders       tvbndrs
       ; arg_tys       <- zonkScaledTcTypesToTypesX ze arg_tys
       ; ctxt          <- zonkTcTypesToTypesX ze ctxt
       ; res_ty        <- zonkTcTypeToTypeX   ze res_ty

       ; let res_tmpl = mkDDHeaderTy dd_info rep_tycon tc_bndrs
             (univ_tvs, ex_tvs, tvbndrs', eq_preds, arg_subst)
               = rejigConRes tc_bndrs res_tmpl tvbndrs res_ty
             -- See Note [rejigConRes]

             ctxt'      = substTys arg_subst ctxt
             arg_tys'   = substScaledTys arg_subst arg_tys
             res_ty'    = substTy  arg_subst res_ty

       -- Can't print univ_tvs, arg_tys etc, because we are inside the knot here
       ; traceTc "tcConDecl 2" (ppr names $$ ppr field_lbls)
       ; fam_envs <- tcGetFamInstEnvs
       ; let
           buildOneDataCon (L _ name) = do
             { is_infix <- tcConIsInfixGADT name hs_args
             ; rep_nm   <- newTyConRepName name

             ; buildDataCon fam_envs name is_infix
                            rep_nm
                            stricts Nothing field_lbls
                            univ_tvs ex_tvs tvbndrs' eq_preds
                            ctxt' arg_tys' res_ty' rep_tycon tag_map
                  -- NB:  we put data_tc, the type constructor gotten from the
                  --      constructor type signature into the data constructor;
                  --      that way checkValidDataCon can complain if it's wrong.
             }
       ; mapM buildOneDataCon names }
  where
    skol_info = DataConSkol (unLoc (head names))

{- Note [GADT return types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data family T :: forall k. k -> Type
  data instance T (a :: Type) where
    MkT :: forall b. T b

What kind does `b` have in the signature for MkT?
Since the return type must be an instance of the type in the header,
we must have (b :: Type), but you can't tell that by looking only at
the type of the data constructor; you have to look at the header too.
If you wrote it out fully, it'd look like
  data instance T @Type (a :: Type) where
    MkT :: forall (b::Type). T @Type b

We could reject the program, and expect the user to add kind
annotations to `MkT` to restrict the signature.  But an easy and
helpful alternative is this: simply instantiate the type from the
header with fresh unification variables, and unify with the return
type of `MkT`. That will force `b` to have kind `Type`.  See #8707
and #14111.

Wrikles
* At first sight it looks as though this would completely subsume the
  return-type check in checkValidDataCon.  But it does not. Suppose we
  have
     data instance T [a] where
        MkT :: T (F (Maybe a))

  where F is a type function.  Then maybe (F (Maybe a)) evaluates to
  [a], so unifyType will succeed.  But we discard the coercion
  returned by unifyType; and we really don't want to accept this
  program.  The check in checkValidDataCon will, however, reject it.
  TL;DR: keep the check in checkValidDataCon.

* Consider a data type, rather than a data instance, declaration
     data S a where { MkS :: b -> S [b]  }
  In tcConDecl, S is knot-tied, so we don't want to unify (S alpha)
  with (S [b]). To put it another way, unifyType should never see a
  TcTycon.  Simple solution: do *not* do the extra unifyType for
  data types (DDataType) only for data instances (DDataInstance); in
  the latter the family constructor is not knot-tied so there is no
  problem.

* Consider this (from an earlier form of GHC itself):

     data Pass = Parsed | ...
     data GhcPass (c :: Pass) where
       GhcPs :: GhcPs
       ...
     type GhcPs   = GhcPass 'Parsed

   Now GhcPs and GhcPass are mutually recursive. If we did unifyType
   for datatypes like GhcPass, we would not be able to expand the type
   synonym (it'd still be a TcTyCon).  So again, we don't do unifyType
   for data types; we leave it to checkValidDataCon.

   We /do/ perform the unifyType for data /instances/, but a data
   instance doesn't declare a new (user-visible) type constructor, so
   there is no mutual recursion with type synonyms to worry about.
   All good.

   TL;DR we do support mutual recursion between type synonyms and
   data type/instance declarations, as above.

Note [GADT return kinds]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   type family Star where Star = Type
   data T :: Type where
      MkT :: Int -> T

If, for some stupid reason, tcInferLHsTypeKind on the return type of
MkT returned (T |> ax, Star), then the return-type check in
checkValidDataCon would reject the decl (although of course there is
nothing wrong with it).  We are implicitly requiring tha
tcInferLHsTypeKind doesn't any gratuitous top-level casts.
-}

-- | Produce an "expected kind" for the arguments of a data/newtype.
-- If the declaration is indeed for a newtype,
-- then this expected kind will be the kind provided. Otherwise,
-- it is OpenKind for datatypes and liftedTypeKind.
-- Why do we not check for -XUnliftedNewtypes? See point <Error Messages>
-- in Note [Implementation of UnliftedNewtypes]
getArgExpKind :: NewOrData -> Kind -> ContextKind
getArgExpKind NewType res_ki = TheKind res_ki
getArgExpKind DataType _     = OpenKind

tcConIsInfixH98 :: Name
             -> HsConDeclH98Details GhcRn
             -> TcM Bool
tcConIsInfixH98 _   details
  = case details of
           InfixCon{}  -> return True
           RecCon{}    -> return False
           PrefixCon{} -> return False

tcConIsInfixGADT :: Name
             -> HsConDeclGADTDetails GhcRn
             -> TcM Bool
tcConIsInfixGADT con details
  = case details of
           RecConGADT{} -> return False
           PrefixConGADT arg_tys       -- See Note [Infix GADT constructors]
               | isSymOcc (getOccName con)
               , [_ty1,_ty2] <- map hsScaledThing arg_tys
                  -> do { fix_env <- getFixityEnv
                        ; return (con `elemNameEnv` fix_env) }
               | otherwise -> return False

tcConH98Args :: ContextKind  -- expected kind of arguments
                             -- always OpenKind for datatypes, but unlifted newtypes
                             -- might have a specific kind
             -> HsConDeclH98Details GhcRn
             -> TcM [(Scaled TcType, HsSrcBang)]
tcConH98Args exp_kind (PrefixCon _ btys)
  = mapM (tcConArg exp_kind) btys
tcConH98Args exp_kind (InfixCon bty1 bty2)
  = do { bty1' <- tcConArg exp_kind bty1
       ; bty2' <- tcConArg exp_kind bty2
       ; return [bty1', bty2'] }
tcConH98Args exp_kind (RecCon fields)
  = tcRecConDeclFields exp_kind fields

tcConGADTArgs :: ContextKind  -- expected kind of arguments
                              -- always OpenKind for datatypes, but unlifted newtypes
                              -- might have a specific kind
              -> HsConDeclGADTDetails GhcRn
              -> TcM [(Scaled TcType, HsSrcBang)]
tcConGADTArgs exp_kind (PrefixConGADT btys)
  = mapM (tcConArg exp_kind) btys
tcConGADTArgs exp_kind (RecConGADT fields)
  = tcRecConDeclFields exp_kind fields

tcConArg :: ContextKind  -- expected kind for args; always OpenKind for datatypes,
                         -- but might be an unlifted type with UnliftedNewtypes
         -> HsScaled GhcRn (LHsType GhcRn) -> TcM (Scaled TcType, HsSrcBang)
tcConArg exp_kind (HsScaled w bty)
  = do  { traceTc "tcConArg 1" (ppr bty)
        ; arg_ty <- tcCheckLHsType (getBangType bty) exp_kind
        ; w' <- tcDataConMult w
        ; traceTc "tcConArg 2" (ppr bty)
        ; return (Scaled w' arg_ty, getBangStrictness bty) }

tcRecConDeclFields :: ContextKind
                   -> Located [LConDeclField GhcRn]
                   -> TcM [(Scaled TcType, HsSrcBang)]
tcRecConDeclFields exp_kind fields
  = mapM (tcConArg exp_kind) btys
  where
    -- We need a one-to-one mapping from field_names to btys
    combined = map (\(L _ f) -> (cd_fld_names f,hsLinear (cd_fld_type f)))
                   (unLoc fields)
    explode (ns,ty) = zip ns (repeat ty)
    exploded = concatMap explode combined
    (_,btys) = unzip exploded

tcDataConMult :: HsArrow GhcRn -> TcM Mult
tcDataConMult arr@(HsUnrestrictedArrow _) = do
  -- See Note [Function arrows in GADT constructors]
  linearEnabled <- xoptM LangExt.LinearTypes
  if linearEnabled then tcMult arr else return oneDataConTy
tcDataConMult arr = tcMult arr

{-
Note [Function arrows in GADT constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the absence of -XLinearTypes, we always interpret function arrows
in GADT constructor types as linear, even if the user wrote an
unrestricted arrow. See the "Without -XLinearTypes" section of the
linear types GHC proposal (#111). We opt to do this in the
typechecker, and not in an earlier pass, to ensure that the AST
matches what the user wrote (#18791).

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


Note [rejigConRes]
~~~~~~~~~~~~~~~~~~
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
errors reported in one pass.  See #7175, and #10836.
-}

-- Example
--   data instance T (b,c) where
--      TI :: forall e. e -> T (e,e)
--
-- The representation tycon looks like this:
--   data :R7T b c where
--      TI :: forall b1 c1. (b1 ~ c1) => b1 -> :R7T b1 c1
-- In this case orig_res_ty = T (e,e)

rejigConRes :: [KnotTied TyConBinder]  -- Template for result type; e.g.
            -> KnotTied Type           -- data instance T [a] b c ...
                                       --      gives template ([a,b,c], T [a] b c)
            -> [InvisTVBinder]    -- The constructor's type variables (both inferred and user-written)
            -> KnotTied Type      -- res_ty
            -> ([TyVar],          -- Universal
                [TyVar],          -- Existential (distinct OccNames from univs)
                [InvisTVBinder],  -- The constructor's rejigged, user-written
                                  -- type variables
                [EqSpec],         -- Equality predicates
                TCvSubst)         -- Substitution to apply to argument types
        -- We don't check that the TyCon given in the ResTy is
        -- the same as the parent tycon, because checkValidDataCon will do it
-- NB: All arguments may potentially be knot-tied
rejigConRes tc_tvbndrs res_tmpl dc_tvbndrs res_ty
        -- E.g.  data T [a] b c where
        --         MkT :: forall x y z. T [(x,y)] z z
        -- The {a,b,c} are the tc_tvs, and the {x,y,z} are the dc_tvs
        --     (NB: unlike the H98 case, the dc_tvs are not all existential)
        -- Then we generate
        --      Univ tyvars     Eq-spec
        --          a              a~(x,y)
        --          b              b~z
        --          z
        -- Existentials are the leftover type vars: [x,y]
        -- The user-written type variables are what is listed in the forall:
        --   [x, y, z] (all specified). We must rejig these as well.
        --   See Note [DataCon user type variable binders] in GHC.Core.DataCon.
        -- So we return ( [a,b,z], [x,y]
        --              , [], [x,y,z]
        --              , [a~(x,y),b~z], <arg-subst> )
  | Just subst <- tcMatchTy res_tmpl res_ty
  = let (univ_tvs, raw_eqs, kind_subst) = mkGADTVars tc_tvs dc_tvs subst
        raw_ex_tvs = dc_tvs `minusList` univ_tvs
        (arg_subst, substed_ex_tvs) = substTyVarBndrs kind_subst raw_ex_tvs

        -- After rejigging the existential tyvars, the resulting substitution
        -- gives us exactly what we need to rejig the user-written tyvars,
        -- since the dcUserTyVarBinders invariant guarantees that the
        -- substitution has *all* the tyvars in its domain.
        -- See Note [DataCon user type variable binders] in GHC.Core.DataCon.
        subst_user_tvs  = mapVarBndrs (getTyVar "rejigConRes" . substTyVar arg_subst)
        substed_tvbndrs = subst_user_tvs dc_tvbndrs

        substed_eqs = map (substEqSpec arg_subst) raw_eqs
    in
    (univ_tvs, substed_ex_tvs, substed_tvbndrs, substed_eqs, arg_subst)

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
        -- See Note [rejigConRes]
  = (tc_tvs, dc_tvs `minusList` tc_tvs, dc_tvbndrs, [], emptyTCvSubst)
  where
    dc_tvs = binderVars dc_tvbndrs
    tc_tvs = binderVars tc_tvbndrs

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
later, but laziness saves us -- see Note [rejigConRes]).
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
                         -- so add it to t_sub (#14162)
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
    addTyConCtxt tc            $
    recoverM recovery_code     $
    do { traceTc "Starting validity for tycon" (ppr tc)
       ; checkValidTyCon tc
       ; traceTc "Done validity for tycon" (ppr tc)
       ; return [tc] }
  where
    recovery_code -- See Note [Recover from validity error]
      = do { traceTc "Aborted validity for tycon" (ppr tc)
           ; return (map mk_fake_tc $
                     tc : child_tycons tc) }

    mk_fake_tc tc
      | isClassTyCon tc = tc   -- Ugh! Note [Recover from validity error]
      | otherwise       = makeRecoveryTyCon tc

    child_tycons tc = tyConATs tc ++ map promoteDataCon (tyConDataCons tc)

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
See indexed-types/should_fail/BadSock and #10896

Some notes:

* We must make fakes for promoted DataCons too. Consider (#15215)
      data T a = MkT ...
      data S a = ...T...MkT....
  If there is an error in the definition of 'T' we add a "fake type
  constructor" to the type environment, so that we can continue to
  typecheck 'S'.  But we /were not/ adding a fake anything for 'MkT'
  and so there was an internal error when we met 'MkT' in the body of
  'S'.

  Similarly for associated types.

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
  alternative might be to make a fake class TyCon.  A job for another day.

* Previously, we used implicitTyConThings to snaffle out the parts
  to add to the context. The problem is that this also grabs data con
  wrapper Ids. These could be filtered out. But, painfully, getting
  the wrapper Ids checks the DataConRep, and forcing the DataConRep
  can panic if there is a levity-polymorphic argument. This is #18534.
  We don't need the wrapper Ids here anyway. So the code just takes what
  it needs, via child_tycons.
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

  | isWiredIn tc     -- validity-checking wired-in tycons is a waste of
                     -- time. More importantly, a wired-in tycon might
                     -- violate assumptions. Example: (~) has a superclass
                     -- mentioning (~#), which is ill-kinded in source Haskell
  = traceTc "Skipping validity check for wired-in" (ppr tc)

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
    cmp_fld (f1,_) (f2,_) = flLabel f1 `uniqCompareFS` flLabel f2
    get_fields con = dataConFieldLabels con `zip` repeat con
        -- dataConFieldLabels may return the empty list, which is fine

    -- See Note [GADT record selectors] in GHC.Tc.TyCl.Utils
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
        res1 = dataConOrigResTy con1
        fty1 = dataConFieldType con1 lbl
        lbl = flLabel label

        checkOne (_, con2)    -- Do it both ways to ensure they are structurally identical
            = do { checkFieldCompat lbl con1 con2 res1 res2 fty1 fty2
                 ; checkFieldCompat lbl con2 con1 res2 res1 fty2 fty1 }
            where
                res2 = dataConOrigResTy con2
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
    loc = getSrcSpan (flSelector fld)
    occ_name = occName fld

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
  = setSrcSpan con_loc $
    addErrCtxt (dataConCtxt [L con_loc con_name]) $
    do  { let tc_tvs      = tyConTyVars tc
              res_ty_tmpl = mkFamilyTyConApp tc (mkTyVarTys tc_tvs)
              orig_res_ty = dataConOrigResTy con
        ; traceTc "checkValidDataCon" (vcat
              [ ppr con, ppr tc, ppr tc_tvs
              , ppr res_ty_tmpl <+> dcolon <+> ppr (tcTypeKind res_ty_tmpl)
              , ppr orig_res_ty <+> dcolon <+> ppr (tcTypeKind orig_res_ty)])


        -- Check that the return type of the data constructor
        -- matches the type constructor; eg reject this:
        --   data T a where { MkT :: Bogus a }
        -- It's important to do this first:
        --  see Note [rejigCon
        --  and c.f. Note [Check role annotations in a second pass]

        -- Check that the return type of the data constructor is an instance
        -- of the header of the header of data decl.  This checks for
        --      data T a where { MkT :: S a }
        --      data instance D [a] where { MkD :: D (Maybe b) }
        -- see Note [GADT return types]
        ; checkTc (isJust (tcMatchTyKi res_ty_tmpl orig_res_ty))
                  (badDataConTyCon con res_ty_tmpl)
            -- Note that checkTc aborts if it finds an error. This is
            -- critical to avoid panicking when we call dataConDisplayType
            -- on an un-rejiggable datacon!
            -- Also NB that we match the *kind* as well as the *type* (#18357)
            -- However, if the kind is the only thing that doesn't match, the
            -- error message is terrible.  E.g. test T18357b
            --    type family Star where Star = Type
            --    newtype T :: Type where MkT :: Int -> (T :: Star)

        ; traceTc "checkValidDataCon 2" (ppr data_con_display_type)

          -- Check that the result type is a *monotype*
          --  e.g. reject this:   MkT :: T (forall a. a->a)
          -- Reason: it's really the argument of an equality constraint
        ; checkValidMonoType orig_res_ty

          -- If we are dealing with a newtype, we allow levity polymorphism
          -- regardless of whether or not UnliftedNewtypes is enabled. A
          -- later check in checkNewDataCon handles this, producing a
          -- better error message than checkForLevPoly would.
        ; unless (isNewTyCon tc) $
            checkNoErrs $
            mapM_ (checkForLevPoly empty) (map scaledThing $ dataConOrigArgTys con)
            -- the checkNoErrs is to prevent a panic in isVanillaDataCon
            -- (called a a few lines down), which can fall over if there is a
            -- bang on a levity-polymorphic argument. This is #18534,
            -- typecheck/should_fail/T18534

          -- Extra checks for newtype data constructors. Importantly, these
          -- checks /must/ come before the call to checkValidType below. This
          -- is because checkValidType invokes the constraint solver, and
          -- invoking the solver on an ill formed newtype constructor can
          -- confuse GHC to the point of panicking. See #17955 for an example.
        ; when (isNewTyCon tc) (checkNewDataCon con)

          -- Check all argument types for validity
        ; checkValidType ctxt data_con_display_type

          -- Check that existentials are allowed if they are used
        ; checkTc (existential_ok || isVanillaDataCon con)
                  (badExistential con)

          -- Check that UNPACK pragmas and bangs work out
          -- E.g.  reject   data T = MkT {-# UNPACK #-} Int     -- No "!"
          --                data T = MkT {-# UNPACK #-} !a      -- Can't unpack
        ; hsc_env <- getTopEnv
        ; let check_bang :: HsSrcBang -> HsImplBang -> Int -> TcM ()
              check_bang bang rep_bang n
               | HsSrcBang _ _ SrcLazy <- bang
               , not (xopt LangExt.StrictData dflags)
               = addErrTc (bad_bang n (text "Lazy annotation (~) without StrictData"))

               | HsSrcBang _ want_unpack strict_mark <- bang
               , isSrcUnpacked want_unpack, not (is_strict strict_mark)
               = addWarnTc NoReason (bad_bang n (text "UNPACK pragma lacks '!'"))

               | HsSrcBang _ want_unpack _ <- bang
               , isSrcUnpacked want_unpack
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
               , isHomeUnitDefinite (hsc_home_unit hsc_env)
               = addWarnTc NoReason (bad_bang n (text "Ignoring unusable UNPACK pragma"))

               | otherwise
               = return ()

        ; zipWith3M_ check_bang (dataConSrcBangs con) (dataConImplBangs con) [1..]

          -- Check the dcUserTyVarBinders invariant
          -- See Note [DataCon user type variable binders] in GHC.Core.DataCon
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
               , text "Datacon wrapper type:" <+> ppr (dataConWrapperType con)
               , text "Datacon rep type:" <+> ppr (dataConRepType con)
               , text "Datacon display type:" <+> ppr data_con_display_type
               , text "Rep typcon binders:" <+> ppr (tyConBinders (dataConTyCon con))
               , case tyConFamInst_maybe (dataConTyCon con) of
                   Nothing -> text "not family"
                   Just (f, _) -> ppr (tyConBinders f) ]
    }
  where
    con_name = dataConName con
    con_loc  = nameSrcSpan con_name
    ctxt = ConArgCtxt con_name
    is_strict = \case
      NoSrcStrict -> xopt LangExt.StrictData dflags
      bang        -> isSrcStrict bang

    bad_bang n herald
      = hang herald 2 (text "on the" <+> speakNth n
                       <+> text "argument of" <+> quotes (ppr con))

    show_linear_types     = xopt LangExt.LinearTypes dflags
    data_con_display_type = dataConDisplayType show_linear_types con

-------------------------------
checkNewDataCon :: DataCon -> TcM ()
-- Further checks for the data constructor of a newtype
checkNewDataCon con
  = do  { checkTc (isSingleton arg_tys) (newtypeFieldErr con (length arg_tys))
              -- One argument

        ; unlifted_newtypes <- xoptM LangExt.UnliftedNewtypes
        ; let allowedArgType =
                unlifted_newtypes || isLiftedType_maybe (scaledThing arg_ty1) == Just True
        ; checkTc allowedArgType $ vcat
          [ text "A newtype cannot have an unlifted argument type"
          , text "Perhaps you intended to use UnliftedNewtypes"
          ]
        ; show_linear_types <- xopt LangExt.LinearTypes <$> getDynFlags

        ; let check_con what msg =
               checkTc what (msg $$ ppr con <+> dcolon <+> ppr (dataConDisplayType show_linear_types con))

        ; checkTc (ok_mult (scaledMult arg_ty1)) $
          text "A newtype constructor must be linear"

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

    (arg_ty1 : _) = arg_tys

    ok_bang (HsSrcBang _ _ SrcStrict) = False
    ok_bang (HsSrcBang _ _ SrcLazy)   = False
    ok_bang _                         = True

    ok_mult One = True
    ok_mult _   = False

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
        -- extension (subsumed by multiparameter type classes, #8993)
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
           -- See Note [Levity polymorphism checking] in GHC.HsToCore.Monad
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
                        -- since there is no possible ambiguity (#10020)

             -- Check that any default declarations for associated types are valid
           ; whenIsJust m_dflt_rhs $ \ (rhs, at_validity_info) ->
             case at_validity_info of
               NoATVI -> pure ()
               ATVI loc pats ->
                 setSrcSpan loc $
                 tcAddFamInstCtxt (text "default type instance") (getName fam_tc) $
                 do { checkValidAssocTyFamDeflt fam_tc pats
                    ; checkValidTyFamEqn fam_tc fam_tvs (mkTyVarTys fam_tvs) rhs }}
        where
          fam_tvs = tyConTyVars fam_tc

    check_dm :: UserTypeCtxt -> Id -> PredType -> Type -> DefMethInfo -> TcM ()
    -- Check validity of the /top-level/ generic-default type
    -- E.g for   class C a where
    --             default op :: forall b. (a~b) => blah
    -- we do not want to do an ambiguity check on a type with
    -- a free TyVar 'a' (#11608).  See TcType
    -- Note [TyVars and TcTyVars during type checking] in GHC.Tc.Utils.TcType
    -- Hence the mkDefaultMethodType to close the type.
    check_dm ctxt sel_id vanilla_cls_pred vanilla_tau
             (Just (dm_name, dm_spec@(GenericDM dm_ty)))
      = setSrcSpan (getSrcSpan dm_name) $ do
            -- We have carefully set the SrcSpan on the generic
            -- default-method Name to be that of the generic
            -- default type signature

          -- First, we check that the method's default type signature
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

checkResultSigFlag :: Name -> FamilyResultSig GhcRn -> TcM ()
checkResultSigFlag tc_name (TyVarSig _ tvb)
  = do { ty_fam_deps <- xoptM LangExt.TypeFamilyDependencies
       ; checkTc ty_fam_deps $
         hang (text "Illegal result type variable" <+> ppr tvb <+> text "for" <+> quotes (ppr tc_name))
            2 (text "Enable TypeFamilyDependencies to allow result variable names") }
checkResultSigFlag _ _ = return ()  -- other cases OK

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
as pointed out in #11793. So the test here rejects the program if
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

This fixes #9415, #9739

Note [Default method type signatures must align]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC enforces the invariant that a class method's default type signature
must "align" with that of the method's non-default type signature, as per
GHC #12918. For instance, if you have:

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
   an instance. See Note [Default methods in instances] in GHC.Tc.TyCl.Instance.
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
context for enum is allowed to be different to accommodate this. As a result,
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
This check checks the partial record field selector, and warns (#7169).

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
          \decl@(L loc (RoleAnnotDecl _ _ the_role_annots)) ->
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
checkRoleAnnot _  (L _ Nothing)   _  = return ()
checkRoleAnnot tv (L _ (Just r1)) r2
  = when (r1 /= r2) $
    addErrTc $ badRoleAnnot (tyVarName tv) r1 r2

-- This is a double-check on the role inference algorithm. It is only run when
-- -dcore-lint is enabled. See Note [Role inference] in GHC.Tc.TyCl.Utils
checkValidRoles :: TyCon -> TcM ()
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism] in GHC.Core.Lint
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
                    eqSpecPreds eq_spec ++ theta ++ (map scaledThing arg_tys) }
                    -- See Note [Role-checking data constructor arguments] in GHC.Tc.TyCl.Utils
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

    check_ty_roles env role (FunTy _ w ty1 ty2)
      =  check_ty_roles env Nominal w
      >> check_ty_roles env role ty1
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
                         text "Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug"]

{-
************************************************************************
*                                                                      *
                Error messages
*                                                                      *
************************************************************************
-}

tcMkDeclCtxt :: TyClDecl GhcRn -> SDoc
tcMkDeclCtxt decl = hsep [text "In the", pprTyClDeclFlavour decl,
                      text "declaration for", quotes (ppr (tcdName decl))]

addVDQNote :: TcTyCon -> TcM a -> TcM a
-- See Note [Inferring visible dependent quantification]
-- Only types without a signature (CUSK or SAK) here
addVDQNote tycon thing_inside
  | ASSERT2( isTcTyCon tycon, ppr tycon )
    ASSERT2( not (tcTyConIsPoly tycon), ppr tycon $$ ppr tc_kind )
    has_vdq
  = addLandmarkErrCtxt vdq_warning thing_inside
  | otherwise
  = thing_inside
  where
      -- Check whether a tycon has visible dependent quantification.
      -- This will *always* be a TcTyCon. Furthermore, it will *always*
      -- be an ungeneralised TcTyCon, straight out of kcInferDeclHeader.
      -- Thus, all the TyConBinders will be anonymous. Thus, the
      -- free variables of the tycon's kind will be the same as the free
      -- variables from all the binders.
    has_vdq  = any is_vdq_tcb (tyConBinders tycon)
    tc_kind  = tyConKind tycon
    kind_fvs = tyCoVarsOfType tc_kind

    is_vdq_tcb tcb = (binderVar tcb `elemVarSet` kind_fvs) &&
                     isVisibleTyConBinder tcb

    vdq_warning = vcat
      [ text "NB: Type" <+> quotes (ppr tycon) <+>
        text "was inferred to use visible dependent quantification."
      , text "Most types with visible dependent quantification are"
      , text "polymorphically recursive and need a standalone kind"
      , text "signature. Perhaps supply one, with StandaloneKindSignatures."
      ]

tcAddDeclCtxt :: TyClDecl GhcRn -> TcM a -> TcM a
tcAddDeclCtxt decl thing_inside
  = addErrCtxt (tcMkDeclCtxt decl) thing_inside

tcAddTyFamInstCtxt :: TyFamInstDecl GhcRn -> TcM a -> TcM a
tcAddTyFamInstCtxt decl
  = tcAddFamInstCtxt (text "type instance") (tyFamInstDeclName decl)

tcMkDataFamInstCtxt :: DataFamInstDecl GhcRn -> SDoc
tcMkDataFamInstCtxt decl@(DataFamInstDecl { dfid_eqn = eqn })
  = tcMkFamInstCtxt (pprDataFamInstFlavour decl <+> text "instance")
                    (unLoc (feqn_tycon eqn))

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

dataConCtxt :: [Located Name] -> SDoc
dataConCtxt cons = text "In the definition of data constructor" <> plural cons
                   <+> ppr_cons cons

dataConResCtxt :: [Located Name] -> SDoc
dataConResCtxt cons = text "In the result type of data constructor" <> plural cons
                      <+> ppr_cons cons

ppr_cons :: [Located Name] -> SDoc
ppr_cons [con] = quotes (ppr con)
ppr_cons cons  = interpp'SP cons

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
  = sep [ text "The associated type" <+> quotes (ppr fam_tc <+> hsep (map ppr (tyConTyVars fam_tc)))
        , text "mentions none of the type or kind variables of the class" <+>
                quotes (ppr clas <+> hsep (map ppr (classTyVars clas)))]

badDataConTyCon :: DataCon -> Type -> SDoc
badDataConTyCon data_con res_ty_tmpl
  = hang (text "Data constructor" <+> quotes (ppr data_con) <+>
                text "returns type" <+> quotes (ppr actual_res_ty))
       2 (text "instead of an instance of its parent type" <+> quotes (ppr res_ty_tmpl))
  where
    actual_res_ty = dataConOrigResTy data_con

badGadtDecl :: Name -> SDoc
badGadtDecl tc_name
  = vcat [ text "Illegal generalised algebraic data declaration for" <+> quotes (ppr tc_name)
         , nest 2 (parens $ text "Enable the GADTs extension to allow this") ]

badExistential :: DataCon -> SDoc
badExistential con
  = sdocOption sdocLinearTypes (\show_linear_types ->
      hang (text "Data constructor" <+> quotes (ppr con) <+>
                  text "has existential type variables, a context, or a specialised result type")
         2 (vcat [ ppr con <+> dcolon <+> ppr (dataConDisplayType show_linear_types con)
                 , parens $ text "Enable ExistentialQuantification or GADTs to allow this" ]))

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

-- | Produce an error for oversaturated type family equations with too many
-- required arguments.
-- See Note [Oversaturated type family equations] in "GHC.Tc.Validity".
wrongNumberOfParmsErr :: Arity -> SDoc
wrongNumberOfParmsErr max_args
  = text "Number of parameters must match family declaration; expected"
    <+> ppr max_args

badRoleAnnot :: Name -> Role -> Role -> SDoc
badRoleAnnot var annot inferred
  = hang (text "Role mismatch on variable" <+> ppr var <> colon)
       2 (sep [ text "Annotation says", ppr annot
              , text "but role", ppr inferred
              , text "is required" ])

wrongNumberOfRoles :: [a] -> LRoleAnnotDecl GhcRn -> SDoc
wrongNumberOfRoles tyvars d@(L _ (RoleAnnotDecl _ _ annots))
  = hang (text "Wrong number of roles listed in role annotation;" $$
          text "Expected" <+> (ppr $ length tyvars) <> comma <+>
          text "got" <+> (ppr $ length annots) <> colon)
       2 (ppr d)


illegalRoleAnnotDecl :: LRoleAnnotDecl GhcRn -> TcM ()
illegalRoleAnnotDecl (L loc (RoleAnnotDecl _ tycon _))
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

wrongTyFamName :: Name -> Name -> SDoc
wrongTyFamName fam_tc_name eqn_tc_name
  = hang (text "Mismatched type name in type family instance.")
       2 (vcat [ text "Expected:" <+> ppr fam_tc_name
               , text "  Actual:" <+> ppr eqn_tc_name ])

addTyConCtxt :: TyCon -> TcM a -> TcM a
addTyConCtxt tc = addTyConFlavCtxt name flav
  where
    name = getName tc
    flav = tyConFlavour tc

addRoleAnnotCtxt :: Name -> TcM a -> TcM a
addRoleAnnotCtxt name
  = addErrCtxt $
    text "while checking a role annotation for" <+> quotes (ppr name)
