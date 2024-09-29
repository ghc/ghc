{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 2000


-}

{-# LANGUAGE DeriveFunctor #-}

-- | Functional dependencies
--
-- It's better to read it as: "if we know these, then we're going to know these"
module GHC.Tc.Instance.FunDeps
   ( FunDepEqn(..)
   , pprEquation
   , improveFromInstEnv
   , improveFromAnother
   , checkInstCoverage
   , checkFunDeps
   , pprFundeps
   , instFD, closeWrtFunDeps
   )
where

import GHC.Prelude

import GHC.Types.Var
import GHC.Core.Class
import GHC.Core.Predicate
import GHC.Core.Type
import GHC.Core.RoughMap( RoughMatchTc(..) )
import GHC.Core.Coercion.Axiom( TypeEqn )
import GHC.Core.Unify
import GHC.Core.InstEnv
import GHC.Core.TyCo.FVs
import GHC.Core.TyCo.Compare( eqTypes, eqType )

import GHC.Tc.Errors.Types ( CoverageProblem(..), FailedCoverageCondition(..) )
import GHC.Tc.Types.Constraint ( isUnsatisfiableCt_maybe )
import GHC.Tc.Utils.TcType( transSuperClasses )

import GHC.Types.Var.Set
import GHC.Types.Var.Env

import GHC.Utils.Outputable
import GHC.Utils.FV
import GHC.Utils.Error( Validity'(..), allValid )
import GHC.Utils.Misc
import GHC.Utils.Panic

import GHC.Data.Pair ( Pair(..) )
import Data.List     ( nubBy )
import Data.Maybe    ( isJust, isNothing )

{-
************************************************************************
*                                                                      *
\subsection{Generate equations from functional dependencies}
*                                                                      *
************************************************************************

Each functional dependency with one variable in the RHS is responsible
for generating a single equality. For instance:
     class C a b | a -> b
The constraints ([Wanted] C Int Bool) and [Wanted] C Int alpha
will generate the following FunDepEqn
     FDEqn { fd_qtvs = []
           , fd_eqs  = [Pair Bool alpha]
           , fd_loc = ... }
However notice that a functional dependency may have more than one variable
in the RHS which will create more than one pair of types in fd_eqs. Example:
     class C a b c | a -> b c
     [Wanted] C Int alpha alpha
     [Wanted] C Int Bool beta
Will generate:
     FDEqn { fd_qtvs = []
           , fd_eqs  = [Pair Bool alpha, Pair alpha beta]
           , fd_loc = ... }

INVARIANT: Corresponding types aren't already equal
That is, there exists at least one non-identity equality in FDEqs.

Note [Improving against instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Assume:
   class C a b | a -> b
   instance C Int Bool
   [W] C Int ty

Then `improveFromInstEnv` should return a FDEqn with
   FDEqn { fd_qtvs = [], fd_eqs = [Pair Bool ty] }

describing an equality (Bool ~ ty).  To do this we /match/ the instance head
against the [W], using just the LHS of the fundep; if we match, we return
an equality for the RHS.

Wrinkles:

(1) meta_tvs: sometimes the instance mentions variables in the RHS that
    are not bound in the LHS.  For example

     class C a b | a -> b
     instance C Int (Maybe x)
     [W] C Int ty

    Note that although the `Int` parts match, that does not fix what `x` is.
    So we just make up a fresh unification variable (a meta_tv), to give the
    "shape" of the RHS.  So we emit the FDEqun
       FDEqn { fd_qtvs = [x], fd_eqs = [Pair (Maybe x) ty] }

    Note that the fd_qtvs can be free in the /first/ component of the Pair,
    but not in the second (which comes from the [W] constraint).

(2) Multi-range fundeps. When these meta_tvs are involved, there is a subtle
    difference between the fundep (a -> b c) and the two fundeps (a->b, a->c).
    Consider
       class D a b c | a -> b c
       instance D Int x (Maybe x)
       [W] D Int Bool ty

    Then we'll generate
       FDEqn { fd_qtvs = [x0], fd_eqs = [ x0 ~ Bool, Maybe x0 ~ ty] }
    which generates one fresh unification variable x0

    But if the fundeps had been (a->b, a->c) we'd generate two FDEqns
       FDEqn { fd_qtvs = [x1], fd_eqs = [ x1 ~ Bool ] }
       FDEqn { fd_qtvs = [x2], fd_eqs = [ Maybe x2 ~ ty ] }
    with two FDEqns, generating two separate unification variables.

(3) improveFromInstEnv doesn't return any equations that already hold.
    Reason: then we know if any actual improvement has happened, in
    which case we need to iterate the solver
-}

data FunDepEqn loc
  = FDEqn { fd_qtvs :: [TyVar]   -- Instantiate these type and kind vars
                                 --   to fresh unification vars,
                                 -- Non-empty only for FunDepEqns arising from instance decls

          , fd_eqs   :: [TypeEqn]  -- Make these pairs of types equal
                                   -- Invariant: In each (Pair ty1 ty2), the fd_qtvs may be
                                   -- free in ty1 but not in ty2.  See Wrinkle (1) of
                                   -- Note [Improving against instances]

          , fd_loc   :: loc  }
    deriving Functor

instance Outputable (FunDepEqn a) where
  ppr = pprEquation

pprEquation :: FunDepEqn a -> SDoc
pprEquation (FDEqn { fd_qtvs = qtvs, fd_eqs = pairs })
  = vcat [text "forall" <+> braces (pprWithCommas ppr qtvs),
          nest 2 (vcat [ ppr t1 <+> text "~" <+> ppr t2
                       | Pair t1 t2 <- pairs])]

{-
Given a bunch of predicates that must hold, such as

        C Int t1, C Int t2, C Bool t3, ?x::t4, ?x::t5

improve figures out what extra equations must hold.
For example, if we have

        class C a b | a->b where ...

then improve will return

        [(t1,t2), (t4,t5)]

NOTA BENE:

  * improve does not iterate.  It's possible that when we make
    t1=t2, for example, that will in turn trigger a new equation.
    This would happen if we also had
        C t1 t7, C t2 t8
    If t1=t2, we also get t7=t8.

    improve does *not* do this extra step.  It relies on the caller
    doing so.

  * The equations unify types that are not already equal.  So there
    is no effect iff the result of improve is empty
-}

instFD :: FunDep TyVar -> [TyVar] -> [Type] -> FunDep Type
-- (instFD fd tvs tys) returns fd instantiated with (tvs -> tys)
instFD (ls,rs) tvs tys
  = (map lookup ls, map lookup rs)
  where
    env       = zipVarEnv tvs tys
    lookup tv = lookupVarEnv_NF env tv

zipAndComputeFDEqs :: (Type -> Type -> Bool) -- Discard this FDEq if true
                   -> [Type] -> [Type]
                   -> [TypeEqn]
-- Create a list of (Type,Type) pairs from two lists of types,
-- making sure that the types are not already equal
zipAndComputeFDEqs discard (ty1:tys1) (ty2:tys2)
 | discard ty1 ty2 = zipAndComputeFDEqs discard tys1 tys2
 | otherwise       = Pair ty1 ty2 : zipAndComputeFDEqs discard tys1 tys2
zipAndComputeFDEqs _ _ _ = []

-- Improve a class constraint from another class constraint
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
improveFromAnother :: loc
                   -> PredType -- Template item (usually given, or inert)
                   -> PredType -- Workitem [that can be improved]
                   -> [FunDepEqn loc]
-- Post: FDEqs always oriented from the other to the workitem
--       Equations have empty quantified variables
improveFromAnother loc pred1 pred2
  | Just (cls1, tys1) <- getClassPredTys_maybe pred1
  , Just (cls2, tys2) <- getClassPredTys_maybe pred2
  , cls1 == cls2
  = [ FDEqn { fd_qtvs = [], fd_eqs = eqs, fd_loc = loc }
    | let (cls_tvs, cls_fds) = classTvsFds cls1
    , fd <- cls_fds
    , let (ltys1, rs1) = instFD fd cls_tvs tys1
          (ltys2, rs2) = instFD fd cls_tvs tys2
    , eqTypes ltys1 ltys2               -- The LHSs match
    , let eqs = zipAndComputeFDEqs eqType rs1 rs2
    , not (null eqs) ]

improveFromAnother _ _ _ = []


-- Improve a class constraint from instance declarations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

improveFromInstEnv :: InstEnvs
                   -> (ClsInst -> loc)
                   -> Class -> [Type]
                   -> [FunDepEqn loc] -- Needs to be a FunDepEqn because
                                      -- of quantified variables
-- See Note [Improving against instances]
-- Post: Equations oriented from the template (matching instance) to the workitem!
improveFromInstEnv inst_env mk_loc cls tys
  = [ FDEqn { fd_qtvs = meta_tvs, fd_eqs = eqs, fd_loc = mk_loc ispec }
    | fd <- cls_fds             -- Iterate through the fundeps first,
                                -- because there often are none!
    , let trimmed_tcs = trimRoughMatchTcs cls_tvs fd rough_tcs
                -- Trim the rough_tcs based on the head of the fundep.
                -- Remember that instanceCantMatch treats both arguments
                -- symmetrically, so it's ok to trim the rough_tcs,
                -- rather than trimming each inst_tcs in turn
    , ispec <- instances
    , (meta_tvs, eqs) <- improveClsFD cls_tvs fd ispec
                                      tys trimmed_tcs -- NB: orientation
    ]
  where
    (cls_tvs, cls_fds) = classTvsFds cls
    instances          = classInstances inst_env cls
    rough_tcs          = RM_KnownTc (className cls) : roughMatchTcs tys

improveClsFD :: [TyVar] -> FunDep TyVar    -- One functional dependency from the class
             -> ClsInst                    -- An instance template
             -> [Type] -> [RoughMatchTc]   -- Arguments of this (C tys) predicate
             -> [([TyCoVar], [TypeEqn])]   -- Empty or singleton
-- See Note [Improving against instances]

improveClsFD clas_tvs fd
             (ClsInst { is_tvs = qtvs, is_tys = tys_inst, is_tcs = rough_tcs_inst })
             tys_actual rough_tcs_actual

  | instanceCantMatch rough_tcs_inst rough_tcs_actual
  = []          -- Filter out ones that can't possibly match,

  | otherwise
  = assertPpr (equalLength tys_inst tys_actual &&
               equalLength tys_inst clas_tvs)
              (ppr tys_inst <+> ppr tys_actual) $

    case tcMatchTyKisX init_subst ltys1 ltys2 of
        Nothing  -> []
        Just subst | isJust (tcMatchTyKisX subst rtys1 rtys2)
                        -- Don't include any equations that already hold.
                        -- See Note [Improving against instances] wrinkle (3)
                        -- In making this check we must taking account of the fact that any
                        -- qtvs that aren't already instantiated can be instantiated to anything
                        -- at all
                        -- NB: We can't do this 'is-useful-equation' check element-wise
                        --     because of:
                        --           class C a b c | a -> b c
                        --           instance C Int x x
                        --           [Wanted] C Int alpha Int
                        -- We would get that  x -> alpha  (isJust) and x -> Int (isJust)
                        -- so we would produce no FDs, which is clearly wrong.
                  -> []

                  | null fdeqs
                  -> []

                  | otherwise
                  -> -- pprTrace "iproveClsFD" (vcat
                     --  [ text "is_tvs =" <+> ppr qtvs
                     --  , text "tys_inst =" <+> ppr tys_inst
                     --  , text "tys_actual =" <+> ppr tys_actual
                     --  , text "ltys1 =" <+> ppr ltys1
                     --  , text "ltys2 =" <+> ppr ltys2
                     --  , text "subst =" <+> ppr subst ]) $
                     [(meta_tvs, fdeqs)]
                        -- We could avoid this substTy stuff by producing the eqn
                        -- (qtvs, ls1++rs1, ls2++rs2)
                        -- which will re-do the ls1/ls2 unification when the equation is
                        -- executed.  What we're doing instead is recording the partial
                        -- work of the ls1/ls2 unification leaving a smaller unification problem
                  where
                    rtys1' = map (substTy subst) rtys1

                    fdeqs = zipAndComputeFDEqs (\_ _ -> False) rtys1' rtys2
                        -- Don't discard anything!
                        -- We could discard equal types but it's an overkill to call
                        -- eqType again, since we know for sure that /at least one/
                        -- equation in there is useful)

                    meta_tvs = [ setVarType tv (substTy subst (varType tv))
                               | tv <- qtvs
                               , tv `notElemSubst` subst
                               , tv `elemVarSet` rtys1_tvs ]
                        -- meta_tvs are the quantified type variables
                        -- that have not been substituted out
                        --
                        -- Eg.  class C a b | a -> b
                        --      instance C Int [y]
                        -- Given constraint C Int z
                        -- we generate the equation
                        --      ({y}, [y], z)
                        --
                        -- But note (a) we get them from the dfun_id, so they are *in order*
                        --              because the kind variables may be mentioned in the
                        --              type variables' kinds
                        --          (b) we must apply 'subst' to the kinds, in case we have
                        --              matched out a kind variable, but not a type variable
                        --              whose kind mentions that kind variable! #6015, #6068
                        --          (c) no need to include tyvars not in rtys1
  where
    init_subst     = mkEmptySubst $ mkInScopeSet $
                     mkVarSet qtvs `unionVarSet` tyCoVarsOfTypes ltys2
    (ltys1, rtys1) = instFD fd clas_tvs tys_inst
    (ltys2, rtys2) = instFD fd clas_tvs tys_actual
    rtys1_tvs      = tyCoVarsOfTypes rtys1

{-
%************************************************************************
%*                                                                      *
        The Coverage condition for instance declarations
*                                                                      *
************************************************************************

Note [Coverage condition]
~~~~~~~~~~~~~~~~~~~~~~~~~
Example
      class C a b | a -> b
      instance theta => C t1 t2

For the coverage condition, we check
   (normal)    fv(t2) `subset` fv(t1)
   (liberal)   fv(t2) `subset` closeWrtFunDeps(fv(t1), theta)

The liberal version  ensures the self-consistency of the instance, but
it does not guarantee termination. Example:

   class Mul a b c | a b -> c where
        (.*.) :: a -> b -> c

   instance Mul Int Int Int where (.*.) = (*)
   instance Mul Int Float Float where x .*. y = fromIntegral x * y
   instance Mul a b c => Mul a [b] [c] where x .*. v = map (x.*.) v

In the third instance, it's not the case that fv([c]) `subset` fv(a,[b]).
But it is the case that fv([c]) `subset` closeWrtFunDeps( theta, fv(a,[b]) )

But it is a mistake to accept the instance because then this defn:
        f = \ b x y -> if b then x .*. [y] else y
makes instance inference go into a loop, because it requires the constraint
        Mul a [b] b
-}

checkInstCoverage :: Bool   -- Be liberal
                  -> Class -> [PredType] -> [Type]
                  -> Validity' CoverageProblem
-- "be_liberal" flag says whether to use "liberal" coverage of
--              See Note [Coverage condition] below
--
-- Return values
--    Nothing  => no problems
--    Just msg => coverage problem described by msg

checkInstCoverage be_liberal clas theta inst_taus
  | any (isJust . isUnsatisfiableCt_maybe) theta
  -- As per [GHC proposal #433](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0433-unsatisfiable.rst),
  -- we skip checking the coverage condition if there is an "Unsatisfiable"
  -- constraint in the instance context.
  --
  -- See Note [Implementation of Unsatisfiable constraints] in GHC.Tc.Errors,
  -- point (E).
  = IsValid
  | otherwise
  = allValid (map fundep_ok fds)
  where
    (tyvars, fds) = classTvsFds clas
    fundep_ok fd
       | all isEmptyVarSet undetermined_tvs
       = IsValid
       | otherwise
       = NotValid not_covered_msg
       where
         (ls,rs) = instFD fd tyvars inst_taus
         ls_tvs = tyCoVarsOfTypes ls
         rs_tvs = visVarsOfTypes rs

         undetermined_tvs | be_liberal = liberal_undet_tvs
                          | otherwise  = conserv_undet_tvs

         closed_ls_tvs = closeWrtFunDeps theta ls_tvs
         liberal_undet_tvs = (`minusVarSet` closed_ls_tvs) <$> rs_tvs
         conserv_undet_tvs = (`minusVarSet` ls_tvs)        <$> rs_tvs

         not_covered_msg =
           CoverageProblem
            { not_covered_fundep        = fd
            , not_covered_fundep_inst   = (ls, rs)
            , not_covered_invis_vis_tvs = undetermined_tvs
            , not_covered_liberal       =
                if be_liberal
                then FailedLICC
                else FailedICC
                      { alsoFailedLICC = not $ all isEmptyVarSet liberal_undet_tvs }
            }


{- Note [Closing over kinds in coverage]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have a fundep  (a::k) -> b
Then if 'a' is instantiated to (x y), where x:k2->*, y:k2,
then fixing x really fixes k2 as well, and so k2 should be added to
the lhs tyvars in the fundep check.

Example (#8391), using liberal coverage
      data Foo a = ...  -- Foo :: forall k. k -> *
      class Bar a b | a -> b
      instance Bar a (Foo a)

    In the instance decl, (a:k) does fix (Foo k a), but only if we notice
    that (a:k) fixes k.  #10109 is another example.

Here is a more subtle example, from HList-0.4.0.0 (#10564)

  class HasFieldM (l :: k) r (v :: Maybe *)
        | l r -> v where ...
  class HasFieldM1 (b :: Maybe [*]) (l :: k) r v
        | b l r -> v where ...
  class HMemberM (e1 :: k) (l :: [k]) (r :: Maybe [k])
        | e1 l -> r

  data Label :: k -> *
  type family LabelsOf (a :: [*]) ::  *

  instance (HMemberM (Label {k} (l::k)) (LabelsOf xs) b,
            HasFieldM1 b l (r xs) v)
         => HasFieldM l (r xs) v where

Is the instance OK? Does {l,r,xs} determine v?  Well:

  * From the instance constraint HMemberM (Label k l) (LabelsOf xs) b,
    plus the fundep "| el l -> r" in class HMameberM,
    we get {l,k,xs} -> b

  * Note the 'k'!! We must call closeOverKinds on the seed set
    ls_tvs = {l,r,xs}, BEFORE doing closeWrtFunDeps, else the {l,k,xs}->b
    fundep won't fire.  This was the reason for #10564.

  * So starting from seeds {l,r,xs,k} we do closeWrtFunDeps to get
    first {l,r,xs,k,b}, via the HMemberM constraint, and then
    {l,r,xs,k,b,v}, via the HasFieldM1 constraint.

  * And that fixes v.

However, we must closeOverKinds whenever augmenting the seed set
in closeWrtFunDeps!  Consider #10109:

  data Succ a   -- Succ :: forall k. k -> *
  class Add (a :: k1) (b :: k2) (ab :: k3) | a b -> ab
  instance (Add a b ab) => Add (Succ {k1} (a :: k1))
                               b
                               (Succ {k3} (ab :: k3})

We start with seed set {a:k1,b:k2} and closeOverKinds to {a,k1,b,k2}.
Now use the fundep to extend to {a,k1,b,k2,ab}.  But we need to
closeOverKinds *again* now to {a,k1,b,k2,ab,k3}, so that we fix all
the variables free in (Succ {k3} ab).

Bottom line:
  * closeOverKinds on initial seeds (done automatically
    by tyCoVarsOfTypes in checkInstCoverage)
  * and closeOverKinds whenever extending those seeds (in closeWrtFunDeps)

Note [The liberal coverage condition]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(closeWrtFunDeps preds tvs) closes the set of type variables tvs,
wrt functional dependencies in preds.  The result is a superset
of the argument set.  For example, if we have
        class C a b | a->b where ...
then
        closeWrtFunDeps [C (x,y) z, C (x,p) q] {x,y} = {x,y,z}
because if we know x and y then that fixes z.

We also use equality predicates in the predicates; if we have an
assumption `t1 ~ t2`, then we use the fact that if we know `t1` we
also know `t2` and the other way.
  eg    closeWrtFunDeps [C (x,y) z, a ~ x] {a,y} = {a,y,z,x}

closeWrtFunDeps is used
 - when checking the coverage condition for an instance declaration
 - when determining which tyvars are unquantifiable during generalization, in
   GHC.Tc.Solver.decidePromotedTyVars.

Note [Equality superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  class (a ~ [b]) => C a b

Remember from Note [The equality types story] in GHC.Builtin.Types.Prim, that
  * (a ~~ b) is a superclass of (a ~ b)
  * (a ~# b) is a superclass of (a ~~ b)

So when closeWrtFunDeps expands superclasses we'll get a (a ~# [b]) superclass.
But that's an EqPred not a ClassPred, and we jolly well do want to
account for the mutual functional dependencies implied by (t1 ~# t2).
Hence the EqPred handling in closeWrtFunDeps.  See #10778.

Note [Care with type functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#12803)
  class C x y | x -> y
  type family F a b
  type family G c d = r | r -> d

Now consider
  closeWrtFunDeps (C (F a b) (G c d)) {a,b}

Knowing {a,b} fixes (F a b) regardless of the injectivity of F.
But knowing (G c d) fixes only {d}, because G is only injective
in its second parameter.

Hence the tyCoVarsOfTypes/injTyVarsOfTypes dance in tv_fds.
-}

closeWrtFunDeps :: [PredType] -> TyCoVarSet -> TyCoVarSet
-- See Note [The liberal coverage condition]
closeWrtFunDeps preds fixed_tvs
  | null tv_fds = fixed_tvs -- Fast escape hatch for common case.
  | otherwise   = assertPpr (closeOverKinds fixed_tvs == fixed_tvs)
                    (vcat [ text "closeWrtFunDeps: fixed_tvs is not closed over kinds"
                          , text "fixed_tvs:" <+> ppr fixed_tvs
                          , text "closure:" <+> ppr (closeOverKinds fixed_tvs) ])
                $ fixVarSet extend fixed_tvs
  where

    extend fixed_tvs = foldl' add fixed_tvs tv_fds
       where
          add fixed_tvs (ls,rs)
            | ls `subVarSet` fixed_tvs = fixed_tvs `unionVarSet` closeOverKinds rs
            | otherwise                = fixed_tvs
            -- closeOverKinds: see Note [Closing over kinds in coverage]

    tv_fds  :: [(TyCoVarSet,TyCoVarSet)]
    tv_fds  = [ (tyCoVarsOfTypes ls, fvVarSet $ injectiveVarsOfTypes True rs)
                  -- See Note [Care with type functions]
              | pred <- preds
              , pred' <- pred : transSuperClasses pred
                   -- Look for fundeps in superclasses too
              , (ls, rs) <- determined pred' ]

    determined :: PredType -> [([Type],[Type])]
    determined pred
       = case classifyPredType pred of
            EqPred NomEq t1 t2 -> [([t1],[t2]), ([t2],[t1])]
               -- See Note [Equality superclasses]

            ClassPred cls tys | not (isIPClass cls)
               -- isIPClass: see Note [closeWrtFunDeps ignores implicit parameters]
                              -> [ instFD fd cls_tvs tys
                                 | let (cls_tvs, cls_fds) = classTvsFds cls
                                 , fd <- cls_fds ]
            _ -> []

{- Note [closeWrtFunDeps ignores implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Implicit params don't really determine a type variable (that is, we might have
IP "c" Bool and IP "c" Int in different places within the same program), and
skipping this causes implicit params to monomorphise too many variables; see
Note [Inheriting implicit parameters] in GHC.Tc.Solver.  Skipping causes
typecheck/should_compile/tc219 to fail.
-}

{- *********************************************************************
*                                                                      *
        Check that a new instance decl is OK wrt fundeps
*                                                                      *
************************************************************************

Here is the bad case:
        class C a b | a->b where ...
        instance C Int Bool where ...
        instance C Int Char where ...

The point is that a->b, so Int in the first parameter must uniquely
determine the second.  In general, given the same class decl, and given

        instance C s1 s2 where ...
        instance C t1 t2 where ...

Then the criterion is: if U=unify(s1,t1) then U(s2) = U(t2).

Matters are a little more complicated if there are free variables in
the s2/t2.

        class D a b c | a -> b
        instance D a b => D [(a,a)] [b] Int
        instance D a b => D [a]     [b] Bool

The instance decls don't overlap, because the third parameter keeps
them separate.  But we want to make sure that given any constraint
        D s1 s2 s3
if s1 matches

Note [Bogus consistency check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In checkFunDeps we check that a new ClsInst is consistent with all the
ClsInsts in the environment.

The bogus aspect is discussed in #10675. Currently it if the two
types are *contradictory*, using (isNothing . tcUnifyTys).  But all
the papers say we should check if the two types are *equal* thus
   not (substTys subst rtys1 `eqTypes` substTys subst rtys2)
For now I'm leaving the bogus form because that's the way it has
been for years.
-}

checkFunDeps :: InstEnvs -> ClsInst -> [ClsInst]
-- The Consistency Check.
-- Check whether adding DFunId would break functional-dependency constraints
-- Used only for instance decls defined in the module being compiled
-- Returns a list of the ClsInst in InstEnvs that are inconsistent
-- with the proposed new ClsInst
checkFunDeps inst_envs (ClsInst { is_tvs = qtvs1, is_cls = cls
                                , is_tys = tys1, is_tcs = rough_tcs1 })
  | null fds
  = []
  | otherwise
  = nubBy eq_inst $
    [ ispec | ispec <- cls_insts
            , fd    <- fds
            , is_inconsistent fd ispec ]
  where
    cls_insts      = classInstances inst_envs cls
    (cls_tvs, fds) = classTvsFds cls
    qtv_set1       = mkVarSet qtvs1

    is_inconsistent fd (ClsInst { is_tvs = qtvs2, is_tys = tys2, is_tcs = rough_tcs2 })
      | instanceCantMatch trimmed_tcs rough_tcs2
      = False
      | otherwise
      = case tcUnifyTyKis bind_fn ltys1 ltys2 of
          Nothing         -> False
          Just subst
            -> isNothing $   -- Bogus legacy test (#10675)
                             -- See Note [Bogus consistency check]
               tcUnifyTyKis bind_fn (substTysUnchecked subst rtys1) (substTysUnchecked subst rtys2)

      where
        trimmed_tcs    = trimRoughMatchTcs cls_tvs fd rough_tcs1
        (ltys1, rtys1) = instFD fd cls_tvs tys1
        (ltys2, rtys2) = instFD fd cls_tvs tys2
        qtv_set2       = mkVarSet qtvs2
        bind_fn        = matchBindFun (qtv_set1 `unionVarSet` qtv_set2)

    eq_inst i1 i2 = instanceDFunId i1 == instanceDFunId i2
        -- A single instance may appear twice in the un-nubbed conflict list
        -- because it may conflict with more than one fundep.  E.g.
        --      class C a b c | a -> b, a -> c
        --      instance C Int Bool Bool
        --      instance C Int Char Char
        -- The second instance conflicts with the first by *both* fundeps

trimRoughMatchTcs :: [TyVar] -> FunDep TyVar -> [RoughMatchTc] -> [RoughMatchTc]
-- Computing rough_tcs for a particular fundep
--     class C a b c | a -> b where ...
-- For each instance .... => C ta tb tc
-- we want to match only on the type ta; so our
-- rough-match thing must similarly be filtered.
-- Hence, we Nothing-ise the tb and tc types right here
--
-- Result list is same length as input list, just with more Nothings
trimRoughMatchTcs _clas_tvs _ [] = panic "trimRoughMatchTcs: nullary [RoughMatchTc]"
trimRoughMatchTcs clas_tvs (ltvs, _) (cls:mb_tcs)
  = cls : zipWith select clas_tvs mb_tcs
  where
    select clas_tv mb_tc | clas_tv `elem` ltvs = mb_tc
                         | otherwise           = RM_WildCard
