{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1998


This module contains definitions for the IdInfo for things that
have a standard form, namely:

- data constructors
- record selectors
- method and superclass selectors
- primitive operations
-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Types.Id.Make (
        mkDictFunId, mkDictSelId, mkDictSelRhs,

        mkFCallId,

        wrapNewTypeBody, unwrapNewTypeBody, wrapFamInstBody,
        DataConBoxer(..), vanillaDataConBoxer,
        mkDataConRep, mkDataConWorkId,
        DataConBangOpts (..), BangOpts (..),

    ) where

import GHC.Prelude

import GHC.Builtin.WiredIn.Prim
import GHC.Builtin.WiredIn.Types
import GHC.Builtin.KnownKeys

import GHC.Core
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Core.TyCo.Rep
import GHC.Core.FamInstEnv
import GHC.Core.Predicate( isUnaryClass )
import GHC.Core.Coercion
import GHC.Core.Reduction
import GHC.Core.Make
import GHC.Core.FVs     ( mkRuleInfo )
import GHC.Core.Utils   ( mkCast, coreAltsType )
import GHC.Core.Unfold.Make
import GHC.Core.SimpleOpt
import GHC.Core.TyCon
import GHC.Core.Class
import GHC.Core.DataCon

import GHC.Types.Literal
import GHC.Types.RepType ( countFunRepArgs, typePrimRep )
import GHC.Types.Name.Set
import GHC.Types.Name
import GHC.Types.ForeignCall
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.InlinePragma
import GHC.Types.Demand
import GHC.Types.Cpr
import GHC.Types.Unique.Supply
import GHC.Types.Basic       hiding ( SuccessFlag(..) )

import GHC.Tc.Utils.TcType as TcType

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Data.FastString
import GHC.Data.List.SetOps
import Data.List        ( zipWith4 )

-- A bit of a shame we must import these here
import GHC.StgToCmm.Types (LambdaFormInfo(..))
import GHC.Runtime.Heap.Layout (ArgDescr(ArgUnknown))

import GHC.Builtin.PrimOps.Ids (primOpId)
import GHC.Builtin.PrimOps (PrimOp(..))
import GHC.Platform (Platform)
import GHC.Platform.Tag (isSmallFamily)

{-
************************************************************************
*                                                                      *
\subsection{Data constructors}
*                                                                      *
************************************************************************

The wrapper for a constructor is an ordinary top-level binding that evaluates
any strict args, unboxes any args that are going to be flattened, and calls
the worker.

We're going to build a constructor that looks like:

        data (Data a, C b) =>  T a b = T1 !a !Int b

        T1 = /\ a b ->
             \d1::Data a, d2::C b ->
             \p q r -> case p of { p ->
                       case q of { q ->
                       Con T1 [a,b] [p,q,r]}}

Notice that

* d2 is thrown away --- a context in a data decl is used to make sure
  one *could* construct dictionaries at the site the constructor
  is used, but the dictionary isn't actually used.

* We have to check that we can construct Data dictionaries for
  the types a and Int.  Once we've done that we can throw d1 away too.

* We use (case p of q -> ...) to evaluate p, rather than "seq" because
  all that matters is that the arguments are evaluated.  "seq" is
  very careful to preserve evaluation order, which we don't need
  to be here.

  You might think that we could simply give constructors some strictness
  info, like PrimOps, and let CoreToStg do the let-to-case transformation.
  But we don't do that because in the case of primops and functions strictness
  is a *property* not a *requirement*.  In the case of constructors we need to
  do something active to evaluate the argument.

  Making an explicit case expression allows the simplifier to eliminate
  it in the (common) case where the constructor arg is already evaluated.

Note [Wrappers for data instance tycons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the case of data instances, the wrapper also applies the coercion turning
the representation type into the family instance type to cast the result of
the wrapper.  For example, consider the declarations

  data family Map k :: * -> *
  data instance Map (a, b) v = MapPair (Map a (Pair b v))

The tycon to which the datacon MapPair belongs gets a unique internal
name of the form :R123Map, and we call it the representation tycon.
In contrast, Map is the family tycon (accessible via
tyConFamInst_maybe). A coercion allows you to move between
representation and family type.  It is accessible from :R123Map via
tyConFamilyCoercion_maybe and has kind

  Co123Map a b v :: {Map (a, b) v ~ :R123Map a b v}

The wrapper and worker of MapPair get the types

        -- Wrapper
  $WMapPair :: forall a b v. Map a (Map a b v) -> Map (a, b) v
  $WMapPair a b v = MapPair a b v `cast` sym (Co123Map a b v)

        -- Worker
  MapPair :: forall a b v. Map a (Map a b v) -> :R123Map a b v

This coercion is conditionally applied by wrapFamInstBody.

It's a bit more complicated if the data instance is a GADT as well!

   data instance T [a] where
        T1 :: forall b. b -> T [Maybe b]

Hence we translate to

        -- Wrapper
  $WT1 :: forall b. b -> T [Maybe b]
  $WT1 b v = T1 (Maybe b) b (Maybe b) v
                        `cast` sym (Co7T (Maybe b))

        -- Worker
  T1 :: forall c b. (c ~ Maybe b) => b -> :R7T c

        -- Coercion from family type to representation type
  Co7T a :: T [a] ~ :R7T a

Newtype instances through an additional wrinkle into the mix. Consider the
following example (adapted from #15318, comment:2):

  data family T a
  newtype instance T [a] = MkT [a]

Within the newtype instance, there are three distinct types at play:

1. The newtype's underlying type, [a].
2. The instance's representation type, TList a (where TList is the
   representation tycon).
3. The family type, T [a].

We need two coercions in order to cast from (1) to (3):

(a) A newtype coercion axiom:

      axiom coTList a :: TList a ~ [a]

    (Where TList is the representation tycon of the newtype instance.)

(b) A data family instance coercion axiom:

      axiom coT a :: T [a] ~ TList a

When we translate the newtype instance to Core, we obtain:

    -- Wrapper
  $WMkT :: forall a. [a] -> T [a]
  $WMkT a x = MkT a x |> Sym (coT a)

    -- Worker
  MkT :: forall a. [a] -> TList [a]
  MkT a x = x |> Sym (coTList a)

Unlike for data instances, the worker for a newtype instance is actually an
executable function which expands to a cast, but otherwise, the general
strategy is essentially the same as for data instances. Also note that we have
a wrapper, which is unusual for a newtype, but we make GHC produce one anyway
for symmetry with the way data instances are handled.

Note [Newtype datacons]
~~~~~~~~~~~~~~~~~~~~~~~
The "data constructor" for a newtype should have no existentials. It's
not quite a "vanilla" data constructor, because the newtype arising from
     class C a => D a
looks like
       newtype T:D a = C:D (C a)
so the data constructor for T:C has a single argument, namely the
predicate (C a).  That ends up in the dcOtherTheta for the data con,
which makes it not vanilla.  So the assert just tests for existentials.
The rest is checked by having a singleton arg_tys.

Note [Newtype workers]
~~~~~~~~~~~~~~~~~~~~~~
A newtype does not really have a worker. Instead, newtype constructors
just unfold into a cast. But we need *something* for, say, MkAge to refer
to. So, we do this:

* The Id used as the newtype worker will have a compulsory unfolding to
  a cast. See Note [Compulsory newtype unfolding]

* This Id is labeled as a DataConWrapId. We don't want to use a DataConWorkId,
  as those have special treatment in the back end.

* There is no top-level binding, because the compulsory unfolding
  means that it will be inlined (to a cast) at every call site.

We probably should have a NewtypeWorkId, but these Ids disappear as soon as
we desugar anyway, so it seems a step too far.

Note [Compulsory newtype unfolding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Newtype wrappers, just like workers, have compulsory unfoldings.
This is needed so that two optimizations involving newtypes have the same
effect whether a wrapper is present or not:

(1) Case-of-known constructor.
    See Note [beta-reduction in exprIsConApp_maybe].

(2) Matching against the map/coerce RULE. Suppose we have the RULE

    {-# RULE "map/coerce" map coerce = ... #-}

    As described in Note [Getting the map/coerce RULE to work],
    the occurrence of 'coerce' is transformed into:

    {-# RULE "map/coerce" forall (c :: T1 ~R# T2).
                          map ((\v -> v) `cast` c) = ... #-}

    We'd like 'map Age' to match the LHS. For this to happen, Age
    must be unfolded, otherwise we'll be stuck. This is tested in T16208.

It also allows for the possibility of representation-polymorphic newtypes
with wrappers (with -XUnliftedNewtypes):

  newtype N (a :: TYPE r) = MkN a

With -XUnliftedNewtypes, this is allowed -- even though MkN is representation-
polymorphic. It's OK because MkN evaporates in the compiled code, becoming
just a cast. That is, it has a compulsory unfolding. As long as its
argument is not representation-polymorphic (which it can't be, according to
Note [Representation polymorphism invariants] in GHC.Core), and it's saturated,
no representation-polymorphic code ends up in the code generator.
The saturation condition is effectively checked in
GHC.Tc.Gen.Head.rejectRepPolyNewtypes.

However, if we make a *wrapper* for a newtype, we get into trouble.
In that case, we generate a forbidden representation-polymorphic
binding, and we must then ensure that it is always instantiated
at a representation-monomorphic type.

The solution is simple, though: just make the newtype wrappers
as ephemeral as the newtype workers. In other words, give the wrappers
compulsory unfoldings and no bindings. The compulsory unfolding is given
in wrap_unf in mkDataConRep, and the lack of a binding happens in
GHC.Iface.Tidy.getTyConImplicitBinds, where we say that a newtype has no
implicit bindings.

Note [Records and linear types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All the fields, in a record constructor, are linear, because there is no syntax
to specify the type of record field. There will be (see the proposal
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst#records-and-projections
), but it isn't implemented yet.

Projections of records can't be linear:

  data Foo = MkFoo { a :: A, b :: B }

If we had

  a :: Foo %1 -> A

We could write

  bad :: A %1 -> B %1 -> A
  bad x y = a (MkFoo { a=x, b=y })

There is an exception: if `b` (more generally all the fields besides `a`) is
unrestricted, then is perfectly possible to have a linear projection. Such a
linear projection has as simple definition.

  data Bar = MkBar { c :: C, d % Many :: D }

  c :: Bar %1 -> C
  c MkBar{ c=x, d=_} = x

The `% Many` syntax, for records, does not exist yet. But there is one important
special case which already happens: when there is a single field (usually a
newtype).

  newtype Baz = MkBaz { unbaz :: E }

unbaz could be linear. And, in fact, it is linear in the proposal design.

However, this hasn't been implemented yet.

************************************************************************
*                                                                      *
\subsection{Dictionary selectors}
*                                                                      *
************************************************************************

Selecting a field for a dictionary.  If there is just one field, then
there's nothing to do.

Dictionary selectors may get nested forall-types.  Thus:

        class Foo a where
          op :: forall b. Ord b => a -> b -> b

Then the top-level type for op is

        op :: forall a. Foo a =>
              forall b. Ord b =>
              a -> b -> b

Note [Type classes and linear types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constraints, in particular type classes, don't have attached linearity
information. Implicitly, they are all unrestricted. See the linear types proposal,
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst .

When translating to core `C => ...` is always translated to an unrestricted
arrow `C % Many -> ...`.

Therefore there is no loss of generality if we make all selectors unrestricted.

-}

mkDictSelId :: Name          -- Name of one of the *value* selectors
                             -- (dictionary superclass or method)
            -> Class -> Id
-- Important: see Note [ClassOp/DFun selection] in GHC.Tc.TyCl.Instance
mkDictSelId name clas
  = mkGlobalId (ClassOpId clas terminating) name sel_ty info
  where
    tycon          = classTyCon clas
    sel_names      = map idName (classAllSelIds clas)
    [data_con]     = tyConDataCons tycon
    tyvars         = dataConUserTyVarBinders data_con
    n_ty_args      = length tyvars
    arg_tys        = dataConRepArgTys data_con  -- Includes the dictionary superclasses
    val_index      = assoc "MkId.mkDictSelId" (sel_names `zip` [0..]) name

    pred_ty = mkClassPred clas (mkTyVarTys (binderVars tyvars))
    res_ty  = scaledThing (getNth arg_tys val_index)
    sel_ty  = mkForAllTys tyvars $
              mkFunctionType ManyTy pred_ty res_ty
             -- See Note [Type classes and linear types]

    terminating = isTerminatingType res_ty || definitelyUnliftedType res_ty
                  -- If the field is unlifted, it can't be bottom
                  -- Ditto if it's a terminating type

    base_info = noCafIdInfo
                `setArityInfo`  1
                `setDmdSigInfo` strict_sig
                `setCprSigInfo` topCprSig

    info = base_info `setRuleInfo` mkRuleInfo [rule]
           -- No unfolding for a dictionary selector; the RULE does the work,
           -- See Note [ClassOp/DFun selection] in GHC.Tc.TyCl.Instance

    -- This is the built-in rule that goes
    --      op (dfT d1 d2) --->  opT d1 d2
    rule = BuiltinRule { ru_name = fsLit "Class op " `appendFS`
                                     occNameFS (getOccName name)
                       , ru_key   = nameUnique name
                       , ru_nargs = n_ty_args + 1
                       , ru_try   = dictSelRule val_index n_ty_args }

        -- The strictness signature is of the form U(AAAVAAAA) -> T
        -- where the V depends on which item we are selecting
        -- It's worth giving one, so that absence info etc is generated
        -- even if the selector isn't inlined, which of course it isn't!

    strict_sig = mkClosedDmdSig [arg_dmd] topDiv
    arg_dmd = C_1N :* mkProd Unboxed dict_field_dmds
            where
              -- The evalDmd below is just a placeholder and will be replaced in
              -- GHC.Types.Demand.dmdTransformDictSel
              dict_field_dmds = [ if name == sel_name then evalDmd else absDmd
                                | sel_name <- sel_names ]

mkDictSelRhs :: Class
             -> Int         -- 0-indexed selector among (superclasses ++ methods)
             -> CoreExpr
-- See Note [ClassOp/DFun selection] in GHC.Tc.TyCl.Instance
mkDictSelRhs clas val_index
  = mkLams tyvars (Lam dict_id rhs_body)
  where
    tycon      = classTyCon clas
    [data_con] = tyConDataCons tycon
    tyvars     = dataConUnivTyVars data_con
    arg_tys    = dataConRepArgTys data_con  -- Includes the dictionary superclasses

    the_arg_id = getNth arg_ids val_index
    pred       = mkClassPred clas (mkTyVarTys tyvars)
    dict_id    = mkTemplateLocal 1 pred
    arg_ids    = mkTemplateLocalsNum 2 (map scaledThing arg_tys)

    rhs_body | isUnaryClass clas   -- Just having one sel_id isn't enough!
                                   -- E.g.  class (a ~# b) => a ~ b where {}
             , let sel_ids = classAllSelIds clas
             = assertPpr (val_index == 0)      (ppr clas) $
               assertPpr (length sel_ids == 1) (ppr clas) $
               Var (head sel_ids) `mkTyApps` mkTyVarTys tyvars `App` Var dict_id
             | otherwise
             = mkSingleAltCase (Var dict_id) dict_id (DataAlt data_con)
                               arg_ids (varToCoreExpr the_arg_id)
                                -- varToCoreExpr needed for equality superclass selectors
                                --   sel a b d = case x of { MkC _ (g:a~b) _ -> CO g }

dictSelRule :: Int -> Arity -> RuleFun
-- Tries to persuade the argument to look like a constructor
-- application, using exprIsConApp_maybe, and then selects
-- from it
--       sel_i t1..tk (D t1..tk op1 ... opm) = opi
--
-- See Note [ClassOp/DFun selection] in GHC.Tc.TyCl.Instance
dictSelRule val_index n_ty_args _ in_scope_env _ args
  | (dict_arg : _) <- drop n_ty_args args
  , Just (_, floats, _, _, con_args)
             <- exprIsConApp_maybe in_scope_env dict_arg
  = Just (wrapFloats floats $ getNth con_args val_index)
  | otherwise
  = Nothing

{-
************************************************************************
*                                                                      *
        Data constructors
*                                                                      *
************************************************************************
-}

mkDataConWorkId :: Name -> DataCon -> Id
mkDataConWorkId wkr_name data_con
  | isNewTyCon tycon       -- See Note [Newtype workers]
  = mkGlobalId (DataConWrapId data_con) wkr_name wkr_ty nt_info

  | otherwise
  = mkGlobalId (DataConWorkId data_con) wkr_name wkr_ty alg_wkr_info

  where
    tycon     = dataConTyCon data_con  -- The representation TyCon
    wkr_ty    = dataConRepType data_con
    univ_tvs  = dataConUnivTyVars data_con
    ex_tcvs   = dataConExTyCoVars data_con
    arg_tys   = dataConRepArgTys  data_con  -- Should be same as dataConOrigArgTys
    str_marks = dataConRepStrictness data_con

    ----------- Workers for data types --------------
    alg_wkr_info = noCafIdInfo
                   `setArityInfo`          wkr_arity
                   `setInlinePragInfo`     wkr_inline_prag
                   `setUnfoldingInfo`      evaldUnfolding  -- Record that it's evaluated,
                                                           -- even if arity = 0
                   `setDmdSigInfo`         wkr_sig
                      -- Workers eval their strict fields
                      -- See Note [Strict fields in Core]
                   `setLFInfo`             wkr_lf_info

    wkr_inline_prag = alwaysConLikePragma
    wkr_arity = dataConRepArity data_con

    wkr_sig = mkClosedDmdSig wkr_dmds topDiv
    wkr_dmds = map mk_dmd str_marks
    mk_dmd MarkedStrict    = evalDmd
    mk_dmd NotMarkedStrict = topDmd

    -- See Note [LFInfo of DataCon workers and wrappers]
    wkr_lf_info
      | wkr_arity == 0 = LFCon data_con
      | otherwise      = LFReEntrant TopLevel (countFunRepArgs wkr_arity wkr_ty) True ArgUnknown
                                            -- LFInfo stores post-unarisation arity

    ----------- Workers for newtypes --------------
    nt_info  = noCafIdInfo          -- The NoCaf-ness is set by noCafIdInfo
               `setArityInfo` 1  -- Arity 1
               `setInlinePragInfo` dataConWrapperInlinePragma
               `setUnfoldingInfo`  mkCompulsoryUnfolding newtype_rhs
               `setLFInfo` (panic "mkDataConWorkId: no LFInfo for newtype worker ids")
                           -- See W1 in Note [LFInfo of DataCon workers and wrappers]

    id_arg1     = mkScaledTemplateLocal 1 (head arg_tys)
    res_ty_args = mkTyCoVarTys univ_tvs
    newtype_rhs =  assertPpr (null ex_tcvs && isSingleton arg_tys) (ppr data_con) $
                              -- Note [Newtype datacons]
                   mkLams univ_tvs $ Lam id_arg1 $
                   wrapNewTypeBody tycon res_ty_args (Var id_arg1)

{-
Note [LFInfo of DataCon workers and wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As noted in Note [The LFInfo of Imported Ids] in GHC.StgToCmm.Closure, it's
crucial that saturated data con applications are given an LFInfo of `LFCon`.

Since for data constructors we never serialise the worker and the wrapper (only
the data type declaration), we never serialise their lambda form info either.

Therefore, when making data constructors workers and wrappers, we construct a
correct `LFInfo` for them right away, and put it it in the `lfInfo` field of the
worker/wrapper Id, ensuring that:

  The `lfInfo` field of a DataCon worker or wrapper is always populated with the correct LFInfo.

How do we construct a /correct/ LFInfo for workers and wrappers?
(Remember: `LFCon` means "a saturated constructor application")

(1) Data constructor workers and wrappers with arity > 0 are unambiguously
    functions and should be given `LFReEntrant`, regardless of the runtime
    relevance of the arguments.  For example:
       `Just :: a -> Maybe a`          is given `LFReEntrant`,
       `HNil :: (a ~# '[]) -> HList a` is given `LFReEntrant` too.

(2) A datacon /worker/ with zero arity is trivially fully saturated -- it takes
    no arguments whatsoever (not even zero-width args), so it is given `LFCon`.

(3) Perhaps surprisingly, a datacon /wrapper/ can be an `LFCon`. See Wrinkle (W1) below.
    A datacon /wrapper/ with zero arity must be a fully saturated application of
    the worker to zero-width arguments only (which are dropped after unarisation),
    and therefore is also given `LFCon`.

For example, consider the following data constructors:

  data T1 a where
    TCon1 :: {-# UNPACK #-} !(a :~: True) -> T1 a

  data T2 a where
    TCon2 :: {-# UNPACK #-} !() -> T2 a

  data T3 a where
    TCon3 :: T3 '[]

`TCon1`'s wrapper has a lifted argument, which is non-zero-width, while the
worker has an unlifted equality argument, which is zero-width.

`TCon2`'s wrapper has a lifted argument, which is non-zero-width, while the
worker has no arguments.

Wrinkle (W1). Perhaps surprisingly, it is possible for the /wrapper/ to be an
`LFCon` even though the /worker/ is not. Consider `T3` above. Here is the
Core representation of the worker and wrapper:

  $WTCon3 :: T3 '[]             -- Wrapper
  $WTCon3 = TCon3 @[] <Refl>    -- A saturated constructor application: LFCon

  TCon3 :: forall (a :: * -> *). (a ~# []) => T a   -- Worker
  TCon3 = /\a. \(co :: a~#[]). TCon3 co             -- A function: LFReEntrant

For `TCon1`, both the wrapper and worker will be given `LFReEntrant` since they
both have arity == 1.

For `TCon2`, the wrapper will be given `LFReEntrant` since it has arity == 1
while the worker is `LFCon` since its arity == 0

For `TCon3`, the wrapper will be given `LFCon` since its arity == 0 and the
worker `LFReEntrant` since its arity == 1

One might think we could give *workers* with only zero-width-args the `LFCon`
LambdaFormInfo, e.g. give `LFCon` to the worker of `TCon1` and `TCon3`.
However, these workers are unambiguously functions
-- which makes `LFReEntrant`, the LambdaFormInfo we give them, correct.
See also the discussion in #23158.

Wrinkles:

(W1) Why do we panic when generating `LFInfo` for newtype workers and wrappers?

  We don't generate code for newtype workers/wrappers, so we should never have to
  look at their LFInfo (and in general we can't; they may be representation-polymorphic).

See also the Note [Imported unlifted nullary datacon wrappers must have correct LFInfo]
in GHC.StgToCmm.Types.

-------------------------------------------------
--         Data constructor representation
--
-- This is where we decide how to wrap/unwrap the
-- constructor fields
--
--------------------------------------------------
-}

type Unboxer = Var -> UniqSM ([Var], CoreExpr -> CoreExpr)
  -- Unbox: bind rep vars by decomposing src var

data Boxer = UnitBox | Boxer (Subst -> UniqSM ([Var], CoreExpr))
  -- Box:   build src arg using these rep vars

-- | Data Constructor Boxer
newtype DataConBoxer = DCB ([Type] -> [Var] -> UniqSM ([Var], [CoreBind]))
                       -- Bind these src-level vars, returning the
                       -- rep-level vars to bind in the pattern

vanillaDataConBoxer :: DataConBoxer
-- No transformation on arguments needed
vanillaDataConBoxer = DCB (\_tys args -> return (args, []))

{-
Note [Inline partially-applied constructor wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We allow the wrapper to inline when partially applied to avoid
boxing values unnecessarily. For example, consider

   data Foo a = Foo !Int a

   instance Traversable Foo where
     traverse f (Foo i a) = Foo i <$> f a

This desugars to

   traverse f foo = case foo of
        Foo i# a -> let i = I# i#
                    in map ($WFoo i) (f a)

If the wrapper `$WFoo` is not inlined, we get a fruitless reboxing of `i`.
But if we inline the wrapper, we get

   map (\a. case i of I# i# a -> Foo i# a) (f a)

and now case-of-known-constructor eliminates the redundant allocation.

-}

data DataConBangOpts
  = FixedBangOpts [HsImplBang]
    -- ^ Used for imported data constructors
    -- See Note [Bangs on imported data constructors]
  | SrcBangOpts !BangOpts

data BangOpts = BangOpts
  { bang_opt_strict_data   :: !Bool -- ^ Strict fields by default
  , bang_opt_unbox_disable :: !Bool -- ^ Disable automatic field unboxing (e.g. if we aren't optimising)
  , bang_opt_unbox_strict  :: !Bool -- ^ Unbox strict fields
  , bang_opt_unbox_small   :: !Bool -- ^ Unbox small strict fields
  }

mkDataConRep :: Platform
             -> DataConBangOpts
             -> FamInstEnvs
             -> Name
             -> DataCon
             -> UniqSM (DataConRep, [HsImplBang], [StrictnessMark])
mkDataConRep platform dc_bang_opts fam_envs wrap_name data_con
  | not wrapper_reqd
  = return (NoDataConRep, arg_ibangs, rep_strs)

  | otherwise
  = do { wrap_args <- mapM (newLocal (fsLit "conrep")) wrap_arg_tys
       ; wrap_body <- mk_rep_app (dropList stupid_theta wrap_args `zip` dropList eq_spec unboxers)
                                 initial_wrap_app
                        -- Drop the stupid theta arguments, as per
                        -- Note [Instantiating stupid theta] in GHC.Core.DataCon.

       ; let wrap_id = mkGlobalId (DataConWrapId data_con) wrap_name wrap_ty wrap_info
             wrap_info = noCafIdInfo
                         `setArityInfo`         wrap_arity
                             -- It's important to specify the arity, so that partial
                             -- applications are treated as values
                         `setInlinePragInfo`    wrap_prag
                         `setUnfoldingInfo`     wrap_unf
                         `setDmdSigInfo`        wrap_sig
                             -- We need to get the CAF info right here because GHC.Iface.Tidy
                             -- does not tidy the IdInfo of implicit bindings (like the wrapper)
                             -- so it not make sure that the CAF info is sane
                         `setLFInfo`            wrap_lf_info

             -- The signature is purely for passes like the Simplifier, not for
             -- DmdAnal itself; see Note [DmdAnal for DataCon wrappers].
             wrap_sig = mkClosedDmdSig wrap_arg_dmds topDiv

             -- See Note [LFInfo of DataCon workers and wrappers]
             wrap_lf_info
               | wrap_arity == 0  = LFCon data_con
               -- See W1 in Note [LFInfo of DataCon workers and wrappers]
               | isNewTyCon tycon = panic "mkDataConRep: we shouldn't look at LFInfo for newtype wrapper ids"
               | otherwise        = LFReEntrant TopLevel (countFunRepArgs wrap_arity wrap_ty) True ArgUnknown
                                                      -- LFInfo stores post-unarisation arity

             wrap_arg_dmds =
               replicate (length stupid_theta + length theta) topDmd
                 ++ map mk_dmd arg_ibangs
               -- Don't forget the dictionary arguments when building
               -- the strictness signature (#14290, #26748).

             mk_dmd str | isBanged str = evalDmd
                        | otherwise    = topDmd

             wrap_prag
               | new_tycon
               -- See Note [Desugaring unlifted newtypes] in GHC.Core.SimpleOpt.
               = dataConWrapperInlinePragma
               | otherwise
               = dataConWrapperInlinePragma
                    `setInlinePragmaActivation` activateDuringFinal
                         -- See Note [Activation for data constructor wrappers]

             -- The wrapper will usually be inlined (see wrap_unf), so its
             -- strictness and CPR info is usually irrelevant. But this is
             -- not always the case; GHC may choose not to inline it. In
             -- particular, the wrapper constructor is not inlined inside
             -- an INLINE rhs or when it is not applied to any arguments.
             -- See Note [Inline partially-applied constructor wrappers]
             -- Passing Nothing here allows the wrapper to inline when
             -- unsaturated.
             wrap_unf | isNewTyCon tycon = mkCompulsoryUnfolding wrap_rhs
                        -- See Note [Compulsory newtype unfolding]
                      | otherwise        = mkDataConUnfolding wrap_rhs
             wrap_rhs = mkCoreTyLams wrap_tvbs $
                        mkCoreLams wrap_args $
                        wrapFamInstBody tycon res_ty_args non_wrap_arg_ty $
                        wrap_body

       ; return (DCR { dcr_wrap_id = wrap_id
                     , dcr_boxer   = mk_boxer boxers
                     , dcr_arg_tys = rep_tys }
                , arg_ibangs, rep_strs) }

  where
    (univ_tvs, ex_tvs, eq_spec, theta, orig_arg_tys, _orig_res_ty)
                 = dataConFullSig data_con
    stupid_theta = dataConStupidTheta data_con
    wrap_tvbs    = dataConUserTyVarBinders data_con
    res_ty_args  = dataConResRepTyArgs data_con

    tycon        = dataConTyCon data_con       -- The representation TyCon (not family)
    wrap_ty      = dataConWrapperType data_con
    ev_tys       = eqSpecPreds eq_spec ++ theta
    all_arg_tys  = map unrestricted ev_tys ++ orig_arg_tys
    ev_ibangs    = map (const HsLazy) ev_tys
    orig_bangs   = dataConSrcBangs data_con

    wrap_arg_tys
      | new_tycon
      -- See Wrinkle [Unlifted newtypes with wrappers]
      -- in Note [Desugaring unlifted newtypes] in GHC.Core.SimpleOpt.
      = map unrestricted stupid_theta
      | otherwise
      = (map unrestricted $ stupid_theta ++ theta) ++ orig_arg_tys
    non_wrap_arg_ty
      | new_tycon
      , [arg_ty] <- map unrestricted theta ++ orig_arg_tys
      = Just arg_ty
      | otherwise
      = Nothing

    wrap_arity   = count isCoVar ex_tvs + length wrap_arg_tys
             -- The wrap_args are the arguments *other than* the eq_spec
             -- Because we are going to apply the eq_spec args manually in the
             -- wrapper

    new_tycon = isNewTyCon tycon
    arg_ibangs
      | new_tycon
      = map (const HsLazy) orig_arg_tys -- See Note [HsImplBangs for newtypes]
                                        -- orig_arg_tys should be a singleton, but
                                        -- if a user declared a wrong newtype we
                                        -- detect this later (see test T2334A)
      | otherwise
      = case dc_bang_opts of
          SrcBangOpts bang_opts -> zipWith (dataConSrcToImplBang platform bang_opts fam_envs)
                                    orig_arg_tys orig_bangs
          FixedBangOpts bangs   -> bangs

    (rep_tys_w_strs, wrappers)
      = unzip (zipWith (dataConArgRep platform) all_arg_tys (ev_ibangs ++ arg_ibangs))

    (unboxers, boxers) = unzip wrappers
    (rep_tys, rep_strs) = unzip (concat rep_tys_w_strs)

    -- This is True if the data constructor or class dictionary constructor
    -- needs a wrapper. This wrapper is injected into the program later in the CoreTidy
    -- pass. See Note [Injecting implicit bindings] in GHC.CoreToStg.AddImplicitBinds
    -- along with the accompanying implementation in getTyConImplicitBinds.
    wrapper_reqd
      | isTypeDataTyCon tycon
        -- `type data` declarations never have data-constructor wrappers
        -- Their data constructors only live at the type level, in the
        -- form of PromotedDataCon, and therefore do not need wrappers.
        -- See wrinkle (W0) in Note [Type data declarations] in GHC.Rename.Module.
      = False

      | isUnaryClassTyCon tycon   -- See (UCM8) in Note [Unary class magic]
      = False                     -- in GHC.Core.TyCon

      | otherwise
      = (not new_tycon
                     -- (Most) newtypes have only a worker, with the exception
                     -- of some newtypes written with GADT syntax.
                     -- See dataConUserTyVarsNeedWrapper below.
         && (any isUnpacked (ev_ibangs ++ arg_ibangs)))
                     -- Some unboxing (includes eq_spec)

      || isFamInstTyCon tycon -- Cast result

      || dataConUserTyVarBindersNeedWrapper data_con
                     -- If the data type was written with GADT syntax and
                     -- orders the type variables differently from what the
                     -- worker expects, it needs a data con wrapper to reorder
                     -- the type variables.
                     -- See Note [Data con wrappers and GADT syntax].
                     --
                     -- NB: All GADTs return true from this function, but there
                     -- is one exception that we must check below.

      || not (null stupid_theta)
                     -- If the data constructor has a datatype context,
                     -- we need a wrapper in order to drop the stupid arguments.
                     -- See Note [Instantiating stupid theta] in GHC.Core.DataCon.

    initial_wrap_app = Var (dataConWorkId data_con)
                       `mkTyApps`  res_ty_args
                       `mkVarApps` ex_tvs
                       `mkCoApps`  map (mkReflCo Nominal . eqSpecType) eq_spec

    mk_boxer :: [Boxer] -> DataConBoxer
    mk_boxer boxers = DCB (\ ty_args src_vars ->
                      do { let (ex_vars, term_vars) = splitAtList ex_tvs src_vars
                               subst1 = zipTvSubst univ_tvs ty_args
                               subst2 = foldl2 extendTvSubstWithClone subst1 ex_tvs ex_vars
                         ; (rep_ids, binds) <- go subst2 boxers term_vars
                         ; return (ex_vars ++ rep_ids, binds) } )

    go _ [] src_vars = assertPpr (null src_vars) (ppr data_con) $ return ([], [])
    go subst (UnitBox : boxers) (src_var : src_vars)
      = do { (rep_ids2, binds) <- go subst boxers src_vars
           ; return (src_var : rep_ids2, binds) }
    go subst (Boxer boxer : boxers) (src_var : src_vars)
      = do { (rep_ids1, arg)  <- boxer subst
           ; (rep_ids2, binds) <- go subst boxers src_vars
           ; return (rep_ids1 ++ rep_ids2, NonRec src_var arg : binds) }
    go _ (_:_) [] = pprPanic "mk_boxer" (ppr data_con)

    mk_rep_app :: [(Id,Unboxer)] -> CoreExpr -> UniqSM CoreExpr
    mk_rep_app [] con_app
      = return con_app
    mk_rep_app ((wrap_arg, unboxer) : prs) con_app
      = do { (rep_ids, unbox_fn) <- unboxer wrap_arg
           ; expr <- mk_rep_app prs (mkVarApps con_app rep_ids)
           ; return (unbox_fn expr) }


dataConWrapperInlinePragma :: InlinePragmaInfo
-- See Note [DataCon wrappers are conlike]
dataConWrapperInlinePragma =  alwaysInlineConLikePragma

{- Note [Activation for data constructor wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Activation on a data constructor wrapper allows it to inline only in FinalPhase.
This way rules have a chance to fire if they mention a data constructor on
the left
   RULE "foo"  f (K a b) = ...
Since the LHS of rules are simplified with InitialPhase, we won't
inline the wrapper on the LHS either.

On the other hand, this means that exprIsConApp_maybe must be able to deal
with wrappers so that case-of-constructor is not delayed; see
Note [exprIsConApp_maybe on data constructors with wrappers] for details.

It used to activate in phases 2 (afterInitial) and later, but it makes it
awkward to write a RULE[1] with a constructor on the left: it would work if a
constructor has no wrapper, but whether a constructor has a wrapper depends, for
instance, on the order of type argument of that constructors. Therefore changing
the order of type argument could make previously working RULEs fail.

See also https://gitlab.haskell.org/ghc/ghc/issues/15840 .

Note [DataCon wrappers are conlike]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DataCon workers are clearly ConLike --- they are the “Con” in
“ConLike”, after all --- but what about DataCon wrappers? Should they
be marked ConLike, too?

Yes, absolutely! As described in Note [CONLIKE pragma] in
GHC.Types.Basic, isConLike influences GHC.Core.Utils.exprIsExpandable,
which is used by both RULE matching and the case-of-known-constructor
optimization. It’s crucial that both of those things can see
applications of DataCon wrappers:

  * User-defined RULEs match on wrappers, not workers, so we might
    need to look through an unfolding built from a DataCon wrapper to
    determine if a RULE matches.

  * Likewise, if we have something like
        let x = $WC a b in ... case x of { C y z -> e } ...
    we still want to apply case-of-known-constructor.

Therefore, it’s important that we consider DataCon wrappers conlike.
This is especially true now that we don’t inline DataCon wrappers
until the final simplifier phase; see Note [Activation for data
constructor wrappers].

For further reading, see:
  * (IA1) in Note [Interesting arguments] in GHC.Core.Op.Simplify.Utils
  * Note [Lone variables] in GHC.Core.Unfold
  * Note [exprIsConApp_maybe on data constructors with wrappers]
    in GHC.Core.SimpleOpt
  * #18012

Note [Bangs on imported data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We pass Maybe [HsImplBang] to mkDataConRep to make use of HsImplBangs
from imported modules.

- Nothing <=> use HsSrcBangs
- Just bangs <=> use HsImplBangs

For imported types we can't work it all out from the HsSrcBangs,
because we want to be very sure to follow what the original module
(where the data type was declared) decided, and that depends on what
flags were enabled when it was compiled. So we record the decisions in
the interface file.

The HsImplBangs passed are in 1-1 correspondence with the
dataConOrigArgTys of the DataCon.

Note [Data con wrappers and unlifted types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   data T = MkT !Int#

We certainly do not want to make a wrapper
   $WMkT x = case x of y { DEFAULT -> MkT y }

For a start, it's still to generate a no-op.  But worse, since wrappers
are currently injected at TidyCore, we don't even optimise it away!
So the stupid case expression stays there.  This actually happened for
the Integer data type (see #1600 comment:66)!

Note [Data con wrappers and GADT syntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider these two very similar data types:

  data T1 a b = MkT1 b

  data T2 a b where
    MkT2 :: forall b a. b -> T2 a b

Despite their similar appearance, T2 will have a data con wrapper but T1 will
not. What sets them apart? The types of their constructors, which are:

  MkT1 :: forall a b. b -> T1 a b
  MkT2 :: forall b a. b -> T2 a b

MkT2's use of GADT syntax allows it to permute the order in which `a` and `b`
would normally appear. See Note [DataCon user type variable binders] in GHC.Core.DataCon
for further discussion on this topic.

The worker data cons for T1 and T2, however, both have types such that `a` is
expected to come before `b` as arguments. Because MkT2 permutes this order, it
needs a data con wrapper to swizzle around the type variables to be in the
order the worker expects.

A somewhat surprising consequence of this is that *newtypes* can have data con
wrappers! After all, a newtype can also be written with GADT syntax:

  newtype T3 a b where
    MkT3 :: forall b a. b -> T3 a b

Again, this needs a wrapper data con to reorder the type variables. It does
mean that this newtype constructor requires another level of indirection when
being called, but the inliner should make swift work of that.

Note [HsImplBangs for newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Most of the time, we use the dataConSrctoImplBang function to decide what
strictness/unpackedness to use for the fields of a data type constructor. But
there is an exception to this rule: newtype constructors. You might not think
that newtypes would pose a challenge, since newtypes are seemingly forbidden
from having strictness annotations in the first place. But consider this
(from #16141):

  {-# LANGUAGE StrictData #-}
  {-# OPTIONS_GHC -O #-}
  newtype T a b where
    MkT :: forall b a. Int -> T a b

Because StrictData (plus optimization) is enabled, invoking
dataConSrcToImplBang would sneak in and unpack the field of type Int to Int#!
This would be disastrous, since the wrapper for `MkT` uses a coercion involving
Int, not Int#.

Bottom line: dataConSrcToImplBang should never be invoked for newtypes. In the
case of a newtype constructor, we simply hardcode its dcr_bangs field to
[HsLazy].
-}

-------------------------

-- | Conjure a fresh local binder.
newLocal :: FastString   -- ^ a string which will form part of the 'Var'\'s name
         -> Scaled Type  -- ^ the type of the 'Var'
         -> UniqSM Var
newLocal name_stem (Scaled w ty) =
    mkSysLocalOrCoVarM name_stem w ty
         -- We should not have "OrCoVar" here, this is a bug (#17545)


-- | Unpack/Strictness decisions from source module.
--
-- This function should only ever be invoked for data constructor fields, and
-- never on the field of a newtype constructor.
-- See @Note [HsImplBangs for newtypes]@.
dataConSrcToImplBang
   :: Platform
   -> BangOpts
   -> FamInstEnvs
   -> Scaled Type
   -> HsSrcBang
   -> HsImplBang

dataConSrcToImplBang platform bang_opts fam_envs arg_ty
                     (HsSrcBang ann unpk NoSrcStrict)
  | bang_opt_strict_data bang_opts -- StrictData => strict field
  = dataConSrcToImplBang platform bang_opts fam_envs arg_ty
                  (HsSrcBang ann unpk SrcStrict)
  | otherwise -- no StrictData => lazy field
  = HsLazy

dataConSrcToImplBang _ _ _ _ (HsSrcBang _ _ SrcLazy)
  = HsLazy

dataConSrcToImplBang platform bang_opts fam_envs arg_ty
                     (HsSrcBang _ unpk_prag SrcStrict)
  | isUnliftedType (scaledThing arg_ty)
    -- NB: non-newtype data constructors can't have representation-polymorphic fields
    -- so this is OK.
  = HsLazy  -- For !Int#, say, use HsLazy
            -- See Note [Data con wrappers and unlifted types]

  | let mb_co   = topNormaliseType_maybe fam_envs (scaledThing arg_ty)
                     -- Unwrap type families and newtypes
        arg_ty' = case mb_co of
                    { Just redn -> scaledSet arg_ty (reductionReducedType redn)
                    ; Nothing   -> arg_ty }
  , shouldUnpackArgTy platform bang_opts unpk_prag fam_envs arg_ty'
  = if bang_opt_unbox_disable bang_opts
    then HsStrict True -- Not unpacking because of -O0
                       -- See Note [Detecting useless UNPACK pragmas] in GHC.Core.DataCon
    else case mb_co of
           Nothing   -> HsUnpack Nothing
           Just redn -> HsUnpack (Just $ reductionCoercion redn)

  | otherwise -- Record the strict-but-no-unpack decision
  = HsStrict False

-- | Wrappers/Workers and representation following Unpack/Strictness
-- decisions
dataConArgRep
  :: Platform
  -> Scaled Type
  -> HsImplBang
  -> ([(Scaled Type,StrictnessMark)] -- Rep types
     ,(Unboxer,Boxer))

dataConArgRep _ arg_ty HsLazy
  = ([(arg_ty, NotMarkedStrict)], (unitUnboxer, unitBoxer))

dataConArgRep _ arg_ty (HsStrict _)
  = ([(arg_ty, MarkedStrict)], (unitUnboxer, unitBoxer)) -- Seqs are inserted in STG

dataConArgRep platform arg_ty (HsUnpack Nothing)
  = dataConArgUnpack platform arg_ty

dataConArgRep platform (Scaled w _) (HsUnpack (Just co))
  | let co_rep_ty = coercionRKind co
  , (rep_tys, wrappers) <- dataConArgUnpack platform (Scaled w co_rep_ty)
  = (rep_tys, wrapCo co co_rep_ty wrappers)


-------------------------
wrapCo :: Coercion -> Type -> (Unboxer, Boxer) -> (Unboxer, Boxer)
wrapCo co rep_ty (unbox_rep, box_rep)  -- co :: arg_ty ~ rep_ty
  = (unboxer, boxer)
  where
    unboxer arg_id = do { rep_id <- newLocal (fsLit "cowrap_unbx") (Scaled (idMult arg_id) rep_ty)
                        ; (rep_ids, rep_fn) <- unbox_rep rep_id
                        ; let co_bind = NonRec rep_id (Var arg_id `Cast` co)
                        ; return (rep_ids, Let co_bind . rep_fn) }
    boxer = Boxer $ \ subst ->
            do { (rep_ids, rep_expr)
                    <- case box_rep of
                         UnitBox -> do { rep_id <- newLocal (fsLit "cowrap_bx")
                                                       (linear $ TcType.substTy subst rep_ty)
                                       ; return ([rep_id], Var rep_id) }
                         Boxer boxer -> boxer subst
               ; let sco = substCo subst co
               ; return (rep_ids, rep_expr `Cast` mkSymCo sco) }

------------------------
unitUnboxer :: Unboxer
unitUnboxer v = return ([v], \e -> e)

unitBoxer :: Boxer
unitBoxer = UnitBox

-------------------------

{- Note [UNPACK for sum types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have a data type D, for example:
    data D = D1 [Int] [Bool]
           | D2

and another data type which unpacks a field of type D:
    data U a = MkU {-# UNPACK #-} !D
                   {-# UNPACK #-} !(a,a)
                   {-# UNPACK #-} !D

Then the wrapper and worker for MkU have these types

  -- Wrapper
  $WMkU :: D -> (a,a) -> D -> U a

  -- Worker
  MkU :: (# (# [Int],[Bool] #) | (# #) #)
      -> a
      -> a
      -> (# (# [Int],[Bool] #) | (# #) #)
      -> U a

For each unpacked /sum/-type argument, the worker gets one argument.
But for each unpacked /product/-type argument, the worker gets N
arguments (here two).

Why treat them differently?  See Note [Why sums and products are treated differently].

The wrapper $WMkU looks like this:

  $WMkU :: D -> (a,a) -> D -> U a
  $WMkU x1 y x2
    = case (case x1 of {
              D1 a b -> (# (# a,b #) | #)
              D2     -> (# | (# #) #) }) of { x1_ubx ->
      case y of { (y1, y2) ->
      case (case x2 of {
              D1 a b -> (# (# a,b #) | #)
              D2     -> (# | (# #) #) }) of { x2_ubx ->
      MkU x1_ubx y1 y2 x2_ubx

Notice the nested case needed for sums.

This different treatment for sums and product is implemented in
dataConArgUnpackSum and dataConArgUnpackProduct respectively.

Note [Why sums and products are treated differently]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Can we handle sums like products, with each wrapper argument
occupying multiple argument slots in the worker?  No: for a sum
type the number of argument slots varies, and that's exactly what
unboxed sums are designed for.

Can we handle products like sums, with each wrapper argument occupying
exactly one argument slot (and unboxed tuple) in the worker?  Yes,
we could.  For example
   data P = MkP {-# UNPACK #-} !Q
   data Q = MkQ {-# NOUNPACK #-} !Int
                {-# NOUNPACK #-} Int

Currently could unpack P thus, taking two slots in the worker
   $WMkP :: Q -> P
   $WMkP x = case x of { MkQ a b -> MkP a b }
   MkP :: Int -> Int -> P  -- Worker

We could instead do this (uniformly with sums)

   $WMkP1 :: Q -> P
   $WMkP1 x = case (case x of { MkQ a b -> (# a, b #) }) of ubx_x
              MkP1 ubx_x
   MkP1 :: (# Int, Int #) -> P  -- Worker

The representation of MkP and MkP1 would be identical (a constructor
with two fields).

BUT, with MkP (as with every data constructor) we record its argument
strictness as a bit-vector, actually [StrictnessMark]
   MkP strictness:  SL
This information is used in Core to record which fields are sure to
be evaluated.  (Look for calls to dataConRepStrictness.)  E.g. in Core
    case v of MkP x y -> ....<here x is known to be evald>....

Alas, with MkP1 this information is hidden by the unboxed pair,
In Core there will be an auxiliary case expression to take apart the pair:
    case v of MkP1 xy -> case xy of (# x,y #) -> ...
And now we have no easy way to know that x is evaluated in the "...".

Fixing this might be possible, but it'd be tricky.  So we avoid the
problem entirely by treating sums and products differently here.
-}

dataConArgUnpack
   :: Platform
   -> Scaled Type
   ->  ( [(Scaled Type, StrictnessMark)]   -- Rep types
       , (Unboxer, Boxer) )
dataConArgUnpack platform scaledTy@(Scaled _ arg_ty)
  | Just (tc, tc_args) <- splitTyConApp_maybe arg_ty
  = assert (not (isNewTyCon tc)) $
    case tyConDataCons tc of
      [con] -> dataConArgUnpackProduct scaledTy tc_args con
      cons | all (null . dataConOrigArgTys) cons
            -> dataConArgUnpackEnum platform scaledTy tc_args cons
      cons  -> dataConArgUnpackSum scaledTy tc_args cons
  | otherwise
  = pprPanic "dataConArgUnpack" (ppr arg_ty)
    -- An interface file specified Unpacked, but we couldn't unpack it

{- Note [UNPACK for enum types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a strict field has an enumeration type (all constructors are nullary),
we unpack it to a single narrow primitive word rather than an unboxed sum.

For example, given:
   data Color = Red | Green | Blue
   data Foo = MkFoo {-# UNPACK #-} !Color

the worker for MkFoo will have a Word8# field.

Avoiding the intermediate unboxed sum allows us to use branchless conversion
operations DataToTag and TagToEnum.
-}

dataConArgUnpackEnum
  :: Platform
  -> Scaled Type
  -> [Type]
  -> [DataCon]
  -> ( [(Scaled Type, StrictnessMark)]   -- Rep types
      , (Unboxer, Boxer) )
dataConArgUnpackEnum platform (Scaled arg_mult ty) _tc_args cons =
  ( [ (scaled_enum_ty, MarkedStrict) ] -- See Note [UNPACK for enum types]
  , ( unboxer, boxer ) )
  where
    !enum_sum_arity = length cons
    conv op e = App (Var (primOpId op)) e

    conv_tag_levpoly op e = App (mkTyApps (Var (primOpId op)) [getLevity ty, ty]) e

    (enum_ty, unbox_convert, box_convert)
       | enum_sum_arity < 256   = (word8PrimTy, conv WordToWord8Op, conv Word8ToWordOp)
       | enum_sum_arity < 65536 = (word16PrimTy, conv WordToWord16Op, conv Word16ToWordOp)
       | otherwise              = (wordPrimTy, id, id)
    scaled_enum_ty = Scaled arg_mult enum_ty

    datatotag_op
       | isSmallFamily platform enum_sum_arity = DataToTagSmallOp
       | otherwise                             = DataToTagLargeOp

    -- Tags are 1-based: add 1 to 0-based DataToTag result
    add_one e = App (App (Var (primOpId IntAddOp)) e)
                    (Lit (LitNumber LitNumInt 1))
    -- Subtract 1 to convert back to 0-based for TagToEnum
    sub_one e = App (App (Var (primOpId IntSubOp)) e)
                    (Lit (LitNumber LitNumInt 1))

    unboxer v = do enum_rep_id <- newLocal (fsLit "unbx_enum") scaled_enum_ty
                   let unbox_fn body
                              = mkSingleAltCase
                                    (unbox_convert (App (Var (primOpId IntToWordOp))
                                                        (add_one (conv_tag_levpoly datatotag_op (Var v)))))
                                    enum_rep_id
                                    DEFAULT
                                    []
                                    body
                   return ([enum_rep_id], unbox_fn)

    boxer = Boxer $ \ subst -> do
                let ty' = TcType.substTyUnchecked subst ty
                    conv_tag' op e = App (mkTyApps (Var (primOpId op)) [ty']) e
                enum_rep_id <- newLocal (fsLit "bx_enum")
                                        (TcType.substScaledTyUnchecked subst scaled_enum_ty)
                let box_fn = conv_tag'
                               TagToEnumOp
                               (sub_one (App (Var (primOpId WordToIntOp))
                                             (box_convert (Var enum_rep_id))))
                return ([enum_rep_id], box_fn)

dataConArgUnpackProduct
  :: Scaled Type
  -> [Type]
  -> DataCon
  -> ( [(Scaled Type, StrictnessMark)]   -- Rep types
     , (Unboxer, Boxer) )
dataConArgUnpackProduct (Scaled arg_mult _) tc_args con =
  assert (null (dataConExTyCoVars con)) $
    -- Note [Unpacking GADTs and existentials]
  let rep_tys = map (scaleScaled arg_mult) $ dataConInstArgTys con tc_args
  in ( rep_tys `zip` dataConRepStrictness con
     , ( \ arg_id ->
         do { rep_ids <- mapM (newLocal (fsLit "unbx")) rep_tys
            ; let r_mult = idMult arg_id
            ; let rep_ids' = map (scaleIdBy r_mult) rep_ids
            ; let unbox_fn body
                    = mkSingleAltCase (Var arg_id) arg_id
                               (DataAlt con) rep_ids' body
            ; return (rep_ids, unbox_fn) }
       , Boxer $ \ subst ->
         do { rep_ids <- mapM (newLocal (fsLit "bx") . TcType.substScaledTyUnchecked subst) rep_tys
            ; return (rep_ids, Var (dataConWorkId con)
                               `mkTyApps` (substTysUnchecked subst tc_args)
                               `mkVarApps` rep_ids ) } ) )

dataConArgUnpackSum
  :: Scaled Type
  -> [Type]
  -> [DataCon]
  -> ( [(Scaled Type, StrictnessMark)]   -- Rep types
     , (Unboxer, Boxer) )
dataConArgUnpackSum (Scaled arg_mult arg_ty) tc_args cons =
  ( [ (sum_ty, MarkedStrict) ] -- The idea: Unpacked variant will
                               -- be one field only, and the type of the
                               -- field will be an unboxed sum.
  , ( unboxer, boxer ) )
  where
    !ubx_sum_arity = length cons
    src_tys = map (\con -> map scaledThing $ dataConInstArgTys con tc_args) cons
    sum_alt_tys = map mkUbxSumAltTy src_tys
    sum_ty_unscaled = mkSumTy sum_alt_tys
    sum_ty = Scaled arg_mult sum_ty_unscaled
    newLocal' fs = newLocal fs . Scaled arg_mult

    -- See Note [UNPACK for sum types]
    unboxer :: Unboxer
    unboxer arg_id = do
      con_arg_binders <- mapM (mapM (newLocal' (fsLit "unbx"))) src_tys
      ubx_sum_bndr <- newLocal (fsLit "unbx") sum_ty

      let
        mk_ubx_sum_alt :: Int -> DataCon -> [Var] -> CoreAlt
        mk_ubx_sum_alt alt con [bndr] = Alt (DataAlt con) [bndr]
            (mkCoreUnboxedSum ubx_sum_arity alt sum_alt_tys (Var bndr))

        mk_ubx_sum_alt alt con bndrs =
          let tuple = mkCoreUnboxedTuple (map Var bndrs)
           in Alt (DataAlt con) bndrs (mkCoreUnboxedSum ubx_sum_arity alt sum_alt_tys tuple )

        ubx_sum :: CoreExpr
        ubx_sum =
          let alts = zipWith3 mk_ubx_sum_alt [ 1 .. ] cons con_arg_binders
           in Case (Var arg_id) arg_id (coreAltsType alts) alts

        unbox_fn :: CoreExpr -> CoreExpr
        unbox_fn body =
          mkSingleAltCase ubx_sum ubx_sum_bndr DEFAULT [] body

      return ([ubx_sum_bndr], unbox_fn)

    boxer :: Boxer
    boxer = Boxer $ \ subst -> do
              unboxed_field_id <- newLocal' (fsLit "bx") (TcType.substTy subst sum_ty_unscaled)
              tuple_bndrs <- mapM (newLocal' (fsLit "bx") . TcType.substTy subst) sum_alt_tys

              let tc_args' = substTys subst tc_args
                  arg_ty' = substTy subst arg_ty

              con_arg_binders <-
                mapM (mapM (newLocal' (fsLit "bx")) . map (TcType.substTy subst)) src_tys

              let mk_sum_alt :: Int -> DataCon -> Var -> [Var] -> CoreAlt
                  mk_sum_alt alt con _ [datacon_bndr] =
                    ( Alt (DataAlt (sumDataCon alt ubx_sum_arity)) [datacon_bndr]
                      (Var (dataConWorkId con) `mkTyApps`  tc_args'
                                              `mkVarApps` [datacon_bndr] ))

                  mk_sum_alt alt con tuple_bndr datacon_bndrs =
                    ( Alt (DataAlt (sumDataCon alt ubx_sum_arity)) [tuple_bndr] (
                      Case (Var tuple_bndr) tuple_bndr arg_ty'
                        [ Alt (DataAlt (tupleDataCon Unboxed (length datacon_bndrs))) datacon_bndrs
                            (Var (dataConWorkId con) `mkTyApps`  tc_args'
                                                    `mkVarApps` datacon_bndrs ) ] ))

              return ( [unboxed_field_id],
                       Case (Var unboxed_field_id) unboxed_field_id arg_ty'
                            (zipWith4 mk_sum_alt [ 1 .. ] cons tuple_bndrs con_arg_binders) )

-- | Every alternative of an unboxed sum has exactly one field, and we use
-- unboxed tuples when we need more than one field. This generates an unboxed
-- tuple when necessary, to be used in unboxed sum alts.
mkUbxSumAltTy :: [Type] -> Type
mkUbxSumAltTy [ty] = ty
mkUbxSumAltTy tys  = mkTupleTy Unboxed tys

shouldUnpackArgTy :: Platform -> BangOpts -> SrcUnpackedness -> FamInstEnvs -> Scaled Type -> Bool
-- True if we ought to unpack the UNPACK the argument type
-- See Note [Recursive unboxing]
-- We look "deeply" inside rather than relying on the DataCons
-- we encounter on the way, because otherwise we might well
-- end up relying on ourselves!
shouldUnpackArgTy platform bang_opts prag fam_envs arg_ty
  | Just data_cons <- unpackable_type_datacons (scaledThing arg_ty)
  , all ok_con data_cons                -- Returns True only if we can't get a
                                        -- loop involving these data cons
  , should_unpack prag arg_ty data_cons -- ...hence the call to dataConArgUnpack in
                                        --    should_unpack won't loop
       -- See Wrinkle (W1b) of Note [Recursive unboxing] for this loopy stuff
  = True

  | otherwise
  = False
  where
    ok_con :: DataCon -> Bool      -- True <=> OK to unpack
    ok_con top_con                 -- False <=> not safe
      = ok_args emptyNameSet top_con
       where
         top_con_name = getName top_con

         ok_args dcs con
           = all (ok_arg dcs) $
             (dataConOrigArgTys con `zip` dataConSrcBangs con)
             -- NB: dataConSrcBangs gives the *user* request;
             -- We'd get a black hole if we used dataConImplBangs

         ok_arg :: NameSet -> (Scaled Type, HsSrcBang) -> Bool
         ok_arg dcs (Scaled _ ty, HsSrcBang _ unpack_prag str_prag)
           | strict_field str_prag
           , Just data_cons <- unpackable_type_datacons (topNormaliseType fam_envs ty)
           , should_unpack_conservative unpack_prag data_cons  -- Wrinkle (W3)
           = all (ok_rec_con dcs) data_cons                    --  of Note [Recursive unboxing]
           | otherwise
           = True        -- NB True here, in contrast to False at top level

         -- See Note [Recursive unboxing]
         --   * Do not look at the HsImplBangs to `con`; see Wrinkle (W1a)
         --   * For the "at the root" comments see Wrinkle (W2)
         ok_rec_con dcs con
           | dc_name == top_con_name   = False  -- Recursion at the root
           | dc_name `elemNameSet` dcs = True   -- Not at the root
           | otherwise                 = ok_args (dcs `extendNameSet` dc_name) con
           where
             dc_name = getName con

    strict_field :: SrcStrictness -> Bool
    -- True <=> strict field
    strict_field NoSrcStrict = bang_opt_strict_data bang_opts
    strict_field SrcStrict   = True
    strict_field SrcLazy     = False

    -- Determine whether we ought to unpack a field,
    -- based on user annotations if present.
    -- A conservative version of should_unpack that doesn't look at how
    -- many fields the field would unpack to... because that leads to a loop.
    -- "Conservative" = err on the side of saying "yes".
    should_unpack_conservative :: SrcUnpackedness -> [DataCon] -> Bool
    should_unpack_conservative SrcNoUnpack _   = False  -- {-# NOUNPACK #-}
    should_unpack_conservative SrcUnpack   _   = True   -- {-# NOUNPACK #-}
    should_unpack_conservative NoSrcUnpack dcs = not (is_sum dcs)
        -- is_sum: we never unpack sums without a pragma; otherwise be conservative

    -- Determine whether we ought to unpack a field,
    -- based on user annotations if present, and heuristics if not.
    should_unpack :: SrcUnpackedness -> Scaled Type -> [DataCon] -> Bool
    should_unpack prag arg_ty data_cons =
      case prag of
        SrcNoUnpack -> False -- {-# NOUNPACK #-}
        SrcUnpack   -> True  -- {-# UNPACK #-}
        NoSrcUnpack -- No explicit unpack pragma, so use heuristics
          | is_sum data_cons
          -> False -- Don't unpack sum types automatically, but they can
                   -- be unpacked with an explicit source UNPACK.
          | otherwise   -- Wrinkle (W4) of Note [Recursive unboxing]
          -> bang_opt_unbox_strict bang_opts
             || (bang_opt_unbox_small bang_opts
                 && is_small_rep)  -- See Note [Unpack one-wide fields]
      where
        (rep_tys, _) = dataConArgUnpack platform arg_ty

        -- Takes in the list of reps used to represent the dataCon after it's unpacked
        -- and tells us if they can fit into 8 bytes. See Note [Unpack one-wide fields]
        is_small_rep =
          let -- Neccesary to look through unboxed tuples.
              prim_reps = concatMap (typePrimRep . scaledThing . fst) $ rep_tys
              -- And then get the actual size of the unpacked constructor.
              rep_size = sum $ map primRepSizeW64_B prim_reps
          in rep_size <= 8

    is_sum :: [DataCon] -> Bool
    -- We never unpack sum types automatically
    -- (Product types, we do. Empty types are weeded out by unpackable_type_datacons.)
    is_sum (_:_:_) = True
    is_sum _       = False



unpackable_type_datacons :: Type -> Maybe [DataCon]
-- Given a type already assumed to have been normalized by topNormaliseType,
--    unpackable_type_datacons (T ty1 .. tyn) = Just datacons
-- iff the type can be unpacked (see Note [Unpacking GADTs and existentials])
-- and `datacons` are the data constructors of T
unpackable_type_datacons ty
  | Just (tc, _) <- splitTyConApp_maybe ty
  , not (isNewTyCon tc)
      -- isNewTyCon: even though `ty` has been normalised, whic includes looking
      -- through newtypes, it could still be a /recursive/ newtype, so we must
      -- check for that case
  , Just cons <- tyConDataCons_maybe tc
  , unpackable_cons cons
  = Just cons
  | otherwise
  = Nothing
  where
    unpackable_cons :: [DataCon] -> Bool
    -- True if we can unpack a value of type (T t1 .. tn),
    -- where T is an algebraic data type with these constructors
    -- See Note [Unpacking GADTs and existentials]
    unpackable_cons []   -- Don't unpack nullary sums; no need.
      = False            -- They already take zero bits; see (UC0)

    unpackable_cons [con]   -- Exactly one data constructor; see (UC1)
      = null (dataConExTyCoVars con)

    unpackable_cons cons  -- More than one data constructor; see (UC2)
      = all isVanillaDataCon cons

{-
Note [Unpacking GADTs and existentials]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Can we unpack a value of an algebraic data type T? For example
   data D a = MkD {-# UNPACK #-} (T a)
Can we unpack that (T a) field?

Three cases to consider in `unpackable_cons`

(UC0) No data constructors; a nullary sum type.  This already takes zero
      bits so there is no point in unpacking it.

(UC1) Single-constructor types (products).  We can just represent it by
   its fields. For example, if `T` is defined as:
      data T a = MkT a a Int
   then we can unpack it as follows.  The worker for MkD takes three unpacked fields:
       data D a = MkD a a Int
       $MkD :: T a -> D a
       $MkD (MkT a1 a2 i) = MkD a1 a2 i

   We currently /can't/ do this if T has existentially-bound type variables,
   hence:   null (dataConExTyCoVars con)   in `unpackable_cons`.
   But see also (UC3) below.

   But we /can/ do it for (some) GADTs, such as:
      data Equal a b where { Equal :: Equal a a }
      data Wom a where { Wom1 :: Int -> Wom Bool }
   We will get a MkD constructor that includes some coercion arguments,
   but that is fine.   See #14978.  We still can't accommodate existentials,
   but these particular examples don't use existentials.

(UC2) Multi-constructor types, e.g.
        data T a = T1 a | T2 Int a
  Here we unpack the field to an unboxed sum type, thus:
    data D a = MkD (# a | (# Int, a #) #)

  However, now we can't deal with GADTs at all, because we'd need an
  unboxed sum whose component was a unboxed tuple, whose component(s)
  have kind (CONSTRAINT r); and that's not well-kinded.  Hence the
    all isVanillaDataCon
  condition in `unpackable_cons`. See #25672.

(UC3)  For single-constructor types, with some more plumbing we could
   allow existentials. e.g.
       data T a = forall b. MkT a (b->Int) b
   could unpack to
       data D a = forall b. MkD a (b->Int) b
       $MkD :: T a -> D a
       $MkD (MkT @b x f y) = MkD @b x f y
   Eminently possible, but more plumbing needed.


Note [Unpack one-wide fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The flag UnboxSmallStrictFields ensures that any field that can
(safely) be unboxed to a word-sized unboxed field, should be so unboxed.
For example:

    data A = A Int#
    newtype B = B A
    data C = C !B
    data D = D !C
    data E = E !()
    data F = F !D
    data G = G !F !F

All of these should have an Int# as their representation, except
G which should have two Int#s.

However

    data T = T !(S Int)
    data S = S !a

Here we can represent T with an Int#.

Special care has to be taken to make sure we don't mistake fields with unboxed
tuple/sum rep or very large reps. See #22309

For consistency we unpack anything that fits into 8 bytes on a 64-bit platform,
even when compiling for 32bit platforms. This way unpacking decisions will be the
same for 32bit and 64bit systems. To do so we use primRepSizeW64_B instead of
primRepSizeB. See also the tests in test case T22309.

Note [Recursive unboxing]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data R = MkR {-# UNPACK #-} !S Int
  data S = MkS {-# UNPACK #-} !Int
The representation arguments of MkR are the *representation* arguments
of S (plus Int); the rep args of MkS are Int#.  This is all fine.

But be careful not to try to unbox this!
        data T = MkT {-# UNPACK #-} !T Int
Because then we'd get an infinite number of arguments.

Note that it's the *argument* type that matters. This is fine:
        data S = MkS S !Int
because Int is non-recursive.

Wrinkles:

(W1a) We have to be careful that the compiler doesn't go into a loop!
      First, we must not look at the HsImplBang decisions of data constructors
      in the same mutually recursive group.  E.g.
         data S = MkS {-# UNPACK #-} !T Int
         data T = MkT {-# UNPACK #-} !S Int
      Each of S and T must decide /independently/ whether to unpack
      and they had better not both say yes. So they must both say no.
      (We could detect when we leave the group, and /then/ we can rely on
      HsImplBangs; but that requires more plumbing.)

(W1b) Here is another way the compiler might go into a loop (test T23307b):
         data data T = MkT !S Int
         data S = MkS !T
     Suppose we call `shouldUnpackArgTy` on the !S arg of `T`.  In `should_unpack`
     we ask if the number of fields that `MkS` unpacks to is small enough
     (via rep_tys `lengthAtMost` 1).  But how many field /does/ `MkS` unpack
     to?  Well it depends on the unpacking decision we make for `MkS`, which
     in turn depends on `MkT`, which we are busy deciding. Black holes beckon.

     So we /first/ call `ok_con` on `MkS` (and `ok_con` is conservative;
     see `should_unpack_conservative`), and only /then/ call `should_unpack`.
     Tricky!

(W2) As #23307 shows,  we /do/ want to unpack the second arg of the Yes
     data constructor in this example, despite the recursion in List:
       data Stream a   = Cons a !(Stream a)
       data Unconsed a = Unconsed a !(Stream a)
       data MUnconsed a = No | Yes {-# UNPACK #-} !(Unconsed a)
     When looking at
       {-# UNPACK #-} (Unconsed a)
     we can take Unconsed apart, but then get into a loop with Stream.
     That's fine: we can still take Unconsed apart.  It's only if we
     have a loop /at the root/ that we must not unpack.

(W3) Moreover (W2) can apply even if there is a recursive loop:
       data List a = Nil | Cons {-# UNPACK #-} !(Unconsed a)
       data Unconsed a = Unconsed a !(List a)
     Here there is mutual recursion between `Unconsed` and `List`; and yet
     we can unpack the field of `Cons` because we will not unpack the second
     field of `Unconsed`: we never unpack a sum type without an explicit
     pragma (see should_unpack).

(W4) Consider
        data T = MkT !Wombat
        data Wombat = MkW {-# UNPACK #-} !S Int
        data S = MkS {-# NOUNPACK #-} !Wombat Int
     Suppose we are deciding whether to unpack the first field of MkT, by
     calling (shouldUnpackArgTy Wombat).  Then we'll try to unpack the !S field
     of MkW, and be stopped by the {-# NOUNPACK #-}, and all is fine; we can
     unpack MkT.

     If that NOUNPACK had been a UNPACK, though, we'd get a loop, and would
     decide not to unpack the Wombat field of MkT.

     But what if there was no pragma in `data S`?  Then we /still/ decide not
     to unpack the Wombat field of MkT (at least when auto-unpacking is on),
     because we don't know for sure which decision will be taken for the
     Wombat field of MkS.

     TL;DR when there is no pragma, behave as if there was a UNPACK, at least
     when auto-unpacking is on.  See `should_unpack` in `shouldUnpackArgTy`.


************************************************************************
*                                                                      *
        Wrapping and unwrapping newtypes and type families
*                                                                      *
************************************************************************
-}

wrapNewTypeBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
-- The wrapper for the data constructor for a newtype looks like this:
--      newtype T a = MkT (a,Int)
--      MkT :: forall a. (a,Int) -> T a
--      MkT = /\a. \(x:(a,Int)). x `cast` sym (CoT a)
-- where CoT is the coercion TyCon associated with the newtype
--
-- The call (wrapNewTypeBody T [a] e) returns the
-- body of the wrapper, namely
--      e `cast` (CoT [a])
--
-- If a coercion constructor is provided in the newtype, then we use
-- it, otherwise the wrap/unwrap are both no-ops

wrapNewTypeBody tycon args result_expr
  = assert (isNewTyCon tycon) $
    mkCast result_expr (mkSymCo co)
  where
    co = mkUnbranchedAxInstCo Representational (newTyConCo tycon) args []

-- When unwrapping, we do *not* apply any family coercion, because this will
-- be done via a CoPat by the type checker.  We have to do it this way as
-- computing the right type arguments for the coercion requires more than just
-- a splitting operation (cf, GHC.Tc.Gen.Pat.tcConPat).

unwrapNewTypeBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
unwrapNewTypeBody tycon args result_expr
  = assert (isNewTyCon tycon) $
    mkCast result_expr (mkUnbranchedAxInstCo Representational (newTyConCo tycon) args [])

-- If the type constructor is a representation type of a data instance, wrap
-- the expression into a cast adjusting the expression type, which is an
-- instance of the representation type, to the corresponding instance of the
-- family instance type.
-- See Note [Wrappers for data instance tycons]
wrapFamInstBody :: TyCon -> [Type] -> Maybe (Scaled Type) -> CoreExpr -> CoreExpr
wrapFamInstBody tycon args mb_fun_arg body
  | Just co_con <- tyConFamilyCoercion_maybe tycon
  = mkCast body (mkSymCo $ mkFun (mkUnbranchedAxInstCo Representational co_con args []))
  | otherwise
  = body
  where
    -- When dealing with a newtype instance, cast the partially applied newtype
    -- constructor and not its application, to avoid creating a lambda abstraction
    -- whose binder doesn't have a fixed RuntimeRep.
    --
    -- See Wrinkle [Unlifted newtypes with wrappers]
    -- in Note [Desugaring unlifted newtypes] in GHC.Core.SimpleOpt.
    mkFun =
      case mb_fun_arg of
        Nothing -> id
        Just (Scaled m ty) ->
          let af = case typeTypeOrConstraint ty of
                     TypeLike -> FTF_T_T
                     ConstraintLike -> FTF_C_T
          in mkFunCo Representational af (mkNomReflCo m) (mkRepReflCo ty)

{-
************************************************************************
*                                                                      *
* Foreign calls
*                                                                      *
************************************************************************
-}

-- For each ccall we manufacture a separate CCallOpId, giving it
-- a fresh unique, a type that is correct for this particular ccall,
-- and a CCall structure that gives the correct details about calling
-- convention etc.
--
-- The *name* of this Id is a local name whose OccName gives the full
-- details of the ccall, type and all.  This means that the interface
-- file reader can reconstruct a suitable Id

mkFCallId :: Unique -> ForeignCall -> Type -> Id
mkFCallId uniq fcall ty
  = assert (noFreeVarsOfType ty) $
    -- A CCallOpId should have no free type variables;
    -- when doing substitutions won't substitute over it
    mkGlobalId (FCallId fcall) name ty info
  where
    occ_str = renderWithContext defaultSDocContext (braces (ppr fcall <+> ppr ty))
    -- The "occurrence name" of a ccall is the full info about the
    -- ccall; it is encoded, but may have embedded spaces etc!

    name = mkFCallName uniq (mkFastString occ_str)

    info = noCafIdInfo
           `setArityInfo`  arity
           `setDmdSigInfo` strict_sig
           `setCprSigInfo` topCprSig

    (bndrs, _) = tcSplitPiTys ty
    arity      = count isAnonPiTyBinder bndrs
    strict_sig = mkVanillaDmdSig arity topDiv
    -- the call does not claim to be strict in its arguments, since they
    -- may be lifted (foreign import prim) and the called code doesn't
    -- necessarily force them. See #11076.
{-
************************************************************************
*                                                                      *
\subsection{DictFuns and default methods}
*                                                                      *
************************************************************************

Note [Dict funs and default methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Dict funs and default methods are *not* ImplicitIds.  Their definition
involves user-written code, so we can't figure out their strictness etc
based on fixed info, as we can for constructors and record selectors (say).

NB: See also Note [Exported LocalIds] in GHC.Types.Id
-}

mkDictFunId :: Name      -- Name to use for the dict fun;
            -> [TyVar]
            -> ThetaType
            -> Class
            -> [Type]
            -> Id
-- Implements the DFun Superclass Invariant (see GHC.Tc.TyCl.Instance)
-- See Note [Dict funs and default methods]

mkDictFunId dfun_name tvs theta clas tys
  = mkExportedLocalId (DFunId is_unary)
                      dfun_name
                      dfun_ty
  where
    is_unary = isUnaryClass clas
    dfun_ty  = TcType.tcMkDFunSigmaTy tvs theta (mkClassPred clas tys)
