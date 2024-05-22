{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Tc.Instance.Class (
     matchGlobalInst, matchEqualityInst,
     ClsInstResult(..),
     InstanceWhat(..), safeOverlap, instanceReturnsDictCon,
     AssocInstInfo(..), isNotAssociated,
     lookupHasFieldLabel
  ) where

import GHC.Prelude

import GHC.Driver.DynFlags

import GHC.Core.TyCo.Rep

import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Instantiate(instDFunType, tcInstType)
import GHC.Tc.Instance.Typeable
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.CtLoc
import GHC.Tc.Types.Origin ( InstanceWhat (..), SafeOverlapping, CtOrigin(GetFieldOrigin) )
import GHC.Tc.Instance.Family( tcGetFamInstEnvs, tcInstNewTyCon_maybe, tcLookupDataFamInst, FamInstEnvs )
import GHC.Rename.Env( addUsedGRE, addUsedDataCons, DeprecationWarnings (..) )

import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Builtin.Names
import GHC.Builtin.PrimOps ( PrimOp(..) )
import GHC.Builtin.PrimOps.Ids ( primOpId )

import GHC.Types.FieldLabel
import GHC.Types.Name.Reader
import GHC.Types.SafeHaskell
import GHC.Types.Name   ( Name )
import GHC.Types.Var.Env ( VarEnv )
import GHC.Types.Id
import GHC.Types.Var

import GHC.Core.Predicate
import GHC.Core.Coercion
import GHC.Core.InstEnv
import GHC.Core.Type
import GHC.Core.Make ( mkCharExpr, mkNaturalExpr, mkStringExprFS, mkCoreLams )
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.Class

import GHC.Core ( Expr(..) )

import GHC.StgToCmm.Closure ( isSmallFamily )

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc( splitAtList, fstOf3 )
import GHC.Data.FastString

import GHC.Unit.Module.Warnings

import GHC.Hs.Extension

import Language.Haskell.Syntax.Basic (FieldLabelString(..))
import GHC.Types.Id.Info
import GHC.Tc.Errors.Types

import Data.Functor
import Data.Maybe

{- *******************************************************************
*                                                                    *
              A helper for associated types within
              class instance declarations
*                                                                    *
**********************************************************************-}

-- | Extra information about the parent instance declaration, needed
-- when type-checking associated types. The 'Class' is the enclosing
-- class, the [TyVar] are the /scoped/ type variable of the instance decl.
-- The @VarEnv Type@ maps class variables to their instance types.
data AssocInstInfo
  = NotAssociated
  | InClsInst { ai_class    :: Class
              , ai_tyvars   :: [TyVar]      -- ^ The /scoped/ tyvars of the instance
                                            -- Why scoped?  See bind_me in
                                            -- 'GHC.Tc.Validity.checkConsistentFamInst'
              , ai_inst_env :: VarEnv Type  -- ^ Maps /class/ tyvars to their instance types
                -- See Note [Matching in the consistent-instantiation check]
    }

isNotAssociated :: AssocInstInfo -> Bool
isNotAssociated (NotAssociated {}) = True
isNotAssociated (InClsInst {})     = False


{- *******************************************************************
*                                                                    *
                       Class lookup
*                                                                    *
**********************************************************************-}

data ClsInstResult
  = NoInstance   -- Definitely no instance

  | OneInst { cir_new_theta   :: [TcPredType]
            , cir_mk_ev       :: [EvExpr] -> EvTerm
            , cir_canonical   :: CanonicalEvidence
                  --   cir_canonical=EvCanonical    => you can specialise on this instance
                  --   cir_canonical=EvNonCanonical => you cannot specialise on this instance
                  --                           (its OverlapFlag is NonCanonical)
                  -- See Note [Coherence and specialisation: overview]
            , cir_what        :: InstanceWhat }

  | NotSure      -- Multiple matches and/or one or more unifiers

instance Outputable ClsInstResult where
  ppr NoInstance = text "NoInstance"
  ppr NotSure    = text "NotSure"
  ppr (OneInst { cir_new_theta = ev
               , cir_what = what })
    = text "OneInst" <+> vcat [ppr ev, ppr what]

safeOverlap :: InstanceWhat -> Bool
safeOverlap (TopLevInstance { iw_safe_over = so }) = so
safeOverlap _                                      = True

instanceReturnsDictCon :: InstanceWhat -> Bool
-- See Note [Solved dictionaries] in GHC.Tc.Solver.InertSet
instanceReturnsDictCon (TopLevInstance {}) = True
instanceReturnsDictCon BuiltinInstance     = True
instanceReturnsDictCon BuiltinTypeableInstance {} = True
instanceReturnsDictCon BuiltinEqInstance   = False
instanceReturnsDictCon LocalInstance       = False

matchGlobalInst :: DynFlags
                -> Bool      -- True <=> caller is the short-cut solver
                             -- See Note [Shortcut solving: overlap]
                -> Class -> [Type] -> Maybe CtLoc
                -> TcM ClsInstResult
-- Precondition: Class does not satisfy GHC.Core.Predicate.isEqualityClass
-- (That is handled by a separate code path: see GHC.Tc.Solver.Dict.solveDict,
--  which calls solveEqualityDict for equality classes.)
matchGlobalInst dflags short_cut clas tys mb_loc
  | cls_name == knownNatClassName      = matchKnownNat    dflags short_cut clas tys
  | cls_name == knownSymbolClassName   = matchKnownSymbol dflags short_cut clas tys
  | cls_name == knownCharClassName     = matchKnownChar   dflags short_cut clas tys
  | isCTupleClass clas                 = matchCTuple                       clas tys
  | cls_name == typeableClassName      = matchTypeable                     clas tys
  | cls_name == withDictClassName      = matchWithDict                          tys
  | cls_name == dataToTagClassName     = matchDataToTag                    clas tys
  | cls_name == hasFieldClassName      = matchHasField    dflags short_cut clas tys mb_loc
  | cls_name == unsatisfiableClassName = matchUnsatisfiable
  | otherwise                          = matchInstEnv     dflags short_cut clas tys
  where
    cls_name = className clas

matchUnsatisfiable :: TcM ClsInstResult
-- See (B) in Note [Implementation of Unsatisfiable constraints] in GHC.Tc.Errors
matchUnsatisfiable
  = return NoInstance

{- ********************************************************************
*                                                                     *
                   Looking in the instance environment
*                                                                     *
***********************************************************************-}


matchInstEnv :: DynFlags -> Bool -> Class -> [Type] -> TcM ClsInstResult
matchInstEnv dflags short_cut_solver clas tys
   = do { instEnvs <- tcGetInstEnvs
        ; let safeOverlapCheck = safeHaskell dflags `elem` [Sf_Safe, Sf_Trustworthy]
              (matches, unify, unsafeOverlaps) = lookupInstEnv True instEnvs clas tys
              safeHaskFail = safeOverlapCheck && not (null unsafeOverlaps)
        ; traceTc "matchInstEnv" $
            vcat [ text "goal:" <+> ppr clas <+> ppr tys
                 , text "matches:" <+> ppr matches
                 , text "unify:" <+> ppr unify ]
        ; case (matches, unify, safeHaskFail) of

            -- Nothing matches
            ([], NoUnifiers{}, _)
                -> do { traceTc "matchClass not matching" (ppr pred $$ ppr (ie_local instEnvs))
                      ; return NoInstance }

            -- A single match (& no safe haskell failure)
            ([(ispec, inst_tys)], NoUnifiers canonical, False)
                | short_cut_solver      -- Called from the short-cut solver
                , isOverlappable ispec
                -- If the instance has OVERLAPPABLE or OVERLAPS or INCOHERENT
                -- then don't let the short-cut solver choose it, because a
                -- later instance might overlap it.  #14434 is an example
                -- See Note [Shortcut solving: overlap]
                -> do { traceTc "matchClass: ignoring overlappable" (ppr pred)
                      ; return NotSure }

                | otherwise
                -> do { let dfun_id = instanceDFunId ispec
                            warn    = instanceWarning ispec
                      ; traceTc "matchClass success" $
                        vcat [text "dict" <+> ppr pred <+> ppr canonical,
                              text "witness" <+> ppr dfun_id
                                             <+> ppr (idType dfun_id) ]
                                -- Record that this dfun is needed
                      ; match_one (null unsafeOverlaps) canonical dfun_id inst_tys warn }

            -- More than one matches (or Safe Haskell fail!). Defer any
            -- reactions of a multitude until we learn more about the reagent
            _   -> do { traceTc "matchClass multiple matches, deferring choice" $
                        vcat [text "dict" <+> ppr pred,
                              text "matches" <+> ppr matches]
                      ; return NotSure } }
   where
     pred = mkClassPred clas tys

match_one :: SafeOverlapping -> CanonicalEvidence -> DFunId -> [DFunInstType]
          -> Maybe (WarningTxt GhcRn) -> TcM ClsInstResult
match_one so canonical dfun_id mb_inst_tys warn
  = do { traceTc "match_one" (ppr dfun_id $$ ppr mb_inst_tys)
       ; (tys, theta) <- instDFunType dfun_id mb_inst_tys
       ; traceTc "match_one 2" (ppr dfun_id $$ ppr tys $$ ppr theta)
       ; return $ OneInst { cir_new_theta   = theta
                          , cir_mk_ev       = evDFunApp dfun_id tys
                          , cir_canonical   = canonical
                          , cir_what        = TopLevInstance { iw_dfun_id = dfun_id
                                                             , iw_safe_over = so
                                                             , iw_warn = warn } } }


{- Note [Shortcut solving: overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  instance {-# OVERLAPPABLE #-} C a where ...
and we are typechecking
  f :: C a => a -> a
  f = e  -- Gives rise to [W] C a

We don't want to solve the wanted constraint with the overlappable
instance; rather we want to use the supplied (C a)! That was the whole
point of it being overlappable!  #14434 wwas an example.

Alas even if the instance has no overlap flag, thus
  instance C a where ...
there is nothing to stop it being overlapped. GHC provides no way to
declare an instance as "final" so it can't be overlapped.  But really
only final instances are OK for short-cut solving.  Sigh. #15135
was a puzzling example.
-}


{- ********************************************************************
*                                                                     *
                   Class lookup for CTuples
*                                                                     *
***********************************************************************-}

matchCTuple :: Class -> [Type] -> TcM ClsInstResult
matchCTuple clas tys   -- (isCTupleClass clas) holds
  = return (OneInst { cir_new_theta   = tys
                    , cir_mk_ev       = tuple_ev
                    , cir_canonical   = EvCanonical
                    , cir_what        = BuiltinInstance })
            -- The dfun *is* the data constructor!
  where
     data_con = tyConSingleDataCon (classTyCon clas)
     tuple_ev = evDFunApp (dataConWrapId data_con) tys

{- ********************************************************************
*                                                                     *
                   Class lookup for Literals
*                                                                     *
***********************************************************************-}

{-
Note [KnownNat & KnownSymbol and EvLit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A part of the type-level literals implementation are the classes
"KnownNat" and "KnownSymbol", which provide a "smart" constructor for
defining singleton values.  Here is the key stuff from GHC.TypeNats

  class KnownNat (n :: Nat) where
    natSing :: SNat n

  newtype SNat (n :: Nat) = SNat Natural

Conceptually, this class has infinitely many instances:

  instance KnownNat 0       where natSing = SNat 0
  instance KnownNat 1       where natSing = SNat 1
  instance KnownNat 2       where natSing = SNat 2
  ...

In practice, we solve `KnownNat` predicates in the type-checker (see
`matchKnownNat` in this module) because we can't have infinitely many
instances.  The evidence (aka "dictionary") for `KnownNat` is of the
form `EvLit (EvNum n)`.

We make the following assumptions about dictionaries in GHC:
  1. The "dictionary" for classes with a single method---like `KnownNat`---is
     a newtype for the type of the method, so using a evidence amounts
     to a coercion, and
  2. Newtypes use the same representation as their definition types.

So, the evidence for `KnownNat` is just a value of the representation type,
wrapped in two newtype constructors: one to make it into a `SNat` value,
and another to make it into a `KnownNat` dictionary.

Also note that `natSing` and `SNat` are never actually exposed from the
library---they are just an implementation detail.  Instead, users see
a more convenient function, defined in terms of `natSing`:

  natVal :: KnownNat n => proxy n -> Natural

The reason we don't use this directly in the class is that it is simpler
and more efficient to pass around a Natural rather than an entire function,
especially when the `KnownNat` evidence is packaged up in an existential.

The story for kind `Symbol` is analogous:
  * class KnownSymbol
  * newtype SSymbol
  * Evidence: a Core literal (e.g. mkNaturalExpr)


Note [Fabricating Evidence for Literals in Backpack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let `T` be a type of kind `Nat`. When solving for a purported instance
of `KnownNat T`, ghc tries to resolve the type `T` to an integer `n`,
in which case the evidence `EvLit (EvNum n)` is generated on the
fly. It might appear that this is sufficient as users cannot define
their own instances of `KnownNat`. However, for backpack module this
would not work (see issue #15379). Consider the signature `Abstract`

> signature Abstract where
>   data T :: Nat
>   instance KnownNat T

and a module `Util` that depends on it:

> module Util where
>  import Abstract
>  printT :: IO ()
>  printT = do print $ natVal (Proxy :: Proxy T)

Clearly, we need to "use" the dictionary associated with `KnownNat T`
in the module `Util`, but it is too early for the compiler to produce
a real dictionary as we still have not fixed what `T` is. Only when we
mixin a concrete module

> module Concrete where
>   type T = 42

do we really get hold of the underlying integer. So the strategy that
we follow is the following

1. If T is indeed available as a type alias for an integer constant,
   generate the dictionary on the fly, failing which

2. Look up the type class environment for the evidence.

Finally actual code gets generate for Util only when a module like
Concrete gets "mixed-in" in place of the signature Abstract. As a
result all things, including the typeclass instances, in Concrete gets
reexported. So `KnownNat` gets resolved the normal way post-Backpack.

A similar generation works for `KnownSymbol` as well

-}

matchKnownNat :: DynFlags
              -> Bool      -- True <=> caller is the short-cut solver
                           -- See Note [Shortcut solving: overlap]
              -> Class -> [Type] -> TcM ClsInstResult
matchKnownNat dflags _ clas [ty]     -- clas = KnownNat
  | Just n <- isNumLitTy ty  = makeLitDict clas ty (mkNaturalExpr (targetPlatform dflags) n)
matchKnownNat df sc clas tys = matchInstEnv df sc clas tys
 -- See Note [Fabricating Evidence for Literals in Backpack] for why
 -- this lookup into the instance environment is required.

matchKnownSymbol :: DynFlags
                 -> Bool      -- True <=> caller is the short-cut solver
                              -- See Note [Shortcut solving: overlap]
                 -> Class -> [Type] -> TcM ClsInstResult
matchKnownSymbol _ _ clas [ty]  -- clas = KnownSymbol
  | Just s <- isStrLitTy ty = do
        et <- mkStringExprFS s
        makeLitDict clas ty et
matchKnownSymbol df sc clas tys = matchInstEnv df sc clas tys
 -- See Note [Fabricating Evidence for Literals in Backpack] for why
 -- this lookup into the instance environment is required.

matchKnownChar :: DynFlags
                 -> Bool      -- True <=> caller is the short-cut solver
                              -- See Note [Shortcut solving: overlap]
                 -> Class -> [Type] -> TcM ClsInstResult
matchKnownChar _ _ clas [ty]  -- clas = KnownChar
  | Just s <- isCharLitTy ty = makeLitDict clas ty (mkCharExpr s)
matchKnownChar df sc clas tys = matchInstEnv df sc clas tys
 -- See Note [Fabricating Evidence for Literals in Backpack] for why
 -- this lookup into the instance environment is required.

makeLitDict :: Class -> Type -> EvExpr -> TcM ClsInstResult
-- makeLitDict adds a coercion that will convert the literal into a dictionary
-- of the appropriate type.  See Note [KnownNat & KnownSymbol and EvLit].
-- The coercion happens in 2 steps:
--
--     Integer -> SNat n     -- representation of literal to singleton
--     SNat n  -> KnownNat n -- singleton to dictionary
--
--     The process is mirrored for Symbols:
--     String    -> SSymbol n
--     SSymbol n -> KnownSymbol n
makeLitDict clas ty et
    | Just (_, co_dict) <- tcInstNewTyCon_maybe (classTyCon clas) [ty]
          -- co_dict :: KnownNat n ~ SNat n
    , [ meth ]   <- classMethods clas
    , Just tcRep <- tyConAppTyCon_maybe (classMethodTy meth)
                    -- If the method type is forall n. KnownNat n => SNat n
                    -- then tcRep is SNat
    , Just (_, co_rep) <- tcInstNewTyCon_maybe tcRep [ty]
          -- SNat n ~ Integer
    , let ev_tm = mkEvCast et (mkSymCo (mkTransCo co_dict co_rep))
    = return $ OneInst { cir_new_theta   = []
                       , cir_mk_ev       = \_ -> ev_tm
                       , cir_canonical   = EvCanonical
                       , cir_what        = BuiltinInstance }

    | otherwise
    = pprPanic "makeLitDict" $
      text "Unexpected evidence for" <+> ppr (className clas)
      $$ vcat (map (ppr . idType) (classMethods clas))

{- ********************************************************************
*                                                                     *
                   Class lookup for WithDict
*                                                                     *
***********************************************************************-}

-- See Note [withDict]
matchWithDict :: [Type] -> TcM ClsInstResult
matchWithDict [cls, mty]
    -- Check that cls is a class constraint `C t_1 ... t_n`, where
    -- `dict_tc = C` and `dict_args = t_1 ... t_n`.
  | Just (dict_tc, dict_args) <- tcSplitTyConApp_maybe cls
    -- Check that C is a class of the form
    -- `class C a_1 ... a_n where op :: meth_ty`
    -- and in that case let
    -- co :: C t1 ..tn ~R# inst_meth_ty
  , Just (inst_meth_ty, co) <- tcInstNewTyCon_maybe dict_tc dict_args
  = do { sv <- mkSysLocalM (fsLit "withDict_s") ManyTy mty
       ; k  <- mkSysLocalM (fsLit "withDict_k") ManyTy (mkInvisFunTy cls openAlphaTy)

       -- Given co2 : mty ~N# inst_meth_ty, construct the method of
       -- the WithDict dictionary:
       --
       --   \@(r :: RuntimeRep) @(a :: TYPE r) (sv :: mty) (k :: cls => a) ->
       --     k (sv |> (sub co ; sym co2))
       ; let evWithDict co2 =
               mkCoreLams [ runtimeRep1TyVar, openAlphaTyVar, sv, k ] $
                 Var k
                   `App`
                 (Var sv `Cast` mkTransCo (mkSubCo co2) (mkSymCo co))

       ; tc <- tcLookupTyCon withDictClassName
       ; let Just withdict_data_con
                 = tyConSingleDataCon_maybe tc    -- "Data constructor"
                                                  -- for WithDict
             mk_ev [c] = evDataConApp withdict_data_con
                            [cls, mty] [evWithDict (evTermCoercion (EvExpr c))]
             mk_ev e   = pprPanic "matchWithDict" (ppr e)

       ; return $ OneInst { cir_new_theta   = [mkPrimEqPred mty inst_meth_ty]
                          , cir_mk_ev       = mk_ev
                          , cir_canonical   = EvNonCanonical -- See (WD6) in Note [withDict]
                          , cir_what        = BuiltinInstance }
       }

matchWithDict _
  = return NoInstance

{-
Note [withDict]
~~~~~~~~~~~~~~~
The class `WithDict` is defined as:

    class WithDict cls meth where
        withDict :: forall {rr :: RuntimeRep} (r :: TYPE rr). meth -> (cls => r) -> r

This class is special, like `Typeable`: GHC automatically solves
for instances of `WithDict`, users cannot write their own.

It is used to implement a primitive that we cannot define in Haskell
but we can write in Core.

`WithDict` is used to create dictionaries for classes with a single method.
Consider a class like this:

   class C a where
     f :: T a

We can use `withDict` to cast values of type `T a` into dictionaries for `C a`.
To do this, we can define a function like this in the library:

  withT :: T a -> (C a => b) -> b
  withT t k = withDict @(C a) @(T a) t k

Here:

* The `cls` in `withDict` is instantiated to `C a`.

* The `meth` in `withDict` is instantiated to `T a`.
  The definition of `T` itself is irrelevant, only that `C a` is a class
  with a single method of type `T a`.

* The `r` in `withDict` is instantiated to `b`.

For any single-method class C:
   class C a1 .. an where op :: meth_ty

The solver will solve the constraint `WithDict (C t1 .. tn) mty`
as if the following instance declaration existed:

instance (mty ~# inst_meth_ty) => WithDict (C t1..tn) mty where
  withDict = \@{rr} @(r :: TYPE rr) (sv :: mty) (k :: C t1..tn => r) ->
    k (sv |> (sub co2 ; sym co))

That is, it matches on the first (constraint) argument of C; if C is
a single-method class, the instance "fires" and emits an equality
constraint `mty ~ inst_meth_ty`, where `inst_meth_ty` is `meth_ty[ti/ai]`.
The coercion `co2` witnesses the equality `mty ~ inst_meth_ty`.

The coercion `co` is a newtype coercion that coerces from `C t1 ... tn`
to `inst_meth_ty`.
This coercion is guaranteed to exist by virtue of the fact that
C is a class with exactly one method and no superclasses, so it
is treated like a newtype when compiled to Core.

The condition that `C` is a single-method class is implemented in the
guards of matchWithDict's definition.
If the conditions are not held, the rewriting will not fire,
and we'll report an unsolved constraint.

Some further observations about `withDict`:

(WD1) The `cls` in the type of withDict must be explicitly instantiated with
      visible type application, as invoking `withDict` would be ambiguous
      otherwise.

      For examples of how `withDict` is used in the `base` library, see `withSNat`
      in GHC.TypeNats, as well as `withSChar` and `withSSymbol` in GHC.TypeLits.

(WD2) The `r` is representation-polymorphic, to support things like
      `withTypeable` in `Data.Typeable.Internal`.

(WD3) As an alternative to `withDict`, one could define functions like `withT`
      above in terms of `unsafeCoerce`. This is more error-prone, however.

(WD4) In order to define things like `withKnownNat` below:

        withKnownNat :: SNat n -> (KnownNat n => r) -> r

      `withDict` needs to be instantiated with `Any`, like so:

        withKnownNat = withDict @(KnownNat Any) @(SNat Any) @r

      The use of `Any` is explained in Note [NOINLINE withSomeSNat] in
      base:GHC.TypeNats.

(WD5) In earlier implementations, `withDict` was implemented as an identifier
      with special handling during either constant-folding or desugaring.
      The current approach is more robust: previously, the type of `withDict`
      did not have a type-class constraint and was overly polymorphic.
      See #19915.

(WD6) In fact, we desugar `withDict @cls @mty @{rr} @r` to

         \@(r :: RuntimeRep) @(a :: TYPE r) (sv :: mty) (k :: cls => a) ->
           k (sv |> (sub co2 ; sym co)))

      That is, we cast the method using a coercion, and apply k to
      it. Moreover, we mark the evidence as non-canonical, resulting in
      the use of the 'nospec' magicId (see Note [nospecId magic] in
      GHC.Types.Id.Make) to ensure that the typeclass specialiser
      doesn't incorrectly common-up distinct evidence terms. This is
      super important! Suppose we have calls

          withDict A k
          withDict B k

      where k1, k2 :: C T -> blah.  If we desugared withDict naively, we'd get

          k (A |> co1)
          k (B |> co2)

      and the Specialiser would assume that those arguments (of type `C T`) are
      the same. It would then specialise `k` for that type, and then call the same,
      specialised function from both call sites.  #21575 is a concrete case in point.

      To avoid this, we need to stop the typeclass specialiser from seeing this
      structure, by using nospec. This function is inlined only in CorePrep; crucially
      this means that it still appears in interface files, so that the desugaring of
      withDict remains opaque to the typeclass specialiser across modules.
      This means the specialiser will always see instead:

          nospec @(cls => a) k (A |> co1)
          nospec @(cls => a) k (B |> co2)

      Why does this work? Recall that nospec is not an overloaded function;
      it has the type

        nospec :: forall a. a -> a

      This means that there is nothing for the specialiser to do with function calls
      such as

        nospec @(cls => a) k (A |> co)

      as the specialiser only looks at calls of the form `f dict` for an
      overloaded function `f` (e.g. with a type such as `f :: Eq a => ...`).

      See test-case T21575b.



Note [DataToTag overview]
~~~~~~~~~~~~~~~~~~~~~~~~~
Class `DataToTag` is defined like this, in GHC.Magic:

  type DataToTag :: forall {lev :: Levity}.
                    TYPE (BoxedRep lev) -> Constraint
  class DataToTag a where
     dataToTag# :: a -> Int#

`dataToTag#`, evaluates its argument and returns the index of the data
constructor used to build that argument.  Clearly, `dataToTag#` cannot
work on /any/ type, only on data types, hence the type-class constraint.

Users cannot define instances of `DataToTag`
(see `GHC.Tc.Validity.check_special_inst_head`).
Instead, GHC's constraint solver has built-in solving behaviour,
 implemented in `GHC.Tc.Instance.Class.matchGlobalInst`.

(#20441: This common handling of special typeclasses is a bit of a
mess and could use some love, and a dedicated Note.)

GHC solves a wanted constraint `DataToTag @{lev} dty`
when all of the following conditions are met:

C1: `dty` is an algebraic data type, i.e. `dty` matches any of:
       * a "data" declaration,
       * a "data instance" declaration,
       * a boxed tuple type
      "type data" declarations are NOT included; see also wrinkle W2c
      of Note [Type data declarations] in GHC.Rename.Module.
      (In principle we could accept newtypes that wrap algebraic data
      types, but we do not do so.)

C2: All of the constructors of that "data" or "data instance"
      declaration are in scope.  Otherwise, `dataToTag#` could be
      used to peek behind the curtain when used with an abstract
      data type whose constructors are intentionally hidden.

C3: `lev` is statically known, either Lifted or Unlifted:
      Otherwise the argument to `dataToTag#` would be
      representation-polymorphic and we couldn't do anything
      with it without Core Lint rightfully complaining.
      This guarantees invariant (DTT1) below.

It would be possible for GHC to generate custom code for each type, like
this:

   instance DataToTag [a] where
     dataToTag# []    = 0#
     dataToTag# (_:_) = 1#

But, to avoid all this boilerplate code, and improve optimisation opportunities,
GHC generates instances like this:

   instance DataToTag [a] where
     dataToTag# = dataToTagSmall#

using one of two dedicated primops: `dataToTagSmall#` and `dataToTagLarge#`.
(Why two primops? What's the difference? See wrinkles DTW4 and DTW5.)
Both primops have the following over-polymorphic type:

  dataToTagLarge# :: forall {l::levity} (a::TYPE (BoxedRep l)). a -> Int#

Every call to either primop that we generate should look like
(dataToTagSmall# @{lev} @ty) with two type arguments that satisfy
these conditions:

(DTT1) `lev` is concrete (either lifted or unlifted), not polymorphic.
   This is an invariant--we must satisfy this or Core Lint will complain.
   (This falls under situation 1 in GHC.Core.Lint's
   Note [Linting representation-polymorphic builtins].)

(DTT2) `ty` is always headed by a TyCon corresponding to one of the following:
   * A boxed tuple
   * A "data" declaration (but NOT a "type data" declaration)
   * The /representation type/ for a "data instance" declaration
     (but NOT the data family TyCon itself)

   This ensures that the DataCons associated with `ty` are easily
   accessible and safe to use in Core without running afoul of
   invariant I1 from Note [Type data declarations] in
   GHC.Rename.Module.  See Note [caseRules for dataToTag] in
   GHC.Core.Opt.ConstantFold for why this matters.

   While wrinkle DTW7 is unresolved, this cannot be a true invariant.
   But with a little effort we can ensure that every primop
   call we generate in a DataToTag instance satisfies this condition.

(DTT3) If the TyCon in wrinkle DTT2 is a "large data type" with more
   constructors than fit in pointer tags on the target, then the
   primop must be dataToTagLarge# and not dataToTagSmall#.
   Otherwise, the primop must be dataToTagSmall# and not dataToTagLarge#.
   (See wrinkles DTW4 and DTW5.)

These two primops have special handling in several parts of
the compiler:

H1. They have a couple of built-in rewrite rules, implemented in
    GHC.Core.Opt.ConstantFold.dataToTagRule

H2. The simplifier rewrites most case expressions scrutinizing their results.
    See Note [caseRules for dataToTag] in GHC.Core.Opt.ConstantFold.

H3. Each evaluates its argument.  But we want to omit this eval when the
    actual argument is already evaluated and properly tagged.  To do this,

    * We have a special case in GHC.Stg.InferTags.Rewrite.rewriteOpApp
      ensuring that any inferred tag information on the argument is
      retained until code generation.

    * We generate code via special cases in GHC.StgToCmm.Expr.cgExpr
      instead of with the other primops in GHC.StgToCmm.Prim.emitPrimOp;
      tag info is not readily available in the latter function.
      (Wrinkle DTW4 describes what we generate after the eval.)

Wrinkles:

(DTW1) To guarantee (DTT2) we need to take care with data families.
  Consider  data family D a
            data instance D (Either p q) = D1 | D2 p q
  To solve the constraint
     [W] DataToTag (D (Either t1 t2))
  GHC uses the built-in instance
     instance DataToTag (D (Either p q)) where
        dataToTag# x = dataToTagSmall# @Lifted @(R:DEither p q)
                                       (x |> sym (ax:DEither p q))
  where `ax:DEither` is the axiom arising from the `data instance`:
    ax:DEither p q :: D (Either p q) ~ R:DEither p q

  Notice that we cast `x` before giving it to `dataToTagSmall#`, so
  that (DTT2) is satisfied.

(DTW2) Suppose we have module A (T(..)) where { data T = TCon }
  and in module B, the constraint `DataToTag T` is needed. Per
  condition C2, we only solve this constraint if `TCon` is in
  scope.  So we had better not later report a warning about the
  import of `TCon` being unused in module B!

  To avoid this simply call `addUsedDataCons` when creating a built-in
  DataToTag instance.

(DTW3) Similar to DTW2, consider this example:

    {-# LANGUAGE MagicHash #-}
    module A (X(X2, X3), g) where
    -- see also testsuite/tests/warnings/should_compile/DataToTagWarnings.hs
    import GHC.Exts (dataToTag#, Int#)
    data X = X1 | X2 | X3 | X4
    g :: X -> Int#
    g X2 = 12#
    g v = dataToTag# v

  QUESTION: What warnings should be emitted with -Wunused-top-binds?

  The X1 and X4 constructors are used only in the solving of a
  `DataToTag X` constraint in the second equation for `g`.  But if
  these constructors were just removed, they would not be needed for
  the solving of that `DataToTag X` constraint!  So for now we take
  the stance that both X1 and X4 should be reported as unused.

  It's not entirely clear if this is the right behavior:
  Notice that removing X1 changes the value of `g X3` from 2# to 1#.
  (Removing X4 causes no observable change in behavior.)
  But this is a very obscure program!  The current "warn about both"
  approach is not obviously wrong, either, and is consistent with the
  behavior of derived Ix instances.

  To get these warnings, we do nothing; in particular we do not call
  keepAlive on the constructor names.
  (Contrast with Note [Unused name reporting and HasField].)

(DTW4) Why have two primops, `dataToTagSmall#` and `dataToTagLarge#`?
  The way tag information is stored at runtime is described in
  Note [Tagging big families] in GHC.StgToCmm.Expr.  In particular,
  for "big data types" we must consult the heap object's info table at
  least in the mAX_PTR_TAG case, while for "small data types" we can
  always just examine the tag bits on the pointer itself. So:

  * dataToTagSmall# consults the tag bits in the pointer, ignoring the
    info table.  It should, therefore, be used only for data type with
    few enough constructors that the tag always fits in the pointer.

  * dataToTagLarge# also consults the tag bits in the pointer, but
    must fall back to examining the info table whenever those tag
    bits are equal to mAX_PTR_TAG.

  One could imagine having one primop with a small/large tag, or just
  the data type width, but the PrimOp data type is not currently set
  up for that.  Looking at the type information on the argument during
  code generation is also possible, but would be less reliable.
  Remember: type information is not always preserved in STG.

(DTW5) How do the two primops differ in their semantics?  We consider
  a call `dataToTagSmall# x` to result in undefined behavior whenever
  the target supports pointer tagging but the actual constructor index
  for `x` is too large to fit in the pointer's tag bits.  Otherwise,
  `dataToTagSmall#` behaves identically to `dataToTagLarge#`.

  This allows the rewrites performed in GHC.Core.Opt.ConstantFold to
  safely treat `dataToTagSmall#` identically to `dataToTagLarge#`:
  the allowed program behaviors for the former is always a superset of
  the allowed program behaviors for the latter.

  This undefined behavior is only observable if a user writes a
  wrongly-sized primop call.  The calls we generate are properly-sized
  (condition DTT3 above) so that the type system protects us.

(DTW6) We make no promises about the primops used to implement
  DataToTag instances.  Changes to GHC's representation of algebraic
  data types at runtime may force us to redesign these primops.
  Indeed, accommodating such changes without breaking users of the
  original (no longer existing) "dataToTag#" primop is one of the
  main reasons the DataToTag class exists!

  In particular, our current two primop implementations (as described
  in wrinkle DTW4) are adequate for every DataToTag instance only
  because every Haskell-land data constructor use gets translated to
  its own "real" heap or static data object at runtime and the index
  of that constructor is always exposed via pointer tagging and via
  the object's info table.

(DTW7) Currently, the generated module GHC.PrimopWrappers in ghc-prim
  contains the following non-sense definitions:

    {-# NOINLINE dataToTagSmall# #-}
    dataToTagSmall# :: a_levpoly -> Int#
    dataToTagSmall# a1 = GHC.Prim.dataToTagSmall# a1
    {-# NOINLINE dataToTagLarge# #-}
    dataToTagLarge# :: a_levpoly -> Int#
    dataToTagLarge# a1 = GHC.Prim.dataToTagLarge# a1

  Why do these exist? GHCi uses these symbols for... something.  There
  is on-going work to get rid of them.  See also #24169, #20155, and !6245.
  Their continued existence makes it difficult to do several nice things:

   * As explained in DTW6, the dataToTag# primops are very internal.
     We would like to hide them from GHC.Prim entirely to prevent
     their mis-use, but doing so would cause GHC.PrimopWrappers to
     fail to compile.

   * The primops are applied at the (confusingly monomorphic) type
     variable `a_levpoly` in the above definitions.  In particular,
     they do not satisfy conditions DTT2 and DTT3 above.  We would
     very much like these conditions to be invariants, but while
     GHC.PrimopWrappers breaks them we cannot do so.  (The code that
     would check these invariants in Core Lint exists but remains
     commented out for now.)

   * This in turn means that `GHC.Core.Opt.ConstantFold.caseRules`
     must check for condition DTT2 before doing the work described in
     Note [caseRules for dataToTag].

   * Likewise, wrinkle DTW5 is only necessary because condition DTT3
     is not an invariant.  Otherwise, invoking the currently-specified
     undefined behavior of `dataToTagSmall# @ty` would require passing it
     an argument which will not really have type `ty` at runtime.  And
     evaluating such an expression is always undefined behavior anyway!



Historical note:
During its time as a primop, `dataToTag#` underwent several changes,
mostly relating to under what circumstances it evaluates its argument.
Today, that story is simple: A dataToTag primop always evaluates its
argument, unless tag inference determines the argument was already
evaluated and correctly tagged.  Getting here was a long journey, with
many similarities to the story behind Note [Strict Field Invariant] in
GHC.Stg.InferTags.  See also #15696.

-}


{- ********************************************************************
*                                                                     *
                   Class lookup for DataToTag
*                                                                     *
***********************************************************************-}

matchDataToTag :: Class -> [Type] -> TcM ClsInstResult
-- See Note [DataToTag overview]
matchDataToTag dataToTagClass [levity, dty] = do
  famEnvs <- tcGetFamInstEnvs
  (gbl_env, _lcl_env) <- getEnvs
  platform <- getPlatform
  if | isConcreteType levity -- condition C3
     , Just (rawTyCon, rawTyConArgs) <- tcSplitTyConApp_maybe dty
     , let (repTyCon, repArgs, repCo)
             = tcLookupDataFamInst famEnvs rawTyCon rawTyConArgs

     , not (isTypeDataTyCon repTyCon)
     , Just constrs <- tyConAlgDataCons_maybe repTyCon
         -- condition C1

     , let  rdr_env = tcg_rdr_env gbl_env
            inScope con = isJust $ lookupGRE_Name rdr_env $ dataConName con
     , all inScope constrs -- condition C2

     , let  repTy = mkTyConApp repTyCon repArgs
            numConstrs = tyConFamilySize repTyCon
            !whichOp -- see wrinkle DTW4
              | isSmallFamily platform numConstrs
                = primOpId DataToTagSmallOp
              | otherwise
                = primOpId DataToTagLargeOp

            -- See wrinkle DTW1; we must apply the underlying
            -- operation at the representation type and cast it
            methodRep = Var whichOp `App` Type levity `App` Type repTy
            methodCo = mkFunCo Representational
                               FTF_T_T
                               (mkNomReflCo ManyTy)
                               (mkSymCo repCo)
                               (mkReflCo Representational intPrimTy)
            dataToTagDataCon = tyConSingleDataCon (classTyCon dataToTagClass)
            mk_ev _ = evDataConApp dataToTagDataCon
                                   [levity, dty]
                                   [methodRep `Cast` methodCo]
     -> addUsedDataCons rdr_env repTyCon -- See wrinkles DTW2 and DTW3
          $> OneInst { cir_new_theta = [] -- (Ignore stupid theta.)
                     , cir_mk_ev = mk_ev
                     , cir_canonical = EvCanonical
                     , cir_what = BuiltinInstance
                     }
     | otherwise -> pure NoInstance

matchDataToTag _ _ = pure NoInstance



{- ********************************************************************
*                                                                     *
                   Class lookup for Typeable
*                                                                     *
***********************************************************************-}

-- | Assumes that we've checked that this is the 'Typeable' class,
-- and it was applied to the correct argument.
matchTypeable :: Class -> [Type] -> TcM ClsInstResult
matchTypeable clas [k,t]  -- clas = Typeable
  -- Forall types: see Note [No Typeable for polytypes or qualified types]
  | isForAllTy k = return NoInstance

  -- Functions; but only with a visible argment
  | Just (af,mult,arg,ret) <- splitFunTy_maybe t
  = if isVisibleFunArg af
    then doFunTy clas t mult arg ret
    else return NoInstance
      -- 'else' case: qualified types like (Num a => blah) are not typeable
      -- see Note [No Typeable for polytypes or qualified types]

  -- Now cases that do work
  | k `eqType` naturalTy      = doTyLit knownNatClassName         t
  | k `eqType` typeSymbolKind = doTyLit knownSymbolClassName      t
  | k `eqType` charTy         = doTyLit knownCharClassName        t

  -- TyCon applied to its kind args
  -- No special treatment of Type and Constraint; they get distinct TypeReps
  -- see wrinkle (W4) of Note [Type and Constraint are not apart]
  --     in GHC.Builtin.Types.Prim.
  | Just (tc, ks) <- splitTyConApp_maybe t -- See Note [Typeable (T a b c)]
  , onlyNamedBndrsApplied tc ks            = doTyConApp clas t tc ks

  | Just (f,kt)   <- splitAppTy_maybe t    = doTyApp    clas t f kt

matchTypeable _ _ = return NoInstance

-- | Representation for a type @ty@ of the form @arg -> ret@.
doFunTy :: Class -> Type -> Mult -> Type -> Type -> TcM ClsInstResult
doFunTy clas ty mult arg_ty ret_ty
  = return $ OneInst { cir_new_theta   = preds
                     , cir_mk_ev       = mk_ev
                     , cir_canonical   = EvCanonical
                     , cir_what        = BuiltinInstance }
  where
    preds = map (mk_typeable_pred clas) [mult, arg_ty, ret_ty]
    mk_ev [mult_ev, arg_ev, ret_ev] = evTypeable ty $
                        EvTypeableTrFun (EvExpr mult_ev) (EvExpr arg_ev) (EvExpr ret_ev)
    mk_ev _ = panic "GHC.Tc.Instance.Class.doFunTy"


-- | Representation for type constructor applied to some kinds.
-- 'onlyNamedBndrsApplied' has ensured that this application results in a type
-- of monomorphic kind (e.g. all kind variables have been instantiated).
doTyConApp :: Class -> Type -> TyCon -> [Kind] -> TcM ClsInstResult
doTyConApp clas ty tc kind_args
  | tyConIsTypeable tc
  = return $ OneInst { cir_new_theta   = map (mk_typeable_pred clas) kind_args
                     , cir_mk_ev       = mk_ev
                     , cir_canonical   = EvCanonical
                     , cir_what        = BuiltinTypeableInstance tc }
  | otherwise
  = return NoInstance
  where
    mk_ev kinds = evTypeable ty $ EvTypeableTyCon tc (map EvExpr kinds)

-- | Representation for TyCon applications of a concrete kind. We just use the
-- kind itself, but first we must make sure that we've instantiated all kind-
-- polymorphism, but no more.
onlyNamedBndrsApplied :: TyCon -> [KindOrType] -> Bool
onlyNamedBndrsApplied tc ks
 = all isNamedTyConBinder used_bndrs &&
   not (any isNamedTyConBinder leftover_bndrs)
 where
   bndrs                        = tyConBinders tc
   (used_bndrs, leftover_bndrs) = splitAtList ks bndrs

doTyApp :: Class -> Type -> Type -> KindOrType -> TcM ClsInstResult
-- Representation for an application of a type to a type-or-kind.
--  This may happen when the type expression starts with a type variable.
--  Example (ignoring kind parameter):
--    Typeable (f Int Char)                      -->
--    (Typeable (f Int), Typeable Char)          -->
--    (Typeable f, Typeable Int, Typeable Char)  --> (after some simp. steps)
--    Typeable f
doTyApp clas ty f tk
  | isForAllTy (typeKind f)
  = return NoInstance -- We can't solve until we know the ctr.
  | otherwise
  = return $ OneInst { cir_new_theta   = map (mk_typeable_pred clas) [f, tk]
                     , cir_mk_ev       = mk_ev
                     , cir_canonical   = EvCanonical
                     , cir_what        = BuiltinInstance }
  where
    mk_ev [t1,t2] = evTypeable ty $ EvTypeableTyApp (EvExpr t1) (EvExpr t2)
    mk_ev _ = panic "doTyApp"


-- Emit a `Typeable` constraint for the given type.
mk_typeable_pred :: Class -> Type -> PredType
mk_typeable_pred clas ty = mkClassPred clas [ typeKind ty, ty ]

  -- Typeable is implied by KnownNat/KnownSymbol. In the case of a type literal
  -- we generate a sub-goal for the appropriate class.
  -- See Note [Typeable for Nat and Symbol]
doTyLit :: Name -> Type -> TcM ClsInstResult
doTyLit kc t = do { kc_clas <- tcLookupClass kc
                  ; let kc_pred    = mkClassPred kc_clas [ t ]
                        mk_ev [ev] = evTypeable t $ EvTypeableTyLit (EvExpr ev)
                        mk_ev _    = panic "doTyLit"
                  ; return (OneInst { cir_new_theta   = [kc_pred]
                                    , cir_mk_ev       = mk_ev
                                    , cir_canonical   = EvCanonical
                                    , cir_what        = BuiltinInstance }) }

{- Note [Typeable (T a b c)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For type applications we always decompose using binary application,
via doTyApp (building a TrApp), until we get to a *kind* instantiation
(building a TrTyCon).  We detect a pure kind instantiation using
`onlyNamedBndrsApplied`.

Example: Proxy :: forall k. k -> *

  To solve Typeable (Proxy @(* -> *) Maybe) we

  - First decompose with doTyApp (onlyNamedBndrsApplied is False)
    to get (Typeable (Proxy @(* -> *))) and Typeable Maybe.
    This step returns a TrApp.

  - Then solve (Typeable (Proxy @(* -> *))) with doTyConApp
    (onlyNamedBndrsApplied is True).
    This step returns a TrTyCon

  So the TypeRep we build is
    TrApp (TrTyCon ("Proxy" @(*->*))) (TrTyCon "Maybe")

Notice also that TYPE and CONSTRAINT are distinct so, in effect, we
allow (Typeable TYPE) and (Typeable CONSTRAINT), giving disinct TypeReps.
This is very important: we may want to get a TypeRep for a kind like
   Type -> Constraint

If we attempt to short-cut by solving it all at once, via
doTyConApp

(this note is sadly truncated FIXME)


Note [No Typeable for polytypes or qualified types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not support impredicative typeable, such as
   Typeable (forall a. a->a)
   Typeable (Eq a => a -> a)
   Typeable (() => Int)
   Typeable (((),()) => Int)

See #9858.  For forall's the case is clear: we simply don't have
a TypeRep for them.  For qualified but not polymorphic types, like
(Eq a => a -> a), things are murkier.  But:

 * We don't need a TypeRep for these things.  TypeReps are for
   monotypes only.

 * Perhaps we could treat `=>` as another type constructor for `Typeable`
   purposes, and thus support things like `Eq Int => Int`, however,
   at the current state of affairs this would be an odd exception as
   no other class works with impredicative types.
   For now we leave it off, until we have a better story for impredicativity.


Note [Typeable for Nat and Symbol]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have special Typeable instances for Nat and Symbol.  Roughly we
have this instance, implemented here by doTyLit:
      instance KnownNat n => Typeable (n :: Nat) where
         typeRep = typeNatTypeRep @n
where
   Data.Typeable.Internal.typeNatTypeRep :: KnownNat a => TypeRep a

Ultimately typeNatTypeRep uses 'natSing' from KnownNat to get a
runtime value 'n'; it turns it into a string with 'show' and uses
that to whiz up a TypeRep TyCon for 'n', with mkTypeLitTyCon.
See #10348.

Because of this rule it's inadvisable (see #15322) to have a constraint
    f :: (Typeable (n :: Nat)) => blah
in a function signature; it gives rise to overlap problems just as
if you'd written
    f :: Eq [a] => blah
-}

{- ********************************************************************
*                                                                     *
                   Class lookup for lifted equality
*                                                                     *
***********************************************************************-}

-- See also Note [The equality types story] in GHC.Builtin.Types.Prim
matchEqualityInst :: Class -> [Type] -> (DataCon, Role, Type, Type)
-- Precondition: `cls` satisfies GHC.Core.Predicate.isEqualityClass
-- See Note [Solving equality classes] in GHC.Tc.Solver.Dict
matchEqualityInst cls args
  | cls `hasKey` eqTyConKey  -- Solves (t1 ~ t2)
  , [_,t1,t2] <- args
  = (eqDataCon, Nominal, t1, t2)

  | cls `hasKey` heqTyConKey -- Solves (t1 ~~ t2)
  , [_,_,t1,t2] <- args
  = (heqDataCon,  Nominal, t1, t2)

  | cls `hasKey` coercibleTyConKey  -- Solves (Coercible t1 t2)
  , [_, t1, t2] <- args
  = (coercibleDataCon, Representational, t1, t2)

  | otherwise  -- Does not satisfy the precondition
  = pprPanic "matchEqualityInst" (ppr (mkClassPred cls args))


{- ********************************************************************
*                                                                     *
              Class lookup for overloaded record fields
*                                                                     *
***********************************************************************-}

{-
Note [HasField instances]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

    data T y = MkT { foo :: [y] }

and `foo` is in scope.  Then GHC will automatically solve a constraint like

    HasField "foo" (T Int) b

by emitting a new wanted

    T alpha -> [alpha] ~# T Int -> b

and building a HasField dictionary out of the selector function `foo`,
appropriately cast.

The HasField class is defined (in GHC.Records) thus:

    type HasField :: forall {k} {r_rep} {a_rep} . k -> TYPE r_rep -> TYPE a_rep -> Constraint
    class HasField x r a | x r -> a where
      getField :: r -> a

Since this is a one-method class, it is represented as a newtype.
Hence we can solve `HasField "foo" (T Int) b` by taking an expression
of type `T Int -> b` and casting it using the newtype coercion.
Note that

    foo :: forall y . T y -> [y]

so the expression we construct is

    foo @alpha |> co

where

    co :: (T alpha -> [alpha]) ~# HasField "foo" (T Int) b

is built from

    co1 :: (T alpha -> [alpha]) ~# (T Int -> b)

which is the new wanted, and

    co2 :: (T Int -> b) ~# HasField "foo" (T Int) b

which can be derived from the newtype coercion.

(HF1) If `foo` is not in scope, or has a higher-rank or existentially
  quantified type, then the constraint is not solved automatically, but
  may be solved by a user-supplied HasField instance.  Similarly, if we
  encounter a HasField constraint where the field is not a literal
  string, or does not belong to the type, then we fall back on the
  normal constraint solver behaviour.


Note [Unused name reporting and HasField]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a HasField constraint is solved by the type-checker, we must record a use
of the corresponding field name, as otherwise it might be reported as unused.
See #19213.  We need to call keepAlive to add the name to the tcg_keep set,
which accumulates names used by the constraint solver, as described by
Note [Tracking unused binding and imports] in GHC.Tc.Types.

We need to call addUsedGRE as well because there may be a deprecation warning on
the field, which will be reported by addUsedGRE.  But calling addUsedGRE without
keepAlive is not enough, because the field might be defined locally, and
addUsedGRE extends tcg_used_gres with imported GREs only.
-}

-- See Note [HasField instances]
matchHasField :: DynFlags -> Bool -> Class -> [Type]
              -> Maybe CtLoc        -- Nothing used only during type validity checking
              -> TcM ClsInstResult
matchHasField dflags short_cut clas tys mb_ct_loc
  = do { fam_inst_envs <- tcGetFamInstEnvs
       ; rdr_env       <- getGlobalRdrEnv
       ; case lookupHasFieldLabel fam_inst_envs rdr_env tys of
            Just (sel_name, gre, r_ty, a_ty) ->
                do { sel_id <- tcLookupId sel_name
                   ; (tv_prs, preds, sel_ty) <- tcInstType newMetaTyVars sel_id

                         -- The first new wanted constraint equates the actual
                         -- type of the selector with the type (r -> a) within
                         -- the HasField x r a dictionary.  The preds will
                         -- typically be empty, but if the datatype has a
                         -- "stupid theta" then we have to include it here.
                   ; let tvs   = mkTyVarTys (map snd tv_prs)
                         theta = mkPrimEqPred sel_ty (mkVisFunTyMany r_ty a_ty) : preds

                         -- Use the equality proof to cast the selector Id to
                         -- type (r -> a), then use the newtype coercion to cast
                         -- it to a HasField dictionary.
                         mk_ev (ev1:evs) = evSelector sel_id tvs evs `evCast` co
                           where
                             co = mkSubCo (evTermCoercion (EvExpr ev1))
                                      `mkTransCo` mkSymCo co2
                         mk_ev [] = panic "matchHasField.mk_ev"

                         Just (_, co2) = tcInstNewTyCon_maybe (classTyCon clas) tys

                     -- The selector must not be "naughty" (i.e. the field
                     -- cannot have an existentially quantified type),
                     -- and it must not be higher-rank.
                   ; if (isNaughtyRecordSelector sel_id) || not (isTauTy sel_ty)
                     then try_user_instances
                     else
                do { case mb_ct_loc of
                       Nothing -> return ()  -- Nothing: happens when type-validity checking
                       Just loc ->  setCtLocM loc $  -- Set location for warnings
                         do { -- See Note [Unused name reporting and HasField]
                              addUsedGRE AllDeprecationWarnings gre
                            ; keepAlive sel_name

                              -- Warn about incomplete record selection
                           ; warnIncompleteRecSel dflags sel_id loc }

                   ; return OneInst { cir_new_theta   = theta
                                    , cir_mk_ev       = mk_ev
                                    , cir_canonical   = EvCanonical
                                    , cir_what        = BuiltinInstance } } }

            Nothing -> try_user_instances }
   where
     -- See (HF1) in Note [HasField instances]
     try_user_instances = matchInstEnv dflags short_cut clas tys

warnIncompleteRecSel :: DynFlags -> Id -> CtLoc -> TcM ()
-- Warn about incomplete record selectors
-- See (IRS6) in Note [Detecting incomplete record selectors] in GHC.HsToCore.Pmc
warnIncompleteRecSel dflags sel_id ct_loc
  | not (isGetFieldOrigin (ctLocOrigin ct_loc))
      -- isGetFieldOrigin: see (IRS7) in
      -- Note [Detecting incomplete record selectors] in GHC.HsToCore.Pmc
  , not (null fallible_cons)
  = addDiagnostic $
    TcRnHasFieldResolvedIncomplete (idName sel_id) fallible_cons maxCons

  | otherwise
  = return ()
  where
    maxCons = maxUncoveredPatterns dflags
    fallible_cons = rsi_undef $ sel_cons $ idDetails sel_id

    -- GHC.Tc.Gen.App.tcInstFun arranges that the CtOrigin of (r.x) is GetFieldOrigin,
    -- despite the expansion to (getField @"x" r)
    isGetFieldOrigin (GetFieldOrigin {}) = True
    isGetFieldOrigin _                   = False

lookupHasFieldLabel
  :: FamInstEnvs -> GlobalRdrEnv -> [Type]
  -> Maybe ( Name          -- Name of the record selector
           , GlobalRdrElt  -- GRE for the selector
           , Type          -- Type of the record value
           , Type )        -- Type of the field of the record
-- If possible, decompose application
--     (HasField @k @rrep @arep @"fld" @(T t1..tn) @fld-ty),
--  or (getField @k @rrep @arep @"fld" @(T t1..tn) @fld-ty)
-- and return the pieces, if the record selector is in scope
--
-- A complication is that `T` might be a data family, so we need to
-- look it up in the `fam_envs` to find its representation tycon.
lookupHasFieldLabel fam_inst_envs rdr_env arg_tys
  |  -- We are matching HasField {k} {r_rep} {a_rep} x r a...
    (_k : _rec_rep : _fld_rep : x_ty : rec_ty : fld_ty : _) <- arg_tys
    -- x should be a literal string
  , Just x <- isStrLitTy x_ty
    -- r should be an applied type constructor
  , Just (tc, args) <- tcSplitTyConApp_maybe rec_ty
    -- Use the representation tycon (if data family); it has the fields
  , let r_tc = fstOf3 (tcLookupDataFamInst fam_inst_envs tc args)
    -- x should be a field of r
  , Just fl <- lookupTyConFieldLabel (FieldLabelString x) r_tc
    -- Ensure the field selector is in scope
  , Just gre <- lookupGRE_FieldLabel rdr_env fl
  = Just (flSelector fl, gre, rec_ty, fld_ty)

  | otherwise
  = Nothing
