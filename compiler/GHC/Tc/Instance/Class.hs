
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Tc.Instance.Class (
     matchGlobalInst,
     ClsInstResult(..),
     safeOverlap, instanceReturnsDictCon,
     AssocInstInfo(..), isNotAssociated,
  ) where

import GHC.Prelude

import GHC.Driver.Session

import GHC.Core.TyCo.Rep

import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Instantiate(instDFunType, tcInstType)
import GHC.Tc.Instance.Typeable
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Origin (InstanceWhat (..), SafeOverlapping)
import GHC.Tc.Instance.Family( tcGetFamInstEnvs, tcInstNewTyCon_maybe, tcLookupDataFamInst )
import GHC.Rename.Env( addUsedGRE )

import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Builtin.Names

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

import GHC.Core ( Expr(Var, App, Cast) )

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc( splitAtList, fstOf3 )
import GHC.Data.FastString

import Language.Haskell.Syntax.Basic (FieldLabelString(..))

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
            , cir_coherence   :: Coherence -- See Note [Coherence and specialisation: overview]
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
                -> Class -> [Type] -> TcM ClsInstResult
matchGlobalInst dflags short_cut clas tys
  | cls_name == knownNatClassName      = matchKnownNat    dflags short_cut clas tys
  | cls_name == knownSymbolClassName   = matchKnownSymbol dflags short_cut clas tys
  | cls_name == knownCharClassName     = matchKnownChar   dflags short_cut clas tys
  | isCTupleClass clas                 = matchCTuple                       clas tys
  | cls_name == typeableClassName      = matchTypeable                     clas tys
  | cls_name == withDictClassName      = matchWithDict                          tys
  | clas `hasKey` heqTyConKey          = matchHeteroEquality                    tys
  | clas `hasKey` eqTyConKey           = matchHomoEquality                      tys
  | clas `hasKey` coercibleTyConKey    = matchCoercible                         tys
  | cls_name == hasFieldClassName      = matchHasField    dflags short_cut clas tys
  | cls_name == unsatisfiableClassName = return NoInstance -- See (B) in Note [Implementation of Unsatisfiable constraints] in GHC.Tc.Errors
  | otherwise                          = matchInstEnv     dflags short_cut clas tys
  where
    cls_name = className clas


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
            ([(ispec, inst_tys)], NoUnifiers coherence, False)
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
                      ; traceTc "matchClass success" $
                        vcat [text "dict" <+> ppr pred,
                              ppr coherence,
                              text "witness" <+> ppr dfun_id
                                             <+> ppr (idType dfun_id) ]
                                -- Record that this dfun is needed
                      ; match_one (null unsafeOverlaps) coherence dfun_id inst_tys }

            -- More than one matches (or Safe Haskell fail!). Defer any
            -- reactions of a multitude until we learn more about the reagent
            _   -> do { traceTc "matchClass multiple matches, deferring choice" $
                        vcat [text "dict" <+> ppr pred,
                              text "matches" <+> ppr matches]
                      ; return NotSure } }
   where
     pred = mkClassPred clas tys

match_one :: SafeOverlapping -> Coherence -> DFunId -> [DFunInstType] -> TcM ClsInstResult
             -- See Note [DFunInstType: instantiating types] in GHC.Core.InstEnv
match_one so coherence dfun_id mb_inst_tys
  = do { traceTc "match_one" (ppr dfun_id $$ ppr mb_inst_tys)
       ; (tys, theta) <- instDFunType dfun_id mb_inst_tys
       ; traceTc "match_one 2" (ppr dfun_id $$ ppr tys $$ ppr theta)
       ; return $ OneInst { cir_new_theta   = theta
                          , cir_mk_ev       = evDFunApp dfun_id tys
                          , cir_coherence   = coherence
                          , cir_what        = TopLevInstance { iw_dfun_id = dfun_id
                                                             , iw_safe_over = so } } }


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
                    , cir_coherence   = IsCoherent
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

In practice, we solve `KnownNat` predicates in the type-checker
(see GHC.Tc.Solver.Interact) because we can't have infinitely many instances.
The evidence (aka "dictionary") for `KnownNat` is of the form `EvLit (EvNum n)`.

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
-- of the appropriate type.  See Note [KnownNat & KnownSymbol and EvLit]
-- in GHC.Tc.Types.Evidence.  The coercion happens in 2 steps:
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
                       , cir_coherence   = IsCoherent
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
                          , cir_coherence   = IsIncoherent -- See (WD6) in Note [withDict]
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
      it. Moreover, we mark the evidence as incoherent, resulting in
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

-}

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
                     , cir_coherence   = IsCoherent
                     , cir_what        = BuiltinInstance }
  where
    preds = map (mk_typeable_pred clas) [mult, arg_ty, ret_ty]
    mk_ev [mult_ev, arg_ev, ret_ev] = evTypeable ty $
                        EvTypeableTrFun (EvExpr mult_ev) (EvExpr arg_ev) (EvExpr ret_ev)
    mk_ev _ = panic "GHC.Tc.Solver.Interact.doFunTy"


-- | Representation for type constructor applied to some kinds.
-- 'onlyNamedBndrsApplied' has ensured that this application results in a type
-- of monomorphic kind (e.g. all kind variables have been instantiated).
doTyConApp :: Class -> Type -> TyCon -> [Kind] -> TcM ClsInstResult
doTyConApp clas ty tc kind_args
  | tyConIsTypeable tc
  = return $ OneInst { cir_new_theta   = map (mk_typeable_pred clas) kind_args
                     , cir_mk_ev       = mk_ev
                     , cir_coherence   = IsCoherent
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
                     , cir_coherence   = IsCoherent
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
                                    , cir_coherence   = IsCoherent
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
matchHeteroEquality :: [Type] -> TcM ClsInstResult
-- Solves (t1 ~~ t2)
matchHeteroEquality args
  = return (OneInst { cir_new_theta   = [ mkTyConApp eqPrimTyCon args ]
                    , cir_mk_ev       = evDataConApp heqDataCon args
                    , cir_coherence   = IsCoherent
                    , cir_what        = BuiltinEqInstance })

matchHomoEquality :: [Type] -> TcM ClsInstResult
-- Solves (t1 ~ t2)
matchHomoEquality args@[k,t1,t2]
  = return (OneInst { cir_new_theta   = [ mkTyConApp eqPrimTyCon [k,k,t1,t2] ]
                    , cir_mk_ev       = evDataConApp eqDataCon args
                    , cir_coherence   = IsCoherent
                    , cir_what        = BuiltinEqInstance })
matchHomoEquality args = pprPanic "matchHomoEquality" (ppr args)

-- See also Note [The equality types story] in GHC.Builtin.Types.Prim
matchCoercible :: [Type] -> TcM ClsInstResult
matchCoercible args@[k, t1, t2]
  = return (OneInst { cir_new_theta   = [ mkTyConApp eqReprPrimTyCon args' ]
                    , cir_mk_ev       = evDataConApp coercibleDataCon args
                    , cir_coherence   = IsCoherent
                    , cir_what        = BuiltinEqInstance })
  where
    args' = [k, k, t1, t2]
matchCoercible args = pprPanic "matchLiftedCoercible" (ppr args)

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

    class HasField (x :: k) r a | x r -> a where
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

If `foo` is not in scope, or has a higher-rank or existentially
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
matchHasField :: DynFlags -> Bool -> Class -> [Type] -> TcM ClsInstResult
matchHasField dflags short_cut clas tys
  = do { fam_inst_envs <- tcGetFamInstEnvs
       ; rdr_env       <- getGlobalRdrEnv
       ; case tys of
           -- We are matching HasField {k} x r a...
           [_k_ty, x_ty, r_ty, a_ty]
               -- x should be a literal string
             | Just x <- isStrLitTy x_ty
               -- r should be an applied type constructor
             , Just (tc, args) <- tcSplitTyConApp_maybe r_ty
               -- use representation tycon (if data family); it has the fields
             , let r_tc = fstOf3 (tcLookupDataFamInst fam_inst_envs tc args)
               -- x should be a field of r
             , Just fl <- lookupTyConFieldLabel (FieldLabelString x) r_tc
               -- the field selector should be in scope
             , Just gre <- lookupGRE_FieldLabel rdr_env fl

             -> do { sel_id <- tcLookupId (flSelector fl)
                   ; (tv_prs, preds, sel_ty) <- tcInstType newMetaTyVars sel_id

                         -- The first new wanted constraint equates the actual
                         -- type of the selector with the type (r -> a) within
                         -- the HasField x r a dictionary.  The preds will
                         -- typically be empty, but if the datatype has a
                         -- "stupid theta" then we have to include it here.
                   ; let theta = mkPrimEqPred sel_ty (mkVisFunTyMany r_ty a_ty) : preds

                         -- Use the equality proof to cast the selector Id to
                         -- type (r -> a), then use the newtype coercion to cast
                         -- it to a HasField dictionary.
                         mk_ev (ev1:evs) = evSelector sel_id tvs evs `evCast` co
                           where
                             co = mkSubCo (evTermCoercion (EvExpr ev1))
                                      `mkTransCo` mkSymCo co2
                         mk_ev [] = panic "matchHasField.mk_ev"

                         Just (_, co2) = tcInstNewTyCon_maybe (classTyCon clas)
                                                              tys

                         tvs = mkTyVarTys (map snd tv_prs)

                     -- The selector must not be "naughty" (i.e. the field
                     -- cannot have an existentially quantified type), and
                     -- it must not be higher-rank.
                   ; if not (isNaughtyRecordSelector sel_id) && isTauTy sel_ty
                     then do { -- See Note [Unused name reporting and HasField]
                               addUsedGRE True gre
                             ; keepAlive (greName gre)
                             ; return OneInst { cir_new_theta   = theta
                                              , cir_mk_ev       = mk_ev
                                              , cir_coherence   = IsCoherent
                                              , cir_what        = BuiltinInstance } }
                     else matchInstEnv dflags short_cut clas tys }

           _ -> matchInstEnv dflags short_cut clas tys }
