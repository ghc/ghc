{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Tc.Instance.Class (
     matchGlobalInst,
     ClsInstResult(..),
     InstanceWhat(..), safeOverlap, instanceReturnsDictCon,
     AssocInstInfo(..), isNotAssociated
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Driver.Session


import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Instantiate(instDFunType, tcInstType)
import GHC.Tc.Instance.Typeable
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Evidence
import GHC.Tc.Instance.Family( tcGetFamInstEnvs, tcInstNewTyCon_maybe, tcLookupDataFamInst )
import GHC.Rename.Env( addUsedGRE )

import GHC.Builtin.Types
import GHC.Builtin.Types.Prim( eqPrimTyCon, eqReprPrimTyCon )
import GHC.Builtin.Names

import GHC.Types.Name.Reader( lookupGRE_FieldLabel )
import GHC.Types.SafeHaskell
import GHC.Types.Name   ( Name, pprDefinedAt )
import GHC.Types.Var.Env ( VarEnv )
import GHC.Types.Id

import GHC.Core.Predicate
import GHC.Core.InstEnv
import GHC.Core.Type
import GHC.Core.Make ( mkCharExpr, mkStringExprFS, mkNaturalExpr )
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.Class

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc( splitAtList, fstOf3 )

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

-- | Indicates if Instance met the Safe Haskell overlapping instances safety
-- check.
--
-- See Note [Safe Haskell Overlapping Instances] in GHC.Tc.Solver
-- See Note [Safe Haskell Overlapping Instances Implementation] in GHC.Tc.Solver
type SafeOverlapping = Bool

data ClsInstResult
  = NoInstance   -- Definitely no instance

  | OneInst { cir_new_theta :: [TcPredType]
            , cir_mk_ev     :: [EvExpr] -> EvTerm
            , cir_what      :: InstanceWhat }

  | NotSure      -- Multiple matches and/or one or more unifiers

data InstanceWhat
  = BuiltinInstance
  | BuiltinEqInstance   -- A built-in "equality instance"; see the
                        -- GHC.Tc.Solver.Monad Note [Solved dictionaries]
  | LocalInstance
  | TopLevInstance { iw_dfun_id   :: DFunId
                   , iw_safe_over :: SafeOverlapping }

instance Outputable ClsInstResult where
  ppr NoInstance = text "NoInstance"
  ppr NotSure    = text "NotSure"
  ppr (OneInst { cir_new_theta = ev
               , cir_what = what })
    = text "OneInst" <+> vcat [ppr ev, ppr what]

instance Outputable InstanceWhat where
  ppr BuiltinInstance   = text "a built-in instance"
  ppr BuiltinEqInstance = text "a built-in equality instance"
  ppr LocalInstance     = text "a locally-quantified instance"
  ppr (TopLevInstance { iw_dfun_id = dfun })
      = hang (text "instance" <+> pprSigmaType (idType dfun))
           2 (text "--" <+> pprDefinedAt (idName dfun))

safeOverlap :: InstanceWhat -> Bool
safeOverlap (TopLevInstance { iw_safe_over = so }) = so
safeOverlap _                                      = True

instanceReturnsDictCon :: InstanceWhat -> Bool
-- See Note [Solved dictionaries] in GHC.Tc.Solver.Monad
instanceReturnsDictCon (TopLevInstance {}) = True
instanceReturnsDictCon BuiltinInstance     = True
instanceReturnsDictCon BuiltinEqInstance   = False
instanceReturnsDictCon LocalInstance       = False

matchGlobalInst :: DynFlags
                -> Bool      -- True <=> caller is the short-cut solver
                             -- See Note [Shortcut solving: overlap]
                -> Class -> [Type] -> TcM ClsInstResult
matchGlobalInst dflags short_cut clas tys
  | cls_name == knownNatClassName
  = matchKnownNat    dflags short_cut clas tys
  | cls_name == knownSymbolClassName
  = matchKnownSymbol dflags short_cut clas tys
  | cls_name == knownCharClassName
  = matchKnownChar dflags short_cut clas tys
  | isCTupleClass clas                = matchCTuple          clas tys
  | cls_name == typeableClassName     = matchTypeable        clas tys
  | clas `hasKey` heqTyConKey         = matchHeteroEquality       tys
  | clas `hasKey` eqTyConKey          = matchHomoEquality         tys
  | clas `hasKey` coercibleTyConKey   = matchCoercible            tys
  | cls_name == hasFieldClassName     = matchHasField dflags short_cut clas tys
  | otherwise                         = matchInstEnv dflags short_cut clas tys
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
            ([], [], _)
                -> do { traceTc "matchClass not matching" (ppr pred)
                      ; return NoInstance }

            -- A single match (& no safe haskell failure)
            ([(ispec, inst_tys)], [], False)
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
                              text "witness" <+> ppr dfun_id
                                             <+> ppr (idType dfun_id) ]
                                -- Record that this dfun is needed
                      ; match_one (null unsafeOverlaps) dfun_id inst_tys }

            -- More than one matches (or Safe Haskell fail!). Defer any
            -- reactions of a multitude until we learn more about the reagent
            _   -> do { traceTc "matchClass multiple matches, deferring choice" $
                        vcat [text "dict" <+> ppr pred,
                              text "matches" <+> ppr matches]
                      ; return NotSure } }
   where
     pred = mkClassPred clas tys

match_one :: SafeOverlapping -> DFunId -> [DFunInstType] -> TcM ClsInstResult
             -- See Note [DFunInstType: instantiating types] in GHC.Core.InstEnv
match_one so dfun_id mb_inst_tys
  = do { traceTc "match_one" (ppr dfun_id $$ ppr mb_inst_tys)
       ; (tys, theta) <- instDFunType dfun_id mb_inst_tys
       ; traceTc "match_one 2" (ppr dfun_id $$ ppr tys $$ ppr theta)
       ; return $ OneInst { cir_new_theta = theta
                          , cir_mk_ev     = evDFunApp dfun_id tys
                          , cir_what      = TopLevInstance { iw_dfun_id = dfun_id
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
  = return (OneInst { cir_new_theta = tys
                    , cir_mk_ev     = tuple_ev
                    , cir_what      = BuiltinInstance })
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
matchKnownNat _ _ clas [ty]     -- clas = KnownNat
  | Just n <- isNumLitTy ty  = makeLitDict clas ty (mkNaturalExpr n)
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
    , let ev_tm = mkEvCast et (mkTcSymCo (mkTcTransCo co_dict co_rep))
    = return $ OneInst { cir_new_theta = []
                       , cir_mk_ev     = \_ -> ev_tm
                       , cir_what      = BuiltinInstance }

    | otherwise
    = pprPanic "makeLitDict" $
      text "Unexpected evidence for" <+> ppr (className clas)
      $$ vcat (map (ppr . idType) (classMethods clas))

{- ********************************************************************
*                                                                     *
                   Class lookup for Typeable
*                                                                     *
***********************************************************************-}

-- | Assumes that we've checked that this is the 'Typeable' class,
-- and it was applied to the correct argument.
matchTypeable :: Class -> [Type] -> TcM ClsInstResult
matchTypeable clas [k,t]  -- clas = Typeable
  -- For the first two cases, See Note [No Typeable for polytypes or qualified types]
  | isForAllTy k                      = return NoInstance   -- Polytype
  | isJust (tcSplitPredFunTy_maybe t) = return NoInstance   -- Qualified type

  -- Now cases that do work
  | k `eqType` naturalTy                   = doTyLit knownNatClassName         t
  | k `eqType` typeSymbolKind              = doTyLit knownSymbolClassName      t
  | k `eqType` charTy                      = doTyLit knownCharClassName        t
  | tcIsConstraintKind t                   = doTyConApp clas t constraintKindTyCon []
  | Just (mult,arg,ret) <- splitFunTy_maybe t   = doFunTy    clas t mult arg ret
  | Just (tc, ks) <- splitTyConApp_maybe t -- See Note [Typeable (T a b c)]
  , onlyNamedBndrsApplied tc ks            = doTyConApp clas t tc ks
  | Just (f,kt)   <- splitAppTy_maybe t    = doTyApp    clas t f kt

matchTypeable _ _ = return NoInstance

-- | Representation for a type @ty@ of the form @arg -> ret@.
doFunTy :: Class -> Type -> Mult -> Type -> Type -> TcM ClsInstResult
doFunTy clas ty mult arg_ty ret_ty
  = return $ OneInst { cir_new_theta = preds
                     , cir_mk_ev     = mk_ev
                     , cir_what      = BuiltinInstance }
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
  = return $ OneInst { cir_new_theta = (map (mk_typeable_pred clas) kind_args)
                     , cir_mk_ev     = mk_ev
                     , cir_what      = BuiltinInstance }
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
  | isForAllTy (tcTypeKind f)
  = return NoInstance -- We can't solve until we know the ctr.
  | otherwise
  = return $ OneInst { cir_new_theta = map (mk_typeable_pred clas) [f, tk]
                     , cir_mk_ev     = mk_ev
                     , cir_what      = BuiltinInstance }
  where
    mk_ev [t1,t2] = evTypeable ty $ EvTypeableTyApp (EvExpr t1) (EvExpr t2)
    mk_ev _ = panic "doTyApp"


-- Emit a `Typeable` constraint for the given type.
mk_typeable_pred :: Class -> Type -> PredType
mk_typeable_pred clas ty = mkClassPred clas [ tcTypeKind ty, ty ]

  -- Typeable is implied by KnownNat/KnownSymbol. In the case of a type literal
  -- we generate a sub-goal for the appropriate class.
  -- See Note [Typeable for Nat and Symbol]
doTyLit :: Name -> Type -> TcM ClsInstResult
doTyLit kc t = do { kc_clas <- tcLookupClass kc
                  ; let kc_pred    = mkClassPred kc_clas [ t ]
                        mk_ev [ev] = evTypeable t $ EvTypeableTyLit (EvExpr ev)
                        mk_ev _    = panic "doTyLit"
                  ; return (OneInst { cir_new_theta = [kc_pred]
                                    , cir_mk_ev     = mk_ev
                                    , cir_what      = BuiltinInstance }) }

{- Note [Typeable (T a b c)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For type applications we always decompose using binary application,
via doTyApp, until we get to a *kind* instantiation.  Example
   Proxy :: forall k. k -> *

To solve Typeable (Proxy (* -> *) Maybe) we
  - First decompose with doTyApp,
    to get (Typeable (Proxy (* -> *))) and Typeable Maybe
  - Then solve (Typeable (Proxy (* -> *))) with doTyConApp

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
  = return (OneInst { cir_new_theta = [ mkTyConApp eqPrimTyCon args ]
                    , cir_mk_ev     = evDataConApp heqDataCon args
                    , cir_what      = BuiltinEqInstance })

matchHomoEquality :: [Type] -> TcM ClsInstResult
-- Solves (t1 ~ t2)
matchHomoEquality args@[k,t1,t2]
  = return (OneInst { cir_new_theta = [ mkTyConApp eqPrimTyCon [k,k,t1,t2] ]
                    , cir_mk_ev     = evDataConApp eqDataCon args
                    , cir_what      = BuiltinEqInstance })
matchHomoEquality args = pprPanic "matchHomoEquality" (ppr args)

-- See also Note [The equality types story] in GHC.Builtin.Types.Prim
matchCoercible :: [Type] -> TcM ClsInstResult
matchCoercible args@[k, t1, t2]
  = return (OneInst { cir_new_theta = [ mkTyConApp eqReprPrimTyCon args' ]
                    , cir_mk_ev     = evDataConApp coercibleDataCon args
                    , cir_what      = BuiltinEqInstance })
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
             , Just fl <- lookupTyConFieldLabel x r_tc
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
                             co = mkTcSubCo (evTermCoercion (EvExpr ev1))
                                      `mkTcTransCo` mkTcSymCo co2
                         mk_ev [] = panic "matchHasField.mk_ev"

                         Just (_, co2) = tcInstNewTyCon_maybe (classTyCon clas)
                                                              tys

                         tvs = mkTyVarTys (map snd tv_prs)

                     -- The selector must not be "naughty" (i.e. the field
                     -- cannot have an existentially quantified type), and
                     -- it must not be higher-rank.
                   ; if not (isNaughtyRecordSelector sel_id) && isTauTy sel_ty
                     then do { addUsedGRE True gre
                             ; return OneInst { cir_new_theta = theta
                                              , cir_mk_ev     = mk_ev
                                              , cir_what      = BuiltinInstance } }
                     else matchInstEnv dflags short_cut clas tys }

           _ -> matchInstEnv dflags short_cut clas tys }
