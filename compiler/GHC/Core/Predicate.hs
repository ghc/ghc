{-# LANGUAGE DerivingStrategies #-}

{-

Describes predicates as they are considered by the solver.

-}

module GHC.Core.Predicate (
  Pred(..), classifyPredType,
  isPredTy, isSimplePredTy,

  -- Equality predicates
  EqRel(..), eqRelRole,
  isEqPred, isReprEqPred, isEqClassPred, isCoVarType,
  getEqPredTys, getEqPredTys_maybe, getEqPredRole,
  predTypeEqRel, pprPredType,
  mkNomEqPred, mkReprEqPred, mkEqPred, mkEqPredRole,

  -- Class predicates
  mkClassPred, isDictTy, typeDeterminesValue,
  isClassPred, isEqualityClass, isCTupleClass, isUnaryClass,
  getClassPredTys, getClassPredTys_maybe,
  classMethodTy, classMethodInstTy,

  -- Implicit parameters
  couldBeIPLike, mightMentionIP, isIPTyCon, isIPClass, decomposeIPPred,
  isCallStackTy, isCallStackPred, isCallStackPredTy,
  isExceptionContextPred, isExceptionContextTy,
  isIPPred_maybe,

  -- Evidence variables
  DictId, isEvId, isDictId,

  -- * Well-scoped free variables
  scopedSort, tyCoVarsOfTypeWellScoped,
  tyCoVarsOfTypesWellScoped,

  -- Equality left-hand sides
  CanEqLHS(..), canEqLHS_maybe, canTyFamEqLHS_maybe,
  canEqLHSKind, canEqLHSType, eqCanEqLHS

  ) where

import GHC.Prelude

import GHC.Core.Type
import GHC.Core.Class
import GHC.Core.TyCo.Compare( tcEqTyConApps )
import GHC.Core.TyCo.FVs( tyCoVarsOfTypeList, tyCoVarsOfTypesList )
import GHC.Core.TyCon
import GHC.Core.TyCon.RecWalk
import GHC.Types.Name( getOccName )
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Core.Multiplicity ( scaledThing )

import GHC.Builtin.Names
import GHC.Builtin.Types.Prim( eqPrimTyCon, eqReprPrimTyCon )

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Data.FastString


{- *********************************************************************
*                                                                      *
*                   Pred and PredType                                  *
*                                                                      *
********************************************************************* -}

{- Note [Types for coercions, predicates, and evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A "predicate" or "predicate type",
    type synonym `PredType`
    returns True to `isPredTy`
is any type of kind (CONSTRAINT r) for some `r`.

  (a) A "class predicate" (aka dictionary type) is the type of a (boxed)
      type-class dictionary
        Test: isDictTy
        Binders: DictIds
        Kind: Constraint
        Examples: (Eq a), and (a ~ b)

  (b) An "equality predicate" is a primitive, unboxed equalities
        Test: isEqPred
        Binders: CoVars (can appear in coercions)
        Kind: CONSTRAINT (TupleRep [])
        Examples: (t1 ~# t2) or (t1 ~R# t2)

  (c) A "simple predicate type" is either a class predicate or an equality predicate
        Test: isSimplePredTy
        Kind: Constraint or CONSTRAINT (TupleRep [])
        Examples: all coercion types and dictionary types

  (d) A "forall-predicate" is the type of a possibly-polymorphic function
      returning a predicate; e.g.
           forall a. Eq a => Eq [a]

  (e) An "irred predicate" is any other type of kind (CONSTRAINT r),
      typically something like `c` or `c Int`, for some suitably-kinded `c`


* Predicates are classified by `classifyPredType`.

* Equality types and dictionary types are mutually exclusive.

* Predicates are the things solved by the constraint solver; and
  /evidence terms/ witness those solutions.  An /evidence variable/
  (or EvId) has a type that is a PredType.

* Generally speaking, the /type/ of a predicate determines its /value/;
  that is, predicates are singleton types.  The big exception is implicit
  parameters.  See Note [Type determines value]

* In a FunTy { ft_af = af }, where af = FTF_C_T or FTF_C_C,
  the argument type is always a Predicate type.
-}

-- | A predicate in the solver. The solver tries to prove Wanted predicates
-- from Given ones.
data Pred

  -- | A typeclass predicate.
  = ClassPred Class [Type]

  -- | A type equality predicate, (t1 ~#N t2) or (t1 ~#R t2)
  | EqPred EqRel Type Type

  -- | An irreducible predicate.
  | IrredPred PredType

  -- | A quantified predicate.
  --
  -- See Note [Quantified constraints] in GHC.Tc.Solver.Solve
  | ForAllPred [TyVar] [PredType] PredType

  -- NB: There is no TuplePred case
  --     Tuple predicates like (Eq a, Ord b) are just treated
  --     as ClassPred, as if we had a tuple class with two superclasses
  --        class (c1, c2) => CTuple2 c1 c2

classifyPredType :: HasDebugCallStack => PredType -> Pred
-- Precondition: the argument is a predicate type, with kind (CONSTRAINT _)
classifyPredType ev_ty
  = assertPpr (isPredTy ev_ty) (ppr ev_ty) $
    case splitTyConApp_maybe ev_ty of
      Just (tc, [_, _, ty1, ty2])
        | tc `hasKey` eqReprPrimTyConKey -> EqPred ReprEq ty1 ty2
        | tc `hasKey` eqPrimTyConKey     -> EqPred NomEq  ty1 ty2

      Just (tc, tys)
        | Just clas <- tyConClass_maybe tc
        -> ClassPred clas tys

      _ | (tvs, rho) <- splitForAllTyCoVars ev_ty
        , (theta, pred) <- splitFunTys rho
        , not (null tvs && null theta)
        -> ForAllPred tvs (map scaledThing theta) pred

        | otherwise
        -> IrredPred ev_ty

isSimplePredTy :: HasDebugCallStack => Type -> Bool
-- Return True for (t1 ~# t2) regardless of role, and (C tys)
-- /Not/ true of quantified-predicate type like (forall a. Eq a => Eq [a])
-- Precondition: expects a type that classifies values (i.e. not a type constructor)
-- See Note [Types for coercions, predicates, and evidence]
isSimplePredTy ty
  = case tyConAppTyCon_maybe ty of
       Nothing -> False
       Just tc -> isClassTyCon tc ||
                  tc `hasKey` eqPrimTyConKey ||
                  tc `hasKey` eqReprPrimTyConKey

isPredTy :: Type -> Bool
-- True of all types of kind (CONSTRAINT r) for some `r`
-- See Note [Types for coercions, predicates, and evidence]
--
-- In particular it is True of
--    - the constraints handled by the constraint solver,
--      including quantified constraints
--    - dictionary functions (forall a. Eq a => Eq [a])
isPredTy ty = case typeTypeOrConstraint ty of
                        TypeLike       -> False
                        ConstraintLike -> True

typeDeterminesValue :: PredType -> Bool
-- ^ Is the type *guaranteed* to determine the value?
-- Might say No even if the type does determine the value.
-- See Note [Type determines value]
typeDeterminesValue ty = isDictTy ty && not (couldBeIPLike ty)


{-
Note [Evidence for quantified constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The superclass mechanism in GHC.Tc.Solver.Dict.makeSuperClasses risks
taking a quantified constraint like
   (forall a. C a => a ~ b)
and generate superclass evidence
   (forall a. C a => a ~# b)

This is a funny thing: neither isPredTy nor isCoVarType are true
of it.  So we are careful not to generate it in the first place:
see Note [Equality superclasses in quantified constraints]
in GHC.Tc.Solver.Dict.
-}

-- --------------------- Equality predicates ---------------------------------

-- | Does this type classify a core (unlifted) Coercion?
-- At either role nominal or representational
--    (t1 ~# t2) or (t1 ~R# t2)
-- See Note [Types for coercions, predicates, and evidence] in "GHC.Core.TyCo.Rep"
isEqPred :: PredType -> Bool
-- True of (s ~# t) (s ~R# t)
-- NB: but NOT true of (s ~ t) or (s ~~ t) or (Coecible s t)
isEqPred ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` eqPrimTyConKey || tc `hasKey` eqReprPrimTyConKey
  | otherwise
  = False

isCoVarType :: Type -> Bool
-- Just a synonym for isEqPred
isCoVarType = isEqPred

isReprEqPred :: PredType -> Bool
-- True of (s ~R# t)
isReprEqPred ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` eqReprPrimTyConKey
  | otherwise
  = False

-- --------------------- Class predicates ---------------------------------

mkClassPred :: Class -> [Type] -> PredType
mkClassPred clas tys = mkTyConApp (classTyCon clas) tys

isClassPred :: PredType -> Bool
isClassPred ty = case tyConAppTyCon_maybe ty of
    Just tc -> isClassTyCon tc
    _       -> False

isDictTy :: Type -> Bool
isDictTy = isClassPred

isEqClassPred :: PredType -> Bool
isEqClassPred ty  -- True of (s ~ t) and (s ~~ t)
                  -- ToDo: should we check saturation?
  | Just tc <- tyConAppTyCon_maybe ty
  , Just cls <- tyConClass_maybe tc
  = isEqualityClass cls
  | otherwise
  = False

isEqualityClass :: Class -> Bool
-- True of (~), (~~), and Coercible
-- These all have a single primitive-equality superclass, either (~N# or ~R#)
isEqualityClass cls
  = cls `hasKey` heqTyConKey
    || cls `hasKey` eqTyConKey
    || cls `hasKey` coercibleTyConKey

isCTupleClass :: Class -> Bool
isCTupleClass cls = isTupleTyCon (classTyCon cls)

isUnaryClass :: Class -> Bool
isUnaryClass cls = isUnaryClassTyCon (classTyCon cls)

getClassPredTys :: HasDebugCallStack => PredType -> (Class, [Type])
getClassPredTys ty = case getClassPredTys_maybe ty of
        Just (clas, tys) -> (clas, tys)
        Nothing          -> pprPanic "getClassPredTys" (ppr ty)

getClassPredTys_maybe :: PredType -> Maybe (Class, [Type])
getClassPredTys_maybe ty = case splitTyConApp_maybe ty of
        Just (tc, tys) | Just clas <- tyConClass_maybe tc -> Just (clas, tys)
        _ -> Nothing

classMethodTy :: Id -> Type
-- Takes a class selector op :: forall a. C a => meth_ty
-- and returns the type of its method, meth_ty
-- The selector can be a superclass selector, in which case
-- you get back a superclass
classMethodTy sel_id
  = funResultTy $        -- meth_ty
    dropForAlls $        -- C a => meth_ty
    varType sel_id        -- forall a. C n => meth_ty

classMethodInstTy :: Id -> [Type] -> Type
-- Takes a class selector op :: forall a b. C a b => meth_ty
-- and the types [ty1, ty2] at which it is instantiated,
-- returns the instantiated type of its method, meth_ty[t1/a,t2/b]
-- The selector can be a superclass selector, in which case
-- you get back a superclass
classMethodInstTy sel_id arg_tys
  = funResultTy $
    piResultTys (varType sel_id) arg_tys

{- Note [Type determines value]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Only specialise on non-impicit-parameter predicates, because these
are the ones whose *type* determines their *value*.  In particular,
with implicit params, the type args *don't* say what the value of the
implicit param is!  See #7101.

So we treat implicit params just like ordinary arguments for the
purposes of specialisation.  Note that we still want to specialise
functions with implicit params if they have *other* dicts which are
class params; see #17930.

It's also not always possible to infer that a type determines the value
if type families are in play. See #19747 for one such example.

-}

-- --------------------- Equality predicates ---------------------------------

-- | A choice of equality relation. This is separate from the type 'Role'
-- because 'Phantom' does not define a (non-trivial) equality relation.
data EqRel = NomEq | ReprEq
  deriving (Eq, Ord)

instance Outputable EqRel where
  ppr NomEq  = text "nominal equality"
  ppr ReprEq = text "representational equality"

eqRelRole :: EqRel -> Role
eqRelRole NomEq  = Nominal
eqRelRole ReprEq = Representational

-- | Creates a primitive nominal type equality predicate.
--      t1 ~# t2
-- Invariant: the types are not Coercions
mkNomEqPred :: Type -> Type -> Type
mkNomEqPred ty1 ty2
  = mkTyConApp eqPrimTyCon [k1, k2, ty1, ty2]
  where
    k1 = typeKind ty1
    k2 = typeKind ty2

-- | Creates a primitive representational type equality predicate.
--      t1 ~R# t2
-- Invariant: the types are not Coercions
mkReprEqPred :: Type -> Type -> Type
mkReprEqPred ty1  ty2
  = mkTyConApp eqReprPrimTyCon [k1, k2, ty1, ty2]
  where
    k1 = typeKind ty1
    k2 = typeKind ty2

-- | Makes a lifted equality predicate at the given role
mkEqPred :: EqRel -> Type -> Type -> PredType
mkEqPred NomEq  = mkNomEqPred
mkEqPred ReprEq = mkReprEqPred

-- | Makes a lifted equality predicate at the given role
mkEqPredRole :: Role -> Type -> Type -> PredType
mkEqPredRole Nominal          = mkNomEqPred
mkEqPredRole Representational = mkReprEqPred
mkEqPredRole Phantom          = panic "mkEqPred phantom"

getEqPredTys :: PredType -> (Type, Type)
getEqPredTys ty
  = case splitTyConApp_maybe ty of
      Just (tc, [_, _, ty1, ty2])
        |  tc `hasKey` eqPrimTyConKey
        || tc `hasKey` eqReprPrimTyConKey
        -> (ty1, ty2)
      _ -> pprPanic "getEqPredTys" (ppr ty)

getEqPredTys_maybe :: PredType -> Maybe (Role, Type, Type)
getEqPredTys_maybe ty
  = case splitTyConApp_maybe ty of
      Just (tc, [_, _, ty1, ty2])
        | tc `hasKey` eqPrimTyConKey     -> Just (Nominal, ty1, ty2)
        | tc `hasKey` eqReprPrimTyConKey -> Just (Representational, ty1, ty2)
      _ -> Nothing

getEqPredRole :: PredType -> Role
-- Precondition: the PredType is (s ~#N t) or (s ~#R t)
getEqPredRole ty = eqRelRole (predTypeEqRel ty)

-- | Get the equality relation relevant for a pred type
-- Returns NomEq for dictionary predicates, etc
predTypeEqRel :: PredType -> EqRel
predTypeEqRel ty
  | isReprEqPred ty = ReprEq
  | otherwise       = NomEq

pprPredType :: PredType -> SDoc
-- Special case for (t1 ~# t2) and (t1 ~R# t2)
pprPredType pred
  = case classifyPredType pred of
      EqPred eq_rel t1 t2 -> sep [ ppr t1, ppr (getOccName eq_tc) <+> ppr t2 ]
         where
           eq_tc = case eq_rel of
                     NomEq  -> eqPrimTyCon
                     ReprEq -> eqReprPrimTyCon
      _ -> ppr pred

{- *********************************************************************
*                                                                      *
              Implicit parameters
*                                                                      *
********************************************************************* -}

-- --------------------- Nomal implicit-parameter predicates ---------------

isIPTyCon :: TyCon -> Bool
isIPTyCon tc = tc `hasKey` ipClassKey
  -- Class and its corresponding TyCon have the same Unique

isIPClass :: Class -> Bool
isIPClass cls = cls `hasKey` ipClassKey

-- | Decomposes a predicate if it is an implicit parameter. Does not look in
-- superclasses. See also [Local implicit parameters].
isIPPred_maybe :: Class -> [Type] -> Maybe (Type, Type)
isIPPred_maybe cls tys
  | isIPClass cls
  , [t1,t2] <- tys
  = Just (t1,t2)
  | otherwise
  = Nothing

-- | Take a type (IP sym ty), where IP is the built in IP class
-- and return (ip, MkIP, [sym,ty]), where
--    `ip` is the class-op for class IP
--    `MkIP` is the data constructor for class IP
decomposeIPPred :: Type -> (Id, [Type])
decomposeIPPred ty
  | Just (cls, tys) <- getClassPredTys_maybe ty
  , [ip_op] <- classMethods cls
  = assertPpr (isIPClass cls && isUnaryClass cls) (ppr ty) $
    (ip_op, tys)
  | otherwise = pprPanic "decomposeIP" (ppr ty)

-- --------------------- ExceptionContext predicates --------------------------

-- | Is a 'PredType' an @ExceptionContext@ implicit parameter?
--
-- If so, return the name of the parameter.
isExceptionContextPred :: Class -> [Type] -> Maybe FastString
isExceptionContextPred cls tys
  | [ty1, ty2] <- tys
  , isIPClass cls
  , isExceptionContextTy ty2
  = isStrLitTy ty1
  | otherwise
  = Nothing

-- | Is a type an 'ExceptionContext'?
isExceptionContextTy :: Type -> Bool
isExceptionContextTy ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` exceptionContextTyConKey
  | otherwise
  = False

-- --------------------- CallStack predicates ---------------------------------

isCallStackPredTy :: Type -> Bool
-- True of HasCallStack, or IP "blah" CallStack
isCallStackPredTy ty
  | Just (tc, tys) <- splitTyConApp_maybe ty
  , Just cls <- tyConClass_maybe tc
  , Just {} <- isCallStackPred cls tys
  = True
  | otherwise
  = False

-- | Is a 'PredType' a 'CallStack' implicit parameter?
--
-- If so, return the name of the parameter.
isCallStackPred :: Class -> [Type] -> Maybe FastString
isCallStackPred cls tys
  | [ty1, ty2] <- tys
  , isIPClass cls
  , isCallStackTy ty2
  = isStrLitTy ty1
  | otherwise
  = Nothing

-- | Is a type a 'CallStack'?
isCallStackTy :: Type -> Bool
isCallStackTy ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` callStackTyConKey
  | otherwise
  = False

-- --------------------- couldBeIPLike and mightMentionIP  --------------------------
--                 See Note [Local implicit parameters]

couldBeIPLike :: Type -> Bool
-- Is `pred`, or any of its superclasses, an implicit parameter?
-- See Note [Local implicit parameters]
couldBeIPLike pred
  = might_mention_ip1 initIPRecTc (const True) (const True) pred

mightMentionIP :: (Type -> Bool) -- ^ predicate on the string
               -> (Type -> Bool) -- ^ predicate on the type
               -> Class
               -> [Type] -> Bool
-- ^ @'mightMentionIP' str_cond ty_cond cls tys@ returns @True@ if:
--
--    - @cls tys@ is of the form @IP str ty@, where @str_cond str@ and @ty_cond ty@
--      are both @True@,
--    - or any superclass of @cls tys@ has this property.
--
-- See Note [Local implicit parameters]
mightMentionIP = might_mention_ip initIPRecTc

might_mention_ip :: RecTcChecker -> (Type -> Bool) -> (Type -> Bool) -> Class -> [Type] -> Bool
might_mention_ip rec_clss str_cond ty_cond cls tys
  | Just (str_ty, ty) <- isIPPred_maybe cls tys
  = str_cond str_ty && ty_cond ty
  | otherwise
  = or [ might_mention_ip1 rec_clss str_cond ty_cond (classMethodInstTy sc_sel_id tys)
       | sc_sel_id <- classSCSelIds cls ]


might_mention_ip1 :: RecTcChecker -> (Type -> Bool) -> (Type -> Bool) -> Type -> Bool
might_mention_ip1 rec_clss str_cond ty_cond ty
  | Just (cls, tys) <- getClassPredTys_maybe ty
  , let tc = classTyCon cls
  , Just rec_clss' <- if isTupleTyCon tc then Just rec_clss
                      else checkRecTc rec_clss tc
  = might_mention_ip rec_clss' str_cond ty_cond cls tys
  | otherwise
  = False -- Includes things like (D []) where D is
          -- a Constraint-ranged family; #7785

initIPRecTc :: RecTcChecker
initIPRecTc = setRecTcMaxBound 1 initRecTc

{- Note [Local implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also wrinkle (SIP1) in Note [Shadowing of implicit parameters] in
GHC.Tc.Solver.Dict.

The function couldBeIPLike tells if this predicate, or any of its
superclasses, is an implicit parameter.

Why are implicit parameters special?  Unlike normal classes, we can
have local instances for implicit parameters, in the form of
   let ?x = True in ...
So in various places we must be careful not to assume that any value
of the right type will do; we must carefully look for the innermost binding.
So couldBeIPLike checks whether this is an implicit parameter, or has
a superclass that is an implicit parameter.

Several wrinkles

* We must be careful with superclasses, as #18649 showed.  Haskell
  doesn't allow an implicit parameter as a superclass
    class (?x::a) => C a where ...
  but with a constraint tuple we might have
     (% Eq a, ?x::Int %)
  and /its/ superclasses, namely (Eq a) and (?x::Int), /do/ include an
  implicit parameter.

  With ConstraintKinds this can apply to /any/ class, e.g.
     class sc => C sc where ...
  Then (C (?x::Int)) has (?x::Int) as a superclass.  So we must
  instantiate and check each superclass, one by one, in
  hasIPSuperClasses.

* With -XUndecidableSuperClasses, the superclass hunt can go on forever,
  so we need a RecTcChecker to cut it off.

* Another apparent additional complexity involves type families. For
  example, consider
         type family D (v::*->*) :: Constraint
         type instance D [] = ()
         f :: D v => v Char -> Int
  If we see a call (f "foo"), we'll pass a "dictionary"
    () |> (g :: () ~ D [])
  and it's good to specialise f at this dictionary.

So the question is: can an implicit parameter "hide inside" a
type-family constraint like (D a).  Well, no.  We don't allow
        type instance D Maybe = ?x:Int
Hence the umbrella 'otherwise' case in is_ip_like_pred.  See #7785.

Small worries (Sept 20):
* I don't see what stops us having that 'type instance'. Indeed I
  think nothing does.
* I'm a little concerned about type variables; such a variable might
  be instantiated to an implicit parameter.  I don't think this
  matters in the cases for which couldBeIPLike is used, and it's pretty
  obscure anyway.
* The superclass hunt stops when it encounters the same class again,
  but in principle we could have the same class, differently instantiated,
  and the second time it could have an implicit parameter
I'm going to treat these as problems for another day. They are all exotic.

Note [Using typesAreApart when calling mightMentionIP]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We call 'mightMentionIP' in two situations:

  (1) to check that a predicate does not contain any implicit parameters
      IP str ty, for a fixed literal str and any type ty,
  (2) to check that a predicate does not contain any HasCallStack or
      HasExceptionContext constraints.

In both of these cases, we want to be sure, so we should be conservative:

  For (1), the predicate might contain an implicit parameter IP Str a, where
  Str is a type family such as:

    type family MyStr where MyStr = "abc"

  To safeguard against this (niche) situation, instead of doing a simple
  type equality check, we use 'typesAreApart'. This allows us to recognise
  that 'IP MyStr a' contains an implicit parameter of the form 'IP "abc" ty'.

  For (2), we similarly might have

    type family MyCallStack where MyCallStack = CallStack

  Again, here we use 'typesAreApart'. This allows us to see that

    (?foo :: MyCallStack)

  is indeed a CallStack constraint, hidden under a type family.
-}

{- *********************************************************************
*                                                                      *
              Evidence variables
*                                                                      *
********************************************************************* -}

isEvId :: Var -> Bool
isEvId var = isPredTy (varType var)

isDictId :: Id -> Bool
isDictId id = isDictTy (varType id)


{- *********************************************************************
*                                                                      *
                 scopedSort

       This function lives here becuase it uses isEvId
*                                                                      *
********************************************************************* -}

{- Note [ScopedSort]
~~~~~~~~~~~~~~~~~~~~
Consider

  foo :: Proxy a -> Proxy (b :: k) -> Proxy (a :: k2) -> ()

This function type is implicitly generalised over [a, b, k, k2]. These
variables will be Specified; that is, they will be available for visible
type application. This is because they are written in the type signature
by the user.

However, we must ask: what order will they appear in? In cases without
dependency, this is easy: we just use the lexical left-to-right ordering
of first occurrence. With dependency, we cannot get off the hook so
easily.

We thus state:

 * These variables appear in the order as given by ScopedSort, where
   the input to ScopedSort is the left-to-right order of first occurrence.

Note that this applies only to *implicit* quantification, without a
`forall`. If the user writes a `forall`, then we just use the order given.

ScopedSort is defined thusly (as proposed in #15743):
  * Work left-to-right through the input list, with a cursor.
  * If variable v at the cursor is depended on by any earlier variable w,
    move v immediately before the leftmost such w.

INVARIANT: The prefix of variables before the cursor form a valid telescope.

Note that ScopedSort makes sense only after type inference is done and all
types/kinds are fully settled and zonked.

-}

-- | Do a topological sort on a list of tyvars,
--   so that binders occur before occurrences
-- E.g. given  @[ a::k, k::Type, b::k ]@
-- it'll return a well-scoped list @[ k::Type, a::k, b::k ]@.
--
-- This is a deterministic sorting operation
-- (that is, doesn't depend on Uniques).
--
-- It is also meant to be stable: that is, variables should not
-- be reordered unnecessarily. This is specified in Note [ScopedSort]
-- See also Note [Ordering of implicit variables] in "GHC.Rename.HsType"

scopedSort :: [Var] -> [Var]
scopedSort = go [] []
  where
    go :: [Var] -- already sorted, in reverse order
       -> [TyCoVarSet] -- each set contains all the variables which must be placed
                       -- before the tv corresponding to the set; they are accumulations
                       -- of the fvs in the sorted Var's types

                       -- This list is in 1-to-1 correspondence with the sorted Vars
                       -- INVARIANT:
                       --   all (\tl -> all (`subVarSet` head tl) (tail tl)) (tails fv_list)
                       -- That is, each set in the list is a superset of all later sets.

       -> [Var] -- yet to be sorted
       -> [Var]
    go acc _fv_list [] = reverse acc
    go acc  fv_list (tv:tvs)
      = go acc' fv_list' tvs
      where
        (acc', fv_list') = insert tv acc fv_list

    insert :: Var           -- var to insert
           -> [Var]         -- sorted list, in reverse order
           -> [TyCoVarSet]  -- list of fvs, as above
           -> ([Var], [TyCoVarSet])   -- augmented lists
    -- Generally we put the new Var at the front of the accumulating list
    -- (leading to a stable sort) unless there is are reason to put it later.
    insert v []     []         = ([v], [tyCoVarsOfType (varType v)])
    insert v (a:as) (fvs:fvss)
      | (isTyVar v && isId a) ||          -- TyVars precede Ids
        (isEvId v && isId a && not (isEvId a)) || -- DictIds precede non-DictIds
        (v `elemVarSet` fvs)
          -- (a) put Ids after TyVars, and (b) respect dependencies
      , (as', fvss') <- insert v as fvss
      = (a:as', fvs `unionVarSet` fv_v : fvss')

      | otherwise  -- Put `v` at the front
      = (v:a:as, fvs `unionVarSet` fv_v : fvs : fvss)
      where
        fv_v = tyCoVarsOfType (varType v)

       -- lists not in correspondence
    insert _ _ _ = panic "scopedSort"

-- | Get the free vars of a type in scoped order
tyCoVarsOfTypeWellScoped :: Type -> [TyVar]
tyCoVarsOfTypeWellScoped = scopedSort . tyCoVarsOfTypeList

-- | Get the free vars of types in scoped order
tyCoVarsOfTypesWellScoped :: [Type] -> [TyVar]
tyCoVarsOfTypesWellScoped = scopedSort . tyCoVarsOfTypesList


{- *********************************************************************
*                                                                      *
*                   Equality left-hand sides
*                                                                      *
********************************************************************* -}

-- | A 'CanEqLHS' is a type that can appear on the left of a canonical
-- equality: a type variable or /exactly-saturated/ type family application.
data CanEqLHS
  = TyVarLHS TyVar
  | TyFamLHS TyCon  -- ^ TyCon of the family
             [Type]   -- ^ Arguments, /exactly saturating/ the family

instance Outputable CanEqLHS where
  ppr (TyVarLHS tv)              = ppr tv
  ppr (TyFamLHS fam_tc fam_args) = ppr (mkTyConApp fam_tc fam_args)

-----------------------------------
-- | Is a type a canonical LHS? That is, is it a tyvar or an exactly-saturated
-- type family application?
-- Does not look through type synonyms.
canEqLHS_maybe :: Type -> Maybe CanEqLHS
canEqLHS_maybe xi
  | Just tv <- getTyVar_maybe xi
  = Just $ TyVarLHS tv

  | otherwise
  = canTyFamEqLHS_maybe xi

canTyFamEqLHS_maybe :: Type -> Maybe CanEqLHS
canTyFamEqLHS_maybe xi
  | Just (tc, args) <- tcSplitTyConApp_maybe xi
  , isTypeFamilyTyCon tc
  , args `lengthIs` tyConArity tc
  = Just $ TyFamLHS tc args

  | otherwise
  = Nothing

-- | Convert a 'CanEqLHS' back into a 'Type'
canEqLHSType :: CanEqLHS -> Type
canEqLHSType (TyVarLHS tv) = mkTyVarTy tv
canEqLHSType (TyFamLHS fam_tc fam_args) = mkTyConApp fam_tc fam_args

-- | Retrieve the kind of a 'CanEqLHS'
canEqLHSKind :: CanEqLHS -> Kind
canEqLHSKind (TyVarLHS tv) = tyVarKind tv
canEqLHSKind (TyFamLHS fam_tc fam_args) = piResultTys (tyConKind fam_tc) fam_args

-- | Are two 'CanEqLHS's equal?
eqCanEqLHS :: CanEqLHS -> CanEqLHS -> Bool
eqCanEqLHS (TyVarLHS tv1) (TyVarLHS tv2) = tv1 == tv2
eqCanEqLHS (TyFamLHS fam_tc1 fam_args1) (TyFamLHS fam_tc2 fam_args2)
  = tcEqTyConApps fam_tc1 fam_args1 fam_tc2 fam_args2
eqCanEqLHS _ _ = False

