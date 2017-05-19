{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Error-checking and other utilities for @deriving@ clauses or declarations.
-}

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}

module TcDerivUtils (
        DerivSpec(..), pprDerivSpec,
        DerivSpecMechanism(..), isDerivSpecStock,
        isDerivSpecNewtype, isDerivSpecAnyClass,
        DerivContext, DerivStatus(..),
        PredOrigin(..), ThetaOrigin(..), mkPredOrigin,
        mkThetaOrigin, mkThetaOriginFromPreds, substPredOrigin,
        checkSideConditions, hasStockDeriving,
        canDeriveAnyClass,
        std_class_via_coercible, non_coercible_class,
        newDerivClsInst, extendLocalInstEnv
    ) where

import Bag
import BasicTypes
import Class
import DataCon
import DynFlags
import ErrUtils
import HscTypes (lookupFixity, mi_fix)
import HsSyn
import Inst
import InstEnv
import LoadIface (loadInterfaceForName)
import Module (getModule)
import Name
import Outputable
import PrelNames
import SrcLoc
import TcGenDeriv
import TcGenFunctor
import TcGenGenerics
import TcRnMonad
import TcType
import THNames (liftClassKey)
import TyCon
import Type
import Util
import VarSet

import qualified GHC.LanguageExtensions as LangExt
import ListSetOps (assocMaybe)

data DerivSpec theta = DS { ds_loc       :: SrcSpan
                          , ds_name      :: Name         -- DFun name
                          , ds_tvs       :: [TyVar]
                          , ds_theta     :: theta
                          , ds_cls       :: Class
                          , ds_tys       :: [Type]
                          , ds_tc        :: TyCon
                          , ds_overlap   :: Maybe OverlapMode
                          , ds_mechanism :: DerivSpecMechanism }
        -- This spec implies a dfun declaration of the form
        --       df :: forall tvs. theta => C tys
        -- The Name is the name for the DFun we'll build
        -- The tyvars bind all the variables in the theta
        -- For type families, the tycon in
        --       in ds_tys is the *family* tycon
        --       in ds_tc is the *representation* type
        -- For non-family tycons, both are the same

        -- the theta is either the given and final theta, in standalone deriving,
        -- or the not-yet-simplified list of constraints together with their origin

        -- ds_mechanism specifies the means by which GHC derives the instance.
        -- See Note [Deriving strategies] in TcDeriv

{-
Example:

     newtype instance T [a] = MkT (Tree a) deriving( C s )
==>
     axiom T [a] = :RTList a
     axiom :RTList a = Tree a

     DS { ds_tvs = [a,s], ds_cls = C, ds_tys = [s, T [a]]
        , ds_tc = :RTList, ds_mechanism = DerivSpecNewtype (Tree a) }
-}

pprDerivSpec :: Outputable theta => DerivSpec theta -> SDoc
pprDerivSpec (DS { ds_loc = l, ds_name = n, ds_tvs = tvs, ds_cls = c,
                   ds_tys = tys, ds_theta = rhs, ds_mechanism = mech })
  = hang (text "DerivSpec")
       2 (vcat [ text "ds_loc       =" <+> ppr l
               , text "ds_name      =" <+> ppr n
               , text "ds_tvs       =" <+> ppr tvs
               , text "ds_cls       =" <+> ppr c
               , text "ds_tys       =" <+> ppr tys
               , text "ds_theta     =" <+> ppr rhs
               , text "ds_mechanism =" <+> ppr mech ])

instance Outputable theta => Outputable (DerivSpec theta) where
  ppr = pprDerivSpec

-- What action to take in order to derive a class instance.
-- See Note [Deriving strategies] in TcDeriv
-- NB: DerivSpecMechanism is purely local to this module
data DerivSpecMechanism
  = DerivSpecStock   -- "Standard" classes
      (SrcSpan -> TyCon -> [Type] -> TcM (LHsBinds GhcPs, BagDerivStuff))

  | DerivSpecNewtype -- -XGeneralizedNewtypeDeriving
      Type -- ^ The newtype rep type

  | DerivSpecAnyClass -- -XDeriveAnyClass

isDerivSpecStock, isDerivSpecNewtype, isDerivSpecAnyClass
  :: DerivSpecMechanism -> Bool
isDerivSpecStock (DerivSpecStock{}) = True
isDerivSpecStock _                  = False

isDerivSpecNewtype (DerivSpecNewtype{}) = True
isDerivSpecNewtype _                    = False

isDerivSpecAnyClass (DerivSpecAnyClass{}) = True
isDerivSpecAnyClass _                     = False

-- A DerivSpecMechanism can be losslessly converted to a DerivStrategy.
mechanismToStrategy :: DerivSpecMechanism -> DerivStrategy
mechanismToStrategy (DerivSpecStock{})    = StockStrategy
mechanismToStrategy (DerivSpecNewtype{})  = NewtypeStrategy
mechanismToStrategy (DerivSpecAnyClass{}) = AnyclassStrategy

instance Outputable DerivSpecMechanism where
  ppr = ppr . mechanismToStrategy

type DerivContext = Maybe ThetaType
   -- Nothing    <=> Vanilla deriving; infer the context of the instance decl
   -- Just theta <=> Standalone deriving: context supplied by programmer

data DerivStatus = CanDerive                 -- Stock class, can derive
                 | DerivableClassError SDoc  -- Stock class, but can't do it
                 | DerivableViaInstance      -- See Note [Deriving any class]
                 | NonDerivableClass SDoc    -- Non-stock class

-- A stock class is one either defined in the Haskell report or for which GHC
-- otherwise knows how to generate code for (possibly requiring the use of a
-- language extension), such as Eq, Ord, Ix, Data, Generic, etc.

-- | A 'PredType' annotated with the origin of the constraint 'CtOrigin',
-- and whether or the constraint deals in types or kinds.
data PredOrigin = PredOrigin PredType CtOrigin TypeOrKind

-- | A list of wanted 'PredOrigin' constraints ('to_wanted_origins') alongside
-- any corresponding given constraints ('to_givens') and locally quantified
-- type variables ('to_tvs').
--
-- In most cases, 'to_givens' will be empty, as most deriving mechanisms (e.g.,
-- stock and newtype deriving) do not require given constraints. The exception
-- is @DeriveAnyClass@, which can involve given constraints. For example,
-- if you tried to derive an instance for the following class using
-- @DeriveAnyClass@:
--
-- @
-- class Foo a where
--   bar :: a -> b -> String
--   default bar :: (Show a, Ix b) => a -> b -> String
--   bar = show
--
--   baz :: Eq a => a -> a -> Bool
--   default baz :: Ord a => a -> a -> Bool
--   baz x y = compare x y == EQ
-- @
--
-- Then it would generate two 'ThetaOrigin's, one for each method:
--
-- @
-- [ ThetaOrigin { to_tvs            = [b]
--               , to_givens         = []
--               , to_wanted_origins = [Show a, Ix b] }
-- , ThetaOrigin { to_tvs            = []
--               , to_givens         = [Eq a]
--               , to_wanted_origins = [Ord a] }
-- ]
-- @
data ThetaOrigin
  = ThetaOrigin { to_tvs            :: [TyVar]
                , to_givens         :: ThetaType
                , to_wanted_origins :: [PredOrigin] }

instance Outputable PredOrigin where
  ppr (PredOrigin ty _ _) = ppr ty -- The origin is not so interesting when debugging

instance Outputable ThetaOrigin where
  ppr (ThetaOrigin { to_tvs = tvs
                   , to_givens = givens
                   , to_wanted_origins = wanted_origins })
    = hang (text "ThetaOrigin")
         2 (vcat [ text "to_tvs            =" <+> ppr tvs
                 , text "to_givens         =" <+> ppr givens
                 , text "to_wanted_origins =" <+> ppr wanted_origins ])

mkPredOrigin :: CtOrigin -> TypeOrKind -> PredType -> PredOrigin
mkPredOrigin origin t_or_k pred = PredOrigin pred origin t_or_k

mkThetaOrigin :: CtOrigin -> TypeOrKind -> [TyVar] -> ThetaType -> ThetaType
              -> ThetaOrigin
mkThetaOrigin origin t_or_k tvs givens
  = ThetaOrigin tvs givens . map (mkPredOrigin origin t_or_k)

-- A common case where the ThetaOrigin only contains wanted constraints, with
-- no givens or locally scoped type variables.
mkThetaOriginFromPreds :: [PredOrigin] -> ThetaOrigin
mkThetaOriginFromPreds = ThetaOrigin [] []

substPredOrigin :: HasCallStack => TCvSubst -> PredOrigin -> PredOrigin
substPredOrigin subst (PredOrigin pred origin t_or_k)
  = PredOrigin (substTy subst pred) origin t_or_k

{-
************************************************************************
*                                                                      *
                Class deriving diagnostics
*                                                                      *
************************************************************************

Only certain blessed classes can be used in a deriving clause (without the
assistance of GeneralizedNewtypeDeriving or DeriveAnyClass). These classes
are listed below in the definition of hasStockDeriving. The sideConditions
function determines the criteria that needs to be met in order for a particular
class to be able to be derived successfully.

A class might be able to be used in a deriving clause if -XDeriveAnyClass
is willing to support it. The canDeriveAnyClass function checks if this is the
case.
-}

hasStockDeriving :: Class
                   -> Maybe (SrcSpan
                             -> TyCon
                             -> [Type]
                             -> TcM (LHsBinds GhcPs, BagDerivStuff))
hasStockDeriving clas
  = assocMaybe gen_list (getUnique clas)
  where
    gen_list :: [(Unique, SrcSpan
                          -> TyCon
                          -> [Type]
                          -> TcM (LHsBinds GhcPs, BagDerivStuff))]
    gen_list = [ (eqClassKey,          simpleM gen_Eq_binds)
               , (ordClassKey,         simpleM gen_Ord_binds)
               , (enumClassKey,        simpleM gen_Enum_binds)
               , (boundedClassKey,     simple gen_Bounded_binds)
               , (ixClassKey,          simpleM gen_Ix_binds)
               , (showClassKey,        with_fix_env gen_Show_binds)
               , (readClassKey,        with_fix_env gen_Read_binds)
               , (dataClassKey,        simpleM gen_Data_binds)
               , (functorClassKey,     simple gen_Functor_binds)
               , (foldableClassKey,    simple gen_Foldable_binds)
               , (traversableClassKey, simple gen_Traversable_binds)
               , (liftClassKey,        simple gen_Lift_binds)
               , (genClassKey,         generic (gen_Generic_binds Gen0))
               , (gen1ClassKey,        generic (gen_Generic_binds Gen1)) ]

    simple gen_fn loc tc _
      = return (gen_fn loc tc)

    simpleM gen_fn loc tc _
      = gen_fn loc tc

    with_fix_env gen_fn loc tc _
      = do { fix_env <- getDataConFixityFun tc
           ; return (gen_fn fix_env loc tc) }

    generic gen_fn _ tc inst_tys
      = do { (binds, faminst) <- gen_fn tc inst_tys
           ; return (binds, unitBag (DerivFamInst faminst)) }

getDataConFixityFun :: TyCon -> TcM (Name -> Fixity)
-- If the TyCon is locally defined, we want the local fixity env;
-- but if it is imported (which happens for standalone deriving)
-- we need to get the fixity env from the interface file
-- c.f. RnEnv.lookupFixity, and Trac #9830
getDataConFixityFun tc
  = do { this_mod <- getModule
       ; if nameIsLocalOrFrom this_mod name
         then do { fix_env <- getFixityEnv
                 ; return (lookupFixity fix_env) }
         else do { iface <- loadInterfaceForName doc name
                            -- Should already be loaded!
                 ; return (mi_fix iface . nameOccName) } }
  where
    name = tyConName tc
    doc = text "Data con fixities for" <+> ppr name

------------------------------------------------------------------
-- Check side conditions that dis-allow derivability for particular classes
-- This is *apart* from the newtype-deriving mechanism
--
-- Here we get the representation tycon in case of family instances as it has
-- the data constructors - but we need to be careful to fall back to the
-- family tycon (with indexes) in error messages.

checkSideConditions :: DynFlags -> DerivContext -> Class -> [TcType]
                    -> TyCon -- tycon
                    -> DerivStatus
checkSideConditions dflags mtheta cls cls_tys rep_tc
  | Just cond <- sideConditions mtheta cls
  = case (cond dflags rep_tc) of
        NotValid err -> DerivableClassError err  -- Class-specific error
        IsValid  | null (filterOutInvisibleTypes (classTyCon cls) cls_tys)
                   -> CanDerive
                   -- All stock derivable classes are unary in the sense that
                   -- there should be not types in cls_tys (i.e., no type args
                   -- other than last). Note that cls_types can contain
                   -- invisible types as well (e.g., for Generic1, which is
                   -- poly-kinded), so make sure those are not counted.
                 | otherwise -> DerivableClassError (classArgsErr cls cls_tys)
                   -- e.g. deriving( Eq s )

  | NotValid err <- canDeriveAnyClass dflags
  = NonDerivableClass err  -- DeriveAnyClass does not work

  | otherwise
  = DerivableViaInstance   -- DeriveAnyClass should work

classArgsErr :: Class -> [Type] -> SDoc
classArgsErr cls cls_tys = quotes (ppr (mkClassPred cls cls_tys)) <+> text "is not a class"

-- Side conditions (whether the datatype must have at least one constructor,
-- required language extensions, etc.) for using GHC's stock deriving
-- mechanism on certain classes (as opposed to classes that require
-- GeneralizedNewtypeDeriving or DeriveAnyClass). Returns Nothing for a
-- class for which stock deriving isn't possible.
sideConditions :: DerivContext -> Class -> Maybe Condition
sideConditions mtheta cls
  | cls_key == eqClassKey          = Just (cond_std `andCond` cond_args cls)
  | cls_key == ordClassKey         = Just (cond_std `andCond` cond_args cls)
  | cls_key == showClassKey        = Just (cond_std `andCond` cond_args cls)
  | cls_key == readClassKey        = Just (cond_std `andCond` cond_args cls)
  | cls_key == enumClassKey        = Just (cond_std `andCond` cond_isEnumeration)
  | cls_key == ixClassKey          = Just (cond_std `andCond` cond_enumOrProduct cls)
  | cls_key == boundedClassKey     = Just (cond_std `andCond` cond_enumOrProduct cls)
  | cls_key == dataClassKey        = Just (checkFlag LangExt.DeriveDataTypeable `andCond`
                                           cond_std `andCond`
                                           cond_args cls)
  | cls_key == functorClassKey     = Just (checkFlag LangExt.DeriveFunctor `andCond`
                                           cond_vanilla `andCond`
                                           cond_functorOK True False)
  | cls_key == foldableClassKey    = Just (checkFlag LangExt.DeriveFoldable `andCond`
                                           cond_vanilla `andCond`
                                           cond_functorOK False True)
                                           -- Functor/Fold/Trav works ok
                                           -- for rank-n types
  | cls_key == traversableClassKey = Just (checkFlag LangExt.DeriveTraversable `andCond`
                                           cond_vanilla `andCond`
                                           cond_functorOK False False)
  | cls_key == genClassKey         = Just (checkFlag LangExt.DeriveGeneric `andCond`
                                           cond_vanilla `andCond`
                                           cond_RepresentableOk)
  | cls_key == gen1ClassKey        = Just (checkFlag LangExt.DeriveGeneric `andCond`
                                           cond_vanilla `andCond`
                                           cond_Representable1Ok)
  | cls_key == liftClassKey        = Just (checkFlag LangExt.DeriveLift `andCond`
                                           cond_vanilla `andCond`
                                           cond_args cls)
  | otherwise                      = Nothing
  where
    cls_key = getUnique cls
    cond_std     = cond_stdOK mtheta False  -- Vanilla data constructors, at least one,
                                            --    and monotype arguments
    cond_vanilla = cond_stdOK mtheta True   -- Vanilla data constructors but
                                            --   allow no data cons or polytype arguments

canDeriveAnyClass :: DynFlags -> Validity
-- IsValid: we can (try to) derive it via an empty instance declaration
-- NotValid s:  we can't, reason s
canDeriveAnyClass dflags
  | not (xopt LangExt.DeriveAnyClass dflags)
  = NotValid (text "Try enabling DeriveAnyClass")
  | otherwise
  = IsValid   -- OK!

type Condition = DynFlags -> TyCon -> Validity
        -- TyCon is the *representation* tycon if the data type is an indexed one
        -- Nothing => OK

orCond :: Condition -> Condition -> Condition
orCond c1 c2 dflags tc
  = case (c1 dflags tc, c2 dflags tc) of
     (IsValid,    _)          -> IsValid    -- c1 succeeds
     (_,          IsValid)    -> IsValid    -- c21 succeeds
     (NotValid x, NotValid y) -> NotValid (x $$ text "  or" $$ y)
                                            -- Both fail

andCond :: Condition -> Condition -> Condition
andCond c1 c2 dflags tc = c1 dflags tc `andValid` c2 dflags tc

cond_stdOK :: DerivContext -- Says whether this is standalone deriving or not;
                           --     if standalone, we just say "yes, go for it"
           -> Bool         -- True <=> permissive: allow higher rank
                           --          args and no data constructors
           -> Condition
cond_stdOK (Just _) _ _ _
  = IsValid     -- Don't check these conservative conditions for
                -- standalone deriving; just generate the code
                -- and let the typechecker handle the result
cond_stdOK Nothing permissive _ rep_tc
  | null data_cons
  , not permissive      = NotValid (no_cons_why rep_tc $$ suggestion)
  | not (null con_whys) = NotValid (vcat con_whys $$ suggestion)
  | otherwise           = IsValid
  where
    suggestion = text "Possible fix: use a standalone deriving declaration instead"
    data_cons  = tyConDataCons rep_tc
    con_whys   = getInvalids (map check_con data_cons)

    check_con :: DataCon -> Validity
    check_con con
      | not (null eq_spec)
      = bad "is a GADT"
      | not (null ex_tvs)
      = bad "has existential type variables in its type"
      | not (null theta)
      = bad "has constraints in its type"
      | not (permissive || all isTauTy (dataConOrigArgTys con))
      = bad "has a higher-rank type"
      | otherwise
      = IsValid
      where
        (_, ex_tvs, eq_spec, theta, _, _) = dataConFullSig con
        bad msg = NotValid (badCon con (text msg))

no_cons_why :: TyCon -> SDoc
no_cons_why rep_tc = quotes (pprSourceTyCon rep_tc) <+>
                     text "must have at least one data constructor"

cond_RepresentableOk :: Condition
cond_RepresentableOk _ tc = canDoGenerics tc

cond_Representable1Ok :: Condition
cond_Representable1Ok _ tc = canDoGenerics1 tc

cond_enumOrProduct :: Class -> Condition
cond_enumOrProduct cls = cond_isEnumeration `orCond`
                         (cond_isProduct `andCond` cond_args cls)

cond_args :: Class -> Condition
-- For some classes (eg Eq, Ord) we allow unlifted arg types
-- by generating specialised code.  For others (eg Data) we don't.
cond_args cls _ tc
  = case bad_args of
      []     -> IsValid
      (ty:_) -> NotValid (hang (text "Don't know how to derive" <+> quotes (ppr cls))
                             2 (text "for type" <+> quotes (ppr ty)))
  where
    bad_args = [ arg_ty | con <- tyConDataCons tc
                        , arg_ty <- dataConOrigArgTys con
                        , isUnliftedType arg_ty
                        , not (ok_ty arg_ty) ]

    cls_key = classKey cls
    ok_ty arg_ty
     | cls_key == eqClassKey   = check_in arg_ty ordOpTbl
     | cls_key == ordClassKey  = check_in arg_ty ordOpTbl
     | cls_key == showClassKey = check_in arg_ty boxConTbl
     | cls_key == liftClassKey = check_in arg_ty litConTbl
     | otherwise               = False    -- Read, Ix etc

    check_in :: Type -> [(Type,a)] -> Bool
    check_in arg_ty tbl = any (eqType arg_ty . fst) tbl


cond_isEnumeration :: Condition
cond_isEnumeration _ rep_tc
  | isEnumerationTyCon rep_tc = IsValid
  | otherwise                 = NotValid why
  where
    why = sep [ quotes (pprSourceTyCon rep_tc) <+>
                  text "must be an enumeration type"
              , text "(an enumeration consists of one or more nullary, non-GADT constructors)" ]
                  -- See Note [Enumeration types] in TyCon

cond_isProduct :: Condition
cond_isProduct _ rep_tc
  | isProductTyCon rep_tc = IsValid
  | otherwise             = NotValid why
  where
    why = quotes (pprSourceTyCon rep_tc) <+>
          text "must have precisely one constructor"

cond_functorOK :: Bool -> Bool -> Condition
-- OK for Functor/Foldable/Traversable class
-- Currently: (a) at least one argument
--            (b) don't use argument contravariantly
--            (c) don't use argument in the wrong place, e.g. data T a = T (X a a)
--            (d) optionally: don't use function types
--            (e) no "stupid context" on data type
cond_functorOK allowFunctions allowExQuantifiedLastTyVar _ rep_tc
  | null tc_tvs
  = NotValid (text "Data type" <+> quotes (ppr rep_tc)
              <+> text "must have some type parameters")

  | not (null bad_stupid_theta)
  = NotValid (text "Data type" <+> quotes (ppr rep_tc)
              <+> text "must not have a class context:" <+> pprTheta bad_stupid_theta)

  | otherwise
  = allValid (map check_con data_cons)
  where
    tc_tvs            = tyConTyVars rep_tc
    Just (_, last_tv) = snocView tc_tvs
    bad_stupid_theta  = filter is_bad (tyConStupidTheta rep_tc)
    is_bad pred       = last_tv `elemVarSet` tyCoVarsOfType pred

    data_cons = tyConDataCons rep_tc
    check_con con = allValid (check_universal con : foldDataConArgs (ft_check con) con)

    check_universal :: DataCon -> Validity
    check_universal con
      | allowExQuantifiedLastTyVar
      = IsValid -- See Note [DeriveFoldable with ExistentialQuantification]
                -- in TcGenFunctor
      | Just tv <- getTyVar_maybe (last (tyConAppArgs (dataConOrigResTy con)))
      , tv `elem` dataConUnivTyVars con
      , not (tv `elemVarSet` tyCoVarsOfTypes (dataConTheta con))
      = IsValid   -- See Note [Check that the type variable is truly universal]
      | otherwise
      = NotValid (badCon con existential)

    ft_check :: DataCon -> FFoldType Validity
    ft_check con = FT { ft_triv = IsValid, ft_var = IsValid
                      , ft_co_var = NotValid (badCon con covariant)
                      , ft_fun = \x y -> if allowFunctions then x `andValid` y
                                                           else NotValid (badCon con functions)
                      , ft_tup = \_ xs  -> allValid xs
                      , ft_ty_app = \_ x   -> x
                      , ft_bad_app = NotValid (badCon con wrong_arg)
                      , ft_forall = \_ x   -> x }

    existential = text "must be truly polymorphic in the last argument of the data type"
    covariant   = text "must not use the type variable in a function argument"
    functions   = text "must not contain function types"
    wrong_arg   = text "must use the type variable only as the last argument of a data type"

checkFlag :: LangExt.Extension -> Condition
checkFlag flag dflags _
  | xopt flag dflags = IsValid
  | otherwise        = NotValid why
  where
    why = text "You need " <> text flag_str
          <+> text "to derive an instance for this class"
    flag_str = case [ flagSpecName f | f <- xFlags , flagSpecFlag f == flag ] of
                 [s]   -> s
                 other -> pprPanic "checkFlag" (ppr other)

std_class_via_coercible :: Class -> Bool
-- These standard classes can be derived for a newtype
-- using the coercible trick *even if no -XGeneralizedNewtypeDeriving
-- because giving so gives the same results as generating the boilerplate
std_class_via_coercible clas
  = classKey clas `elem` [eqClassKey, ordClassKey, ixClassKey, boundedClassKey]
        -- Not Read/Show because they respect the type
        -- Not Enum, because newtypes are never in Enum


non_coercible_class :: Class -> Bool
-- *Never* derive Read, Show, Typeable, Data, Generic, Generic1, Lift
-- by Coercible, even with -XGeneralizedNewtypeDeriving
-- Also, avoid Traversable, as the Coercible-derived instance and the "normal"-derived
-- instance behave differently if there's a non-lawful Applicative out there.
-- Besides, with roles, Coercible-deriving Traversable is ill-roled.
non_coercible_class cls
  = classKey cls `elem` ([ readClassKey, showClassKey, dataClassKey
                         , genClassKey, gen1ClassKey, typeableClassKey
                         , traversableClassKey, liftClassKey ])

badCon :: DataCon -> SDoc -> SDoc
badCon con msg = text "Constructor" <+> quotes (ppr con) <+> msg

------------------------------------------------------------------

newDerivClsInst :: ThetaType -> DerivSpec theta -> TcM ClsInst
newDerivClsInst theta (DS { ds_name = dfun_name, ds_overlap = overlap_mode
                          , ds_tvs = tvs, ds_cls = clas, ds_tys = tys })
  = newClsInst overlap_mode dfun_name tvs theta clas tys

extendLocalInstEnv :: [ClsInst] -> TcM a -> TcM a
-- Add new locally-defined instances; don't bother to check
-- for functional dependency errors -- that'll happen in TcInstDcls
extendLocalInstEnv dfuns thing_inside
 = do { env <- getGblEnv
      ; let  inst_env' = extendInstEnvList (tcg_inst_env env) dfuns
             env'      = env { tcg_inst_env = inst_env' }
      ; setGblEnv env' thing_inside }

{-
Note [Deriving any class]
~~~~~~~~~~~~~~~~~~~~~~~~~
Classic uses of a deriving clause, or a standalone-deriving declaration, are
for:
  * a stock class like Eq or Show, for which GHC knows how to generate
    the instance code
  * a newtype, via the mechanism enabled by GeneralizedNewtypeDeriving

The DeriveAnyClass extension adds a third way to derive instances, based on
empty instance declarations.

The canonical use case is in combination with GHC.Generics and default method
signatures. These allow us to have instance declarations being empty, but still
useful, e.g.

  data T a = ...blah..blah... deriving( Generic )
  instance C a => C (T a)  -- No 'where' clause

where C is some "random" user-defined class.

This boilerplate code can be replaced by the more compact

  data T a = ...blah..blah... deriving( Generic, C )

if DeriveAnyClass is enabled.

This is not restricted to Generics; any class can be derived, simply giving
rise to an empty instance.

Unfortunately, it is not clear how to determine the context (when using a
deriving clause; in standalone deriving, the user provides the context).
GHC uses the same heuristic for figuring out the class context that it uses for
Eq in the case of *-kinded classes, and for Functor in the case of
* -> *-kinded classes. That may not be optimal or even wrong. But in such
cases, standalone deriving can still be used.

Note [Check that the type variable is truly universal]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For Functor and Traversable instances, we must check that the *last argument*
of the type constructor is used truly universally quantified.  Example

   data T a b where
     T1 :: a -> b -> T a b      -- Fine! Vanilla H-98
     T2 :: b -> c -> T a b      -- Fine! Existential c, but we can still map over 'b'
     T3 :: b -> T Int b         -- Fine! Constraint 'a', but 'b' is still polymorphic
     T4 :: Ord b => b -> T a b  -- No!  'b' is constrained
     T5 :: b -> T b b           -- No!  'b' is constrained
     T6 :: T a (b,b)            -- No!  'b' is constrained

Notice that only the first of these constructors is vanilla H-98. We only
need to take care about the last argument (b in this case).  See Trac #8678.
Eg. for T1-T3 we can write

     fmap f (T1 a b) = T1 a (f b)
     fmap f (T2 b c) = T2 (f b) c
     fmap f (T3 x)   = T3 (f x)

We need not perform these checks for Foldable instances, however, since
functions in Foldable can only consume existentially quantified type variables,
rather than produce them (as is the case in Functor and Traversable functions.)
As a result, T can have a derived Foldable instance:

    foldr f z (T1 a b) = f b z
    foldr f z (T2 b c) = f b z
    foldr f z (T3 x)   = f x z
    foldr f z (T4 x)   = f x z
    foldr f z (T5 x)   = f x z
    foldr _ z T6       = z

See Note [DeriveFoldable with ExistentialQuantification] in TcGenFunctor.
-}
