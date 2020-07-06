{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

{-# LANGUAGE TypeFamilies #-}

-- | Error-checking and other utilities for @deriving@ clauses or declarations.
module GHC.Tc.Deriv.Utils (
        DerivM, DerivEnv(..),
        DerivSpec(..), pprDerivSpec, DerivInstTys(..),
        DerivSpecMechanism(..), derivSpecMechanismToStrategy, isDerivSpecStock,
        isDerivSpecNewtype, isDerivSpecAnyClass, isDerivSpecVia,
        DerivContext(..), OriginativeDerivStatus(..),
        isStandaloneDeriv, isStandaloneWildcardDeriv, mkDerivOrigin,
        PredOrigin(..), ThetaOrigin(..), mkPredOrigin,
        mkThetaOrigin, mkThetaOriginFromPreds, substPredOrigin,
        checkOriginativeSideConditions, hasStockDeriving,
        canDeriveAnyClass,
        std_class_via_coercible, non_coercible_class,
        newDerivClsInst, extendLocalInstEnv
    ) where

import GHC.Prelude

import GHC.Data.Bag
import GHC.Types.Basic
import GHC.Core.Class
import GHC.Core.DataCon
import GHC.Driver.Session
import GHC.Utils.Error
import GHC.Types.Fixity.Env (lookupFixity)
import GHC.Hs
import GHC.Tc.Utils.Instantiate
import GHC.Core.InstEnv
import GHC.Iface.Load   (loadInterfaceForName)
import GHC.Unit.Module (getModule)
import GHC.Unit.Module.ModIface (mi_fix)
import GHC.Types.Name
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Builtin.Names
import GHC.Types.SrcLoc
import GHC.Tc.Deriv.Generate
import GHC.Tc.Deriv.Functor
import GHC.Tc.Deriv.Generics
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Builtin.Names.TH (liftClassKey)
import GHC.Core.TyCon
import GHC.Core.Multiplicity
import GHC.Core.TyCo.Ppr (pprSourceTyCon)
import GHC.Core.Type
import GHC.Utils.Misc
import GHC.Types.Var.Set

import Control.Monad.Trans.Reader
import Data.Maybe
import qualified GHC.LanguageExtensions as LangExt
import GHC.Data.List.SetOps (assocMaybe)

-- | To avoid having to manually plumb everything in 'DerivEnv' throughout
-- various functions in "GHC.Tc.Deriv" and "GHC.Tc.Deriv.Infer", we use 'DerivM', which
-- is a simple reader around 'TcRn'.
type DerivM = ReaderT DerivEnv TcRn

-- | Is GHC processing a standalone deriving declaration?
isStandaloneDeriv :: DerivM Bool
isStandaloneDeriv = asks (go . denv_ctxt)
  where
    go :: DerivContext -> Bool
    go (InferContext wildcard) = isJust wildcard
    go (SupplyContext {})      = True

-- | Is GHC processing a standalone deriving declaration with an
-- extra-constraints wildcard as the context?
-- (e.g., @deriving instance _ => Eq (Foo a)@)
isStandaloneWildcardDeriv :: DerivM Bool
isStandaloneWildcardDeriv = asks (go . denv_ctxt)
  where
    go :: DerivContext -> Bool
    go (InferContext wildcard) = isJust wildcard
    go (SupplyContext {})      = False

-- | @'mkDerivOrigin' wc@ returns 'StandAloneDerivOrigin' if @wc@ is 'True',
-- and 'DerivClauseOrigin' if @wc@ is 'False'. Useful for error-reporting.
mkDerivOrigin :: Bool -> CtOrigin
mkDerivOrigin standalone_wildcard
  | standalone_wildcard = StandAloneDerivOrigin
  | otherwise           = DerivClauseOrigin

-- | Contains all of the information known about a derived instance when
-- determining what its @EarlyDerivSpec@ should be.
-- See @Note [DerivEnv and DerivSpecMechanism]@.
data DerivEnv = DerivEnv
  { denv_overlap_mode :: Maybe OverlapMode
    -- ^ Is this an overlapping instance?
  , denv_tvs          :: [TyVar]
    -- ^ Universally quantified type variables in the instance
  , denv_cls          :: Class
    -- ^ Class for which we need to derive an instance
  , denv_inst_tys     :: [Type]
    -- ^ All arguments to 'denv_cls' in the derived instance.
  , denv_ctxt         :: DerivContext
    -- ^ @'SupplyContext' theta@ for standalone deriving (where @theta@ is the
    --   context of the instance).
    --   'InferContext' for @deriving@ clauses, or for standalone deriving that
    --   uses a wildcard constraint.
    --   See @Note [Inferring the instance context]@.
  , denv_strat        :: Maybe (DerivStrategy GhcTc)
    -- ^ 'Just' if user requests a particular deriving strategy.
    --   Otherwise, 'Nothing'.
  }

instance Outputable DerivEnv where
  ppr (DerivEnv { denv_overlap_mode = overlap_mode
                , denv_tvs          = tvs
                , denv_cls          = cls
                , denv_inst_tys     = inst_tys
                , denv_ctxt         = ctxt
                , denv_strat        = mb_strat })
    = hang (text "DerivEnv")
         2 (vcat [ text "denv_overlap_mode" <+> ppr overlap_mode
                 , text "denv_tvs"          <+> ppr tvs
                 , text "denv_cls"          <+> ppr cls
                 , text "denv_inst_tys"     <+> ppr inst_tys
                 , text "denv_ctxt"         <+> ppr ctxt
                 , text "denv_strat"        <+> ppr mb_strat ])

data DerivSpec theta = DS { ds_loc                 :: SrcSpan
                          , ds_name                :: Name         -- DFun name
                          , ds_tvs                 :: [TyVar]
                          , ds_theta               :: theta
                          , ds_cls                 :: Class
                          , ds_tys                 :: [Type]
                          , ds_overlap             :: Maybe OverlapMode
                          , ds_standalone_wildcard :: Maybe SrcSpan
                              -- See Note [Inferring the instance context]
                              -- in GHC.Tc.Deriv.Infer
                          , ds_mechanism           :: DerivSpecMechanism }
        -- This spec implies a dfun declaration of the form
        --       df :: forall tvs. theta => C tys
        -- The Name is the name for the DFun we'll build
        -- The tyvars bind all the variables in the theta

        -- the theta is either the given and final theta, in standalone deriving,
        -- or the not-yet-simplified list of constraints together with their origin

        -- ds_mechanism specifies the means by which GHC derives the instance.
        -- See Note [Deriving strategies] in GHC.Tc.Deriv

{-
Example:

     newtype instance T [a] = MkT (Tree a) deriving( C s )
==>
     axiom T [a] = :RTList a
     axiom :RTList a = Tree a

     DS { ds_tvs = [a,s], ds_cls = C, ds_tys = [s, T [a]]
        , ds_mechanism = DerivSpecNewtype (Tree a) }
-}

pprDerivSpec :: Outputable theta => DerivSpec theta -> SDoc
pprDerivSpec (DS { ds_loc = l, ds_name = n, ds_tvs = tvs, ds_cls = c,
                   ds_tys = tys, ds_theta = rhs,
                   ds_standalone_wildcard = wildcard, ds_mechanism = mech })
  = hang (text "DerivSpec")
       2 (vcat [ text "ds_loc                  =" <+> ppr l
               , text "ds_name                 =" <+> ppr n
               , text "ds_tvs                  =" <+> ppr tvs
               , text "ds_cls                  =" <+> ppr c
               , text "ds_tys                  =" <+> ppr tys
               , text "ds_theta                =" <+> ppr rhs
               , text "ds_standalone_wildcard  =" <+> ppr wildcard
               , text "ds_mechanism            =" <+> ppr mech ])

instance Outputable theta => Outputable (DerivSpec theta) where
  ppr = pprDerivSpec

-- | Information about the arguments to the class in a stock- or
-- newtype-derived instance.
-- See @Note [DerivEnv and DerivSpecMechanism]@.
data DerivInstTys = DerivInstTys
  { dit_cls_tys     :: [Type]
    -- ^ Other arguments to the class except the last
  , dit_tc          :: TyCon
    -- ^ Type constructor for which the instance is requested
    --   (last arguments to the type class)
  , dit_tc_args     :: [Type]
    -- ^ Arguments to the type constructor
  , dit_rep_tc      :: TyCon
    -- ^ The representation tycon for 'dit_tc'
    --   (for data family instances). Otherwise the same as 'dit_tc'.
  , dit_rep_tc_args :: [Type]
    -- ^ The representation types for 'dit_tc_args'
    --   (for data family instances). Otherwise the same as 'dit_tc_args'.
  }

instance Outputable DerivInstTys where
  ppr (DerivInstTys { dit_cls_tys = cls_tys, dit_tc = tc, dit_tc_args = tc_args
                    , dit_rep_tc = rep_tc, dit_rep_tc_args = rep_tc_args })
    = hang (text "DITTyConHead")
         2 (vcat [ text "dit_cls_tys"     <+> ppr cls_tys
                 , text "dit_tc"          <+> ppr tc
                 , text "dit_tc_args"     <+> ppr tc_args
                 , text "dit_rep_tc"      <+> ppr rep_tc
                 , text "dit_rep_tc_args" <+> ppr rep_tc_args ])

-- | What action to take in order to derive a class instance.
-- See @Note [DerivEnv and DerivSpecMechanism]@, as well as
-- @Note [Deriving strategies]@ in "GHC.Tc.Deriv".
data DerivSpecMechanism
    -- | \"Standard\" classes
  = DerivSpecStock
    { dsm_stock_dit    :: DerivInstTys
      -- ^ Information about the arguments to the class in the derived
      -- instance, including what type constructor the last argument is
      -- headed by. See @Note [DerivEnv and DerivSpecMechanism]@.
    , dsm_stock_gen_fn ::
        SrcSpan -> TyCon  -- dit_rep_tc
                -> [Type] -- dit_rep_tc_args
                -> [Type] -- inst_tys
                -> TcM (LHsBinds GhcPs, [LSig GhcPs], BagDerivStuff, [Name])
      -- ^ This function returns four things:
      --
      -- 1. @LHsBinds GhcPs@: The derived instance's function bindings
      --    (e.g., @compare (T x) (T y) = compare x y@)
      --
      -- 2. @[LSig GhcPs]@: A list of instance specific signatures/pragmas.
      --    Most likely INLINE pragmas for class methods.
      --
      -- 3. @BagDerivStuff@: Auxiliary bindings needed to support the derived
      --    instance. As examples, derived 'Generic' instances require
      --    associated type family instances, and derived 'Eq' and 'Ord'
      --    instances require top-level @con2tag@ functions.
      --    See @Note [Auxiliary binders]@ in "GHC.Tc.Deriv.Generate".
      --
      -- 4. @[Name]@: A list of Names for which @-Wunused-binds@ should be
      --    suppressed. This is used to suppress unused warnings for record
      --    selectors when deriving 'Read', 'Show', or 'Generic'.
      --    See @Note [Deriving and unused record selectors]@.
    }

    -- | @GeneralizedNewtypeDeriving@
  | DerivSpecNewtype
    { dsm_newtype_dit    :: DerivInstTys
      -- ^ Information about the arguments to the class in the derived
      -- instance, including what type constructor the last argument is
      -- headed by. See @Note [DerivEnv and DerivSpecMechanism]@.
    , dsm_newtype_rep_ty :: Type
      -- ^ The newtype rep type.
    }

    -- | @DeriveAnyClass@
  | DerivSpecAnyClass

    -- | @DerivingVia@
  | DerivSpecVia
    { dsm_via_cls_tys :: [Type]
      -- ^ All arguments to the class besides the last one.
    , dsm_via_inst_ty :: Type
      -- ^ The last argument to the class.
    , dsm_via_ty      :: Type
      -- ^ The @via@ type
    }

-- | Convert a 'DerivSpecMechanism' to its corresponding 'DerivStrategy'.
derivSpecMechanismToStrategy :: DerivSpecMechanism -> DerivStrategy GhcTc
derivSpecMechanismToStrategy DerivSpecStock{}      = StockStrategy noExtField
derivSpecMechanismToStrategy DerivSpecNewtype{}    = NewtypeStrategy noExtField
derivSpecMechanismToStrategy DerivSpecAnyClass     = AnyclassStrategy noExtField
derivSpecMechanismToStrategy (DerivSpecVia{dsm_via_ty = t}) = ViaStrategy t

isDerivSpecStock, isDerivSpecNewtype, isDerivSpecAnyClass, isDerivSpecVia
  :: DerivSpecMechanism -> Bool
isDerivSpecStock (DerivSpecStock{}) = True
isDerivSpecStock _                  = False

isDerivSpecNewtype (DerivSpecNewtype{}) = True
isDerivSpecNewtype _                    = False

isDerivSpecAnyClass DerivSpecAnyClass = True
isDerivSpecAnyClass _                 = False

isDerivSpecVia (DerivSpecVia{}) = True
isDerivSpecVia _                = False

instance Outputable DerivSpecMechanism where
  ppr (DerivSpecStock{dsm_stock_dit = dit})
    = hang (text "DerivSpecStock")
         2 (vcat [ text "dsm_stock_dit" <+> ppr dit ])
  ppr (DerivSpecNewtype { dsm_newtype_dit = dit, dsm_newtype_rep_ty = rep_ty })
    = hang (text "DerivSpecNewtype")
         2 (vcat [ text "dsm_newtype_dit"    <+> ppr dit
                 , text "dsm_newtype_rep_ty" <+> ppr rep_ty ])
  ppr DerivSpecAnyClass = text "DerivSpecAnyClass"
  ppr (DerivSpecVia { dsm_via_cls_tys = cls_tys, dsm_via_inst_ty = inst_ty
                    , dsm_via_ty = via_ty })
    = hang (text "DerivSpecVia")
         2 (vcat [ text "dsm_via_cls_tys" <+> ppr cls_tys
                 , text "dsm_via_inst_ty" <+> ppr inst_ty
                 , text "dsm_via_ty"      <+> ppr via_ty ])

{-
Note [DerivEnv and DerivSpecMechanism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DerivEnv contains all of the bits and pieces that are common to every
deriving strategy. (See Note [Deriving strategies] in GHC.Tc.Deriv.) Some deriving
strategies impose stricter requirements on the types involved in the derived
instance than others, and these differences are factored out into the
DerivSpecMechanism type. Suppose that the derived instance looks like this:

  instance ... => C arg_1 ... arg_n

Each deriving strategy imposes restrictions on arg_1 through arg_n as follows:

* stock (DerivSpecStock):

  Stock deriving requires that:

  - n must be a positive number. This is checked by
    GHC.Tc.Deriv.expectNonNullaryClsArgs
  - arg_n must be an application of an algebraic type constructor. Here,
    "algebraic type constructor" means:

    + An ordinary data type constructor, or
    + A data family type constructor such that the arguments it is applied to
      give rise to a data family instance.

    This is checked by GHC.Tc.Deriv.expectAlgTyConApp.

  This extra structure is witnessed by the DerivInstTys data type, which stores
  arg_1 through arg_(n-1) (dit_cls_tys), the algebraic type constructor
  (dit_tc), and its arguments (dit_tc_args). If dit_tc is an ordinary data type
  constructor, then dit_rep_tc/dit_rep_tc_args are the same as
  dit_tc/dit_tc_args. If dit_tc is a data family type constructor, then
  dit_rep_tc is the representation type constructor for the data family
  instance, and dit_rep_tc_args are the arguments to the representation type
  constructor in the corresponding instance.

* newtype (DerivSpecNewtype):

  Newtype deriving imposes the same DerivInstTys requirements as stock
  deriving. This is necessary because we need to know what the underlying type
  that the newtype wraps is, and this information can only be learned by
  knowing dit_rep_tc.

* anyclass (DerivSpecAnyclass):

  DeriveAnyClass is the most permissive deriving strategy of all, as it
  essentially imposes no requirements on the derived instance. This is because
  DeriveAnyClass simply derives an empty instance, so it does not need any
  particular knowledge about the types involved. It can do several things
  that stock/newtype deriving cannot do (#13154):

  - n can be 0. That is, one is allowed to anyclass-derive an instance with
    no arguments to the class, such as in this example:

      class C
      deriving anyclass instance C

  - One can derive an instance for a type that is not headed by a type
    constructor, such as in the following example:

      class C (n :: Nat)
      deriving instance C 0
      deriving instance C 1
      ...

  - One can derive an instance for a data family with no data family instances,
    such as in the following example:

      data family Foo a
      class C a
      deriving anyclass instance C (Foo a)

* via (DerivSpecVia):

  Like newtype deriving, DerivingVia requires that n must be a positive number.
  This is because when one derives something like this:

    deriving via Foo instance C Bar

  Then the generated code must specifically mention Bar. However, in
  contrast with newtype deriving, DerivingVia does *not* require Bar to be
  an application of an algebraic type constructor. This is because the
  generated code simply defers to invoking `coerce`, which does not need to
  know anything in particular about Bar (besides that it is representationally
  equal to Foo). This allows DerivingVia to do some things that are not
  possible with newtype deriving, such as deriving instances for data families
  without data instances (#13154):

    data family Foo a
    newtype ByBar a = ByBar a
    class Baz a where ...
    instance Baz (ByBar a) where ...
    deriving via ByBar (Foo a) instance Baz (Foo a)
-}

-- | Whether GHC is processing a @deriving@ clause or a standalone deriving
-- declaration.
data DerivContext
  = InferContext (Maybe SrcSpan) -- ^ @'InferContext mb_wildcard@ is either:
                                 --
                                 -- * A @deriving@ clause (in which case
                                 --   @mb_wildcard@ is 'Nothing').
                                 --
                                 -- * A standalone deriving declaration with
                                 --   an extra-constraints wildcard as the
                                 --   context (in which case @mb_wildcard@ is
                                 --   @'Just' loc@, where @loc@ is the location
                                 --   of the wildcard.
                                 --
                                 -- GHC should infer the context.

  | SupplyContext ThetaType      -- ^ @'SupplyContext' theta@ is a standalone
                                 -- deriving declaration, where @theta@ is the
                                 -- context supplied by the user.

instance Outputable DerivContext where
  ppr (InferContext standalone) = text "InferContext"  <+> ppr standalone
  ppr (SupplyContext theta)     = text "SupplyContext" <+> ppr theta

-- | Records whether a particular class can be derived by way of an
-- /originative/ deriving strategy (i.e., @stock@ or @anyclass@).
--
-- See @Note [Deriving strategies]@ in "GHC.Tc.Deriv".
data OriginativeDerivStatus
  = CanDeriveStock            -- Stock class, can derive
      (SrcSpan -> TyCon -> [Type] -> [Type]
               -> TcM (LHsBinds GhcPs, [LSig GhcPs], BagDerivStuff, [Name]))
  | StockClassError SDoc      -- Stock class, but can't do it
  | CanDeriveAnyClass         -- See Note [Deriving any class]
  | NonDerivableClass SDoc    -- Cannot derive with either stock or anyclass

-- A stock class is one either defined in the Haskell report or for which GHC
-- otherwise knows how to generate code for (possibly requiring the use of a
-- language extension), such as Eq, Ord, Ix, Data, Generic, etc.)

-- | A 'PredType' annotated with the origin of the constraint 'CtOrigin',
-- and whether or the constraint deals in types or kinds.
data PredOrigin = PredOrigin PredType CtOrigin TypeOrKind

-- | A list of wanted 'PredOrigin' constraints ('to_wanted_origins') to
-- simplify when inferring a derived instance's context. These are used in all
-- deriving strategies, but in the particular case of @DeriveAnyClass@, we
-- need extra information. In particular, we need:
--
-- * 'to_anyclass_skols', the list of type variables bound by a class method's
--   regular type signature, which should be rigid.
--
-- * 'to_anyclass_metas', the list of type variables bound by a class method's
--   default type signature. These can be unified as necessary.
--
-- * 'to_anyclass_givens', the list of constraints from a class method's
--   regular type signature, which can be used to help solve constraints
--   in the 'to_wanted_origins'.
--
-- (Note that 'to_wanted_origins' will likely contain type variables from the
-- derived type class or data type, neither of which will appear in
-- 'to_anyclass_skols' or 'to_anyclass_metas'.)
--
-- For all other deriving strategies, it is always the case that
-- 'to_anyclass_skols', 'to_anyclass_metas', and 'to_anyclass_givens' are
-- empty.
--
-- Here is an example to illustrate this:
--
-- @
-- class Foo a where
--   bar :: forall b. Ix b => a -> b -> String
--   default bar :: forall y. (Show a, Ix y) => a -> y -> String
--   bar x y = show x ++ show (range (y, y))
--
--   baz :: Eq a => a -> a -> Bool
--   default baz :: Ord a => a -> a -> Bool
--   baz x y = compare x y == EQ
--
-- data Quux q = Quux deriving anyclass Foo
-- @
--
-- Then it would generate two 'ThetaOrigin's, one for each method:
--
-- @
-- [ ThetaOrigin { to_anyclass_skols  = [b]
--               , to_anyclass_metas  = [y]
--               , to_anyclass_givens = [Ix b]
--               , to_wanted_origins  = [ Show (Quux q), Ix y
--                                      , (Quux q -> b -> String) ~
--                                        (Quux q -> y -> String)
--                                      ] }
-- , ThetaOrigin { to_anyclass_skols  = []
--               , to_anyclass_metas  = []
--               , to_anyclass_givens = [Eq (Quux q)]
--               , to_wanted_origins  = [ Ord (Quux q)
--                                      , (Quux q -> Quux q -> Bool) ~
--                                        (Quux q -> Quux q -> Bool)
--                                      ] }
-- ]
-- @
--
-- (Note that the type variable @q@ is bound by the data type @Quux@, and thus
-- it appears in neither 'to_anyclass_skols' nor 'to_anyclass_metas'.)
--
-- See @Note [Gathering and simplifying constraints for DeriveAnyClass]@
-- in "GHC.Tc.Deriv.Infer" for an explanation of how 'to_wanted_origins' are
-- determined in @DeriveAnyClass@, as well as how 'to_anyclass_skols',
-- 'to_anyclass_metas', and 'to_anyclass_givens' are used.
data ThetaOrigin
  = ThetaOrigin { to_anyclass_skols  :: [TyVar]
                , to_anyclass_metas  :: [TyVar]
                , to_anyclass_givens :: ThetaType
                , to_wanted_origins  :: [PredOrigin] }

instance Outputable PredOrigin where
  ppr (PredOrigin ty _ _) = ppr ty -- The origin is not so interesting when debugging

instance Outputable ThetaOrigin where
  ppr (ThetaOrigin { to_anyclass_skols  = ac_skols
                   , to_anyclass_metas  = ac_metas
                   , to_anyclass_givens = ac_givens
                   , to_wanted_origins  = wanted_origins })
    = hang (text "ThetaOrigin")
         2 (vcat [ text "to_anyclass_skols  =" <+> ppr ac_skols
                 , text "to_anyclass_metas  =" <+> ppr ac_metas
                 , text "to_anyclass_givens =" <+> ppr ac_givens
                 , text "to_wanted_origins  =" <+> ppr wanted_origins ])

mkPredOrigin :: CtOrigin -> TypeOrKind -> PredType -> PredOrigin
mkPredOrigin origin t_or_k pred = PredOrigin pred origin t_or_k

mkThetaOrigin :: CtOrigin -> TypeOrKind
              -> [TyVar] -> [TyVar] -> ThetaType -> ThetaType
              -> ThetaOrigin
mkThetaOrigin origin t_or_k skols metas givens
  = ThetaOrigin skols metas givens . map (mkPredOrigin origin t_or_k)

-- A common case where the ThetaOrigin only contains wanted constraints, with
-- no givens or locally scoped type variables.
mkThetaOriginFromPreds :: [PredOrigin] -> ThetaOrigin
mkThetaOriginFromPreds = ThetaOrigin [] [] []

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
are listed below in the definition of hasStockDeriving. The stockSideConditions
function determines the criteria that needs to be met in order for a particular
stock class to be able to be derived successfully.

A class might be able to be used in a deriving clause if -XDeriveAnyClass
is willing to support it. The canDeriveAnyClass function checks if this is the
case.
-}

hasStockDeriving
  :: Class -> Maybe (SrcSpan
                     -> TyCon
                     -> [Type]
                     -> [Type]
                     -> TcM (LHsBinds GhcPs, [LSig GhcPs], BagDerivStuff, [Name]))
hasStockDeriving clas
  = assocMaybe gen_list (getUnique clas)
  where
    gen_list
      :: [(Unique, SrcSpan
                   -> TyCon
                   -> [Type]
                   -> [Type]
                   -> TcM (LHsBinds GhcPs, [LSig GhcPs], BagDerivStuff, [Name]))]
    gen_list = [ (eqClassKey,          simpleM gen_Eq_binds)
               , (ordClassKey,         simpleM gen_Ord_binds)
               , (enumClassKey,        simpleM gen_Enum_binds)
               , (boundedClassKey,     simple gen_Bounded_binds)
               , (ixClassKey,          simpleM gen_Ix_binds)
               , (showClassKey,        read_or_show gen_Show_binds)
               , (readClassKey,        read_or_show gen_Read_binds)
               , (dataClassKey,        simpleM gen_Data_binds)
               , (functorClassKey,     simple gen_Functor_binds)
               , (foldableClassKey,    simple gen_Foldable_binds)
               , (traversableClassKey, simple gen_Traversable_binds)
               , (liftClassKey,        simple gen_Lift_binds)
               , (genClassKey,         generic (gen_Generic_binds Gen0))
               , (gen1ClassKey,        generic (gen_Generic_binds Gen1)) ]

    simple gen_fn loc tc tc_args _
      = let (binds, deriv_stuff) = gen_fn loc tc tc_args
        in return (binds, [], deriv_stuff, [])

    -- Like `simple`, but monadic. The only monadic thing that these functions
    -- do is allocate new Uniques, which are used for generating the names of
    -- auxiliary bindings.
    -- See Note [Auxiliary binders] in GHC.Tc.Deriv.Generate.
    simpleM gen_fn loc tc tc_args _
      = do { (binds, deriv_stuff) <- gen_fn loc tc tc_args
           ; return (binds, [], deriv_stuff, []) }

    read_or_show gen_fn loc tc tc_args _
      = do { fix_env <- getDataConFixityFun tc
           ; let (binds, deriv_stuff) = gen_fn fix_env loc tc tc_args
                 field_names          = all_field_names tc
           ; return (binds, [], deriv_stuff, field_names) }

    generic gen_fn _ tc _ inst_tys
      = do { (binds, sigs, faminst) <- gen_fn tc inst_tys
           ; let field_names = all_field_names tc
           ; return (binds, sigs, unitBag (DerivFamInst faminst), field_names) }

    -- See Note [Deriving and unused record selectors]
    all_field_names = map flSelector . concatMap dataConFieldLabels
                                     . tyConDataCons

{-
Note [Deriving and unused record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (see #13919):

  module Main (main) where

  data Foo = MkFoo {bar :: String} deriving Show

  main :: IO ()
  main = print (Foo "hello")

Strictly speaking, the record selector `bar` is unused in this module, since
neither `main` nor the derived `Show` instance for `Foo` mention `bar`.
However, the behavior of `main` is affected by the presence of `bar`, since
it will print different output depending on whether `MkFoo` is defined using
record selectors or not. Therefore, we do not to issue a
"Defined but not used: ‘bar’" warning for this module, since removing `bar`
changes the program's behavior. This is the reason behind the [Name] part of
the return type of `hasStockDeriving`—it tracks all of the record selector
`Name`s for which -Wunused-binds should be suppressed.

Currently, the only three stock derived classes that require this are Read,
Show, and Generic, as their derived code all depend on the record selectors
of the derived data type's constructors.

See also Note [Newtype deriving and unused constructors] in GHC.Tc.Deriv for
another example of a similar trick.
-}

getDataConFixityFun :: TyCon -> TcM (Name -> Fixity)
-- If the TyCon is locally defined, we want the local fixity env;
-- but if it is imported (which happens for standalone deriving)
-- we need to get the fixity env from the interface file
-- c.f. GHC.Rename.Env.lookupFixity, and #9830
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
-- Check side conditions that dis-allow derivability for the originative
-- deriving strategies (stock and anyclass).
-- See Note [Deriving strategies] in GHC.Tc.Deriv for an explanation of what
-- "originative" means.
--
-- This is *apart* from the coerce-based strategies, newtype and via.
--
-- Here we get the representation tycon in case of family instances as it has
-- the data constructors - but we need to be careful to fall back to the
-- family tycon (with indexes) in error messages.

checkOriginativeSideConditions
  :: DynFlags -> DerivContext -> Class -> [TcType]
  -> TyCon -> TyCon
  -> OriginativeDerivStatus
checkOriginativeSideConditions dflags deriv_ctxt cls cls_tys tc rep_tc
    -- First, check if stock deriving is possible...
  | Just cond <- stockSideConditions deriv_ctxt cls
  = case (cond dflags tc rep_tc) of
        NotValid err -> StockClassError err  -- Class-specific error
        IsValid  | null (filterOutInvisibleTypes (classTyCon cls) cls_tys)
                   -- All stock derivable classes are unary in the sense that
                   -- there should be not types in cls_tys (i.e., no type args
                   -- other than last). Note that cls_types can contain
                   -- invisible types as well (e.g., for Generic1, which is
                   -- poly-kinded), so make sure those are not counted.
                 , Just gen_fn <- hasStockDeriving cls
                   -> CanDeriveStock gen_fn
                 | otherwise -> StockClassError (classArgsErr cls cls_tys)
                   -- e.g. deriving( Eq s )

    -- ...if not, try falling back on DeriveAnyClass.
  | NotValid err <- canDeriveAnyClass dflags
  = NonDerivableClass err  -- Neither anyclass nor stock work

  | otherwise
  = CanDeriveAnyClass   -- DeriveAnyClass should work

classArgsErr :: Class -> [Type] -> SDoc
classArgsErr cls cls_tys = quotes (ppr (mkClassPred cls cls_tys)) <+> text "is not a class"

-- Side conditions (whether the datatype must have at least one constructor,
-- required language extensions, etc.) for using GHC's stock deriving
-- mechanism on certain classes (as opposed to classes that require
-- GeneralizedNewtypeDeriving or DeriveAnyClass). Returns Nothing for a
-- class for which stock deriving isn't possible.
stockSideConditions :: DerivContext -> Class -> Maybe Condition
stockSideConditions deriv_ctxt cls
  | cls_key == eqClassKey          = Just (cond_std `andCond` cond_args cls)
  | cls_key == ordClassKey         = Just (cond_std `andCond` cond_args cls)
  | cls_key == showClassKey        = Just (cond_std `andCond` cond_args cls)
  | cls_key == readClassKey        = Just (cond_std `andCond` cond_args cls)
  | cls_key == enumClassKey        = Just (cond_std `andCond` cond_isEnumeration)
  | cls_key == ixClassKey          = Just (cond_std `andCond` cond_enumOrProduct cls)
  | cls_key == boundedClassKey     = Just (cond_std `andCond` cond_enumOrProduct cls)
  | cls_key == dataClassKey        = Just (checkFlag LangExt.DeriveDataTypeable `andCond`
                                           cond_vanilla `andCond`
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
    cond_std     = cond_stdOK deriv_ctxt False
      -- Vanilla data constructors, at least one, and monotype arguments
    cond_vanilla = cond_stdOK deriv_ctxt True
      -- Vanilla data constructors but allow no data cons or polytype arguments

canDeriveAnyClass :: DynFlags -> Validity
-- IsValid: we can (try to) derive it via an empty instance declaration
-- NotValid s:  we can't, reason s
canDeriveAnyClass dflags
  | not (xopt LangExt.DeriveAnyClass dflags)
  = NotValid (text "Try enabling DeriveAnyClass")
  | otherwise
  = IsValid   -- OK!

type Condition
   = DynFlags

  -> TyCon    -- ^ The data type's 'TyCon'. For data families, this is the
              -- family 'TyCon'.

  -> TyCon    -- ^ For data families, this is the representation 'TyCon'.
              -- Otherwise, this is the same as the other 'TyCon' argument.

  -> Validity -- ^ 'IsValid' if deriving an instance for this 'TyCon' is
              -- possible. Otherwise, it's @'NotValid' err@, where @err@
              -- explains what went wrong.

orCond :: Condition -> Condition -> Condition
orCond c1 c2 dflags tc rep_tc
  = case (c1 dflags tc rep_tc, c2 dflags tc rep_tc) of
     (IsValid,    _)          -> IsValid    -- c1 succeeds
     (_,          IsValid)    -> IsValid    -- c21 succeeds
     (NotValid x, NotValid y) -> NotValid (x $$ text "  or" $$ y)
                                            -- Both fail

andCond :: Condition -> Condition -> Condition
andCond c1 c2 dflags tc rep_tc
  = c1 dflags tc rep_tc `andValid` c2 dflags tc rep_tc

-- | Some common validity checks shared among stock derivable classes. One
-- check that absolutely must hold is that if an instance @C (T a)@ is being
-- derived, then @T@ must be a tycon for a data type or a newtype. The
-- remaining checks are only performed if using a @deriving@ clause (i.e.,
-- they're ignored if using @StandaloneDeriving@):
--
-- 1. The data type must have at least one constructor (this check is ignored
--    if using @EmptyDataDeriving@).
--
-- 2. The data type cannot have any GADT constructors.
--
-- 3. The data type cannot have any constructors with existentially quantified
--    type variables.
--
-- 4. The data type cannot have a context (e.g., @data Foo a = Eq a => MkFoo@).
--
-- 5. The data type cannot have fields with higher-rank types.
cond_stdOK
  :: DerivContext -- ^ 'SupplyContext' if this is standalone deriving with a
                  -- user-supplied context, 'InferContext' if not.
                  -- If it is the former, we relax some of the validity checks
                  -- we would otherwise perform (i.e., "just go for it").

  -> Bool         -- ^ 'True' <=> allow higher rank arguments and empty data
                  -- types (with no data constructors) even in the absence of
                  -- the -XEmptyDataDeriving extension.

  -> Condition
cond_stdOK deriv_ctxt permissive dflags tc rep_tc
  = valid_ADT `andValid` valid_misc
  where
    valid_ADT, valid_misc :: Validity
    valid_ADT
      | isAlgTyCon tc || isDataFamilyTyCon tc
      = IsValid
      | otherwise
        -- Complain about functions, primitive types, and other tycons that
        -- stock deriving can't handle.
      = NotValid $ text "The last argument of the instance must be a"
               <+> text "data or newtype application"

    valid_misc
      = case deriv_ctxt of
         SupplyContext _ -> IsValid
                -- Don't check these conservative conditions for
                -- standalone deriving; just generate the code
                -- and let the typechecker handle the result
         InferContext wildcard
           | null data_cons -- 1.
           , not permissive
           -> checkFlag LangExt.EmptyDataDeriving dflags tc rep_tc `orValid`
              NotValid (no_cons_why rep_tc $$ empty_data_suggestion)
           | not (null con_whys)
           -> NotValid (vcat con_whys $$ possible_fix_suggestion wildcard)
           | otherwise
           -> IsValid

    empty_data_suggestion =
      text "Use EmptyDataDeriving to enable deriving for empty data types"
    possible_fix_suggestion wildcard
      = case wildcard of
          Just _ ->
            text "Possible fix: fill in the wildcard constraint yourself"
          Nothing ->
            text "Possible fix: use a standalone deriving declaration instead"
    data_cons  = tyConDataCons rep_tc
    con_whys   = getInvalids (map check_con data_cons)

    check_con :: DataCon -> Validity
    check_con con
      | not (null eq_spec) -- 2.
      = bad "is a GADT"
      | not (null ex_tvs) -- 3.
      = bad "has existential type variables in its type"
      | not (null theta) -- 4.
      = bad "has constraints in its type"
      | not (permissive || all isTauTy (map scaledThing $ dataConOrigArgTys con)) -- 5.
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
cond_RepresentableOk _ _ rep_tc = canDoGenerics rep_tc

cond_Representable1Ok :: Condition
cond_Representable1Ok _ _ rep_tc = canDoGenerics1 rep_tc

cond_enumOrProduct :: Class -> Condition
cond_enumOrProduct cls = cond_isEnumeration `orCond`
                         (cond_isProduct `andCond` cond_args cls)

cond_args :: Class -> Condition
-- ^ For some classes (eg 'Eq', 'Ord') we allow unlifted arg types
-- by generating specialised code.  For others (eg 'Data') we don't.
-- For even others (eg 'Lift'), unlifted types aren't even a special
-- consideration!
cond_args cls _ _ rep_tc
  = case bad_args of
      []     -> IsValid
      (ty:_) -> NotValid (hang (text "Don't know how to derive" <+> quotes (ppr cls))
                             2 (text "for type" <+> quotes (ppr ty)))
  where
    bad_args = [ arg_ty | con <- tyConDataCons rep_tc
                        , Scaled _ arg_ty <- dataConOrigArgTys con
                        , isLiftedType_maybe arg_ty /= Just True
                        , not (ok_ty arg_ty) ]

    cls_key = classKey cls
    ok_ty arg_ty
     | cls_key == eqClassKey   = check_in arg_ty ordOpTbl
     | cls_key == ordClassKey  = check_in arg_ty ordOpTbl
     | cls_key == showClassKey = check_in arg_ty boxConTbl
     | cls_key == liftClassKey = True     -- Lift is levity-polymorphic
     | otherwise               = False    -- Read, Ix etc

    check_in :: Type -> [(Type,a)] -> Bool
    check_in arg_ty tbl = any (eqType arg_ty . fst) tbl


cond_isEnumeration :: Condition
cond_isEnumeration _ _ rep_tc
  | isEnumerationTyCon rep_tc = IsValid
  | otherwise                 = NotValid why
  where
    why = sep [ quotes (pprSourceTyCon rep_tc) <+>
                  text "must be an enumeration type"
              , text "(an enumeration consists of one or more nullary, non-GADT constructors)" ]
                  -- See Note [Enumeration types] in GHC.Core.TyCon

cond_isProduct :: Condition
cond_isProduct _ _ rep_tc
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
cond_functorOK allowFunctions allowExQuantifiedLastTyVar _ _ rep_tc
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
    last_tv           = last tc_tvs
    bad_stupid_theta  = filter is_bad (tyConStupidTheta rep_tc)
    is_bad pred       = last_tv `elemVarSet` exactTyCoVarsOfType pred
      -- See Note [Check that the type variable is truly universal]

    data_cons = tyConDataCons rep_tc
    check_con con = allValid (check_universal con : foldDataConArgs (ft_check con) con)

    check_universal :: DataCon -> Validity
    check_universal con
      | allowExQuantifiedLastTyVar
      = IsValid -- See Note [DeriveFoldable with ExistentialQuantification]
                -- in GHC.Tc.Deriv.Functor
      | Just tv <- getTyVar_maybe (last (tyConAppArgs (dataConOrigResTy con)))
      , tv `elem` dataConUnivTyVars con
      , not (tv `elemVarSet` exactTyCoVarsOfTypes (dataConTheta con))
      = IsValid   -- See Note [Check that the type variable is truly universal]
      | otherwise
      = NotValid (badCon con existential)

    ft_check :: DataCon -> FFoldType Validity
    ft_check con = FT { ft_triv = IsValid, ft_var = IsValid
                      , ft_co_var = NotValid (badCon con covariant)
                      , ft_fun = \x y -> if allowFunctions then x `andValid` y
                                                           else NotValid (badCon con functions)
                      , ft_tup = \_ xs  -> allValid xs
                      , ft_ty_app = \_ _ x -> x
                      , ft_bad_app = NotValid (badCon con wrong_arg)
                      , ft_forall = \_ x   -> x }

    existential = text "must be truly polymorphic in the last argument of the data type"
    covariant   = text "must not use the type variable in a function argument"
    functions   = text "must not contain function types"
    wrong_arg   = text "must use the type variable only as the last argument of a data type"

checkFlag :: LangExt.Extension -> Condition
checkFlag flag dflags _ _
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
-- for functional dependency errors -- that'll happen in GHC.Tc.TyCl.Instance
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

See Note [Gathering and simplifying constraints for DeriveAnyClass] in
GHC.Tc.Deriv.Infer for an explanation hof how the instance context is inferred for
DeriveAnyClass.

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
need to take care about the last argument (b in this case).  See #8678.
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

See Note [DeriveFoldable with ExistentialQuantification] in GHC.Tc.Deriv.Functor.

For Functor and Traversable, we must take care not to let type synonyms
unfairly reject a type for not being truly universally quantified. An
example of this is:

    type C (a :: Constraint) b = a
    data T a b = C (Show a) b => MkT b

Here, the existential context (C (Show a) b) does technically mention the last
type variable b. But this is OK, because expanding the type synonym C would give
us the context (Show a), which doesn't mention b. Therefore, we must make sure
to expand type synonyms before performing this check. Not doing so led to #13813.
-}
