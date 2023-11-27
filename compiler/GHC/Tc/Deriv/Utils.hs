{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

-- | Error-checking and other utilities for @deriving@ clauses or declarations.
module GHC.Tc.Deriv.Utils (
        DerivM, DerivEnv(..),
        DerivSpec(..), pprDerivSpec, setDerivSpecTheta, zonkDerivSpec,
        DerivSpecMechanism(..), derivSpecMechanismToStrategy, isDerivSpecStock,
        isDerivSpecNewtype, isDerivSpecAnyClass,
        isDerivSpecVia, zonkDerivSpecMechanism,
        DerivContext(..), OriginativeDerivStatus(..), StockGenFns(..),
        isStandaloneDeriv, isStandaloneWildcardDeriv,
        askDerivUserTypeCtxt, mkDerivOrigin,
        PredSpec(..), ThetaSpec,
        mkDirectThetaSpec, substPredSpec, captureThetaSpecConstraints,
        checkOriginativeSideConditions, hasStockDeriving,
        std_class_via_coercible, non_coercible_class,
        newDerivClsInst, extendLocalInstEnv
    ) where

import GHC.Prelude

import GHC.Hs.Extension
import GHC.Data.Bag
import GHC.Types.Basic

import GHC.Tc.Utils.Instantiate
import GHC.Tc.Deriv.Generate
import GHC.Tc.Deriv.Functor
import GHC.Tc.Deriv.Generics
import GHC.Tc.Errors.Types
import GHC.Tc.Types.Constraint (WantedConstraints, mkNonCanonical)
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Unify (tcSubTypeSigma)
import GHC.Tc.Zonk.Type

import GHC.Core.Class
import GHC.Core.DataCon
import GHC.Core.FamInstEnv
import GHC.Core.InstEnv
import GHC.Core.TyCon
import GHC.Core.Type

import GHC.Hs
import GHC.Driver.Session
import GHC.Unit.Module (getModule)
import GHC.Unit.Module.Warnings
import GHC.Unit.Module.ModIface (mi_fix)

import GHC.Types.Fixity.Env (lookupFixity)
import GHC.Iface.Load   (loadInterfaceForName)
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Utils.Misc
import GHC.Types.Var.Set

import GHC.Builtin.Names
import GHC.Builtin.Names.TH (liftClassKey)

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Error
import GHC.Utils.Unique (sameUnique)

import Control.Monad.Trans.Reader
import Data.Foldable (traverse_)
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

-- | Return 'InstDeclCtxt' if processing with a standalone @deriving@
-- declaration or 'DerivClauseCtxt' if processing a @deriving@ clause.
askDerivUserTypeCtxt :: DerivM UserTypeCtxt
askDerivUserTypeCtxt = asks (go . denv_ctxt)
  where
    go :: DerivContext -> UserTypeCtxt
    go (SupplyContext {})     = InstDeclCtxt True
    go (InferContext Just{})  = InstDeclCtxt True
    go (InferContext Nothing) = DerivClauseCtxt

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
    -- ^ Universally quantified type variables in the instance. If the
    --   @denv_ctxt@ is 'InferContext', these will be 'TcTyVar' skolems.
    --   If the @denv_ctxt@ is 'SupplyContext', these will be ordinary 'TyVar's.
    --   See @Note [Overlap and deriving]@ in "GHC.Tc.Deriv.Infer".
    --
    --   All type variables that appear in the 'denv_inst_tys', 'denv_ctxt',
    --   'denv_skol_info', and 'denv_strat' should come from 'denv_tvs'.
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
  , denv_skol_info    :: SkolemInfo
    -- ^ The 'SkolemInfo' used to skolemise the @denv_tvs@ in the case where
    --   the 'denv_ctxt' is 'InferContext'.
  , denv_strat        :: Maybe (DerivStrategy GhcTc)
    -- ^ 'Just' if user requests a particular deriving strategy.
    --   Otherwise, 'Nothing'.
  , denv_warn         :: Maybe (WarningTxt GhcRn)
    -- ^ A warning to emit whenever the derived instance is used
  }

instance Outputable DerivEnv where
  ppr (DerivEnv { denv_overlap_mode = overlap_mode
                , denv_tvs          = tvs
                , denv_cls          = cls
                , denv_inst_tys     = inst_tys
                , denv_ctxt         = ctxt
                , denv_skol_info    = skol_info
                , denv_strat        = mb_strat })
    = hang (text "DerivEnv")
         2 (vcat [ text "denv_overlap_mode" <+> ppr overlap_mode
                 , text "denv_tvs"          <+> ppr tvs
                 , text "denv_cls"          <+> ppr cls
                 , text "denv_inst_tys"     <+> ppr inst_tys
                 , text "denv_ctxt"         <+> ppr ctxt
                 , text "denv_skol_info"    <+> ppr skol_info
                 , text "denv_strat"        <+> ppr mb_strat ])

data DerivSpec theta = DS { ds_loc                 :: SrcSpan
                          , ds_name                :: Name         -- DFun name
                          , ds_tvs                 :: [TyVar]
                          , ds_theta               :: theta
                          , ds_cls                 :: Class
                          , ds_tys                 :: [Type]
                          , ds_skol_info           :: SkolemInfo
                          , ds_user_ctxt           :: UserTypeCtxt
                          , ds_overlap             :: Maybe OverlapMode
                          , ds_standalone_wildcard :: Maybe SrcSpan
                              -- See Note [Inferring the instance context]
                              -- in GHC.Tc.Deriv.Infer
                          , ds_mechanism           :: DerivSpecMechanism
                          , ds_warn                :: Maybe (WarningTxt GhcRn)}
        -- This spec implies a dfun declaration of the form
        --       df :: forall tvs. theta => C tys
        -- The Name is the name for the DFun we'll build
        -- The tyvars bind all the variables in the rest of the DerivSpec.
        -- If we are inferring an instance context, the tyvars will be TcTyVar
        -- skolems. After the instance context inference is over, the tyvars
        -- will be zonked to TyVars. See
        -- Note [Overlap and deriving] in GHC.Tc.Deriv.Infer.

        -- the theta is either the given and final theta, in standalone deriving,
        -- or the not-yet-simplified list of constraints together with their origin

        -- The ds_skol_info is the SkolemInfo that was used to skolemise the
        -- TcTyVars (if we are inferring an instance context). The ds_user_ctxt
        -- is the UserTypeCtxt that allows error messages to know if we are in
        -- a deriving clause or a standalone deriving declaration.

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
                   ds_tys = tys, ds_theta = rhs, ds_skol_info = skol_info,
                   ds_standalone_wildcard = wildcard, ds_mechanism = mech })
  = hang (text "DerivSpec")
       2 (vcat [ text "ds_loc                  =" <+> ppr l
               , text "ds_name                 =" <+> ppr n
               , text "ds_tvs                  =" <+> ppr tvs
               , text "ds_cls                  =" <+> ppr c
               , text "ds_tys                  =" <+> ppr tys
               , text "ds_theta                =" <+> ppr rhs
               , text "ds_skol_info            =" <+> ppr skol_info
               , text "ds_standalone_wildcard  =" <+> ppr wildcard
               , text "ds_mechanism            =" <+> ppr mech ])

instance Outputable theta => Outputable (DerivSpec theta) where
  ppr = pprDerivSpec

-- | Set the 'ds_theta' in a 'DerivSpec'.
setDerivSpecTheta :: theta' -> DerivSpec theta -> DerivSpec theta'
setDerivSpecTheta theta ds = ds{ds_theta = theta}

-- | Zonk the 'TcTyVar's in a 'DerivSpec' to 'TyVar's.
-- See @Note [What is zonking?]@ in "GHC.Tc.Zonk.Type".
--
-- This is only used in the final zonking step when inferring
-- the context for a derived instance.
-- See @Note [Overlap and deriving]@ in "GHC.Tc.Deriv.Infer".
zonkDerivSpec :: DerivSpec ThetaType -> ZonkTcM (DerivSpec ThetaType)
zonkDerivSpec ds@(DS { ds_tvs = tvs, ds_theta = theta
                     , ds_tys = tys, ds_mechanism = mechanism
                     }) =
  runZonkBndrT (zonkTyBndrsX tvs) $ \ tvs' -> do
    theta'     <- zonkTcTypesToTypesX theta
    tys'       <- zonkTcTypesToTypesX tys
    mechanism' <- zonkDerivSpecMechanism mechanism
    pure ds{ ds_tvs = tvs', ds_theta = theta'
           , ds_tys = tys', ds_mechanism = mechanism'
           }

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
    , dsm_stock_gen_fns :: StockGenFns
      -- ^ How to generate the instance bindings and associated type family
      -- instances.
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

-- | Zonk the 'TcTyVar's in a 'DerivSpecMechanism' to 'TyVar's.
-- See @Note [What is zonking?]@ in "GHC.Tc.Zonk.Type".
--
-- This is only used in the final zonking step when inferring
-- the context for a derived instance.
-- See @Note [Overlap and deriving]@ in "GHC.Tc.Deriv.Infer".
zonkDerivSpecMechanism :: DerivSpecMechanism -> ZonkTcM DerivSpecMechanism
zonkDerivSpecMechanism mechanism =
  case mechanism of
    DerivSpecStock { dsm_stock_dit     = dit
                   , dsm_stock_gen_fns = gen_fns
                   } -> do
      dit' <- zonkDerivInstTys dit
      pure $ DerivSpecStock { dsm_stock_dit     = dit'
                            , dsm_stock_gen_fns = gen_fns
                            }
    DerivSpecNewtype { dsm_newtype_dit    = dit
                     , dsm_newtype_rep_ty = rep_ty
                     } -> do
      dit'    <- zonkDerivInstTys dit
      rep_ty' <- zonkTcTypeToTypeX rep_ty
      pure $ DerivSpecNewtype { dsm_newtype_dit    = dit'
                              , dsm_newtype_rep_ty = rep_ty'
                              }
    DerivSpecAnyClass ->
      pure DerivSpecAnyClass
    DerivSpecVia { dsm_via_cls_tys = cls_tys
                 , dsm_via_inst_ty = inst_ty
                 , dsm_via_ty      = via_ty
                 } -> do
      cls_tys' <- zonkTcTypesToTypesX cls_tys
      inst_ty' <- zonkTcTypeToTypeX inst_ty
      via_ty'  <- zonkTcTypeToTypeX via_ty
      pure $ DerivSpecVia { dsm_via_cls_tys = cls_tys'
                          , dsm_via_inst_ty = inst_ty'
                          , dsm_via_ty      = via_ty'
                          }

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
  (dit_tc), and its arguments (dit_tc_args). A DerivInstTys value can be seen
  as a more structured representation of the denv_inst_tys field of DerivEnv.

  If dit_tc is an ordinary data type constructor, then
  dit_rep_tc/dit_rep_tc_args are the same as dit_tc/dit_tc_args. If dit_tc is a
  data family type constructor, then dit_rep_tc is the representation type
  constructor for the data family instance, and dit_rep_tc_args are the
  arguments to the representation type constructor in the corresponding
  instance.

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
  = CanDeriveStock StockGenFns -- Stock class, can derive
  | StockClassError !DeriveInstanceErrReason -- Stock class, but can't do it
  | CanDeriveAnyClass         -- See Note [Deriving any class]
  | NonDerivableClass -- Cannot derive with either stock or anyclass

-- | Describes how to generate instance bindings ('stock_gen_binds') and
-- associated type family instances ('stock_gen_fam_insts') for a particular
-- stock-derived instance.
data StockGenFns = StockGenFns
  { stock_gen_binds ::
         SrcSpan -> DerivInstTys
      -> TcM (LHsBinds GhcPs, [LSig GhcPs], Bag AuxBindSpec, [Name])
    -- ^ Describes how to generate instance bindings for a stock-derived
    -- instance.
    --
    -- This function takes two arguments:
    --
    -- 1. 'SrcSpan': the source location where the instance is being derived.
    --    This will eventually be instantiated with the 'ds_loc' field of a
    --    'DerivSpec'.
    --
    -- 2. 'DerivInstTys': information about the argument types to which a
    --    class is applied in a derived instance. This will eventually be
    --    instantiated with the 'dsm_stock_dit' field of a
    --    'DerivSpecMechanism'.
    --
    -- This function returns four things:
    --
    -- 1. @'LHsBinds' 'GhcPs'@: The derived instance's function bindings
    --    (e.g., @compare (T x) (T y) = compare x y@)
    --
    -- 2. @['LSig' 'GhcPs']@: A list of instance specific signatures/pragmas.
    --    Most likely @INLINE@ pragmas for class methods.
    --
    -- 3. @'Bag' 'AuxBindSpec'@: Auxiliary bindings needed to support the
    --    derived instance. As examples, derived 'Eq' and 'Ord' instances
    --    sometimes require top-level @con2tag@ functions.
    --    See @Note [Auxiliary binders]@ in "GHC.Tc.Deriv.Generate".
    --
    -- 4. @['Name']@: A list of Names for which @-Wunused-binds@ should be
    --    suppressed. This is used to suppress unused warnings for record
    --    selectors when deriving 'Read', 'Show', or 'Generic'.
    --    See @Note [Deriving and unused record selectors]@.
  , stock_gen_fam_insts ::
         SrcSpan -> DerivInstTys
      -> TcM [FamInst]
    -- ^ Describes how to generate associated type family instances for a
    -- stock-derived instance. This function takes the same arguments as the
    -- 'stock_gen_binds' function but returns a list of 'FamInst's instead.
    -- Generating type family instances is done separately from
    -- 'stock_gen_binds' since the type family instances must be generated
    -- before the instance bindings can be typechecked. See
    -- @Note [Staging of tcDeriving]@ in "GHC.Tc.Deriv".
  }

-- A stock class is one either defined in the Haskell report or for which GHC
-- otherwise knows how to generate code for (possibly requiring the use of a
-- language extension), such as Eq, Ord, Ix, Data, Generic, etc.)

-- | A 'PredSpec' specifies a constraint to emitted when inferring the
-- instance context for a derived instance in 'GHC.Tc.Deriv.simplifyInfer'.
data PredSpec
  = -- | An ordinary 'PredSpec' that directly stores a 'PredType', which
    -- will be emitted as a wanted constraint in the constraint solving
    -- machinery. This is the simple case, as there are no skolems,
    -- metavariables, or given constraints involved.
    SimplePredSpec
      { sps_pred :: TcPredType
        -- ^ The constraint to emit as a wanted
      , sps_origin :: CtOrigin
        -- ^ The origin of the constraint
      , sps_type_or_kind :: TypeOrKind
        -- ^ Whether the constraint is a type or kind
      }
  | -- | A special 'PredSpec' that is only used by @DeriveAnyClass@. This
    -- will check if @stps_ty_actual@ is a subtype of (i.e., more polymorphic
    -- than) @stps_ty_expected@ in the constraint solving machinery, emitting an
    -- implication constraint as a side effect. For more details on how this
    -- works, see @Note [Gathering and simplifying constraints for DeriveAnyClass]@
    -- in "GHC.Tc.Deriv.Infer".
    SubTypePredSpec
      { stps_ty_actual :: TcSigmaType
        -- ^ The actual type. In the context of @DeriveAnyClass@, this is the
        -- default method type signature.
      , stps_ty_expected :: TcSigmaType
        -- ^ The expected type. In the context of @DeriveAnyClass@, this is the
        -- original method type signature.
      , stps_origin :: CtOrigin
        -- ^ The origin of the constraint
      }

-- | A list of 'PredSpec' constraints to simplify when inferring a
-- derived instance's context. For the @stock@, @newtype@, and @via@ deriving
-- strategies, these will consist of 'SimplePredSpec's, and for
-- @DeriveAnyClass@, these will consist of 'SubTypePredSpec's. Here is an
-- example to illustrate the latter:
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
-- Then it would generate two 'SubTypePredSpec's, one for each method:
--
-- @
-- [ SubTypePredSpec
--     { stps_ty_actual   = forall y. (Show (Quux q), Ix y) => Quux q -> y -> String
--     , stps_ty_expected = forall b.                (Ix b) => Quux q -> b -> String
--     , stps_ty_origin   = DerivClauseCtxt
--     }
-- , SubTypePredSpec
--     { stps_ty_actual   = Ord (Quux q) => Quux q -> Quux q -> Bool
--     , stps_ty_expected = Eq  (Quux q) => Quux q -> Quux q -> Bool
--     , stps_ty_origin   = DerivClauseCtxt
--     }
-- ]
-- @
--
-- (Note that the type variable @q@ is bound by the data type @Quux@, and thus
-- appears free in the 'stps_ty_actual's and 'stps_ty_expected's.)
--
-- See @Note [Gathering and simplifying constraints for DeriveAnyClass]@
-- in "GHC.Tc.Deriv.Infer" for an explanation of how these 'SubTypePredSpec's
-- are used to compute implication constraints.
type ThetaSpec = [PredSpec]

instance Outputable PredSpec where
  ppr (SimplePredSpec{sps_pred = ty}) =
    hang (text "SimplePredSpec")
       2 (vcat [ text "sps_pred" <+> ppr ty ])
  ppr (SubTypePredSpec { stps_ty_actual = ty_actual
                       , stps_ty_expected = ty_expected }) =
    hang (text "SubTypePredSpec")
       2 (vcat [ text "stps_ty_actual"   <+> ppr ty_actual
               , text "stps_ty_expected" <+> ppr ty_expected
               ])

-- | Build a list of 'SimplePredSpec's, using the supplied 'CtOrigin' and
-- 'TypeOrKind' values for each 'PredType'.
mkDirectThetaSpec :: CtOrigin -> TypeOrKind -> ThetaType -> ThetaSpec
mkDirectThetaSpec origin t_or_k =
  map (\p -> SimplePredSpec
               { sps_pred = p
               , sps_origin = origin
               , sps_type_or_kind = t_or_k
               })

substPredSpec :: HasDebugCallStack => Subst -> PredSpec -> PredSpec
substPredSpec subst ps =
  case ps of
    SimplePredSpec { sps_pred = pred
                   , sps_origin = origin
                   , sps_type_or_kind = t_or_k
                   }
      -> SimplePredSpec { sps_pred = substTy subst pred
                        , sps_origin = origin
                        , sps_type_or_kind = t_or_k
                        }

    SubTypePredSpec { stps_ty_actual = ty_actual
                    , stps_ty_expected = ty_expected
                    , stps_origin = origin
                    }
      -> SubTypePredSpec { stps_ty_actual = substTy subst ty_actual
                         , stps_ty_expected = substTy subst ty_expected
                         , stps_origin = origin
                         }

-- | Capture wanted constraints from a 'ThetaSpec'.
captureThetaSpecConstraints ::
     UserTypeCtxt -- ^ Used to inform error messages as to whether
                  -- we are in a @deriving@ clause or a standalone
                  -- @deriving@ declaration
  -> ThetaSpec    -- ^ The specs from which constraints will be created
  -> TcM (TcLevel, WantedConstraints)
captureThetaSpecConstraints user_ctxt theta =
  pushTcLevelM $ mk_wanteds theta
  where
    -- Create the constraints we need to solve. For stock and newtype
    -- deriving, these constraints will be simple wanted constraints
    -- like (C a, Ord b).
    -- But with DeriveAnyClass, we make an implication constraint.
    -- See Note [Gathering and simplifying constraints for DeriveAnyClass]
    -- in GHC.Tc.Deriv.Infer.
    mk_wanteds :: ThetaSpec -> TcM WantedConstraints
    mk_wanteds preds
      = do { (_, wanteds) <- captureConstraints $
                             traverse_ emit_constraints preds
           ; pure wanteds }

    -- Emit the appropriate constraints depending on what sort of
    -- PredSpec we are dealing with.
    emit_constraints :: PredSpec -> TcM ()
    emit_constraints ps =
      case ps of
        -- For constraints like (C a, Ord b), emit the
        -- constraints directly as simple wanted constraints.
        SimplePredSpec { sps_pred = wanted
                       , sps_origin = orig
                       , sps_type_or_kind = t_or_k
                       } -> do
          ev <- newWanted orig (Just t_or_k) wanted
          emitSimple (mkNonCanonical ev)

        -- For DeriveAnyClass, check if ty_actual is a subtype of
        -- ty_expected, which emits an implication constraint as a
        -- side effect. See
        -- Note [Gathering and simplifying constraints for DeriveAnyClass].
        -- in GHC.Tc.Deriv.Infer.
        SubTypePredSpec { stps_ty_actual   = ty_actual
                        , stps_ty_expected = ty_expected
                        , stps_origin      = orig
                        } -> do
          _ <- tcSubTypeSigma orig user_ctxt ty_actual ty_expected
          return ()

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
is willing to support it.
-}

hasStockDeriving
  :: Class -> Maybe StockGenFns
hasStockDeriving clas
  = assocMaybe gen_list (getUnique clas)
  where
    gen_list :: [(Unique, StockGenFns)]
    gen_list =
      [ (eqClassKey,          mk (simple_bindsM gen_Eq_binds) no_fam_insts)
      , (ordClassKey,         mk (simple_bindsM gen_Ord_binds) no_fam_insts)
      , (enumClassKey,        mk (simple_bindsM gen_Enum_binds) no_fam_insts)
      , (boundedClassKey,     mk (simple_binds gen_Bounded_binds) no_fam_insts)
      , (ixClassKey,          mk (simple_bindsM gen_Ix_binds) no_fam_insts)
      , (showClassKey,        mk (read_or_show_binds gen_Show_binds) no_fam_insts)
      , (readClassKey,        mk (read_or_show_binds gen_Read_binds) no_fam_insts)
      , (dataClassKey,        mk (simple_bindsM gen_Data_binds) no_fam_insts)
      , (functorClassKey,     mk (simple_binds gen_Functor_binds) no_fam_insts)
      , (foldableClassKey,    mk (simple_binds gen_Foldable_binds) no_fam_insts)
      , (traversableClassKey, mk (simple_binds gen_Traversable_binds) no_fam_insts)
      , (liftClassKey,        mk (simple_binds gen_Lift_binds) no_fam_insts)
      , (genClassKey,         mk (generic_binds Gen0) (generic_fam_inst Gen0))
      , (gen1ClassKey,        mk (generic_binds Gen1) (generic_fam_inst Gen1))
      ]

    mk gen_binds_fn gen_fam_insts_fn = StockGenFns
      { stock_gen_binds     = gen_binds_fn
      , stock_gen_fam_insts = gen_fam_insts_fn
      }

    simple_binds gen_fn loc dit
      = let (binds, aux_specs) = gen_fn loc dit
        in return (binds, [], aux_specs, [])

    -- Like `simple`, but monadic. The only monadic thing that these functions
    -- do is allocate new Uniques, which are used for generating the names of
    -- auxiliary bindings.
    -- See Note [Auxiliary binders] in GHC.Tc.Deriv.Generate.
    simple_bindsM gen_fn loc dit
      = do { (binds, aux_specs) <- gen_fn loc dit
           ; return (binds, [], aux_specs, []) }

    read_or_show_binds gen_fn loc dit
      = do { let tc = dit_rep_tc dit
           ; fix_env <- getDataConFixityFun tc
           ; let (binds, aux_specs) = gen_fn fix_env loc dit
                 field_names        = all_field_names tc
           ; return (binds, [], aux_specs, field_names) }

    generic_binds gk loc dit
      = do { let tc = dit_rep_tc dit
           ; (binds, sigs) <- gen_Generic_binds gk loc dit
           ; let field_names = all_field_names tc
           ; return (binds, sigs, emptyBag, field_names) }

    generic_fam_inst gk loc dit
      = do { let tc = dit_rep_tc dit
           ; fix_env <- getDataConFixityFun tc
           ; faminst <- gen_Generic_fam_inst gk fix_env loc dit
           ; return [faminst] }

    no_fam_insts _ _ = pure []

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

See also Note [Unused constructors and deriving clauses] in GHC.Tc.Deriv for
another example of a similar trick.
-}

getDataConFixityFun :: TyCon -> TcM (Name -> Fixity)
-- If the TyCon is locally defined, we want the local fixity env;
-- but if it is imported (which happens for standalone deriving)
-- we need to get the fixity env from the interface file
-- c.f. GHC.Rename.Env.lookupFixity, #9830, and #20994
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

checkOriginativeSideConditions :: DerivInstTys -> DerivM OriginativeDerivStatus
checkOriginativeSideConditions dit@(DerivInstTys{dit_cls_tys = cls_tys}) =
  do DerivEnv { denv_cls  = cls
              , denv_ctxt = deriv_ctxt } <- ask
     dflags <- getDynFlags

     if    -- First, check if stock deriving is possible...
        |  Just cond <- stockSideConditions deriv_ctxt cls
        -> case cond dflags dit of
             NotValid err -> pure $ StockClassError err  -- Class-specific error
             IsValid  |  null (filterOutInvisibleTypes (classTyCon cls) cls_tys)
                         -- All stock derivable classes are unary in the sense that
                         -- there should be not types in cls_tys (i.e., no type args
                         -- other than last). Note that cls_types can contain
                         -- invisible types as well (e.g., for Generic1, which is
                         -- poly-kinded), so make sure those are not counted.
                      ,  Just gen_fn <- hasStockDeriving cls
                      -> pure $ CanDeriveStock gen_fn
                      |  otherwise
                      -> pure $ StockClassError $ classArgsErr cls cls_tys
                        -- e.g. deriving( Eq s )

           -- ...if not, try falling back on DeriveAnyClass.
        |  xopt LangExt.DeriveAnyClass dflags
        -> pure CanDeriveAnyClass   -- DeriveAnyClass should work

        |  otherwise
        -> pure NonDerivableClass -- Neither anyclass nor stock work


classArgsErr :: Class -> [Type] -> DeriveInstanceErrReason
classArgsErr cls cls_tys = DerivErrNotAClass (mkClassPred cls cls_tys)

-- Side conditions (whether the datatype must have at least one constructor,
-- required language extensions, etc.) for using GHC's stock deriving
-- mechanism on certain classes (as opposed to classes that require
-- GeneralizedNewtypeDeriving or DeriveAnyClass). Returns Nothing for a
-- class for which stock deriving isn't possible.
stockSideConditions :: DerivContext -> Class -> Maybe Condition
stockSideConditions deriv_ctxt cls
  | sameUnique cls_key eqClassKey          = Just (cond_std `andCond` cond_args cls)
  | sameUnique cls_key ordClassKey         = Just (cond_std `andCond` cond_args cls)
  | sameUnique cls_key showClassKey        = Just (cond_std `andCond` cond_args cls)
  | sameUnique cls_key readClassKey        = Just (cond_std `andCond` cond_args cls)
  | sameUnique cls_key enumClassKey        = Just (cond_std `andCond` cond_isEnumeration)
  | sameUnique cls_key ixClassKey          = Just (cond_std `andCond` cond_enumOrProduct cls)
  | sameUnique cls_key boundedClassKey     = Just (cond_std `andCond` cond_enumOrProduct cls)
  | sameUnique cls_key dataClassKey        = Just (checkFlag LangExt.DeriveDataTypeable `andCond`
                                                   cond_vanilla `andCond`
                                                   cond_args cls)
  | sameUnique cls_key functorClassKey     = Just (checkFlag LangExt.DeriveFunctor `andCond`
                                                   cond_vanilla `andCond`
                                                   cond_functorOK True False)
  | sameUnique cls_key foldableClassKey    = Just (checkFlag LangExt.DeriveFoldable `andCond`
                                                   cond_vanilla `andCond`
                                                   cond_functorOK False True)
                                                   -- Functor/Fold/Trav works ok
                                                   -- for rank-n types
  | sameUnique cls_key traversableClassKey = Just (checkFlag LangExt.DeriveTraversable `andCond`
                                                   cond_vanilla `andCond`
                                                   cond_functorOK False False)
  | sameUnique cls_key genClassKey         = Just (checkFlag LangExt.DeriveGeneric `andCond`
                                                   cond_vanilla `andCond`
                                                   cond_RepresentableOk)
  | sameUnique cls_key gen1ClassKey        = Just (checkFlag LangExt.DeriveGeneric `andCond`
                                                   cond_vanilla `andCond`
                                                   cond_Representable1Ok)
  | sameUnique cls_key liftClassKey        = Just (checkFlag LangExt.DeriveLift `andCond`
                                                   cond_vanilla `andCond`
                                                   cond_args cls)
  | otherwise                        = Nothing
  where
    cls_key = getUnique cls
    cond_std     = cond_stdOK deriv_ctxt False
      -- Vanilla data constructors, at least one, and monotype arguments
    cond_vanilla = cond_stdOK deriv_ctxt True
      -- Vanilla data constructors but allow no data cons or polytype arguments

type Condition
   = DynFlags

  -> DerivInstTys -- ^ Information about the type arguments to the class.

  -> Validity' DeriveInstanceErrReason
     -- ^ 'IsValid' if deriving an instance for this type is
     -- possible. Otherwise, it's @'NotValid' err@, where @err@
     -- explains what went wrong.

andCond :: Condition -> Condition -> Condition
andCond c1 c2 dflags dit
  = c1 dflags dit `andValid` c2 dflags dit

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
cond_stdOK deriv_ctxt permissive dflags
           dit@(DerivInstTys{dit_tc = tc, dit_rep_tc = rep_tc})
  = valid_ADT `andValid` valid_misc
  where
    valid_ADT, valid_misc :: Validity' DeriveInstanceErrReason
    valid_ADT
      | isAlgTyCon tc || isDataFamilyTyCon tc
      = IsValid
      | otherwise
        -- Complain about functions, primitive types, and other tycons that
        -- stock deriving can't handle.
      = NotValid DerivErrLastArgMustBeApp

    valid_misc
      = case deriv_ctxt of
         SupplyContext _ -> IsValid
                -- Don't check these conservative conditions for
                -- standalone deriving; just generate the code
                -- and let the typechecker handle the result
         InferContext wildcard
           | null data_cons -- 1.
           , not permissive
           , not (xopt LangExt.EmptyDataDeriving dflags)
           -> NotValid (no_cons_why rep_tc)
           | not (null con_whys)
           -> NotValid $ DerivErrBadConstructor (Just $ has_wildcard wildcard) con_whys
           | otherwise
           -> IsValid

    has_wildcard wildcard
      = case wildcard of
          Just _  -> YesHasWildcard
          Nothing -> NoHasWildcard
    data_cons  = tyConDataCons rep_tc
    con_whys   = getInvalids (map check_con data_cons)

    check_con :: DataCon -> Validity' DeriveInstanceBadConstructor
    check_con con
      | not (null eq_spec) -- 2.
      = bad DerivErrBadConIsGADT
      | not (null ex_tvs) -- 3.
      = bad DerivErrBadConHasExistentials
      | not (null theta) -- 4.
      = bad DerivErrBadConHasConstraints
      | not (permissive || all isTauTy (derivDataConInstArgTys con dit)) -- 5.
      = bad DerivErrBadConHasHigherRankType
      | otherwise
      = IsValid
      where
        (_, ex_tvs, eq_spec, theta, _, _) = dataConFullSig con
        bad mkErr = NotValid $ mkErr con

no_cons_why :: TyCon -> DeriveInstanceErrReason
no_cons_why = DerivErrNoConstructors

cond_RepresentableOk :: Condition
cond_RepresentableOk _ dit =
  case canDoGenerics dit of
    IsValid -> IsValid
    NotValid generic_errs -> NotValid $ DerivErrGenerics generic_errs

cond_Representable1Ok :: Condition
cond_Representable1Ok _ dit =
  case canDoGenerics1 dit of
    IsValid -> IsValid
    NotValid generic_errs -> NotValid $ DerivErrGenerics generic_errs

cond_enumOrProduct :: Class -> Condition
cond_enumOrProduct cls = cond_isEnumeration `orCond`
                         (cond_isProduct `andCond` cond_args cls)
  where
    orCond :: Condition -> Condition -> Condition
    orCond c1 c2 dflags dit
      = case (c1 dflags dit, c2 dflags dit) of
         (IsValid,    _)          -> IsValid    -- c1 succeeds
         (_,          IsValid)    -> IsValid    -- c21 succeeds
         (NotValid x, NotValid y) -> NotValid $ DerivErrEnumOrProduct x y
                                                -- Both fail


cond_args :: Class -> Condition
-- ^ For some classes (eg 'Eq', 'Ord') we allow unlifted arg types
-- by generating specialised code.  For others (eg 'Data') we don't.
-- For even others (eg 'Lift'), unlifted types aren't even a special
-- consideration!
cond_args cls _ dit@(DerivInstTys{dit_rep_tc = rep_tc})
  = case bad_args of
      []     -> IsValid
      (ty:_) -> NotValid $ DerivErrDunnoHowToDeriveForType ty
  where
    bad_args = [ arg_ty | con <- tyConDataCons rep_tc
                        , arg_ty <- derivDataConInstArgTys con dit
                        , mightBeUnliftedType arg_ty
                        , not (ok_ty arg_ty) ]

    cls_key = classKey cls
    ok_ty arg_ty
     | cls_key == eqClassKey   = check_in arg_ty ordOpTbl
     | cls_key == ordClassKey  = check_in arg_ty ordOpTbl
     | cls_key == showClassKey = check_in arg_ty boxConTbl
     | cls_key == liftClassKey = True     -- Lift is representation-polymorphic
     | otherwise               = False    -- Read, Ix etc

    check_in :: Type -> [(Type,a)] -> Bool
    check_in arg_ty tbl = any (eqType arg_ty . fst) tbl


cond_isEnumeration :: Condition
cond_isEnumeration _ (DerivInstTys{dit_rep_tc = rep_tc})
  | NormalEnum <- tyConEnumSort rep_tc = IsValid
  | otherwise                 = NotValid $ DerivErrMustBeEnumType rep_tc

cond_isProduct :: Condition
cond_isProduct _ (DerivInstTys{dit_rep_tc = rep_tc})
  | Just _ <- tyConSingleDataCon_maybe rep_tc
  = IsValid
  | otherwise
  = NotValid $ DerivErrMustHaveExactlyOneConstructor rep_tc

cond_functorOK :: Bool -> Bool -> Condition
-- OK for Functor/Foldable/Traversable class
-- Currently: (a) at least one argument
--            (b) don't use argument contravariantly
--            (c) don't use argument in the wrong place, e.g. data T a = T (X a a)
--            (d) optionally: don't use function types
--            (e) no "stupid context" on data type
cond_functorOK allowFunctions allowExQuantifiedLastTyVar _
               dit@(DerivInstTys{dit_rep_tc = rep_tc})
  | null tc_tvs
  = NotValid $ DerivErrMustHaveSomeParameters rep_tc

    -- We can't handle stupid contexts that mention the last type argument,
    -- so error out if we encounter one.
    -- See Note [The stupid context] in GHC.Core.DataCon.
  | not (null bad_stupid_theta)
  = NotValid $ DerivErrMustNotHaveClassContext rep_tc bad_stupid_theta

  | otherwise
  = allValid (map check_con data_cons)
  where
    tc_tvs            = tyConTyVars rep_tc
    last_tv           = last tc_tvs
    bad_stupid_theta  = filter is_bad (tyConStupidTheta rep_tc)
    is_bad pred       = last_tv `elemVarSet` exactTyCoVarsOfType pred
      -- See Note [Check that the type variable is truly universal]

    data_cons = tyConDataCons rep_tc
    check_con con = allValid (check_universal con : foldDataConArgs (ft_check con) con dit)

    check_universal :: DataCon -> Validity' DeriveInstanceErrReason
    check_universal con
      | allowExQuantifiedLastTyVar
      = IsValid -- See Note [DeriveFoldable with ExistentialQuantification]
                -- in GHC.Tc.Deriv.Functor
      | Just tv <- getTyVar_maybe (last (tyConAppArgs (dataConOrigResTy con)))
      , tv `elem` dataConUnivTyVars con
      , not (tv `elemVarSet` exactTyCoVarsOfTypes (dataConTheta con))
      = IsValid   -- See Note [Check that the type variable is truly universal]
      | otherwise
      = NotValid $ DerivErrBadConstructor Nothing [DerivErrBadConExistential con]

    ft_check :: DataCon -> FFoldType (Validity' DeriveInstanceErrReason)
    ft_check con = FT { ft_triv = IsValid, ft_var = IsValid
                      , ft_co_var = NotValid $ DerivErrBadConstructor Nothing [DerivErrBadConCovariant con]
                      , ft_fun = \x y -> if allowFunctions then x `andValid` y
                                                           else NotValid $ DerivErrBadConstructor Nothing [DerivErrBadConFunTypes con]
                      , ft_tup = \_ xs  -> allValid xs
                      , ft_ty_app = \_ _ x -> x
                      , ft_bad_app = NotValid $ DerivErrBadConstructor Nothing [DerivErrBadConWrongArg con]
                      , ft_forall = \_ x   -> x }


checkFlag :: LangExt.Extension -> Condition
checkFlag flag dflags _
  | xopt flag dflags = IsValid
  | otherwise        = NotValid why
  where
    why = DerivErrLangExtRequired the_flag
    the_flag = case [ flagSpecFlag f | f <- xFlags , flagSpecFlag f == flag ] of
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

------------------------------------------------------------------

newDerivClsInst :: DerivSpec ThetaType -> TcM ClsInst
newDerivClsInst (DS { ds_name = dfun_name, ds_overlap = overlap_mode
                    , ds_tvs = tvs, ds_theta = theta
                    , ds_cls = clas, ds_tys = tys
                    , ds_warn = warn })
  = newClsInst overlap_mode dfun_name tvs theta clas tys warn

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
