{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section{@Vars@: Variables}
-}

{-# LANGUAGE FlexibleContexts, MultiWayIf, FlexibleInstances, DeriveDataTypeable,
             PatternSynonyms, BangPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- |
-- #name_types#
-- GHC uses several kinds of name internally:
--
-- * 'GHC.Types.Name.Occurrence.OccName': see "GHC.Types.Name.Occurrence#name_types"
--
-- * 'GHC.Types.Name.Reader.RdrName': see "GHC.Types.Name.Reader#name_types"
--
-- * 'GHC.Types.Name.Name': see "GHC.Types.Name#name_types"
--
-- * 'GHC.Types.Id.Id': see "GHC.Types.Id#name_types"
--
-- * 'GHC.Types.Var.Var' is a synonym for the 'GHC.Types.Id.Id' type but it may additionally
--   potentially contain type variables, which have a 'GHC.Core.TyCo.Rep.Kind'
--   rather than a 'GHC.Core.TyCo.Rep.Type' and only contain some extra
--   details during typechecking.
--
--   These 'Var' names may either be global or local, see "GHC.Types.Var#globalvslocal"
--
-- #globalvslocal#
-- Global 'Id's and 'Var's are those that are imported or correspond
--    to a data constructor, primitive operation, or record selectors.
-- Local 'Id's and 'Var's are those bound within an expression
--    (e.g. by a lambda) or at the top level of the module being compiled.

module GHC.Types.Var (
        -- * The main data type and synonyms
        Var, CoVar, Id, NcId, DictId, DFunId, EvVar, EqVar, EvId, IpId, JoinId,
        TyVar, TcTyVar, TypeVar, KindVar, TKVar, TyCoVar,

        -- * In and Out variants
        InVar,  InCoVar,  InId,  InTyVar,
        OutVar, OutCoVar, OutId, OutTyVar,

        -- ** Taking 'Var's apart
        varName, varUnique, varType,
        varMult, varMultMaybe,

        -- ** Modifying 'Var's
        setVarName, setVarUnique, setVarType,
        updateVarType, updateVarTypeM,

        -- ** Constructing, taking apart, modifying 'Id's
        mkGlobalVar, mkLocalVar, mkExportedLocalVar, mkCoVar,
        idInfo, idDetails,
        lazySetIdInfo, setIdDetails, globaliseId,
        setIdExported, setIdNotExported, setIdMult,
        updateIdTypeButNotMult,
        updateIdTypeAndMult, updateIdTypeAndMultM,

        -- ** Predicates
        isId, isTyVar, isTcTyVar,
        isLocalVar, isLocalId, isCoVar, isNonCoVarId, isTyCoVar,
        isGlobalId, isExportedId,
        mustHaveLocalBinding,

        -- * ArgFlags
        ArgFlag(Invisible,Required,Specified,Inferred),
        AnonArgFlag(..), Specificity(..),
        isVisibleArgFlag, isInvisibleArgFlag, isInferredArgFlag,
        sameVis,

        -- * TyVar's
        VarBndr(..), TyCoVarBinder, TyVarBinder, InvisTVBinder, ReqTVBinder,
        binderVar, binderVars, binderArgFlag, binderType,
        mkTyCoVarBinder, mkTyCoVarBinders,
        mkTyVarBinder, mkTyVarBinders,
        isTyVarBinder,
        tyVarSpecToBinder, tyVarSpecToBinders, tyVarReqToBinder, tyVarReqToBinders,
        mapVarBndr, mapVarBndrs, lookupVarBndr,

        -- ** Constructing TyVar's
        mkTyVar, mkTcTyVar,

        -- ** Taking 'TyVar's apart
        tyVarName, tyVarKind, tcTyVarDetails, setTcTyVarDetails,

        -- ** Modifying 'TyVar's
        setTyVarName, setTyVarUnique, setTyVarKind, updateTyVarKind,
        updateTyVarKindM,

        nonDetCmpVar
        ) where

import GHC.Prelude

import {-# SOURCE #-}   GHC.Core.TyCo.Rep( Type, Kind, Mult )
import {-# SOURCE #-}   GHC.Core.TyCo.Ppr( pprKind )
import {-# SOURCE #-}   GHC.Tc.Utils.TcType( TcTyVarDetails, pprTcTyVarDetails, vanillaSkolemTvUnk )
import {-# SOURCE #-}   GHC.Types.Id.Info( IdDetails, IdInfo, coVarDetails, isCoVarDetails,
                                           vanillaIdInfo, pprIdDetails )
import {-# SOURCE #-}   GHC.Builtin.Types ( manyDataConTy )
import GHC.Types.Name hiding (varName)
import GHC.Types.Unique ( Uniquable, Unique, getKey, getUnique
                        , mkUniqueGrimily, nonDetCmpUnique )
import GHC.Utils.Misc
import GHC.Utils.Binary
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain

import Data.Data

{-
************************************************************************
*                                                                      *
                     Synonyms
*                                                                      *
************************************************************************
-- These synonyms are here and not in Id because otherwise we need a very
-- large number of SOURCE imports of "GHC.Types.Id" :-(
-}

-- | Identifier
type Id    = Var       -- A term-level identifier
                       --  predicate: isId

-- | Coercion Variable
type CoVar = Id        -- See Note [Evidence: EvIds and CoVars]
                       --   predicate: isCoVar

-- |
type NcId  = Id        -- A term-level (value) variable that is
                       -- /not/ an (unlifted) coercion
                       --    predicate: isNonCoVarId

-- | Type or kind Variable
type TyVar   = Var     -- Type *or* kind variable (historical)

-- | Type or Kind Variable
type TKVar   = Var     -- Type *or* kind variable (historical)

-- | Type variable that might be a metavariable
type TcTyVar = Var

-- | Type Variable
type TypeVar = Var     -- Definitely a type variable

-- | Kind Variable
type KindVar = Var     -- Definitely a kind variable
                       -- See Note [Kind and type variables]

-- See Note [Evidence: EvIds and CoVars]
-- | Evidence Identifier
type EvId   = Id        -- Term-level evidence: DictId, IpId, or EqVar

-- | Evidence Variable
type EvVar  = EvId      -- ...historical name for EvId

-- | Dictionary Function Identifier
type DFunId = Id        -- A dictionary function

-- | Dictionary Identifier
type DictId = EvId      -- A dictionary variable

-- | Implicit parameter Identifier
type IpId   = EvId      -- A term-level implicit parameter

-- | Equality Variable
type EqVar  = EvId      -- Boxed equality evidence
type JoinId = Id        -- A join variable

-- | Type or Coercion Variable
type TyCoVar = Id       -- Type, *or* coercion variable
                        --   predicate: isTyCoVar


{- Many passes apply a substitution, and it's very handy to have type
   synonyms to remind us whether or not the substitution has been applied -}

type InVar      = Var
type InTyVar    = TyVar
type InCoVar    = CoVar
type InId       = Id
type OutVar     = Var
type OutTyVar   = TyVar
type OutCoVar   = CoVar
type OutId      = Id



{- Note [Evidence: EvIds and CoVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* An EvId (evidence Id) is a term-level evidence variable
  (dictionary, implicit parameter, or equality). Could be boxed or unboxed.

* DictId, IpId, and EqVar are synonyms when we know what kind of
  evidence we are talking about.  For example, an EqVar has type (t1 ~ t2).

* A CoVar is always an un-lifted coercion, of type (t1 ~# t2) or (t1 ~R# t2)

Note [Kind and type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before kind polymorphism, TyVar were used to mean type variables. Now
they are used to mean kind *or* type variables. KindVar is used when we
know for sure that it is a kind variable. In future, we might want to
go over the whole compiler code to use:
   - TKVar   to mean kind or type variables
   - TypeVar to mean         type variables only
   - KindVar to mean kind         variables


************************************************************************
*                                                                      *
\subsection{The main data type declarations}
*                                                                      *
************************************************************************


Every @Var@ has a @Unique@, to uniquify it and for fast comparison, a
@Type@, and an @IdInfo@ (non-essential info about it, e.g.,
strictness).  The essential info about different kinds of @Vars@ is
in its @VarDetails@.
-}

-- | Variable
--
-- Essentially a typed 'Name', that may also contain some additional information
-- about the 'Var' and its use sites.
data Var
  = TyVar {  -- Type and kind variables
             -- see Note [Kind and type variables]
        varName    :: !Name,
        realUnique :: {-# UNPACK #-} !Int,
                                     -- ^ Key for fast comparison
                                     -- Identical to the Unique in the name,
                                     -- cached here for speed
        varType    :: Kind           -- ^ The type or kind of the 'Var' in question
 }

  | TcTyVar {                           -- Used only during type inference
                                        -- Used for kind variables during
                                        -- inference, as well
        varName        :: !Name,
        realUnique     :: {-# UNPACK #-} !Int,
        varType        :: Kind,
        tc_tv_details  :: TcTyVarDetails
  }

  | Id {
        varName    :: !Name,
        realUnique :: {-# UNPACK #-} !Int,
        varType    :: Type,
        varMult    :: Mult,             -- See Note [Multiplicity of let binders]
        idScope    :: IdScope,
        id_details :: IdDetails,        -- Stable, doesn't change
        id_info    :: IdInfo }          -- Unstable, updated by simplifier

-- | Identifier Scope
data IdScope    -- See Note [GlobalId/LocalId]
  = GlobalId
  | LocalId ExportFlag

data ExportFlag   -- See Note [ExportFlag on binders]
  = NotExported   -- ^ Not exported: may be discarded as dead code.
  | Exported      -- ^ Exported: kept alive

{- Note [ExportFlag on binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An ExportFlag of "Exported" on a top-level binder says "keep this
binding alive; do not drop it as dead code".  This transitively
keeps alive all the other top-level bindings that this binding refers
to.  This property is persisted all the way down the pipeline, so that
the binding will be compiled all the way to object code, and its
symbols will appear in the linker symbol table.

However, note that this use of "exported" is quite different to the
export list on a Haskell module.  Setting the ExportFlag on an Id does
/not/ mean that if you import the module (in Haskell source code) you
will see this Id.  Of course, things that appear in the export list
of the source Haskell module do indeed have their ExportFlag set.
But many other things, such as dictionary functions, are kept alive
by having their ExportFlag set, even though they are not exported
in the source-code sense.

We should probably use a different term for ExportFlag, like
KeepAlive.

Note [GlobalId/LocalId]
~~~~~~~~~~~~~~~~~~~~~~~
A GlobalId is
  * always a constant (top-level)
  * imported, or data constructor, or primop, or record selector
  * has a Unique that is globally unique across the whole
    GHC invocation (a single invocation may compile multiple modules)
  * never treated as a candidate by the free-variable finder;
        it's a constant!

A LocalId is
  * bound within an expression (lambda, case, local let(rec))
  * or defined at top level in the module being compiled
  * always treated as a candidate by the free-variable finder

After CoreTidy, top-level LocalIds are turned into GlobalIds

Note [Multiplicity of let binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Core, let-binders' multiplicity is always completely determined by syntax:
a recursive let will always have multiplicity Many (it's a prerequisite for
being recursive), and non-recursive let doesn't have a conventional multiplicity,
instead they act, for the purpose of multiplicity, as an alias for their
right-hand side.

Therefore, the `varMult` field of identifier is only used by binders in lambda
and case expressions. In a let expression the `varMult` field holds an
arbitrary value which will (and must!) be ignored.
-}

instance Outputable Var where
  ppr var = sdocOption sdocSuppressVarKinds $ \supp_var_kinds ->
            getPprDebug $ \debug ->
            getPprStyle $ \sty ->
            let
              ppr_var = case var of
                  (TyVar {})
                     | debug
                     -> brackets (text "tv")

                  (TcTyVar {tc_tv_details = d})
                     | dumpStyle sty || debug
                     -> brackets (pprTcTyVarDetails d)

                  (Id { idScope = s, id_details = d })
                     | debug
                     -> brackets (ppr_id_scope s <> pprIdDetails d)

                  _  -> empty
            in if
               |  debug && (not supp_var_kinds)
                 -> parens (ppr (varName var) <+> ppr (varMultMaybe var)
                                              <+> ppr_var <+>
                          dcolon <+> pprKind (tyVarKind var))
               |  otherwise
                 -> ppr (varName var) <> ppr_var

ppr_id_scope :: IdScope -> SDoc
ppr_id_scope GlobalId              = text "gid"
ppr_id_scope (LocalId Exported)    = text "lidx"
ppr_id_scope (LocalId NotExported) = text "lid"

instance NamedThing Var where
  getName = varName

instance Uniquable Var where
  getUnique = varUnique

instance Eq Var where
    a == b = realUnique a == realUnique b

instance Ord Var where
    a <= b = realUnique a <= realUnique b
    a <  b = realUnique a <  realUnique b
    a >= b = realUnique a >= realUnique b
    a >  b = realUnique a >  realUnique b
    a `compare` b = a `nonDetCmpVar` b

-- | Compare Vars by their Uniques.
-- This is what Ord Var does, provided here to make it explicit at the
-- call-site that it can introduce non-determinism.
-- See Note [Unique Determinism]
nonDetCmpVar :: Var -> Var -> Ordering
nonDetCmpVar a b = varUnique a `nonDetCmpUnique` varUnique b

instance Data Var where
  -- don't traverse?
  toConstr _   = abstractConstr "Var"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Var"

instance HasOccName Var where
  occName = nameOccName . varName

varUnique :: Var -> Unique
varUnique var = mkUniqueGrimily (realUnique var)

varMultMaybe :: Id -> Maybe Mult
varMultMaybe (Id { varMult = mult }) = Just mult
varMultMaybe _ = Nothing

setVarUnique :: Var -> Unique -> Var
setVarUnique var uniq
  = var { realUnique = getKey uniq,
          varName = setNameUnique (varName var) uniq }

setVarName :: Var -> Name -> Var
setVarName var new_name
  = var { realUnique = getKey (getUnique new_name),
          varName = new_name }

setVarType :: Var -> Type -> Var
setVarType id ty = id { varType = ty }

-- | Update a 'Var's type. Does not update the /multiplicity/
-- stored in an 'Id', if any. Because of the possibility for
-- abuse, ASSERTs that there is no multiplicity to update.
updateVarType :: (Type -> Type) -> Var -> Var
updateVarType upd var
  = case var of
      Id { id_details = details } -> assert (isCoVarDetails details) $
                                     result
      _ -> result
  where
    result = var { varType = upd (varType var) }

-- | Update a 'Var's type monadically. Does not update the /multiplicity/
-- stored in an 'Id', if any. Because of the possibility for
-- abuse, ASSERTs that there is no multiplicity to update.
updateVarTypeM :: Monad m => (Type -> m Type) -> Var -> m Var
updateVarTypeM upd var
  = case var of
      Id { id_details = details } -> assert (isCoVarDetails details) $
                                     result
      _ -> result
  where
    result = do { ty' <- upd (varType var)
                ; return (var { varType = ty' }) }

{- *********************************************************************
*                                                                      *
*                   ArgFlag
*                                                                      *
********************************************************************* -}

-- | Argument Flag
--
-- Is something required to appear in source Haskell ('Required'),
-- permitted by request ('Specified') (visible type application), or
-- prohibited entirely from appearing in source Haskell ('Inferred')?
-- See Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility] in "GHC.Core.TyCo.Rep"
data ArgFlag = Invisible Specificity
             | Required
  deriving (Eq, Ord, Data)
  -- (<) on ArgFlag means "is less visible than"

-- | Whether an 'Invisible' argument may appear in source Haskell.
data Specificity = InferredSpec
                   -- ^ the argument may not appear in source Haskell, it is
                   -- only inferred.
                 | SpecifiedSpec
                   -- ^ the argument may appear in source Haskell, but isn't
                   -- required.
  deriving (Eq, Ord, Data)

pattern Inferred, Specified :: ArgFlag
pattern Inferred  = Invisible InferredSpec
pattern Specified = Invisible SpecifiedSpec

{-# COMPLETE Required, Specified, Inferred #-}

-- | Does this 'ArgFlag' classify an argument that is written in Haskell?
isVisibleArgFlag :: ArgFlag -> Bool
isVisibleArgFlag af = not (isInvisibleArgFlag af)

-- | Does this 'ArgFlag' classify an argument that is not written in Haskell?
isInvisibleArgFlag :: ArgFlag -> Bool
isInvisibleArgFlag (Invisible {}) = True
isInvisibleArgFlag Required       = False

isInferredArgFlag :: ArgFlag -> Bool
-- More restrictive than isInvisibleArgFlag
isInferredArgFlag (Invisible InferredSpec) = True
isInferredArgFlag _                        = False

-- | Do these denote the same level of visibility? 'Required'
-- arguments are visible, others are not. So this function
-- equates 'Specified' and 'Inferred'. Used for printing.
sameVis :: ArgFlag -> ArgFlag -> Bool
sameVis Required      Required      = True
sameVis (Invisible _) (Invisible _) = True
sameVis _             _             = False

instance Outputable ArgFlag where
  ppr Required  = text "[req]"
  ppr Specified = text "[spec]"
  ppr Inferred  = text "[infrd]"

instance Binary Specificity where
  put_ bh SpecifiedSpec = putByte bh 0
  put_ bh InferredSpec  = putByte bh 1

  get bh = do
    h <- getByte bh
    case h of
      0 -> return SpecifiedSpec
      _ -> return InferredSpec

instance Binary ArgFlag where
  put_ bh Required  = putByte bh 0
  put_ bh Specified = putByte bh 1
  put_ bh Inferred  = putByte bh 2

  get bh = do
    h <- getByte bh
    case h of
      0 -> return Required
      1 -> return Specified
      _ -> return Inferred

-- | The non-dependent version of 'ArgFlag'.
-- See Note [AnonArgFlag]
-- Appears here partly so that it's together with its friends ArgFlag
-- and ForallVisFlag, but also because it is used in IfaceType, rather
-- early in the compilation chain
data AnonArgFlag
  = VisArg    -- ^ Used for @(->)@: an ordinary non-dependent arrow.
              --   The argument is visible in source code.
  | InvisArg  -- ^ Used for @(=>)@: a non-dependent predicate arrow.
              --   The argument is invisible in source code.
  deriving (Eq, Ord, Data)

instance Outputable AnonArgFlag where
  ppr VisArg   = text "[vis]"
  ppr InvisArg = text "[invis]"

instance Binary AnonArgFlag where
  put_ bh VisArg   = putByte bh 0
  put_ bh InvisArg = putByte bh 1

  get bh = do
    h <- getByte bh
    case h of
      0 -> return VisArg
      _ -> return InvisArg

{- Note [AnonArgFlag]
~~~~~~~~~~~~~~~~~~~~~
AnonArgFlag is used principally in the FunTy constructor of Type.
  FunTy VisArg   t1 t2   means   t1 -> t2
  FunTy InvisArg t1 t2   means   t1 => t2

However, the AnonArgFlag in a FunTy is just redundant, cached
information.  In (FunTy { ft_af = af, ft_arg = t1, ft_res = t2 })
  * if (isPredTy t1 = True)  then af = InvisArg
  * if (isPredTy t1 = False) then af = VisArg
where isPredTy is defined in GHC.Core.Type, and sees if t1's
kind is Constraint.  See GHC.Core.TyCo.Rep
Note [Types for coercions, predicates, and evidence]

GHC.Core.Utils.mkFunctionType :: Mult -> Type -> Type -> Type
uses isPredTy to decide the AnonArgFlag for the FunTy.

The term (Lam b e), and coercion (FunCo co1 co2) don't carry
AnonArgFlags; instead they use mkFunctionType when we want to
get their types; see mkLamType and coercionLKind/RKind resp.
This is just an engineering choice; we could cache here too
if we wanted.

Why bother with all this? After all, we are in Core, where (=>) and
(->) behave the same.  We maintain this distinction throughout Core so
that we can cheaply and conveniently determine
* How to print a type
* How to split up a type: tcSplitSigmaTy
* How to specialise it (over type classes; GHC.Core.Opt.Specialise)

For the specialisation point, consider
(\ (d :: Ord a). blah).  We want to give it type
           (Ord a => blah_ty)
with a fat arrow; that is, using mkInvisFunTy, not mkVisFunTy.
Why?  Because the /specialiser/ treats dictionary arguments specially.
Suppose we do w/w on 'foo', thus (#11272, #6056)
   foo :: Ord a => Int -> blah
   foo a d x = case x of I# x' -> $wfoo @a d x'

   $wfoo :: Ord a => Int# -> blah

Now, at a call we see (foo @Int dOrdInt).  The specialiser will
specialise this to $sfoo, where
   $sfoo :: Int -> blah
   $sfoo x = case x of I# x' -> $wfoo @Int dOrdInt x'

Now we /must/ also specialise $wfoo!  But it wasn't user-written,
and has a type built with mkLamTypes.

Conclusion: the easiest thing is to make mkLamType build
            (c => ty)
when the argument is a predicate type.  See GHC.Core.TyCo.Rep
Note [Types for coercions, predicates, and evidence]
-}

{- *********************************************************************
*                                                                      *
*                   VarBndr, TyCoVarBinder
*                                                                      *
********************************************************************* -}

{- Note [The VarBndr type and its uses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
VarBndr is polymorphic in both var and visibility fields.
Currently there are nine different uses of 'VarBndr':

* Var.TyCoVarBinder = VarBndr TyCoVar ArgFlag
  Binder of a forall-type; see ForAllTy in GHC.Core.TyCo.Rep

* Var.TyVarBinder = VarBndr TyVar ArgFlag
  Subset of TyCoVarBinder when we are sure the binder is a TyVar

* Var.InvisTVBinder = VarBndr TyVar Specificity
  Specialised form of TyVarBinder, when ArgFlag = Invisible s
  See GHC.Core.Type.splitForAllInvisTVBinders

* Var.ReqTVBinder = VarBndr TyVar ()
  Specialised form of TyVarBinder, when ArgFlag = Required
  See GHC.Core.Type.splitForAllReqTVBinders
  This one is barely used

* TyCon.TyConBinder = VarBndr TyVar TyConBndrVis
  Binders of a TyCon; see TyCon in GHC.Core.TyCon

* TyCon.TyConTyCoBinder = VarBndr TyCoVar TyConBndrVis
  Binders of a PromotedDataCon
  See Note [Promoted GADT data constructors] in GHC.Core.TyCon

* IfaceType.IfaceForAllBndr     = VarBndr IfaceBndr ArgFlag
* IfaceType.IfaceForAllSpecBndr = VarBndr IfaceBndr Specificity
* IfaceType.IfaceTyConBinder    = VarBndr IfaceBndr TyConBndrVis
-}

data VarBndr var argf = Bndr var argf
  -- See Note [The VarBndr type and its uses]
  deriving( Data )

-- | Variable Binder
--
-- A 'TyCoVarBinder' is the binder of a ForAllTy
-- It's convenient to define this synonym here rather its natural
-- home in "GHC.Core.TyCo.Rep", because it's used in GHC.Core.DataCon.hs-boot
--
-- A 'TyVarBinder' is a binder with only TyVar
type TyCoVarBinder     = VarBndr TyCoVar ArgFlag
type TyVarBinder       = VarBndr TyVar   ArgFlag
type InvisTVBinder     = VarBndr TyVar   Specificity
type ReqTVBinder       = VarBndr TyVar   ()

tyVarSpecToBinders :: [VarBndr a Specificity] -> [VarBndr a ArgFlag]
tyVarSpecToBinders = map tyVarSpecToBinder

tyVarSpecToBinder :: VarBndr a Specificity -> VarBndr a ArgFlag
tyVarSpecToBinder (Bndr tv vis) = Bndr tv (Invisible vis)

tyVarReqToBinders :: [VarBndr a ()] -> [VarBndr a ArgFlag]
tyVarReqToBinders = map tyVarReqToBinder

tyVarReqToBinder :: VarBndr a () -> VarBndr a ArgFlag
tyVarReqToBinder (Bndr tv _) = Bndr tv Required

binderVar :: VarBndr tv argf -> tv
binderVar (Bndr v _) = v

binderVars :: [VarBndr tv argf] -> [tv]
binderVars tvbs = map binderVar tvbs

binderArgFlag :: VarBndr tv argf -> argf
binderArgFlag (Bndr _ argf) = argf

binderType :: VarBndr TyCoVar argf -> Type
binderType (Bndr tv _) = varType tv

-- | Make a named binder
mkTyCoVarBinder :: vis -> TyCoVar -> VarBndr TyCoVar vis
mkTyCoVarBinder vis var = Bndr var vis

-- | Make a named binder
-- 'var' should be a type variable
mkTyVarBinder :: vis -> TyVar -> VarBndr TyVar vis
mkTyVarBinder vis var
  = assert (isTyVar var) $
    Bndr var vis

-- | Make many named binders
mkTyCoVarBinders :: vis -> [TyCoVar] -> [VarBndr TyCoVar vis]
mkTyCoVarBinders vis = map (mkTyCoVarBinder vis)

-- | Make many named binders
-- Input vars should be type variables
mkTyVarBinders :: vis -> [TyVar] -> [VarBndr TyVar vis]
mkTyVarBinders vis = map (mkTyVarBinder vis)

isTyVarBinder :: TyCoVarBinder -> Bool
isTyVarBinder (Bndr v _) = isTyVar v

mapVarBndr :: (var -> var') -> (VarBndr var flag) -> (VarBndr var' flag)
mapVarBndr f (Bndr v fl) = Bndr (f v) fl

mapVarBndrs :: (var -> var') -> [VarBndr var flag] -> [VarBndr var' flag]
mapVarBndrs f = map (mapVarBndr f)

lookupVarBndr :: Eq var => var -> [VarBndr var flag] -> Maybe flag
lookupVarBndr var bndrs = lookup var zipped_bndrs
  where
    zipped_bndrs = map (\(Bndr v f) -> (v,f)) bndrs

instance Outputable tv => Outputable (VarBndr tv ArgFlag) where
  ppr (Bndr v Required)  = ppr v
  ppr (Bndr v Specified) = char '@' <> ppr v
  ppr (Bndr v Inferred)  = braces (ppr v)

instance Outputable tv => Outputable (VarBndr tv Specificity) where
  ppr = ppr . tyVarSpecToBinder

instance (Binary tv, Binary vis) => Binary (VarBndr tv vis) where
  put_ bh (Bndr tv vis) = do { put_ bh tv; put_ bh vis }

  get bh = do { tv <- get bh; vis <- get bh; return (Bndr tv vis) }

instance NamedThing tv => NamedThing (VarBndr tv flag) where
  getName (Bndr tv _) = getName tv

{-
************************************************************************
*                                                                      *
*                 Type and kind variables                              *
*                                                                      *
************************************************************************
-}

tyVarName :: TyVar -> Name
tyVarName = varName

tyVarKind :: TyVar -> Kind
tyVarKind = varType

setTyVarUnique :: TyVar -> Unique -> TyVar
setTyVarUnique = setVarUnique

setTyVarName :: TyVar -> Name -> TyVar
setTyVarName   = setVarName

setTyVarKind :: TyVar -> Kind -> TyVar
setTyVarKind tv k = tv {varType = k}

updateTyVarKind :: (Kind -> Kind) -> TyVar -> TyVar
updateTyVarKind update tv = tv {varType = update (tyVarKind tv)}

updateTyVarKindM :: (Monad m) => (Kind -> m Kind) -> TyVar -> m TyVar
updateTyVarKindM update tv
  = do { k' <- update (tyVarKind tv)
       ; return $ tv {varType = k'} }

mkTyVar :: Name -> Kind -> TyVar
mkTyVar name kind = TyVar { varName    = name
                          , realUnique = getKey (nameUnique name)
                          , varType  = kind
                          }

mkTcTyVar :: Name -> Kind -> TcTyVarDetails -> TyVar
mkTcTyVar name kind details
  = -- NB: 'kind' may be a coercion kind; cf, 'GHC.Tc.Utils.TcMType.newMetaCoVar'
    TcTyVar {   varName    = name,
                realUnique = getKey (nameUnique name),
                varType  = kind,
                tc_tv_details = details
        }

tcTyVarDetails :: TyVar -> TcTyVarDetails
-- See Note [TcTyVars and TyVars in the typechecker] in GHC.Tc.Utils.TcType
tcTyVarDetails (TcTyVar { tc_tv_details = details }) = details
-- MP: This should never happen, but it does. Future work is to turn this into a panic.
tcTyVarDetails (TyVar {})                            = vanillaSkolemTvUnk
tcTyVarDetails var = pprPanic "tcTyVarDetails" (ppr var <+> dcolon <+> pprKind (tyVarKind var))

setTcTyVarDetails :: TyVar -> TcTyVarDetails -> TyVar
setTcTyVarDetails tv details = tv { tc_tv_details = details }

{-
%************************************************************************
%*                                                                      *
\subsection{Ids}
*                                                                      *
************************************************************************
-}

idInfo :: HasDebugCallStack => Id -> IdInfo
idInfo (Id { id_info = info }) = info
idInfo other                   = pprPanic "idInfo" (ppr other)

idDetails :: Id -> IdDetails
idDetails (Id { id_details = details }) = details
idDetails other                         = pprPanic "idDetails" (ppr other)

-- The next three have a 'Var' suffix even though they always build
-- Ids, because "GHC.Types.Id" uses 'mkGlobalId' etc with different types
mkGlobalVar :: IdDetails -> Name -> Type -> IdInfo -> Id
mkGlobalVar details name ty info
  = mk_id name manyDataConTy ty GlobalId details info
  -- There is no support for linear global variables yet. They would require
  -- being checked at link-time, which can be useful, but is not a priority.

mkLocalVar :: IdDetails -> Name -> Mult -> Type -> IdInfo -> Id
mkLocalVar details name w ty info
  = mk_id name w ty (LocalId NotExported) details  info

mkCoVar :: Name -> Type -> CoVar
-- Coercion variables have no IdInfo
mkCoVar name ty = mk_id name manyDataConTy ty (LocalId NotExported) coVarDetails vanillaIdInfo

-- | Exported 'Var's will not be removed as dead code
mkExportedLocalVar :: IdDetails -> Name -> Type -> IdInfo -> Id
mkExportedLocalVar details name ty info
  = mk_id name manyDataConTy ty (LocalId Exported) details info
  -- There is no support for exporting linear variables. See also [mkGlobalVar]

mk_id :: Name -> Mult -> Type -> IdScope -> IdDetails -> IdInfo -> Id
mk_id name !w ty scope details info
  = Id { varName    = name,
         realUnique = getKey (nameUnique name),
         varMult    = w,
         varType    = ty,
         idScope    = scope,
         id_details = details,
         id_info    = info }

-------------------
lazySetIdInfo :: Id -> IdInfo -> Var
lazySetIdInfo id info = id { id_info = info }

setIdDetails :: Id -> IdDetails -> Id
setIdDetails id details = id { id_details = details }

globaliseId :: Id -> Id
-- ^ If it's a local, make it global
globaliseId id = id { idScope = GlobalId }

setIdExported :: Id -> Id
-- ^ Exports the given local 'Id'. Can also be called on global 'Id's, such as data constructors
-- and class operations, which are born as global 'Id's and automatically exported
setIdExported id@(Id { idScope = LocalId {} }) = id { idScope = LocalId Exported }
setIdExported id@(Id { idScope = GlobalId })   = id
setIdExported tv                               = pprPanic "setIdExported" (ppr tv)

setIdNotExported :: Id -> Id
-- ^ We can only do this to LocalIds
setIdNotExported id = assert (isLocalId id) $
                      id { idScope = LocalId NotExported }

-----------------------
updateIdTypeButNotMult :: (Type -> Type) -> Id -> Id
updateIdTypeButNotMult f id = id { varType = f (varType id) }


updateIdTypeAndMult :: (Type -> Type) -> Id -> Id
updateIdTypeAndMult f id@(Id { varType = ty
                             , varMult = mult })
  = id { varType = ty'
       , varMult = mult' }
  where
    !ty'   = f ty
    !mult' = f mult
updateIdTypeAndMult _ other = pprPanic "updateIdTypeAndMult" (ppr other)

updateIdTypeAndMultM :: Monad m => (Type -> m Type) -> Id -> m Id
updateIdTypeAndMultM f id@(Id { varType = ty
                              , varMult = mult })
  = do { !ty' <- f ty
       ; !mult' <- f mult
       ; return (id { varType = ty', varMult = mult' }) }
updateIdTypeAndMultM _ other = pprPanic "updateIdTypeAndMultM" (ppr other)

setIdMult :: Id -> Mult -> Id
setIdMult id !r | isId id = id { varMult = r }
                | otherwise = pprPanic "setIdMult" (ppr id <+> ppr r)

{-
************************************************************************
*                                                                      *
\subsection{Predicates over variables}
*                                                                      *
************************************************************************
-}

-- | Is this a type-level (i.e., computationally irrelevant, thus erasable)
-- variable? Satisfies @isTyVar = not . isId@.
isTyVar :: Var -> Bool        -- True of both TyVar and TcTyVar
isTyVar (TyVar {})   = True
isTyVar (TcTyVar {}) = True
isTyVar _            = False

isTcTyVar :: Var -> Bool      -- True of TcTyVar only
isTcTyVar (TcTyVar {}) = True
isTcTyVar _            = False

isTyCoVar :: Var -> Bool
isTyCoVar v = isTyVar v || isCoVar v

-- | Is this a value-level (i.e., computationally relevant) 'Id'entifier?
-- Satisfies @isId = not . isTyVar@.
isId :: Var -> Bool
isId (Id {}) = True
isId _       = False

-- | Is this a coercion variable?
-- Satisfies @'isId' v ==> 'isCoVar' v == not ('isNonCoVarId' v)@.
isCoVar :: Var -> Bool
isCoVar (Id { id_details = details }) = isCoVarDetails details
isCoVar _                             = False

-- | Is this a term variable ('Id') that is /not/ a coercion variable?
-- Satisfies @'isId' v ==> 'isCoVar' v == not ('isNonCoVarId' v)@.
isNonCoVarId :: Var -> Bool
isNonCoVarId (Id { id_details = details }) = not (isCoVarDetails details)
isNonCoVarId _                             = False

isLocalId :: Var -> Bool
isLocalId (Id { idScope = LocalId _ }) = True
isLocalId _                            = False

-- | 'isLocalVar' returns @True@ for type variables as well as local 'Id's
-- These are the variables that we need to pay attention to when finding free
-- variables, or doing dependency analysis.
isLocalVar :: Var -> Bool
isLocalVar v = not (isGlobalId v)

isGlobalId :: Var -> Bool
isGlobalId (Id { idScope = GlobalId }) = True
isGlobalId _                           = False

-- | 'mustHaveLocalBinding' returns @True@ of 'Id's and 'TyVar's
-- that must have a binding in this module.  The converse
-- is not quite right: there are some global 'Id's that must have
-- bindings, such as record selectors.  But that doesn't matter,
-- because it's only used for assertions
mustHaveLocalBinding        :: Var -> Bool
mustHaveLocalBinding var = isLocalVar var

-- | 'isExportedIdVar' means \"don't throw this away\"
isExportedId :: Var -> Bool
isExportedId (Id { idScope = GlobalId })        = True
isExportedId (Id { idScope = LocalId Exported}) = True
isExportedId _ = False
