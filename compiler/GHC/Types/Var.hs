{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section{@Vars@: Variables}
-}

{-# LANGUAGE MultiWayIf, PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE DeriveFunctor #-}

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
        InVar,  InCoVar,  InId,  InTyVar,  InTyCoVar,
        OutVar, OutCoVar, OutId, OutTyVar, OutTyCoVar,

        -- ** Taking 'Var's apart
        varName, varUnique, varType,
        varMultMaybe, idMult,

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
        isLocalVar, isLocalId, isLocalId_maybe, isCoVar, isNonCoVarId, isTyCoVar,
        isGlobalId, isExportedId,
        mustHaveLocalBinding,

        -- * ForAllTyFlags
        ForAllTyFlag(Invisible,Required,Specified,Inferred),
        Specificity(..),
        isVisibleForAllTyFlag, isInvisibleForAllTyFlag, isInferredForAllTyFlag,
        isSpecifiedForAllTyFlag,
        coreTyLamForAllTyFlag,

        -- * FunTyFlag
        FunTyFlag(..), isVisibleFunArg, isInvisibleFunArg, isFUNArg,
        mkFunTyFlag, visArg, invisArg,
        visArgTypeLike, visArgConstraintLike,
        invisArgTypeLike, invisArgConstraintLike,
        funTyFlagArgTypeOrConstraint, funTyFlagResultTypeOrConstraint,
        TypeOrConstraint(..),  -- Re-export this: it's an argument of FunTyFlag

        -- * PiTyBinder
        PiTyBinder(..), PiTyVarBinder,
        isInvisiblePiTyBinder, isInvisibleAnonPiTyBinder,
        isVisiblePiTyBinder,
        isTyBinder, isNamedPiTyBinder, isAnonPiTyBinder,
        namedPiTyBinder_maybe, anonPiTyBinderType_maybe, piTyBinderType,

        -- * TyVar's
        VarBndr(..), ForAllTyBinder, TyVarBinder,
        InvisTyBinder, InvisTVBinder, ReqTyBinder, ReqTVBinder,
        binderVar, binderVars, binderFlag, binderFlags, binderType,
        mkForAllTyBinder, mkForAllTyBinders,
        mkTyVarBinder, mkTyVarBinders,
        isVisibleForAllTyBinder, isInvisibleForAllTyBinder, isTyVarBinder,
        tyVarSpecToBinder, tyVarSpecToBinders, tyVarReqToBinder, tyVarReqToBinders,
        mapVarBndr, mapVarBndrs,

        -- ** ExportFlag
        ExportFlag(..),

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

import {-# SOURCE #-}   GHC.Core.TyCo.Rep( Type, Kind, Mult, Scaled, scaledThing )
import {-# SOURCE #-}   GHC.Core.TyCo.Ppr( pprKind )
import {-# SOURCE #-}   GHC.Tc.Utils.TcType( TcTyVarDetails, pprTcTyVarDetails, vanillaSkolemTvUnk )
import {-# SOURCE #-}   GHC.Types.Id.Info( IdDetails, IdInfo, coVarDetails, isCoVarDetails,
                                           vanillaIdInfo, pprIdDetails )
import {-# SOURCE #-}   GHC.Builtin.Types ( manyDataConTy )
import GHC.Types.Name hiding (varName)
import GHC.Types.Unique ( Uniquable, Unique, getKey, getUnique
                        , nonDetCmpUnique )
import GHC.Types.Basic( TypeOrConstraint(..) )
import GHC.Utils.Misc
import GHC.Utils.Binary
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Hs.Specificity ()
import Language.Haskell.Syntax.Specificity
import Control.DeepSeq

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
type InTyCoVar  = TyCoVar
type InId       = Id
type OutVar     = Var
type OutTyVar   = TyVar
type OutCoVar   = CoVar
type OutTyCoVar = TyCoVar
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
        realUnique :: {-# UNPACK #-} !Unique,
                                     -- ^ Key for fast comparison
                                     -- Identical to the Unique in the name,
                                     -- cached here for speed
        varType    :: Kind           -- ^ The type or kind of the 'Var' in question
 }

  | TcTyVar {                           -- Used only during type inference
                                        -- Used for kind variables during
                                        -- inference, as well
        varName        :: !Name,
        realUnique     :: {-# UNPACK #-} !Unique,
        varType        :: Kind,
        tc_tv_details  :: TcTyVarDetails
  }

  | Id {
        varName    :: !Name,
        realUnique :: {-# UNPACK #-} !Unique,
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

In the output of CoreTidy, top level Ids are all GlobalIds, which are then
serialised into interface files. Do note however that CorePrep may introduce new
LocalIds for local floats (even at the top level). These will be visible in STG
and end up in generated code.

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
  ppr var = docWithStyle ppr_code ppr_normal
    where
      -- don't display debug info with Code style (#25255)
      ppr_code = ppr (varName var)
      ppr_normal sty = sdocOption sdocSuppressVarKinds $ \supp_var_kinds ->
            getPprDebug $ \debug ->
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
    a <= b = getKey (realUnique a) <= getKey (realUnique b)
    a <  b = getKey (realUnique a) <  getKey (realUnique b)
    a >= b = getKey (realUnique a) >= getKey (realUnique b)
    a >  b = getKey (realUnique a) >  getKey (realUnique b)
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
varUnique var = realUnique var

varMultMaybe :: Id -> Maybe Mult
varMultMaybe (Id { varMult = mult }) = Just mult
varMultMaybe _ = Nothing

idMult :: HasDebugCallStack => Id -> Mult
idMult (Id { varMult = mult }) = mult
idMult non_id                  = pprPanic "idMult" (ppr non_id)

setVarUnique :: Var -> Unique -> Var
setVarUnique var uniq
  = var { realUnique = uniq,
          varName = setNameUnique (varName var) uniq }

setVarName :: Var -> Name -> Var
setVarName var new_name
  = var { realUnique = getUnique new_name,
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
*                   FunTyFlag
*                                                                      *
********************************************************************* -}

-- | The non-dependent version of 'ForAllTyFlag'.
-- See Note [FunTyFlag]
-- Appears here partly so that it's together with its friends ForAllTyFlag
-- and ForallVisFlag, but also because it is used in IfaceType, rather
-- early in the compilation chain
data FunTyFlag
  = FTF_T_T           -- (->)  Type -> Type
  | FTF_T_C           -- (-=>) Type -> Constraint
  | FTF_C_T           -- (=>)  Constraint -> Type
  | FTF_C_C           -- (==>) Constraint -> Constraint
  deriving (Eq, Ord, Data)

instance Outputable FunTyFlag where
  ppr FTF_T_T  = text "[->]"
  ppr FTF_T_C  = text "[-=>]"
  ppr FTF_C_T  = text "[=>]"
  ppr FTF_C_C  = text "[==>]"

instance Binary FunTyFlag where
  put_ bh FTF_T_T = putByte bh 0
  put_ bh FTF_T_C = putByte bh 1
  put_ bh FTF_C_T = putByte bh 2
  put_ bh FTF_C_C = putByte bh 3

  get bh = do
    h <- getByte bh
    case h of
      0 -> return FTF_T_T
      1 -> return FTF_T_C
      2 -> return FTF_C_T
      _ -> return FTF_C_C

instance NFData FunTyFlag where
  rnf FTF_T_T = ()
  rnf FTF_T_C = ()
  rnf FTF_C_T = ()
  rnf FTF_C_C = ()

mkFunTyFlag :: TypeOrConstraint -> TypeOrConstraint -> FunTyFlag
mkFunTyFlag TypeLike       torc = visArg torc
mkFunTyFlag ConstraintLike torc = invisArg torc

visArg :: TypeOrConstraint -> FunTyFlag
visArg TypeLike       = FTF_T_T
visArg ConstraintLike = FTF_T_C

visArgTypeLike :: FunTyFlag
visArgTypeLike = FTF_T_T

visArgConstraintLike :: FunTyFlag
visArgConstraintLike = FTF_T_C

invisArg :: TypeOrConstraint -> FunTyFlag
invisArg TypeLike       = FTF_C_T
invisArg ConstraintLike = FTF_C_C

invisArgTypeLike :: FunTyFlag
invisArgTypeLike = FTF_C_T

invisArgConstraintLike :: FunTyFlag
invisArgConstraintLike = FTF_C_C

isInvisibleFunArg :: FunTyFlag -> Bool
isInvisibleFunArg af = not (isVisibleFunArg af)

isVisibleFunArg :: FunTyFlag -> Bool
isVisibleFunArg FTF_T_T = True
isVisibleFunArg FTF_T_C = True
isVisibleFunArg _       = False

isFUNArg :: FunTyFlag -> Bool
-- This one, FUN, or (->), has an extra multiplicity argument
isFUNArg FTF_T_T = True
isFUNArg _       = False

funTyFlagArgTypeOrConstraint :: FunTyFlag -> TypeOrConstraint
-- Whether it /takes/ a type or a constraint
funTyFlagArgTypeOrConstraint FTF_T_T = TypeLike
funTyFlagArgTypeOrConstraint FTF_T_C = TypeLike
funTyFlagArgTypeOrConstraint _       = ConstraintLike

funTyFlagResultTypeOrConstraint :: FunTyFlag -> TypeOrConstraint
-- Whether it /returns/ a type or a constraint
funTyFlagResultTypeOrConstraint FTF_T_T = TypeLike
funTyFlagResultTypeOrConstraint FTF_C_T = TypeLike
funTyFlagResultTypeOrConstraint _       = ConstraintLike

{- Note [FunTyFlag]
~~~~~~~~~~~~~~~~~~~~~
FunTyFlag is used principally in the FunTy constructor of Type.
  FunTy FTF_T_T t1 t2   means   t1 -> t2
  FunTy FTF_C_T t1 t2   means   t1 => t2
  FunTy FTF_T_C t1 t2   means   t1 -=> t2
  FunTy FTF_C_C t1 t2   means   t1 ==> t2

However, the FunTyFlag in a FunTy is just redundant, cached
information.  In (FunTy { ft_af = af, ft_arg = t1, ft_res = t2 })
  ---------------------------------------------
  (isPredTy t1)   (isPredTy ty)     FunTyFlag
  ---------------------------------------------
     False           False         FTF_T_T
     False           True          FTF_T_C
     True            False         FTF_C_T
     True            True          FTF_C_C
where isPredTy is defined in GHC.Core.Type, and sees if t1's
kind is Constraint.  See GHC.Core.Type.chooseFunTyFlag, and
GHC.Core.TyCo.Rep Note [Types for coercions, predicates, and evidence]

The term (Lam b e) donesn't carry an FunTyFlag; instead it uses
mkFunctionType when we want to get its types; see mkLamType.  This is
just an engineering choice; we could cache here too if we wanted.

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
*                   VarBndr, ForAllTyBinder
*                                                                      *
********************************************************************* -}

{- Note [The VarBndr type and its uses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
VarBndr is polymorphic in both var and visibility fields.
Currently there are nine different uses of 'VarBndr':

* Var.ForAllTyBinder = VarBndr TyCoVar ForAllTyFlag
  Binder of a forall-type; see ForAllTy in GHC.Core.TyCo.Rep

* Var.TyVarBinder = VarBndr TyVar ForAllTyFlag
  Subset of ForAllTyBinder when we are sure the binder is a TyVar

* Var.InvisTVBinder = VarBndr TyVar Specificity
  Specialised form of TyVarBinder, when ForAllTyFlag = Invisible s
  See GHC.Core.Type.splitForAllInvisTVBinders

* Var.ReqTVBinder = VarBndr TyVar ()
  Specialised form of TyVarBinder, when ForAllTyFlag = Required
  See GHC.Core.Type.splitForAllReqTVBinders
  This one is barely used

* TyCon.TyConBinder = VarBndr TyVar TyConBndrVis
  Binders of a TyCon; see TyCon in GHC.Core.TyCon

* IfaceType.IfaceForAllBndr     = VarBndr IfaceBndr ForAllTyFlag
* IfaceType.IfaceForAllSpecBndr = VarBndr IfaceBndr Specificity
* IfaceType.IfaceTyConBinder    = VarBndr IfaceBndr TyConBndrVis
-}

data VarBndr var argf = Bndr var argf
  -- See Note [The VarBndr type and its uses]
  deriving( Data, Eq, Ord)

-- | Variable Binder
--
-- A 'ForAllTyBinder' is the binder of a ForAllTy
-- It's convenient to define this synonym here rather its natural
-- home in "GHC.Core.TyCo.Rep", because it's used in GHC.Core.DataCon.hs-boot
-- See Note [VarBndrs, ForAllTyBinders, TyConBinders, and visibility]
--
-- A 'TyVarBinder' is a binder with only TyVar
type ForAllTyBinder = VarBndr TyCoVar ForAllTyFlag
type InvisTyBinder  = VarBndr TyCoVar Specificity
type ReqTyBinder    = VarBndr TyCoVar ()

type TyVarBinder    = VarBndr TyVar   ForAllTyFlag
type InvisTVBinder  = VarBndr TyVar   Specificity
type ReqTVBinder    = VarBndr TyVar   ()

tyVarSpecToBinders :: [VarBndr a Specificity] -> [VarBndr a ForAllTyFlag]
tyVarSpecToBinders = map tyVarSpecToBinder

tyVarSpecToBinder :: VarBndr a Specificity -> VarBndr a ForAllTyFlag
tyVarSpecToBinder (Bndr tv vis) = Bndr tv (Invisible vis)

tyVarReqToBinders :: [VarBndr a ()] -> [VarBndr a ForAllTyFlag]
tyVarReqToBinders = map tyVarReqToBinder

tyVarReqToBinder :: VarBndr a () -> VarBndr a ForAllTyFlag
tyVarReqToBinder (Bndr tv _) = Bndr tv Required

isVisibleForAllTyBinder :: ForAllTyBinder -> Bool
isVisibleForAllTyBinder (Bndr _ vis) = isVisibleForAllTyFlag vis

isInvisibleForAllTyBinder :: ForAllTyBinder -> Bool
isInvisibleForAllTyBinder (Bndr _ vis) = isInvisibleForAllTyFlag vis

binderVar :: VarBndr tv argf -> tv
binderVar (Bndr v _) = v

binderVars :: [VarBndr tv argf] -> [tv]
binderVars tvbs = map binderVar tvbs

binderFlag :: VarBndr tv argf -> argf
binderFlag (Bndr _ argf) = argf

binderFlags :: [VarBndr tv argf] -> [argf]
binderFlags tvbs = map binderFlag tvbs

binderType :: VarBndr TyCoVar argf -> Type
binderType (Bndr tv _) = varType tv

isTyVarBinder :: VarBndr TyCoVar vis -> Bool
isTyVarBinder (Bndr tcv _) = isTyVar tcv

-- | Make a named binder
mkForAllTyBinder :: vis -> TyCoVar -> VarBndr TyCoVar vis
mkForAllTyBinder vis var = Bndr var vis

-- | Make a named binder
-- 'var' should be a type variable
mkTyVarBinder :: vis -> TyVar -> VarBndr TyVar vis
mkTyVarBinder vis var
  = assert (isTyVar var) $
    Bndr var vis

-- | Make many named binders
mkForAllTyBinders :: vis -> [TyCoVar] -> [VarBndr TyCoVar vis]
mkForAllTyBinders vis = map (mkForAllTyBinder vis)

-- | Make many named binders
-- Input vars should be type variables
mkTyVarBinders :: vis -> [TyVar] -> [VarBndr TyVar vis]
mkTyVarBinders vis = map (mkTyVarBinder vis)

mapVarBndr :: (var -> var') -> (VarBndr var flag) -> (VarBndr var' flag)
mapVarBndr f (Bndr v fl) = Bndr (f v) fl

mapVarBndrs :: (var -> var') -> [VarBndr var flag] -> [VarBndr var' flag]
mapVarBndrs f = map (mapVarBndr f)

instance Outputable tv => Outputable (VarBndr tv ForAllTyFlag) where
  ppr (Bndr v Required)  = ppr v
  ppr (Bndr v Specified) = char '@' <> ppr v
  ppr (Bndr v Inferred)  = braces (ppr v)

instance Outputable tv => Outputable (VarBndr tv Specificity) where
  ppr = ppr . tyVarSpecToBinder

instance (Binary tv, Binary vis) => Binary (VarBndr tv vis) where
  put_ bh (Bndr tv vis) = do { put_ bh tv; put_ bh vis }

  get bh = do { tv <- get bh; vis <- get bh; return (Bndr tv vis) }

instance (NFData tv, NFData vis) => NFData (VarBndr tv vis) where
  rnf (Bndr tv vis) = rnf tv `seq` rnf vis

instance NamedThing tv => NamedThing (VarBndr tv flag) where
  getName (Bndr tv _) = getName tv


{- **********************************************************************
*                                                                       *
                  PiTyBinder
*                                                                       *
********************************************************************** -}

-- | A 'PiTyBinder' represents an argument to a function. PiTyBinders can be
-- dependent ('Named') or nondependent ('Anon'). They may also be visible or
-- not. See Note [PiTyBinders]
data PiTyBinder
  = Named ForAllTyBinder          -- A type-lambda binder, with a ForAllTyFlag
  | Anon (Scaled Type) FunTyFlag  -- A term-lambda binder. Type here can be CoercionTy.
                                  -- The arrow is described by the FunTyFlag
  deriving Data

instance Outputable PiTyBinder where
  ppr (Anon ty af) = ppr af <+> ppr ty
  ppr (Named (Bndr v Required))  = ppr v
  ppr (Named (Bndr v Specified)) = char '@' <> ppr v
  ppr (Named (Bndr v Inferred))  = braces (ppr v)

-- | 'PiTyVarBinder' is like 'PiTyBinder', but there can only be 'TyVar'
-- in the 'Named' field.
type PiTyVarBinder = PiTyBinder

-- | Does this binder bind an invisible argument?
isInvisiblePiTyBinder :: PiTyBinder -> Bool
isInvisiblePiTyBinder (Named (Bndr _ vis)) = isInvisibleForAllTyFlag vis
isInvisiblePiTyBinder (Anon _ af)          = isInvisibleFunArg af

isInvisibleAnonPiTyBinder :: PiTyBinder -> Bool
isInvisibleAnonPiTyBinder (Named {})  = False
isInvisibleAnonPiTyBinder (Anon _ af) = isInvisibleFunArg af

-- | Does this binder bind a visible argument?
isVisiblePiTyBinder :: PiTyBinder -> Bool
isVisiblePiTyBinder = not . isInvisiblePiTyBinder

isNamedPiTyBinder :: PiTyBinder -> Bool
isNamedPiTyBinder (Named {}) = True
isNamedPiTyBinder (Anon {})  = False

namedPiTyBinder_maybe :: PiTyBinder -> Maybe TyCoVar
namedPiTyBinder_maybe (Named tv) = Just $ binderVar tv
namedPiTyBinder_maybe _          = Nothing

-- | Does this binder bind a variable that is /not/ erased? Returns
-- 'True' for anonymous binders.
isAnonPiTyBinder :: PiTyBinder -> Bool
isAnonPiTyBinder (Named {}) = False
isAnonPiTyBinder (Anon {})  = True

-- | Extract a relevant type, if there is one.
anonPiTyBinderType_maybe :: PiTyBinder -> Maybe Type
anonPiTyBinderType_maybe (Named {})  = Nothing
anonPiTyBinderType_maybe (Anon ty _) = Just (scaledThing ty)

-- | If its a named binder, is the binder a tyvar?
-- Returns True for nondependent binder.
-- This check that we're really returning a *Ty*Binder (as opposed to a
-- coercion binder). That way, if/when we allow coercion quantification
-- in more places, we'll know we missed updating some function.
isTyBinder :: PiTyBinder -> Bool
isTyBinder (Named bnd) = isTyVarBinder bnd
isTyBinder _ = True

piTyBinderType :: PiTyBinder -> Type
piTyBinderType (Named (Bndr tv _)) = varType tv
piTyBinderType (Anon ty _)         = scaledThing ty

{- Note [PiTyBinders]
~~~~~~~~~~~~~~~~~~~
But a type like
   forall a. Maybe a -> forall b. (a,b) -> b

can be decomposed to a telescope of type [PiTyBinder], using splitPiTys.
That function splits off all leading foralls and arrows, giving
   ([Named a, Anon (Maybe a), Named b, Anon (a,b)], b)

A PiTyBinder represents the type of binders -- that is, the type of an
argument to a Pi-type. GHC Core currently supports two different
Pi-types:

 * Anon ty1 fun_flag: a non-dependent function type,
   written with ->, e.g. ty1 -> ty2
   represented as FunTy ty1 ty2. These are
   lifted to Coercions with the corresponding FunCo.

 * Named (Var tv forall_flag)
    A dependent compile-time-only polytype,
   written with forall, e.g.  forall (a:*). ty
   represented as ForAllTy (Bndr a v) ty

Both forms of Pi-types classify terms/types that take an argument. In other
words, if `x` is either a function or a polytype, `x arg` makes sense
(for an appropriate `arg`).

Wrinkles

* The Anon constructor of PiTyBinder contains a FunTyFlag.  Since
  the PiTyBinder really only describes the /argument/ it should perhaps
  only have a TypeOrConstraint rather than a full FunTyFlag.  But it's
  very convenient to have the full FunTyFlag, say in mkPiTys, so that's
  what we do.


Note [VarBndrs, ForAllTyBinders, TyConBinders, and visibility]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* A ForAllTy (used for both types and kinds) contains a ForAllTyBinder.
  Each ForAllTyBinder
      Bndr a tvis
  is equipped with tvis::ForAllTyFlag, which says whether or not arguments
  for this binder should be visible (explicit) in source Haskell.

* A TyCon contains a list of TyConBinders.  Each TyConBinder
      Bndr a cvis
  is equipped with cvis::TyConBndrVis, which says whether or not type
  and kind arguments for this TyCon should be visible (explicit) in
  source Haskell.

This table summarises the visibility rules:
---------------------------------------------------------------------------------------
|                                                      Occurrences look like this
|                             GHC displays type as     in Haskell source code
|--------------------------------------------------------------------------------------
| Bndr a tvis :: ForAllTyBinder, in the binder of ForAllTy for a term
|  tvis :: ForAllTyFlag
|  tvis = Inferred:            f :: forall {a}. type    Arg not allowed:  f
                               f :: forall {co}. type   Arg not allowed:  f
|  tvis = Specified:           f :: forall a. type      Arg optional:     f  or  f @Int
|  tvis = Required:            f :: forall k -> type    Arg required:     f (type Int)
|
| Bndr k cvis :: TyConBinder, in the TyConBinders of a TyCon
|  cvis :: TyConBndrVis
|  cvis = AnonTCB:             T :: kind -> kind        Required:            T *
|  cvis = NamedTCB Inferred:   T :: forall {k}. kind    Arg not allowed:     T
|                              T :: forall {co}. kind   Arg not allowed:     T
|  cvis = NamedTCB Specified:  T :: forall k. kind      Arg not allowed[1]:  T
|  cvis = NamedTCB Required:   T :: forall k -> kind    Required:            T *
---------------------------------------------------------------------------------------

[1] In types, in the Specified case, it would make sense to allow
    optional kind applications, thus (T @*), but we have not
    yet implemented that

---- In term declarations ----

* Inferred.  Function defn, with no signature:  f1 x = x
  We infer f1 :: forall {a}. a -> a, with 'a' Inferred
  It's Inferred because it doesn't appear in any
  user-written signature for f1

* Specified.  Function defn, with signature (implicit forall):
     f2 :: a -> a; f2 x = x
  So f2 gets the type f2 :: forall a. a -> a, with 'a' Specified
  even though 'a' is not bound in the source code by an explicit forall

* Specified.  Function defn, with signature (explicit forall):
     f3 :: forall a. a -> a; f3 x = x
  So f3 gets the type f3 :: forall a. a -> a, with 'a' Specified

* Required.  Function defn, with signature (explicit forall):
     f4 :: forall a -> a -> a; f4 (type _) x = x
  So f4 gets the type f4 :: forall a -> a -> a, with 'a' Required
  This is the experimental RequiredTypeArguments extension,
  see GHC Proposal #281 "Visible forall in types of terms"

* Inferred.  Function defn, with signature (explicit forall), marked as inferred:
     f5 :: forall {a}. a -> a; f5 x = x
  So f5 gets the type f5 :: forall {a}. a -> a, with 'a' Inferred
  It's Inferred because the user marked it as such, even though it does appear
  in the user-written signature for f5

* Inferred/Specified.  Function signature with inferred kind polymorphism.
     f6 :: a b -> Int
  So 'f6' gets the type f6 :: forall {k} (a :: k -> Type) (b :: k). a b -> Int
  Here 'k' is Inferred (it's not mentioned in the type),
  but 'a' and 'b' are Specified.

* Specified.  Function signature with explicit kind polymorphism
     f7 :: a (b :: k) -> Int
  This time 'k' is Specified, because it is mentioned explicitly,
  so we get f7 :: forall (k :: Type) (a :: k -> Type) (b :: k). a b -> Int

* Similarly pattern synonyms:
  Inferred - from inferred types (e.g. no pattern type signature)
           - or from inferred kind polymorphism

---- In type declarations ----

* Inferred (k)
     data T1 a b = MkT1 (a b)
  Here T1's kind is  T1 :: forall {k:*}. (k->*) -> k -> *
  The kind variable 'k' is Inferred, since it is not mentioned

  Note that 'a' and 'b' correspond to /Anon/ PiTyBinders in T1's kind,
  and Anon binders don't have a visibility flag. (Or you could think
  of Anon having an implicit Required flag.)

* Specified (k)
     data T2 (a::k->*) b = MkT (a b)
  Here T's kind is  T :: forall (k:*). (k->*) -> k -> *
  The kind variable 'k' is Specified, since it is mentioned in
  the signature.

* Required (k)
     data T k (a::k->*) b = MkT (a b)
  Here T's kind is  T :: forall k:* -> (k->*) -> k -> *
  The kind is Required, since it bound in a positional way in T's declaration
  Every use of T must be explicitly applied to a kind

* Inferred (k1), Specified (k)
     data T a b (c :: k) = MkT (a b) (Proxy c)
  Here T's kind is  T :: forall {k1:*} (k:*). (k1->*) -> k1 -> k -> *
  So 'k' is Specified, because it appears explicitly,
  but 'k1' is Inferred, because it does not

Generally, in the list of TyConBinders for a TyCon,

* Inferred arguments always come first
* Specified, Anon and Required can be mixed

e.g.
  data Foo (a :: Type) :: forall b. (a -> b -> Type) -> Type where ...

Here Foo's TyConBinders are
   [Required 'a', Specified 'b', Anon]
and its kind prints as
   Foo :: forall a -> forall b. (a -> b -> Type) -> Type

See also Note [Required, Specified, and Inferred for types] in GHC.Tc.TyCl

---- Printing -----

 We print forall types with enough syntax to tell you their visibility
 flag.  But this is not source Haskell, and these types may not all
 be parsable.

 Specified: a list of Specified binders is written between `forall` and `.`:
               const :: forall a b. a -> b -> a

 Inferred: like Specified, but every binder is written in braces:
               f :: forall {k} (a :: k). S k a -> Int

 Required: binders are put between `forall` and `->`:
              T :: forall k -> *

---- Other points -----

* In classic Haskell, all named binders (that is, the type variables in
  a polymorphic function type f :: forall a. a -> a) have been Inferred.

* Inferred variables correspond to "generalized" variables from the
  Visible Type Applications paper (ESOP'16).
-}



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
                          , realUnique = nameUnique name
                          , varType  = kind
                          }

mkTcTyVar :: Name -> Kind -> TcTyVarDetails -> TyVar
mkTcTyVar name kind details
  = -- NB: 'kind' may be a coercion kind; cf, 'GHC.Tc.Utils.TcMType.newMetaCoVar'
    TcTyVar {   varName    = name,
                realUnique = nameUnique name,
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
         realUnique = nameUnique name,
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

isLocalId_maybe :: Var -> Maybe ExportFlag
isLocalId_maybe (Id { idScope = LocalId ef }) = Just ef
isLocalId_maybe _                             = Nothing

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
