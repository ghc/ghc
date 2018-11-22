{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section{@Vars@: Variables}
-}

{-# LANGUAGE CPP, FlexibleContexts, MultiWayIf, FlexibleInstances, DeriveDataTypeable #-}

-- |
-- #name_types#
-- GHC uses several kinds of name internally:
--
-- * 'OccName.OccName': see "OccName#name_types"
--
-- * 'RdrName.RdrName': see "RdrName#name_types"
--
-- * 'Name.Name': see "Name#name_types"
--
-- * 'Id.Id': see "Id#name_types"
--
-- * 'Var.Var' is a synonym for the 'Id.Id' type but it may additionally
--   potentially contain type variables, which have a 'TyCoRep.Kind'
--   rather than a 'TyCoRep.Type' and only contain some extra
--   details during typechecking.
--
--   These 'Var.Var' names may either be global or local, see "Var#globalvslocal"
--
-- #globalvslocal#
-- Global 'Id's and 'Var's are those that are imported or correspond
--    to a data constructor, primitive operation, or record selectors.
-- Local 'Id's and 'Var's are those bound within an expression
--    (e.g. by a lambda) or at the top level of the module being compiled.

module Var (
        -- * The main data type and synonyms
        Var, CoVar, Id, NcId, DictId, DFunId, EvVar, EqVar, EvId, IpId, JoinId,
        TyVar, TcTyVar, TypeVar, KindVar, TKVar, TyCoVar,

        -- * In and Out variants
        InVar,  InCoVar,  InId,  InTyVar,
        OutVar, OutCoVar, OutId, OutTyVar,

        -- ** Taking 'Var's apart
        varName, varUnique, varType,

        -- ** Modifying 'Var's
        setVarName, setVarUnique, setVarType, updateVarType,
        updateVarTypeM,

        -- ** Constructing, taking apart, modifying 'Id's
        mkGlobalVar, mkLocalVar, mkExportedLocalVar, mkCoVar,
        idInfo, idDetails,
        lazySetIdInfo, setIdDetails, globaliseId,
        setIdExported, setIdNotExported,

        -- ** Predicates
        isId, isTyVar, isTcTyVar,
        isLocalVar, isLocalId, isCoVar, isNonCoVarId, isTyCoVar,
        isGlobalId, isExportedId,
        mustHaveLocalBinding,

        -- * TyVar's
        VarBndr(..), ArgFlag(..), TyCoVarBinder, TyVarBinder,
        binderVar, binderVars, binderArgFlag, binderType,
        isVisibleArgFlag, isInvisibleArgFlag, sameVis,
        mkTyCoVarBinder, mkTyCoVarBinders,
        mkTyVarBinder, mkTyVarBinders,
        isTyVarBinder,

        -- ** Constructing TyVar's
        mkTyVar, mkTcTyVar,

        -- ** Taking 'TyVar's apart
        tyVarName, tyVarKind, tcTyVarDetails, setTcTyVarDetails,

        -- ** Modifying 'TyVar's
        setTyVarName, setTyVarUnique, setTyVarKind, updateTyVarKind,
        updateTyVarKindM,

        nonDetCmpVar

    ) where

#include "HsVersions.h"

import GhcPrelude

import {-# SOURCE #-}   TyCoRep( Type, Kind, pprKind )
import {-# SOURCE #-}   TcType( TcTyVarDetails, pprTcTyVarDetails, vanillaSkolemTv )
import {-# SOURCE #-}   IdInfo( IdDetails, IdInfo, coVarDetails, isCoVarDetails,
                                vanillaIdInfo, pprIdDetails )

import Name hiding (varName)
import Unique ( Uniquable, Unique, getKey, getUnique
              , mkUniqueGrimily, nonDetCmpUnique )
import Util
import Binary
import DynFlags
import Outputable

import Data.Data

{-
************************************************************************
*                                                                      *
                     Synonyms
*                                                                      *
************************************************************************
-- These synonyms are here and not in Id because otherwise we need a very
-- large number of SOURCE imports of Id.hs :-(
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
-}

instance Outputable Var where
  ppr var = sdocWithDynFlags $ \dflags ->
            getPprStyle $ \ppr_style ->
            if |  debugStyle ppr_style && (not (gopt Opt_SuppressVarKinds dflags))
                 -> parens (ppr (varName var) <+> ppr_debug var ppr_style <+>
                          dcolon <+> pprKind (tyVarKind var))
               |  otherwise
                 -> ppr (varName var) <> ppr_debug var ppr_style

ppr_debug :: Var -> PprStyle -> SDoc
ppr_debug (TyVar {}) sty
  | debugStyle sty = brackets (text "tv")
ppr_debug (TcTyVar {tc_tv_details = d}) sty
  | dumpStyle sty || debugStyle sty = brackets (pprTcTyVarDetails d)
ppr_debug (Id { idScope = s, id_details = d }) sty
  | debugStyle sty = brackets (ppr_id_scope s <> pprIdDetails d)
ppr_debug _ _ = empty

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

setVarUnique :: Var -> Unique -> Var
setVarUnique var uniq
  = var { realUnique = getKey uniq,
          varName = setNameUnique (varName var) uniq }

setVarName :: Var -> Name -> Var
setVarName var new_name
  = var { realUnique = getKey (getUnique new_name),
          varName = new_name }

setVarType :: Id -> Type -> Id
setVarType id ty = id { varType = ty }

updateVarType :: (Type -> Type) -> Id -> Id
updateVarType f id = id { varType = f (varType id) }

updateVarTypeM :: Monad m => (Type -> m Type) -> Id -> m Id
updateVarTypeM f id = do { ty' <- f (varType id)
                         ; return (id { varType = ty' }) }

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
-- See Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility] in TyCoRep
data ArgFlag = Inferred | Specified | Required
  deriving (Eq, Ord, Data)
  -- (<) on ArgFlag means "is less visible than"

-- | Does this 'ArgFlag' classify an argument that is written in Haskell?
isVisibleArgFlag :: ArgFlag -> Bool
isVisibleArgFlag Required = True
isVisibleArgFlag _        = False

-- | Does this 'ArgFlag' classify an argument that is not written in Haskell?
isInvisibleArgFlag :: ArgFlag -> Bool
isInvisibleArgFlag = not . isVisibleArgFlag

-- | Do these denote the same level of visibility? 'Required'
-- arguments are visible, others are not. So this function
-- equates 'Specified' and 'Inferred'. Used for printing.
sameVis :: ArgFlag -> ArgFlag -> Bool
sameVis Required Required = True
sameVis Required _        = False
sameVis _        Required = False
sameVis _        _        = True

instance Outputable ArgFlag where
  ppr Required  = text "[req]"
  ppr Specified = text "[spec]"
  ppr Inferred  = text "[infrd]"

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

{- *********************************************************************
*                                                                      *
*                   VarBndr, TyCoVarBinder
*                                                                      *
********************************************************************* -}

-- Variable Binder
--
-- VarBndr is polymorphic in both var and visibility fields.
-- Currently there are six different uses of 'VarBndr':
--   * Var.TyVarBinder   = VarBndr TyVar ArgFlag
--   * Var.TyCoVarBinder = VarBndr TyCoVar ArgFlag
--   * TyCon.TyConBinder     = VarBndr TyVar TyConBndrVis
--   * TyCon.TyConTyCoBinder = VarBndr TyCoVar TyConBndrVis
--   * IfaceType.IfaceForAllBndr  = VarBndr IfaceBndr ArgFlag
--   * IfaceType.IfaceTyConBinder = VarBndr IfaceBndr TyConBndrVis
data VarBndr var argf = Bndr var argf
  deriving( Data )

-- | Variable Binder
--
-- A 'TyCoVarBinder' is the binder of a ForAllTy
-- It's convenient to define this synonym here rather its natural
-- home in TyCoRep, because it's used in DataCon.hs-boot
--
-- A 'TyVarBinder' is a binder with only TyVar
type TyCoVarBinder = VarBndr TyCoVar ArgFlag
type TyVarBinder   = VarBndr TyVar ArgFlag

binderVar :: VarBndr tv argf -> tv
binderVar (Bndr v _) = v

binderVars :: [VarBndr tv argf] -> [tv]
binderVars tvbs = map binderVar tvbs

binderArgFlag :: VarBndr tv argf -> argf
binderArgFlag (Bndr _ argf) = argf

binderType :: VarBndr TyCoVar argf -> Type
binderType (Bndr tv _) = varType tv

-- | Make a named binder
mkTyCoVarBinder :: ArgFlag -> TyCoVar -> TyCoVarBinder
mkTyCoVarBinder vis var = Bndr var vis

-- | Make a named binder
-- 'var' should be a type variable
mkTyVarBinder :: ArgFlag -> TyVar -> TyVarBinder
mkTyVarBinder vis var
  = ASSERT( isTyVar var )
    Bndr var vis

-- | Make many named binders
mkTyCoVarBinders :: ArgFlag -> [TyCoVar] -> [TyCoVarBinder]
mkTyCoVarBinders vis = map (mkTyCoVarBinder vis)

-- | Make many named binders
-- Input vars should be type variables
mkTyVarBinders :: ArgFlag -> [TyVar] -> [TyVarBinder]
mkTyVarBinders vis = map (mkTyVarBinder vis)

isTyVarBinder :: TyCoVarBinder -> Bool
isTyVarBinder (Bndr v _) = isTyVar v

instance Outputable tv => Outputable (VarBndr tv ArgFlag) where
  ppr (Bndr v Required)  = ppr v
  ppr (Bndr v Specified) = char '@' <> ppr v
  ppr (Bndr v Inferred)  = braces (ppr v)

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
  = -- NB: 'kind' may be a coercion kind; cf, 'TcMType.newMetaCoVar'
    TcTyVar {   varName    = name,
                realUnique = getKey (nameUnique name),
                varType  = kind,
                tc_tv_details = details
        }

tcTyVarDetails :: TyVar -> TcTyVarDetails
-- See Note [TcTyVars in the typechecker] in TcType
tcTyVarDetails (TcTyVar { tc_tv_details = details }) = details
tcTyVarDetails (TyVar {})                            = vanillaSkolemTv
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
-- Ids, because Id.hs uses 'mkGlobalId' etc with different types
mkGlobalVar :: IdDetails -> Name -> Type -> IdInfo -> Id
mkGlobalVar details name ty info
  = mk_id name ty GlobalId details info

mkLocalVar :: IdDetails -> Name -> Type -> IdInfo -> Id
mkLocalVar details name ty info
  = mk_id name ty (LocalId NotExported) details  info

mkCoVar :: Name -> Type -> CoVar
-- Coercion variables have no IdInfo
mkCoVar name ty = mk_id name ty (LocalId NotExported) coVarDetails vanillaIdInfo

-- | Exported 'Var's will not be removed as dead code
mkExportedLocalVar :: IdDetails -> Name -> Type -> IdInfo -> Id
mkExportedLocalVar details name ty info
  = mk_id name ty (LocalId Exported) details info

mk_id :: Name -> Type -> IdScope -> IdDetails -> IdInfo -> Id
mk_id name ty scope details info
  = Id { varName    = name,
         realUnique = getKey (nameUnique name),
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
setIdNotExported id = ASSERT( isLocalId id )
                      id { idScope = LocalId NotExported }

{-
************************************************************************
*                                                                      *
\subsection{Predicates over variables}
*                                                                      *
************************************************************************
-}

isTyVar :: Var -> Bool        -- True of both TyVar and TcTyVar
isTyVar (TyVar {})   = True
isTyVar (TcTyVar {}) = True
isTyVar _            = False

isTcTyVar :: Var -> Bool      -- True of TcTyVar only
isTcTyVar (TcTyVar {}) = True
isTcTyVar _            = False

isTyCoVar :: Var -> Bool
isTyCoVar v = isTyVar v || isCoVar v

isId :: Var -> Bool
isId (Id {}) = True
isId _       = False

isCoVar :: Var -> Bool
-- A coercion variable
isCoVar (Id { id_details = details }) = isCoVarDetails details
isCoVar _                             = False

isNonCoVarId :: Var -> Bool
-- A term variable (Id) that is /not/ a coercion variable
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
