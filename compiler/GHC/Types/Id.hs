{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[Id]{@Ids@: Value and constructor identifiers}
-}



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
-- * 'GHC.Types.Id.Id' represents names that not only have a 'GHC.Types.Name.Name' but also a
--   'GHC.Core.TyCo.Rep.Type' and some additional details (a 'GHC.Types.Id.Info.IdInfo' and
--   one of LocalIdDetails or GlobalIdDetails) that are added,
--   modified and inspected by various compiler passes. These 'GHC.Types.Var.Var' names
--   may either be global or local, see "GHC.Types.Var#globalvslocal"
--
-- * 'GHC.Types.Var.Var': see "GHC.Types.Var#name_types"

module GHC.Types.Id (
        -- * The main types
        Var, Id, isId,

        -- * In and Out variants
        InVar,  InId,
        OutVar, OutId,

        -- ** Simple construction
        mkGlobalId, mkVanillaGlobal, mkVanillaGlobalWithInfo,
        mkLocalId, mkLocalCoVar, mkLocalIdOrCoVar,
        mkLocalIdWithInfo, mkExportedLocalId, mkExportedVanillaId,
        mkSysLocal, mkSysLocalM, mkSysLocalOrCoVar, mkSysLocalOrCoVarM,
        mkUserLocal, mkUserLocalOrCoVar,
        mkTemplateLocals, mkTemplateLocalsNum, mkTemplateLocal,
        mkScaledTemplateLocal,
        mkWorkerId,

        -- ** Taking an Id apart
        idName, idType, idMult, idScaledType, idUnique, idInfo, idDetails,
        recordSelectorTyCon,
        recordSelectorTyCon_maybe,

        -- ** Modifying an Id
        setIdName, setIdUnique, GHC.Types.Id.setIdType, setIdMult,
        updateIdTypeButNotMult, updateIdTypeAndMult, updateIdTypeAndMultM,
        setIdExported, setIdNotExported,
        globaliseId, localiseId,
        setIdInfo, lazySetIdInfo, modifyIdInfo, maybeModifyIdInfo,
        zapLamIdInfo, zapIdDemandInfo, zapIdUsageInfo, zapIdUsageEnvInfo,
        zapIdUsedOnceInfo, zapIdTailCallInfo,
        zapFragileIdInfo, zapIdDmdSig, zapStableUnfolding,
        transferPolyIdInfo, scaleIdBy, scaleVarBy,

        -- ** Predicates on Ids
        isImplicitId, isDeadBinder,
        isStrictId,
        isExportedId, isLocalId, isGlobalId,
        isRecordSelector, isNaughtyRecordSelector,
        isPatSynRecordSelector,
        isDataConRecordSelector,
        isClassOpId,
        isClassOpId_maybe, isDFunId,
        isPrimOpId, isPrimOpId_maybe,
        isFCallId, isFCallId_maybe,
        isDataConWorkId, isDataConWorkId_maybe,
        isDataConWrapId, isDataConWrapId_maybe,
        isDataConId_maybe,
        idDataCon,
        isConLikeId, isDeadEndId, idIsFrom,
        hasNoBinding,

        -- ** Join variables
        JoinId, isJoinId, isJoinId_maybe, idJoinArity,
        asJoinId, asJoinId_maybe, zapJoinId,

        -- ** Inline pragma stuff
        idInlinePragma, setInlinePragma, modifyInlinePragma,
        idInlineActivation, setInlineActivation, idRuleMatchInfo,

        -- ** One-shot lambdas
        isOneShotBndr, isProbablyOneShotLambda,
        setOneShotLambda, clearOneShotLambda,
        updOneShotInfo, setIdOneShotInfo,
        isStateHackType, stateHackOneShot, typeOneShot,

        -- ** Reading 'IdInfo' fields
        idArity,
        idCallArity, idFunRepArity,
        idUnfolding, realIdUnfolding,
        idSpecialisation, idCoreRules, idHasRules,
        idCafInfo, idLFInfo_maybe,
        idOneShotInfo, idStateHackOneShotInfo,
        idOccInfo,
        isNeverRepPolyId,

        -- ** Writing 'IdInfo' fields
        setIdUnfolding, setCaseBndrEvald,
        setIdArity,
        setIdCallArity,

        setIdSpecialisation,
        setIdCafInfo,
        setIdOccInfo, zapIdOccInfo,
        setIdLFInfo,

        setIdDemandInfo,
        setIdDmdSig,
        setIdCprSig,

        idDemandInfo,
        idDmdSig,
        idCprSig,

    ) where

import GHC.Prelude

import GHC.Core ( CoreRule, isStableUnfolding, evaldUnfolding,
                 isCompulsoryUnfolding, Unfolding( NoUnfolding ) )

import GHC.Types.Id.Info
import GHC.Types.Basic

-- Imported and re-exported
import GHC.Types.Var( Id, CoVar, JoinId,
            InId,  InVar,
            OutId, OutVar,
            idInfo, idDetails, setIdDetails, globaliseId,
            isId, isLocalId, isGlobalId, isExportedId,
            setIdMult, updateIdTypeAndMult, updateIdTypeButNotMult, updateIdTypeAndMultM)
import qualified GHC.Types.Var as Var

import GHC.Core.Type
import GHC.Types.RepType
import GHC.Builtin.Types.Prim
import GHC.Core.DataCon
import GHC.Types.Demand
import GHC.Types.Cpr
import GHC.Types.Name
import GHC.Unit.Module
import GHC.Core.Class
import {-# SOURCE #-} GHC.Builtin.PrimOps (PrimOp)
import GHC.Types.ForeignCall
import GHC.Data.Maybe
import GHC.Types.SrcLoc
import GHC.Types.Unique
import GHC.Builtin.Uniques (mkBuiltinUnique)
import GHC.Types.Unique.Supply
import GHC.Data.FastString
import GHC.Core.Multiplicity

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.GlobalVars
import GHC.Utils.Trace

-- infixl so you can say (id `set` a `set` b)
infixl  1 `setIdUnfolding`,
          `setIdArity`,
          `setIdCallArity`,
          `setIdOccInfo`,
          `setIdOneShotInfo`,

          `setIdSpecialisation`,
          `setInlinePragma`,
          `setInlineActivation`,
          `idCafInfo`,

          `setIdDemandInfo`,
          `setIdDmdSig`,
          `setIdCprSig`,

          `asJoinId`,
          `asJoinId_maybe`

{-
************************************************************************
*                                                                      *
\subsection{Basic Id manipulation}
*                                                                      *
************************************************************************
-}

idName   :: Id -> Name
idName    = Var.varName

idUnique :: Id -> Unique
idUnique  = Var.varUnique

idType   :: Id -> Kind
idType    = Var.varType

idMult :: Id -> Mult
idMult = Var.varMult

idScaledType :: Id -> Scaled Type
idScaledType id = Scaled (idMult id) (idType id)

scaleIdBy :: Mult -> Id -> Id
scaleIdBy m id = setIdMult id (m `mkMultMul` idMult id)

-- | Like 'scaleIdBy', but skips non-Ids. Useful for scaling
-- a mixed list of ids and tyvars.
scaleVarBy :: Mult -> Var -> Var
scaleVarBy m id
  | isId id   = scaleIdBy m id
  | otherwise = id

setIdName :: Id -> Name -> Id
setIdName = Var.setVarName

setIdUnique :: Id -> Unique -> Id
setIdUnique = Var.setVarUnique

-- | Not only does this set the 'Id' 'Type', it also evaluates the type to try and
-- reduce space usage
setIdType :: Id -> Type -> Id
setIdType id ty = seqType ty `seq` Var.setVarType id ty

setIdExported :: Id -> Id
setIdExported = Var.setIdExported

setIdNotExported :: Id -> Id
setIdNotExported = Var.setIdNotExported

localiseId :: Id -> Id
-- Make an Id with the same unique and type as the
-- incoming Id, but with an *Internal* Name and *LocalId* flavour
localiseId id
  | assert (isId id) $ isLocalId id && isInternalName name
  = id
  | otherwise
  = Var.mkLocalVar (idDetails id) (localiseName name) (Var.varMult id) (idType id) (idInfo id)
  where
    name = idName id

lazySetIdInfo :: Id -> IdInfo -> Id
lazySetIdInfo = Var.lazySetIdInfo

setIdInfo :: Id -> IdInfo -> Id
setIdInfo id info = info `seq` (lazySetIdInfo id info)
        -- Try to avoid space leaks by seq'ing

modifyIdInfo :: HasDebugCallStack => (IdInfo -> IdInfo) -> Id -> Id
modifyIdInfo fn id = setIdInfo id (fn (idInfo id))

-- maybeModifyIdInfo tries to avoid unnecessary thrashing
maybeModifyIdInfo :: Maybe IdInfo -> Id -> Id
maybeModifyIdInfo (Just new_info) id = lazySetIdInfo id new_info
maybeModifyIdInfo Nothing         id = id

{-
************************************************************************
*                                                                      *
\subsection{Simple Id construction}
*                                                                      *
************************************************************************

Absolutely all Ids are made by mkId.  It is just like Var.mkId,
but in addition it pins free-tyvar-info onto the Id's type,
where it can easily be found.

Note [Free type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~
At one time we cached the free type variables of the type of an Id
at the root of the type in a TyNote.  The idea was to avoid repeating
the free-type-variable calculation.  But it turned out to slow down
the compiler overall. I don't quite know why; perhaps finding free
type variables of an Id isn't all that common whereas applying a
substitution (which changes the free type variables) is more common.
Anyway, we removed it in March 2008.
-}

-- | For an explanation of global vs. local 'Id's, see "GHC.Types.Var.Var#globalvslocal"
mkGlobalId :: IdDetails -> Name -> Type -> IdInfo -> Id
mkGlobalId = Var.mkGlobalVar

-- | Make a global 'Id' without any extra information at all
mkVanillaGlobal :: Name -> Type -> Id
mkVanillaGlobal name ty = mkVanillaGlobalWithInfo name ty vanillaIdInfo

-- | Make a global 'Id' with no global information but some generic 'IdInfo'
mkVanillaGlobalWithInfo :: Name -> Type -> IdInfo -> Id
mkVanillaGlobalWithInfo = mkGlobalId VanillaId


-- | For an explanation of global vs. local 'Id's, see "GHC.Types.Var#globalvslocal"
mkLocalId :: HasDebugCallStack => Name -> Mult -> Type -> Id
mkLocalId name w ty = mkLocalIdWithInfo name w (assert (not (isCoVarType ty)) ty) vanillaIdInfo

-- | Make a local CoVar
mkLocalCoVar :: Name -> Type -> CoVar
mkLocalCoVar name ty
  = assert (isCoVarType ty) $
    Var.mkLocalVar CoVarId name Many ty vanillaIdInfo

-- | Like 'mkLocalId', but checks the type to see if it should make a covar
mkLocalIdOrCoVar :: Name -> Mult -> Type -> Id
mkLocalIdOrCoVar name w ty
  -- We should assert (eqType w Many) in the isCoVarType case.
  -- However, currently this assertion does not hold.
  -- In tests with -fdefer-type-errors, such as T14584a,
  -- we create a linear 'case' where the scrutinee is a coercion
  -- (see castBottomExpr). This problem is covered by #17291.
  | isCoVarType ty = mkLocalCoVar name   ty
  | otherwise      = mkLocalId    name w ty

    -- proper ids only; no covars!
mkLocalIdWithInfo :: HasDebugCallStack => Name -> Mult -> Type -> IdInfo -> Id
mkLocalIdWithInfo name w ty info =
  Var.mkLocalVar VanillaId name w (assert (not (isCoVarType ty)) ty) info
        -- Note [Free type variables]

-- | Create a local 'Id' that is marked as exported.
-- This prevents things attached to it from being removed as dead code.
-- See Note [Exported LocalIds]
mkExportedLocalId :: IdDetails -> Name -> Type -> Id
mkExportedLocalId details name ty = Var.mkExportedLocalVar details name ty vanillaIdInfo
        -- Note [Free type variables]

mkExportedVanillaId :: Name -> Type -> Id
mkExportedVanillaId name ty = Var.mkExportedLocalVar VanillaId name ty vanillaIdInfo
        -- Note [Free type variables]


-- | Create a system local 'Id'. These are local 'Id's (see "Var#globalvslocal")
-- that are created by the compiler out of thin air
mkSysLocal :: FastString -> Unique -> Mult -> Type -> Id
mkSysLocal fs uniq w ty = assert (not (isCoVarType ty)) $
                        mkLocalId (mkSystemVarName uniq fs) w ty

-- | Like 'mkSysLocal', but checks to see if we have a covar type
mkSysLocalOrCoVar :: FastString -> Unique -> Mult -> Type -> Id
mkSysLocalOrCoVar fs uniq w ty
  = mkLocalIdOrCoVar (mkSystemVarName uniq fs) w ty

mkSysLocalM :: MonadUnique m => FastString -> Mult -> Type -> m Id
mkSysLocalM fs w ty = getUniqueM >>= (\uniq -> return (mkSysLocal fs uniq w ty))

mkSysLocalOrCoVarM :: MonadUnique m => FastString -> Mult -> Type -> m Id
mkSysLocalOrCoVarM fs w ty
  = getUniqueM >>= (\uniq -> return (mkSysLocalOrCoVar fs uniq w ty))

-- | Create a user local 'Id'. These are local 'Id's (see "GHC.Types.Var#globalvslocal") with a name and location that the user might recognize
mkUserLocal :: OccName -> Unique -> Mult -> Type -> SrcSpan -> Id
mkUserLocal occ uniq w ty loc = assert (not (isCoVarType ty)) $
                                mkLocalId (mkInternalName uniq occ loc) w ty

-- | Like 'mkUserLocal', but checks if we have a coercion type
mkUserLocalOrCoVar :: OccName -> Unique -> Mult -> Type -> SrcSpan -> Id
mkUserLocalOrCoVar occ uniq w ty loc
  = mkLocalIdOrCoVar (mkInternalName uniq occ loc) w ty

{-
Make some local @Ids@ for a template @CoreExpr@.  These have bogus
@Uniques@, but that's OK because the templates are supposed to be
instantiated before use.
-}

-- | Workers get local names. "CoreTidy" will externalise these if necessary
mkWorkerId :: Unique -> Id -> Type -> Id
mkWorkerId uniq unwrkr ty
  = mkLocalId (mkDerivedInternalName mkWorkerOcc uniq (getName unwrkr)) Many ty

-- | Create a /template local/: a family of system local 'Id's in bijection with @Int@s, typically used in unfoldings
mkTemplateLocal :: Int -> Type -> Id
mkTemplateLocal i ty = mkScaledTemplateLocal i (unrestricted ty)

mkScaledTemplateLocal :: Int -> Scaled Type -> Id
mkScaledTemplateLocal i (Scaled w ty) = mkSysLocalOrCoVar (fsLit "v") (mkBuiltinUnique i) w ty
   -- "OrCoVar" since this is used in a superclass selector,
   -- and "~" and "~~" have coercion "superclasses".

-- | Create a template local for a series of types
mkTemplateLocals :: [Type] -> [Id]
mkTemplateLocals = mkTemplateLocalsNum 1

-- | Create a template local for a series of type, but start from a specified template local
mkTemplateLocalsNum :: Int -> [Type] -> [Id]
mkTemplateLocalsNum n tys = zipWith mkTemplateLocal [n..] tys

{- Note [Exported LocalIds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use mkExportedLocalId for things like
 - Dictionary functions (DFunId)
 - Wrapper and matcher Ids for pattern synonyms
 - Default methods for classes
 - Pattern-synonym matcher and builder Ids
 - etc

They marked as "exported" in the sense that they should be kept alive
even if apparently unused in other bindings, and not dropped as dead
code by the occurrence analyser.  (But "exported" here does not mean
"brought into lexical scope by an import declaration". Indeed these
things are always internal Ids that the user never sees.)

It's very important that they are *LocalIds*, not GlobalIds, for lots
of reasons:

 * We want to treat them as free variables for the purpose of
   dependency analysis (e.g. GHC.Core.FVs.exprFreeVars).

 * Look them up in the current substitution when we come across
   occurrences of them (in Subst.lookupIdSubst). Lacking this we
   can get an out-of-date unfolding, which can in turn make the
   simplifier go into an infinite loop (#9857)

 * Ensure that for dfuns that the specialiser does not float dict uses
   above their defns, which would prevent good simplifications happening.

 * The strictness analyser treats a occurrence of a GlobalId as
   imported and assumes it contains strictness in its IdInfo, which
   isn't true if the thing is bound in the same module as the
   occurrence.

In CoreTidy we must make all these LocalIds into GlobalIds, so that in
importing modules (in --make mode) we treat them as properly global.
That is what is happening in, say tidy_insts in GHC.Iface.Tidy.

************************************************************************
*                                                                      *
\subsection{Special Ids}
*                                                                      *
************************************************************************
-}

-- | If the 'Id' is that for a record selector, extract the 'sel_tycon'. Panic otherwise.
recordSelectorTyCon :: Id -> RecSelParent
recordSelectorTyCon id
  = case recordSelectorTyCon_maybe id of
        Just parent -> parent
        _ -> panic "recordSelectorTyCon"

recordSelectorTyCon_maybe :: Id -> Maybe RecSelParent
recordSelectorTyCon_maybe id
  = case Var.idDetails id of
        RecSelId { sel_tycon = parent } -> Just parent
        _ -> Nothing

isRecordSelector        :: Id -> Bool
isNaughtyRecordSelector :: Id -> Bool
isPatSynRecordSelector  :: Id -> Bool
isDataConRecordSelector  :: Id -> Bool
isPrimOpId              :: Id -> Bool
isFCallId               :: Id -> Bool
isDataConWorkId         :: Id -> Bool
isDataConWrapId         :: Id -> Bool
isDFunId                :: Id -> Bool
isClassOpId             :: Id -> Bool

isClassOpId_maybe       :: Id -> Maybe Class
isPrimOpId_maybe        :: Id -> Maybe PrimOp
isFCallId_maybe         :: Id -> Maybe ForeignCall
isDataConWorkId_maybe   :: Id -> Maybe DataCon
isDataConWrapId_maybe   :: Id -> Maybe DataCon

isRecordSelector id = case Var.idDetails id of
                        RecSelId {}     -> True
                        _               -> False

isDataConRecordSelector id = case Var.idDetails id of
                        RecSelId {sel_tycon = RecSelData _} -> True
                        _               -> False

isPatSynRecordSelector id = case Var.idDetails id of
                        RecSelId {sel_tycon = RecSelPatSyn _} -> True
                        _               -> False

isNaughtyRecordSelector id = case Var.idDetails id of
                        RecSelId { sel_naughty = n } -> n
                        _                               -> False

isClassOpId id = case Var.idDetails id of
                        ClassOpId _   -> True
                        _other        -> False

isClassOpId_maybe id = case Var.idDetails id of
                        ClassOpId cls -> Just cls
                        _other        -> Nothing

isPrimOpId id = case Var.idDetails id of
                        PrimOpId {} -> True
                        _           -> False

isDFunId id = case Var.idDetails id of
                        DFunId {} -> True
                        _         -> False

isPrimOpId_maybe id = case Var.idDetails id of
                        PrimOpId op _ -> Just op
                        _             -> Nothing

isFCallId id = case Var.idDetails id of
                        FCallId _ -> True
                        _         -> False

isFCallId_maybe id = case Var.idDetails id of
                        FCallId call -> Just call
                        _            -> Nothing

isDataConWorkId id = case Var.idDetails id of
                        DataConWorkId _ -> True
                        _               -> False

isDataConWorkId_maybe id = case Var.idDetails id of
                        DataConWorkId con -> Just con
                        _                 -> Nothing

isDataConWrapId id = case Var.idDetails id of
                       DataConWrapId _ -> True
                       _               -> False

isDataConWrapId_maybe id = case Var.idDetails id of
                        DataConWrapId con -> Just con
                        _                 -> Nothing

isDataConId_maybe :: Id -> Maybe DataCon
isDataConId_maybe id = case Var.idDetails id of
                         DataConWorkId con -> Just con
                         DataConWrapId con -> Just con
                         _                 -> Nothing

isJoinId :: Var -> Bool
-- It is convenient in GHC.Core.Opt.SetLevels.lvlMFE to apply isJoinId
-- to the free vars of an expression, so it's convenient
-- if it returns False for type variables
isJoinId id
  | isId id = case Var.idDetails id of
                JoinId {} -> True
                _         -> False
  | otherwise = False

isJoinId_maybe :: Var -> Maybe JoinArity
isJoinId_maybe id
 | isId id  = assertPpr (isId id) (ppr id) $
              case Var.idDetails id of
                JoinId arity -> Just arity
                _            -> Nothing
 | otherwise = Nothing

idDataCon :: Id -> DataCon
-- ^ Get from either the worker or the wrapper 'Id' to the 'DataCon'. Currently used only in the desugarer.
--
-- INVARIANT: @idDataCon (dataConWrapId d) = d@: remember, 'dataConWrapId' can return either the wrapper or the worker
idDataCon id = isDataConId_maybe id `orElse` pprPanic "idDataCon" (ppr id)

hasNoBinding :: Id -> Bool
-- ^ Returns @True@ of an 'Id' which may not have a
-- binding, even though it is defined in this module.

-- Data constructor workers used to be things of this kind, but they aren't any
-- more.  Instead, we inject a binding for them at the CorePrep stage. The
-- exception to this is unboxed tuples and sums datacons, which definitely have
-- no binding
hasNoBinding id = case Var.idDetails id of

-- TEMPORARILY make all primops hasNoBinding, to avoid #20155
-- The goal is to understand #20155 and revert to the commented out version
                        PrimOpId _ _ -> True    -- See Note [Eta expanding primops] in GHC.Builtin.PrimOps
--                        PrimOpId _ lev_poly -> lev_poly    -- TEMPORARILY commented out

                        FCallId _        -> True
                        DataConWorkId dc -> isUnboxedTupleDataCon dc || isUnboxedSumDataCon dc
                        _                -> isCompulsoryUnfolding (idUnfolding id)

isImplicitId :: Id -> Bool
-- ^ 'isImplicitId' tells whether an 'Id's info is implied by other
-- declarations, so we don't need to put its signature in an interface
-- file, even if it's mentioned in some other interface unfolding.
isImplicitId id
  = case Var.idDetails id of
        FCallId {}       -> True
        ClassOpId {}     -> True
        PrimOpId {}      -> True
        DataConWorkId {} -> True
        DataConWrapId {} -> True
                -- These are implied by their type or class decl;
                -- remember that all type and class decls appear in the interface file.
                -- The dfun id is not an implicit Id; it must *not* be omitted, because
                -- it carries version info for the instance decl
        _               -> False

idIsFrom :: Module -> Id -> Bool
idIsFrom mod id = nameIsLocalOrFrom mod (idName id)

isDeadBinder :: Id -> Bool
isDeadBinder bndr | isId bndr = isDeadOcc (idOccInfo bndr)
                  | otherwise = False   -- TyVars count as not dead

{-
************************************************************************
*                                                                      *
              Join variables
*                                                                      *
************************************************************************
-}

idJoinArity :: JoinId -> JoinArity
idJoinArity id = isJoinId_maybe id `orElse` pprPanic "idJoinArity" (ppr id)

asJoinId :: Id -> JoinArity -> JoinId
asJoinId id arity = warnPprTrace (not (isLocalId id))
                         (text "global id being marked as join var:" <+> ppr id) $
                    warnPprTrace (not (is_vanilla_or_join id))
                         (ppr id <+> pprIdDetails (idDetails id)) $
                    id `setIdDetails` JoinId arity
  where
    is_vanilla_or_join id = case Var.idDetails id of
                              VanillaId -> True
                              JoinId {} -> True
                              _         -> False

zapJoinId :: Id -> Id
-- May be a regular id already
zapJoinId jid | isJoinId jid = zapIdTailCallInfo (jid `setIdDetails` VanillaId)
                                 -- Core Lint may complain if still marked
                                 -- as AlwaysTailCalled
              | otherwise    = jid

asJoinId_maybe :: Id -> Maybe JoinArity -> Id
asJoinId_maybe id (Just arity) = asJoinId id arity
asJoinId_maybe id Nothing      = zapJoinId id

{-
************************************************************************
*                                                                      *
\subsection{IdInfo stuff}
*                                                                      *
************************************************************************
-}

        ---------------------------------
        -- ARITY
idArity :: Id -> Arity
idArity id = arityInfo (idInfo id)

setIdArity :: Id -> Arity -> Id
setIdArity id arity = modifyIdInfo (`setArityInfo` arity) id

idCallArity :: Id -> Arity
idCallArity id = callArityInfo (idInfo id)

setIdCallArity :: Id -> Arity -> Id
setIdCallArity id arity = modifyIdInfo (`setCallArityInfo` arity) id

idFunRepArity :: Id -> RepArity
idFunRepArity x = countFunRepArgs (idArity x) (idType x)

-- | Returns true if an application to n args diverges or throws an exception
-- See Note [Dead ends] in "GHC.Types.Demand".
isDeadEndId :: Var -> Bool
isDeadEndId v
  | isId v    = isDeadEndSig (idDmdSig v)
  | otherwise = False

-- | Accesses the 'Id''s 'dmdSigInfo'.
idDmdSig :: Id -> DmdSig
idDmdSig id = dmdSigInfo (idInfo id)

setIdDmdSig :: Id -> DmdSig -> Id
setIdDmdSig id sig = modifyIdInfo (`setDmdSigInfo` sig) id

idCprSig :: Id -> CprSig
idCprSig id = cprSigInfo (idInfo id)

setIdCprSig :: Id -> CprSig -> Id
setIdCprSig id sig = modifyIdInfo (\info -> setCprSigInfo info sig) id

zapIdDmdSig :: Id -> Id
zapIdDmdSig id = modifyIdInfo (`setDmdSigInfo` nopSig) id

-- | This predicate says whether the 'Id' has a strict demand placed on it or
-- has a type such that it can always be evaluated strictly (i.e an
-- unlifted type, as of GHC 7.6).  We need to
-- check separately whether the 'Id' has a so-called \"strict type\" because if
-- the demand for the given @id@ hasn't been computed yet but @id@ has a strict
-- type, we still want @isStrictId id@ to be @True@.
isStrictId :: Id -> Bool
isStrictId id
  | assertPpr (isId id) (text "isStrictId: not an id: " <+> ppr id) $
    isJoinId id = False
  | otherwise   = isStrictType (idType id) ||
                  isStrUsedDmd (idDemandInfo id)
                  -- Take the best of both strictnesses - old and new

---------------------------------
-- UNFOLDING

-- | Returns the 'Id's unfolding, but does not expose the unfolding of a strong
-- loop breaker. See 'unfoldingInfo'.
--
-- If you really want the unfolding of a strong loopbreaker, call 'realIdUnfolding'.
idUnfolding :: Id -> Unfolding
idUnfolding id = unfoldingInfo (idInfo id)

realIdUnfolding :: Id -> Unfolding
-- ^ Expose the unfolding if there is one, including for loop breakers
realIdUnfolding id = realUnfoldingInfo (idInfo id)

setIdUnfolding :: Id -> Unfolding -> Id
setIdUnfolding id unfolding = modifyIdInfo (`setUnfoldingInfo` unfolding) id

idDemandInfo       :: Id -> Demand
idDemandInfo       id = demandInfo (idInfo id)

setIdDemandInfo :: Id -> Demand -> Id
setIdDemandInfo id dmd = modifyIdInfo (`setDemandInfo` dmd) id

setCaseBndrEvald :: StrictnessMark -> Id -> Id
-- Used for variables bound by a case expressions, both the case-binder
-- itself, and any pattern-bound variables that are argument of a
-- strict constructor.  It just marks the variable as already-evaluated,
-- so that (for example) a subsequent 'seq' can be dropped
setCaseBndrEvald str id
  | isMarkedStrict str = id `setIdUnfolding` evaldUnfolding
  | otherwise          = id

        ---------------------------------
        -- SPECIALISATION

-- See Note [Specialisations and RULES in IdInfo] in GHC.Types.Id.Info

idSpecialisation :: Id -> RuleInfo
idSpecialisation id = ruleInfo (idInfo id)

idCoreRules :: Id -> [CoreRule]
idCoreRules id = ruleInfoRules (idSpecialisation id)

idHasRules :: Id -> Bool
idHasRules id = not (isEmptyRuleInfo (idSpecialisation id))

setIdSpecialisation :: Id -> RuleInfo -> Id
setIdSpecialisation id spec_info = modifyIdInfo (`setRuleInfo` spec_info) id

        ---------------------------------
        -- CAF INFO
idCafInfo :: Id -> CafInfo
idCafInfo id = cafInfo (idInfo id)

setIdCafInfo :: Id -> CafInfo -> Id
setIdCafInfo id caf_info = modifyIdInfo (`setCafInfo` caf_info) id

        ---------------------------------
        -- Lambda form info

idLFInfo_maybe :: Id -> Maybe LambdaFormInfo
idLFInfo_maybe = lfInfo . idInfo

setIdLFInfo :: Id -> LambdaFormInfo -> Id
setIdLFInfo id lf = modifyIdInfo (`setLFInfo` lf) id

        ---------------------------------
        -- Occurrence INFO
idOccInfo :: Id -> OccInfo
idOccInfo id = occInfo (idInfo id)

setIdOccInfo :: Id -> OccInfo -> Id
setIdOccInfo id occ_info = modifyIdInfo (`setOccInfo` occ_info) id

zapIdOccInfo :: Id -> Id
zapIdOccInfo b = b `setIdOccInfo` noOccInfo

{-
        ---------------------------------
        -- INLINING
The inline pragma tells us to be very keen to inline this Id, but it's still
OK not to if optimisation is switched off.
-}

idInlinePragma :: Id -> InlinePragma
idInlinePragma id = inlinePragInfo (idInfo id)

setInlinePragma :: Id -> InlinePragma -> Id
setInlinePragma id prag = modifyIdInfo (`setInlinePragInfo` prag) id

modifyInlinePragma :: Id -> (InlinePragma -> InlinePragma) -> Id
modifyInlinePragma id fn = modifyIdInfo (\info -> info `setInlinePragInfo` (fn (inlinePragInfo info))) id

idInlineActivation :: Id -> Activation
idInlineActivation id = inlinePragmaActivation (idInlinePragma id)

setInlineActivation :: Id -> Activation -> Id
setInlineActivation id act = modifyInlinePragma id (\prag -> setInlinePragmaActivation prag act)

idRuleMatchInfo :: Id -> RuleMatchInfo
idRuleMatchInfo id = inlinePragmaRuleMatchInfo (idInlinePragma id)

isConLikeId :: Id -> Bool
isConLikeId id = isConLike (idRuleMatchInfo id)

{-
        ---------------------------------
        -- ONE-SHOT LAMBDAS
-}

idOneShotInfo :: Id -> OneShotInfo
idOneShotInfo id = oneShotInfo (idInfo id)

-- | Like 'idOneShotInfo', but taking the Horrible State Hack in to account
-- See Note [The state-transformer hack] in "GHC.Core.Opt.Arity"
idStateHackOneShotInfo :: Id -> OneShotInfo
idStateHackOneShotInfo id
    | isStateHackType (idType id) = stateHackOneShot
    | otherwise                   = idOneShotInfo id

-- | Returns whether the lambda associated with the 'Id' is certainly applied at most once
-- This one is the "business end", called externally.
-- It works on type variables as well as Ids, returning True
-- Its main purpose is to encapsulate the Horrible State Hack
-- See Note [The state-transformer hack] in "GHC.Core.Opt.Arity"
isOneShotBndr :: Var -> Bool
isOneShotBndr var
  | isTyVar var                              = True
  | OneShotLam <- idStateHackOneShotInfo var = True
  | otherwise                                = False

-- | Should we apply the state hack to values of this 'Type'?
stateHackOneShot :: OneShotInfo
stateHackOneShot = OneShotLam

typeOneShot :: Type -> OneShotInfo
typeOneShot ty
   | isStateHackType ty = stateHackOneShot
   | otherwise          = NoOneShotInfo

isStateHackType :: Type -> Bool
isStateHackType ty
  | unsafeHasNoStateHack
  = False
  | otherwise
  = case tyConAppTyCon_maybe ty of
        Just tycon -> tycon == statePrimTyCon
        _          -> False
        -- This is a gross hack.  It claims that
        -- every function over realWorldStatePrimTy is a one-shot
        -- function.  This is pretty true in practice, and makes a big
        -- difference.  For example, consider
        --      a `thenST` \ r -> ...E...
        -- The early full laziness pass, if it doesn't know that r is one-shot
        -- will pull out E (let's say it doesn't mention r) to give
        --      let lvl = E in a `thenST` \ r -> ...lvl...
        -- When `thenST` gets inlined, we end up with
        --      let lvl = E in \s -> case a s of (r, s') -> ...lvl...
        -- and we don't re-inline E.
        --
        -- It would be better to spot that r was one-shot to start with, but
        -- I don't want to rely on that.
        --
        -- Another good example is in fill_in in PrelPack.hs.  We should be able to
        -- spot that fill_in has arity 2 (and when Keith is done, we will) but we can't yet.

isProbablyOneShotLambda :: Id -> Bool
isProbablyOneShotLambda id = case idStateHackOneShotInfo id of
                               OneShotLam    -> True
                               NoOneShotInfo -> False

setOneShotLambda :: Id -> Id
setOneShotLambda id = modifyIdInfo (`setOneShotInfo` OneShotLam) id

clearOneShotLambda :: Id -> Id
clearOneShotLambda id = modifyIdInfo (`setOneShotInfo` NoOneShotInfo) id

setIdOneShotInfo :: Id -> OneShotInfo -> Id
setIdOneShotInfo id one_shot = modifyIdInfo (`setOneShotInfo` one_shot) id

updOneShotInfo :: Id -> OneShotInfo -> Id
-- Combine the info in the Id with new info
updOneShotInfo id one_shot
  | do_upd    = setIdOneShotInfo id one_shot
  | otherwise = id
  where
    do_upd = case (idOneShotInfo id, one_shot) of
                (NoOneShotInfo, _) -> True
                (OneShotLam,    _) -> False

-- The OneShotLambda functions simply fiddle with the IdInfo flag
-- But watch out: this may change the type of something else
--      f = \x -> e
-- If we change the one-shot-ness of x, f's type changes

zapInfo :: (IdInfo -> Maybe IdInfo) -> Id -> Id
zapInfo zapper id = maybeModifyIdInfo (zapper (idInfo id)) id

zapLamIdInfo :: Id -> Id
zapLamIdInfo = zapInfo zapLamInfo

zapFragileIdInfo :: Id -> Id
zapFragileIdInfo = zapInfo zapFragileInfo

zapIdDemandInfo :: Id -> Id
zapIdDemandInfo = zapInfo zapDemandInfo

zapIdUsageInfo :: Id -> Id
zapIdUsageInfo = zapInfo zapUsageInfo

zapIdUsageEnvInfo :: Id -> Id
zapIdUsageEnvInfo = zapInfo zapUsageEnvInfo

zapIdUsedOnceInfo :: Id -> Id
zapIdUsedOnceInfo = zapInfo zapUsedOnceInfo

zapIdTailCallInfo :: Id -> Id
zapIdTailCallInfo = zapInfo zapTailCallInfo

zapStableUnfolding :: Id -> Id
zapStableUnfolding id
 | isStableUnfolding (realIdUnfolding id) = setIdUnfolding id NoUnfolding
 | otherwise                              = id

{-
Note [transferPolyIdInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~
This transfer is used in three places:
        FloatOut (long-distance let-floating)
        GHC.Core.Opt.Simplify.Utils.abstractFloats (short-distance let-floating)
        StgLiftLams (selectively lambda-lift local functions to top-level)

Consider the short-distance let-floating:

   f = /\a. let g = rhs in ...

Then if we float thus

   g' = /\a. rhs
   f = /\a. ...[g' a/g]....

we *do not* want to lose g's
  * strictness information
  * arity
  * inline pragma (though that is bit more debatable)
  * occurrence info

Mostly this is just an optimisation, but it's *vital* to
transfer the occurrence info.  Consider

   NonRec { f = /\a. let Rec { g* = ..g.. } in ... }

where the '*' means 'LoopBreaker'.  Then if we float we must get

   Rec { g'* = /\a. ...(g' a)... }
   NonRec { f = /\a. ...[g' a/g]....}

where g' is also marked as LoopBreaker.  If not, terrible things
can happen if we re-simplify the binding (and the Simplifier does
sometimes simplify a term twice); see #4345.

It's not so simple to retain
  * worker info
  * rules
so we simply discard those.  Sooner or later this may bite us.

If we abstract wrt one or more *value* binders, we must modify the
arity and strictness info before transferring it.  E.g.
      f = \x. e
-->
      g' = \y. \x. e
      + substitute (g' y) for g
Notice that g' has an arity one more than the original g
-}

transferPolyIdInfo :: Id        -- Original Id
                   -> [Var]     -- Abstract wrt these variables
                   -> Id        -- New Id
                   -> Id
transferPolyIdInfo old_id abstract_wrt new_id
  = modifyIdInfo transfer new_id
  where
    arity_increase = count isId abstract_wrt    -- Arity increases by the
                                                -- number of value binders

    old_info        = idInfo old_id
    old_arity       = arityInfo old_info
    old_inline_prag = inlinePragInfo old_info
    old_occ_info    = occInfo old_info
    new_arity       = old_arity + arity_increase
    new_occ_info    = zapOccTailCallInfo old_occ_info

    old_strictness  = dmdSigInfo old_info
    new_strictness  = prependArgsDmdSig arity_increase old_strictness
    old_cpr         = cprSigInfo old_info

    transfer new_info = new_info `setArityInfo` new_arity
                                 `setInlinePragInfo` old_inline_prag
                                 `setOccInfo` new_occ_info
                                 `setDmdSigInfo` new_strictness
                                 `setCprSigInfo` old_cpr

isNeverRepPolyId :: Id -> Bool
isNeverRepPolyId = isNeverRepPolyIdInfo . idInfo
