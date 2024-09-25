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
        zapLamIdInfo, floatifyIdDemandInfo, zapIdUsageInfo, zapIdUsageEnvInfo,
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
        isDataConId, isDataConId_maybe,
        idDataCon,
        isConLikeId, isWorkerLikeId, isDeadEndId, idIsFrom,
        hasNoBinding,

        -- ** Join variables
        JoinId, JoinPointHood,
        isJoinId, idJoinPointHood, idJoinArity,
        asJoinId, asJoinId_maybe, zapJoinId,

        -- ** Inline pragma stuff
        idInlinePragma, setInlinePragma, modifyInlinePragma,
        idInlineActivation, setInlineActivation, idRuleMatchInfo,

        -- ** One-shot lambdas
        setOneShotLambda, clearOneShotLambda,
        updOneShotInfo, setIdOneShotInfo,

        -- ** Reading 'IdInfo' fields
        idArity,
        idCallArity, idFunRepArity,
        idSpecialisation, idCoreRules, idHasRules,
        idCafInfo, idLFInfo_maybe,
        idOneShotInfo,
        idOccInfo,

        IdUnfoldingFun, idUnfolding, realIdUnfolding,
        alwaysActiveUnfoldingFun, whenActiveUnfoldingFun, noUnfoldingFun,

        -- ** Writing 'IdInfo' fields
        setIdUnfolding, zapIdUnfolding, setCaseBndrEvald,
        setIdArity,
        setIdCallArity,

        setIdSpecialisation,
        setIdCafInfo,
        setIdOccInfo, zapIdOccInfo,
        setIdLFInfo,

        setIdDemandInfo,
        setIdDmdSig,
        setIdCprSig,
        setIdCbvMarks,
        idCbvMarks_maybe,
        idCbvMarkArity,
        asWorkerLikeId, asNonWorkerLikeId,

        idDemandInfo,
        idDmdSig,
        idCprSig,

        idTagSig_maybe,
        setIdTagSig
    ) where

import GHC.Prelude

import GHC.Core ( CoreRule, isStableUnfolding, evaldUnfolding
                , isCompulsoryUnfolding, Unfolding( NoUnfolding )
                , IdUnfoldingFun, isEvaldUnfolding, hasSomeUnfolding, noUnfolding )

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
import GHC.Core.Predicate( isCoVarType )
import GHC.Core.DataCon
import GHC.Core.Class
import GHC.Core.Multiplicity

import GHC.Types.RepType
import GHC.Types.Demand
import GHC.Types.Cpr
import GHC.Types.Name
import GHC.Types.ForeignCall
import GHC.Types.SrcLoc
import GHC.Types.Unique

import GHC.Stg.InferTags.TagSig

import GHC.Unit.Module
import {-# SOURCE #-} GHC.Builtin.PrimOps (PrimOp)
import GHC.Builtin.Uniques (mkBuiltinUnique)
import GHC.Types.Unique.Supply

import GHC.Data.Maybe
import GHC.Data.FastString

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

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
          `asJoinId_maybe`,
          `setIdCbvMarks`

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

-- maybeModifyIdInfo tries to avoid unnecessary thrashing
maybeModifyIdDetails :: Maybe IdDetails  -> Id -> Id
maybeModifyIdDetails (Just new_details) id = setIdDetails id new_details
maybeModifyIdDetails Nothing         id = id

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
mkVanillaGlobal :: HasDebugCallStack => Name -> Type -> Id
mkVanillaGlobal name ty = mkVanillaGlobalWithInfo name ty vanillaIdInfo

-- | Make a global 'Id' with no global information but some generic 'IdInfo'
mkVanillaGlobalWithInfo :: HasDebugCallStack => Name -> Type -> IdInfo -> Id
mkVanillaGlobalWithInfo nm =
  assertPpr (not $ isFieldNameSpace $ nameNameSpace nm)
    (text "mkVanillaGlobalWithInfo called on record field:" <+> ppr nm) $
    mkGlobalId VanillaId nm

-- | For an explanation of global vs. local 'Id's, see "GHC.Types.Var#globalvslocal"
mkLocalId :: HasDebugCallStack => Name -> Mult -> Type -> Id
mkLocalId name w ty = mkLocalIdWithInfo name w (assert (not (isCoVarType ty)) ty) vanillaIdInfo

-- | Make a local CoVar
mkLocalCoVar :: HasDebugCallStack => Name -> Type -> CoVar
mkLocalCoVar name ty
  = assert (isCoVarType ty) $
    Var.mkLocalVar CoVarId name ManyTy ty vanillaIdInfo

-- | Like 'mkLocalId', but checks the type to see if it should make a covar
mkLocalIdOrCoVar :: HasDebugCallStack => Name -> Mult -> Type -> Id
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
mkExportedVanillaId name ty =
  assertPpr (not $ isFieldNameSpace $ nameNameSpace name)
    (text "mkExportedVanillaId called on record field:" <+> ppr name) $
    Var.mkExportedLocalVar VanillaId name ty vanillaIdInfo
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
  = mkLocalId (mkDerivedInternalName mkWorkerOcc uniq (getName unwrkr)) ManyTy ty

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
                        _                                   -> False

isPatSynRecordSelector id = case Var.idDetails id of
                        RecSelId {sel_tycon = RecSelPatSyn _} -> True
                        _                                     -> False

isNaughtyRecordSelector id = case Var.idDetails id of
                        RecSelId { sel_naughty = n } -> n
                        _                            -> False

isClassOpId id = case Var.idDetails id of
                        ClassOpId {} -> True
                        _other       -> False

isClassOpId_maybe id = case Var.idDetails id of
                        ClassOpId cls _ -> Just cls
                        _other          -> Nothing

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

isDataConId :: Id -> Bool
isDataConId id = case Var.idDetails id of
                         DataConWorkId {} -> True
                         DataConWrapId {} -> True
                         _                 -> False

-- | An Id for which we might require all callers to pass strict arguments properly tagged + evaluated.
--
-- See Note [CBV Function Ids]
isWorkerLikeId :: Id -> Bool
isWorkerLikeId id = case Var.idDetails id of
  WorkerLikeId _  -> True
  JoinId _ Just{}   -> True
  _                 -> False

isJoinId :: Var -> Bool
-- It is convenient in GHC.Core.Opt.SetLevels.lvlMFE to apply isJoinId
-- to the free vars of an expression, so it's convenient
-- if it returns False for type variables
isJoinId id
  | isId id = case Var.idDetails id of
                JoinId {} -> True
                _         -> False
  | otherwise = False

-- | Doesn't return strictness marks
idJoinPointHood :: Var -> JoinPointHood
idJoinPointHood id
 | isId id  = case Var.idDetails id of
                JoinId arity _marks -> JoinPoint arity
                _                   -> NotJoinPoint
 | otherwise = NotJoinPoint

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
                        _                -> isCompulsoryUnfolding (realIdUnfolding id)
  -- Note: this function must be very careful not to force
  -- any of the fields that aren't the 'uf_src' field of
  -- the 'Unfolding' of the 'Id'. This is because these fields are computed
  -- in terms of the 'uf_tmpl' field, which is not available
  -- until we have finished Core Lint for the unfolding, which calls 'hasNoBinding'
  -- in 'checkCanEtaExpand'.
  --
  -- In particular, calling 'idUnfolding' rather than 'realIdUnfolding' here can
  -- force the 'uf_tmpl' field, because 'trimUnfolding' forces the 'uf_is_value' field,
  -- and this field is usually computed in terms of the 'uf_tmpl' field,
  -- so we will force that as well.
  --
  -- See Note [Lazily checking Unfoldings] in GHC.IfaceToCore.

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
idJoinArity id = case idJoinPointHood id of
                   JoinPoint ar -> ar
                   NotJoinPoint -> pprPanic "idJoinArity" (ppr id)

asJoinId :: Id -> JoinArity -> JoinId
asJoinId id arity = warnPprTrace (not (isLocalId id))
                         "global id being marked as join var"  (ppr id) $
                    warnPprTrace (not (is_vanilla_or_join id))
                         "asJoinId"
                         (ppr id <+> pprIdDetails (idDetails id)) $
                    id `setIdDetails` JoinId arity (idCbvMarks_maybe id)
  where
    is_vanilla_or_join id = case Var.idDetails id of
                              VanillaId -> True
                              -- Can workers become join ids? Yes!
                              WorkerLikeId {} -> pprTraceDebug "asJoinId (call by value function)" (ppr id) True
                              JoinId {} -> True
                              _         -> False

zapJoinId :: Id -> Id
-- May be a regular id already
zapJoinId jid | isJoinId jid = zapIdTailCallInfo (newIdDetails `seq` jid `setIdDetails` newIdDetails)
                                 -- Core Lint may complain if still marked
                                 -- as AlwaysTailCalled
              | otherwise    = jid
              where
                newIdDetails = case idDetails jid of
                  -- We treat join points as CBV functions. Even after they are floated out.
                  -- See Note [Use CBV semantics only for join points and workers]
                  JoinId _ (Just marks) -> WorkerLikeId marks
                  JoinId _ Nothing      -> WorkerLikeId []
                  _                     -> panic "zapJoinId: newIdDetails can only be used if Id was a join Id."


asJoinId_maybe :: Id -> JoinPointHood -> Id
asJoinId_maybe id (JoinPoint arity) = asJoinId id arity
asJoinId_maybe id NotJoinPoint      = zapJoinId id

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

-- | This function counts all arguments post-unarisation, which includes
-- arguments with no runtime representation -- see Note [Unarisation and arity]
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

-- | `isStrictId` says whether either
--   (a) the 'Id' has a strict demand placed on it or
--   (b) definitely has a \"strict type\", such that it can always be
--       evaluated strictly (i.e an unlifted type)
-- We need to check (b) as well as (a), because when the demand for the
-- given `id` hasn't been computed yet but `id` has a strict
-- type, we still want `isStrictId id` to be `True`.
-- Returns False if the type is levity polymorphic; False is always safe.
isStrictId :: Id -> Bool
isStrictId id
  | assertPpr (isId id) (text "isStrictId: not an id: " <+> ppr id) $
    isJoinId id = False
  | otherwise   = isStrictType (idType id) ||
                  isStrUsedDmd (idDemandInfo id)
                  -- Take the best of both strictnesses - old and new

idTagSig_maybe :: Id -> Maybe TagSig
idTagSig_maybe = tagSig . idInfo

---------------------------------
-- UNFOLDING

-- | Returns the 'Id's unfolding, but does not expose the unfolding of a strong
-- loop breaker. See 'unfoldingInfo'.
--
-- If you really want the unfolding of a strong loopbreaker, call 'realIdUnfolding'.
idUnfolding :: IdUnfoldingFun
idUnfolding id = unfoldingInfo (idInfo id)

noUnfoldingFun :: IdUnfoldingFun
noUnfoldingFun _id = noUnfolding

-- | Returns an unfolding only if
--   (a) not a strong loop breaker and
--   (b) always active
alwaysActiveUnfoldingFun :: IdUnfoldingFun
alwaysActiveUnfoldingFun id
  | isAlwaysActive (idInlineActivation id) = idUnfolding id
  | otherwise                              = noUnfolding

-- | Returns an unfolding only if
--   (a) not a strong loop breaker and
--   (b) active in according to is_active
whenActiveUnfoldingFun :: (Activation -> Bool) -> IdUnfoldingFun
whenActiveUnfoldingFun is_active id
  | is_active (idInlineActivation id) = idUnfolding id
  | otherwise                         = NoUnfolding

realIdUnfolding :: Id -> Unfolding
-- ^ Expose the unfolding if there is one, including for loop breakers
realIdUnfolding id = realUnfoldingInfo (idInfo id)

setIdUnfolding :: Id -> Unfolding -> Id
setIdUnfolding id unfolding = modifyIdInfo (`setUnfoldingInfo` unfolding) id

idDemandInfo       :: Id -> Demand
idDemandInfo       id = demandInfo (idInfo id)

setIdDemandInfo :: Id -> Demand -> Id
setIdDemandInfo id dmd = modifyIdInfo (`setDemandInfo` dmd) id

setIdTagSig :: Id -> TagSig -> Id
setIdTagSig id sig = modifyIdInfo (`setTagSig` sig) id

-- | If all marks are NotMarkedStrict we just set nothing.
setIdCbvMarks :: Id -> [CbvMark] -> Id
setIdCbvMarks id marks
  | not (any isMarkedCbv marks) = id
  | otherwise =
      -- pprTrace "setMarks:" (ppr id <> text ":" <> ppr marks) $
      case idDetails id of
        -- good ol (likely worker) function
        VanillaId ->      id `setIdDetails` (WorkerLikeId trimmedMarks)
        JoinId arity _ -> id `setIdDetails` (JoinId arity (Just trimmedMarks))
        -- Updating an existing call by value function.
        WorkerLikeId _ -> id `setIdDetails` (WorkerLikeId trimmedMarks)
        -- Do nothing for these
        RecSelId{} -> id
        DFunId{} -> id
        _ -> pprTrace "setIdCbvMarks: Unable to set cbv marks for" (ppr id $$
              text "marks:" <> ppr marks $$
              text "idDetails:" <> ppr (idDetails id)) id

    where
      -- (Currently) no point in passing args beyond the arity unlifted.
      -- We would have to eta expand all call sites to (length marks).
      -- Perhaps that's sensible but for now be conservative.
      -- Similarly we don't need any lazy marks at the end of the list.
      -- This way the length of the list is always exactly number of arguments
      -- that must be visible to CodeGen. See See Note [CBV Function Ids]
      -- for more details.
      trimmedMarks = dropWhileEndLE (not . isMarkedCbv) $ take (idArity id) marks

idCbvMarks_maybe :: Id -> Maybe [CbvMark]
idCbvMarks_maybe id = case idDetails id of
  WorkerLikeId marks -> Just marks
  JoinId _arity marks  -> marks
  _                    -> Nothing

-- Id must be called with at least this arity in order to allow arguments to
-- be passed unlifted.
idCbvMarkArity :: Id -> Arity
idCbvMarkArity fn = maybe 0 length (idCbvMarks_maybe fn)

-- | Remove any cbv marks on arguments from a given Id.
asNonWorkerLikeId :: Id -> Id
asNonWorkerLikeId id =
  let details = case idDetails id of
        WorkerLikeId{}      -> Just $ VanillaId
        JoinId arity Just{} -> Just $ JoinId arity Nothing
        _                   -> Nothing
  in maybeModifyIdDetails details id

-- | Turn this id into a WorkerLikeId if possible.
asWorkerLikeId :: Id -> Id
asWorkerLikeId id =
  let details = case idDetails id of
        WorkerLikeId{}        -> Nothing
        JoinId _arity Just{}  -> Nothing
        JoinId arity Nothing  -> Just (JoinId arity (Just []))
        VanillaId             -> Just $ WorkerLikeId []
        _                     -> Nothing
  in maybeModifyIdDetails details id

setCaseBndrEvald :: StrictnessMark -> Id -> Id
-- Used for variables bound by a case expressions, both the case-binder
-- itself, and any pattern-bound variables that are argument of a
-- strict constructor.  It just marks the variable as already-evaluated,
-- so that (for example) a subsequent 'seq' can be dropped
setCaseBndrEvald str id
  | isMarkedStrict str = id `setIdUnfolding` evaldUnfolding
  | otherwise          = id

-- | Similar to trimUnfolding, but also removes evaldness info.
zapIdUnfolding :: Id -> Id
zapIdUnfolding v
  | isId v, hasSomeUnfolding (idUnfolding v) = setIdUnfolding v noUnfolding
  | otherwise = v

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

setOneShotLambda :: Id -> Id
setOneShotLambda id = modifyIdInfo (`setOneShotInfo` OneShotLam) id

clearOneShotLambda :: Id -> Id
clearOneShotLambda id = modifyIdInfo (`setOneShotInfo` NoOneShotInfo) id

setIdOneShotInfo :: Id -> OneShotInfo -> Id
setIdOneShotInfo id one_shot = modifyIdInfo (`setOneShotInfo` one_shot) id

updOneShotInfo :: Id -> OneShotInfo -> Id
-- Combine the info in the Id with new info
updOneShotInfo id one_shot
  | OneShotLam <- one_shot
  , NoOneShotInfo <- idOneShotInfo id
  = setIdOneShotInfo id OneShotLam
  | otherwise
  = id

-- The OneShotLambda functions simply fiddle with the IdInfo flag
-- But watch out: this may change the type of something else
--      f = \x -> e
-- If we change the one-shot-ness of x, f's type changes

-- Replaces the id info if the zapper returns @Just idinfo@
zapInfo :: (IdInfo -> Maybe IdInfo) -> Id -> Id
zapInfo zapper id = maybeModifyIdInfo (zapper (idInfo id)) id

zapLamIdInfo :: Id -> Id
zapLamIdInfo = zapInfo zapLamInfo

zapFragileIdInfo :: Id -> Id
zapFragileIdInfo = zapInfo zapFragileInfo

floatifyIdDemandInfo :: Id -> Id
-- See Note [Floatifying demand info when floating] in GHC.Core.Opt.SetLevels
floatifyIdDemandInfo = zapInfo floatifyDemandInfo

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
  = modifyIdInfo transfer new_id `setIdCbvMarks` new_cbv_marks
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
    new_cpr         = prependArgsCprSig arity_increase old_cpr

    old_cbv_marks   = fromMaybe (replicate old_arity NotMarkedCbv) (idCbvMarks_maybe old_id)
    abstr_cbv_marks = mapMaybe getMark abstract_wrt
    new_cbv_marks   = abstr_cbv_marks ++ old_cbv_marks

    getMark v
      | not (isId v)
      = Nothing
      | isId v
      , isEvaldUnfolding (idUnfolding v)
      , mightBeLiftedType (idType v)
      = Just MarkedCbv
      | otherwise = Just NotMarkedCbv
    transfer new_info = new_info `setArityInfo`      new_arity
                                 `setInlinePragInfo` old_inline_prag
                                 `setOccInfo`        new_occ_info
                                 `setDmdSigInfo`     new_strictness
                                 `setCprSigInfo`     new_cpr
