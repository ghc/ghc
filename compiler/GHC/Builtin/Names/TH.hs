-- %************************************************************************
-- %*                                                                   *
--              The known-key names for Template Haskell
-- %*                                                                   *
-- %************************************************************************

module GHC.Builtin.Names.TH where

import GHC.Prelude ()

import GHC.Builtin.Names( mk_known_key_name )
import GHC.Unit.Types
import GHC.Unit.Module.Name
import GHC.Types.Name( Name )
import GHC.Types.Name.Occurrence( tcName, clsName, dataName, varName )
import GHC.Types.Name.Reader( RdrName, nameRdrName )
import GHC.Types.Unique
import GHC.Builtin.Uniques
import GHC.Data.FastString

-- To add a name, do three things
--
--  1) Allocate a key
--  2) Make a "Name"
--  3) Add the name to templateHaskellNames

templateHaskellNames :: [Name]
-- The names that are implicitly mentioned by ``bracket''
-- Should stay in sync with the import list of GHC.HsToCore.Quote

templateHaskellNames = [
    returnQName, bindQName, sequenceQName, newNameName, liftName, liftTypedName,
    mkNameName, mkNameG_vName, mkNameG_dName, mkNameG_tcName, mkNameLName,
    mkNameSName,
    mkModNameName,
    liftStringName,
    unTypeName, unTypeCodeName,
    unsafeCodeCoerceName,

    -- Lit
    charLName, stringLName, integerLName, intPrimLName, wordPrimLName,
    floatPrimLName, doublePrimLName, rationalLName, stringPrimLName,
    charPrimLName,
    -- Pat
    litPName, varPName, tupPName, unboxedTupPName, unboxedSumPName,
    conPName, tildePName, bangPName, infixPName,
    asPName, wildPName, recPName, listPName, sigPName, viewPName,
    -- FieldPat
    fieldPatName,
    -- Match
    matchName,
    -- Clause
    clauseName,
    -- Exp
    varEName, conEName, litEName, appEName, appTypeEName, infixEName,
    infixAppName, sectionLName, sectionRName, lamEName, lamCaseEName,
    tupEName, unboxedTupEName, unboxedSumEName,
    condEName, multiIfEName, letEName, caseEName, doEName, mdoEName, compEName,
    fromEName, fromThenEName, fromToEName, fromThenToEName,
    listEName, sigEName, recConEName, recUpdEName, staticEName, unboundVarEName,
    labelEName, implicitParamVarEName,
    -- FieldExp
    fieldExpName,
    -- Body
    guardedBName, normalBName,
    -- Guard
    normalGEName, patGEName,
    -- Stmt
    bindSName, letSName, noBindSName, parSName, recSName,
    -- Dec
    funDName, valDName, dataDName, newtypeDName, tySynDName,
    classDName, instanceWithOverlapDName,
    standaloneDerivWithStrategyDName, sigDName, kiSigDName, forImpDName,
    pragInlDName, pragSpecDName, pragSpecInlDName, pragSpecInstDName,
    pragRuleDName, pragCompleteDName, pragAnnDName, defaultSigDName,
    dataFamilyDName, openTypeFamilyDName, closedTypeFamilyDName,
    dataInstDName, newtypeInstDName, tySynInstDName,
    infixLDName, infixRDName, infixNDName,
    roleAnnotDName, patSynDName, patSynSigDName,
    implicitParamBindDName,
    -- Cxt
    cxtName,

    -- SourceUnpackedness
    noSourceUnpackednessName, sourceNoUnpackName, sourceUnpackName,
    -- SourceStrictness
    noSourceStrictnessName, sourceLazyName, sourceStrictName,
    -- Con
    normalCName, recCName, infixCName, forallCName, gadtCName, recGadtCName,
    -- Bang
    bangName,
    -- BangType
    bangTypeName,
    -- VarBangType
    varBangTypeName,
    -- PatSynDir (for pattern synonyms)
    unidirPatSynName, implBidirPatSynName, explBidirPatSynName,
    -- PatSynArgs (for pattern synonyms)
    prefixPatSynName, infixPatSynName, recordPatSynName,
    -- Type
    forallTName, forallVisTName, varTName, conTName, infixTName, appTName,
    appKindTName, equalityTName, tupleTName, unboxedTupleTName,
    unboxedSumTName, arrowTName, mulArrowTName, listTName, sigTName, litTName,
    promotedTName, promotedTupleTName, promotedNilTName, promotedConsTName,
    wildCardTName, implicitParamTName,
    -- TyLit
    numTyLitName, strTyLitName, charTyLitName,
    -- TyVarBndr
    plainTVName, kindedTVName,
    plainInvisTVName, kindedInvisTVName,
    -- Specificity
    specifiedSpecName, inferredSpecName,
    -- Role
    nominalRName, representationalRName, phantomRName, inferRName,
    -- Kind
    starKName, constraintKName,
    -- FamilyResultSig
    noSigName, kindSigName, tyVarSigName,
    -- InjectivityAnn
    injectivityAnnName,
    -- Callconv
    cCallName, stdCallName, cApiCallName, primCallName, javaScriptCallName,
    -- Safety
    unsafeName,
    safeName,
    interruptibleName,
    -- Inline
    noInlineDataConName, inlineDataConName, inlinableDataConName,
    -- RuleMatch
    conLikeDataConName, funLikeDataConName,
    -- Phases
    allPhasesDataConName, fromPhaseDataConName, beforePhaseDataConName,
    -- Overlap
    overlappableDataConName, overlappingDataConName, overlapsDataConName,
    incoherentDataConName,
    -- DerivStrategy
    stockStrategyName, anyclassStrategyName,
    newtypeStrategyName, viaStrategyName,
    -- RuleBndr
    ruleVarName, typedRuleVarName,
    -- FunDep
    funDepName,
    -- TySynEqn
    tySynEqnName,
    -- AnnTarget
    valueAnnotationName, typeAnnotationName, moduleAnnotationName,
    -- DerivClause
    derivClauseName,

    -- The type classes
    liftClassName, quoteClassName,

    -- And the tycons
    qTyConName, nameTyConName, patTyConName, fieldPatTyConName, matchTyConName,
    expQTyConName, fieldExpTyConName, predTyConName,
    stmtTyConName,  decsTyConName, conTyConName, bangTypeTyConName,
    varBangTypeTyConName, typeQTyConName, expTyConName, decTyConName,
    typeTyConName, tyVarBndrUnitTyConName, tyVarBndrSpecTyConName, clauseTyConName,
    patQTyConName, funDepTyConName, decsQTyConName,
    ruleBndrTyConName, tySynEqnTyConName,
    roleTyConName, codeTyConName, injAnnTyConName, kindTyConName,
    overlapTyConName, derivClauseTyConName, derivStrategyTyConName,
    modNameTyConName,

    -- Quasiquoting
    quoteDecName, quoteTypeName, quoteExpName, quotePatName]

thSyn, thLib, qqLib :: Module
thSyn = mkTHModule (fsLit "Language.Haskell.TH.Syntax")
thLib = mkTHModule (fsLit "Language.Haskell.TH.Lib.Internal")
qqLib = mkTHModule (fsLit "Language.Haskell.TH.Quote")

mkTHModule :: FastString -> Module
mkTHModule m = mkModule thUnit (mkModuleNameFS m)

libFun, libTc, thFun, thTc, thCls, thCon, qqFun :: FastString -> Unique -> Name
libFun = mk_known_key_name varName  thLib
libTc  = mk_known_key_name tcName   thLib
thFun  = mk_known_key_name varName  thSyn
thTc   = mk_known_key_name tcName   thSyn
thCls  = mk_known_key_name clsName  thSyn
thCon  = mk_known_key_name dataName thSyn
qqFun  = mk_known_key_name varName  qqLib

-------------------- TH.Syntax -----------------------
liftClassName :: Name
liftClassName = thCls (fsLit "Lift") liftClassKey

quoteClassName :: Name
quoteClassName = thCls (fsLit "Quote") quoteClassKey

qTyConName, nameTyConName, fieldExpTyConName, patTyConName,
    fieldPatTyConName, expTyConName, decTyConName, typeTyConName,
    matchTyConName, clauseTyConName, funDepTyConName, predTyConName,
    codeTyConName, injAnnTyConName, overlapTyConName, decsTyConName,
    modNameTyConName :: Name
qTyConName             = thTc (fsLit "Q")              qTyConKey
nameTyConName          = thTc (fsLit "Name")           nameTyConKey
fieldExpTyConName      = thTc (fsLit "FieldExp")       fieldExpTyConKey
patTyConName           = thTc (fsLit "Pat")            patTyConKey
fieldPatTyConName      = thTc (fsLit "FieldPat")       fieldPatTyConKey
expTyConName           = thTc (fsLit "Exp")            expTyConKey
decTyConName           = thTc (fsLit "Dec")            decTyConKey
decsTyConName          = libTc (fsLit "Decs")           decsTyConKey
typeTyConName          = thTc (fsLit "Type")           typeTyConKey
matchTyConName         = thTc (fsLit "Match")          matchTyConKey
clauseTyConName        = thTc (fsLit "Clause")         clauseTyConKey
funDepTyConName        = thTc (fsLit "FunDep")         funDepTyConKey
predTyConName          = thTc (fsLit "Pred")           predTyConKey
codeTyConName          = thTc (fsLit "Code")           codeTyConKey
injAnnTyConName        = thTc (fsLit "InjectivityAnn") injAnnTyConKey
overlapTyConName       = thTc (fsLit "Overlap")        overlapTyConKey
modNameTyConName       = thTc (fsLit "ModName")        modNameTyConKey

returnQName, bindQName, sequenceQName, newNameName, liftName,
    mkNameName, mkNameG_vName, mkNameG_dName, mkNameG_tcName,
    mkNameLName, mkNameSName, liftStringName, unTypeName, unTypeCodeName,
    unsafeCodeCoerceName, liftTypedName, mkModNameName :: Name
returnQName    = thFun (fsLit "returnQ")   returnQIdKey
bindQName      = thFun (fsLit "bindQ")     bindQIdKey
sequenceQName  = thFun (fsLit "sequenceQ") sequenceQIdKey
newNameName    = thFun (fsLit "newName")   newNameIdKey
liftName       = thFun (fsLit "lift")      liftIdKey
liftStringName = thFun (fsLit "liftString")  liftStringIdKey
mkNameName     = thFun (fsLit "mkName")     mkNameIdKey
mkNameG_vName  = thFun (fsLit "mkNameG_v")  mkNameG_vIdKey
mkNameG_dName  = thFun (fsLit "mkNameG_d")  mkNameG_dIdKey
mkNameG_tcName = thFun (fsLit "mkNameG_tc") mkNameG_tcIdKey
mkNameLName    = thFun (fsLit "mkNameL")    mkNameLIdKey
mkNameSName    = thFun (fsLit "mkNameS")    mkNameSIdKey
mkModNameName  = thFun (fsLit "mkModName")  mkModNameIdKey
unTypeName     = thFun (fsLit "unType")     unTypeIdKey
unTypeCodeName    = thFun (fsLit "unTypeCode") unTypeCodeIdKey
unsafeCodeCoerceName = thFun (fsLit "unsafeCodeCoerce") unsafeCodeCoerceIdKey
liftTypedName = thFun (fsLit "liftTyped") liftTypedIdKey


-------------------- TH.Lib -----------------------
-- data Lit = ...
charLName, stringLName, integerLName, intPrimLName, wordPrimLName,
    floatPrimLName, doublePrimLName, rationalLName, stringPrimLName,
    charPrimLName :: Name
charLName       = libFun (fsLit "charL")       charLIdKey
stringLName     = libFun (fsLit "stringL")     stringLIdKey
integerLName    = libFun (fsLit "integerL")    integerLIdKey
intPrimLName    = libFun (fsLit "intPrimL")    intPrimLIdKey
wordPrimLName   = libFun (fsLit "wordPrimL")   wordPrimLIdKey
floatPrimLName  = libFun (fsLit "floatPrimL")  floatPrimLIdKey
doublePrimLName = libFun (fsLit "doublePrimL") doublePrimLIdKey
rationalLName   = libFun (fsLit "rationalL")     rationalLIdKey
stringPrimLName = libFun (fsLit "stringPrimL") stringPrimLIdKey
charPrimLName   = libFun (fsLit "charPrimL")   charPrimLIdKey

-- data Pat = ...
litPName, varPName, tupPName, unboxedTupPName, unboxedSumPName, conPName,
    infixPName, tildePName, bangPName, asPName, wildPName, recPName, listPName,
    sigPName, viewPName :: Name
litPName   = libFun (fsLit "litP")   litPIdKey
varPName   = libFun (fsLit "varP")   varPIdKey
tupPName   = libFun (fsLit "tupP")   tupPIdKey
unboxedTupPName = libFun (fsLit "unboxedTupP") unboxedTupPIdKey
unboxedSumPName = libFun (fsLit "unboxedSumP") unboxedSumPIdKey
conPName   = libFun (fsLit "conP")   conPIdKey
infixPName = libFun (fsLit "infixP") infixPIdKey
tildePName = libFun (fsLit "tildeP") tildePIdKey
bangPName  = libFun (fsLit "bangP")  bangPIdKey
asPName    = libFun (fsLit "asP")    asPIdKey
wildPName  = libFun (fsLit "wildP")  wildPIdKey
recPName   = libFun (fsLit "recP")   recPIdKey
listPName  = libFun (fsLit "listP")  listPIdKey
sigPName   = libFun (fsLit "sigP")   sigPIdKey
viewPName  = libFun (fsLit "viewP")  viewPIdKey

-- type FieldPat = ...
fieldPatName :: Name
fieldPatName = libFun (fsLit "fieldPat") fieldPatIdKey

-- data Match = ...
matchName :: Name
matchName = libFun (fsLit "match") matchIdKey

-- data Clause = ...
clauseName :: Name
clauseName = libFun (fsLit "clause") clauseIdKey

-- data Exp = ...
varEName, conEName, litEName, appEName, appTypeEName, infixEName, infixAppName,
    sectionLName, sectionRName, lamEName, lamCaseEName, tupEName,
    unboxedTupEName, unboxedSumEName, condEName, multiIfEName, letEName,
    caseEName, doEName, mdoEName, compEName, staticEName, unboundVarEName,
    labelEName, implicitParamVarEName :: Name
varEName              = libFun (fsLit "varE")              varEIdKey
conEName              = libFun (fsLit "conE")              conEIdKey
litEName              = libFun (fsLit "litE")              litEIdKey
appEName              = libFun (fsLit "appE")              appEIdKey
appTypeEName          = libFun (fsLit "appTypeE")          appTypeEIdKey
infixEName            = libFun (fsLit "infixE")            infixEIdKey
infixAppName          = libFun (fsLit "infixApp")          infixAppIdKey
sectionLName          = libFun (fsLit "sectionL")          sectionLIdKey
sectionRName          = libFun (fsLit "sectionR")          sectionRIdKey
lamEName              = libFun (fsLit "lamE")              lamEIdKey
lamCaseEName          = libFun (fsLit "lamCaseE")          lamCaseEIdKey
tupEName              = libFun (fsLit "tupE")              tupEIdKey
unboxedTupEName       = libFun (fsLit "unboxedTupE")       unboxedTupEIdKey
unboxedSumEName       = libFun (fsLit "unboxedSumE")       unboxedSumEIdKey
condEName             = libFun (fsLit "condE")             condEIdKey
multiIfEName          = libFun (fsLit "multiIfE")          multiIfEIdKey
letEName              = libFun (fsLit "letE")              letEIdKey
caseEName             = libFun (fsLit "caseE")             caseEIdKey
doEName               = libFun (fsLit "doE")               doEIdKey
mdoEName              = libFun (fsLit "mdoE")              mdoEIdKey
compEName             = libFun (fsLit "compE")             compEIdKey
-- ArithSeq skips a level
fromEName, fromThenEName, fromToEName, fromThenToEName :: Name
fromEName             = libFun (fsLit "fromE")             fromEIdKey
fromThenEName         = libFun (fsLit "fromThenE")         fromThenEIdKey
fromToEName           = libFun (fsLit "fromToE")           fromToEIdKey
fromThenToEName       = libFun (fsLit "fromThenToE")       fromThenToEIdKey
-- end ArithSeq
listEName, sigEName, recConEName, recUpdEName :: Name
listEName             = libFun (fsLit "listE")             listEIdKey
sigEName              = libFun (fsLit "sigE")              sigEIdKey
recConEName           = libFun (fsLit "recConE")           recConEIdKey
recUpdEName           = libFun (fsLit "recUpdE")           recUpdEIdKey
staticEName           = libFun (fsLit "staticE")           staticEIdKey
unboundVarEName       = libFun (fsLit "unboundVarE")       unboundVarEIdKey
labelEName            = libFun (fsLit "labelE")            labelEIdKey
implicitParamVarEName = libFun (fsLit "implicitParamVarE") implicitParamVarEIdKey

-- type FieldExp = ...
fieldExpName :: Name
fieldExpName = libFun (fsLit "fieldExp") fieldExpIdKey

-- data Body = ...
guardedBName, normalBName :: Name
guardedBName = libFun (fsLit "guardedB") guardedBIdKey
normalBName  = libFun (fsLit "normalB")  normalBIdKey

-- data Guard = ...
normalGEName, patGEName :: Name
normalGEName = libFun (fsLit "normalGE") normalGEIdKey
patGEName    = libFun (fsLit "patGE")    patGEIdKey

-- data Stmt = ...
bindSName, letSName, noBindSName, parSName, recSName :: Name
bindSName   = libFun (fsLit "bindS")   bindSIdKey
letSName    = libFun (fsLit "letS")    letSIdKey
noBindSName = libFun (fsLit "noBindS") noBindSIdKey
parSName    = libFun (fsLit "parS")    parSIdKey
recSName    = libFun (fsLit "recS")    recSIdKey

-- data Dec = ...
funDName, valDName, dataDName, newtypeDName, tySynDName, classDName,
    instanceWithOverlapDName, sigDName, kiSigDName, forImpDName, pragInlDName,
    pragSpecDName, pragSpecInlDName, pragSpecInstDName, pragRuleDName,
    pragAnnDName, standaloneDerivWithStrategyDName, defaultSigDName,
    dataInstDName, newtypeInstDName, tySynInstDName, dataFamilyDName,
    openTypeFamilyDName, closedTypeFamilyDName, infixLDName, infixRDName,
    infixNDName, roleAnnotDName, patSynDName, patSynSigDName,
    pragCompleteDName, implicitParamBindDName :: Name
funDName                         = libFun (fsLit "funD")                         funDIdKey
valDName                         = libFun (fsLit "valD")                         valDIdKey
dataDName                        = libFun (fsLit "dataD")                        dataDIdKey
newtypeDName                     = libFun (fsLit "newtypeD")                     newtypeDIdKey
tySynDName                       = libFun (fsLit "tySynD")                       tySynDIdKey
classDName                       = libFun (fsLit "classD")                       classDIdKey
instanceWithOverlapDName         = libFun (fsLit "instanceWithOverlapD")         instanceWithOverlapDIdKey
standaloneDerivWithStrategyDName = libFun (fsLit "standaloneDerivWithStrategyD") standaloneDerivWithStrategyDIdKey
sigDName                         = libFun (fsLit "sigD")                         sigDIdKey
kiSigDName                       = libFun (fsLit "kiSigD")                       kiSigDIdKey
defaultSigDName                  = libFun (fsLit "defaultSigD")                  defaultSigDIdKey
forImpDName                      = libFun (fsLit "forImpD")                      forImpDIdKey
pragInlDName                     = libFun (fsLit "pragInlD")                     pragInlDIdKey
pragSpecDName                    = libFun (fsLit "pragSpecD")                    pragSpecDIdKey
pragSpecInlDName                 = libFun (fsLit "pragSpecInlD")                 pragSpecInlDIdKey
pragSpecInstDName                = libFun (fsLit "pragSpecInstD")                pragSpecInstDIdKey
pragRuleDName                    = libFun (fsLit "pragRuleD")                    pragRuleDIdKey
pragCompleteDName                = libFun (fsLit "pragCompleteD")                pragCompleteDIdKey
pragAnnDName                     = libFun (fsLit "pragAnnD")                     pragAnnDIdKey
dataInstDName                    = libFun (fsLit "dataInstD")                    dataInstDIdKey
newtypeInstDName                 = libFun (fsLit "newtypeInstD")                 newtypeInstDIdKey
tySynInstDName                   = libFun (fsLit "tySynInstD")                   tySynInstDIdKey
openTypeFamilyDName              = libFun (fsLit "openTypeFamilyD")              openTypeFamilyDIdKey
closedTypeFamilyDName            = libFun (fsLit "closedTypeFamilyD")            closedTypeFamilyDIdKey
dataFamilyDName                  = libFun (fsLit "dataFamilyD")                  dataFamilyDIdKey
infixLDName                      = libFun (fsLit "infixLD")                      infixLDIdKey
infixRDName                      = libFun (fsLit "infixRD")                      infixRDIdKey
infixNDName                      = libFun (fsLit "infixND")                      infixNDIdKey
roleAnnotDName                   = libFun (fsLit "roleAnnotD")                   roleAnnotDIdKey
patSynDName                      = libFun (fsLit "patSynD")                      patSynDIdKey
patSynSigDName                   = libFun (fsLit "patSynSigD")                   patSynSigDIdKey
implicitParamBindDName           = libFun (fsLit "implicitParamBindD")           implicitParamBindDIdKey

-- type Ctxt = ...
cxtName :: Name
cxtName = libFun (fsLit "cxt") cxtIdKey

-- data SourceUnpackedness = ...
noSourceUnpackednessName, sourceNoUnpackName, sourceUnpackName :: Name
noSourceUnpackednessName = libFun (fsLit "noSourceUnpackedness") noSourceUnpackednessKey
sourceNoUnpackName       = libFun (fsLit "sourceNoUnpack")       sourceNoUnpackKey
sourceUnpackName         = libFun (fsLit "sourceUnpack")         sourceUnpackKey

-- data SourceStrictness = ...
noSourceStrictnessName, sourceLazyName, sourceStrictName :: Name
noSourceStrictnessName = libFun (fsLit "noSourceStrictness") noSourceStrictnessKey
sourceLazyName         = libFun (fsLit "sourceLazy")         sourceLazyKey
sourceStrictName       = libFun (fsLit "sourceStrict")       sourceStrictKey

-- data Con = ...
normalCName, recCName, infixCName, forallCName, gadtCName, recGadtCName :: Name
normalCName  = libFun (fsLit "normalC" ) normalCIdKey
recCName     = libFun (fsLit "recC"    ) recCIdKey
infixCName   = libFun (fsLit "infixC"  ) infixCIdKey
forallCName  = libFun (fsLit "forallC" ) forallCIdKey
gadtCName    = libFun (fsLit "gadtC"   ) gadtCIdKey
recGadtCName = libFun (fsLit "recGadtC") recGadtCIdKey

-- data Bang = ...
bangName :: Name
bangName = libFun (fsLit "bang") bangIdKey

-- type BangType = ...
bangTypeName :: Name
bangTypeName = libFun (fsLit "bangType") bangTKey

-- type VarBangType = ...
varBangTypeName :: Name
varBangTypeName = libFun (fsLit "varBangType") varBangTKey

-- data PatSynDir = ...
unidirPatSynName, implBidirPatSynName, explBidirPatSynName :: Name
unidirPatSynName    = libFun (fsLit "unidir")    unidirPatSynIdKey
implBidirPatSynName = libFun (fsLit "implBidir") implBidirPatSynIdKey
explBidirPatSynName = libFun (fsLit "explBidir") explBidirPatSynIdKey

-- data PatSynArgs = ...
prefixPatSynName, infixPatSynName, recordPatSynName :: Name
prefixPatSynName = libFun (fsLit "prefixPatSyn") prefixPatSynIdKey
infixPatSynName  = libFun (fsLit "infixPatSyn")  infixPatSynIdKey
recordPatSynName = libFun (fsLit "recordPatSyn") recordPatSynIdKey

-- data Type = ...
forallTName, forallVisTName, varTName, conTName, infixTName, tupleTName,
    unboxedTupleTName, unboxedSumTName, arrowTName, mulArrowTName, listTName,
    appTName, appKindTName, sigTName, equalityTName, litTName, promotedTName,
    promotedTupleTName, promotedNilTName, promotedConsTName,
    wildCardTName, implicitParamTName :: Name
forallTName         = libFun (fsLit "forallT")        forallTIdKey
forallVisTName      = libFun (fsLit "forallVisT")     forallVisTIdKey
varTName            = libFun (fsLit "varT")           varTIdKey
conTName            = libFun (fsLit "conT")           conTIdKey
tupleTName          = libFun (fsLit "tupleT")         tupleTIdKey
unboxedTupleTName   = libFun (fsLit "unboxedTupleT")  unboxedTupleTIdKey
unboxedSumTName     = libFun (fsLit "unboxedSumT")    unboxedSumTIdKey
arrowTName          = libFun (fsLit "arrowT")         arrowTIdKey
mulArrowTName       = libFun (fsLit "mulArrowT")      mulArrowTIdKey
listTName           = libFun (fsLit "listT")          listTIdKey
appTName            = libFun (fsLit "appT")           appTIdKey
appKindTName        = libFun (fsLit "appKindT")       appKindTIdKey
sigTName            = libFun (fsLit "sigT")           sigTIdKey
equalityTName       = libFun (fsLit "equalityT")      equalityTIdKey
litTName            = libFun (fsLit "litT")           litTIdKey
promotedTName       = libFun (fsLit "promotedT")      promotedTIdKey
promotedTupleTName  = libFun (fsLit "promotedTupleT") promotedTupleTIdKey
promotedNilTName    = libFun (fsLit "promotedNilT")   promotedNilTIdKey
promotedConsTName   = libFun (fsLit "promotedConsT")  promotedConsTIdKey
wildCardTName       = libFun (fsLit "wildCardT")      wildCardTIdKey
infixTName          = libFun (fsLit "infixT")         infixTIdKey
implicitParamTName  = libFun (fsLit "implicitParamT") implicitParamTIdKey

-- data TyLit = ...
numTyLitName, strTyLitName, charTyLitName :: Name
numTyLitName = libFun (fsLit "numTyLit") numTyLitIdKey
strTyLitName = libFun (fsLit "strTyLit") strTyLitIdKey
charTyLitName = libFun (fsLit "charTyLit") charTyLitIdKey

-- data TyVarBndr = ...
plainTVName, kindedTVName :: Name
plainTVName  = libFun (fsLit "plainTV")  plainTVIdKey
kindedTVName = libFun (fsLit "kindedTV") kindedTVIdKey

plainInvisTVName, kindedInvisTVName :: Name
plainInvisTVName  = libFun (fsLit "plainInvisTV")  plainInvisTVIdKey
kindedInvisTVName = libFun (fsLit "kindedInvisTV") kindedInvisTVIdKey

-- data Specificity = ...
specifiedSpecName, inferredSpecName :: Name
specifiedSpecName = libFun (fsLit "specifiedSpec") specifiedSpecKey
inferredSpecName  = libFun (fsLit "inferredSpec")  inferredSpecKey

-- data Role = ...
nominalRName, representationalRName, phantomRName, inferRName :: Name
nominalRName          = libFun (fsLit "nominalR")          nominalRIdKey
representationalRName = libFun (fsLit "representationalR") representationalRIdKey
phantomRName          = libFun (fsLit "phantomR")          phantomRIdKey
inferRName            = libFun (fsLit "inferR")            inferRIdKey

-- data Kind = ...
starKName, constraintKName :: Name
starKName       = libFun (fsLit "starK")        starKIdKey
constraintKName = libFun (fsLit "constraintK")  constraintKIdKey

-- data FamilyResultSig = ...
noSigName, kindSigName, tyVarSigName :: Name
noSigName    = libFun (fsLit "noSig")    noSigIdKey
kindSigName  = libFun (fsLit "kindSig")  kindSigIdKey
tyVarSigName = libFun (fsLit "tyVarSig") tyVarSigIdKey

-- data InjectivityAnn = ...
injectivityAnnName :: Name
injectivityAnnName = libFun (fsLit "injectivityAnn") injectivityAnnIdKey

-- data Callconv = ...
cCallName, stdCallName, cApiCallName, primCallName, javaScriptCallName :: Name
cCallName = libFun (fsLit "cCall") cCallIdKey
stdCallName = libFun (fsLit "stdCall") stdCallIdKey
cApiCallName = libFun (fsLit "cApi") cApiCallIdKey
primCallName = libFun (fsLit "prim") primCallIdKey
javaScriptCallName = libFun (fsLit "javaScript") javaScriptCallIdKey

-- data Safety = ...
unsafeName, safeName, interruptibleName :: Name
unsafeName     = libFun (fsLit "unsafe") unsafeIdKey
safeName       = libFun (fsLit "safe") safeIdKey
interruptibleName = libFun (fsLit "interruptible") interruptibleIdKey

-- data RuleBndr = ...
ruleVarName, typedRuleVarName :: Name
ruleVarName      = libFun (fsLit ("ruleVar"))      ruleVarIdKey
typedRuleVarName = libFun (fsLit ("typedRuleVar")) typedRuleVarIdKey

-- data FunDep = ...
funDepName :: Name
funDepName     = libFun (fsLit "funDep") funDepIdKey

-- data TySynEqn = ...
tySynEqnName :: Name
tySynEqnName = libFun (fsLit "tySynEqn") tySynEqnIdKey

-- data AnnTarget = ...
valueAnnotationName, typeAnnotationName, moduleAnnotationName :: Name
valueAnnotationName  = libFun (fsLit "valueAnnotation")  valueAnnotationIdKey
typeAnnotationName   = libFun (fsLit "typeAnnotation")   typeAnnotationIdKey
moduleAnnotationName = libFun (fsLit "moduleAnnotation") moduleAnnotationIdKey

-- type DerivClause = ...
derivClauseName :: Name
derivClauseName = libFun (fsLit "derivClause") derivClauseIdKey

-- data DerivStrategy = ...
stockStrategyName, anyclassStrategyName, newtypeStrategyName,
  viaStrategyName :: Name
stockStrategyName    = libFun (fsLit "stockStrategy")    stockStrategyIdKey
anyclassStrategyName = libFun (fsLit "anyclassStrategy") anyclassStrategyIdKey
newtypeStrategyName  = libFun (fsLit "newtypeStrategy")  newtypeStrategyIdKey
viaStrategyName      = libFun (fsLit "viaStrategy")      viaStrategyIdKey

patQTyConName, expQTyConName, stmtTyConName,
    conTyConName, bangTypeTyConName,
    varBangTypeTyConName, typeQTyConName,
    decsQTyConName, ruleBndrTyConName, tySynEqnTyConName, roleTyConName,
    derivClauseTyConName, kindTyConName,
    tyVarBndrUnitTyConName, tyVarBndrSpecTyConName,
    derivStrategyTyConName :: Name
-- These are only used for the types of top-level splices
expQTyConName           = libTc (fsLit "ExpQ")           expQTyConKey
decsQTyConName          = libTc (fsLit "DecsQ")          decsQTyConKey  -- Q [Dec]
typeQTyConName          = libTc (fsLit "TypeQ")          typeQTyConKey
patQTyConName           = libTc (fsLit "PatQ")           patQTyConKey

-- These are used in GHC.HsToCore.Quote but always wrapped in a type variable
stmtTyConName           = thTc (fsLit "Stmt")            stmtTyConKey
conTyConName            = thTc (fsLit "Con")             conTyConKey
bangTypeTyConName       = thTc (fsLit "BangType")      bangTypeTyConKey
varBangTypeTyConName    = thTc (fsLit "VarBangType")     varBangTypeTyConKey
ruleBndrTyConName      = thTc (fsLit "RuleBndr")      ruleBndrTyConKey
tySynEqnTyConName       = thTc  (fsLit "TySynEqn")       tySynEqnTyConKey
roleTyConName           = libTc (fsLit "Role")           roleTyConKey
derivClauseTyConName   = thTc (fsLit "DerivClause")   derivClauseTyConKey
kindTyConName          = thTc (fsLit "Kind")          kindTyConKey
tyVarBndrUnitTyConName = libTc (fsLit "TyVarBndrUnit") tyVarBndrUnitTyConKey
tyVarBndrSpecTyConName = libTc (fsLit "TyVarBndrSpec") tyVarBndrSpecTyConKey
derivStrategyTyConName = thTc (fsLit "DerivStrategy") derivStrategyTyConKey

-- quasiquoting
quoteExpName, quotePatName, quoteDecName, quoteTypeName :: Name
quoteExpName        = qqFun (fsLit "quoteExp")  quoteExpKey
quotePatName        = qqFun (fsLit "quotePat")  quotePatKey
quoteDecName        = qqFun (fsLit "quoteDec")  quoteDecKey
quoteTypeName       = qqFun (fsLit "quoteType") quoteTypeKey

-- data Inline = ...
noInlineDataConName, inlineDataConName, inlinableDataConName :: Name
noInlineDataConName  = thCon (fsLit "NoInline")  noInlineDataConKey
inlineDataConName    = thCon (fsLit "Inline")    inlineDataConKey
inlinableDataConName = thCon (fsLit "Inlinable") inlinableDataConKey

-- data RuleMatch = ...
conLikeDataConName, funLikeDataConName :: Name
conLikeDataConName = thCon (fsLit "ConLike") conLikeDataConKey
funLikeDataConName = thCon (fsLit "FunLike") funLikeDataConKey

-- data Phases = ...
allPhasesDataConName, fromPhaseDataConName, beforePhaseDataConName :: Name
allPhasesDataConName   = thCon (fsLit "AllPhases")   allPhasesDataConKey
fromPhaseDataConName   = thCon (fsLit "FromPhase")   fromPhaseDataConKey
beforePhaseDataConName = thCon (fsLit "BeforePhase") beforePhaseDataConKey

-- data Overlap = ...
overlappableDataConName,
  overlappingDataConName,
  overlapsDataConName,
  incoherentDataConName :: Name
overlappableDataConName = thCon (fsLit "Overlappable") overlappableDataConKey
overlappingDataConName  = thCon (fsLit "Overlapping")  overlappingDataConKey
overlapsDataConName     = thCon (fsLit "Overlaps")     overlapsDataConKey
incoherentDataConName   = thCon (fsLit "Incoherent")   incoherentDataConKey

{- *********************************************************************
*                                                                      *
                     Class keys
*                                                                      *
********************************************************************* -}

-- ClassUniques available: 200-299
-- Check in GHC.Builtin.Names if you want to change this

liftClassKey :: Unique
liftClassKey = mkPreludeClassUnique 200

quoteClassKey :: Unique
quoteClassKey = mkPreludeClassUnique 201

{- *********************************************************************
*                                                                      *
                     TyCon keys
*                                                                      *
********************************************************************* -}

-- TyConUniques available: 200-299
-- Check in GHC.Builtin.Names if you want to change this

expTyConKey, matchTyConKey, clauseTyConKey, qTyConKey, expQTyConKey,
    patTyConKey,
    stmtTyConKey, conTyConKey, typeQTyConKey, typeTyConKey,
    tyVarBndrUnitTyConKey, tyVarBndrSpecTyConKey,
    decTyConKey, bangTypeTyConKey, varBangTypeTyConKey,
    fieldExpTyConKey, fieldPatTyConKey, nameTyConKey, patQTyConKey,
    funDepTyConKey, predTyConKey,
    predQTyConKey, decsQTyConKey, ruleBndrTyConKey, tySynEqnTyConKey,
    roleTyConKey, codeTyConKey, injAnnTyConKey, kindTyConKey,
    overlapTyConKey, derivClauseTyConKey, derivStrategyTyConKey, decsTyConKey,
    modNameTyConKey  :: Unique
expTyConKey             = mkPreludeTyConUnique 200
matchTyConKey           = mkPreludeTyConUnique 201
clauseTyConKey          = mkPreludeTyConUnique 202
qTyConKey               = mkPreludeTyConUnique 203
expQTyConKey            = mkPreludeTyConUnique 204
patTyConKey             = mkPreludeTyConUnique 206
stmtTyConKey            = mkPreludeTyConUnique 209
conTyConKey             = mkPreludeTyConUnique 210
typeQTyConKey           = mkPreludeTyConUnique 211
typeTyConKey            = mkPreludeTyConUnique 212
decTyConKey             = mkPreludeTyConUnique 213
bangTypeTyConKey        = mkPreludeTyConUnique 214
varBangTypeTyConKey     = mkPreludeTyConUnique 215
fieldExpTyConKey        = mkPreludeTyConUnique 216
fieldPatTyConKey        = mkPreludeTyConUnique 217
nameTyConKey            = mkPreludeTyConUnique 218
patQTyConKey            = mkPreludeTyConUnique 219
funDepTyConKey          = mkPreludeTyConUnique 222
predTyConKey            = mkPreludeTyConUnique 223
predQTyConKey           = mkPreludeTyConUnique 224
tyVarBndrUnitTyConKey   = mkPreludeTyConUnique 225
decsQTyConKey           = mkPreludeTyConUnique 226
ruleBndrTyConKey        = mkPreludeTyConUnique 227
tySynEqnTyConKey        = mkPreludeTyConUnique 228
roleTyConKey            = mkPreludeTyConUnique 229
injAnnTyConKey          = mkPreludeTyConUnique 231
kindTyConKey            = mkPreludeTyConUnique 232
overlapTyConKey         = mkPreludeTyConUnique 233
derivClauseTyConKey     = mkPreludeTyConUnique 234
derivStrategyTyConKey   = mkPreludeTyConUnique 235
decsTyConKey            = mkPreludeTyConUnique 236
tyVarBndrSpecTyConKey   = mkPreludeTyConUnique 237
codeTyConKey            = mkPreludeTyConUnique 238
modNameTyConKey         = mkPreludeTyConUnique 239

{- *********************************************************************
*                                                                      *
                     DataCon keys
*                                                                      *
********************************************************************* -}

-- DataConUniques available: 100-150
-- If you want to change this, make sure you check in GHC.Builtin.Names

-- data Inline = ...
noInlineDataConKey, inlineDataConKey, inlinableDataConKey :: Unique
noInlineDataConKey  = mkPreludeDataConUnique 200
inlineDataConKey    = mkPreludeDataConUnique 201
inlinableDataConKey = mkPreludeDataConUnique 202

-- data RuleMatch = ...
conLikeDataConKey, funLikeDataConKey :: Unique
conLikeDataConKey = mkPreludeDataConUnique 203
funLikeDataConKey = mkPreludeDataConUnique 204

-- data Phases = ...
allPhasesDataConKey, fromPhaseDataConKey, beforePhaseDataConKey :: Unique
allPhasesDataConKey   = mkPreludeDataConUnique 205
fromPhaseDataConKey   = mkPreludeDataConUnique 206
beforePhaseDataConKey = mkPreludeDataConUnique 207

-- data Overlap = ..
overlappableDataConKey,
  overlappingDataConKey,
  overlapsDataConKey,
  incoherentDataConKey :: Unique
overlappableDataConKey = mkPreludeDataConUnique 209
overlappingDataConKey  = mkPreludeDataConUnique 210
overlapsDataConKey     = mkPreludeDataConUnique 211
incoherentDataConKey   = mkPreludeDataConUnique 212

{- *********************************************************************
*                                                                      *
                     Id keys
*                                                                      *
********************************************************************* -}

-- IdUniques available: 200-499
-- If you want to change this, make sure you check in GHC.Builtin.Names

returnQIdKey, bindQIdKey, sequenceQIdKey, liftIdKey, newNameIdKey,
    mkNameIdKey, mkNameG_vIdKey, mkNameG_dIdKey, mkNameG_tcIdKey,
    mkNameLIdKey, mkNameSIdKey, unTypeIdKey, unTypeCodeIdKey,
    unsafeCodeCoerceIdKey, liftTypedIdKey, mkModNameIdKey :: Unique
returnQIdKey        = mkPreludeMiscIdUnique 200
bindQIdKey          = mkPreludeMiscIdUnique 201
sequenceQIdKey      = mkPreludeMiscIdUnique 202
liftIdKey           = mkPreludeMiscIdUnique 203
newNameIdKey         = mkPreludeMiscIdUnique 204
mkNameIdKey          = mkPreludeMiscIdUnique 205
mkNameG_vIdKey       = mkPreludeMiscIdUnique 206
mkNameG_dIdKey       = mkPreludeMiscIdUnique 207
mkNameG_tcIdKey      = mkPreludeMiscIdUnique 208
mkNameLIdKey         = mkPreludeMiscIdUnique 209
mkNameSIdKey         = mkPreludeMiscIdUnique 210
unTypeIdKey          = mkPreludeMiscIdUnique 211
unTypeCodeIdKey      = mkPreludeMiscIdUnique 212
liftTypedIdKey        = mkPreludeMiscIdUnique 214
mkModNameIdKey        = mkPreludeMiscIdUnique 215
unsafeCodeCoerceIdKey = mkPreludeMiscIdUnique 216


-- data Lit = ...
charLIdKey, stringLIdKey, integerLIdKey, intPrimLIdKey, wordPrimLIdKey,
    floatPrimLIdKey, doublePrimLIdKey, rationalLIdKey, stringPrimLIdKey,
    charPrimLIdKey:: Unique
charLIdKey        = mkPreludeMiscIdUnique 220
stringLIdKey      = mkPreludeMiscIdUnique 221
integerLIdKey     = mkPreludeMiscIdUnique 222
intPrimLIdKey     = mkPreludeMiscIdUnique 223
wordPrimLIdKey    = mkPreludeMiscIdUnique 224
floatPrimLIdKey   = mkPreludeMiscIdUnique 225
doublePrimLIdKey  = mkPreludeMiscIdUnique 226
rationalLIdKey    = mkPreludeMiscIdUnique 227
stringPrimLIdKey  = mkPreludeMiscIdUnique 228
charPrimLIdKey    = mkPreludeMiscIdUnique 229

liftStringIdKey :: Unique
liftStringIdKey     = mkPreludeMiscIdUnique 230

-- data Pat = ...
litPIdKey, varPIdKey, tupPIdKey, unboxedTupPIdKey, unboxedSumPIdKey, conPIdKey,
  infixPIdKey, tildePIdKey, bangPIdKey, asPIdKey, wildPIdKey, recPIdKey,
  listPIdKey, sigPIdKey, viewPIdKey :: Unique
litPIdKey         = mkPreludeMiscIdUnique 240
varPIdKey         = mkPreludeMiscIdUnique 241
tupPIdKey         = mkPreludeMiscIdUnique 242
unboxedTupPIdKey  = mkPreludeMiscIdUnique 243
unboxedSumPIdKey  = mkPreludeMiscIdUnique 244
conPIdKey         = mkPreludeMiscIdUnique 245
infixPIdKey       = mkPreludeMiscIdUnique 246
tildePIdKey       = mkPreludeMiscIdUnique 247
bangPIdKey        = mkPreludeMiscIdUnique 248
asPIdKey          = mkPreludeMiscIdUnique 249
wildPIdKey        = mkPreludeMiscIdUnique 250
recPIdKey         = mkPreludeMiscIdUnique 251
listPIdKey        = mkPreludeMiscIdUnique 252
sigPIdKey         = mkPreludeMiscIdUnique 253
viewPIdKey        = mkPreludeMiscIdUnique 254

-- type FieldPat = ...
fieldPatIdKey :: Unique
fieldPatIdKey       = mkPreludeMiscIdUnique 260

-- data Match = ...
matchIdKey :: Unique
matchIdKey          = mkPreludeMiscIdUnique 261

-- data Clause = ...
clauseIdKey :: Unique
clauseIdKey         = mkPreludeMiscIdUnique 262


-- data Exp = ...
varEIdKey, conEIdKey, litEIdKey, appEIdKey, appTypeEIdKey, infixEIdKey,
    infixAppIdKey, sectionLIdKey, sectionRIdKey, lamEIdKey, lamCaseEIdKey,
    tupEIdKey, unboxedTupEIdKey, unboxedSumEIdKey, condEIdKey, multiIfEIdKey,
    letEIdKey, caseEIdKey, doEIdKey, compEIdKey,
    fromEIdKey, fromThenEIdKey, fromToEIdKey, fromThenToEIdKey,
    listEIdKey, sigEIdKey, recConEIdKey, recUpdEIdKey, staticEIdKey,
    unboundVarEIdKey, labelEIdKey, implicitParamVarEIdKey, mdoEIdKey :: Unique
varEIdKey              = mkPreludeMiscIdUnique 270
conEIdKey              = mkPreludeMiscIdUnique 271
litEIdKey              = mkPreludeMiscIdUnique 272
appEIdKey              = mkPreludeMiscIdUnique 273
appTypeEIdKey          = mkPreludeMiscIdUnique 274
infixEIdKey            = mkPreludeMiscIdUnique 275
infixAppIdKey          = mkPreludeMiscIdUnique 276
sectionLIdKey          = mkPreludeMiscIdUnique 277
sectionRIdKey          = mkPreludeMiscIdUnique 278
lamEIdKey              = mkPreludeMiscIdUnique 279
lamCaseEIdKey          = mkPreludeMiscIdUnique 280
tupEIdKey              = mkPreludeMiscIdUnique 281
unboxedTupEIdKey       = mkPreludeMiscIdUnique 282
unboxedSumEIdKey       = mkPreludeMiscIdUnique 283
condEIdKey             = mkPreludeMiscIdUnique 284
multiIfEIdKey          = mkPreludeMiscIdUnique 285
letEIdKey              = mkPreludeMiscIdUnique 286
caseEIdKey             = mkPreludeMiscIdUnique 287
doEIdKey               = mkPreludeMiscIdUnique 288
compEIdKey             = mkPreludeMiscIdUnique 289
fromEIdKey             = mkPreludeMiscIdUnique 290
fromThenEIdKey         = mkPreludeMiscIdUnique 291
fromToEIdKey           = mkPreludeMiscIdUnique 292
fromThenToEIdKey       = mkPreludeMiscIdUnique 293
listEIdKey             = mkPreludeMiscIdUnique 294
sigEIdKey              = mkPreludeMiscIdUnique 295
recConEIdKey           = mkPreludeMiscIdUnique 296
recUpdEIdKey           = mkPreludeMiscIdUnique 297
staticEIdKey           = mkPreludeMiscIdUnique 298
unboundVarEIdKey       = mkPreludeMiscIdUnique 299
labelEIdKey            = mkPreludeMiscIdUnique 300
implicitParamVarEIdKey = mkPreludeMiscIdUnique 301
mdoEIdKey              = mkPreludeMiscIdUnique 302

-- type FieldExp = ...
fieldExpIdKey :: Unique
fieldExpIdKey       = mkPreludeMiscIdUnique 305

-- data Body = ...
guardedBIdKey, normalBIdKey :: Unique
guardedBIdKey     = mkPreludeMiscIdUnique 306
normalBIdKey      = mkPreludeMiscIdUnique 307

-- data Guard = ...
normalGEIdKey, patGEIdKey :: Unique
normalGEIdKey     = mkPreludeMiscIdUnique 308
patGEIdKey        = mkPreludeMiscIdUnique 309

-- data Stmt = ...
bindSIdKey, letSIdKey, noBindSIdKey, parSIdKey, recSIdKey :: Unique
bindSIdKey       = mkPreludeMiscIdUnique 310
letSIdKey        = mkPreludeMiscIdUnique 311
noBindSIdKey     = mkPreludeMiscIdUnique 312
parSIdKey        = mkPreludeMiscIdUnique 313
recSIdKey        = mkPreludeMiscIdUnique 314

-- data Dec = ...
funDIdKey, valDIdKey, dataDIdKey, newtypeDIdKey, tySynDIdKey, classDIdKey,
    instanceWithOverlapDIdKey, instanceDIdKey, sigDIdKey, forImpDIdKey,
    pragInlDIdKey, pragSpecDIdKey, pragSpecInlDIdKey, pragSpecInstDIdKey,
    pragRuleDIdKey, pragAnnDIdKey, defaultSigDIdKey, dataFamilyDIdKey,
    openTypeFamilyDIdKey, closedTypeFamilyDIdKey, dataInstDIdKey,
    newtypeInstDIdKey, tySynInstDIdKey, standaloneDerivWithStrategyDIdKey,
    infixLDIdKey, infixRDIdKey, infixNDIdKey, roleAnnotDIdKey, patSynDIdKey,
    patSynSigDIdKey, pragCompleteDIdKey, implicitParamBindDIdKey,
    kiSigDIdKey :: Unique
funDIdKey                         = mkPreludeMiscIdUnique 320
valDIdKey                         = mkPreludeMiscIdUnique 321
dataDIdKey                        = mkPreludeMiscIdUnique 322
newtypeDIdKey                     = mkPreludeMiscIdUnique 323
tySynDIdKey                       = mkPreludeMiscIdUnique 324
classDIdKey                       = mkPreludeMiscIdUnique 325
instanceWithOverlapDIdKey         = mkPreludeMiscIdUnique 326
instanceDIdKey                    = mkPreludeMiscIdUnique 327
sigDIdKey                         = mkPreludeMiscIdUnique 328
forImpDIdKey                      = mkPreludeMiscIdUnique 329
pragInlDIdKey                     = mkPreludeMiscIdUnique 330
pragSpecDIdKey                    = mkPreludeMiscIdUnique 331
pragSpecInlDIdKey                 = mkPreludeMiscIdUnique 332
pragSpecInstDIdKey                = mkPreludeMiscIdUnique 333
pragRuleDIdKey                    = mkPreludeMiscIdUnique 334
pragAnnDIdKey                     = mkPreludeMiscIdUnique 335
dataFamilyDIdKey                  = mkPreludeMiscIdUnique 336
openTypeFamilyDIdKey              = mkPreludeMiscIdUnique 337
dataInstDIdKey                    = mkPreludeMiscIdUnique 338
newtypeInstDIdKey                 = mkPreludeMiscIdUnique 339
tySynInstDIdKey                   = mkPreludeMiscIdUnique 340
closedTypeFamilyDIdKey            = mkPreludeMiscIdUnique 341
infixLDIdKey                      = mkPreludeMiscIdUnique 342
infixRDIdKey                      = mkPreludeMiscIdUnique 343
infixNDIdKey                      = mkPreludeMiscIdUnique 344
roleAnnotDIdKey                   = mkPreludeMiscIdUnique 345
standaloneDerivWithStrategyDIdKey = mkPreludeMiscIdUnique 346
defaultSigDIdKey                  = mkPreludeMiscIdUnique 347
patSynDIdKey                      = mkPreludeMiscIdUnique 348
patSynSigDIdKey                   = mkPreludeMiscIdUnique 349
pragCompleteDIdKey                = mkPreludeMiscIdUnique 350
implicitParamBindDIdKey           = mkPreludeMiscIdUnique 351
kiSigDIdKey                       = mkPreludeMiscIdUnique 352

-- type Cxt = ...
cxtIdKey :: Unique
cxtIdKey               = mkPreludeMiscIdUnique 361

-- data SourceUnpackedness = ...
noSourceUnpackednessKey, sourceNoUnpackKey, sourceUnpackKey :: Unique
noSourceUnpackednessKey = mkPreludeMiscIdUnique 362
sourceNoUnpackKey       = mkPreludeMiscIdUnique 363
sourceUnpackKey         = mkPreludeMiscIdUnique 364

-- data SourceStrictness = ...
noSourceStrictnessKey, sourceLazyKey, sourceStrictKey :: Unique
noSourceStrictnessKey   = mkPreludeMiscIdUnique 365
sourceLazyKey           = mkPreludeMiscIdUnique 366
sourceStrictKey         = mkPreludeMiscIdUnique 367

-- data Con = ...
normalCIdKey, recCIdKey, infixCIdKey, forallCIdKey, gadtCIdKey,
  recGadtCIdKey :: Unique
normalCIdKey      = mkPreludeMiscIdUnique 368
recCIdKey         = mkPreludeMiscIdUnique 369
infixCIdKey       = mkPreludeMiscIdUnique 370
forallCIdKey      = mkPreludeMiscIdUnique 371
gadtCIdKey        = mkPreludeMiscIdUnique 372
recGadtCIdKey     = mkPreludeMiscIdUnique 373

-- data Bang = ...
bangIdKey :: Unique
bangIdKey         = mkPreludeMiscIdUnique 374

-- type BangType = ...
bangTKey :: Unique
bangTKey          = mkPreludeMiscIdUnique 375

-- type VarBangType = ...
varBangTKey :: Unique
varBangTKey       = mkPreludeMiscIdUnique 376

-- data PatSynDir = ...
unidirPatSynIdKey, implBidirPatSynIdKey, explBidirPatSynIdKey :: Unique
unidirPatSynIdKey    = mkPreludeMiscIdUnique 377
implBidirPatSynIdKey = mkPreludeMiscIdUnique 378
explBidirPatSynIdKey = mkPreludeMiscIdUnique 379

-- data PatSynArgs = ...
prefixPatSynIdKey, infixPatSynIdKey, recordPatSynIdKey :: Unique
prefixPatSynIdKey = mkPreludeMiscIdUnique 380
infixPatSynIdKey  = mkPreludeMiscIdUnique 381
recordPatSynIdKey = mkPreludeMiscIdUnique 382

-- data Type = ...
forallTIdKey, forallVisTIdKey, varTIdKey, conTIdKey, tupleTIdKey,
    unboxedTupleTIdKey, unboxedSumTIdKey, arrowTIdKey, listTIdKey, appTIdKey,
    appKindTIdKey, sigTIdKey, equalityTIdKey, litTIdKey, promotedTIdKey,
    promotedTupleTIdKey, promotedNilTIdKey, promotedConsTIdKey,
    wildCardTIdKey, implicitParamTIdKey, infixTIdKey :: Unique
forallTIdKey        = mkPreludeMiscIdUnique 390
forallVisTIdKey     = mkPreludeMiscIdUnique 391
varTIdKey           = mkPreludeMiscIdUnique 392
conTIdKey           = mkPreludeMiscIdUnique 393
tupleTIdKey         = mkPreludeMiscIdUnique 394
unboxedTupleTIdKey  = mkPreludeMiscIdUnique 395
unboxedSumTIdKey    = mkPreludeMiscIdUnique 396
arrowTIdKey         = mkPreludeMiscIdUnique 397
listTIdKey          = mkPreludeMiscIdUnique 398
appTIdKey           = mkPreludeMiscIdUnique 399
appKindTIdKey       = mkPreludeMiscIdUnique 400
sigTIdKey           = mkPreludeMiscIdUnique 401
equalityTIdKey      = mkPreludeMiscIdUnique 402
litTIdKey           = mkPreludeMiscIdUnique 403
promotedTIdKey      = mkPreludeMiscIdUnique 404
promotedTupleTIdKey = mkPreludeMiscIdUnique 405
promotedNilTIdKey   = mkPreludeMiscIdUnique 406
promotedConsTIdKey  = mkPreludeMiscIdUnique 407
wildCardTIdKey      = mkPreludeMiscIdUnique 408
implicitParamTIdKey = mkPreludeMiscIdUnique 409
infixTIdKey         = mkPreludeMiscIdUnique 410

-- data TyLit = ...
numTyLitIdKey, strTyLitIdKey, charTyLitIdKey :: Unique
numTyLitIdKey  = mkPreludeMiscIdUnique 411
strTyLitIdKey  = mkPreludeMiscIdUnique 412
charTyLitIdKey = mkPreludeMiscIdUnique 413

-- data TyVarBndr = ...
plainTVIdKey, kindedTVIdKey :: Unique
plainTVIdKey       = mkPreludeMiscIdUnique 414
kindedTVIdKey      = mkPreludeMiscIdUnique 415

plainInvisTVIdKey, kindedInvisTVIdKey :: Unique
plainInvisTVIdKey       = mkPreludeMiscIdUnique 482
kindedInvisTVIdKey      = mkPreludeMiscIdUnique 483

-- data Role = ...
nominalRIdKey, representationalRIdKey, phantomRIdKey, inferRIdKey :: Unique
nominalRIdKey          = mkPreludeMiscIdUnique 416
representationalRIdKey = mkPreludeMiscIdUnique 417
phantomRIdKey          = mkPreludeMiscIdUnique 418
inferRIdKey            = mkPreludeMiscIdUnique 419

-- data Kind = ...
starKIdKey, constraintKIdKey :: Unique
starKIdKey        = mkPreludeMiscIdUnique 425
constraintKIdKey  = mkPreludeMiscIdUnique 426

-- data FamilyResultSig = ...
noSigIdKey, kindSigIdKey, tyVarSigIdKey :: Unique
noSigIdKey        = mkPreludeMiscIdUnique 427
kindSigIdKey      = mkPreludeMiscIdUnique 428
tyVarSigIdKey     = mkPreludeMiscIdUnique 429

-- data InjectivityAnn = ...
injectivityAnnIdKey :: Unique
injectivityAnnIdKey = mkPreludeMiscIdUnique 430

-- data Callconv = ...
cCallIdKey, stdCallIdKey, cApiCallIdKey, primCallIdKey,
  javaScriptCallIdKey :: Unique
cCallIdKey          = mkPreludeMiscIdUnique 431
stdCallIdKey        = mkPreludeMiscIdUnique 432
cApiCallIdKey       = mkPreludeMiscIdUnique 433
primCallIdKey       = mkPreludeMiscIdUnique 434
javaScriptCallIdKey = mkPreludeMiscIdUnique 435

-- data Safety = ...
unsafeIdKey, safeIdKey, interruptibleIdKey :: Unique
unsafeIdKey        = mkPreludeMiscIdUnique 440
safeIdKey          = mkPreludeMiscIdUnique 441
interruptibleIdKey = mkPreludeMiscIdUnique 442

-- data FunDep = ...
funDepIdKey :: Unique
funDepIdKey = mkPreludeMiscIdUnique 445

-- mulArrow
mulArrowTIdKey :: Unique
mulArrowTIdKey = mkPreludeMiscIdUnique 446

-- data TySynEqn = ...
tySynEqnIdKey :: Unique
tySynEqnIdKey = mkPreludeMiscIdUnique 460

-- quasiquoting
quoteExpKey, quotePatKey, quoteDecKey, quoteTypeKey :: Unique
quoteExpKey  = mkPreludeMiscIdUnique 470
quotePatKey  = mkPreludeMiscIdUnique 471
quoteDecKey  = mkPreludeMiscIdUnique 472
quoteTypeKey = mkPreludeMiscIdUnique 473

-- data RuleBndr = ...
ruleVarIdKey, typedRuleVarIdKey :: Unique
ruleVarIdKey      = mkPreludeMiscIdUnique 480
typedRuleVarIdKey = mkPreludeMiscIdUnique 481

-- data AnnTarget = ...
valueAnnotationIdKey, typeAnnotationIdKey, moduleAnnotationIdKey :: Unique
valueAnnotationIdKey  = mkPreludeMiscIdUnique 490
typeAnnotationIdKey   = mkPreludeMiscIdUnique 491
moduleAnnotationIdKey = mkPreludeMiscIdUnique 492

-- type DerivPred = ...
derivClauseIdKey :: Unique
derivClauseIdKey = mkPreludeMiscIdUnique 493

-- data DerivStrategy = ...
stockStrategyIdKey, anyclassStrategyIdKey, newtypeStrategyIdKey,
  viaStrategyIdKey :: Unique
stockStrategyIdKey    = mkPreludeDataConUnique 494
anyclassStrategyIdKey = mkPreludeDataConUnique 495
newtypeStrategyIdKey  = mkPreludeDataConUnique 496
viaStrategyIdKey      = mkPreludeDataConUnique 497

-- data Specificity = ...
specifiedSpecKey, inferredSpecKey :: Unique
specifiedSpecKey = mkPreludeMiscIdUnique 498
inferredSpecKey  = mkPreludeMiscIdUnique 499

{-
************************************************************************
*                                                                      *
                        RdrNames
*                                                                      *
************************************************************************
-}

lift_RDR, liftTyped_RDR, mkNameG_dRDR, mkNameG_vRDR, unsafeCodeCoerce_RDR :: RdrName
lift_RDR     = nameRdrName liftName
liftTyped_RDR = nameRdrName liftTypedName
unsafeCodeCoerce_RDR = nameRdrName unsafeCodeCoerceName
mkNameG_dRDR = nameRdrName mkNameG_dName
mkNameG_vRDR = nameRdrName mkNameG_vName

-- data Exp = ...
conE_RDR, litE_RDR, appE_RDR, infixApp_RDR :: RdrName
conE_RDR     = nameRdrName conEName
litE_RDR     = nameRdrName litEName
appE_RDR     = nameRdrName appEName
infixApp_RDR = nameRdrName infixAppName

-- data Lit = ...
stringL_RDR, intPrimL_RDR, wordPrimL_RDR, floatPrimL_RDR,
    doublePrimL_RDR, stringPrimL_RDR, charPrimL_RDR :: RdrName
stringL_RDR     = nameRdrName stringLName
intPrimL_RDR    = nameRdrName intPrimLName
wordPrimL_RDR   = nameRdrName wordPrimLName
floatPrimL_RDR  = nameRdrName floatPrimLName
doublePrimL_RDR = nameRdrName doublePrimLName
stringPrimL_RDR = nameRdrName stringPrimLName
charPrimL_RDR   = nameRdrName charPrimLName
