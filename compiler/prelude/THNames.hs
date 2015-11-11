-- %************************************************************************
-- %*                                                                   *
--              The known-key names for Template Haskell
-- %*                                                                   *
-- %************************************************************************

module THNames where

import PrelNames( mk_known_key_name )
import Module( Module, mkModuleNameFS, mkModule, thUnitId )
import Name( Name )
import OccName( tcName, clsName, dataName, varName )
import RdrName( RdrName, nameRdrName )
import Unique
import FastString

-- To add a name, do three things
--
--  1) Allocate a key
--  2) Make a "Name"
--  3) Add the name to templateHaskellNames

templateHaskellNames :: [Name]
-- The names that are implicitly mentioned by ``bracket''
-- Should stay in sync with the import list of DsMeta

templateHaskellNames = [
    returnQName, bindQName, sequenceQName, newNameName, liftName,
    mkNameName, mkNameG_vName, mkNameG_dName, mkNameG_tcName, mkNameLName,
    mkNameSName,
    liftStringName,
    unTypeName,
    unTypeQName,
    unsafeTExpCoerceName,

    -- Lit
    charLName, stringLName, integerLName, intPrimLName, wordPrimLName,
    floatPrimLName, doublePrimLName, rationalLName, stringPrimLName,
    charPrimLName,
    -- Pat
    litPName, varPName, tupPName, unboxedTupPName,
    conPName, tildePName, bangPName, infixPName,
    asPName, wildPName, recPName, listPName, sigPName, viewPName,
    -- FieldPat
    fieldPatName,
    -- Match
    matchName,
    -- Clause
    clauseName,
    -- Exp
    varEName, conEName, litEName, appEName, infixEName,
    infixAppName, sectionLName, sectionRName, lamEName, lamCaseEName,
    tupEName, unboxedTupEName,
    condEName, multiIfEName, letEName, caseEName, doEName, compEName,
    fromEName, fromThenEName, fromToEName, fromThenToEName,
    listEName, sigEName, recConEName, recUpdEName, staticEName, unboundVarEName,
    -- FieldExp
    fieldExpName,
    -- Body
    guardedBName, normalBName,
    -- Guard
    normalGEName, patGEName,
    -- Stmt
    bindSName, letSName, noBindSName, parSName,
    -- Dec
    funDName, valDName, dataDName, newtypeDName, tySynDName,
    classDName, instanceDName, standaloneDerivDName, sigDName, forImpDName,
    pragInlDName, pragSpecDName, pragSpecInlDName, pragSpecInstDName,
    pragRuleDName, pragAnnDName, defaultSigDName,
    dataFamilyDName, openTypeFamilyDName, closedTypeFamilyDName,
    dataInstDName, newtypeInstDName, tySynInstDName,
    infixLDName, infixRDName, infixNDName,
    roleAnnotDName,
    -- Cxt
    cxtName,
    -- Strict
    isStrictName, notStrictName, unpackedName,
    -- Con
    normalCName, recCName, infixCName, forallCName, gadtCName, recGadtCName,
    -- StrictType
    strictTypeName,
    -- VarStrictType
    varStrictTypeName,
    -- Type
    forallTName, varTName, conTName, appTName, equalityTName,
    tupleTName, unboxedTupleTName, arrowTName, listTName, sigTName, litTName,
    promotedTName, promotedTupleTName, promotedNilTName, promotedConsTName,
    wildCardTName, namedWildCardTName,
    -- TyLit
    numTyLitName, strTyLitName,
    -- TyVarBndr
    plainTVName, kindedTVName,
    -- Role
    nominalRName, representationalRName, phantomRName, inferRName,
    -- Kind
    varKName, conKName, tupleKName, arrowKName, listKName, appKName,
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
    -- TExp
    tExpDataConName,
    -- RuleBndr
    ruleVarName, typedRuleVarName,
    -- FunDep
    funDepName,
    -- FamFlavour
    typeFamName, dataFamName,
    -- TySynEqn
    tySynEqnName,
    -- AnnTarget
    valueAnnotationName, typeAnnotationName, moduleAnnotationName,

    -- The type classes
    liftClassName,

    -- And the tycons
    qTyConName, nameTyConName, patTyConName, fieldPatTyConName, matchQTyConName,
    clauseQTyConName, expQTyConName, fieldExpTyConName, predTyConName,
    stmtQTyConName, decQTyConName, conQTyConName, strictTypeQTyConName,
    varStrictTypeQTyConName, typeQTyConName, expTyConName, decTyConName,
    typeTyConName, tyVarBndrTyConName, matchTyConName, clauseTyConName,
    patQTyConName, fieldPatQTyConName, fieldExpQTyConName, funDepTyConName,
    predQTyConName, decsQTyConName, ruleBndrQTyConName, tySynEqnQTyConName,
    roleTyConName, tExpTyConName, injAnnTyConName, kindTyConName,

    -- Quasiquoting
    quoteDecName, quoteTypeName, quoteExpName, quotePatName]

thSyn, thLib, qqLib :: Module
thSyn = mkTHModule (fsLit "Language.Haskell.TH.Syntax")
thLib = mkTHModule (fsLit "Language.Haskell.TH.Lib")
qqLib = mkTHModule (fsLit "Language.Haskell.TH.Quote")

mkTHModule :: FastString -> Module
mkTHModule m = mkModule thUnitId (mkModuleNameFS m)

libFun, libTc, thFun, thTc, thCls, thCon, qqFun :: FastString -> Unique -> Name
libFun = mk_known_key_name OccName.varName  thLib
libTc  = mk_known_key_name OccName.tcName   thLib
thFun  = mk_known_key_name OccName.varName  thSyn
thTc   = mk_known_key_name OccName.tcName   thSyn
thCls  = mk_known_key_name OccName.clsName  thSyn
thCon  = mk_known_key_name OccName.dataName thSyn
qqFun  = mk_known_key_name OccName.varName  qqLib

-------------------- TH.Syntax -----------------------
liftClassName :: Name
liftClassName = thCls (fsLit "Lift") liftClassKey

qTyConName, nameTyConName, fieldExpTyConName, patTyConName,
    fieldPatTyConName, expTyConName, decTyConName, typeTyConName,
    tyVarBndrTyConName, matchTyConName, clauseTyConName, funDepTyConName,
    predTyConName, tExpTyConName, injAnnTyConName, kindTyConName :: Name
qTyConName        = thTc (fsLit "Q")              qTyConKey
nameTyConName     = thTc (fsLit "Name")           nameTyConKey
fieldExpTyConName = thTc (fsLit "FieldExp")       fieldExpTyConKey
patTyConName      = thTc (fsLit "Pat")            patTyConKey
fieldPatTyConName = thTc (fsLit "FieldPat")       fieldPatTyConKey
expTyConName      = thTc (fsLit "Exp")            expTyConKey
decTyConName      = thTc (fsLit "Dec")            decTyConKey
typeTyConName     = thTc (fsLit "Type")           typeTyConKey
tyVarBndrTyConName= thTc (fsLit "TyVarBndr")      tyVarBndrTyConKey
matchTyConName    = thTc (fsLit "Match")          matchTyConKey
clauseTyConName   = thTc (fsLit "Clause")         clauseTyConKey
funDepTyConName   = thTc (fsLit "FunDep")         funDepTyConKey
predTyConName     = thTc (fsLit "Pred")           predTyConKey
tExpTyConName     = thTc (fsLit "TExp")           tExpTyConKey
injAnnTyConName   = thTc (fsLit "InjectivityAnn") injAnnTyConKey
kindTyConName     = thTc (fsLit "Kind")           kindTyConKey


returnQName, bindQName, sequenceQName, newNameName, liftName,
    mkNameName, mkNameG_vName, mkNameG_dName, mkNameG_tcName,
    mkNameLName, mkNameSName, liftStringName, unTypeName, unTypeQName,
    unsafeTExpCoerceName :: Name
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
unTypeName     = thFun (fsLit "unType")     unTypeIdKey
unTypeQName    = thFun (fsLit "unTypeQ")    unTypeQIdKey
unsafeTExpCoerceName = thFun (fsLit "unsafeTExpCoerce") unsafeTExpCoerceIdKey


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
litPName, varPName, tupPName, unboxedTupPName, conPName, infixPName, tildePName, bangPName,
    asPName, wildPName, recPName, listPName, sigPName, viewPName :: Name
litPName   = libFun (fsLit "litP")   litPIdKey
varPName   = libFun (fsLit "varP")   varPIdKey
tupPName   = libFun (fsLit "tupP")   tupPIdKey
unboxedTupPName = libFun (fsLit "unboxedTupP") unboxedTupPIdKey
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
varEName, conEName, litEName, appEName, infixEName, infixAppName,
    sectionLName, sectionRName, lamEName, lamCaseEName, tupEName,
    unboxedTupEName, condEName, multiIfEName, letEName, caseEName,
    doEName, compEName, staticEName, unboundVarEName :: Name
varEName        = libFun (fsLit "varE")        varEIdKey
conEName        = libFun (fsLit "conE")        conEIdKey
litEName        = libFun (fsLit "litE")        litEIdKey
appEName        = libFun (fsLit "appE")        appEIdKey
infixEName      = libFun (fsLit "infixE")      infixEIdKey
infixAppName    = libFun (fsLit "infixApp")    infixAppIdKey
sectionLName    = libFun (fsLit "sectionL")    sectionLIdKey
sectionRName    = libFun (fsLit "sectionR")    sectionRIdKey
lamEName        = libFun (fsLit "lamE")        lamEIdKey
lamCaseEName    = libFun (fsLit "lamCaseE")    lamCaseEIdKey
tupEName        = libFun (fsLit "tupE")        tupEIdKey
unboxedTupEName = libFun (fsLit "unboxedTupE") unboxedTupEIdKey
condEName       = libFun (fsLit "condE")       condEIdKey
multiIfEName    = libFun (fsLit "multiIfE")    multiIfEIdKey
letEName        = libFun (fsLit "letE")        letEIdKey
caseEName       = libFun (fsLit "caseE")       caseEIdKey
doEName         = libFun (fsLit "doE")         doEIdKey
compEName       = libFun (fsLit "compE")       compEIdKey
-- ArithSeq skips a level
fromEName, fromThenEName, fromToEName, fromThenToEName :: Name
fromEName       = libFun (fsLit "fromE")       fromEIdKey
fromThenEName   = libFun (fsLit "fromThenE")   fromThenEIdKey
fromToEName     = libFun (fsLit "fromToE")     fromToEIdKey
fromThenToEName = libFun (fsLit "fromThenToE") fromThenToEIdKey
-- end ArithSeq
listEName, sigEName, recConEName, recUpdEName :: Name
listEName       = libFun (fsLit "listE")       listEIdKey
sigEName        = libFun (fsLit "sigE")        sigEIdKey
recConEName     = libFun (fsLit "recConE")     recConEIdKey
recUpdEName     = libFun (fsLit "recUpdE")     recUpdEIdKey
staticEName     = libFun (fsLit "staticE")     staticEIdKey
unboundVarEName = libFun (fsLit "unboundVarE") unboundVarEIdKey

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
bindSName, letSName, noBindSName, parSName :: Name
bindSName   = libFun (fsLit "bindS")   bindSIdKey
letSName    = libFun (fsLit "letS")    letSIdKey
noBindSName = libFun (fsLit "noBindS") noBindSIdKey
parSName    = libFun (fsLit "parS")    parSIdKey

-- data Dec = ...
funDName, valDName, dataDName, newtypeDName, tySynDName, classDName,
    instanceDName, sigDName, forImpDName, pragInlDName, pragSpecDName,
    pragSpecInlDName, pragSpecInstDName, pragRuleDName, pragAnnDName,
    standaloneDerivDName, defaultSigDName,
    dataInstDName, newtypeInstDName, tySynInstDName,
    dataFamilyDName, openTypeFamilyDName, closedTypeFamilyDName,
    infixLDName, infixRDName, infixNDName, roleAnnotDName :: Name
funDName             = libFun (fsLit "funD")              funDIdKey
valDName             = libFun (fsLit "valD")              valDIdKey
dataDName            = libFun (fsLit "dataD")             dataDIdKey
newtypeDName         = libFun (fsLit "newtypeD")          newtypeDIdKey
tySynDName           = libFun (fsLit "tySynD")            tySynDIdKey
classDName           = libFun (fsLit "classD")            classDIdKey
instanceDName        = libFun (fsLit "instanceD")         instanceDIdKey
standaloneDerivDName = libFun (fsLit "standaloneDerivD")  standaloneDerivDIdKey
sigDName             = libFun (fsLit "sigD")              sigDIdKey
defaultSigDName      = libFun (fsLit "defaultSigD")       defaultSigDIdKey
forImpDName          = libFun (fsLit "forImpD")           forImpDIdKey
pragInlDName         = libFun (fsLit "pragInlD")          pragInlDIdKey
pragSpecDName        = libFun (fsLit "pragSpecD")         pragSpecDIdKey
pragSpecInlDName     = libFun (fsLit "pragSpecInlD")      pragSpecInlDIdKey
pragSpecInstDName    = libFun (fsLit "pragSpecInstD")     pragSpecInstDIdKey
pragRuleDName        = libFun (fsLit "pragRuleD")         pragRuleDIdKey
pragAnnDName         = libFun (fsLit "pragAnnD")          pragAnnDIdKey
dataInstDName        = libFun (fsLit "dataInstD")         dataInstDIdKey
newtypeInstDName     = libFun (fsLit "newtypeInstD")      newtypeInstDIdKey
tySynInstDName       = libFun (fsLit "tySynInstD")        tySynInstDIdKey
openTypeFamilyDName  = libFun (fsLit "openTypeFamilyD")   openTypeFamilyDIdKey
closedTypeFamilyDName= libFun (fsLit "closedTypeFamilyD") closedTypeFamilyDIdKey
dataFamilyDName      = libFun (fsLit "dataFamilyD")       dataFamilyDIdKey
infixLDName          = libFun (fsLit "infixLD")           infixLDIdKey
infixRDName          = libFun (fsLit "infixRD")           infixRDIdKey
infixNDName          = libFun (fsLit "infixND")           infixNDIdKey
roleAnnotDName       = libFun (fsLit "roleAnnotD")        roleAnnotDIdKey

-- type Ctxt = ...
cxtName :: Name
cxtName = libFun (fsLit "cxt") cxtIdKey

-- data Strict = ...
isStrictName, notStrictName, unpackedName :: Name
isStrictName      = libFun  (fsLit "isStrict")      isStrictKey
notStrictName     = libFun  (fsLit "notStrict")     notStrictKey
unpackedName      = libFun  (fsLit "unpacked")      unpackedKey

-- data Con = ...
normalCName, recCName, infixCName, forallCName, gadtCName, recGadtCName :: Name
normalCName  = libFun (fsLit "normalC" ) normalCIdKey
recCName     = libFun (fsLit "recC"    ) recCIdKey
infixCName   = libFun (fsLit "infixC"  ) infixCIdKey
forallCName  = libFun (fsLit "forallC" ) forallCIdKey
gadtCName    = libFun (fsLit "gadtC"   ) gadtCIdKey
recGadtCName = libFun (fsLit "recGadtC") recGadtCIdKey

-- type StrictType = ...
strictTypeName :: Name
strictTypeName    = libFun  (fsLit "strictType")    strictTKey

-- type VarStrictType = ...
varStrictTypeName :: Name
varStrictTypeName = libFun  (fsLit "varStrictType") varStrictTKey

-- data Type = ...
forallTName, varTName, conTName, tupleTName, unboxedTupleTName, arrowTName,
    listTName, appTName, sigTName, equalityTName, litTName,
    promotedTName, promotedTupleTName,
    promotedNilTName, promotedConsTName,
    wildCardTName, namedWildCardTName :: Name
forallTName         = libFun (fsLit "forallT")        forallTIdKey
varTName            = libFun (fsLit "varT")           varTIdKey
conTName            = libFun (fsLit "conT")           conTIdKey
tupleTName          = libFun (fsLit "tupleT")         tupleTIdKey
unboxedTupleTName   = libFun (fsLit "unboxedTupleT")  unboxedTupleTIdKey
arrowTName          = libFun (fsLit "arrowT")         arrowTIdKey
listTName           = libFun (fsLit "listT")          listTIdKey
appTName            = libFun (fsLit "appT")           appTIdKey
sigTName            = libFun (fsLit "sigT")           sigTIdKey
equalityTName       = libFun (fsLit "equalityT")      equalityTIdKey
litTName            = libFun (fsLit "litT")           litTIdKey
promotedTName       = libFun (fsLit "promotedT")      promotedTIdKey
promotedTupleTName  = libFun (fsLit "promotedTupleT") promotedTupleTIdKey
promotedNilTName    = libFun (fsLit "promotedNilT")   promotedNilTIdKey
promotedConsTName   = libFun (fsLit "promotedConsT")  promotedConsTIdKey
wildCardTName       = libFun (fsLit "wildCardT")      wildCardTIdKey
namedWildCardTName  = libFun (fsLit "namedWildCardT") namedWildCardTIdKey


-- data TyLit = ...
numTyLitName, strTyLitName :: Name
numTyLitName = libFun (fsLit "numTyLit") numTyLitIdKey
strTyLitName = libFun (fsLit "strTyLit") strTyLitIdKey

-- data TyVarBndr = ...
plainTVName, kindedTVName :: Name
plainTVName       = libFun (fsLit "plainTV")       plainTVIdKey
kindedTVName      = libFun (fsLit "kindedTV")      kindedTVIdKey

-- data Role = ...
nominalRName, representationalRName, phantomRName, inferRName :: Name
nominalRName          = libFun (fsLit "nominalR")          nominalRIdKey
representationalRName = libFun (fsLit "representationalR") representationalRIdKey
phantomRName          = libFun (fsLit "phantomR")          phantomRIdKey
inferRName            = libFun (fsLit "inferR")            inferRIdKey

-- data Kind = ...
varKName, conKName, tupleKName, arrowKName, listKName, appKName,
  starKName, constraintKName :: Name
varKName        = libFun (fsLit "varK")         varKIdKey
conKName        = libFun (fsLit "conK")         conKIdKey
tupleKName      = libFun (fsLit "tupleK")       tupleKIdKey
arrowKName      = libFun (fsLit "arrowK")       arrowKIdKey
listKName       = libFun (fsLit "listK")        listKIdKey
appKName        = libFun (fsLit "appK")         appKIdKey
starKName       = libFun (fsLit "starK")        starKIdKey
constraintKName = libFun (fsLit "constraintK")  constraintKIdKey

-- data FamilyResultSig = ...
noSigName, kindSigName, tyVarSigName :: Name
noSigName       = libFun (fsLit "noSig")        noSigIdKey
kindSigName     = libFun (fsLit "kindSig")      kindSigIdKey
tyVarSigName    = libFun (fsLit "tyVarSig")     tyVarSigIdKey

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

-- newtype TExp a = ...
tExpDataConName :: Name
tExpDataConName = thCon (fsLit "TExp") tExpDataConKey

-- data RuleBndr = ...
ruleVarName, typedRuleVarName :: Name
ruleVarName      = libFun (fsLit ("ruleVar"))      ruleVarIdKey
typedRuleVarName = libFun (fsLit ("typedRuleVar")) typedRuleVarIdKey

-- data FunDep = ...
funDepName :: Name
funDepName     = libFun (fsLit "funDep") funDepIdKey

-- data FamFlavour = ...
typeFamName, dataFamName :: Name
typeFamName = libFun (fsLit "typeFam") typeFamIdKey
dataFamName = libFun (fsLit "dataFam") dataFamIdKey

-- data TySynEqn = ...
tySynEqnName :: Name
tySynEqnName = libFun (fsLit "tySynEqn") tySynEqnIdKey

-- data AnnTarget = ...
valueAnnotationName, typeAnnotationName, moduleAnnotationName :: Name
valueAnnotationName  = libFun (fsLit "valueAnnotation")  valueAnnotationIdKey
typeAnnotationName   = libFun (fsLit "typeAnnotation")   typeAnnotationIdKey
moduleAnnotationName = libFun (fsLit "moduleAnnotation") moduleAnnotationIdKey

matchQTyConName, clauseQTyConName, expQTyConName, stmtQTyConName,
    decQTyConName, conQTyConName, strictTypeQTyConName,
    varStrictTypeQTyConName, typeQTyConName, fieldExpQTyConName,
    patQTyConName, fieldPatQTyConName, predQTyConName, decsQTyConName,
    ruleBndrQTyConName, tySynEqnQTyConName, roleTyConName :: Name
matchQTyConName         = libTc (fsLit "MatchQ")         matchQTyConKey
clauseQTyConName        = libTc (fsLit "ClauseQ")        clauseQTyConKey
expQTyConName           = libTc (fsLit "ExpQ")           expQTyConKey
stmtQTyConName          = libTc (fsLit "StmtQ")          stmtQTyConKey
decQTyConName           = libTc (fsLit "DecQ")           decQTyConKey
decsQTyConName          = libTc (fsLit "DecsQ")          decsQTyConKey  -- Q [Dec]
conQTyConName           = libTc (fsLit "ConQ")           conQTyConKey
strictTypeQTyConName    = libTc (fsLit "StrictTypeQ")    strictTypeQTyConKey
varStrictTypeQTyConName = libTc (fsLit "VarStrictTypeQ") varStrictTypeQTyConKey
typeQTyConName          = libTc (fsLit "TypeQ")          typeQTyConKey
fieldExpQTyConName      = libTc (fsLit "FieldExpQ")      fieldExpQTyConKey
patQTyConName           = libTc (fsLit "PatQ")           patQTyConKey
fieldPatQTyConName      = libTc (fsLit "FieldPatQ")      fieldPatQTyConKey
predQTyConName          = libTc (fsLit "PredQ")          predQTyConKey
ruleBndrQTyConName      = libTc (fsLit "RuleBndrQ")      ruleBndrQTyConKey
tySynEqnQTyConName      = libTc (fsLit "TySynEqnQ")      tySynEqnQTyConKey
roleTyConName           = libTc (fsLit "Role")           roleTyConKey

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


{- *********************************************************************
*                                                                      *
                     Class keys
*                                                                      *
********************************************************************* -}

-- ClassUniques available: 200-299
-- Check in PrelNames if you want to change this

liftClassKey :: Unique
liftClassKey = mkPreludeClassUnique 200

{- *********************************************************************
*                                                                      *
                     TyCon keys
*                                                                      *
********************************************************************* -}

-- TyConUniques available: 200-299
-- Check in PrelNames if you want to change this

expTyConKey, matchTyConKey, clauseTyConKey, qTyConKey, expQTyConKey,
    decQTyConKey, patTyConKey, matchQTyConKey, clauseQTyConKey,
    stmtQTyConKey, conQTyConKey, typeQTyConKey, typeTyConKey, tyVarBndrTyConKey,
    decTyConKey, varStrictTypeQTyConKey, strictTypeQTyConKey,
    fieldExpTyConKey, fieldPatTyConKey, nameTyConKey, patQTyConKey,
    fieldPatQTyConKey, fieldExpQTyConKey, funDepTyConKey, predTyConKey,
    predQTyConKey, decsQTyConKey, ruleBndrQTyConKey, tySynEqnQTyConKey,
    roleTyConKey, tExpTyConKey, injAnnTyConKey, kindTyConKey :: Unique
expTyConKey             = mkPreludeTyConUnique 200
matchTyConKey           = mkPreludeTyConUnique 201
clauseTyConKey          = mkPreludeTyConUnique 202
qTyConKey               = mkPreludeTyConUnique 203
expQTyConKey            = mkPreludeTyConUnique 204
decQTyConKey            = mkPreludeTyConUnique 205
patTyConKey             = mkPreludeTyConUnique 206
matchQTyConKey          = mkPreludeTyConUnique 207
clauseQTyConKey         = mkPreludeTyConUnique 208
stmtQTyConKey           = mkPreludeTyConUnique 209
conQTyConKey            = mkPreludeTyConUnique 210
typeQTyConKey           = mkPreludeTyConUnique 211
typeTyConKey            = mkPreludeTyConUnique 212
decTyConKey             = mkPreludeTyConUnique 213
varStrictTypeQTyConKey  = mkPreludeTyConUnique 214
strictTypeQTyConKey     = mkPreludeTyConUnique 215
fieldExpTyConKey        = mkPreludeTyConUnique 216
fieldPatTyConKey        = mkPreludeTyConUnique 217
nameTyConKey            = mkPreludeTyConUnique 218
patQTyConKey            = mkPreludeTyConUnique 219
fieldPatQTyConKey       = mkPreludeTyConUnique 220
fieldExpQTyConKey       = mkPreludeTyConUnique 221
funDepTyConKey          = mkPreludeTyConUnique 222
predTyConKey            = mkPreludeTyConUnique 223
predQTyConKey           = mkPreludeTyConUnique 224
tyVarBndrTyConKey       = mkPreludeTyConUnique 225
decsQTyConKey           = mkPreludeTyConUnique 226
ruleBndrQTyConKey       = mkPreludeTyConUnique 227
tySynEqnQTyConKey       = mkPreludeTyConUnique 228
roleTyConKey            = mkPreludeTyConUnique 229
tExpTyConKey            = mkPreludeTyConUnique 230
injAnnTyConKey          = mkPreludeTyConUnique 231
kindTyConKey            = mkPreludeTyConUnique 232

{- *********************************************************************
*                                                                      *
                     DataCon keys
*                                                                      *
********************************************************************* -}

-- DataConUniques available: 100-150
-- If you want to change this, make sure you check in PrelNames

-- data Inline = ...
noInlineDataConKey, inlineDataConKey, inlinableDataConKey :: Unique
noInlineDataConKey  = mkPreludeDataConUnique 100
inlineDataConKey    = mkPreludeDataConUnique 101
inlinableDataConKey = mkPreludeDataConUnique 102

-- data RuleMatch = ...
conLikeDataConKey, funLikeDataConKey :: Unique
conLikeDataConKey = mkPreludeDataConUnique 103
funLikeDataConKey = mkPreludeDataConUnique 104

-- data Phases = ...
allPhasesDataConKey, fromPhaseDataConKey, beforePhaseDataConKey :: Unique
allPhasesDataConKey   = mkPreludeDataConUnique 105
fromPhaseDataConKey   = mkPreludeDataConUnique 106
beforePhaseDataConKey = mkPreludeDataConUnique 107

-- newtype TExp a = ...
tExpDataConKey :: Unique
tExpDataConKey = mkPreludeDataConUnique 108


{- *********************************************************************
*                                                                      *
                     Id keys
*                                                                      *
********************************************************************* -}

-- IdUniques available: 200-499
-- If you want to change this, make sure you check in PrelNames

returnQIdKey, bindQIdKey, sequenceQIdKey, liftIdKey, newNameIdKey,
    mkNameIdKey, mkNameG_vIdKey, mkNameG_dIdKey, mkNameG_tcIdKey,
    mkNameLIdKey, mkNameSIdKey, unTypeIdKey, unTypeQIdKey,
    unsafeTExpCoerceIdKey :: Unique
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
unTypeQIdKey         = mkPreludeMiscIdUnique 212
unsafeTExpCoerceIdKey = mkPreludeMiscIdUnique 213


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
litPIdKey, varPIdKey, tupPIdKey, unboxedTupPIdKey, conPIdKey, infixPIdKey, tildePIdKey, bangPIdKey,
    asPIdKey, wildPIdKey, recPIdKey, listPIdKey, sigPIdKey, viewPIdKey :: Unique
litPIdKey         = mkPreludeMiscIdUnique 240
varPIdKey         = mkPreludeMiscIdUnique 241
tupPIdKey         = mkPreludeMiscIdUnique 242
unboxedTupPIdKey  = mkPreludeMiscIdUnique 243
conPIdKey         = mkPreludeMiscIdUnique 244
infixPIdKey       = mkPreludeMiscIdUnique 245
tildePIdKey       = mkPreludeMiscIdUnique 246
bangPIdKey        = mkPreludeMiscIdUnique 247
asPIdKey          = mkPreludeMiscIdUnique 248
wildPIdKey        = mkPreludeMiscIdUnique 249
recPIdKey         = mkPreludeMiscIdUnique 250
listPIdKey        = mkPreludeMiscIdUnique 251
sigPIdKey         = mkPreludeMiscIdUnique 252
viewPIdKey        = mkPreludeMiscIdUnique 253

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
varEIdKey, conEIdKey, litEIdKey, appEIdKey, infixEIdKey, infixAppIdKey,
    sectionLIdKey, sectionRIdKey, lamEIdKey, lamCaseEIdKey, tupEIdKey,
    unboxedTupEIdKey, condEIdKey, multiIfEIdKey,
    letEIdKey, caseEIdKey, doEIdKey, compEIdKey,
    fromEIdKey, fromThenEIdKey, fromToEIdKey, fromThenToEIdKey,
    listEIdKey, sigEIdKey, recConEIdKey, recUpdEIdKey, staticEIdKey,
    unboundVarEIdKey :: Unique
varEIdKey         = mkPreludeMiscIdUnique 270
conEIdKey         = mkPreludeMiscIdUnique 271
litEIdKey         = mkPreludeMiscIdUnique 272
appEIdKey         = mkPreludeMiscIdUnique 273
infixEIdKey       = mkPreludeMiscIdUnique 274
infixAppIdKey     = mkPreludeMiscIdUnique 275
sectionLIdKey     = mkPreludeMiscIdUnique 276
sectionRIdKey     = mkPreludeMiscIdUnique 277
lamEIdKey         = mkPreludeMiscIdUnique 278
lamCaseEIdKey     = mkPreludeMiscIdUnique 279
tupEIdKey         = mkPreludeMiscIdUnique 280
unboxedTupEIdKey  = mkPreludeMiscIdUnique 281
condEIdKey        = mkPreludeMiscIdUnique 282
multiIfEIdKey     = mkPreludeMiscIdUnique 283
letEIdKey         = mkPreludeMiscIdUnique 284
caseEIdKey        = mkPreludeMiscIdUnique 285
doEIdKey          = mkPreludeMiscIdUnique 286
compEIdKey        = mkPreludeMiscIdUnique 287
fromEIdKey        = mkPreludeMiscIdUnique 288
fromThenEIdKey    = mkPreludeMiscIdUnique 289
fromToEIdKey      = mkPreludeMiscIdUnique 290
fromThenToEIdKey  = mkPreludeMiscIdUnique 291
listEIdKey        = mkPreludeMiscIdUnique 292
sigEIdKey         = mkPreludeMiscIdUnique 293
recConEIdKey      = mkPreludeMiscIdUnique 294
recUpdEIdKey      = mkPreludeMiscIdUnique 295
staticEIdKey      = mkPreludeMiscIdUnique 296
unboundVarEIdKey  = mkPreludeMiscIdUnique 297

-- type FieldExp = ...
fieldExpIdKey :: Unique
fieldExpIdKey       = mkPreludeMiscIdUnique 310

-- data Body = ...
guardedBIdKey, normalBIdKey :: Unique
guardedBIdKey     = mkPreludeMiscIdUnique 311
normalBIdKey      = mkPreludeMiscIdUnique 312

-- data Guard = ...
normalGEIdKey, patGEIdKey :: Unique
normalGEIdKey     = mkPreludeMiscIdUnique 313
patGEIdKey        = mkPreludeMiscIdUnique 314

-- data Stmt = ...
bindSIdKey, letSIdKey, noBindSIdKey, parSIdKey :: Unique
bindSIdKey       = mkPreludeMiscIdUnique 320
letSIdKey        = mkPreludeMiscIdUnique 321
noBindSIdKey     = mkPreludeMiscIdUnique 322
parSIdKey        = mkPreludeMiscIdUnique 323

-- data Dec = ...
funDIdKey, valDIdKey, dataDIdKey, newtypeDIdKey, tySynDIdKey,
    classDIdKey, instanceDIdKey, sigDIdKey, forImpDIdKey, pragInlDIdKey,
    pragSpecDIdKey, pragSpecInlDIdKey, pragSpecInstDIdKey, pragRuleDIdKey,
    pragAnnDIdKey, defaultSigDIdKey, dataFamilyDIdKey, openTypeFamilyDIdKey,
    closedTypeFamilyDIdKey, dataInstDIdKey, newtypeInstDIdKey, tySynInstDIdKey,
    standaloneDerivDIdKey, infixLDIdKey, infixRDIdKey, infixNDIdKey,
    roleAnnotDIdKey :: Unique
funDIdKey              = mkPreludeMiscIdUnique 330
valDIdKey              = mkPreludeMiscIdUnique 331
dataDIdKey             = mkPreludeMiscIdUnique 332
newtypeDIdKey          = mkPreludeMiscIdUnique 333
tySynDIdKey            = mkPreludeMiscIdUnique 334
classDIdKey            = mkPreludeMiscIdUnique 335
instanceDIdKey         = mkPreludeMiscIdUnique 336
sigDIdKey              = mkPreludeMiscIdUnique 337
forImpDIdKey           = mkPreludeMiscIdUnique 338
pragInlDIdKey          = mkPreludeMiscIdUnique 339
pragSpecDIdKey         = mkPreludeMiscIdUnique 340
pragSpecInlDIdKey      = mkPreludeMiscIdUnique 341
pragSpecInstDIdKey     = mkPreludeMiscIdUnique 342
pragRuleDIdKey         = mkPreludeMiscIdUnique 343
pragAnnDIdKey          = mkPreludeMiscIdUnique 344
dataFamilyDIdKey       = mkPreludeMiscIdUnique 345
openTypeFamilyDIdKey   = mkPreludeMiscIdUnique 346
dataInstDIdKey         = mkPreludeMiscIdUnique 347
newtypeInstDIdKey      = mkPreludeMiscIdUnique 348
tySynInstDIdKey        = mkPreludeMiscIdUnique 349
closedTypeFamilyDIdKey = mkPreludeMiscIdUnique 350
infixLDIdKey           = mkPreludeMiscIdUnique 352
infixRDIdKey           = mkPreludeMiscIdUnique 353
infixNDIdKey           = mkPreludeMiscIdUnique 354
roleAnnotDIdKey        = mkPreludeMiscIdUnique 355
standaloneDerivDIdKey  = mkPreludeMiscIdUnique 356
defaultSigDIdKey       = mkPreludeMiscIdUnique 357

-- type Cxt = ...
cxtIdKey :: Unique
cxtIdKey            = mkPreludeMiscIdUnique 360

-- data Strict = ...
isStrictKey, notStrictKey, unpackedKey :: Unique
isStrictKey         = mkPreludeMiscIdUnique 363
notStrictKey        = mkPreludeMiscIdUnique 364
unpackedKey         = mkPreludeMiscIdUnique 365

-- data Con = ...
normalCIdKey, recCIdKey, infixCIdKey, forallCIdKey, gadtCIdKey,
  recGadtCIdKey :: Unique
normalCIdKey      = mkPreludeMiscIdUnique 370
recCIdKey         = mkPreludeMiscIdUnique 371
infixCIdKey       = mkPreludeMiscIdUnique 372
forallCIdKey      = mkPreludeMiscIdUnique 373
gadtCIdKey        = mkPreludeMiscIdUnique 374
recGadtCIdKey     = mkPreludeMiscIdUnique 375

-- type StrictType = ...
strictTKey :: Unique
strictTKey        = mkPreludeMiscIdUnique 376

-- type VarStrictType = ...
varStrictTKey :: Unique
varStrictTKey     = mkPreludeMiscIdUnique 377

-- data Type = ...
forallTIdKey, varTIdKey, conTIdKey, tupleTIdKey, unboxedTupleTIdKey, arrowTIdKey,
    listTIdKey, appTIdKey, sigTIdKey, equalityTIdKey, litTIdKey,
    promotedTIdKey, promotedTupleTIdKey,
    promotedNilTIdKey, promotedConsTIdKey,
    wildCardTIdKey, namedWildCardTIdKey :: Unique
forallTIdKey        = mkPreludeMiscIdUnique 380
varTIdKey           = mkPreludeMiscIdUnique 381
conTIdKey           = mkPreludeMiscIdUnique 382
tupleTIdKey         = mkPreludeMiscIdUnique 383
unboxedTupleTIdKey  = mkPreludeMiscIdUnique 384
arrowTIdKey         = mkPreludeMiscIdUnique 385
listTIdKey          = mkPreludeMiscIdUnique 386
appTIdKey           = mkPreludeMiscIdUnique 387
sigTIdKey           = mkPreludeMiscIdUnique 388
equalityTIdKey      = mkPreludeMiscIdUnique 389
litTIdKey           = mkPreludeMiscIdUnique 390
promotedTIdKey      = mkPreludeMiscIdUnique 391
promotedTupleTIdKey = mkPreludeMiscIdUnique 392
promotedNilTIdKey   = mkPreludeMiscIdUnique 393
promotedConsTIdKey  = mkPreludeMiscIdUnique 394
wildCardTIdKey      = mkPreludeMiscIdUnique 395
namedWildCardTIdKey = mkPreludeMiscIdUnique 396

-- data TyLit = ...
numTyLitIdKey, strTyLitIdKey :: Unique
numTyLitIdKey = mkPreludeMiscIdUnique 400
strTyLitIdKey = mkPreludeMiscIdUnique 401

-- data TyVarBndr = ...
plainTVIdKey, kindedTVIdKey :: Unique
plainTVIdKey       = mkPreludeMiscIdUnique 402
kindedTVIdKey      = mkPreludeMiscIdUnique 403

-- data Role = ...
nominalRIdKey, representationalRIdKey, phantomRIdKey, inferRIdKey :: Unique
nominalRIdKey          = mkPreludeMiscIdUnique 404
representationalRIdKey = mkPreludeMiscIdUnique 405
phantomRIdKey          = mkPreludeMiscIdUnique 406
inferRIdKey            = mkPreludeMiscIdUnique 407

-- data Kind = ...
varKIdKey, conKIdKey, tupleKIdKey, arrowKIdKey, listKIdKey, appKIdKey,
  starKIdKey, constraintKIdKey :: Unique
varKIdKey         = mkPreludeMiscIdUnique 408
conKIdKey         = mkPreludeMiscIdUnique 409
tupleKIdKey       = mkPreludeMiscIdUnique 410
arrowKIdKey       = mkPreludeMiscIdUnique 411
listKIdKey        = mkPreludeMiscIdUnique 412
appKIdKey         = mkPreludeMiscIdUnique 413
starKIdKey        = mkPreludeMiscIdUnique 414
constraintKIdKey  = mkPreludeMiscIdUnique 415

-- data FamilyResultSig = ...
noSigIdKey, kindSigIdKey, tyVarSigIdKey :: Unique
noSigIdKey        = mkPreludeMiscIdUnique 416
kindSigIdKey      = mkPreludeMiscIdUnique 417
tyVarSigIdKey     = mkPreludeMiscIdUnique 418

-- data InjectivityAnn = ...
injectivityAnnIdKey :: Unique
injectivityAnnIdKey = mkPreludeMiscIdUnique 419

-- data Callconv = ...
cCallIdKey, stdCallIdKey, cApiCallIdKey, primCallIdKey,
  javaScriptCallIdKey :: Unique
cCallIdKey          = mkPreludeMiscIdUnique 420
stdCallIdKey        = mkPreludeMiscIdUnique 421
cApiCallIdKey       = mkPreludeMiscIdUnique 422
primCallIdKey       = mkPreludeMiscIdUnique 423
javaScriptCallIdKey = mkPreludeMiscIdUnique 424

-- data Safety = ...
unsafeIdKey, safeIdKey, interruptibleIdKey :: Unique
unsafeIdKey        = mkPreludeMiscIdUnique 430
safeIdKey          = mkPreludeMiscIdUnique 431
interruptibleIdKey = mkPreludeMiscIdUnique 432

-- data FunDep = ...
funDepIdKey :: Unique
funDepIdKey = mkPreludeMiscIdUnique 440

-- data FamFlavour = ...
typeFamIdKey, dataFamIdKey :: Unique
typeFamIdKey = mkPreludeMiscIdUnique 450
dataFamIdKey = mkPreludeMiscIdUnique 451

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

{-
************************************************************************
*                                                                      *
                        RdrNames
*                                                                      *
************************************************************************
-}

lift_RDR, mkNameG_dRDR, mkNameG_vRDR :: RdrName
lift_RDR     = nameRdrName liftName
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
