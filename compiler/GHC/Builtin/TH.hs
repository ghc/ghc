-- %************************************************************************
-- %*                                                                   *
--              The known-key names for Template Haskell
-- %*                                                                   *
-- %************************************************************************

module GHC.Builtin.TH where

import GHC.Prelude ()

import GHC.Types.Name( KnownOcc )
import GHC.Types.Name.Occurrence
import GHC.Data.FastString

-------------------- TH.Syntax -----------------------
quoteClassOcc :: KnownOcc
quoteClassOcc = mkTcOcc "Quote"

qTyConOcc, nameTyConOcc, fieldExpTyConOcc, patTyConOcc, fieldPatTyConOcc,
  expTyConOcc, decTyConOcc, typeTyConOcc, matchTyConOcc, funDepTyConOcc,
  codeTyConOcc, injAnnTyConOcc, overlapTyConOcc, decsTyConOcc, modNameTyConOcc,
  quasiQuoterTyConOcc :: KnownOcc
qTyConOcc             = mkTcOcc "Q"
nameTyConOcc          = mkTcOcc "Name"
fieldExpTyConOcc      = mkTcOcc "FieldExp"
patTyConOcc           = mkTcOcc "Pat"
fieldPatTyConOcc      = mkTcOcc "FieldPat"
expTyConOcc           = mkTcOcc "Exp"
decTyConOcc           = mkTcOcc "Dec"
decsTyConOcc          = mkTcOcc "Decs"
typeTyConOcc          = mkTcOcc "Type"
matchTyConOcc         = mkTcOcc "Match"
funDepTyConOcc        = mkTcOcc "FunDep"
codeTyConOcc          = mkTcOcc "Code"
injAnnTyConOcc        = mkTcOcc "InjectivityAnn"
overlapTyConOcc       = mkTcOcc "Overlap"
modNameTyConOcc       = mkTcOcc "ModName"
quasiQuoterTyConOcc   = mkTcOcc "QuasiQuoter"

sequenceQOcc :: KnownOcc
sequenceQOcc = mkVarOcc "sequenceQ"

newNameOcc, mkNameG_vOcc, mkNameG_fldOcc, mkNameG_dOcc, mkNameG_tcOcc,
  mkNameLOcc, mkNameSOcc, unTypeCodeOcc, mkModNameOcc, mkNameQOcc :: KnownOcc
newNameOcc    = mkVarOcc "newName"
mkNameG_vOcc  = mkVarOcc "mkNameG_v"
mkNameG_dOcc  = mkVarOcc "mkNameG_d"
mkNameG_tcOcc = mkVarOcc "mkNameG_tc"
mkNameG_fldOcc= mkVarOcc "mkNameG_fld"
mkNameLOcc    = mkVarOcc "mkNameL"
mkNameQOcc    = mkVarOcc "mkNameQ"
mkNameSOcc    = mkVarOcc "mkNameS"
mkModNameOcc  = mkVarOcc "mkModName"
unTypeCodeOcc = mkVarOcc "unTypeCode"

liftIdOcc, unsafeCodeCoerceOcc :: KnownOcc
liftIdOcc           = mkVarOcc "lift"
unsafeCodeCoerceOcc = mkVarOcc "unsafeCodeCoerce"

-------------------- TH.Lib -----------------------
-- data Lit = ...
charLOcc, stringLOcc, integerLOcc, intPrimLOcc, wordPrimLOcc,
    floatPrimLOcc, doublePrimLOcc, rationalLOcc, stringPrimLOcc,
    charPrimLOcc :: KnownOcc
charLOcc       = mkVarOcc "charL"
stringLOcc     = mkVarOcc "stringL"
integerLOcc    = mkVarOcc "integerL"
intPrimLOcc    = mkVarOcc "intPrimL"
wordPrimLOcc   = mkVarOcc "wordPrimL"
floatPrimLOcc  = mkVarOcc "floatPrimL"
doublePrimLOcc = mkVarOcc "doublePrimL"
rationalLOcc   = mkVarOcc "rationalL"
stringPrimLOcc = mkVarOcc "stringPrimL"
charPrimLOcc   = mkVarOcc "charPrimL"

-- data Pat = ...
litPOcc, varPOcc, tupPOcc, unboxedTupPOcc, unboxedSumPOcc, conPOcc,
    infixPOcc, tildePOcc, bangPOcc, asPOcc, wildPOcc, recPOcc, listPOcc,
    sigPOcc, viewPOcc, typePOcc, invisPOcc, orPOcc :: KnownOcc
litPOcc        = mkVarOcc "litP"
varPOcc        = mkVarOcc "varP"
tupPOcc        = mkVarOcc "tupP"
unboxedTupPOcc = mkVarOcc "unboxedTupP"
unboxedSumPOcc = mkVarOcc "unboxedSumP"
conPOcc        = mkVarOcc "conP"
infixPOcc      = mkVarOcc "infixP"
tildePOcc      = mkVarOcc "tildeP"
bangPOcc       = mkVarOcc "bangP"
asPOcc         = mkVarOcc "asP"
wildPOcc       = mkVarOcc "wildP"
recPOcc        = mkVarOcc "recP"
listPOcc       = mkVarOcc "listP"
sigPOcc        = mkVarOcc "sigP"
viewPOcc       = mkVarOcc "viewP"
orPOcc         = mkVarOcc "orP"
typePOcc       = mkVarOcc "typeP"
invisPOcc      = mkVarOcc "invisP"

-- type FieldPat = ...
fieldPatOcc, matchOcc, clauseOcc ::KnownOcc
fieldPatOcc = mkVarOcc "fieldPat"
matchOcc    = mkVarOcc "match"
clauseOcc   = mkVarOcc "clause"

-- data Exp = ...
varEOcc, conEOcc, litEOcc, appEOcc, appTypeEOcc, infixAppOcc, sectionLOcc,
  sectionROcc, lamEOcc, lamCaseEOcc, lamCasesEOcc, tupEOcc, unboxedTupEOcc,
  unboxedSumEOcc, condEOcc, multiIfEOcc, letEOcc, caseEOcc, doEOcc, mdoEOcc,
  compEOcc, staticEOcc, unboundVarEOcc, labelEOcc, implicitParamVarEOcc,
  getFieldEOcc, projectionEOcc, typeEOcc, forallEOcc, forallVisEOcc,
  constrainedEOcc :: KnownOcc
varEOcc              = mkVarOcc "varE"
conEOcc              = mkVarOcc "conE"
litEOcc              = mkVarOcc "litE"
appEOcc              = mkVarOcc "appE"
appTypeEOcc          = mkVarOcc "appTypeE"
infixAppOcc          = mkVarOcc "infixApp"
sectionLOcc          = mkVarOcc "sectionL"
sectionROcc          = mkVarOcc "sectionR"
lamEOcc              = mkVarOcc "lamE"
lamCaseEOcc          = mkVarOcc "lamCaseE"
lamCasesEOcc         = mkVarOcc "lamCasesE"
tupEOcc              = mkVarOcc "tupE"
unboxedTupEOcc       = mkVarOcc "unboxedTupE"
unboxedSumEOcc       = mkVarOcc "unboxedSumE"
condEOcc             = mkVarOcc "condE"
multiIfEOcc          = mkVarOcc "multiIfE"
letEOcc              = mkVarOcc "letE"
caseEOcc             = mkVarOcc "caseE"
doEOcc               = mkVarOcc "doE"
mdoEOcc              = mkVarOcc "mdoE"
compEOcc             = mkVarOcc "compE"
-- ArithSeq skips a level
fromEOcc, fromThenEOcc, fromToEOcc, fromThenToEOcc :: KnownOcc
fromEOcc             = mkVarOcc "fromE"
fromThenEOcc         = mkVarOcc "fromThenE"
fromToEOcc           = mkVarOcc "fromToE"
fromThenToEOcc       = mkVarOcc "fromThenToE"
-- end ArithSeq
listEOcc, sigEOcc, recConEOcc, recUpdEOcc :: KnownOcc
listEOcc             = mkVarOcc "listE"
sigEOcc              = mkVarOcc "sigE"
recConEOcc           = mkVarOcc "recConE"
recUpdEOcc           = mkVarOcc "recUpdE"
staticEOcc           = mkVarOcc "staticE"
unboundVarEOcc       = mkVarOcc "unboundVarE"
labelEOcc            = mkVarOcc "labelE"
implicitParamVarEOcc = mkVarOcc "implicitParamVarE"
getFieldEOcc         = mkVarOcc "getFieldE"
projectionEOcc       = mkVarOcc "projectionE"
typeEOcc             = mkVarOcc "typeE"
forallEOcc           = mkVarOcc "forallE"
forallVisEOcc        = mkVarOcc "forallVisE"
constrainedEOcc      = mkVarOcc "constrainedE"

-- type FieldExp = ...
fieldExpOcc :: KnownOcc
fieldExpOcc = mkVarOcc "fieldExp"

-- data Body = ...
guardedBOcc, normalBOcc :: KnownOcc
guardedBOcc = mkVarOcc "guardedB"
normalBOcc  = mkVarOcc "normalB"

-- data Guard = ...
normalGEOcc, patGEOcc :: KnownOcc
normalGEOcc = mkVarOcc "normalGE"
patGEOcc    = mkVarOcc "patGE"

-- data Stmt = ...
bindSOcc, letSOcc, noBindSOcc, parSOcc, recSOcc :: KnownOcc
bindSOcc   = mkVarOcc "bindS"
letSOcc    = mkVarOcc "letS"
noBindSOcc = mkVarOcc "noBindS"
parSOcc    = mkVarOcc "parS"
recSOcc    = mkVarOcc "recS"

-- data Dec = ...
funDOcc, valDOcc, dataDOcc, newtypeDOcc, typeDataDOcc, tySynDOcc, classDOcc,
    instanceWithOverlapDOcc, sigDOcc, kiSigDOcc, forImpDOcc, pragInlDOcc,
    pragSpecDOcc, pragSpecInlDOcc, pragSpecEDOcc, pragSpecInlEDOcc,
    pragSpecInstDOcc, pragRuleDOcc,
    pragAnnDOcc, pragSCCFunDOcc, pragSCCFunNamedDOcc,
    standaloneDerivWithStrategyDOcc, defaultSigDOcc, defaultDOcc,
    dataInstDOcc, newtypeInstDOcc, tySynInstDOcc, dataFamilyDOcc,
    openTypeFamilyDOcc, closedTypeFamilyDOcc, infixLWithSpecDOcc,
    infixRWithSpecDOcc, infixNWithSpecDOcc, roleAnnotDOcc, patSynDOcc,
    patSynSigDOcc, pragCompleteDOcc, implicitParamBindDOcc, pragOpaqueDOcc :: KnownOcc
funDOcc                         = mkVarOcc "funD"
valDOcc                         = mkVarOcc "valD"
dataDOcc                        = mkVarOcc "dataD"
newtypeDOcc                     = mkVarOcc "newtypeD"
typeDataDOcc                    = mkVarOcc "typeDataD"
tySynDOcc                       = mkVarOcc "tySynD"
classDOcc                       = mkVarOcc "classD"
instanceWithOverlapDOcc         = mkVarOcc "instanceWithOverlapD"
standaloneDerivWithStrategyDOcc = mkVarOcc "standaloneDerivWithStrategyD"
sigDOcc                         = mkVarOcc "sigD"
kiSigDOcc                       = mkVarOcc "kiSigD"
defaultDOcc                     = mkVarOcc "defaultD"
defaultSigDOcc                  = mkVarOcc "defaultSigD"
forImpDOcc                      = mkVarOcc "forImpD"
pragInlDOcc                     = mkVarOcc "pragInlD"
pragOpaqueDOcc                  = mkVarOcc "pragOpaqueD"
pragSpecDOcc                    = mkVarOcc "pragSpecD"
pragSpecInlDOcc                 = mkVarOcc "pragSpecInlD"
pragSpecEDOcc                   = mkVarOcc "pragSpecED"
pragSpecInlEDOcc                = mkVarOcc "pragSpecInlED"
pragSpecInstDOcc                = mkVarOcc "pragSpecInstD"
pragRuleDOcc                    = mkVarOcc "pragRuleD"
pragCompleteDOcc                = mkVarOcc "pragCompleteD"
pragAnnDOcc                     = mkVarOcc "pragAnnD"
pragSCCFunDOcc                  = mkVarOcc "pragSCCFunD"
pragSCCFunNamedDOcc             = mkVarOcc "pragSCCFunNamedD"
dataInstDOcc                    = mkVarOcc "dataInstD"
newtypeInstDOcc                 = mkVarOcc "newtypeInstD"
tySynInstDOcc                   = mkVarOcc "tySynInstD"
openTypeFamilyDOcc              = mkVarOcc "openTypeFamilyD"
closedTypeFamilyDOcc            = mkVarOcc "closedTypeFamilyD"
dataFamilyDOcc                  = mkVarOcc "dataFamilyD"
infixLWithSpecDOcc              = mkVarOcc "infixLWithSpecD"
infixRWithSpecDOcc              = mkVarOcc "infixRWithSpecD"
infixNWithSpecDOcc              = mkVarOcc "infixNWithSpecD"
roleAnnotDOcc                   = mkVarOcc "roleAnnotD"
patSynDOcc                      = mkVarOcc "patSynD"
patSynSigDOcc                   = mkVarOcc "patSynSigD"
implicitParamBindDOcc           = mkVarOcc "implicitParamBindD"

-- type Ctxt = ...
cxtName :: KnownOcc
cxtName = mkVarOcc "cxt"

-- data SourceUnpackedness = ...
noSourceUnpackednessOcc, sourceNoUnpackOcc, sourceUnpackOcc :: KnownOcc
noSourceUnpackednessOcc = mkVarOcc "noSourceUnpackedness"
sourceNoUnpackOcc       = mkVarOcc "sourceNoUnpack"
sourceUnpackOcc         = mkVarOcc "sourceUnpack"

-- data SourceStrictness = ...
noSourceStrictnessOcc, sourceLazyOcc, sourceStrictOcc :: KnownOcc
noSourceStrictnessOcc = mkVarOcc "noSourceStrictness"
sourceLazyOcc         = mkVarOcc "sourceLazy"
sourceStrictOcc       = mkVarOcc "sourceStrict"

-- data Con = ...
normalCOcc, recCOcc, infixCOcc, forallCOcc, gadtCOcc, recGadtCOcc :: KnownOcc
normalCOcc  = mkVarOcc "normalC"
recCOcc     = mkVarOcc "recC"
infixCOcc   = mkVarOcc "infixC"
forallCOcc  = mkVarOcc "forallC"
gadtCOcc    = mkVarOcc "gadtC"
recGadtCOcc = mkVarOcc "recGadtC"

-- data Bang = ...
bangName :: KnownOcc
bangName = mkVarOcc "bang"

-- type BangType = ...
bangTypeName :: KnownOcc
bangTypeName = mkVarOcc "bangType"

-- type VarBangType = ...
varBangTypeName :: KnownOcc
varBangTypeName = mkVarOcc "varBangType"

-- data PatSynDir = ...
unidirPatSynName, implBidirPatSynName, explBidirPatSynName :: KnownOcc
unidirPatSynName    = mkVarOcc "unidir"
implBidirPatSynName = mkVarOcc "implBidir"
explBidirPatSynName = mkVarOcc "explBidir"

-- data PatSynArgs = ...
prefixPatSynName, infixPatSynName, recordPatSynName :: KnownOcc
prefixPatSynName = mkVarOcc "prefixPatSyn"
infixPatSynName  = mkVarOcc "infixPatSyn"
recordPatSynName = mkVarOcc "recordPatSyn"

-- data Type = ...
forallTName, forallVisTName, varTName, conTName, infixTName, tupleTName,
    unboxedTupleTName, unboxedSumTName, arrowTName, mulArrowTName, listTName,
    appTName, appKindTName, sigTName, equalityTName, litTName, promotedTName,
    promotedTupleTName, promotedNilTName, promotedConsTName,
    wildCardTName, implicitParamTName :: KnownOcc
forallTName         = mkVarOcc "forallT"
forallVisTName      = mkVarOcc "forallVisT"
varTName            = mkVarOcc "varT"
conTName            = mkVarOcc "conT"
tupleTName          = mkVarOcc "tupleT"
unboxedTupleTName   = mkVarOcc "unboxedTupleT"
unboxedSumTName     = mkVarOcc "unboxedSumT"
arrowTName          = mkVarOcc "arrowT"
mulArrowTName       = mkVarOcc "mulArrowT"
listTName           = mkVarOcc "listT"
appTName            = mkVarOcc "appT"
appKindTName        = mkVarOcc "appKindT"
sigTName            = mkVarOcc "sigT"
equalityTName       = mkVarOcc "equalityT"
litTName            = mkVarOcc "litT"
promotedTName       = mkVarOcc "promotedT"
promotedTupleTName  = mkVarOcc "promotedTupleT"
promotedNilTName    = mkVarOcc "promotedNilT"
promotedConsTName   = mkVarOcc "promotedConsT"
wildCardTName       = mkVarOcc "wildCardT"
infixTName          = mkVarOcc "infixT"
implicitParamTName  = mkVarOcc "implicitParamT"

-- data TyLit = ...
numTyLitName, strTyLitName, charTyLitName :: KnownOcc
numTyLitName  = mkVarOcc "numTyLit"
strTyLitName  = mkVarOcc "strTyLit"
charTyLitName = mkVarOcc "charTyLit"

-- data TyVarBndr = ...
plainTVName, kindedTVName :: KnownOcc
plainTVName  = mkVarOcc "plainTV"
kindedTVName = mkVarOcc "kindedTV"

plainInvisTVName, kindedInvisTVName :: KnownOcc
plainInvisTVName  = mkVarOcc "plainInvisTV"
kindedInvisTVName = mkVarOcc "kindedInvisTV"

plainBndrTVName, kindedBndrTVName :: KnownOcc
plainBndrTVName  = mkVarOcc "plainBndrTV"
kindedBndrTVName = mkVarOcc "kindedBndrTV"

-- data Specificity = ...
specifiedSpecName, inferredSpecName :: KnownOcc
specifiedSpecName = mkVarOcc "specifiedSpec"
inferredSpecName  = mkVarOcc "inferredSpec"

-- data BndrVis = ...
bndrReqName, bndrInvisName :: KnownOcc
bndrReqName   = mkVarOcc "bndrReq"
bndrInvisName = mkVarOcc "bndrInvis"

-- data Role = ...
nominalRName, representationalRName, phantomRName, inferRName :: KnownOcc
nominalRName          = mkVarOcc "nominalR"
representationalRName = mkVarOcc "representationalR"
phantomRName          = mkVarOcc "phantomR"
inferRName            = mkVarOcc "inferR"

-- data Kind = ...
starKName, constraintKName :: KnownOcc
starKName       = mkVarOcc "starK"
constraintKName = mkVarOcc "constraintK"

-- data FamilyResultSig = ...
noSigName, kindSigName, tyVarSigName :: KnownOcc
noSigName    = mkVarOcc "noSig"
kindSigName  = mkVarOcc "kindSig"
tyVarSigName = mkVarOcc "tyVarSig"

-- data InjectivityAnn = ...
injectivityAnnName :: KnownOcc
injectivityAnnName = mkVarOcc "injectivityAnn"

-- data Callconv = ...
cCallName, stdCallName, cApiCallName, primCallName, javaScriptCallName :: KnownOcc
cCallName          = mkVarOcc "cCall"
stdCallName        = mkVarOcc "stdCall"
cApiCallName       = mkVarOcc "cApi"
primCallName       = mkVarOcc "prim"
javaScriptCallName = mkVarOcc "javaScript"

-- data Safety = ...
unsafeName, safeName, interruptibleName :: KnownOcc
unsafeName        = mkVarOcc "unsafe"
safeName          = mkVarOcc "safe"
interruptibleName = mkVarOcc "interruptible"

-- data RuleBndr = ...
ruleVarName, typedRuleVarName :: KnownOcc
ruleVarName      = mkVarOcc "ruleVar"
typedRuleVarName = mkVarOcc "typedRuleVar"

-- data FunDep = ...
funDepName :: KnownOcc
funDepName = mkVarOcc "funDep"

-- data TySynEqn = ...
tySynEqnName :: KnownOcc
tySynEqnName = mkVarOcc "tySynEqn"

-- data AnnTarget = ...
valueAnnotationName, typeAnnotationName, moduleAnnotationName :: KnownOcc
valueAnnotationName  = mkVarOcc "valueAnnotation"
typeAnnotationName   = mkVarOcc "typeAnnotation"
moduleAnnotationName = mkVarOcc "moduleAnnotation"

-- type DerivClause = ...
derivClauseName :: KnownOcc
derivClauseName = mkVarOcc "derivClause"

-- data DerivStrategy = ...
stockStrategyName, anyclassStrategyName, newtypeStrategyName,
  viaStrategyName :: KnownOcc
stockStrategyName    = mkVarOcc "stockStrategy"
anyclassStrategyName = mkVarOcc "anyclassStrategy"
newtypeStrategyName  = mkVarOcc "newtypeStrategy"
viaStrategyName      = mkVarOcc "viaStrategy"

patQTyConOcc, expQTyConOcc, stmtTyConOcc,
    conTyConOcc, bangTypeTyConOcc,
    varBangTypeTyConOcc, typeQTyConOcc,
    decsQTyConOcc, ruleBndrTyConOcc, tySynEqnTyConOcc, roleTyConOcc,
    derivClauseTyConOcc, kindTyConOcc,
    tyVarBndrUnitTyConOcc, tyVarBndrSpecTyConOcc, tyVarBndrVisTyConOcc,
    derivStrategyTyConOcc :: KnownOcc
-- These are only used for the types of top-level splices
expQTyConOcc           = mkTcOcc "ExpQ"
decsQTyConOcc          = mkTcOcc "DecsQ"
typeQTyConOcc          = mkTcOcc "TypeQ"
patQTyConOcc           = mkTcOcc "PatQ"

-- These are used in GHC.HsToCore.Quote but always wrapped in a type variable
stmtTyConOcc          = mkTcOcc "Stmt"
conTyConOcc           = mkTcOcc "Con"
bangTypeTyConOcc      = mkTcOcc "BangType"
varBangTypeTyConOcc   = mkTcOcc "VarBangType"
ruleBndrTyConOcc      = mkTcOcc "RuleBndr"
tySynEqnTyConOcc      = mkTcOcc "TySynEqn"
roleTyConOcc          = mkTcOcc "Role"
derivClauseTyConOcc   = mkTcOcc "DerivClause"
kindTyConOcc          = mkTcOcc "Kind"
tyVarBndrUnitTyConOcc = mkTcOcc "TyVarBndrUnit"
tyVarBndrSpecTyConOcc = mkTcOcc "TyVarBndrSpec"
tyVarBndrVisTyConOcc  = mkTcOcc "TyVarBndrVis"
derivStrategyTyConOcc = mkTcOcc "DerivStrategy"

-- quasiquoting
quoteExpName, quotePatName, quoteDecName, quoteTypeName :: KnownOcc
quoteExpName  = mkRecFieldOcc (fsLit "QuasiQuoter") "quoteExp"
quotePatName  = mkRecFieldOcc (fsLit "QuasiQuoter") "quotePat"
quoteDecName  = mkRecFieldOcc (fsLit "QuasiQuoter") "quoteDec"
quoteTypeName = mkRecFieldOcc (fsLit "QuasiQuoter") "quoteType"

-- data Inline = ...
noInlineDataConName, inlineDataConName, inlinableDataConName :: KnownOcc
noInlineDataConName  = mkDataOcc "NoInline"
inlineDataConName    = mkDataOcc "Inline"
inlinableDataConName = mkDataOcc "Inlinable"

-- data RuleMatch = ...
conLikeDataConName, funLikeDataConName :: KnownOcc
conLikeDataConName = mkDataOcc "ConLike"
funLikeDataConName = mkDataOcc "FunLike"

-- data Phases = ...
allPhasesDataConName, fromPhaseDataConName, beforePhaseDataConName :: KnownOcc
allPhasesDataConName   = mkDataOcc "AllPhases"
fromPhaseDataConName   = mkDataOcc "FromPhase"
beforePhaseDataConName = mkDataOcc "BeforePhase"

-- data Overlap = ...
overlappableDataConName,
  overlappingDataConName,
  overlapsDataConName,
  incoherentDataConName :: KnownOcc
overlappableDataConName = mkDataOcc "Overlappable"
overlappingDataConName  = mkDataOcc "Overlapping"
overlapsDataConName     = mkDataOcc "Overlaps"
incoherentDataConName   = mkDataOcc "Incoherent"

-- data NamespaceSpecifier = ...
noNamespaceSpecifierDataConName,
  typeNamespaceSpecifierDataConName,
  dataNamespaceSpecifierDataConName :: KnownOcc
noNamespaceSpecifierDataConName   = mkDataOcc "NoNamespaceSpecifier"
typeNamespaceSpecifierDataConName = mkDataOcc "TypeNamespaceSpecifier"
dataNamespaceSpecifierDataConName = mkDataOcc "DataNamespaceSpecifier"
