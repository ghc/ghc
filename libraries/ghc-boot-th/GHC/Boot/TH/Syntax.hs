{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}
module GHC.Boot.TH.Syntax
  ( module GHC.Internal.TH.Syntax

    -- * Forcing to normal form
  , rnfModName
  , rnfPkgName
  , rnfModule
  , rnfOccName
  , rnfName
  , rnfNameFlavour
  , rnfNameSpace
  , rnfFixity
  , rnfFixityDirection
  , rnfLit
  , rnfBytes
  , rnfPat
  , rnfMatch
  , rnfClause
  , rnfExp
  , rnfBody
  , rnfGuard
  , rnfStmt
  , rnfRange
  , rnfDec
  , rnfNamespaceSpecifier
  , rnfOverlap
  , rnfDerivClause
  , rnfDerivStrategy
  , rnfTypeFamilyHead
  , rnfTySynEqn
  , rnfFunDep
  , rnfForeign
  , rnfCallconv
  , rnfSafety
  , rnfPragma
  , rnfInline
  , rnfRuleMatch
  , rnfPhases
  , rnfRuleBndr
  , rnfAnnTarget
  , rnfSourceUnpackedness
  , rnfSourceStrictness
  , rnfCon
  , rnfBang
  , rnfPatSynDir
  , rnfPatSynArgs
  , rnfType
  , rnfSpecificity
  , rnfTyVarBndr
  , rnfBndrVis
  , rnfFamilyResultSig
  , rnfInjectivityAnn
  , rnfTyLit
  , rnfRole
  , rnfAnnLookup
  , rnfDocLoc
  , rnfForeignSrcLang
  , rnfExtension
  ) where

import GHC.Internal.TH.Syntax

import Control.DeepSeq (rnf, rwhnf, liftRnf, liftRnf2)
import Prelude (seq)

-- These functions force TH syntax to normal form, for
-- 'GHC.Boot.TH.Monad.forceMetaHandlersArgs'. They are plain functions rather
-- than 'NFData' instances so that this package does not define orphan
-- instances.

rnfModName :: ModName -> ()
rnfModName (ModName s) = rnf s

rnfPkgName :: PkgName -> ()
rnfPkgName (PkgName s) = rnf s

rnfModule :: Module -> ()
rnfModule (Module pkg m) = rnfPkgName pkg `seq` rnfModName m

rnfOccName :: OccName -> ()
rnfOccName (OccName s) = rnf s

rnfName :: Name -> ()
rnfName (Name occ flav) = rnfOccName occ `seq` rnfNameFlavour flav

rnfNameFlavour :: NameFlavour -> ()
rnfNameFlavour NameS            = ()
rnfNameFlavour (NameQ m)        = rnfModName m
rnfNameFlavour (NameU u)        = rnf u
rnfNameFlavour (NameL u)        = rnf u
rnfNameFlavour (NameG ns pkg m) = rnfNameSpace ns `seq` rnfPkgName pkg `seq` rnfModName m

rnfNameSpace :: NameSpace -> ()
rnfNameSpace VarName       = ()
rnfNameSpace DataName      = ()
rnfNameSpace TcClsName     = ()
rnfNameSpace (FldName par) = rnf par

rnfFixity :: Fixity -> ()
rnfFixity (Fixity prec dir) = rnf prec `seq` rnfFixityDirection dir

rnfFixityDirection :: FixityDirection -> ()
rnfFixityDirection InfixL = ()
rnfFixityDirection InfixR = ()
rnfFixityDirection InfixN = ()

rnfLit :: Lit -> ()
rnfLit (CharL c)        = rnf c
rnfLit (StringL s)      = rnf s
rnfLit (IntegerL i)     = rnf i
rnfLit (RationalL r)    = rnf r
rnfLit (IntPrimL i)     = rnf i
rnfLit (WordPrimL i)    = rnf i
rnfLit (FloatPrimL r)   = rnf r
rnfLit (DoublePrimL r)  = rnf r
rnfLit (StringPrimL ws) = rnf ws
rnfLit (BytesPrimL b)   = rnfBytes b
rnfLit (CharPrimL c)    = rnf c

rnfBytes :: Bytes -> ()
rnfBytes (Bytes ptr off size) = ptr `seq` rnf off `seq` rnf size

rnfPat :: Pat -> ()
rnfPat (LitP l)                  = rnfLit l
rnfPat (VarP n)                  = rnfName n
rnfPat (TupP ps)                 = liftRnf rnfPat ps
rnfPat (UnboxedTupP ps)          = liftRnf rnfPat ps
rnfPat (UnboxedSumP p alt arity) = rnfPat p `seq` rnf alt `seq` rnf arity
rnfPat (ConP n tys ps)           = rnfName n `seq` liftRnf rnfType tys `seq` liftRnf rnfPat ps
rnfPat (InfixP p1 n p2)          = rnfPat p1 `seq` rnfName n `seq` rnfPat p2
rnfPat (UInfixP p1 n p2)         = rnfPat p1 `seq` rnfName n `seq` rnfPat p2
rnfPat (ParensP p)               = rnfPat p
rnfPat (TildeP p)                = rnfPat p
rnfPat (BangP p)                 = rnfPat p
rnfPat (AsP n p)                 = rnfName n `seq` rnfPat p
rnfPat WildP                     = ()
rnfPat (RecP n fps)              = rnfName n `seq` liftRnf (liftRnf2 rnfName rnfPat) fps
rnfPat (ListP ps)                = liftRnf rnfPat ps
rnfPat (SigP p t)                = rnfPat p `seq` rnfType t
rnfPat (ViewP e p)               = rnfExp e `seq` rnfPat p
rnfPat (TypeP t)                 = rnfType t
rnfPat (InvisP t)                = rnfType t
rnfPat (OrP ps)                  = liftRnf rnfPat ps

rnfMatch :: Match -> ()
rnfMatch (Match p body ds) = rnfPat p `seq` rnfBody body `seq` liftRnf rnfDec ds

rnfClause :: Clause -> ()
rnfClause (Clause ps body ds) = liftRnf rnfPat ps `seq` rnfBody body `seq` liftRnf rnfDec ds

rnfExp :: Exp -> ()
rnfExp (VarE n)                  = rnfName n
rnfExp (ConE n)                  = rnfName n
rnfExp (LitE l)                  = rnfLit l
rnfExp (AppE e1 e2)              = rnfExp e1 `seq` rnfExp e2
rnfExp (AppTypeE e t)            = rnfExp e `seq` rnfType t
rnfExp (InfixE me1 e me2)        = liftRnf rnfExp me1 `seq` rnfExp e `seq` liftRnf rnfExp me2
rnfExp (UInfixE e1 e2 e3)        = rnfExp e1 `seq` rnfExp e2 `seq` rnfExp e3
rnfExp (ParensE e)               = rnfExp e
rnfExp (LamE ps e)               = liftRnf rnfPat ps `seq` rnfExp e
rnfExp (LamCaseE ms)             = liftRnf rnfMatch ms
rnfExp (LamCasesE cs)            = liftRnf rnfClause cs
rnfExp (TupE mes)                = liftRnf (liftRnf rnfExp) mes
rnfExp (UnboxedTupE mes)         = liftRnf (liftRnf rnfExp) mes
rnfExp (UnboxedSumE e alt arity) = rnfExp e `seq` rnf alt `seq` rnf arity
rnfExp (CondE e1 e2 e3)          = rnfExp e1 `seq` rnfExp e2 `seq` rnfExp e3
rnfExp (MultiIfE ges)            = liftRnf (liftRnf2 rnfGuard rnfExp) ges
rnfExp (LetE ds e)               = liftRnf rnfDec ds `seq` rnfExp e
rnfExp (CaseE e ms)              = rnfExp e `seq` liftRnf rnfMatch ms
rnfExp (DoE mm ss)               = liftRnf rnfModName mm `seq` liftRnf rnfStmt ss
rnfExp (MDoE mm ss)              = liftRnf rnfModName mm `seq` liftRnf rnfStmt ss
rnfExp (CompE ss)                = liftRnf rnfStmt ss
rnfExp (ArithSeqE r)             = rnfRange r
rnfExp (ListE es)                = liftRnf rnfExp es
rnfExp (SigE e t)                = rnfExp e `seq` rnfType t
rnfExp (RecConE n fes)           = rnfName n `seq` liftRnf (liftRnf2 rnfName rnfExp) fes
rnfExp (RecUpdE e fes)           = rnfExp e `seq` liftRnf (liftRnf2 rnfName rnfExp) fes
rnfExp (StaticE e)               = rnfExp e
rnfExp (UnboundVarE n)           = rnfName n
rnfExp (LabelE s)                = rnf s
rnfExp (ImplicitParamVarE s)     = rnf s
rnfExp (GetFieldE e f)           = rnfExp e `seq` rnf f
rnfExp (ProjectionE fs)          = rnf fs
rnfExp (TypedBracketE e)         = rnfExp e
rnfExp (TypedSpliceE e)          = rnfExp e
rnfExp (TypeE t)                 = rnfType t
rnfExp (ForallE tvs e)           = liftRnf (rnfTyVarBndr rnfSpecificity) tvs `seq` rnfExp e
rnfExp (ForallVisE tvs e)        = liftRnf (rnfTyVarBndr rnf) tvs `seq` rnfExp e
rnfExp (ConstrainedE es e)       = liftRnf rnfExp es `seq` rnfExp e

rnfBody :: Body -> ()
rnfBody (GuardedB ges) = liftRnf (liftRnf2 rnfGuard rnfExp) ges
rnfBody (NormalB e)    = rnfExp e

rnfGuard :: Guard -> ()
rnfGuard (NormalG e) = rnfExp e
rnfGuard (PatG ss)   = liftRnf rnfStmt ss

rnfStmt :: Stmt -> ()
rnfStmt (BindS p e) = rnfPat p `seq` rnfExp e
rnfStmt (LetS ds)   = liftRnf rnfDec ds
rnfStmt (NoBindS e) = rnfExp e
rnfStmt (ParS sss)  = liftRnf (liftRnf rnfStmt) sss
rnfStmt (RecS ss)   = liftRnf rnfStmt ss

rnfRange :: Range -> ()
rnfRange (FromR e1)             = rnfExp e1
rnfRange (FromThenR e1 e2)      = rnfExp e1 `seq` rnfExp e2
rnfRange (FromToR e1 e2)        = rnfExp e1 `seq` rnfExp e2
rnfRange (FromThenToR e1 e2 e3) = rnfExp e1 `seq` rnfExp e2 `seq` rnfExp e3

rnfDec :: Dec -> ()
rnfDec (FunD n cs)
  = rnfName n `seq` liftRnf rnfClause cs
rnfDec (ValD p body ds)
  = rnfPat p `seq` rnfBody body `seq` liftRnf rnfDec ds
rnfDec (DataD cxt n tvs mk cons derivs)
  = liftRnf rnfType cxt `seq` rnfName n `seq` liftRnf (rnfTyVarBndr rnfBndrVis) tvs
    `seq` liftRnf rnfType mk `seq` liftRnf rnfCon cons `seq` liftRnf rnfDerivClause derivs
rnfDec (NewtypeD cxt n tvs mk con derivs)
  = liftRnf rnfType cxt `seq` rnfName n `seq` liftRnf (rnfTyVarBndr rnfBndrVis) tvs
    `seq` liftRnf rnfType mk `seq` rnfCon con `seq` liftRnf rnfDerivClause derivs
rnfDec (TypeDataD n tvs mk cons)
  = rnfName n `seq` liftRnf (rnfTyVarBndr rnfBndrVis) tvs `seq` liftRnf rnfType mk
    `seq` liftRnf rnfCon cons
rnfDec (TySynD n tvs t)
  = rnfName n `seq` liftRnf (rnfTyVarBndr rnfBndrVis) tvs `seq` rnfType t
rnfDec (ClassD cxt n tvs fds ds)
  = liftRnf rnfType cxt `seq` rnfName n `seq` liftRnf (rnfTyVarBndr rnfBndrVis) tvs
    `seq` liftRnf rnfFunDep fds `seq` liftRnf rnfDec ds
rnfDec (InstanceD mo cxt t ds)
  = liftRnf rnfOverlap mo `seq` liftRnf rnfType cxt `seq` rnfType t `seq` liftRnf rnfDec ds
rnfDec (SigD n t)
  = rnfName n `seq` rnfType t
rnfDec (KiSigD n k)
  = rnfName n `seq` rnfType k
rnfDec (ForeignD f)
  = rnfForeign f
rnfDec (InfixD fix ns n)
  = rnfFixity fix `seq` rnfNamespaceSpecifier ns `seq` rnfName n
rnfDec (DefaultD tys)
  = liftRnf rnfType tys
rnfDec (PragmaD prag)
  = rnfPragma prag
rnfDec (DataFamilyD n tvs mk)
  = rnfName n `seq` liftRnf (rnfTyVarBndr rnfBndrVis) tvs `seq` liftRnf rnfType mk
rnfDec (DataInstD cxt mtvs t mk cons derivs)
  = liftRnf rnfType cxt `seq` liftRnf (liftRnf (rnfTyVarBndr rnf)) mtvs `seq` rnfType t
    `seq` liftRnf rnfType mk `seq` liftRnf rnfCon cons `seq` liftRnf rnfDerivClause derivs
rnfDec (NewtypeInstD cxt mtvs t mk con derivs)
  = liftRnf rnfType cxt `seq` liftRnf (liftRnf (rnfTyVarBndr rnf)) mtvs `seq` rnfType t
    `seq` liftRnf rnfType mk `seq` rnfCon con `seq` liftRnf rnfDerivClause derivs
rnfDec (TySynInstD eqn)
  = rnfTySynEqn eqn
rnfDec (OpenTypeFamilyD hd)
  = rnfTypeFamilyHead hd
rnfDec (ClosedTypeFamilyD hd eqns)
  = rnfTypeFamilyHead hd `seq` liftRnf rnfTySynEqn eqns
rnfDec (RoleAnnotD n roles)
  = rnfName n `seq` liftRnf rnfRole roles
rnfDec (StandaloneDerivD mds cxt t)
  = liftRnf rnfDerivStrategy mds `seq` liftRnf rnfType cxt `seq` rnfType t
rnfDec (DefaultSigD n t)
  = rnfName n `seq` rnfType t
rnfDec (PatSynD n args dir p)
  = rnfName n `seq` rnfPatSynArgs args `seq` rnfPatSynDir dir `seq` rnfPat p
rnfDec (PatSynSigD n t)
  = rnfName n `seq` rnfType t
rnfDec (ImplicitParamBindD s e)
  = rnf s `seq` rnfExp e

rnfNamespaceSpecifier :: NamespaceSpecifier -> ()
rnfNamespaceSpecifier NoNamespaceSpecifier   = ()
rnfNamespaceSpecifier TypeNamespaceSpecifier = ()
rnfNamespaceSpecifier DataNamespaceSpecifier = ()

rnfOverlap :: Overlap -> ()
rnfOverlap Overlappable = ()
rnfOverlap Overlapping  = ()
rnfOverlap Overlaps     = ()
rnfOverlap Incoherent   = ()

rnfDerivClause :: DerivClause -> ()
rnfDerivClause (DerivClause mds cxt) = liftRnf rnfDerivStrategy mds `seq` liftRnf rnfType cxt

rnfDerivStrategy :: DerivStrategy -> ()
rnfDerivStrategy StockStrategy    = ()
rnfDerivStrategy AnyclassStrategy = ()
rnfDerivStrategy NewtypeStrategy  = ()
rnfDerivStrategy (ViaStrategy t)  = rnfType t

rnfTypeFamilyHead :: TypeFamilyHead -> ()
rnfTypeFamilyHead (TypeFamilyHead n tvs res minj)
  = rnfName n `seq` liftRnf (rnfTyVarBndr rnfBndrVis) tvs `seq` rnfFamilyResultSig res
    `seq` liftRnf rnfInjectivityAnn minj

rnfTySynEqn :: TySynEqn -> ()
rnfTySynEqn (TySynEqn mtvs lhs rhs)
  = liftRnf (liftRnf (rnfTyVarBndr rnf)) mtvs `seq` rnfType lhs `seq` rnfType rhs

rnfFunDep :: FunDep -> ()
rnfFunDep (FunDep ns1 ns2) = liftRnf rnfName ns1 `seq` liftRnf rnfName ns2

rnfForeign :: Foreign -> ()
rnfForeign (ImportF cc safety ent n t)
  = rnfCallconv cc `seq` rnfSafety safety `seq` rnf ent `seq` rnfName n `seq` rnfType t
rnfForeign (ExportF cc ent n t)
  = rnfCallconv cc `seq` rnf ent `seq` rnfName n `seq` rnfType t

rnfCallconv :: Callconv -> ()
rnfCallconv CCall      = ()
rnfCallconv StdCall    = ()
rnfCallconv CApi       = ()
rnfCallconv Prim       = ()
rnfCallconv JavaScript = ()

rnfSafety :: Safety -> ()
rnfSafety Unsafe        = ()
rnfSafety Safe          = ()
rnfSafety Interruptible = ()

rnfPragma :: Pragma -> ()
rnfPragma (InlineP n inl rm phases)
  = rnfName n `seq` rnfInline inl `seq` rnfRuleMatch rm `seq` rnfPhases phases
rnfPragma (OpaqueP n)
  = rnfName n
rnfPragma (SpecialiseEP mtvs bndrs e minl phases)
  = liftRnf (liftRnf (rnfTyVarBndr rnf)) mtvs `seq` liftRnf rnfRuleBndr bndrs `seq` rnfExp e
    `seq` liftRnf rnfInline minl `seq` rnfPhases phases
rnfPragma (SpecialiseInstP t)
  = rnfType t
rnfPragma (RuleP name mtvs bndrs lhs rhs phases)
  = rnf name `seq` liftRnf (liftRnf (rnfTyVarBndr rnf)) mtvs `seq` liftRnf rnfRuleBndr bndrs
    `seq` rnfExp lhs `seq` rnfExp rhs `seq` rnfPhases phases
rnfPragma (AnnP target e)
  = rnfAnnTarget target `seq` rnfExp e
rnfPragma (LineP line file)
  = rnf line `seq` rnf file
rnfPragma (CompleteP ns mty)
  = liftRnf rnfName ns `seq` liftRnf rnfName mty
rnfPragma (SCCP n mlabel)
  = rnfName n `seq` rnf mlabel

rnfInline :: Inline -> ()
rnfInline NoInline  = ()
rnfInline Inline    = ()
rnfInline Inlinable = ()

rnfRuleMatch :: RuleMatch -> ()
rnfRuleMatch ConLike = ()
rnfRuleMatch FunLike = ()

rnfPhases :: Phases -> ()
rnfPhases AllPhases       = ()
rnfPhases (FromPhase i)   = rnf i
rnfPhases (BeforePhase i) = rnf i

rnfRuleBndr :: RuleBndr -> ()
rnfRuleBndr (RuleVar n)        = rnfName n
rnfRuleBndr (TypedRuleVar n t) = rnfName n `seq` rnfType t

rnfAnnTarget :: AnnTarget -> ()
rnfAnnTarget ModuleAnnotation    = ()
rnfAnnTarget (TypeAnnotation n)  = rnfName n
rnfAnnTarget (ValueAnnotation n) = rnfName n

rnfSourceUnpackedness :: SourceUnpackedness -> ()
rnfSourceUnpackedness NoSourceUnpackedness = ()
rnfSourceUnpackedness SourceNoUnpack       = ()
rnfSourceUnpackedness SourceUnpack         = ()

rnfSourceStrictness :: SourceStrictness -> ()
rnfSourceStrictness NoSourceStrictness = ()
rnfSourceStrictness SourceLazy         = ()
rnfSourceStrictness SourceStrict       = ()

rnfCon :: Con -> ()
rnfCon (NormalC n bts)
  = rnfName n `seq` liftRnf rnfBangType bts
rnfCon (RecC n vbts)
  = rnfName n `seq` liftRnf rnfVarBangType vbts
rnfCon (InfixC bt1 n bt2)
  = rnfBangType bt1 `seq` rnfName n `seq` rnfBangType bt2
rnfCon (ForallC tvs cxt con)
  = liftRnf (rnfTyVarBndr rnfSpecificity) tvs `seq` liftRnf rnfType cxt `seq` rnfCon con
rnfCon (GadtC ns bts res_ty)
  = liftRnf rnfName ns `seq` liftRnf rnfBangType bts `seq` rnfType res_ty
rnfCon (RecGadtC ns vbts res_ty)
  = liftRnf rnfName ns `seq` liftRnf rnfVarBangType vbts `seq` rnfType res_ty

rnfBangType :: BangType -> ()
rnfBangType = liftRnf2 rnfBang rnfType

rnfVarBangType :: VarBangType -> ()
rnfVarBangType (n, b, t) = rnfName n `seq` rnfBang b `seq` rnfType t

rnfBang :: Bang -> ()
rnfBang (Bang su ss) = rnfSourceUnpackedness su `seq` rnfSourceStrictness ss

rnfPatSynDir :: PatSynDir -> ()
rnfPatSynDir Unidir         = ()
rnfPatSynDir ImplBidir      = ()
rnfPatSynDir (ExplBidir cs) = liftRnf rnfClause cs

rnfPatSynArgs :: PatSynArgs -> ()
rnfPatSynArgs (PrefixPatSyn ns)   = liftRnf rnfName ns
rnfPatSynArgs (InfixPatSyn n1 n2) = rnfName n1 `seq` rnfName n2
rnfPatSynArgs (RecordPatSyn ns)   = liftRnf rnfName ns

rnfType :: Type -> ()
rnfType (ForallT tvs cxt t)        = liftRnf (rnfTyVarBndr rnfSpecificity) tvs
                                     `seq` liftRnf rnfType cxt `seq` rnfType t
rnfType (ForallVisT tvs t)         = liftRnf (rnfTyVarBndr rnf) tvs `seq` rnfType t
rnfType (AppT t1 t2)               = rnfType t1 `seq` rnfType t2
rnfType (AppKindT t k)             = rnfType t `seq` rnfType k
rnfType (SigT t k)                 = rnfType t `seq` rnfType k
rnfType (VarT n)                   = rnfName n
rnfType (ConT n)                   = rnfName n
rnfType (PromotedT n)              = rnfName n
rnfType (InfixT t1 n t2)           = rnfType t1 `seq` rnfName n `seq` rnfType t2
rnfType (UInfixT t1 n t2)          = rnfType t1 `seq` rnfName n `seq` rnfType t2
rnfType (PromotedInfixT t1 n t2)   = rnfType t1 `seq` rnfName n `seq` rnfType t2
rnfType (PromotedUInfixT t1 n t2)  = rnfType t1 `seq` rnfName n `seq` rnfType t2
rnfType (ParensT t)                = rnfType t
rnfType (TupleT arity)             = rnf arity
rnfType (UnboxedTupleT arity)      = rnf arity
rnfType (UnboxedSumT arity)        = rnf arity
rnfType ArrowT                     = ()
rnfType MulArrowT                  = ()
rnfType EqualityT                  = ()
rnfType ListT                      = ()
rnfType (PromotedTupleT arity)     = rnf arity
rnfType PromotedNilT               = ()
rnfType PromotedConsT              = ()
rnfType StarT                      = ()
rnfType ConstraintT                = ()
rnfType (LitT l)                   = rnfTyLit l
rnfType WildCardT                  = ()
rnfType (ImplicitParamT s t)       = rnf s `seq` rnfType t

rnfSpecificity :: Specificity -> ()
rnfSpecificity SpecifiedSpec = ()
rnfSpecificity InferredSpec  = ()

rnfTyVarBndr :: (flag -> ()) -> TyVarBndr flag -> ()
rnfTyVarBndr rnf_flag (PlainTV n flag)    = rnfName n `seq` rnf_flag flag
rnfTyVarBndr rnf_flag (KindedTV n flag k) = rnfName n `seq` rnf_flag flag `seq` rnfType k

rnfBndrVis :: BndrVis -> ()
rnfBndrVis BndrReq   = ()
rnfBndrVis BndrInvis = ()

rnfFamilyResultSig :: FamilyResultSig -> ()
rnfFamilyResultSig NoSig         = ()
rnfFamilyResultSig (KindSig k)   = rnfType k
rnfFamilyResultSig (TyVarSig tv) = rnfTyVarBndr rnf tv

rnfInjectivityAnn :: InjectivityAnn -> ()
rnfInjectivityAnn (InjectivityAnn n ns) = rnfName n `seq` liftRnf rnfName ns

rnfTyLit :: TyLit -> ()
rnfTyLit (NumTyLit i)  = rnf i
rnfTyLit (StrTyLit s)  = rnf s
rnfTyLit (CharTyLit c) = rnf c

rnfRole :: Role -> ()
rnfRole NominalR          = ()
rnfRole RepresentationalR = ()
rnfRole PhantomR          = ()
rnfRole InferR            = ()

rnfAnnLookup :: AnnLookup -> ()
rnfAnnLookup (AnnLookupModule m) = rnfModule m
rnfAnnLookup (AnnLookupName n)   = rnfName n

rnfDocLoc :: DocLoc -> ()
rnfDocLoc ModuleDoc    = ()
rnfDocLoc (DeclDoc n)  = rnfName n
rnfDocLoc (ArgDoc n i) = rnfName n `seq` rnf i
rnfDocLoc (InstDoc t)  = rnfType t

rnfForeignSrcLang :: ForeignSrcLang -> ()
rnfForeignSrcLang LangC      = ()
rnfForeignSrcLang LangCxx    = ()
rnfForeignSrcLang LangObjc   = ()
rnfForeignSrcLang LangObjcxx = ()
rnfForeignSrcLang LangAsm    = ()
rnfForeignSrcLang LangJs     = ()
rnfForeignSrcLang RawObject  = ()

-- 'Extension' derives 'Enum', so all of its constructors are nullary.
rnfExtension :: Extension -> ()
rnfExtension = rwhnf
