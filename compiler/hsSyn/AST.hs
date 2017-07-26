{-# LANGUAGE TypeFamilies #-}
module AST where

import SrcLoc
         (Located)
import FastString
         (FastString)
import Data.ByteString
         (ByteString)
import BasicTypes
         (IntegralLit, FractionalLit, Boxity, ConTag, Arity, SourceText,
          StringLiteral, Fixity, LexicalFixity, Origin, InlinePragma,
          SpliceExplicitFlag, DerivStrategy, OverlapMode, RuleName,
          Activation, WarningTxt)
import HsExtension
         (LIdP, IdP)
import DataCon
         (HsSrcBang, SrcStrictness, FieldLbl)
import RdrName      -- hopefully we can get rid of this with proper design
         (RdrName)
import SrcLoc
         (SrcSpan)
import PlaceHolder  -- hopefully we can get rid of this with proper design
         (NameOrRdrName)
import Bag
import BooleanFormula
         (LBooleanFormula)
import Name
         (Name) -- TODO: we should get rid of this by carefull redesign
import Class
         (FunDep)
import ForeignCall
         (CType, CCallConv, Safety, Header, CLabelString, CCallTarget,
          CExportSpec)
import CoAxiom
         (Role)
import Module
         ( ModuleName )


----------------------------------------------------------------------------
-- * Literal
----------------------------------------------------------------------------

data Lit x
  = Char
      (XChar x)
      Char

  | CharPrim
      (XCharPrim x)
      Char

  | String
      (XString x)
      FastString

  | StringPrim
      (XStringPrim x)
      ByteString

  | IntPrim
      (XIntPrim x)
      Integer

  | WordPrim
      (XWordPrim x)
      Integer

  | Int64Prim
      (XInt64Prim x)
      Integer

  | Word64Prim
      (XWord64Prim x)
      Integer

  | FloatPrim
      (XFloatPrim x)
      FractionalLit

  | DoublePrim
      (XDoublePrim x)
      FractionalLit

  | NewLit (XNewLit x)

type family XChar       x
type family XCharPrim   x
type family XString     x
type family XStringPrim x
type family XIntPrim    x
type family XWordPrim   x
type family XInt64Prim  x
type family XWord64Prim x
type family XFloatPrim  x
type family XDoublePrim x
type family XNewLit     x

type ForallXLit c x  =
       ( c (XChar       x)
       , c (XCharPrim   x)
       , c (XString     x)
       , c (XStringPrim x)
       , c (XIntPrim    x)
       , c (XWordPrim   x)
       , c (XInt64Prim  x)
       , c (XWord64Prim x)
       , c (XFloatPrim  x)
       , c (XDoublePrim x)
       , c (XNewLit     x)
       )

----------------------------------------------------------------------------

data OverLit x
  = OverLit
      (XOverLit x)
      OverLitVal

  | NewOverLit
      (XNewOverLit x)

type family XOverLit    x
type family XNewOverLit x

type ForallXOverLit c x =
       ( c (XOverLit    x)
       , c (XNewOverLit x)
       )

type LOverLit x = Located (OverLit x)

----------------------------------------------------------------------------

data OverLitVal
  = Integral
      !IntegralLit
    -- ^ Integer-looking literals;
  | Fractional
      !FractionalLit
    -- ^ Frac-looking literals
  | IsString
      !SourceText
      !FastString
    -- ^ String-looking literals

-- TODO:  make above extensible and remove sourcetext

----------------------------------------------------------------------------
-- * Type
----------------------------------------------------------------------------

data Type x
  = ForAllTy
      (XForAllTy x)
      [LTyVarBndr x]
      (LType x)

  | QualTy
      (XQualTy x)
      (LContext x)
      (LType x)

  | TyVar
      (XTyVar x)
      Promoted
      (LIdP x)

  | AppsTy
      (XAppsTy x)
      [LAppType x]

  | AppTy
      (XAppTy x)
      (LType x)
      (LType x)

  | FunTy
      (XFunTy x)
      (LType x)
      (LType x)

  | ListTy
      (XListTy x)
      (LType x)

  | PArrTy
      (XPArrTy x)
      (LType x)

  | TupleTy
      (XTupleTy x)
      TupleSort
      [LType x]

  | SumTy
      (XSumTy x)
      [LType x]

  | OpTy
      (XOpTy x)
      (LType x)
      (LIdP x)
      (LType x)

  | ParTy
      (XParTy x)
      (LType x)

  | IParamTy
      (XIParamTy x)
      LIPName
      (LType x)

  | EqTy
      (XEqTy x)
      (LType x)
      (LType x)

  | KindSig
      (XKindSig x)
      (LType x)
      (LKind x)

  | SpliceTy
      (XSpliceTy x)
      (Splice x)

  | DocTy
      (XDocTy x)
      (LType x)
      LDocString

  | BangTy
      (XBangTy x)
      HsSrcBang
      (LType x)

  | RecTy
      (XRecTy x)
      [LConDeclField x]

  | ExplicitListTy
      (XExplicitListTy x)
      Promoted
      [LType x]

  | ExplicitTupleTy
      (XExplicitTupleTy x)
      [LType x]

  | TyLit
      (XTyLit x)
      TyLit

  | WildCardTy
      (XWildCardTy x)

  | NewType
      (XNewType x)

type family XForAllTy        x
type family XQualTy          x
type family XTyVar           x
type family XAppsTy          x
type family XAppTy           x
type family XFunTy           x
type family XListTy          x
type family XPArrTy          x
type family XTupleTy         x
type family XSumTy           x
type family XOpTy            x
type family XParTy           x
type family XIParamTy        x
type family XEqTy            x
type family XKindSig         x
type family XSpliceTy        x
type family XDocTy           x
type family XBangTy          x
type family XRecTy           x
type family XExplicitListTy  x
type family XExplicitTupleTy x
type family XTyLit           x
type family XWildCardTy      x
type family XNewType         x

type ForallXType c x  =
       ( c (XForAllTy        x)
       , c (XQualTy          x)
       , c (XTyVar           x)
       , c (XAppsTy          x)
       , c (XAppTy           x)
       , c (XFunTy           x)
       , c (XListTy          x)
       , c (XPArrTy          x)
       , c (XTupleTy         x)
       , c (XSumTy           x)
       , c (XOpTy            x)
       , c (XParTy           x)
       , c (XIParamTy        x)
       , c (XEqTy            x)
       , c (XKindSig         x)
       , c (XSpliceTy        x)
       , c (XDocTy           x)
       , c (XBangTy          x)
       , c (XRecTy           x)
       , c (XExplicitListTy  x)
       , c (XExplicitTupleTy x)
       , c (XTyLit           x)
       , c (XWildCardTy      x)
       , c (XNewType         x)
       )

type LType x = Located (Type x)

----------------------------------------------------------------------------

data TyVarBndr x
  = UserTyVar'
      (XUserTyVar x)
      (LIdP x)

  | KindedTyVar'
      (XKindedTyVar x)
      (LIdP x)
      (LKind x)

  | NewTyVarBndr
      (XNewTyVarBndr x)

type family XUserTyVar      x
type family XKindedTyVar    x
type family XNewTyVarBndr   x

type ForallXTyVarBndr c x  =
       ( c (XUserTyVar      x)
       , c (XKindedTyVar    x)
       , c (XNewTyVarBndr   x)
       )

type LTyVarBndr x = Located (TyVarBndr x)

----------------------------------------------------------------------------

type BangType x  = Type x

type LBangType x = Located (BangType x)

----------------------------------------------------------------------------

type Context  x = [LType x]

type LContext x = Located (Context x)

----------------------------------------------------------------------------

type Kind  x = Type x

type LKind x = Located (Kind x)

----------------------------------------------------------------------------

data TupleSort
  = UnboxedTuple
  | BoxedTuple
  | ConstraintTuple
  | BoxedOrConstraintTuple

----------------------------------------------------------------------------

data Promoted
  = Promoted
  | NotPromoted

----------------------------------------------------------------------------

data AppType x
  = AppInfix
      (XAppInfix x)
      (LIdP x)

  | AppPrefix
      (XAppPrefix x)
      (LType x)

  | NewAppType
      (XNewAppType x)

type family XAppInfix   x
type family XAppPrefix  x
type family XNewAppType x

type ForallXAppType c x =
       ( c (XAppInfix   x)
       , c (XAppPrefix  x)
       , c (XNewAppType x)
       )

type LAppType x = Located (AppType x)

----------------------------------------------------------------------------

data FieldOcc x
  = FieldOcc
      (XFieldOcc x)
      (Located RdrName)

  | NewFieldOcc
      (XNewFieldOcc x)

type family XFieldOcc    x
type family XNewFieldOcc x

type ForallXFieldOcc c x =
       ( c (XFieldOcc    x)
       , c (XNewFieldOcc x)
       )

type LFieldOcc x = Located (FieldOcc x)

----------------------------------------------------------------------------

newtype IPName = IPName FastString

type LIPName = Located IPName

----------------------------------------------------------------------------

data TyLit
  = NumTy SourceText Integer
  | StrTy SourceText FastString

-- TODO:  make above extensible and remove sourcetext

----------------------------------------------------------------------------

data ConDeclField x
  = ConDeclField
      (XConDeclField x)
      [LFieldOcc x]
      (LBangType x)
      (Maybe LDocString)

  | NewConDeclField
      (XNewConDeclField x)

type family XConDeclField    x
type family XNewConDeclField x

type ForallXConDeclField c x =
       ( c (XConDeclField    x)
       , c (XNewConDeclField x)
       )

type LConDeclField x = Located (ConDeclField x)

----------------------------------------------------------------------------

type LSigType   x = ImplicitBndrs x (LType x)

type LWcType    x = WildCardBndrs x (LType x)

type LSigWcType x = WildCardBndrs x (LSigType x)

----------------------------------------------------------------------------

data ConDetails arg rec
  = PrefixCon [arg]
  | RecCon    rec
  | InfixCon  arg arg

----------------------------------------------------------------------------

data ImplicitBndrs x thing
  = IB
      (XIB x thing)
      thing

  | NewImplicitBndrs
      (XNewImplicitBndrs x thing)

type family XIB               x thing
type family XNewImplicitBndrs x thing

type ForallXImplicitBndrs c x thing =
       ( c (XIB               x thing)
       , c (XNewImplicitBndrs x thing)
       )

----------------------------------------------------------------------------


data WildCardBndrs x thing
  = WC
      (XWC x thing)
      thing

  | NewWildCardBndrs
      (XNewWildCardBndrs x thing)

type family XWC               x thing
type family XNewWildCardBndrs x thing

type ForallXWildCardBndrs c x thing =
       ( c (XWC               x thing)
       , c (XNewWildCardBndrs x thing)
       )

----------------------------------------------------------------------------

data LQTyVars x
  = QTvs
      (XHsQTvs x)
      [LTyVarBndr x]

  | NewLHsQTyVars
      (XNewLHsQTyVars x)

type family XHsQTvs        x
type family XNewLHsQTyVars x

type ForallXLHsQTyVars c x =
       ( c (XHsQTvs        x)
       , c (XNewLHsQTyVars x)
       )

----------------------------------------------------------------------------
-- * Doc String
----------------------------------------------------------------------------

newtype DocString = DocString FastString

type LDocString = Located DocString

----------------------------------------------------------------------------
-- * Pattern
----------------------------------------------------------------------------

data Pat x
  = WildPat
      (XWildPat x)

  | VarPat
      (XVarPat x)
      (LIdP x)

  | LazyPat
      (XLazyPat x)
      (LPat x)

  | AsPat
      (XAsPat x)
      (LIdP x)
      (LPat x)

  | ParPat
      (XParPat x)
      (LPat x)

  | BangPat
      (XBangPat x)
      (LPat x)

  | ListPat
      (XListPat x)
      [LPat x]

  | TuplePat
      (XTuplePat x)
      [LPat x]
      Boxity

  | SumPat
      (XSumPat x)
      (LPat x)
      ConTag
      Arity

  | PArrPat
      (XPArrPat x)
      [LPat x]

  | ConPat
      (XConPat x)
      (LIdP x)
      (ConPatDetails x)

  | ViewPat
      (XViewPat x)
      (LExpr x)
      (LPat x)

  | SplicePat
      (XSplicePat x)
      (Splice x)

  | LitPat
      (XLitPat x)
      (Lit x)

  | NPat
      (XNPat x)
      (LOverLit x)

  | NPlusKPat
      (XNPlusKPat x)
      (LIdP x)
      (LOverLit x)
      (OverLit x)

  | SigPat
      (XSigPat x)
      (LPat x)
      (LSigWcType x)

  | NewPat
      (XNewPat x)

type family XWildPat   x
type family XVarPat    x
type family XLazyPat   x
type family XAsPat     x
type family XParPat    x
type family XBangPat   x
type family XListPat   x
type family XTuplePat  x
type family XSumPat    x
type family XPArrPat   x
type family XConPat    x
type family XViewPat   x
type family XSplicePat x
type family XLitPat    x
type family XNPat      x
type family XNPlusKPat x
type family XSigPat    x
type family XNewPat    x

type ForallXPat c x =
       ( c (XWildPat   x)
       , c (XVarPat    x)
       , c (XLazyPat   x)
       , c (XAsPat     x)
       , c (XParPat    x)
       , c (XBangPat   x)
       , c (XListPat   x)
       , c (XTuplePat  x)
       , c (XSumPat    x)
       , c (XPArrPat   x)
       , c (XConPat    x)
       , c (XViewPat   x)
       , c (XSplicePat x)
       , c (XLitPat    x)
       , c (XNPat      x)
       , c (XNPlusKPat x)
       , c (XSigPat    x)
       , c (XNewPat    x)
       )

type LPat x = Located (Pat x)

----------------------------------------------------------------------------

data RecFields pass arg
  = RecFields
      [LRecField pass arg]
      (Maybe Int)

type LRecFields pass arg = Located (RecFields pass arg)

----------------------------------------------------------------------------

data RecField' id arg
  = RecField
      (Located id)
      arg
      Bool

type LRecField' id arg = Located (RecField' id arg)

-- Todo: maybe redesign with `pass` instead of `id` to make it extensible?

----------------------------------------------------------------------------

type ConPatDetails x = ConDetails (LPat x) (RecFields x (LPat x))

----------------------------------------------------------------------------

type RecField  x arg = RecField' (FieldOcc x) arg

type LRecField x arg = Located (RecField x arg)

----------------------------------------------------------------------------

type RecUpdField   x = RecField' (AmbiguousFieldOcc x) (LExpr x)

type LRecUpdField  x = Located (RecUpdField x)

type LRecUpdFields x = [LRecUpdField x]

----------------------------------------------------------------------------

data AmbiguousFieldOcc x
  = Unambiguous
      (XUnambiguous x)
      (Located RdrName)

  | Ambiguous
      (XAmbiguous x)
      (Located RdrName)

  | NewAmbiguousFieldOcc
      (XNewAmbiguousFieldOcc x)

type family XUnambiguous          x
type family XAmbiguous            x
type family XNewAmbiguousFieldOcc x

type ForallXAmbiguousFieldOcc c x =
       ( c (XUnambiguous          x)
       , c (XAmbiguous            x)
       , c (XNewAmbiguousFieldOcc x)
       )

-- TODO: redesign this so that LRdrName is an annotation

----------------------------------------------------------------------------
-- * Expressions
----------------------------------------------------------------------------

data Expr x
  = Var
      (XVar x)
      (LIdP x)

  | RecFld
      (XRecFld x)
      (AmbiguousFieldOcc x)

  | OverLabel
      (XOverLabel x)
      (Maybe (IdP x))
      FastString

  | IPVar
      (XIPVar x)
      IPName

  | OverLitE
      (XOverLitE x)
      (OverLit x)

  | Lit
      (XLit x)
      (Lit x)

  | Lam
      (XLam x)
      (MatchGroup x (LExpr x))

  | LamCase
      (XLamCase x)
      (MatchGroup x (LExpr x))

  | App
      (XApp x)
      (LExpr x)
      (LExpr x)

  | AppType
      (XAppType x)
      (LExpr x)
      (LWcType x)

  | OpApp
      (XOpApp x)
      (LExpr x)
      (LExpr x)
      (LExpr x)

  | NegApp
      (XNegApp x)
      (LExpr x)

  | Par
      (XPar x)
      (LExpr x)

  | SectionL
      (XSectionL x)
      (LExpr x)
      (LExpr x)

  | SectionR
      (XSectionR x)
      (LExpr x)
      (LExpr x)

  | ExplicitTuple
      (XExplicitTuple x)
      [LTupArg x]
      Boxity

  | ExplicitSum
      (XExplicitSum x)
      ConTag
      Arity
      (LExpr x)

  | Case
      (XCase x)
      (LExpr x)
      (MatchGroup x (LExpr x))

  | If
      (XIf x)
      (LExpr x)
      (LExpr x)
      (LExpr x)

  | MultiIf
      (XMultiIf x)
      [LGRHS x (LExpr x)]

  | Let
      (XLet x)
      (LLocalBinds x)
      (LExpr x)

  | Do
      (XDo x)
      (StmtContext Name) -- TODO: rename this
      (LExprLStmts x)

  | ExplicitList
      (XExplicitList x)
      [LExpr x]

  | ExplicitPArr
      (XExplicitPArr x)
      [LExpr x]

  | RecordCon
      (XRecordCon x)
      (LIdP x)
      (RecordBinds x)

  | RecordUpd
      (XRecordUpd x)
      (LExpr x)
      (LRecUpdFields x)

  | ExprWithTySig
      (XExprWithTySig x)
      (LExpr x)
      (LSigWcType x)

  | ArithSeq
      (XArithSeq x)
      (ArithSeqInfo x)

  | PArrSeq
      (XPArrSeq x)
      (ArithSeqInfo x)

  | SCC
      (XSCC x)
      SourceText
      StringLiteral
      (LExpr x)

  | CoreAnn
      (XCoreAnn x)
      SourceText
      StringLiteral
      (LExpr x)

  | Bracket
      (XBracket x)
      (Bracket x)

  | SpliceE
      (XSpliceE x)
      (Splice x)

  | Proc
      (XProc x)
      (LPat x)
      (LCmdTop x)

  | Static
      (XStatic x)
      (LExpr x)

  | ArrApp
      (XArrApp x)
      (LExpr x)
      (LExpr x)
      ArrAppType
      Bool

  | ArrForm
      (XArrForm x)
      (LExpr x)
      (Maybe Fixity)
      [LCmdTop x]

  | TickPragma
      (XTickPragma x)
      SourceText
      (StringLiteral, (Int, Int), (Int, Int))
      ((SourceText, SourceText), (SourceText, SourceText))
      (LExpr x)

-- TODO: decide where to put the pattern related con
  | EWildPat
      (XEWildPat x)

  | EAsPat
      (XEAsPat x)
      (LIdP x)
      (LExpr x)

  | EViewPat
      (XEViewPat x)
      (LExpr x)
      (LExpr x)

  | ELazyPat
      (XELazyPat x)
      (LExpr x)

  | NewExpr
      (XNewExpr x)

type family XVar            x
type family XRecFld         x
type family XOverLabel      x
type family XIPVar          x
type family XOverLitE       x
type family XLit            x
type family XLam            x
type family XLamCase        x
type family XApp            x
type family XAppType        x
type family XOpApp          x
type family XNegApp         x
type family XPar            x
type family XSectionL       x
type family XSectionR       x
type family XExplicitTuple  x
type family XExplicitSum    x
type family XCase           x
type family XIf             x
type family XMultiIf        x
type family XLet            x
type family XDo             x
type family XExplicitList   x
type family XExplicitPArr   x
type family XRecordCon      x
type family XRecordUpd      x
type family XExprWithTySig  x
type family XArithSeq       x
type family XPArrSeq        x
type family XSCC            x
type family XCoreAnn        x
type family XBracket        x
type family XSpliceE        x
type family XProc           x
type family XStatic         x
type family XArrApp         x
type family XArrForm        x
type family XTickPragma     x
type family XEWildPat       x
type family XEAsPat         x
type family XEViewPat       x
type family XELazyPat       x
type family XNewExpr        x

type ForallXExpr c x =
       ( c (XVar            x)
       , c (XRecFld         x)
       , c (XOverLabel      x)
       , c (XIPVar          x)
       , c (XOverLitE       x)
       , c (XLit            x)
       , c (XLam            x)
       , c (XLamCase        x)
       , c (XApp            x)
       , c (XAppType        x)
       , c (XOpApp          x)
       , c (XNegApp         x)
       , c (XPar            x)
       , c (XSectionL       x)
       , c (XSectionR       x)
       , c (XExplicitTuple  x)
       , c (XExplicitSum    x)
       , c (XCase           x)
       , c (XIf             x)
       , c (XMultiIf        x)
       , c (XLet            x)
       , c (XDo             x)
       , c (XExplicitList   x)
       , c (XExplicitPArr   x)
       , c (XRecordCon      x)
       , c (XRecordUpd      x)
       , c (XExprWithTySig  x)
       , c (XArithSeq       x)
       , c (XPArrSeq        x)
       , c (XSCC            x)
       , c (XCoreAnn        x)
       , c (XBracket        x)
       , c (XSpliceE        x)
       , c (XProc           x)
       , c (XStatic         x)
       , c (XArrApp         x)
       , c (XArrForm        x)
       , c (XTickPragma     x)
       , c (XEWildPat       x)
       , c (XEAsPat         x)
       , c (XEViewPat       x)
       , c (XELazyPat       x)
       , c (XNewExpr        x)
       )

type LExpr x = Located (Expr x)

----------------------------------------------------------------------------

data TupArg x
  = Present
      (XPresent x)
      (LExpr x)

  | Missing
      (XMissing x)

  | NewTupArg
      (XNewTupArg x)

type family XPresent   x
type family XMissing   x
type family XNewTupArg x

type ForallXTupArg c x =
       ( c (XPresent   x)
       , c (XMissing   x)
       , c (XNewTupArg x)
       )

type LTupArg x = Located (TupArg x)

----------------------------------------------------------------------------

-- Not Extensible

data ArithSeqInfo x
  = From
      (LExpr x)

  | FromThen
      (LExpr x)
      (LExpr x)

  | FromTo
      (LExpr x)
      (LExpr x)

  | FromThenTo
      (LExpr x)
      (LExpr x)
      (LExpr x)

-- TODO: Maybe make above extensible?

----------------------------------------------------------------------------

data Splice x
  = TypedSplice
      (XTypedSplice x)
      SpliceDecoration
      (IdP x)
      (LExpr x)

  | UntypedSplice
      (XUntypedSplice x)
      SpliceDecoration
      (IdP x)
      (LExpr x)

  | QuasiQuote
      (XQuasiQuote x)
      (IdP x)
      (IdP x)
      SrcSpan
      FastString

  | NewSplice
      (XNewSplice x)

type family XTypedSplice   x
type family XUntypedSplice x
type family XQuasiQuote    x
type family XNewSplice     x

type ForallXSplice c x =
       ( c (XTypedSplice   x)
       , c (XUntypedSplice x)
       , c (XQuasiQuote    x)
       , c (XNewSplice     x)
       )

type LSplice x = Located (Splice x)

----------------------------------------------------------------------------

-- Not Extensible

-- | A splice can appear with various decorations wrapped around it. This data
-- type captures explicitly how it was originally written, for use in the pretty
-- printer.
data SpliceDecoration
  = HasParens -- ^ $( splice ) or $$( splice )
  | HasDollar -- ^ $splice or $$splice
  | NoParens  -- ^ bare splice

----------------------------------------------------------------------------

-- Not Extensible

data MatchContext id -- Not an extensible tag
  = FunRhs
      (Located id)
      LexicalFixity
      SrcStrictness

  | LambdaExpr

  | CaseAlt

  | IfAlt

  | ProcExpr

  | PatBindRhs

  | RecUpd

  | StmtCtxt
      (StmtContext id)

  | ThPatSplice

  | ThPatQuote

  | PatSyn

----------------------------------------------------------------------------

-- Not Extensible

data StmtContext id
  = ListComp

  | MonadComp

  | PArrComp

  | DoExpr

  | MDoExpr

  | ArrowExpr

  | GhciStmtCtxt

  | PatGuard
      (MatchContext id)

  | ParStmtCtxt
      (StmtContext id)

  | TransStmtCtxt
      (StmtContext id)

----------------------------------------------------------------------------

data Bracket x
  = ExpBr
      (XExpBr x)
      (LExpr x)

  | PatBr
      (XPatBr x)
      (LPat x)
{- TODO: uncomment when LDecl is brought in
  | DecBrL
      (XDecBrL x)
      [LDecl x]
-}
  | TypBr
      (XTypBr x)
      (LType x)

  | VarBr
      (XVarBr x)
      Bool
      (IdP x)

  | TExpBr
      (XTExpBr x)
      (LExpr x)

  | NewBracket
      (XNewBracket x)

type family XExpBr      x
type family XPatBr      x
-- type family XDecBrL     x
type family XTypBr      x
type family XVarBr      x
type family XTExpBr     x
type family XNewBracket x

type ForallXBracket c x =
       ( c (XExpBr      x)
       , c (XPatBr      x)
--       , c (XDecBrL     x)
       , c (XTypBr      x)
       , c (XVarBr      x)
       , c (XTExpBr     x)
       , c (XNewBracket x)
       )

----------------------------------------------------------------------------

data ArrAppType
  = HigherOrderApp

  | FirstOrderApp

----------------------------------------------------------------------------

data CmdTop x
  = CmdTop
      (XCmdTop x)
      (LCmd x)

  | NewCmdTop
      (XNewCmdTop x)

type family XCmdTop    x
type family XNewCmdTop x

type ForallXCmdTop c x =
       ( c (XCmdTop    x)
       , c (XNewCmdTop x)
       )

type LCmdTop x = Located (CmdTop x)

----------------------------------------------------------------------------

type RecordBinds x = RecFields x (LExpr x)

----------------------------------------------------------------------------

data Cmd x
  = CmdArrApp
      (XCmdArrApp x)
      (LExpr x)
      (LExpr x)
      ArrAppType
      Bool

  | CmdArrForm
      (XCmdArrForm x)
      (LExpr x)
      LexicalFixity
      (Maybe Fixity)
      [LCmdTop x]

  | CmdApp
      (XCmdApp x)
      (LCmd x)
      (LExpr x)

  | CmdLam
      (XCmdLam x)
      (MatchGroup x (LCmd x))

  | CmdPar
      (XCmdPar x)
      (LCmd x)

  | CmdCase
      (XCmdCase x)
      (LExpr x)
      (MatchGroup x (LCmd x))

  | CmdIf
      (XCmdIf x)
      (LExpr x)
      (LCmd x)
      (LCmd x)

  | CmdLet
      (XCmdLet x)
      (LLocalBinds x)
      (LCmd x)

  | CmdDo
      (XCmdDo x)
      (Located [CmdLStmt x])

  | NewCmd
      (XNewCmd x)

type family XCmdArrApp  x
type family XCmdArrForm x
type family XCmdApp     x
type family XCmdLam     x
type family XCmdPar     x
type family XCmdCase    x
type family XCmdIf      x
type family XCmdLet     x
type family XCmdDo      x
type family XNewCmd     x

type ForallXCmd c x =
       ( c (XCmdArrApp  x)
       , c (XCmdArrForm x)
       , c (XCmdApp     x)
       , c (XCmdLam     x)
       , c (XCmdPar     x)
       , c (XCmdCase    x)
       , c (XCmdIf      x)
       , c (XCmdLet     x)
       , c (XCmdDo      x)
       , c (XNewCmd     x)
       )

type
  LCmd x = Located (Cmd x)

----------------------------------------------------------------------------

data MatchGroup x body
  = MG
      (XMG x body)
      (Located [LMatch x body])
      Origin

  | NewMatchGroup
      (XNewMatchGroup x body)

type family XMG            x body
type family XNewMatchGroup x body

type ForallXMatchGroup c x body =
       ( c (XMG            x body)
       , c (XNewMatchGroup x body)
       )

type
  LMatchGroup x body = Located (MatchGroup x body)

----------------------------------------------------------------------------

data Match x body
  = Match
      (XMatch x body)
      (MatchContext (NameOrRdrName (IdP x)))
      [LPat x]
      (Maybe (LType x))
      (GRHSs x body)

  | NewMatch
      (XNewMatch x body)

type family XMatch    x body
type family XNewMatch x body

type ForallXMatch c x body =
       ( c (XMatch    x body)
       , c (XNewMatch x body)
       )

type
  LMatch x body = Located (Match x body)

----------------------------------------------------------------------------

data GRHSs x body
  = GRHSs
      (XGRHSs x body)
      [LGRHS x body]
      (LLocalBinds x)

  | NewGRHSs
      (XNewGRHSs x body)

type family XGRHSs    x body
type family XNewGRHSs x body

type ForallXGRHSs c x body =
       ( c (XGRHSs    x body)
       , c (XNewGRHSs x body)
       )

type
  LGRHSs x body = Located (GRHSs x body)

----------------------------------------------------------------------------

data GRHS x body
  = GRHS
      (XGRHS x body)
      [GuardLStmt x]
      body

  | NewGRHS
      (XNewGRHS x body)

type family XGRHS    x body
type family XNewGRHS x body

type ForallXGRHS c x body =
       ( c (XGRHS    x body)
       , c (XNewGRHS x body)
       )

type
  LGRHS x body = Located (GRHS x body)

----------------------------------------------------------------------------

data ParStmtBlock x x'
  = ParStmtBlock
      (XParStmtBlock x x')
      [ExprLStmt x]
      [IdP x']

  | NewParStmtBlock
      (XNewParStmtBlock x x')

type family XParStmtBlock    x x'
type family XNewParStmtBlock x x'

type ForallXParStmtBlock c x x' =
       ( c (XParStmtBlock    x x')
       , c (XNewParStmtBlock x x')
       )

type
  LParStmtBlock x x' = Located (ParStmtBlock x x')

----------------------------------------------------------------------------

data TransForm
  = ThenForm
  | GroupForm

----------------------------------------------------------------------------

data StmtLR x x' body
  = LastStmt
      (XLastStmt x x' body)
      body
      Bool

  | BindStmt
      (XBindStmt x x' body)
      (LPat x)
      body

  | BodyStmt
      (XBodyStmt x x' body)
      body

  | LetStmt
      (XLetStmt x x' body)
      (LLocalBindsLR x x')

  | ParStmt
      (XParStmt x x' body)
      [ParStmtBlock x x']
      (Expr x')

  | TransStmt
      (XTransStmt x x' body)
      TransForm
      [ExprLStmt x]
      [(IdP x', IdP x')] -- TODO: should change to be an extension
      (LExpr x')
      (Maybe (LExpr x'))
      (Expr x')

  | RecStmt
      (XRecStmt x x' body)
      [LStmtLR x x' body]
      [IdP x']
      [IdP x']

  | NewStmtLR
      (XNewStmtLR x x' body)

type family XLastStmt        x x' body
type family XBindStmt        x x' body
type family XBodyStmt        x x' body
type family XLetStmt         x x' body
type family XParStmt         x x' body
type family XTransStmt       x x' body
type family XRecStmt         x x' body
type family XNewStmtLR       x x' body

type ForallXStmtLR c x x' body =
       ( c (XLastStmt        x x' body)
       , c (XBindStmt        x x' body)
       , c (XBodyStmt        x x' body)
       , c (XLetStmt         x x' body)
       , c (XParStmt         x x' body)
       , c (XTransStmt       x x' body)
       , c (XRecStmt         x x' body)
       , c (XNewStmtLR       x x' body)
       )

type
  LStmtLR x x' body = Located (StmtLR x x' body)

----------------------------------------------------------------------------

type Stmt  x body = StmtLR x x body

type LStmt x body = LStmtLR x x body

----------------------------------------------------------------------------

type CmdStmt  x = Stmt  x (LCmd x)

type CmdLStmt x = LStmt x (LCmd x)

----------------------------------------------------------------------------

type ExprLStmt   x = LStmt x (LExpr x)

type ExprStmt    x = Stmt  x (LExpr x)

type ExprLStmts  x = [ExprLStmt x]

type LExprLStmts x = Located (ExprLStmts x)

----------------------------------------------------------------------------

type GuardLStmt x = LStmt x (LExpr x)

type GuardStmt  x = Stmt  x (LExpr x)

-- TODO: maybe enforce more static checks?

----------------------------------------------------------------------------

type GhciLStmt x = LStmt x (LExpr x)

type GhciStmt  x = Stmt  x (LExpr x)

-- TODO: maybe enforce more static checks?

----------------------------------------------------------------------------
-- * Bind
----------------------------------------------------------------------------

data BindLR x x'
  = FunBind
      (XFunBind x x')
      (Located (IdP x))
      (MatchGroup x' (LExpr x'))

  | PatBind
      (XPatBind x x')
      (LPat x)
      (GRHSs x' (LExpr x'))

  | VarBind
      (XVarBind x x')
      (IdP x)
      (LExpr x')
      (Bool)

  | PatSynBind
      (XPatSynBind x x')
      (PatSynBind x x')

  | NewBindLR
      (XNewBindLR x x')

type family XFunBind    x x'
type family XPatBind    x x'
type family XVarBind    x x'
type family XPatSynBind x x'
type family XNewBindLR  x x'

type ForallXBindLR c x x' =
       ( c (XFunBind    x x')
       , c (XPatBind    x x')
       , c (XVarBind    x x')
       , c (XPatSynBind x x')
       , c (XNewBindLR  x x')
       )

type
  LBindLR x x' = Located (BindLR x x')

----------------------------------------------------------------------------

type Bind  x = BindLR  x x

type LBind x = LBindLR x x

----------------------------------------------------------------------------

data ValBindsLR x x'
  = ValBinds
      (XValBinds x x')
      (LBindsLR  x x')
      [LSig x']

  | NewValBindsLR
      (XNewValBindsLR x x')

type family XValBinds      x x'
type family XNewValBindsLR x x'

type ForallXValBindsLR c x x' =
       ( c (XValBinds      x x')
       , c (XNewValBindsLR x x')
       )

type
  LValBindsLR x x' = Located (ValBindsLR x x')

----------------------------------------------------------------------------

type ValBinds  x = ValBindsLR  x x

type LValBinds x = LValBindsLR x x

----------------------------------------------------------------------------

data LocalBindsLR x x'
  = ValBindsLocal
      (XValBindsLocal  x x')
      (ValBindsLR      x x')

  | IPBindsLocal
      (XIPBindsLocal x x')
      (IPBinds x')

  | EmptyLocalBinds
      (XEmptyLocalBinds x x')

  | NewLocalBindsLR
      (XNewLocalBindsLR x x')

type family XValBindsLocal   x x'
type family XIPBindsLocal    x x'
type family XEmptyLocalBinds x x'
type family XNewLocalBindsLR x x'

type ForallXLocalBindsLR c x x' =
       ( c (XValBindsLocal   x x')
       , c (XIPBindsLocal    x x')
       , c (XEmptyLocalBinds x x')
       , c (XNewLocalBindsLR x x')
       )

type
  LLocalBindsLR x x' = Located (LocalBindsLR x x')

----------------------------------------------------------------------------

type LocalBinds  x = LocalBindsLR  x x

type LLocalBinds x = LLocalBindsLR x x

----------------------------------------------------------------------------

type LBindsLR x x' = Bag (LBindLR x x')

type LBinds x = LBindsLR x x

----------------------------------------------------------------------------

data IPBinds x
  = IPBinds
      (XIPBinds x)
      [LIPBind x]

  | NewIPBinds
      (XNewIPBinds x)

type family XIPBinds    x
type family XNewIPBinds x

type ForallXIPBinds c x =
       ( c (XIPBinds      x)
       , c (XNewIPBinds x)
       )

type
  LIPBinds x = Located (IPBinds x)

----------------------------------------------------------------------------

type LIPBind x = Located (IPBind x)

data IPBind x
  = IPBind (Either (Located IPName) (IdP x)) (LExpr x)

----------------------------------------------------------------------------

data Sig x
  = TypeSig
      (XTypeSig x)
      [LIdP x]
      (LSigWcType x)

  | PatSynSig
      (XPatSynSig x)
      [LIdP x]
      (LSigType x)

  | ClassOpSig
      (XClassOpSig x)
      Bool
      [LIdP x]
      (LSigType x)

  | FixSig
      (XFixSig x)
      (FixitySig x)

  | InlineSig
      (XInlineSig x)
      (LIdP x)
      InlinePragma

  | SpecSig
      (XSpecSig x)
      (LIdP x)
      [LSigType x]
      InlinePragma

  | SpecInstSig
      (XSpecInstSig x)
      (LSigType x)

  | MinimalSig
      (XMinimalSig x)
      (LBooleanFormula (LIdP x))

  | SCCFunSig
      (XSCCFunSig x)
      (LIdP x)
      (Maybe (Located StringLiteral))

  | CompleteMatchSig
      (XCompleteMatchSig x)
      (Located [LIdP x])
      (Maybe (LIdP x))

  | NewSig
      (XNewSig x)

type family XTypeSig          x
type family XPatSynSig        x
type family XClassOpSig       x
type family XFixSig           x
type family XInlineSig        x
type family XSpecSig          x
type family XSpecInstSig      x
type family XMinimalSig       x
type family XSCCFunSig        x
type family XCompleteMatchSig x
type family XNewSig           x

type ForallXSig c x =
       ( c (XTypeSig          x)
       , c (XPatSynSig        x)
       , c (XClassOpSig       x)
       , c (XFixSig           x)
       , c (XInlineSig        x)
       , c (XSpecSig          x)
       , c (XSpecInstSig      x)
       , c (XMinimalSig       x)
       , c (XSCCFunSig        x)
       , c (XCompleteMatchSig x)
       , c (XNewSig           x)
       )

type
  LSig x = Located (Sig x)

----------------------------------------------------------------------------

data FixitySig x
  = FixitySig
      [LIdP x]
      Fixity

type LFixitySig x = Located (FixitySig x)

----------------------------------------------------------------------------

data PatSynBind x x'
  = PSB
      (XPSB x x')
      (LIdP x)
      (PatSynDetails (LIdP x'))
      (LPat x')
      (PatSynDir x')

  | NewPatSynBind
      (XNewPatSynBind x x')

type family XPSB           x x'
type family XNewPatSynBind x x'

type ForallXPatSynBind c x x' =
       ( c (XPSB           x x')
       , c (XNewPatSynBind x x')
       )

type
  LPatSynBind x x' = Located (PatSynBind x x')

----------------------------------------------------------------------------

-- Not Extensible

data PatSynDir x
  = Unidirectional
  | ImplicitBidirectional
  | ExplicitBidirectional (MatchGroup x (LExpr x))

-- TODO: maybe make above extensible

type LPatSynDir x = Located (PatSynDir x)

----------------------------------------------------------------------------

-- Not Extensible

data PatSynDetails a
  = InfixPatSyn a a
  | PrefixPatSyn [a]
  | RecordPatSyn [RecordPatSynField a]

-- TODO: maybe make above extensible

type
  LPatSynDetails a = Located (PatSynDetails a)

----------------------------------------------------------------------------

data RecordPatSynField a
  = RecordPatSynField
      a
      a

type
  LRecordPatSynField a = Located (RecordPatSynField a)

----------------------------------------------------------------------------
-- * Declarations
----------------------------------------------------------------------------

data Decl x
  = TyClD
      (XTyClD x)
      (TyClDecl x)

  | InstD
      (XInstD x)
      (InstDecl x)

  | DerivD
      (XDerivD x)
      (DerivDecl x)

  | ValD
      (XValD x)
      (Bind x)

  | SigD
      (XSigD x)
      (Sig x)

  | DefD
      (XDefD x)
      (DefaultDecl x)

  | ForD
      (XForD x)
      (ForeignDecl x)

  | WarningD
      (XWarningD x)
      (WarnDecls x)

  | AnnD
      (XAnnD x)
      (AnnDecl x)

  | RuleD
      (XRuleD x)
      (RuleDecls x)

  | VectD
      (XVectD x)
      (VectDecl x)

  | SpliceD
      (XSpliceD x)
      (SpliceDecl x)

  | DocD
      (XDocD x)
      DocDecl

  | RoleAnnotD
      (XRoleAnnotD x)
      (RoleAnnotDecl x)

  | NewDecl
      (XNewDecl x)

type family XTyClD      x
type family XInstD      x
type family XDerivD     x
type family XValD       x
type family XSigD       x
type family XDefD       x
type family XForD       x
type family XWarningD   x
type family XAnnD       x
type family XRuleD      x
type family XVectD      x
type family XSpliceD    x
type family XDocD       x
type family XRoleAnnotD x
type family XNewDecl  x

type ForallXDecl c x =
       ( c (XTyClD      x)
       , c (XInstD      x)
       , c (XDerivD     x)
       , c (XValD       x)
       , c (XSigD       x)
       , c (XDefD       x)
       , c (XForD       x)
       , c (XWarningD   x)
       , c (XAnnD       x)
       , c (XRuleD      x)
       , c (XVectD      x)
       , c (XSpliceD    x)
       , c (XDocD       x)
       , c (XRoleAnnotD x)
       , c (XNewDecl  x)
       )

type
  LDecl x = Located (Decl x)

-- -----------------------------------------------------------------------------

data Group x
  = Group
      (XGroup x)
      (ValBinds x)
      [LSpliceDecl x]
      [TyClGroup x]
      [LDerivDecl x]
      [LFixitySig x]
      [LDefaultDecl x]
      [LForeignDecl x]
      [LWarnDecls x]
      [LAnnDecl x]
      [LRuleDecls x]
      [LVectDecl x]
      [LDocDecl]

  | NewGroup
      (XNewGroup x)

type family XGroup    x
type family XNewGroup x

type ForallXGroup c x =
       ( c (XGroup    x)
       , c (XNewGroup x)
       )

type
  LGroup x = Located (Group x)
-- -----------------------------------------------------------------------------

data SpliceDecl x
  = SpliceDecl
      (XSpliceDecl x)
      (LSplice x)
      SpliceExplicitFlag

  | NewSpliceDecl
      (XNewSpliceDecl x)

type family XSpliceDecl    x
type family XNewSpliceDecl x

type ForallXSpliceDecl c x =
       ( c (XSpliceDecl    x)
       , c (XNewSpliceDecl x)
       )

type
  LSpliceDecl x = Located (SpliceDecl x)

-- -----------------------------------------------------------------------------

data TyClDecl x
  = FamDecl
      (XFamDecl x)
      (FamilyDecl x)

  | SynDecl
      (XSynDecl x)
      (LIdP x)
      (LQTyVars x)
      LexicalFixity
      (LType x)

  | DataDecl
      (XDataDecl x)
      (LIdP x)
      (LQTyVars x)
      LexicalFixity
      (DataDefn x)

  | ClassDecl
      (XClassDecl x)
      (LContext x)
      (LIdP x)
      (LQTyVars x)
      LexicalFixity
      [Located (FunDep (LIdP x))]
      [LSig x]
      (LBinds x)
      [LFamilyDecl x]
      [LTyFamDefltEqn x]
      [LDocDecl]

  | NewTyClDecl
      (XNewTyClDecl x)

type family XFamDecl     x
type family XSynDecl     x
type family XDataDecl    x
type family XClassDecl   x
type family XNewTyClDecl x

type ForallXTyClDecl c x =
       ( c (XFamDecl     x)
       , c (XSynDecl     x)
       , c (XDataDecl    x)
       , c (XClassDecl   x)
       , c (XNewTyClDecl x)
       )

type
  LTyClDecl x = Located (TyClDecl x)

-- -----------------------------------------------------------------------------

data TyClGroup x
  = TyClGroup
      (XTyClGroup x)
      [LTyClDecl x]
      [LRoleAnnotDecl x]
      [LInstDecl x]

  | NewTyClGroup
      (XNewTyClGroup x)

type family XTyClGroup    x
type family XNewTyClGroup x

type ForallXTyClGroup c x =
       ( c (XTyClGroup    x)
       , c (XNewTyClGroup x)
       )

type
  LTyClGroup x = Located (TyClGroup x)

-- -----------------------------------------------------------------------------

data FamilyResultSig x
  = NoSig
      (XNoSig x)

  | KindSigR
      (XKindSigR x) -- renamed
      (LKind x)

  | TyVarSig
      (XTyVarSig x)
      (LTyVarBndr x)

  | NewFamilyResultSig
      (XNewFamilyResultSig x)

type family XNoSig              x
type family XKindSigR           x
type family XTyVarSig           x
type family XNewFamilyResultSig x

type ForallXFamilyResultSig c x =
       ( c (XNoSig              x)
       , c (XKindSig            x)
       , c (XTyVarSig           x)
       , c (XNewFamilyResultSig x)
       )

type
  LFamilyResultSig x = Located (FamilyResultSig x)

-- -----------------------------------------------------------------------------

data FamilyDecl x
  = FamilyDecl
      (XFamilyDecl x)
      (FamilyInfo x)
      (LIdP x)
      (LQTyVars x)
      LexicalFixity
      (LFamilyResultSig x)
      (Maybe (LInjectivityAnn x))

  | NewFamilyDecl
      (XNewFamilyDecl x)

type family XFamilyDecl    x
type family XNewFamilyDecl x

type ForallXFamilyDecl c x =
       ( c (XFamilyDecl    x)
       , c (XNewFamilyDecl x)
       )

type
  LFamilyDecl x = Located (FamilyDecl x)

-- -----------------------------------------------------------------------------

data InjectivityAnn x
  = InjectivityAnn
      (XInjectivityAnn x)
      (LIdP x)
      [LIdP x]

  | NewInjectivityAnn
      (XNewInjectivityAnn x)

type family XInjectivityAnn    x
type family XNewInjectivityAnn x

type ForallXInjectivityAnn c x =
       ( c (XInjectivityAnn    x)
       , c (XNewInjectivityAnn x)
       )

type
  LInjectivityAnn x = Located (InjectivityAnn x)

-- -----------------------------------------------------------------------------

data FamilyInfo x
  = DataFamily
      (XDataFamily x)

  | OpenTypeFamily
      (XOpenTypeFamily x)

  | ClosedTypeFamily
      (XClosedTypeFamily x)
      (Maybe [LTyFamInstEqn x])

  | NewFamilyInfo
      (XNewFamilyInfo x)

type family XDataFamily       x
type family XOpenTypeFamily   x
type family XClosedTypeFamily x
type family XNewFamilyInfo    x

type ForallXFamilyInfo c x =
       ( c (XDataFamily       x)
       , c (XOpenTypeFamily   x)
       , c (XClosedTypeFamily x)
       , c (XNewFamilyInfo    x)
       )

type
  LFamilyInfo x = Located (FamilyInfo x)

-- -----------------------------------------------------------------------------

data DataDefn x
  = DataDefn
      (XDataDefn x)
      NewOrData
      (LContext x)
      (Maybe (Located CType))
      (Maybe (LKind x))
      [LConDecl x]
      (Deriving x)

  | NewDataDefn
      (XNewDataDefn x)

type family XDataDefn    x
type family XNewDataDefn x

type ForallXDataDefn c x =
       ( c (XDataDefn    x)
       , c (XNewDataDefn x)
       )

type
  LDataDefn x = Located (DataDefn x)

-- -----------------------------------------------------------------------------

type
  Deriving x = Located [LDerivingClause x]

type
  LDeriving x = Located (Deriving x)

-- -----------------------------------------------------------------------------

data DerivingClause x
  = DerivingClause
      (XDerivingClause x)
      (Maybe (Located DerivStrategy))
      (Located [LSigType x])

  | NewDerivingClause
      (XNewDerivingClause x)

type family XDerivingClause    x
type family XNewDerivingClause x

type ForallXDerivingClause c x =
       ( c (XDerivingClause    x)
       , c (XNewDerivingClause x)
       )

type
  LDerivingClause x = Located (DerivingClause x)

-- -----------------------------------------------------------------------------

data NewOrData
  = NewTypeN -- renamed +N
  | DataType

type
  LNewOrData  = Located NewOrData

-- -----------------------------------------------------------------------------

data ConDecl x
  = ConDeclGADT
      (XConDeclGADT x)
      [LIdP x]
      (LSigType x)
      (Maybe LDocString)

  | ConDeclH98
      (XConDeclH98 x)
      (LIdP x)
      (Maybe (LQTyVars x))
      (Maybe (LContext x))
      (ConDeclDetails x)
      (Maybe LDocString)

  | NewConDecl
      (XNewConDecl x)

type family XConDeclGADT x
type family XConDeclH98  x
type family XNewConDecl  x

type ForallXConDecl c x =
       ( c (XConDeclGADT x)
       , c (XConDeclH98  x)
       , c (XNewConDecl  x)
       )

type
  LConDecl x = Located (ConDecl x)

-- -----------------------------------------------------------------------------

type
  ConDeclDetails x = ConDetails (LBangType x) (Located [LConDeclField x])

type
  LConDeclDetails x = Located (ConDeclDetails x)

-- -----------------------------------------------------------------------------

type
  TyFamInstEqn x = TyFamEqn x (TyPats x)

type
  LTyFamInstEqn x = Located (TyFamInstEqn x)

-- -----------------------------------------------------------------------------

type
  TyFamDefltEqn x = TyFamEqn x (LQTyVars x)

type
  LTyFamDefltEqn x = Located (TyFamDefltEqn x)

-- -----------------------------------------------------------------------------

type
  TyPats x = ImplicitBndrs x [LType x]

type
  LTyPats x = Located (TyPats x)

-- -----------------------------------------------------------------------------

data TyFamEqn x pats
  = TyFamEqn
      (XTyFamEqn x pats)
      (LIdP x)
      pats
      LexicalFixity
      (LType x)

  | NewTyFamEqn
      (XNewTyFamEqn x pats)

type family XTyFamEqn    x pats
type family XNewTyFamEqn x pats

type ForallXTyFamEqn c x pats =
       ( c (XTyFamEqn    x pats)
       , c (XNewTyFamEqn x pats)
       )

type
  LTyFamEqn x pats = Located (TyFamEqn x pats)

-- -----------------------------------------------------------------------------

data TyFamInstDecl x
  = TyFamInstDecl
      (XTyFamInstDecl x)
      (LTyFamInstEqn x)

  | NewTyFamInstDecl
      (XNewTyFamInstDecl x)

type family XTyFamInstDecl    x
type family XNewTyFamInstDecl x

type ForallXTyFamInstDecl c x =
       ( c (XTyFamInstDecl    x)
       , c (XNewTyFamInstDecl x)
       )

type
  LTyFamInstDecl x = Located (TyFamInstDecl x)
-- -----------------------------------------------------------------------------

data DataFamInstDecl x
  = DataFamInstDecl
      (XDataFamInstDecl x)
      (LIdP x)
      (TyPats x)
      LexicalFixity
      (DataDefn x)

  | NewDataFamInstDecl
      (XNewDataFamInstDecl x)

type family XDataFamInstDecl    x
type family XNewDataFamInstDecl x

type ForallXDataFamInstDecl c x =
       ( c (XDataFamInstDecl    x)
       , c (XNewDataFamInstDecl x)
       )

type
  LDataFamInstDecl x = Located (DataFamInstDecl x)

-- -----------------------------------------------------------------------------

data ClsInstDecl x
  = ClsInstDecl
      (XClsInstDecl x)
      (LSigType x)
      (LBinds x)
      [LSig x]
      [LTyFamInstDecl x]
      [LDataFamInstDecl x]
      (Maybe (Located OverlapMode))

  | NewClsInstDecl
      (XNewClsInstDecl x)

type family XClsInstDecl    x
type family XNewClsInstDecl x

type ForallXClsInstDecl c x =
       ( c (XClsInstDecl    x)
       , c (XNewClsInstDecl x)
       )

type
  LClsInstDecl x = Located (ClsInstDecl x)

-- -----------------------------------------------------------------------------

data InstDecl x
  = ClsInstD
      (XClsInstD x)
      (ClsInstDecl x)

  | DataFamInstD
      (XDataFamInstD x)
      (DataFamInstDecl x)

  | TyFamInstD
      (XTyFamInstD x)
      (TyFamInstDecl x)

  | NewInstDecl
      (XNewInstDecl x)

type family XClsInstD     x
type family XDataFamInstD x
type family XTyFamInstD   x
type family XNewInstDecl  x

type ForallXInstDecl c x =
       ( c (XClsInstD     x)
       , c (XDataFamInstD x)
       , c (XTyFamInstD   x)
       , c (XNewInstDecl  x)
       )

type
  LInstDecl x = Located (InstDecl x)

-- -----------------------------------------------------------------------------

data DerivDecl x
  = DerivDecl
      (XDerivDecl x)
      (LSigType x)
      (Maybe (Located DerivStrategy))
      (Maybe (Located OverlapMode))

  | NewDerivDecl
      (XNewDerivDecl x)

type family XDerivDecl    x
type family XNewDerivDecl x

type ForallXDerivDecl c x =
       ( c (XDerivDecl    x)
       , c (XNewDerivDecl x)
       )

type
  LDerivDecl x = Located (DerivDecl x)

-- -----------------------------------------------------------------------------

data DefaultDecl x
  = DefaultDecl
      (XDefaultDecl x)
      [LType x]

  | NewDefaultDecl
      (XNewDefaultDecl x)

type family XDefaultDecl    x
type family XNewDefaultDecl x

type ForallXDefaultDecl c x =
       ( c (XDefaultDecl    x)
       , c (XNewDefaultDecl x)
       )

type
  LDefaultDecl x = Located (DefaultDecl x)

-- -----------------------------------------------------------------------------

data ForeignDecl x
  = ForeignImport
      (XForeignImport x)
      (LIdP x)
      (LSigType x)
      ForeignImport

  | ForeignExport
      (XForeignExport x)
      (LIdP x)
      (LSigType x)
      ForeignExport

  | NewForeignDecl
      (XNewForeignDecl x)

type family XForeignImport  x
type family XForeignExport  x
type family XNewForeignDecl x

type ForallXForeignDecl c x =
       ( c (XForeignImport  x)
       , c (XForeignExport  x)
       , c (XNewForeignDecl x)
       )

type
  LForeignDecl x = Located (ForeignDecl x)

-- -----------------------------------------------------------------------------

data ForeignImport
  = CImport
      (Located CCallConv)
      (Located Safety)
      (Maybe Header)
      CImportSpec
      (Located SourceText) -- <--- extension?

type
  LForeignImport = Located ForeignImport

-- -----------------------------------------------------------------------------

data CImportSpec
  = CLabel
      CLabelString

  | CFunction
      CCallTarget

  | CWrapper

type
  LCImportSpec  = Located CImportSpec

-- -----------------------------------------------------------------------------

data ForeignExport
  = CExport
      (Located CExportSpec)
      (Located SourceText)

type
  LForeignExport = Located ForeignExport

-- -----------------------------------------------------------------------------

data RuleDecls x
  = Rules
      (XRules x)
      (SourceText)
      [LRuleDecl x]

  | NewRuleDecls
      (XNewRuleDecls x)

type family XRules      x
type family XNewRuleDecls x

type ForallXRuleDecls c x =
       ( c (XRules      x)
       , c (XNewRuleDecls x)
       )

type
  LRuleDecls x = Located (RuleDecls x)

-- -----------------------------------------------------------------------------

data RuleDecl x
  = Rule
      (XRule x)
      (Located (SourceText, RuleName)) -- sourcetext ?
      Activation
      [LRuleBndr x]
      (Located (Expr x))
      (Located (Expr x))

  | NewRuleDecl
      (XNewRuleDecl x)

type family XRule      x
type family XNewRuleDecl x

type ForallXRuleDecl c x =
       ( c (XRule      x)
       , c (XNewRuleDecl x)
       )

type
  LRuleDecl x = Located (RuleDecl x)

-- -----------------------------------------------------------------------------

data RuleBndr x
  = RuleBndr
      (XRuleBndr x)
      (LIdP x)

  | RuleBndrSig
      (XRuleBndrSig x)
      (LIdP x)
      (LSigWcType x)

  | NewRuleBndr
      (XNewRuleBndr x)

type family XRuleBndr    x
type family XRuleBndrSig x
type family XNewRuleBndr x

type ForallXRuleBndr c x =
       ( c (XRuleBndr    x)
       , c (XRuleBndrSig x)
       , c (XNewRuleBndr x)
       )

type
  LRuleBndr x = Located (RuleBndr x)

-- -----------------------------------------------------------------------------

data VectDecl x
  = Vect
      (XVect x)
      SourceText -- extension?
      (LIdP x)
      (LExpr x)

  | NoVect
      (XNoVect x)
      SourceText -- extension?
      (LIdP x)

  | VectType
      (XVectType x)
      SourceText -- extension?
      Bool
      (LIdP x)
      (Maybe (LIdP x))

  | VectClass
      (XVectClass x)
      SourceText -- extension?
      (LIdP x)

  | VectInst
      (XVectInst x)
      (LSigType x)

  | NewVectDecl
      (XNewVectDecl x)

type family XVect        x
type family XNoVect      x
type family XVectType    x
type family XVectClass   x
type family XVectInst    x
type family XNewVectDecl x

type ForallXVectDecl c x =
       ( c (XVect         x)
       , c (XNoVect       x)
       , c (XVectType     x)
       , c (XVectClass    x)
       , c (XVectInst     x)
       , c (XNewVectDecl  x)
       )

type
  LVectDecl x = Located (VectDecl x)

-- -----------------------------------------------------------------------------

data DocDecl
  = DocCommentNext
      DocString

  | DocCommentPrev
      DocString

  | DocCommentNamed
      String
      DocString

  | DocGroup
      Int
      DocString

type
  LDocDecl = Located DocDecl

-- -----------------------------------------------------------------------------

data WarnDecls x
  = Warnings
      (XWarnings x)
      SourceText -- extension?
      [LWarnDecl x]

  | NewWarnDecls
      (XNewWarnDecls x)

type family XWarnings     x
type family XNewWarnDecls x

type ForallXWarnDecls c x =
       ( c (XWarnings     x)
       , c (XNewWarnDecls x)
       )

type
  LWarnDecls x = Located (WarnDecls x)

-- -----------------------------------------------------------------------------

data WarnDecl x
  = Warning
      (XWarning x)
      [LIdP x]
      WarningTxt

  | NewWarnDecl
      (XNewWarnDecl x)

type family XWarning     x
type family XNewWarnDecl x

type ForallXWarnDecl c x =
       ( c (XWarning     x)
       , c (XNewWarnDecl x)
       )

type
  LWarnDecl x = Located (WarnDecl x)

-- -----------------------------------------------------------------------------

data AnnDecl x
  = Annotation
      (XAnnotation x)
      SourceText -- extension?
      (AnnProvenance (IdP x))
      (LExpr x)

  | NewAnnDecl
      (XNewAnnDecl x)

type family XAnnotation x
type family XNewAnnDecl   x

type ForallXAnnDecl c x =
       ( c (XAnnotation x)
       , c (XNewAnnDecl x)
       )

type
  LAnnDecl x = Located (AnnDecl x)

-- -----------------------------------------------------------------------------

-- todo fix x --> id
data AnnProvenance id
  = ValueAnnProvenance
      (Located id)

  | TypeAnnProvenance
      (Located id)

  | ModuleAnnProvenance

type
  LAnnProvenance id = Located (AnnProvenance id)

-- -----------------------------------------------------------------------------

data RoleAnnotDecl x
  = RoleAnnotDecl
      (XRoleAnnotDecl x)
      (LIdP x)
      [Located (Maybe Role)]

  | NewRoleAnnotDecl
      (XNewRoleAnnotDecl x)

type family XRoleAnnotDecl    x
type family XNewRoleAnnotDecl x

type ForallXRoleAnnotDecl c x =
       ( c (XRoleAnnotDecl    x)
       , c (XNewRoleAnnotDecl x)
       )

type
  LRoleAnnotDecl x = Located (RoleAnnotDecl x)


----------------------------------------------------------------------------
-- * Implicit Expressions
----------------------------------------------------------------------------

data ImportDecl x
  = ImportDecl
      (XImportDecl x)
      (SourceText) -- consider as extension?
      (Located ModuleName)
      (Maybe StringLiteral)
      (Bool)
      (Bool)
      (Bool)
      (Bool)
      (Maybe (Located ModuleName))
      (Maybe (Bool, Located [LIE x]))

  | NewImportDecl
      (XNewImportDecl x)

type family XImportDecl    x
type family XNewImportDecl x

type ForallXImportDecl c x =
       ( c (XImportDecl    x)
       , c (XNewImportDecl x)
       )

type
  LImportDecl x = Located (ImportDecl x)

----------------------------------------------------------------------------


-- Non-Extensible

data IEWrappedName id
  = IEName    (Located id)
  | IEPattern (Located id)
  | IEType    (Located id)

type
  LIEWrappedName id = Located (IEWrappedName id)

----------------------------------------------------------------------------

data IEWildcard = NoIEWildcard | IEWildcard Int

----------------------------------------------------------------------------

data IE x
  = IEVar
      (XIEVar x)
      (LIEWrappedName (IdP x))

  | IEThingAbs
      (XIEThingAbs x)
      (LIEWrappedName (IdP x))

  | IEThingAll
      (XIEThingAll x)
      (LIEWrappedName (IdP x))

  | IEThingWith
      (XIEThingWith x)
      (LIEWrappedName (IdP x))
      IEWildcard
      [LIEWrappedName (IdP x)]
      [Located (FieldLbl (IdP x))]

  | IEModuleContents
      (XIEModuleContents x)
      (Located ModuleName)

  | IEGroup
      (XIEGroup x)
      Int
      DocString

  | IEDoc
      (XIEDoc x)
      DocString

  | IEDocNamed
      (XIEDocNamed x)
      String

  | NewIE
      (XNewIE x)

type family XIEVar            x
type family XIEThingAbs       x
type family XIEThingAll       x
type family XIEThingWith      x
type family XIEModuleContents x
type family XIEGroup          x
type family XIEDoc            x
type family XIEDocNamed       x
type family XNewIE            x

type ForallXIE c x =
       ( c (XIEVar            x)
       , c (XIEThingAbs       x)
       , c (XIEThingAll       x)
       , c (XIEThingWith      x)
       , c (XIEModuleContents x)
       , c (XIEGroup          x)
       , c (XIEDoc            x)
       , c (XIEDocNamed       x)
       , c (XNewIE            x)
       )

type
  LIE x = Located (IE x)


----------------------------------------------------------------------------
-- * Module
----------------------------------------------------------------------------

data Module x
  = Module
      (XModule x)
      (Maybe (Located ModuleName))
      (Maybe (Located [LIE x]))
      ([LImportDecl x])
      ([LDecl x])
      (Maybe (Located WarningTxt))
      (Maybe LDocString)

  | NewModule
      (XNewModule x)

type family XModule    x
type family XNewModule x

type ForallXModule c x =
       ( c (XModule    x)
       , c (XNewModule x)
       )

type
  LModule x = Located (Module x)
