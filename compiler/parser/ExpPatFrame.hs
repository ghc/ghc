{-# LANGUAGE ViewPatterns #-}

module ExpPatFrame (

    -- * The expression/pattern frame
    ExpPatFrame(..),
    LExpPatFrame,

    -- * Tuple elements
    TupArgFrame(..),
    LTupArgFrame,

    -- * Record elements
    FrameRecordBinds,
    FrameRecUpdField,
    LFrameRecUpdField,

    -- * Match elements
    FrameMatch(..),
    LFrameMatch,
    FrameGRHSs(..),
    FrameGRHS(..),
    LFrameGRHS,

    -- * Statements
    FrameStmt(..),
    LFrameStmt,

    -- * Conversion
    fromTupArgPresent,
    checkExpr,
    checkExprStmt,
    checkExprMatch,
    checkExprGRHSs,
    checkExprGRHS,

    -- * Construction
    unguardedFrameRHS

  ) where

import GhcPrelude
import FastString
import Outputable
import SrcLoc
import Name
import RdrName
import BasicTypes
import HsSyn

type LExpPatFrame = Located ExpPatFrame

{-

There are places in the grammar where we do not know whether we are parsing an
expression or a pattern without infinite lookahead (which we do not have in
'happy'):

1. View patterns:
     f (Con a b     ) = ...  -- 'Con a b' is a pattern
     f (Con a b -> x) = ...  -- 'Con a b' is an expression

2. do-notation:
     do { Con a b <- x } -- 'Con a b' is a pattern
     do { Con a b }      -- 'Con a b' is an expression

3. Guards:
     x | True <- p && q = ...  -- 'True' is a pattern
     x | True           = ...  -- 'True' is an expression

4. Top-level value/function declarations (FunBind/PatBind):
     f !a         -- TH splice
     f !a = ...   -- function declaration
   Until we encounter the = sign, we don't know if it's a top-level
   TemplateHaskell splice where ! is an infix operator, or if it's a function
   declaration where ! is a strictness annotation.

An ExpPatFrame (expression/pattern frame) is an intermediate data structure for
parsing expressions and patterns. We convert to HsExpr or HsPat when we can
resolve the ambiguity.

See https://ghc.haskell.org/trac/ghc/wiki/Design/ExpPatFrame for details.

-}
data ExpPatFrame
  = FrameVar RdrName
    -- ^ Identifier: Just, map, BS.length
  | FrameIPVar HsIPName
    -- ^ Implicit parameter: ?x
  | FrameOverLabel FastString
    -- ^ Overloaded label: #label
  | FrameLit (HsLit GhcPs)
    -- ^ Non-overloaded literal: 'c', "str"
  | FrameOverLit (HsOverLit GhcPs)
    -- ^ Overloaded literal: 15, 2.4
  | FramePar LExpPatFrame
    -- ^ Parentheses
  | FrameSum ConTag Arity LExpPatFrame
    -- ^ Sum: (a||), (|a|), (||a)
  | FrameTuple [LTupArgFrame] Boxity
    -- ^ Tuple (section): (a,b) (a,b,c) (a,,) (,a,)
  | FrameList [LExpPatFrame]
    -- ^ List: [1, 2, 3]
  | FrameComp (HsStmtContext Name) [LFrameStmt] LExpPatFrame
    -- ^ List/monad comprehension: [ a | x <- f n, p, q ]
  | FrameArithSeq (ArithSeqInfo GhcPs)
    -- ^ Arithmetic sequence: [1..], [1,2..], [1..5]
  | FrameWild
    -- ^ Wildcard: _
  | FrameSplice (HsSplice GhcPs)
    -- ^ TH splice: $a, $(expr), $$(expr), [quasi| ... |]
  | FrameBracket (HsBracket GhcPs)
    -- ^ TH bracket: [|expr|], [p|pat|], 'x, ''T
  | FrameArrForm LExpPatFrame [LHsCmdTop GhcPs]
    -- ^ Command formation (arrows): (| e cmd1 cmd2 cmd3 |)
  | FrameRecordUpd LExpPatFrame [LFrameRecUpdField]
    -- ^ Record update: (f x) { a = z }
  | FrameRecordCon (Located RdrName) FrameRecordBinds
    -- ^ Record constructor: D { x, y = f t, .. }
  | FrameAsPat (Located RdrName) LExpPatFrame
    -- ^ As-pattern: x@(D a b)
  | FrameLam [LPat GhcPs] LExpPatFrame
    -- ^ Lambda-expression: \x -> e
  | FrameLet (LHsLocalBinds GhcPs) LExpPatFrame
    -- ^ Let-expression: let p = t in e
  | FrameLamCase [LFrameMatch]
    -- ^ Lambda-expression: \x -> e
  | FrameIf LExpPatFrame LExpPatFrame LExpPatFrame
    -- ^ If-expression: if p then x else y
  | FrameMultiIf [LFrameGRHS]
    -- ^ Multi-way if-expression: if | p = x \n | q = x
  | FrameCase LExpPatFrame [LFrameMatch]
    -- ^ Case-expression: case x of { p1 -> e1; p2 -> e2 }
  | FrameDo (HsStmtContext Name) [LFrameStmt]
    -- ^ Do-expression: do { s1; a <- s2; s3 }
  | FrameProc (LPat GhcPs) (LHsCmdTop GhcPs)
    -- ^ Proc-expression: proc p -> cmd
  | FrameViewPat LExpPatFrame LExpPatFrame
    -- ^ View pattern: e -> p
  | FrameTySig LExpPatFrame (LHsSigWcType GhcPs)
    -- ^ Type signature: x :: ty
  | FrameArrApp LExpPatFrame LExpPatFrame HsArrAppType Bool
    -- ^ Arrow application: f -< arg, f -<< arg, arg >- f, arg >>- f
  | FrameSCC SourceText StringLiteral LExpPatFrame
    -- ^ SCC annotation: {-# SCC .. #-} e
  | FrameTickPragma
     SourceText
     (StringLiteral,(Int,Int),(Int,Int))
     ((SourceText,SourceText),(SourceText,SourceText))
     LExpPatFrame
    -- ^ Tick pragma: {-# GENERATED .. #-} e
  | FrameCoreAnn SourceText StringLiteral LExpPatFrame
    -- ^ Core annotation: {-# CORE .. #-} e
  | FrameApp LExpPatFrame LExpPatFrame
    -- ^ Function application: f a
  | FrameAppType LExpPatFrame (LHsWcType GhcPs)
    -- ^ Visible type application: f @t
  | FrameOpApp LExpPatFrame LExpPatFrame LExpPatFrame
    -- ^ Operator application: x # y
  | FrameSectionL LExpPatFrame LExpPatFrame
    -- ^ Left section: (x #)
  | FrameSectionR LExpPatFrame LExpPatFrame
    -- ^ Right section: (# y)
  | FrameNegApp LExpPatFrame
    -- ^ Prefix negation: -a
  | FrameLazyPat LExpPatFrame
    -- ^ Lazy pattern: ~a
  | FrameStatic LExpPatFrame
    -- ^ Static expression: static e

instance Outputable ExpPatFrame where
  ppr = ppr . unLoc . checkExpr . noLoc

type FrameRecordBinds = HsRecFields GhcPs LExpPatFrame
type FrameRecUpdField = HsRecField' (AmbiguousFieldOcc GhcPs) LExpPatFrame
type LFrameRecUpdField = Located FrameRecUpdField

type LTupArgFrame = Located TupArgFrame

data TupArgFrame
  = TupArgFramePresent LExpPatFrame
  | TupArgFrameMissing

fromTupArgPresent :: TupArgFrame -> Maybe LExpPatFrame
fromTupArgPresent (TupArgFramePresent e) = Just e
fromTupArgPresent TupArgFrameMissing = Nothing

type LFrameMatch = Located FrameMatch

data FrameMatch =
  FrameMatch (HsMatchContext RdrName) [LPat GhcPs] FrameGRHSs

type LFrameStmt = Located FrameStmt

data FrameGRHSs =
  FrameGRHSs [LFrameGRHS] (LHsLocalBinds GhcPs)

type LFrameGRHS = Located FrameGRHS

data FrameGRHS = FrameGRHS [GuardLStmt GhcPs] LExpPatFrame

data FrameStmt
  = FrameTransformStmt [LFrameStmt] LExpPatFrame
    -- ^ TransformListComp statement: then f
  | FrameTransformByStmt [LFrameStmt] LExpPatFrame LExpPatFrame
    -- ^ TransformListComp statement: then f by e
  | FrameGroupUsingStmt [LFrameStmt] LExpPatFrame
    -- ^ TransformListComp statement: then group using f
  | FrameGroupByUsingStmt [LFrameStmt] LExpPatFrame LExpPatFrame
    -- ^ TransformListComp statement: then group by e using f
  | FrameBindStmt (LPat GhcPs) LExpPatFrame
    -- ^ Binding statement: p <- e
  | FrameBodyStmt LExpPatFrame
    -- ^ Body statement: e
  | FrameLetStmt (LHsLocalBinds GhcPs)
    -- ^ Let statement: let p = t
  | FrameRecStmt [LFrameStmt]
    -- ^ Rec statement: rec { s1; s2; ... }
  | FrameParStmt [[LFrameStmt]]
    -- ^ Parallel statement: s1 | s2

instance Outputable FrameStmt where
  ppr = ppr . unLoc . checkExprStmt . noLoc

{-

Convert an expression/pattern frame to an expression. In the future, this
function will perform validation and reject FrameAsPat, FrameViewPat,
FrameLazyPat, and so on:

  checkExpr :: LExpPatFrame -> P (LHsExpr GhcPs)

-}
checkExpr :: LExpPatFrame -> LHsExpr GhcPs
checkExpr (dL->L l epf) = cL l $ case epf of
  FrameVar name -> HsVar noExt (cL l name)
  FrameIPVar ipname -> HsIPVar noExt ipname
  FrameOverLabel str -> HsOverLabel noExt Nothing str
  FrameLit lit -> HsLit noExt lit
  FrameOverLit lit -> HsOverLit noExt lit
  FramePar e -> HsPar noExt (checkExpr e)
  FrameSum alt arity e -> ExplicitSum noExt alt arity (checkExpr e)
  FrameTuple args boxity ->
    ExplicitTuple noExt (map checkExprTupArg args) boxity
  FrameList xs -> ExplicitList noExt Nothing (map checkExpr xs)
  FrameComp ctxt quals e ->
    mkHsComp ctxt (map checkExprStmt quals) (checkExpr e)
  FrameArithSeq a -> ArithSeq noExt Nothing a
  FrameWild -> EWildPat noExt
  FrameSplice splice -> HsSpliceE noExt splice
  FrameBracket br -> HsBracket noExt br
  FrameArrForm op cmds -> HsArrForm noExt (checkExpr op) Nothing cmds
  FrameRecordUpd exp flds ->
    RecordUpd noExt (checkExpr exp) ((fmap.fmap.fmap) checkExpr flds)
  FrameRecordCon con flds -> RecordCon noExt con (fmap checkExpr flds)
  FrameAsPat v e -> EAsPat noExt v (checkExpr e)
  FrameLam ps e ->
    HsLam noExt $
    mkMatchGroup FromSource
      [cL l $ Match { m_ext = noExt
                    , m_ctxt = LambdaExpr
                    , m_pats = ps
                    , m_grhss = unguardedGRHSs (checkExpr e) }]
  FrameLet binds expr -> HsLet noExt binds (checkExpr expr)
  FrameLamCase matches ->
    HsLamCase noExt $
    mkMatchGroup FromSource (map checkExprMatch matches)
  FrameIf c a b -> mkHsIf (checkExpr c) (checkExpr a) (checkExpr b)
  FrameMultiIf alts -> HsMultiIf noExt (map checkExprGRHS alts)
  FrameCase scrut matches ->
    HsCase noExt (checkExpr scrut) $
    mkMatchGroup FromSource (map checkExprMatch matches)
  FrameDo ctxt stmts -> mkHsDo ctxt (map checkExprStmt stmts)
  FrameProc pat cmd -> HsProc noExt pat cmd
  FrameViewPat p e -> EViewPat noExt (checkExpr p) (checkExpr e)
  FrameTySig e sig -> ExprWithTySig noExt (checkExpr e) sig
  FrameArrApp f a haat b ->
    HsArrApp noExt (checkExpr f) (checkExpr a) haat b
  FrameSCC src lbl e -> HsSCC noExt src lbl (checkExpr e)
  FrameTickPragma src info srcInfo e ->
    HsTickPragma noExt src info srcInfo (checkExpr e)
  FrameCoreAnn src lbl e -> HsCoreAnn noExt src lbl (checkExpr e)
  FrameApp f a -> HsApp noExt (checkExpr f) (checkExpr a)
  FrameAppType f t -> HsAppType noExt (checkExpr f) t
  FrameOpApp e1 op e2 ->
    OpApp noExt (checkExpr e1) (checkExpr op) (checkExpr e2)
  FrameSectionL e1 op -> SectionL noExt (checkExpr e1) (checkExpr op)
  FrameSectionR op e2 -> SectionR noExt (checkExpr op) (checkExpr e2)
  FrameNegApp e -> NegApp noExt (checkExpr e) noSyntaxExpr
  FrameLazyPat p -> ELazyPat noExt (checkExpr p)
  FrameStatic e -> HsStatic noExt (checkExpr e)

checkExprTupArg :: LTupArgFrame -> LHsTupArg GhcPs
checkExprTupArg = mapLoc go
  where
    go (TupArgFramePresent e) = Present noExt (checkExpr e)
    go TupArgFrameMissing = Missing noExt

checkExprStmt :: LFrameStmt -> LStmt GhcPs (LHsExpr GhcPs)
checkExprStmt (dL->L l stmt) = cL l $ case stmt of
  FrameTransformStmt ss f ->
    mkTransformStmt (map checkExprStmt ss) (checkExpr f)
  FrameTransformByStmt ss f e ->
    mkTransformByStmt (map checkExprStmt ss) (checkExpr f) (checkExpr e)
  FrameGroupUsingStmt ss f ->
    mkGroupUsingStmt (map checkExprStmt ss) (checkExpr f)
  FrameGroupByUsingStmt ss e f ->
    mkGroupByUsingStmt (map checkExprStmt ss) (checkExpr e) (checkExpr f)
  FrameBindStmt p e -> mkBindStmt p (checkExpr e)
  FrameBodyStmt e -> mkBodyStmt (checkExpr e)
  FrameLetStmt binds -> LetStmt noExt binds
  FrameRecStmt ss -> mkRecStmt (map checkExprStmt ss)
  FrameParStmt qss ->
    ParStmt noExt
      [ParStmtBlock noExt (map checkExprStmt qs) [] noSyntaxExpr
        | qs <- qss]
      noExpr
      noSyntaxExpr

checkExprMatch :: LFrameMatch -> LMatch GhcPs (LHsExpr GhcPs)
checkExprMatch (dL->L l match) = cL l $
  let FrameMatch ctxt pats grhss = match in
  Match { m_ext = NoExt,
          m_ctxt = ctxt,
          m_pats = pats,
          m_grhss = checkExprGRHSs grhss }

checkExprGRHSs :: FrameGRHSs -> GRHSs GhcPs (LHsExpr GhcPs)
checkExprGRHSs (FrameGRHSs grhss binds) =
  GRHSs { grhssExt = noExt
        , grhssGRHSs = map checkExprGRHS grhss
        , grhssLocalBinds = binds }

checkExprGRHS :: LFrameGRHS -> LGRHS GhcPs (LHsExpr GhcPs)
checkExprGRHS (dL->L l grhs) = cL l $
  let FrameGRHS guards rhs = grhs in
  GRHS noExt guards (checkExpr rhs)

unguardedFrameRHS :: SrcSpan -> LExpPatFrame -> [LFrameGRHS]
unguardedFrameRHS loc rhs = [cL loc (FrameGRHS [] rhs)]
