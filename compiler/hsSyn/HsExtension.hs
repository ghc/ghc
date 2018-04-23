{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder

module HsExtension where

-- This module captures the type families to precisely identify the extension
-- points for HsSyn

import GhcPrelude

import GHC.Exts (Constraint)
import Data.Data hiding ( Fixity )
import PlaceHolder
import BasicTypes
import ConLike
import NameSet
import Name
import RdrName
import Var
import Type       ( Type )
import Outputable
import SrcLoc (Located)
import Coercion
import TcEvidence

{-
Note [Trees that grow]
~~~~~~~~~~~~~~~~~~~~~~

See https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow

The hsSyn AST is reused across multiple compiler passes. We also have the
Template Haskell AST, and the haskell-src-exts one (outside of GHC)

Supporting multiple passes means the AST has various warts on it to cope with
the specifics for the phases, such as the 'ValBindsOut', 'ConPatOut',
'SigPatOut' etc.

The growable AST will allow each of these variants to be captured explicitly,
such that they only exist in the given compiler pass AST, as selected by the
type parameter to the AST.

In addition it will allow tool writers to define their own extensions to capture
additional information for the tool, in a natural way.

A further goal is to provide a means to harmonise the Template Haskell and
haskell-src-exts ASTs as well.

-}

-- | Used when constructing a term with an unused extension point.
noExt :: PlaceHolder
noExt = PlaceHolder

-- | Used as a data type index for the hsSyn AST
data GhcPass (c :: Pass)
deriving instance Eq (GhcPass c)
deriving instance Typeable c => Data (GhcPass c)

data Pass = Parsed | Renamed | Typechecked
         deriving (Data)

-- Type synonyms as a shorthand for tagging
type GhcPs   = GhcPass 'Parsed      -- Old 'RdrName' type param
type GhcRn   = GhcPass 'Renamed     -- Old 'Name' type param
type GhcTc   = GhcPass 'Typechecked -- Old 'Id' type para,
type GhcTcId = GhcTc                -- Old 'TcId' type param


-- | Types that are not defined until after type checking
type family PostTc x ty -- Note [Pass sensitive types] in PlaceHolder
type instance PostTc GhcPs ty = PlaceHolder
type instance PostTc GhcRn ty = PlaceHolder
type instance PostTc GhcTc ty = ty

-- | Types that are not defined until after renaming
type family PostRn x ty  -- Note [Pass sensitive types] in PlaceHolder
type instance PostRn GhcPs ty = PlaceHolder
type instance PostRn GhcRn ty = ty
type instance PostRn GhcTc ty = ty

-- | Maps the "normal" id type for a given pass
type family IdP p
type instance IdP GhcPs = RdrName
type instance IdP GhcRn = Name
type instance IdP GhcTc = Id

type LIdP p = Located (IdP p)

-- =====================================================================
-- Type families for the HsBinds extension points

-- HsLocalBindsLR type families
type family XHsValBinds      x x'
type family XHsIPBinds       x x'
type family XEmptyLocalBinds x x'
type family XXHsLocalBindsLR x x'

type ForallXHsLocalBindsLR (c :: * -> Constraint) (x :: *) (x' :: *) =
       ( c (XHsValBinds      x x')
       , c (XHsIPBinds       x x')
       , c (XEmptyLocalBinds x x')
       , c (XXHsLocalBindsLR x x')
       )

-- ValBindsLR type families
type family XValBinds    x x'
type family XXValBindsLR x x'

type ForallXValBindsLR (c :: * -> Constraint) (x :: *) (x' :: *) =
       ( c (XValBinds    x x')
       , c (XXValBindsLR x x')
       )


-- HsBindsLR type families
type family XFunBind    x x'
type family XPatBind    x x'
type family XVarBind    x x'
type family XAbsBinds   x x'
type family XPatSynBind x x'
type family XXHsBindsLR x x'

type ForallXHsBindsLR (c :: * -> Constraint) (x :: *) (x' :: *) =
       ( c (XFunBind    x x')
       , c (XPatBind    x x')
       , c (XVarBind    x x')
       , c (XAbsBinds   x x')
       , c (XPatSynBind x x')
       , c (XXHsBindsLR x x')
       )

-- ABExport type families
type family XABE x
type family XXABExport x

type ForallXABExport (c :: * -> Constraint) (x :: *) =
       ( c (XABE       x)
       , c (XXABExport x)
       )

-- PatSynBind type families
type family XPSB x x'
type family XXPatSynBind x x'

type ForallXPatSynBind  (c :: * -> Constraint) (x :: *) (x' :: *) =
       ( c (XPSB         x x')
       , c (XXPatSynBind x x')
       )

-- HsIPBinds type families
type family XIPBinds    x
type family XXHsIPBinds x

type ForallXHsIPBinds (c :: * -> Constraint) (x :: *) =
       ( c (XIPBinds    x)
       , c (XXHsIPBinds x)
       )

-- IPBind type families
type family XIPBind  x
type family XXIPBind x

type ForallXIPBind (c :: * -> Constraint) (x :: *) =
       ( c (XIPBind  x)
       , c (XXIPBind x)
       )

-- Sig type families
type family XTypeSig          x
type family XPatSynSig        x
type family XClassOpSig       x
type family XIdSig            x
type family XFixSig           x
type family XInlineSig        x
type family XSpecSig          x
type family XSpecInstSig      x
type family XMinimalSig       x
type family XSCCFunSig        x
type family XCompleteMatchSig x
type family XXSig             x

type ForallXSig (c :: * -> Constraint) (x :: *) =
       ( c (XTypeSig          x)
       , c (XPatSynSig        x)
       , c (XClassOpSig       x)
       , c (XIdSig            x)
       , c (XFixSig           x)
       , c (XInlineSig        x)
       , c (XSpecSig          x)
       , c (XSpecInstSig      x)
       , c (XMinimalSig       x)
       , c (XSCCFunSig        x)
       , c (XCompleteMatchSig x)
       , c (XXSig             x)
       )

-- FixitySig type families
type family XFixitySig          x
type family XXFixitySig         x

type ForallXFixitySig (c :: * -> Constraint) (x :: *) =
       ( c (XFixitySig         x)
       , c (XXFixitySig        x)
       )

-- =====================================================================
-- Type families for the HsDecls extension points


-- TODO

-- =====================================================================
-- Type families for the HsExpr extension points

type family XVar            x
type family XUnboundVar     x
type family XConLikeOut     x
type family XRecFld         x
type family XOverLabel      x
type family XIPVar          x
type family XOverLitE       x
type family XLitE           x
type family XLam            x
type family XLamCase        x
type family XApp            x
type family XAppTypeE       x
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
type family XRnBracketOut   x
type family XTcBracketOut   x
type family XSpliceE        x
type family XProc           x
type family XStatic         x
type family XArrApp         x
type family XArrForm        x
type family XTick           x
type family XBinTick        x
type family XTickPragma     x
type family XEWildPat       x
type family XEAsPat         x
type family XEViewPat       x
type family XELazyPat       x
type family XWrap           x
type family XXExpr          x

type ForallXExpr (c :: * -> Constraint) (x :: *) =
       ( c (XVar            x)
       , c (XUnboundVar     x)
       , c (XConLikeOut     x)
       , c (XRecFld         x)
       , c (XOverLabel      x)
       , c (XIPVar          x)
       , c (XOverLitE       x)
       , c (XLitE           x)
       , c (XLam            x)
       , c (XLamCase        x)
       , c (XApp            x)
       , c (XAppTypeE       x)
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
       , c (XRnBracketOut   x)
       , c (XTcBracketOut   x)
       , c (XSpliceE        x)
       , c (XProc           x)
       , c (XStatic         x)
       , c (XArrApp         x)
       , c (XArrForm        x)
       , c (XTick           x)
       , c (XBinTick        x)
       , c (XTickPragma     x)
       , c (XEWildPat       x)
       , c (XEAsPat         x)
       , c (XEViewPat       x)
       , c (XELazyPat       x)
       , c (XWrap           x)
       , c (XXExpr          x)
       )
-- ---------------------------------------------------------------------

type family XUnambiguous        x
type family XAmbiguous          x
type family XXAmbiguousFieldOcc x

type ForallXAmbiguousFieldOcc (c :: * -> Constraint) (x :: *) =
       ( c (XUnambiguous        x)
       , c (XAmbiguous          x)
       , c (XXAmbiguousFieldOcc x)
       )

-- ----------------------------------------------------------------------

type family XPresent  x
type family XMissing  x
type family XXTupArg  x

type ForallXTupArg (c :: * -> Constraint) (x :: *) =
       ( c (XPresent x)
       , c (XMissing x)
       , c (XXTupArg x)
       )

-- ---------------------------------------------------------------------

type family XTypedSplice   x
type family XUntypedSplice x
type family XQuasiQuote    x
type family XSpliced       x
type family XXSplice       x

type ForallXSplice (c :: * -> Constraint) (x :: *) =
       ( c (XTypedSplice   x)
       , c (XUntypedSplice x)
       , c (XQuasiQuote    x)
       , c (XSpliced       x)
       , c (XXSplice       x)
       )

-- ---------------------------------------------------------------------

type family XExpBr      x
type family XPatBr      x
type family XDecBrL     x
type family XDecBrG     x
type family XTypBr      x
type family XVarBr      x
type family XTExpBr     x
type family XXBracket   x

type ForallXBracket (c :: * -> Constraint) (x :: *) =
       ( c (XExpBr      x)
       , c (XPatBr      x)
       , c (XDecBrL     x)
       , c (XDecBrG     x)
       , c (XTypBr      x)
       , c (XVarBr      x)
       , c (XTExpBr     x)
       , c (XXBracket   x)
       )

-- ---------------------------------------------------------------------

type family XCmdTop  x
type family XXCmdTop x

type ForallXCmdTop (c :: * -> Constraint) (x :: *) =
       ( c (XCmdTop  x)
       , c (XXCmdTop x)
       )

-- ---------------------------------------------------------------------

type family XCmdArrApp  x
type family XCmdArrForm x
type family XCmdApp     x
type family XCmdLam     x
type family XCmdPar     x
type family XCmdCase    x
type family XCmdIf      x
type family XCmdLet     x
type family XCmdDo      x
type family XCmdWrap    x
type family XXCmd       x

type ForallXCmd (c :: * -> Constraint) (x :: *) =
       ( c (XCmdArrApp  x)
       , c (XCmdArrForm x)
       , c (XCmdApp     x)
       , c (XCmdLam     x)
       , c (XCmdPar     x)
       , c (XCmdCase    x)
       , c (XCmdIf      x)
       , c (XCmdLet     x)
       , c (XCmdDo      x)
       , c (XCmdWrap    x)
       , c (XXCmd       x)
       )

-- ---------------------------------------------------------------------

type family XParStmtBlock  x x'
type family XXParStmtBlock x x'

type ForallXParStmtBlock (c :: * -> Constraint) (x :: *) (x' :: *) =
       ( c (XParStmtBlock  x x')
       , c (XXParStmtBlock x x')
       )

-- =====================================================================
-- Type families for the HsImpExp extension points

-- TODO

-- =====================================================================
-- Type families for the HsLit extension points

-- We define a type family for each extension point. This is based on prepending
-- 'X' to the constructor name, for ease of reference.
type family XHsChar x
type family XHsCharPrim x
type family XHsString x
type family XHsStringPrim x
type family XHsInt x
type family XHsIntPrim x
type family XHsWordPrim x
type family XHsInt64Prim x
type family XHsWord64Prim x
type family XHsInteger x
type family XHsRat x
type family XHsFloatPrim x
type family XHsDoublePrim x
type family XXLit x

-- | Helper to apply a constraint to all extension points. It has one
-- entry per extension point type family.
type ForallXHsLit (c :: * -> Constraint) (x :: *) =
  ( c (XHsChar       x)
  , c (XHsCharPrim   x)
  , c (XHsDoublePrim x)
  , c (XHsFloatPrim  x)
  , c (XHsInt        x)
  , c (XHsInt64Prim  x)
  , c (XHsIntPrim    x)
  , c (XHsInteger    x)
  , c (XHsRat        x)
  , c (XHsString     x)
  , c (XHsStringPrim x)
  , c (XHsWord64Prim x)
  , c (XHsWordPrim   x)
  , c (XXLit         x)
  )

type family XOverLit  x
type family XXOverLit x

type ForallXOverLit (c :: * -> Constraint) (x :: *) =
       ( c (XOverLit  x)
       , c (XXOverLit x)
       )

-- =====================================================================
-- Type families for the HsPat extension points

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
type family XCoPat     x
type family XXPat      x


type ForallXPat (c :: * -> Constraint) (x :: *) =
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
       , c (XViewPat   x)
       , c (XSplicePat x)
       , c (XLitPat    x)
       , c (XNPat      x)
       , c (XNPlusKPat x)
       , c (XSigPat    x)
       , c (XCoPat     x)
       , c (XXPat      x)
       )

-- =====================================================================
-- Type families for the HsTypes type families

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
type family XXType           x

-- | Helper to apply a constraint to all extension points. It has one
-- entry per extension point type family.
type ForallXType (c :: * -> Constraint) (x :: *) =
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
       , c (XXType           x)
       )

-- ---------------------------------------------------------------------

type family XUserTyVar   x
type family XKindedTyVar x
type family XXTyVarBndr  x

type ForallXTyVarBndr (c :: * -> Constraint) (x :: *) =
       ( c (XUserTyVar      x)
       , c (XKindedTyVar    x)
       , c (XXTyVarBndr     x)
       )

-- ---------------------------------------------------------------------

type family XAppInfix  x
type family XAppPrefix x
type family XXAppType  x

type ForallXAppType (c :: * -> Constraint) (x :: *) =
       ( c (XAppInfix   x)
       , c (XAppPrefix  x)
       , c (XXAppType   x)
       )

-- ---------------------------------------------------------------------

type family XFieldOcc  x
type family XXFieldOcc x

type ForallXFieldOcc (c :: * -> Constraint) (x :: *) =
       ( c (XFieldOcc  x)
       , c (XXFieldOcc x)
       )


-- =====================================================================
-- End of Type family definitions
-- =====================================================================

-- ----------------------------------------------------------------------
-- | Conversion of annotations from one type index to another. This is required
-- where the AST is converted from one pass to another, and the extension values
-- need to be brought along if possible. So for example a 'SourceText' is
-- converted via 'id', but needs a type signature to keep the type checker
-- happy.
class Convertable a b  | a -> b where
  convert :: a -> b

instance Convertable a a where
  convert = id

-- | A constraint capturing all the extension points that can be converted via
-- @instance Convertable a a@
type ConvertIdX a b =
  (XHsDoublePrim a ~ XHsDoublePrim b,
   XHsFloatPrim a ~ XHsFloatPrim b,
   XHsRat a ~ XHsRat b,
   XHsInteger a ~ XHsInteger b,
   XHsWord64Prim a ~ XHsWord64Prim b,
   XHsInt64Prim a ~ XHsInt64Prim b,
   XHsWordPrim a ~ XHsWordPrim b,
   XHsIntPrim a ~ XHsIntPrim b,
   XHsInt a ~ XHsInt b,
   XHsStringPrim a ~ XHsStringPrim b,
   XHsString a ~ XHsString b,
   XHsCharPrim a ~ XHsCharPrim b,
   XHsChar a ~ XHsChar b,
   XXLit a ~ XXLit b)

-- ----------------------------------------------------------------------

-- | Provide a summary constraint that gives all am Outputable constraint to
-- extension points needing one
type OutputableX p =
  ( Outputable (XXPat p)
  , Outputable (XXPat GhcRn)

  , Outputable (XSigPat p)
  , Outputable (XSigPat GhcRn)

  , Outputable (XXLit p)

  , Outputable (XXOverLit p)

  , Outputable (XXType p)

  , Outputable (XXABExport p)

  , Outputable (XIPBinds    p)
  , Outputable (XXHsIPBinds p)
  , Outputable (XXIPBind    p)
  , Outputable (XXIPBind    GhcRn)
  , Outputable (XXSig       p)
  , Outputable (XXFixitySig p)

  , Outputable (XExprWithTySig p)
  , Outputable (XExprWithTySig GhcRn)

  , Outputable (XAppTypeE p)
  , Outputable (XAppTypeE GhcRn)

  -- , Outputable (XXParStmtBlock (GhcPass idL) idR)
  )
-- TODO: Should OutputableX be included in OutputableBndrId?

-- ----------------------------------------------------------------------

--
type DataId p =
  ( Data p

  , ForallXHsLit Data p
  , ForallXPat   Data p

  -- Th following GhcRn constraints should go away once TTG is fully implemented
  , ForallXPat     Data GhcRn
  , ForallXType    Data GhcRn
  , ForallXExpr    Data GhcRn
  , ForallXTupArg  Data GhcRn
  , ForallXSplice  Data GhcRn
  , ForallXBracket Data GhcRn
  , ForallXCmdTop  Data GhcRn
  , ForallXCmd     Data GhcRn

  , ForallXOverLit           Data p
  , ForallXType              Data p
  , ForallXTyVarBndr         Data p
  , ForallXAppType           Data p
  , ForallXFieldOcc          Data p
  , ForallXAmbiguousFieldOcc Data p

  , ForallXExpr      Data p
  , ForallXTupArg    Data p
  , ForallXSplice    Data p
  , ForallXBracket   Data p
  , ForallXCmdTop    Data p
  , ForallXCmd       Data p
  , ForallXABExport  Data p
  , ForallXHsIPBinds Data p
  , ForallXIPBind    Data p
  , ForallXSig       Data p
  , ForallXFixitySig Data p

  , Data (NameOrRdrName (IdP p))

  , Data (IdP p)
  , Data (PostRn p (IdP p))
  , Data (PostRn p (Located Name))
  , Data (PostRn p Bool)
  , Data (PostRn p Fixity)
  , Data (PostRn p NameSet)
  , Data (PostRn p [Name])

  , Data (PostTc p (IdP p))
  , Data (PostTc p Coercion)
  , Data (PostTc p ConLike)
  , Data (PostTc p HsWrapper)
  , Data (PostTc p Type)
  , Data (PostTc p [ConLike])
  , Data (PostTc p [Type])
  )

type DataIdLR pL pR =
  ( DataId pL
  , DataId pR

  , ForallXHsLocalBindsLR Data pL pR
  , ForallXHsLocalBindsLR Data pL pL
  , ForallXHsLocalBindsLR Data pR pR

  , ForallXValBindsLR     Data pL pR
  , ForallXValBindsLR     Data pL pL
  , ForallXValBindsLR     Data pR pR

  , ForallXHsBindsLR      Data pL pR
  , ForallXHsBindsLR      Data pL pL
  , ForallXHsBindsLR      Data pR pR

  , ForallXPatSynBind     Data pL pR
  , ForallXPatSynBind     Data pL pL
  , ForallXPatSynBind     Data pR pR
  -- , ForallXPatSynBind     Data GhcPs GhcRn
  -- , ForallXPatSynBind     Data GhcRn GhcRn

  , ForallXParStmtBlock   Data pL pR
  , ForallXParStmtBlock   Data pL pL
  , ForallXParStmtBlock   Data pR pR

  , ForallXParStmtBlock Data GhcRn GhcRn
  )

-- |Constraint type to bundle up the requirement for 'OutputableBndr' on both
-- the @id@ and the 'NameOrRdrName' type for it
type OutputableBndrId id =
  ( OutputableBndr (NameOrRdrName (IdP id))
  , OutputableBndr (IdP id)
  , OutputableX id
  )
