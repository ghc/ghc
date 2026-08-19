{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- for the XBF* equalities on BooleanFormulaDefault

{-# OPTIONS_GHC -Wno-orphans #-}
-- The above is required because this module owns the GhcPass instantiation of
-- the Trees-that-Grow extension points, while both the type families and
-- GhcPass are defined elsewhere:
--   * XRec, Anno
--   * XMultiLineDocString, XNestedDocString, XGeneratedDocString, XXHsDocString
--   * UnXRec, MapXRec
--   * BooleanFormulaDefault

module GHC.Hs.Extension.Instances where

import GHC.Prelude

import GHC.Hs.Extension.Pass
import GHC.Types.SrcLoc (GenLocated(..), SrcSpan, unLoc)
import Language.Haskell.Syntax.BooleanFormula (BooleanFormulaDefault(..))
import Language.Haskell.Syntax.Doc (HsDocString, HsDocStringChunk)
import Language.Haskell.Syntax.Extension

-- See Note [XRec and Anno in the AST] in GHC.Parser.Annotation
type instance XRec (GhcPass p) a = XRecGhc a

-- (XRecGhc tree) wraps `tree` in a GHC-specific,
-- but pass-independent, source location
type XRecGhc a = GenLocated (Anno a) a

type instance Anno (HsDocString (GhcPass _))  = SrcSpan
type instance Anno HsDocStringChunk           = SrcSpan

type instance XMultiLineDocString (GhcPass _) = NoExtField
type instance XNestedDocString    (GhcPass _) = NoExtField
type instance XGeneratedDocString (GhcPass _) = NoExtField
type instance XXHsDocString       (GhcPass _) = DataConCantHappen

instance UnXRec (GhcPass p) where
  unXRec = unLoc
instance MapXRec (GhcPass p) where
  mapXRec = fmap

-- The XBFAnd/XBFOr equations live in GHC.Data.BooleanFormula, which this module
-- cannot import, so the two the body needs are taken as constraints. Callers that
-- can see the equations discharge them by reduction.
instance ( XBFAnd (GhcPass p) ~ NoExtField
         , XBFOr  (GhcPass p) ~ NoExtField
         ) => BooleanFormulaDefault (GhcPass p) where
  bfAnnAnd = noExtField
  bfAnnOr  = noExtField
