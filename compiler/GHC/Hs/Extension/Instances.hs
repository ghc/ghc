{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- The above is required for the type-family instances:
--   * Anno HsDocStringChunk = SrcSpan

module GHC.Hs.Extension.Instances where

import GHC.Hs.Extension.Pass
import GHC.Prelude.Basic (fmap)
import GHC.Types.SrcLoc (GenLocated(..), SrcSpan, unLoc)
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
-- instance WrapXRec (GhcPass p) a where
--   wrapXRec = noLocA
