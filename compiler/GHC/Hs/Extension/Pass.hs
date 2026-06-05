{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- The above is required for the type-family instances:
--   * Anno HsDocStringChunk = SrcSpan

module GHC.Hs.Extension.Pass where

import GHC.Prelude

import Data.Data
import GHC.Types.SrcLoc (GenLocated(..), SrcSpan, unLoc)
import GHC.Utils.Panic
import Language.Haskell.Syntax.Doc (HsDocString, HsDocStringChunk)
import Language.Haskell.Syntax.Extension

-- | Used as a data type index for the hsSyn AST; also serves
-- as a singleton type for Pass
data GhcPass (c :: Pass) where
  GhcPs :: GhcPass 'Parsed
  GhcRn :: GhcPass 'Renamed
  GhcTc :: GhcPass 'Typechecked

-- This really should never be entered, but the data-deriving machinery
-- needs the instance to exist.
instance Typeable p => Data (GhcPass p) where
  gunfold _ _ _ = panic "instance Data GhcPass"
  toConstr  _   = panic "instance Data GhcPass"
  dataTypeOf _  = panic "instance Data GhcPass"

data Pass = Parsed | Renamed | Typechecked
         deriving (Data)

-- Type synonyms as a shorthand for tagging
type GhcPs   = GhcPass 'Parsed      -- Output of parser
type GhcRn   = GhcPass 'Renamed     -- Output of renamer
type GhcTc   = GhcPass 'Typechecked -- Output of typechecker

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
