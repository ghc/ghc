{-# LANGUAGE RoleAnnotations #-}
module GHC.Hs.Doc where

{-
-- See #21592 for progress on removing this boot file.

import Language.Haskell.Textual.Location
import Language.Haskell.Textual.Documentation.String
import Data.Kind

type role WithHsDocIdentifiers representational nominal
type WithHsDocIdentifiers :: Type -> Type -> Type
data WithHsDocIdentifiers a pass

type HsDoc :: Type -> Type
type HsDoc = WithHsDocIdentifiers HsDocString

type LHsDoc :: Type -> Type
type LHsDoc pass = Located (HsDoc pass)
-}