-- | Foreign export stubs
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
module GHC.Types.ForeignStubs (
  CStub (..),
  ForeignStubs,
  simpleCStub,
  initializerCStub,
  finalizerCStub,
  CDoc (..),
  getCDoc,
  renderCStubs,
  renderSimpleCStubs,
) where

import {-# SOURCE #-} GHC.Cmm.CLabel

import GHC.Platform
import GHC.Prelude
import GHC.Utils.Outputable
import Data.Semigroup (Semigroup (..), All (All))
import GHC.Generics (Generically (Generically), Generic)
import Data.Coerce (coerce)
import Data.List.NonEmpty (nonEmpty)
import Data.Foldable (fold)

newtype CDoc = CDoc SDoc

getCDoc :: CDoc -> SDoc
getCDoc = coerce

instance Outputable CDoc where
  ppr (CDoc d) = ppr d

instance Semigroup CDoc where
  CDoc l <> CDoc r = CDoc (l $$ r)

instance Monoid CDoc where
  mempty = CDoc empty

data CStub =
  CStub {
    header :: CDoc,
    source :: CDoc,
    initializers :: [CLabel],
    finalizers :: [CLabel],
    simple :: All
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via (Generically CStub)

simpleCStub :: SDoc -> SDoc -> CStub
simpleCStub header source = mempty {header = CDoc header, source = CDoc source}

functionCStub :: Platform -> CLabel -> SDoc -> SDoc -> CStub
functionCStub platform clbl declarations body =
  mempty {
    source,
    simple = All False
  }
  where
    source = CDoc $ vcat
      [ declarations
      , hsep [text "void", pprCLabel platform clbl, text "(void)"]
      , braces body
      ]

-- | @initializerCStub fn_nm decls body@ is a 'CStub' containing C initializer
-- function (e.g. an entry of the @.init_array@ section) named
-- @fn_nm@ with the given body and the given set of declarations.
initializerCStub :: Platform -> CLabel -> SDoc -> SDoc -> CStub
initializerCStub platform clbl declarations body =
  (functionCStub platform clbl declarations body) {initializers = [clbl]}

-- | @finalizerCStub fn_nm decls body@ is a 'CStub' containing C finalizer
-- function (e.g. an entry of the @.fini_array@ section) named
-- @fn_nm@ with the given body and the given set of declarations.
finalizerCStub :: Platform -> CLabel -> SDoc -> SDoc -> CStub
finalizerCStub platform clbl declarations body =
  (functionCStub platform clbl declarations body) {finalizers = [clbl]}

type ForeignStubs = [CStub]

renderCStubs :: [CStub] -> Maybe CStub
renderCStubs =
  fmap fold .
  nonEmpty

renderSimpleCStubs :: [CStub] -> Maybe CStub
renderSimpleCStubs =
  renderCStubs .
  filter (\ CStub {simple = All simple} -> simple)
