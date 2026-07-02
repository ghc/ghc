{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Data-types describing the raw and lexical docstrings of
the Haskell programming language.
-}
module Language.Haskell.Syntax.Doc
  ( HsDoc
  , WithHsDocIdentifiers(..)

  , HsDocString(..)
  -- ** Construction
  , mkGeneratedHsDocString

  , HsDocStringChunk(..)
  -- ** Construction
  , mkHsDocStringChunkUtf8ByteString
  -- ** Query
  , nullHDSC

  , HsDocStringDecorator(..)
  , LHsDoc
  , LHsDocStringChunk
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Data
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..))
import Prelude
import Language.Haskell.Syntax.Extension

-- | A docstring with the (probable) identifiers found in it.
type HsDoc (pass :: Type) = WithHsDocIdentifiers (HsDocString pass) pass

-- | Haskell Documentation String
--
-- Rich structure to support exact printing
-- The location around each chunk doesn't include the decorators
data HsDocString pass
  = MultiLineDocString
      (XMultiLineDocString pass)
      !HsDocStringDecorator
      !(NonEmpty (LHsDocStringChunk pass))
     -- ^ The first chunk is preceded by "-- <decorator>" and each following chunk is preceded by "--"
     -- Example: -- | This is a docstring for 'foo'. It is the line with the decorator '|' and is always included
     --          -- This continues that docstring and is the second element in the NonEmpty list
     --          foo :: a -> a
  | NestedDocString
      (XNestedDocString pass)
      !HsDocStringDecorator
      (LHsDocStringChunk pass)
     -- ^ The docstring is preceded by "{-<decorator>" and followed by "-}"
     -- The chunk contains balanced pairs of '{-' and '-}'
  | GeneratedDocString
      (XGeneratedDocString pass)
      HsDocStringChunk
     -- ^ A docstring generated either internally or via TH
     -- Pretty printed with the '-- |' decorator
     -- This is because it may contain unbalanced pairs of '{-' and '-}' and
     -- not form a valid 'NestedDocString'
  | XHsDocString
      !(XXHsDocString pass)

instance
  ( NFData (XMultiLineDocString pass)
  , NFData (XNestedDocString pass)
  , NFData (XGeneratedDocString pass)
  , NFData (XXHsDocString pass)
  , NFData (LHsDocStringChunk pass)
  ) => NFData (HsDocString pass) where
  rnf (MultiLineDocString x a b) = rnf x `seq` rnf a `seq` rnf b
  rnf (NestedDocString x a b) = rnf x `seq` rnf a `seq` rnf b
  rnf (GeneratedDocString x a) = rnf x `seq` rnf a
  rnf (XHsDocString x) = rnf x

mkGeneratedHsDocString :: XGeneratedDocString p -> HsDocStringChunk -> HsDocString p
mkGeneratedHsDocString x = GeneratedDocString x

type LHsDoc pass = XRec pass (HsDoc pass)

type LHsDocStringChunk pass = XRec pass HsDocStringChunk

-- | A contiguous chunk of documentation
newtype HsDocStringChunk = HsDocStringChunk ByteString
  deriving stock (Eq,Ord,Data, Show)
  deriving newtype (NFData)

-- | Create a 'HsDocStringChunk' from a UTF8-encoded 'ByteString'.
mkHsDocStringChunkUtf8ByteString :: ByteString -> HsDocStringChunk
mkHsDocStringChunkUtf8ByteString = HsDocStringChunk

nullHDSC :: HsDocStringChunk -> Bool
nullHDSC (HsDocStringChunk bs) = BS.null bs

data HsDocStringDecorator
  = HsDocStringNext          -- ^ '|' is the decorator
  | HsDocStringPrevious      -- ^ '^' is the decorator
  | HsDocStringNamed !String -- ^ '$<string>' is the decorator
  | HsDocStringGroup !Int    -- ^ The decorator is the given number of '*'s
  deriving (Eq, Ord, Show, Data)

instance NFData HsDocStringDecorator where
  rnf HsDocStringNext = ()
  rnf HsDocStringPrevious = ()
  rnf (HsDocStringNamed x) = rnf x
  rnf (HsDocStringGroup x) = rnf x

-- | Annotate a value with the probable identifiers found in it
-- These will be used by haddock to generate links.
--
-- The identifiers are bundled along with their location in the source file.
-- This is useful for tooling to know exactly where they originate.
--
-- This type is currently used in two places - for regular documentation comments,
-- with 'a' set to 'HsDocString', and for adding identifier information to
-- warnings, where 'a' is 'StringLiteral'
data WithHsDocIdentifiers a pass = WithHsDocIdentifiers
  { hsDocString      :: !a
  , hsDocIdentifiers :: ![LIdP pass]
  }

instance (UnXRec pass, NFData (IdP pass), NFData a) => NFData (WithHsDocIdentifiers a pass) where
  rnf (WithHsDocIdentifiers d i) = rnf d `seq` rnf (map (unXRec @pass) i)
