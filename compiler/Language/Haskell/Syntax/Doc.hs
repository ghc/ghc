{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Eq XOverlapMode, NFData OverlapMode

{-
## Migrate and restructure `LHsDoc`

[X] 1. Create a new **`L.H.S.Doc`** module and move the following into it.
[X] 2. Move `HsDoc` *(no change)*
[X] 3. Move `HsDocStringChunk` *(no change)*
[X] 4. Move `HsDocStringDecorator` *(no change)*
[_] 5. Move `LHsDoc p` as `XRec p (HsDoc p)`
[X] 6. Move `WithHsDocIdentifiers p` as `Located (IdP p) as LIdP p`
[_] 7. Move `HsDocString`, adding TTG parameter and extension point
[X] 8. Move `LHsDocStringChunk = Located HsDocStringChunk` as `type LHsDocStringChunk pass = XRec pass HsDocStringChunk`
[ ] 9. Add `type instance Anno HsDocStringChunk = SrcSpan`
-}

{- |
Data-types describing the raw and lexical docstrings of
the Haskell programming language.
-}
module Language.Haskell.Syntax.Doc
  ( HsDoc
  , WithHsDocIdentifiers(..)

  , HsDocString(..)
  -- ** Construcction
  , mkGeneratedHsDocString

  , HsDocStringChunk(..)
  -- ** Construction
  , mkHsDocStringChunk
  , mkHsDocStringChunkUtf8ByteString
  -- ** Deconstruction
  , unpackHDSC
  -- ** Query
  , nullHDSC

  , HsDocStringDecorator(..)
  , LHsDoc
  , LHsDocStringChunk
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as SBS
import Data.Data
import Data.Kind (Type)
import Data.Eq
import Data.List.NonEmpty (NonEmpty(..))
import Data.Function
import Prelude
import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.UTF8

-- | A docstring with the (probable) identifiers found in it.
type HsDoc (pass :: Type) = WithHsDocIdentifiers (HsDocString pass) pass

-- | Haskell Documentation String
--
-- Rich structure to support exact printing
-- The location around each chunk doesn't include the decorators
data HsDocString pass
  = MultiLineDocString
      !(XMultiLineDocString pass)
      !HsDocStringDecorator
      !(NonEmpty (LHsDocStringChunk pass))
     -- ^ The first chunk is preceded by "-- <decorator>" and each following chunk is preceded by "--"
     -- Example: -- | This is a docstring for 'foo'. It is the line with the decorator '|' and is always included
     --          -- This continues that docstring and is the second element in the NonEmpty list
     --          foo :: a -> a
  | NestedDocString
      !(XNestedDocString pass)
      !HsDocStringDecorator
      (LHsDocStringChunk pass)
     -- ^ The docstring is preceded by "{-<decorator>" and followed by "-}"
     -- The chunk contains balanced pairs of '{-' and '-}'
  | GeneratedDocString
      !(XGeneratedDocString pass)
      HsDocStringChunk
     -- ^ A docstring generated either internally or via TH
     -- Pretty printed with the '-- |' decorator
     -- This is because it may contain unbalanced pairs of '{-' and '-}' and
     -- not form a valid 'NestedDocString'
  | XHsDocString
      !(XXHsDocString pass)
{-
deriving stock instance (
  Eq (XMultiLineDocString pass),
  Eq (XNestedDocString pass),
  Eq (XGeneratedDocString pass),
  Eq (XXHsDocString pass),
  Eq (XRec pass HsDocStringChunk),
  Typeable pass
  ) => Eq (HsDocString pass)

deriving stock instance (
  Show (XMultiLineDocString pass),
  Show (XNestedDocString pass),
  Show (XGeneratedDocString pass),
  Show (XXHsDocString pass),
  Show (XRec pass HsDocStringChunk),
  Typeable pass
  ) => Show (HsDocString pass)

instance {-# OVERLAPPABLE #-} (
  NFData (XMultiLineDocString pass),
  NFData (XNestedDocString pass),
  NFData (XGeneratedDocString pass),
  NFData (XXHsDocString pass),
  NFData (XRec pass HsDocStringChunk)
  ) => NFData (HsDocString pass) where
    rnf = \case
       MultiLineDocString x a b -> rnf x `seq` rnf a `seq` rnf b
       NestedDocString    x a b -> rnf x `seq` rnf a `seq` rnf b
       GeneratedDocString x a   -> rnf x `seq` rnf a
       XHsDocString       x     -> rnf x
-}
mkGeneratedHsDocString :: XGeneratedDocString p -> String -> HsDocString p
mkGeneratedHsDocString x = GeneratedDocString x . mkHsDocStringChunk

type LHsDoc pass = XRec pass (HsDoc pass)
--type LHsDoc pass = Located (HsDoc pass)
--type LIdP p = XRec p (IdP p)

type LHsDocStringChunk pass = XRec pass HsDocStringChunk

-- | A contiguous chunk of documentation
newtype HsDocStringChunk = HsDocStringChunk TextUTF8
  deriving stock (Eq,Ord,Data, Show)
  deriving newtype (NFData)

mkHsDocStringChunk :: String -> HsDocStringChunk
mkHsDocStringChunk = HsDocStringChunk . encodeUTF8

mkHsDocStringChunkUtf8ByteString :: ByteString -> HsDocStringChunk
mkHsDocStringChunkUtf8ByteString =
  HsDocStringChunk . unsafeFromShortByteString . SBS.toShort

unpackHDSC :: HsDocStringChunk -> String
unpackHDSC (HsDocStringChunk bs) = decodeUTF8 bs

nullHDSC :: HsDocStringChunk -> Bool
nullHDSC (HsDocStringChunk bs) = headUTF8 bs == Nothing

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
