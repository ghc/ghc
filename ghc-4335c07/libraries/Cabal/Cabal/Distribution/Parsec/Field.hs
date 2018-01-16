{-# LANGUAGE DeriveFunctor #-}
-- | Cabal-like file AST types: 'Field', 'Section' etc
--
-- These types are parametrized by an annotation.
module Distribution.Parsec.Field (
    -- * Cabal file
    Field (..),
    fieldName,
    fieldAnn,
    fieldUniverse,
    FieldLine (..),
    SectionArg (..),
    sectionArgAnn,
    -- * Name
    FieldName,
    Name (..),
    mkName,
    getName,
    nameAnn,
    ) where

import           Prelude ()
import           Distribution.Compat.Prelude
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as B
import qualified Data.Char                   as Char

-------------------------------------------------------------------------------
-- Cabal file
-------------------------------------------------------------------------------

-- | A Cabal-like file consists of a series of fields (@foo: bar@) and sections (@library ...@).
data Field ann
    = Field   !(Name ann) [FieldLine ann]
    | Section !(Name ann) [SectionArg ann] [Field ann]
  deriving (Eq, Show, Functor)

-- | Section of field name
fieldName :: Field ann -> Name ann
fieldName (Field n _ )    = n
fieldName (Section n _ _) = n

fieldAnn :: Field ann -> ann
fieldAnn = nameAnn . fieldName

-- | All transitive descendands of 'Field', including itself.
--
-- /Note:/ the resulting list is never empty.
--
fieldUniverse :: Field ann -> [Field ann]
fieldUniverse f@(Section _ _ fs) = f : concatMap fieldUniverse fs
fieldUniverse f@(Field _ _)      = [f]

-- | A line of text representing the value of a field from a Cabal file.
-- A field may contain multiple lines.
--
-- /Invariant:/ 'ByteString' has no newlines.
data FieldLine ann  = FieldLine  !ann !ByteString
  deriving (Eq, Show, Functor)

-- | Section arguments, e.g. name of the library
data SectionArg ann
    = SecArgName  !ann !ByteString
      -- ^ identifier, or omething which loos like number. Also many dot numbers, i.e. "7.6.3"
    | SecArgStr   !ann !ByteString
      -- ^ quoted string
    | SecArgOther !ann !ByteString
      -- ^ everything else, mm. operators (e.g. in if-section conditionals)
  deriving (Eq, Show, Functor)

-- | Extract annotation from 'SectionArg'.
sectionArgAnn :: SectionArg ann -> ann
sectionArgAnn (SecArgName ann _)  = ann
sectionArgAnn (SecArgStr ann _)   = ann
sectionArgAnn (SecArgOther ann _) = ann

-------------------------------------------------------------------------------
-- Name
-------------------------------------------------------------------------------

type FieldName = ByteString

-- | A field name.
--
-- /Invariant/: 'ByteString' is lower-case ASCII.
data Name ann  = Name       !ann !FieldName
  deriving (Eq, Show, Functor)

mkName :: ann -> FieldName -> Name ann
mkName ann bs = Name ann (B.map Char.toLower bs)

getName :: Name ann -> FieldName
getName (Name _ bs) = bs

nameAnn :: Name ann -> ann
nameAnn (Name ann _) = ann
