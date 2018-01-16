-- | Module containing small types
module Distribution.Parsec.Common (
    -- * Diagnostics
    PError (..),
    showPError,
    PWarning (..),
    PWarnType (..),
    showPWarning,
    -- * Field parser
    FieldParser,
    -- * Position
    Position (..),
    incPos,
    retPos,
    showPos,
    zeroPos,
    ) where

import           Prelude ()
import           Distribution.Compat.Prelude
import           System.FilePath             (normalise)
import qualified Text.Parsec                 as Parsec

-- | Parser error.
data PError = PError Position String
    deriving (Show)

-- | Type of parser warning. We do classify warnings.
--
-- Different application may decide not to show some, or have fatal behaviour on others
data PWarnType
    = PWTOther                 -- ^ Unclassified warning
    | PWTUTF                   -- ^ Invalid UTF encoding
    | PWTBoolCase              -- ^ @true@ or @false@, not @True@ or @False@
    | PWTVersionTag            -- ^ there are version with tags
    | PWTNewSyntax             -- ^ New syntax used, but no @cabal-version: >= 1.2@ specified
    | PWTOldSyntax             -- ^ Old syntax used, and @cabal-version >= 1.2@ specified
    | PWTDeprecatedField
    | PWTInvalidSubsection
    | PWTUnknownField
    | PWTUnknownSection
    | PWTTrailingFields
    | PWTExtraMainIs           -- ^ extra main-is field
    | PWTExtraTestModule       -- ^ extra test-module field
    | PWTExtraBenchmarkModule  -- ^ extra benchmark-module field
    | PWTLexNBSP
    | PWTLexBOM
    | PWTQuirkyCabalFile       -- ^ legacy cabal file that we know how to patch
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Parser warning.
data PWarning = PWarning !PWarnType !Position String
    deriving (Show)

showPWarning :: FilePath -> PWarning -> String
showPWarning fpath (PWarning _ pos msg) =
  normalise fpath ++ ":" ++ showPos pos ++ ": " ++ msg

showPError :: FilePath -> PError -> String
showPError fpath (PError pos msg) =
  normalise fpath ++ ":" ++ showPos pos ++ ": " ++ msg

-------------------------------------------------------------------------------
-- Field parser
-------------------------------------------------------------------------------

-- | Field value parsers.
type FieldParser = Parsec.Parsec String [PWarning] -- :: * -> *


-------------------------------------------------------------------------------
-- Position
-------------------------------------------------------------------------------

-- | 1-indexed row and column positions in a file.
data Position = Position
    {-# UNPACK #-}  !Int           -- row
    {-# UNPACK #-}  !Int           -- column
  deriving (Eq, Ord, Show)

-- | Shift position by n columns to the right.
incPos :: Int -> Position -> Position
incPos n (Position row col) = Position row (col + n)

-- | Shift position to beginning of next row.
retPos :: Position -> Position
retPos (Position row _col) = Position (row + 1) 1

showPos :: Position -> String
showPos (Position row col) = show row ++ ":" ++ show col

zeroPos :: Position
zeroPos = Position 0 0
