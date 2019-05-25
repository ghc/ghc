module OutputOptions where

import GhcPrelude

import qualified PprColour as Col
import Util ( OverridingBool )

-- Output style options
data OutputOptions = OutputOptions
  { outputOptions_pprUserLength :: Int
  , outputOptions_pprCols :: Int
  , outputOptions_useUnicode :: Bool
  , outputOptions_useColor :: OverridingBool
  , outputOptions_canUseColor :: Bool
  , outputOptions_colScheme :: Col.Scheme

  , outputOptions_hasPprDebug :: Bool
  , outputOptions_hasNoDebugOutput :: Bool
  }

class HasOutputOptions a where
  getOutputOptions :: a -> OutputOptions

hasPprDebug :: HasOutputOptions a => a -> Bool
hasPprDebug = outputOptions_hasPprDebug . getOutputOptions

hasNoDebugOutput :: HasOutputOptions a => a -> Bool
hasNoDebugOutput = outputOptions_hasNoDebugOutput . getOutputOptions
