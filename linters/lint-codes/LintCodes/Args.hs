module LintCodes.Args
  ( Mode(..)
  , parseArgs
  )
  where

-- lint-codes
import LintCodes.Static
  ( LibDir(..) )

--------------------------------------------------------------------------------

-- | Mode in which to run the 'lint-codes' executable.
data Mode
  -- | Run the 'lint-codes' test, checking:
  --
  --   1. all non-outdated 'GhcDiagnosticCode' equations are statically used;
  --   2. all outdated 'GhcDiagnosticCode' equations are statically unused;
  --   3. all statically used diagnostic codes are covered by the testsuite.
  = Test
  -- | List all statically used diagnostic codes.
  | List
  -- | List outdated diagnostic codes.
  | Outdated

parseArgs :: [String] -> (Mode, Maybe LibDir)
parseArgs args
  | not (any isHelp args)
  , mode_arg : rest <- args
  = ( parseMode mode_arg, parseMbLibDir rest )
  | otherwise
  = error $ errorMsgWithHeader lintCodesHeader

parseMode :: String -> Mode
parseMode "test"     = Test
parseMode "list"     = List
parseMode "outdated" = Outdated
parseMode mode =
  error $ errorMsgWithHeader
    "Invalid mode of operation '" ++ mode ++ "'."

isHelp :: String -> Bool
isHelp "help"   = True
isHelp "-h"     = True
isHelp "--help" = True
isHelp _        = False


parseMbLibDir :: [String] -> Maybe LibDir
parseMbLibDir [] = Nothing
parseMbLibDir (fp:_) = Just $ LibDir { libDir = fp }

lintCodesHeader :: String
lintCodesHeader = "lint-codes - GHC diagnostic code coverage tool"

errorMsgWithHeader :: String -> String
errorMsgWithHeader header = unlines
  [ header
  , ""
  , "Usage: lint-codes (test|list|outdated) [libdir]"
  , ""
  , "  - Use 'test' to check consistency and coverage of GHC diagnostic codes"
  , "      (must be inside a GHC Git tree)."
  , "  - Use 'list' to list all diagnostic codes emitted by GHC."
  , "  - Use 'outdated' to list outdated diagnostic codes."
  , ""
  , ""
  , "If you see an error of the form:"
  , "  lint-codes: Missing file: test/lib/settings"
  , "It likely means you are passing an incorrect libdir."
  , "You can query the libdir for the GHC you are using with 'ghc --print-libdir'."
  ]
