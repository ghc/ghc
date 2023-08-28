module LintCodes.Coverage
  ( getCoveredCodes )
  where

-- containers
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( fromList )

-- ghc
import GHC.Types.Error
  ( DiagnosticCode(..) )

-- process
import System.Process
  ( readProcess )

--------------------------------------------------------------------------------
-- Diagnostic code coverage from testsuite .stdout and .stderr files

-- | Get all diagnostic codes that appear in testsuite .stdout and .stderr
-- files.
getCoveredCodes :: IO (Set DiagnosticCode)
getCoveredCodes =
  -- Run git grep on .stdout and .stderr files in the testsuite subfolder.
  do { codes <- lines
            <$> readProcess "git"
                [ "grep", "-Eoh", codeRegex
                        -- -oh: only show the match, and omit the filename.
                , "--", ":/testsuite/*.stdout", ":/testsuite/*.stderr"
                , ":!*/codes.stdout" -- Don't include the output of this test itself.
                ] ""
     ; return $ Set.fromList $ map parseCode codes }

-- | Regular expression to parse a diagnostic code.
codeRegex :: String
codeRegex = "\\[[A-Za-z]+-[0-9]+\\]"

-- | Turn a string that matches the 'codeRegex' regular expression
-- into its corresponding 'DiagnosticCode'.
parseCode :: String -> DiagnosticCode
parseCode c =
  case break (== '-') $ drop 1 c of
    (ns, rest) ->
      DiagnosticCode ns ( read $ init $ drop 1 rest )
