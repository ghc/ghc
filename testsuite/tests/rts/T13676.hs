-- T13676 test driver.
-- Tests that the command dumped by the RTS into the stats file is properly escaped.

module T13676_Driver (GhcPath(GhcPath), test_t13676) where

import Control.Monad
import Data.Maybe

import System.Exit
import System.Process
import System.FilePath

-- This expression contains quotation marks and spaces which must be escaped.
expr :: String
expr = "'$' == '\\x0024'"

-- Check that evaluation of expr succeeds.
check_output :: String -> IO ()
check_output out =
  unless (lines out == ["True"]) $
    exitWith (ExitFailure 13)

-- A name for the .t file.
tfilename :: String
tfilename = "T13676.t"

newtype GhcPath = GhcPath FilePath

-- GHC arguments for the initial invocation.
initial_cmd_args :: [String]
initial_cmd_args = ["-e", expr, "+RTS", "-t" ++ tfilename]

test_t13676 :: GhcPath -> IO ()
test_t13676 (GhcPath ghcPath) = do
  initial_out <- readCreateProcess (proc ghcPath initial_cmd_args) ""
  check_output initial_out
  tfile_content <- readFile tfilename
  dumped_cmd <-
    case listToMaybe (lines tfile_content) of
      Nothing -> exitWith (ExitFailure 14)
      Just str -> return str
  secondary_out <- readCreateProcess (shell dumped_cmd) ""
  check_output secondary_out
