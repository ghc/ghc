{-# LANGUAGE TypeApplications #-}

module Main where

import System.Random ( mkStdGen, uniformR )
import Data.Hashable ( hash )

import System.Environment ( getArgs )
import System.Exit ( ExitCode(..), die )
import System.Process ( readProcessWithExitCode )
import Data.Tuple ( swap )
import Data.List ( mapAccumL )
import Control.Monad ( when, unless )
import Text.Read ( readMaybe )

import GHC.Driver.Errors.Types ( GhcMessage )
import GHC.Driver.Errors.Ppr ()   -- instance GhcDiagnostic GhcMessage
import GHC.Types.Error
  ( numDigitsInGhcDiagnosticCode, usedDiagnosticCodes
  , retiredDiagnosticCodes, ghcDiagnosticCodeNumber )

import qualified Data.Set as S

main :: IO ()
main = do
  args <- getArgs
  num_to_gen <- case args of
    [] -> return 1
    [num_string]
      | Just num <- readMaybe num_string
      -> return num
    _ -> die "Usage: gen-error-codes [<number>]"

  let all_codes = S.fromList (usedDiagnosticCodes @GhcMessage ++ retiredDiagnosticCodes @GhcMessage)
      all_code_numbers = S.map ghcDiagnosticCodeNumber all_codes

      code_upper_bound = 10^numDigitsInGhcDiagnosticCode - 1

  (git_exit_code, git_stdout, git_stderr) <- readProcessWithExitCode "git" ["branch", "--show-current"] ""

  case git_exit_code of
    ExitFailure code -> die $ "git branch --show-current failed with exit code " ++ show code
    ExitSuccess -> return ()

  unless (null git_stderr) $
    die $ "git branch --show-current produced error output:\n" ++ git_stderr

  git_branch_name <- case lines git_stdout of
    [branch_name] -> return branch_name
    _ -> die $ "Unexpected output from git. Are you on a detached HEAD? Output follows:\n" ++ git_stdout

  when (fromIntegral (length all_codes) >= fromIntegral code_upper_bound * 0.9) $
    die $ "At least 90% of the possible codes between 1 and " ++ show code_upper_bound ++ "\n" ++
          "are used up. Please increase the upper bound by changing numDigitsInGhcDiagnosticCode\n" ++
          "in GHC.Types.Error, but be mindful not to add extra padding to existing error codes,\n" ++
          "which are already used widely on the internet. This will require some extra complexity."

  putStrLn $ "Found " ++ show (length all_codes) ++ " codes; git branch = " ++ git_branch_name

  let seed_gen = mkStdGen (hash (all_code_numbers, git_branch_name))

      gen_code (g, already_made)
        | attempt `S.member` all_code_numbers = gen_code (next_g, already_made)
        | attempt `S.member` already_made     = gen_code (next_g, already_made)
        | otherwise                           = ((next_g, S.insert attempt already_made), attempt)
        where
          (attempt, next_g) = uniformR (1, code_upper_bound) g

      new_codes :: [Int]
      (_, new_codes) =
        mapAccumL (\ g _n -> gen_code g) (seed_gen, S.empty) [1..num_to_gen]

  mapM_ print new_codes
