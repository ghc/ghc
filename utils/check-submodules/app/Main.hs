module Main (main) where

import CheckVersions
import CheckTags
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["check-versions"] -> checkVersions
    ["check-tags"] -> checkTags
    ["summarize"] -> summarize
    ["email"] -> maintainerEmails >>= putStrLn
    _ -> fail "invalid mode (valid modes: check-versions, check-tags, summarize, email)"
