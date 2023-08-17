module Rules.Codes
  ( codesRules
  ) where

import Base
import Packages ( programPath, lintCodes )
import Settings.Program ( programContext )

data Usage
  = Used
  | Outdated

describeUsage :: Usage -> String
describeUsage Used     = "used"
describeUsage Outdated = "outdated"

usageArg :: Usage -> String
usageArg Used     = "list"
usageArg Outdated = "outdated"

codesRules :: Rules ()
codesRules = do
  "codes:used"     ~> codes Used
  "codes:outdated" ~> codes Outdated
  "codes"          ~> codes Used

codes :: Usage -> Action ()
codes usage = do
  let stage = Stage1 -- ?
  codesProgram <- programPath =<< programContext stage lintCodes
  need [ codesProgram ]
  ghcLibDir <- stageLibPath stage
  let args = [ usageArg usage, ghcLibDir ]
      cmdLine = unwords ( codesProgram : args )
  putBuild $ "| Computing " ++ describeUsage usage ++ " diagnostic codes."
  putBuild $ "| " <> cmdLine
  cmd_ cmdLine
