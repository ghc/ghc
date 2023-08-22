module Rules.Codes
  ( codesRules
  ) where

import Base
import Builder
import Context
import Hadrian.Target
import Packages
import Utilities


describeUsage :: CodesMode -> String
describeUsage Used     = "used"
describeUsage Outdated = "outdated"

codesRules :: Rules ()
codesRules = do
  "codes:used"     ~> codes Used
  "codes:outdated" ~> codes Outdated
  "codes"          ~> codes Used

codes :: CodesMode -> Action ()
codes usage = do
  putBuild $ "| Computing " ++ describeUsage usage ++ " diagnostic codes."
  let codesContext = vanillaContext Stage1 lintCodes
  build (target codesContext (CodesUtil usage) [] [])
