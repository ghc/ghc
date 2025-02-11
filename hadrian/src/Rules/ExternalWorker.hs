module Rules.ExternalWorker (externalWorkerRules) where

import Base

-- | TODO: Drop code duplication
externalWorkerRules :: Rules ()
externalWorkerRules = do
    root <- buildRootRules

    root -/- stageString stage0InTree -/- "external"  -/- "server" %> \serverPath ->
      buildServer serverPath

    -- Rules for programs that are actually built by hadrian.
    root -/- stageString stage0InTree -/- "external"  -/- "client" %> \clientPath ->
      buildClient clientPath

buildClient :: FilePath -> Action ()
buildClient clientPath =
  copyFile ("worker" -/- "client") clientPath

buildServer :: FilePath -> Action ()
buildServer serverPath =
  copyFile ("worker" -/- "server") serverPath


