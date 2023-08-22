module Settings.Builders.CodesUtil (codesUtilBuilderArgs) where

import Settings.Builders.Common

codesUtilBuilderArgs :: Args
codesUtilBuilderArgs = do
  stage <- getStage
  libDir <- expr $ stageLibPath stage
  mconcat $
    [ builder (CodesUtil Used) ?  arg "list"
    , builder (CodesUtil Outdated) ? arg "outdated"
    , builder CodesUtil ? arg libDir ]
