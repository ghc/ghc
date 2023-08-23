module Settings.Builders.CodesUtil (codesUtilBuilderArgs) where

import Settings.Builders.Common

codesUtilBuilderArgs :: Args
codesUtilBuilderArgs = do
  stage <- getStage
  libDir <- expr $ stageLibPath stage
  dbPath <- expr $ packageDbPath (PackageDbLoc stage Final)
  mconcat $
    [ builder (CodesUtil Used) ?  arg "list"
    , builder (CodesUtil Outdated) ? arg "outdated"
    , builder CodesUtil ? mconcat
                            [ arg dbPath
                            , arg "/nix/store/wvb9vgnlyzyxp2adn06668ghanhpgskm-ghc-9.6.2/lib/ghc-9.6.2/lib" ]]
