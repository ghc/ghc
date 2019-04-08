module Settings.Builders.GhcPkg (ghcPkgBuilderArgs) where

import Settings.Builders.Common

ghcPkgBuilderArgs :: Args
ghcPkgBuilderArgs = mconcat
    [ builder (GhcPkg Copy) ? do
        verbosity <- expr getVerbosity
        stage     <- getStage
        pkgDb     <- expr $ packageDbPath stage
        targetArch <- getSetting TargetArch
        targetOS  <- getSetting TargetOs
        version <- getSetting ProjectVersion
        mconcat
          [ arg "--global-package-db"
          , arg pkgDb
          , -- this flag is new in this version
            notStage0 ? mconcat
              [ arg "--default-user-package-db-subdir"
              , arg (intercalate "-" [targetArch, targetOS, version])
              ]
          , arg "register"
          , verbosity < Chatty ? arg "-v0"
          ]
    , builder (GhcPkg Unregister) ? do
        verbosity <- expr getVerbosity
        stage     <- getStage
        pkgDb     <- expr $ packageDbPath stage
        targetArch <- getSetting TargetArch
        targetOS  <- getSetting TargetOs
        version <- getSetting ProjectVersion
        mconcat [ arg "--global-package-db"
                , arg pkgDb
                , -- this flag is new in this version
                  notStage0 ? mconcat
                    [ arg "--default-user-package-db-subdir"
                    , arg (intercalate "-" [targetArch, targetOS, version])
                    ]
                , arg "unregister"
                , arg "--force"
                , verbosity < Chatty ? arg "-v0"
                ]
    , builder (GhcPkg Update) ? do
        verbosity <- expr getVerbosity
        context   <- getContext
        config    <- expr $ pkgInplaceConfig context
        stage     <- getStage
        pkgDb     <- expr $ packageDbPath stage
        targetArch <- getSetting TargetArch
        targetOS  <- getSetting TargetOs
        version <- getSetting ProjectVersion
        mconcat
          [ -- The wrapper scripts provide this for stage0, but the freshly
            -- built tools are unwrapped for the later stages.
            notStage0 ? mconcat
              [ arg "--global-package-db"
              , arg pkgDb
              , -- Additionally, this flag is new in this version
                arg "--default-user-package-db-subdir"
              , arg (intercalate "-" [targetArch, targetOS, version])
              ]
          , arg "update"
          , arg "--force"
          , verbosity < Chatty ? arg "-v0"
          , bootPackageDatabaseArgs
          , arg config
          ]
    ]
