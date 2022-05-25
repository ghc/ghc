module Settings.Builders.GhcPkg (ghcPkgBuilderArgs) where

import Settings.Builders.Common

ghcPkgBuilderArgs :: Args
ghcPkgBuilderArgs = mconcat
    [ builder (GhcPkg Init) ? do
        stage     <- getStage
        pkgDb     <- expr $ packageDbPath stage
        -- Confusingly calls recache rather than init because shake "creates"
        -- the package db by virtue of creating the path to it, so we just recache
        -- to create the package.cache file.
        mconcat [ use_db pkgDb, arg "recache" ]

    , builder (GhcPkg Copy) ? do
        verbosity <- expr getVerbosity
        stage     <- getStage
        pkgDb     <- expr $ packageDbPath stage
        mconcat [ use_db pkgDb
                , arg "register"
                , verbosity < Verbose ? arg "-v0"
                ]
    , builder (GhcPkg Unregister) ? do
        verbosity <- expr getVerbosity
        stage     <- getStage
        pkgDb     <- expr $ packageDbPath stage
        mconcat [ use_db pkgDb
                , arg "unregister"
                , arg "--force"
                , verbosity < Verbose ? arg "-v0"
                ]
    , builder (GhcPkg Update) ? do
        verbosity <- expr getVerbosity
        context   <- getContext
        config    <- expr $ pkgInplaceConfig context
        stage     <- getStage
        pkgDb     <- expr $ packageDbPath stage
        mconcat [ notM stage0 ? use_db pkgDb
                , arg "update"
                , arg "--force"
                , verbosity < Verbose ? arg "-v0"
                , bootPackageDatabaseArgs
                , arg config ] ]
    where
        use_db db = mconcat
            -- We use ghc-pkg's --global-package-db to manipulate our databases.
            -- We can't use --package-db (at least with stage0's ghc-pkg)
            -- because units in stage0's global package db would be in scope and
            -- ghc-pkg would disallow us the register a second "rts" unit in our
            -- database.
            --
            -- However ghc-pkg uses the path to the global package db to find
            -- the compiler "settings" file... So when it finds our newly
            -- generated settings file in _build/stageN, it may crash if it
            -- isn't the format it expects (#17601).
            --
            -- By chance, ghc-pkg only needs the "settings" file to query the
            -- arch/os to generate the path to the user package db, which we
            -- don't need.  So we disable it below to avoid failures.
            [ arg "--no-user-package-db"
            , arg "--global-package-db"
            , arg db
            ]

