import Test.Cabal.Prelude
main = cabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [7,8])
    withSandbox $ do
        cabal "install" ["--enable-shared"]
        env <- getTestEnv
        is_windows <- isWindows
        let sandbox_dir = testSandboxDir env
            work_dir    = testWorkDir env
            lib_dir =
                -- This is dumb but it's been this way for a long time.
                if is_windows
                    then sandbox_dir
                    else sandbox_dir </> "lib"
        gcc [ "UseLib.c"
            , "-o", work_dir </> "UseLib"
            , "-l" ++ "myforeignlib"
            , "-L" ++ lib_dir ]
        recordMode RecordAll $
            cabal "exec" ["-v0", work_dir </> "UseLib"]
