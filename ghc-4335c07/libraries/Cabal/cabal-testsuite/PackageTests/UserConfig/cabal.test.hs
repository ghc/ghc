import Test.Cabal.Prelude
main = cabalTest $ do
    workdir <- fmap testWorkDir getTestEnv
    let conf = workdir </> "cabal-config"
    cabalG ["--config-file", conf] "user-config" ["init"]
    shouldExist conf
    fails $ cabalG ["--config-file", workdir </> "cabal-config"] "user-config" ["init"]
    cabalG ["--config-file", conf] "user-config" ["-f", "init"]
    shouldExist conf
    let conf2 = workdir </> "cabal-config2"
    withEnv [("CABAL_CONFIG", Just conf2)] $ do
        cabal "user-config" ["init"]
        shouldExist conf2
