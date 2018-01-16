import Test.Cabal.Prelude
-- Test --benchmark-option(s) flags on ./Setup bench
main = setupAndCabalTest $ do
    setup_build ["--enable-benchmarks"]
    setup "bench" [ "--benchmark-options=1 2 3" ]
    setup "bench" [ "--benchmark-option=1"
                  , "--benchmark-option=2"
                  , "--benchmark-option=3"
                  ]
