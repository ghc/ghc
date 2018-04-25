import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    withPackageDb $ do
      setup_install []
      _ <- runM "touch" ["indef/Foo.hs"]
      setup "build" []
      runExe' "exe" [] >>= assertOutputContains "fromList [(0,2),(2,4)]"

