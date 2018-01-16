import Test.Cabal.Prelude hiding (cabal)
import qualified Test.Cabal.Prelude as P
-- See #4332, dep solving output is not deterministic
main = cabalTest . recordMode DoNotRecord $ do
    fails $ cabal "new-build" []
    cabal "new-build" ["--allow-newer"]
    fails $ cabal "new-build" ["--allow-newer=baz,quux"]
    cabal "new-build" ["--allow-newer=base", "--allow-newer=baz,quux"]
    cabal "new-build" ["--allow-newer=bar", "--allow-newer=base,baz"
                      ,"--allow-newer=quux"]
    fails $ cabal "new-build" ["--enable-tests"]
    cabal "new-build" ["--enable-tests", "--allow-newer"]
    fails $ cabal "new-build" ["--enable-benchmarks"]
    cabal "new-build" ["--enable-benchmarks", "--allow-newer"]
    fails $ cabal "new-build" ["--enable-benchmarks", "--enable-tests"]
    cabal "new-build" ["--enable-benchmarks", "--enable-tests"
                      ,"--allow-newer"]
    fails $ cabal "new-build" ["--allow-newer=Foo:base"]
    fails $ cabal "new-build" ["--allow-newer=Foo:base"
                                   ,"--enable-tests", "--enable-benchmarks"]
    cabal "new-build" ["--allow-newer=AllowNewer:base"]
    cabal "new-build" ["--allow-newer=AllowNewer:base"
                      ,"--allow-newer=Foo:base"]
    cabal "new-build" ["--allow-newer=AllowNewer:base"
                      ,"--allow-newer=Foo:base"
                      ,"--enable-tests", "--enable-benchmarks"]
  where
    cabal cmd args = P.cabal cmd ("--dry-run" : args)
