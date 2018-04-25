import Test.Cabal.Prelude hiding (cabal)
import qualified Test.Cabal.Prelude as P
-- See #4332, dep solving output is not deterministic
main = cabalTest . recordMode DoNotRecord $ do
    fails $ cabal "new-build" []
    cabal "new-build" ["--allow-older"]
    fails $ cabal "new-build" ["--allow-older=baz,quux"]
    cabal "new-build" ["--allow-older=base", "--allow-older=baz,quux"]
    cabal "new-build" ["--allow-older=bar", "--allow-older=base,baz"
                      ,"--allow-older=quux"]
    fails $ cabal "new-build" ["--enable-tests"]
    cabal "new-build" ["--enable-tests", "--allow-older"]
    fails $ cabal "new-build" ["--enable-benchmarks"]
    cabal "new-build" ["--enable-benchmarks", "--allow-older"]
    fails $ cabal "new-build" ["--enable-benchmarks", "--enable-tests"]
    cabal "new-build" ["--enable-benchmarks", "--enable-tests"
                      ,"--allow-older"]
    fails $ cabal "new-build" ["--allow-older=Foo:base"]
    fails $ cabal "new-build" ["--allow-older=Foo:base"
                                   ,"--enable-tests", "--enable-benchmarks"]
    cabal "new-build" ["--allow-older=AllowOlder:base"]
    cabal "new-build" ["--allow-older=AllowOlder:base"
                      ,"--allow-older=Foo:base"]
    cabal "new-build" ["--allow-older=AllowOlder:base"
                      ,"--allow-older=Foo:base"
                      ,"--enable-tests", "--enable-benchmarks"]
  where
    cabal cmd args = P.cabal cmd ("--dry-run" : args)
