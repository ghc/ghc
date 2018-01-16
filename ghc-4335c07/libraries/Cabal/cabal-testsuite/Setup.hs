import Distribution.Simple
main :: IO ()
main = defaultMain

-- Although this looks like the Simple build type, it is in fact vital that
-- we use this Setup.hs because we need to compile against the very same
-- version of the Cabal library that the test suite will be compiled
-- against.  When this happens, it will mean that we'll be able to
-- read the LocalBuildInfo of our build environment, which we will
-- subsequently use to make decisions about PATHs etc.  Important!
