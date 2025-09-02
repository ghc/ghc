# Test that GHC links to the C++ standard library as expected
# when the system-cxx-std-lib package is used.
test('T20010', req_c, makefile_test, [])
test('T20010-ghci', [req_c, extra_files(['T20010_c.cpp', 'T20010.hs']),
                     # this test is broken for dynamic builds.  However, windows
                     # is dynamic, but not really, and works significanlty
                     # different from linux and darwin to not be broken when
                     # ghc claims to be dynamic.
                     unless(ghc_dynamic() and not opsys('mingw32'), expect_broken(20706))],
     makefile_test, [])
