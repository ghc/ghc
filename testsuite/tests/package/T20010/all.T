# Test that GHC links to the C++ standard library as expected
# when the system-cxx-std-lib package is used.
test('T20010', normal, makefile_test, [])
test('T20010-ghci', extra_files(['T20010_c.cpp', 'T20010.hs']), makefile_test, [])
