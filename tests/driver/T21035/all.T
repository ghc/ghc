# This test checks that supplied object files are usable to run TH splices from external packages.
# Rather than necessarily relying on shared objects.
#
# We could make this test work for static ghc as well but it's a bit fiddly so
# we have the unless(ghc_dynamic(), skip) clause.

test('T21035', [unless(ghc_dynamic(), skip)
               , grep_errmsg("-Wmissed-extra-shared-lib")
               ,extra_files(["cat-hsdep-info.sh", "hsdep", "Main.hs", "M.hs"])], makefile_test, [])
