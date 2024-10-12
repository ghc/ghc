# This tests for correct recompilation when a package version changes.
# See the patch
#   "Fix a recompilation checking bug when a package dependency changes"

test('recomp007', [ extra_files(['Setup.hs', 'a1/', 'a2/', 'b/'])
                  , when(fast(), skip)
                  , normalise_slashes
                  ], makefile_test, [])
