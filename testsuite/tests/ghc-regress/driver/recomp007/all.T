# This tests for correct recompilation when a package version changes.
# See the patch
#   "Fix a recompilation checking bug when a package dependency changes"

test('recomp007',
     [ skip_if_fast, clean_cmd('$MAKE -s clean'), normalise_slashes ],
     run_command,
     ['$MAKE -s --no-print-directory recomp007'])

