test('T14794a', normal, compile, [''])
test('T14794b', normal, compile, [''])
test('T14794c', normal, compile, [''])
test('T14794d', normal, compile, [''])
test('T14794e', normal, compile, [''])
test('T14794f', normal, compile, [''])

test('T9178', [], multimod_compile, ['T9178', '-Wall'])

# T9230.hs contains a tab character. Test that -Wtabs is enabled by default.
test('T9230', normal, compile, [''])

test('T10908', normal, compile, [''])
test('T10930', normal, compile, [''])
test('T10930b', normal, compile, [''])
test('T11077', normal, compile, ['-fwarn-missing-exported-signatures'])
test('T13256', normal, compile, [''])
test('T15460', normal, compile, [''])
test('PluralS', normal, compile, [''])

# T12574 Test that suggest current flag over deprecated
test('T12574',normal, compile, ['-fwarn-missing-local-signatures'])

test('DeprU', [extra_files(['DeprM.hs'])], multimod_compile, ['DeprU', '-Wall'])

test('Werror01', normal, compile, [''])
test('Werror02', normal, compile, [''])

test('MissingMod', normal, multimod_compile, ['MissingMod', '-Wmissing-home-modules'])
test('T16551', [extra_files(['T16551/'])], multimod_compile, ['T16551/A.hs T16551/B.hs', '-Wmissing-home-modules'])

test('StarBinder', normal, compile, [''])

test('Overflow', expect_broken_for(16543, ['hpc']), compile, [''])

test('UnusedPackages', [ normalise_version('bytestring')
                       , normalise_version('base')
                       , normalise_version('ghc-internal')
                       , normalise_version('process')
                       ], multimod_compile,
    ['UnusedPackages.hs', '-package=bytestring -package=base -package=process -package=ghc -Wunused-packages'])

test('T23212', [normalise_version('ghc-prim')
               ], multimod_compile,
    ['T23212', '-v0 -package=ghc-prim -Werror -Wunused-packages'])

test('T18402', normal, compile, [''])

test('T19564a', normal, compile, [''])
test('T19564b', normal, compile, [''])
test('T19564c', normal, compile, [''])
test('T19564d', normal, compile, [''])
# When warning about redundant constraints, test only Function context is highlighted by caret diagnostics
# Also, suppress uniques as one of the warnings is unstable in CI, otherwise.
test('T19296', normal, compile, ['-fdiagnostics-show-caret -Wredundant-constraints -dsuppress-uniques'])
test('DodgyExports01', normal, compile, ['-Wdodgy-exports'])
test('DerivingTypeable', normal, compile, ['-Wderiving-typeable'])
test('T18862a', normal, compile, [''])
test('T18862b', normal, compile, [''])
test('T20312', normal, compile,['-Wall'])
test('T22151', normal, compile, ['-Wredundant-constraints'])
test('T22759', normal, compile, [''])
test('T22676', [extra_files(['src'])], multimod_compile, ['src.hs', '-working-dir src -Wmissing-home-modules -v0'])
test('DataToTagWarnings', normal, compile, ['-Wunused-top-binds -Wunused-imports'])
test('DodgyImports', normal, compile, ['-Wdodgy-imports'])
test('DodgyImports_hiding', normal, compile, ['-Wdodgy-imports'])
test('T22702a', normal, compile, [''])
test('T22702b', normal, compile, [''])
test('T22826', normal, compile, [''])
test('T23573', [extra_files(["T23573.hs", "T23573A.hs", "T23573B.hs"])], multimod_compile, ['T23573', '-v0'])
test('T23465', normal, compile, ['-ddump-parsed'])
test('WarnNoncanonical', normal, compile, [''])
test('T24396', [extra_files(["T24396a.hs", "T24396b.hs"])], multimod_compile, ['T24396b', ''])
test('SpecMultipleTys', normal, compile, ['']) # compile_fail from GHC 9.18
