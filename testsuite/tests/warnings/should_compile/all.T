# -fwarn-missing-exported-signatures should take precedence over -fwarn-missing-signatures
test('T2526', normal, compile, ['-fwarn-missing-signatures -fwarn-missing-exported-signatures'])
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

test('UnusedPackages', normal, multimod_compile,
    ['UnusedPackages.hs', '-package=bytestring -package=base -package=process -package=ghc -Wunused-packages'])
