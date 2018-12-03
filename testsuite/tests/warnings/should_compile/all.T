# -fwarn-missing-exported-signatures should take precedence over -fwarn-missing-signatures
test('T2526', normal, compile, ['-fwarn-missing-signatures -fwarn-missing-exported-signatures'])
test('T9178', [], multimod_compile, ['T9178', '-Wall'])

# T9230.hs contains a tab charater. Test that -Wtabs is enabled by default.
test('T9230', normal, compile, [''])

test('T10908', normal, compile, [''])
test('T10930', normal, compile, [''])
test('T10930b', normal, compile, [''])
test('T11077', normal, compile, ['-fwarn-missing-exported-signatures'])
test('T11128', normal, compile, [''])
test('T11128b', normal, compile, [''])
test('T13256', normal, compile, [''])
test('T15460', normal, compile, [''])
test('PluralS', normal, compile, [''])

# T12574 Test that suggest current flag over deprecated
test('T12574',normal, compile, ['-fwarn-missing-local-signatures'])

test('DeprU', [extra_files(['DeprM.hs'])], multimod_compile, ['DeprU', '-Wall'])

test('Werror01', normal, compile, [''])
test('Werror02', normal, compile, [''])

test('MissingMod', normal, multimod_compile, ['MissingMod', '-Wmissing-home-modules'])

test('StarBinder', normal, compile, [''])

test('Overflow', normal, compile, [''])
