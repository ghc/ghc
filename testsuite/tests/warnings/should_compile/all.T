# -fwarn-missing-exported-signatures should take precedence over -fwarn-missing-signatures
test('T2526', normal, compile, ['-fwarn-missing-signatures -fwarn-missing-exported-signatures'])
test('T9178', extra_clean(['T9178.o', 'T9178DataType.o',
			   'T9178.hi', 'T9178DataType.hi']),
	      multimod_compile, ['T9178', '-Wall'])

# T9230.hs contains a tab charater. Test that -Wtabs is enabled by default.
test('T9230', normal, compile, [''])

test('T10908', normal, compile, [''])
test('T11077', normal, compile, ['-fwarn-missing-exported-signatures'])
test('T11128', normal, compile, [''])
test('T11128b', normal, compile, [''])
test('PluralS', normal, compile, [''])

# T12574 Test that suggest current flag over deprecated
test('T12574',normal, compile, ['-fwarn-missing-local-signatures'])

test('DeprU',
	extra_clean([
		'DeprM.o',  'DeprU.o',
		'DeprM.hi', 'DeprU.hi']),
	multimod_compile, ['DeprU', '-Wall'])
