# -fwarn-missing-exported-sigs should take precedence over -fwarn-missing-signatures
test('T2526', normal, compile, ['-fwarn-missing-signatures -fwarn-missing-exported-sigs'])
test('T9178', extra_clean(['T9178.o', 'T9178DataType.o',
			   'T9178.hi', 'T9178DataType.hi']),
	      multimod_compile, ['T9178', '-Wall'])
test('T9230', normal, compile_without_flag('-fno-warn-tabs'), [''])
