test('T9178', extra_clean(['T9178.o', 'T9178DataType.o',
			   'T9178.hi', 'T9178DataType.hi']),
	      multimod_compile, ['T9178', '-Wall'])