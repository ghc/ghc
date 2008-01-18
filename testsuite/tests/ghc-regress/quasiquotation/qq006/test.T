test('qq006', compose( skip_if_fast, only_compiler_types(['ghc']) ),
	multimod_compile_fail,
	['Main', ''])
clean_o_hi()
