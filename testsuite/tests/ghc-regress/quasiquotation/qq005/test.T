test('qq005', compose( skip_if_fast, only_compiler_types(['ghc']) ),
	multimod_compile_and_run,
	['Main', ''])
clean_o_hi()
