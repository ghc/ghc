test('qq006', [ skip_if_fast, only_compiler_types(['ghc']),
                if_compiler_lt( 'ghc', '6.9', skip ) ],
	multimod_compile_fail,
	['Main', ''])
clean_o_hi()
