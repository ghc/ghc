# uses GHC-specific scoped type variables
test('thurston-modular-arith', only_compiler_types(['ghc']),
	multimod_compile_and_run,
	['Main', ''])
clean_o_hi()

