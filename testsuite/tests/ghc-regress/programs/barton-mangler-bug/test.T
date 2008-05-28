# Exhausts Hugs's heap (CAF leak)

test('barton-mangler-bug',
	composes([skip_if_fast,
              omit_compiler_types(['hugs']),
              if_compiler_lt('ghc', '6.9', omit_ways(['profc']))]),
	multimod_compile_and_run, ['Main', ''])
clean_o_hi()
