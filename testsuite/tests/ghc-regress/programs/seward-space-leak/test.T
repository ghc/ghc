setTestOpts(omit_compiler_types(['hugs']))	# takes much too long

test('seward-space-leak', skip_if_fast, multimod_compile_and_run, ['Main', ''])
clean_o_hi()

