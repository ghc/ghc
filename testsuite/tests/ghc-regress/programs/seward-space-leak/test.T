setTestOpts(omit_compiler_types(['hugs']))	# takes much too long

test('seward-space-leak', normal, multimod_compile_and_run, ['Main', ''])
clean_o_hi()

