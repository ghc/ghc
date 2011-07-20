setTestOpts(omit_compiler_types(['hugs']))	# takes much too long

test('seward-space-leak',
     [skip_if_fast, extra_clean(['Main.hi', 'Main.o'])],
     multimod_compile_and_run,
     ['Main', ''])

