test('ioref001', extra_run_opts('+RTS -K16m -RTS'), \
	compile_and_run, ['+RTS -K16m -RTS'])

# need to add -K16m to the compiler opts, so that GHCi gets it too
