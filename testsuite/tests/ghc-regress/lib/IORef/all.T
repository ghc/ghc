test('ioref001', compose(skip_if_fast,extra_run_opts('+RTS -K32m -RTS')), \
	compile_and_run, ['+RTS -K32m -RTS'])

# need to add -K32m to the compiler opts, so that GHCi gets it too
