test('T10052', [unless(config.have_RTS_linker, skip),
	       	req_interp],
     makefile_test, ['T10052'])
