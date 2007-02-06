
test('net001', compose(only_compiler_types(['ghc']), reqlib('network')), compile_and_run, ['-package network'])
test('net002', compose(compose(only_compiler_types(['ghc']), extra_run_opts('3')), reqlib('network')),
	compile_and_run, ['-package network'])

test('uri001', compose(skip_if_fast, reqlib('network')), compile_and_run, ['-package network -package HUnit'])
