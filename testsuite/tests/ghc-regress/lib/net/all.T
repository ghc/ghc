
test('net001', only_compiler_types(['ghc']), compile_and_run, ['-package network'])
test('net002', compose(only_compiler_types(['ghc']), extra_run_opts('3')),
	compile_and_run, ['-package network'])

test('uri001', skip_if_fast, compile_and_run, ['-package network -package HUnit'])
