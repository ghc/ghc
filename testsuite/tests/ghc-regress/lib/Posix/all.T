if config.platform == 'i386-unknown-mingw32':
   conf = skip
else:
   conf = normal

test('signals001',  conf, compile_and_run, ['-package unix -cpp'])
test('signals002',  compose(only_compiler_types(['ghc']), conf),
	compile_and_run, ['-package unix'])
test('fileexist01', conf, compile_and_run, ['-package unix'])
test('forkprocess01', compose(only_compiler_types(['ghc']), 
		      compose(expect_fail_for('threaded2'), conf)),
	compile_and_run, ['-package unix'])

if config.platform == 'i386-unknown-freebsd':
        conf = expect_fail

test('queryfdoption01', compose(omit_ways(['ghci']), compose(only_compiler_types(['ghc']), conf)),
	compile_and_run, ['-package unix'])
