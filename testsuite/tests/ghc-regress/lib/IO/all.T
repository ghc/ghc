
def expect_fail_if_windows(opts):
   f = expect_fail_if_platform('i386-unknown-mingw32');
   return f(opts);

test('IOError001', compose(omit_ways(['ghci']), set_stdin('IOError001.hs')),
	compile_and_run, [''])

test('IOError002',      normal, compile_and_run, [''])
test('finalization001', normal, compile_and_run, [''])
test('hClose001',       normal, compile_and_run, [''])
clean(['hClose001.tmp'])
test('hFileSize001',    normal, compile_and_run, [''])
test('hFileSize002',    omit_ways(['ghci']), compile_and_run, [''])
clean(['hFileSize002.out'])
test('hFlush001',       normal, compile_and_run, [''])
clean(['hFlush001.out'])

test('hGetBuffering001', 
	compose(omit_ways(['ghci']), set_stdin('hGetBuffering001.hs')), 
	compile_and_run, [''])

test('hGetChar001', normal, compile_and_run, [''])
test('hGetLine001', set_stdin('hGetLine001.hs'), compile_and_run, ['-cpp'])
test('hGetLine002', normal, compile_and_run, [''])
test('hGetLine003', normal, compile_and_run, [''])
test('hGetPosn001', normal, compile_and_run, ['-cpp'])
clean(['hGetPosn001.out'])
test('hIsEOF001',   normal, compile_and_run, [''])
test('hIsEOF002',   normal, compile_and_run, ['-cpp'])
clean(['hIsEOF002.out'])

# hReady doesn't work at the end of a file in GHC
test('hReady001', expect_fail_if_compiler_type('ghc'), compile_and_run, ['-cpp'])

test('hSeek001', normal, compile_and_run, [''])
test('hSeek002', normal, compile_and_run, ['-cpp'])
test('hSeek003', normal, compile_and_run, ['-cpp'])
test('hSeek004', normal, compile_and_run, ['-cpp'])
clean(['hSeek004.out'])

test('hSetBuffering002', set_stdin('hSetBuffering002.hs'), compile_and_run, [''])

test('hSetBuffering003', compose(omit_ways(['ghci']), 
				 set_stdin('hSetBuffering003.hs')), 
	compile_and_run, [''])

test('ioeGetErrorString001', normal, compile_and_run, ['-cpp'])
test('ioeGetFileName001',    normal, compile_and_run, ['-cpp'])
test('ioeGetHandle001',      normal, compile_and_run, ['-cpp'])
test('isEOF001',     normal, compile_and_run, [''])

test('misc001', extra_run_opts('misc001.hs misc001.out'), \
	compile_and_run, [''])
clean(['misc001.out'])

test('openFile001',  normal, compile_and_run, [''])
test('openFile002',  exit_code(1), compile_and_run, [''])
test('openFile003',  normal, compile_and_run, [''])
test('openFile004',  normal, compile_and_run, [''])
clean(['openFile004.out'])
test('openFile005',  compose(expect_fail_if_compiler_type('hugs'),
			expect_fail_if_windows), compile_and_run, [''])
clean(['openFile005.out1', 'openFile005.out2'])
test('openFile006',  normal, compile_and_run, [''])
clean(['openFile006.out'])
test('openFile007',  compose(expect_fail_if_compiler_type('hugs'),
			expect_fail_if_windows), compile_and_run, [''])
clean(['openFile007.out'])
test('putStr001',    normal, compile_and_run, [''])
test('readFile001',  compose(expect_fail_if_compiler_type('hugs'),
			expect_fail_if_windows), compile_and_run, [''])
clean(['readFile001.out'])
test('readwrite001', normal, compile_and_run, ['-cpp'])
clean(['readwrite001.inout'])

test('readwrite002', compose(omit_ways(['ghci']), set_stdin('readwrite002.hs')),
	compile_and_run, ['-cpp'])
clean(['readwrite002.inout'])

test('hGetBuf001', compose(only_compiler_types(['ghc']),
		   compose(skip_if_fast,
		     expect_fail_if_windows)), compile_and_run, ['-package unix'])

test('hDuplicateTo001', normal, compile_and_run, [''])
clean(['tmp'])

