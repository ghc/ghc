
def expect_fail_if_windows(opts):
   f = if_platform('i386-unknown-mingw32', expect_fail);
   return f(opts);

test('IOError001', compose(omit_ways(['ghci']), set_stdin('IOError001.hs')),
	compile_and_run, [''])

test('IOError002',      normal, compile_and_run, [''])
test('finalization001', normal, compile_and_run, [''])
test('hClose001',       extra_clean(['hClose001.tmp']), compile_and_run, [''])
test('hClose002',       extra_clean(['hClose002.tmp']), compile_and_run, [''])
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

# hReady is broken for files (always returns False) on Unix platforms
# (bug #1063) , but is broken in a different way on Windows (always
# returns True) (bug #1198).
test('hReady001', if_compiler_type('ghc', expect_broken(1063)), compile_and_run, ['-cpp'])

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
test('openFile005',  compose(if_compiler_type('hugs', expect_fail),
			expect_fail_if_windows), compile_and_run, [''])
clean(['openFile005.out1', 'openFile005.out2'])
test('openFile006',  normal, compile_and_run, [''])
clean(['openFile006.out'])
test('openFile007',  compose(if_compiler_type('hugs', expect_fail),
			expect_fail_if_windows), compile_and_run, [''])
clean(['openFile007.out'])
test('openFile008', cmd_prefix('ulimit -n 1024; '), compile_and_run, [''])

test('putStr001',    normal, compile_and_run, [''])
test('readFile001',  compose(if_compiler_type('hugs', expect_fail),
			expect_fail_if_windows), compile_and_run, [''])
clean(['readFile001.out'])
test('readwrite001', normal, compile_and_run, ['-cpp'])
clean(['readwrite001.inout'])


test('readwrite002',
     composes([omit_ways(['ghci']), 
               set_stdin('readwrite002.hs'),
               if_platform('i386-unknown-mingw32', skip) # see #1198
               ]),
     compile_and_run, ['-cpp'])
clean(['readwrite002.inout'])

test('hGetBuf001', compose(only_compiler_types(['ghc']),
		   compose(skip_if_fast,
		     expect_fail_if_windows)), compile_and_run, ['-package unix'])

test('hDuplicateTo001', normal, compile_and_run, [''])
clean(['tmp'])

test('countReaders001', compose(extra_clean(['countReaders001.txt']),
                                expect_fail_if_windows),
      compile_and_run, [''])

test('concio001', skip, run_command, ['$MAKE -s --no-print-directory test.concio001'])
test('concio001.thr', skip, run_command, ['$MAKE -s --no-print-directory test.concio001.thr'])

test('concio002', reqlib('process'), compile_and_run, [''])

test('2122', extra_clean(['2122-test']), compile_and_run, [''])
