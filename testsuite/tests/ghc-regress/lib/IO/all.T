
def expect_fail_if_windows( opts ):
   return expect_fail_if_platform("i386-unknown-mingw32");

test('IOError001', compose(omit_ways(['ghci']), set_stdin('IOError001.hs')),
	compile_and_run, [''])

test('IOError002',      normal, compile_and_run, [''])
test('finalization001', normal, compile_and_run, [''])
test('hClose001',       normal, compile_and_run, [''])
test('hFileSize001',    normal, compile_and_run, [''])
test('hFileSize002',    omit_ways(['ghci']), compile_and_run, [''])
test('hFlush001',       normal, compile_and_run, [''])

test('hGetBuffering001', 
	compose(omit_ways(['ghci']), set_stdin('hGetBuffering001.hs')), 
	compile_and_run, [''])

test('hGetChar001', normal, compile_and_run, [''])
test('hGetLine001', set_stdin('hGetLine001.hs'), compile_and_run, ['-cpp'])
test('hGetLine002', normal, compile_and_run, [''])
test('hGetLine003', normal, compile_and_run, [''])
test('hGetPosn001', normal, compile_and_run, ['-cpp'])
test('hIsEOF001',   normal, compile_and_run, [''])
test('hIsEOF002',   normal, compile_and_run, ['-cpp'])

# hReady doesn't work at the end of a file in GHC
test('hReady001', expect_fail, compile_and_run, ['-cpp'])

test('hSeek001', normal, compile_and_run, ['-cpp'])
test('hSeek002', normal, compile_and_run, ['-cpp'])
test('hSeek003', normal, compile_and_run, ['-cpp'])
test('hSeek004', normal, compile_and_run, ['-cpp'])

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

test('openFile001',  normal, compile_and_run, [''])
test('openFile002',  exit_code(1), compile_and_run, [''])
test('openFile003',  normal, compile_and_run, [''])
test('openFile004',  normal, compile_and_run, [''])
test('openFile005',  expect_fail_if_windows, compile_and_run, [''])
test('openFile006',  normal, compile_and_run, [''])
test('openFile007',  expect_fail_if_windows, compile_and_run, [''])
test('putStr001',    normal, compile_and_run, [''])
test('readFile001',  expect_fail_if_windows, compile_and_run, [''])
test('readwrite001', normal, compile_and_run, ['-cpp'])

test('readwrite002', compose(omit_ways(['ghci']), set_stdin('readwrite002.hs')),
	compile_and_run, ['-cpp'])
