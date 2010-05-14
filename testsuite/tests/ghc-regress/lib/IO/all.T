
def expect_fail_if_windows(opts):
   f = if_platform('i386-unknown-mingw32', expect_fail);
   return f(opts);

test('IOError001', compose(omit_ways(['ghci']), set_stdin('IOError001.hs')),
	compile_and_run, [''])

test('IOError002',      normal, compile_and_run, [''])
test('finalization001', normal, compile_and_run, [''])
test('hClose001',       extra_clean(['hClose001.tmp']), compile_and_run, [''])
test('hClose002',       extra_clean(['hClose002.tmp']), compile_and_run, [''])
test('hClose003',       reqlib('unix'), compile_and_run, ['-package unix'])
test('hFileSize001',    normal, compile_and_run, [''])
test('hFileSize002',
     [omit_ways(['ghci']),
      extra_clean(['hFileSize002.out'])],
     compile_and_run, [''])
test('hFlush001',
     extra_clean(['hFlush001.out']),
     compile_and_run, [''])

test('hGetBuffering001', 
	compose(omit_ways(['ghci']), set_stdin('hGetBuffering001.hs')), 
	compile_and_run, [''])

test('hGetChar001', normal, compile_and_run, [''])
test('hGetLine001', set_stdin('hGetLine001.hs'), compile_and_run, ['-cpp'])
test('hGetLine002', normal, compile_and_run, [''])
test('hGetLine003', normal, compile_and_run, [''])
test('hGetPosn001',
     extra_clean(['hGetPosn001.out']),
     compile_and_run, ['-cpp'])
test('hIsEOF001',   normal, compile_and_run, [''])
test('hIsEOF002', extra_clean(['hIsEOF002.out']), compile_and_run, ['-cpp'])

test('hReady001', normal, compile_and_run, ['-cpp'])

test('hSeek001', normal, compile_and_run, [''])
test('hSeek002', normal, compile_and_run, ['-cpp'])
test('hSeek003', normal, compile_and_run, ['-cpp'])
test('hSeek004', extra_clean(['hSeek004.out']), compile_and_run, ['-cpp'])

test('hSetBuffering002', set_stdin('hSetBuffering002.hs'), compile_and_run, [''])

test('hSetBuffering003', compose(omit_ways(['ghci']), 
				 set_stdin('hSetBuffering003.hs')), 
	compile_and_run, [''])

test('hSetBuffering004', set_stdin('hSetBuffering004.hs'), compile_and_run, [''])

test('ioeGetErrorString001', normal, compile_and_run, ['-cpp'])
test('ioeGetFileName001',    normal, compile_and_run, ['-cpp'])
test('ioeGetHandle001',      normal, compile_and_run, ['-cpp'])
test('isEOF001',     normal, compile_and_run, [''])

test('misc001',
     [extra_run_opts('misc001.hs misc001.out'),
      extra_clean(['misc001.out'])],
	compile_and_run, [''])

test('openFile001',  normal, compile_and_run, [''])
test('openFile002',  exit_code(1), compile_and_run, [''])
test('openFile003',  normal, compile_and_run, [''])
test('openFile004',  extra_clean(['openFile004.out']), compile_and_run, [''])
test('openFile005',
     [if_compiler_type('hugs', expect_fail),
      extra_clean(['openFile005.out1', 'openFile005.out2'])],
     compile_and_run, [''])
test('openFile006', extra_clean(['openFile006.out']), compile_and_run, [''])
test('openFile007',
     [if_compiler_type('hugs', expect_fail),
      extra_clean(['openFile007.out'])],
     compile_and_run, [''])
test('openFile008', cmd_prefix('ulimit -n 1024; '), compile_and_run, [''])

test('putStr001',    normal, compile_and_run, [''])
test('readFile001',
     [if_compiler_type('hugs', expect_fail),
      extra_clean(['readFile001.out'])],
     compile_and_run, [''])
test('readwrite001',
     extra_clean(['readwrite001.inout']),
     compile_and_run,
     ['-cpp'])


test('readwrite002',
     [omit_ways(['ghci']), 
      set_stdin('readwrite002.hs'),
      extra_clean(['readwrite002.inout'])],
     compile_and_run, ['-cpp'])

test('readwrite003', extra_clean(['readwrite003.txt']), compile_and_run, [''])

test('hGetBuf001', compose(only_compiler_types(['ghc']),
		   compose(skip_if_fast,
		     expect_fail_if_windows)), compile_and_run, ['-package unix'])

test('hDuplicateTo001', extra_clean(['tmp']), compile_and_run, [''])

test('countReaders001',
      extra_clean(['countReaders001.txt']),
      compile_and_run, [''])

test('concio001', skip, run_command, ['$MAKE -s --no-print-directory test.concio001'])
test('concio001.thr', skip, run_command, ['$MAKE -s --no-print-directory test.concio001.thr'])

test('concio002', reqlib('process'), compile_and_run, [''])

test('2122', extra_clean(['2122-test']), compile_and_run, [''])

test('hSetEncoding001',extra_run_opts('hSetEncoding001.in'), compile_and_run, [''])
test('hSetEncoding002',normal, compile_and_run, [''])
test('decodingerror001',normal, compile_and_run, [''])

test('encoding001',
     extra_clean([
           'encoding001.utf16', 'encoding001.utf16.utf16be',
           'encoding001.utf16.utf16le', 'encoding001.utf16.utf32',
           'encoding001.utf16.utf32be', 'encoding001.utf16.utf32le',
           'encoding001.utf16.utf8', 'encoding001.utf16be',
           'encoding001.utf16be.utf16', 'encoding001.utf16be.utf16le',
           'encoding001.utf16be.utf32', 'encoding001.utf16be.utf32be',
           'encoding001.utf16be.utf32le', 'encoding001.utf16be.utf8',
           'encoding001.utf16le', 'encoding001.utf16le.utf16',
           'encoding001.utf16le.utf16be', 'encoding001.utf16le.utf32',
           'encoding001.utf16le.utf32be', 'encoding001.utf16le.utf32le',
           'encoding001.utf16le.utf8', 'encoding001.utf32',
           'encoding001.utf32.utf16', 'encoding001.utf32.utf16be',
           'encoding001.utf32.utf16le', 'encoding001.utf32.utf32be',
           'encoding001.utf32.utf32le', 'encoding001.utf32.utf8',
           'encoding001.utf32be', 'encoding001.utf32be.utf16',
           'encoding001.utf32be.utf16be', 'encoding001.utf32be.utf16le',
           'encoding001.utf32be.utf32', 'encoding001.utf32be.utf32le',
           'encoding001.utf32be.utf8', 'encoding001.utf32le',
           'encoding001.utf32le.utf16', 'encoding001.utf32le.utf16be',
           'encoding001.utf32le.utf16le', 'encoding001.utf32le.utf32',
           'encoding001.utf32le.utf32be', 'encoding001.utf32le.utf8',
           'encoding001.utf8', 'encoding001.utf8.utf16',
           'encoding001.utf8.utf16be', 'encoding001.utf8.utf16le',
           'encoding001.utf8.utf32', 'encoding001.utf8.utf32be',
           'encoding001.utf8.utf32le']), 
     compile_and_run, [''])

test('newline001', extra_clean(['newline001.out']), compile_and_run, [''])

test('openTempFile001', normal, compile_and_run, [''])
