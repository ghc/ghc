# -*- coding: utf-8 -*-

def expect_fail_if_windows(name, opts):
   f = when(opsys('mingw32'), expect_fail);
   return f(normal, opts);

test('IOError001', [omit_ways(['ghci']), set_stdin('IOError001.hs')],
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
	[omit_ways(['ghci']), set_stdin('hGetBuffering001.hs')], 
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

# hReady002 tests that hReady returns False for a pipe that has no
# data to read.  It relies on piping input from 'sleep 1', which doesn't
# work for the 'ghci' way because in that case we already pipe input from
# a script, so hence omit_ways(['ghci'])
test('hReady002', [ no_stdin, cmd_prefix('sleep 1 |'),
                    omit_ways(['ghci']) ],
                   compile_and_run, [''])

test('hSeek001', normal, compile_and_run, [''])
test('hSeek002', normal, compile_and_run, ['-cpp'])
test('hSeek003', normal, compile_and_run, ['-cpp'])
test('hSeek004', extra_clean(['hSeek004.out']), compile_and_run, ['-cpp'])

test('hSetBuffering002', set_stdin('hSetBuffering002.hs'), compile_and_run, [''])

test('hSetBuffering003',
    [omit_ways(['ghci']), set_stdin('hSetBuffering003.hs')], 
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
test('openFile003',  extra_clean(['openFile003Dir']), compile_and_run, [''])
test('openFile004',  extra_clean(['openFile004.out']), compile_and_run, [''])
test('openFile005',
     [when(compiler_type('hugs'), expect_fail),
      extra_clean(['openFile005.out1', 'openFile005.out2'])],
     compile_and_run, [''])
test('openFile006', extra_clean(['openFile006.out']), compile_and_run, [''])
test('openFile007',
     [when(compiler_type('hugs'), expect_fail),
      extra_clean(['openFile007.out'])],
     compile_and_run, [''])
test('openFile008', cmd_prefix('ulimit -n 1024; '), compile_and_run, [''])

test('putStr001',    normal, compile_and_run, [''])
test('readFile001',
     [when(compiler_type('hugs'), expect_fail),
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

test('hGetBuf001',
     [only_compiler_types(['ghc']),
      when(fast(), skip),
      expect_fail_if_windows],
     compile_and_run, ['-package unix'])

test('hDuplicateTo001', extra_clean(['tmp']), compile_and_run, [''])

test('countReaders001',
      extra_clean(['countReaders001.txt']),
      compile_and_run, [''])

test('concio001', skip, run_command, ['$MAKE -s --no-print-directory test.concio001'])
test('concio001.thr', skip, run_command, ['$MAKE -s --no-print-directory test.concio001.thr'])

test('concio002', reqlib('process'), compile_and_run, [''])

test('T2122', extra_clean(['T2122-test']), compile_and_run, [''])
test('T3307',
     [when(msys(), expect_fail), # Doesn't work on MSYS; see #5599
      extra_clean(['chinese-file-小说', 'chinese-name'])],
     run_command,
     ['$MAKE -s --no-print-directory T3307-test'])
test('T4855', normal, compile_and_run, [''])

test('hSetEncoding001',extra_run_opts('hSetEncoding001.in'), compile_and_run, [''])
test('decodingerror001',normal, compile_and_run, [''])
test('decodingerror002',normal, compile_and_run, [''])

encoding001Encodings = ["utf8", "utf8_bom", "utf16", "utf16le",
                        "utf16be", "utf32", "utf32le", "utf32be"]
encoding001CleanFiles = []
for e in encoding001Encodings:
    encoding001CleanFiles.append('encoding001.' + e)
for e1 in encoding001Encodings:
    for e2 in encoding001Encodings:
        encoding001CleanFiles.append('encoding001.' + e1 + '.' + e2)
test('encoding001',
     extra_clean(encoding001CleanFiles),
     compile_and_run, [''])

test('encoding002', normal, compile_and_run, [''])
test('encoding003', normal, compile_and_run, [''])
test('encoding004', normal, compile_and_run, [''])

test('environment001',
     [when(msys(), expect_fail), # Doesn't work on MSYS; see #5599
      extra_clean(['environment001'])],
     run_command,
     ['$MAKE -s --no-print-directory environment001-test'])

test('newline001', extra_clean(['newline001.out']), compile_and_run, [''])

test('openTempFile001', normal, compile_and_run, [''])

test('T4144', normal, compile_and_run, [''])

test('encodingerror001', normal, compile_and_run, [''])

test('T4808', [exit_code(1), extra_clean(['T4808.test'])], compile_and_run, [''])
test('T4895', normal, compile_and_run, [''])
test('T7853', normal, compile_and_run, [''])
