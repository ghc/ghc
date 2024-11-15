# -*- coding: utf-8 -*-

def expect_fail_if_windows(name, opts):
   f = when(opsys('mingw32'), expect_fail);
   return f(normal, opts);

test('IOError001', [set_stdin('IOError001.hs')], compile_and_run, [''])

test('IOError002',      normal, compile_and_run, [''])
test('finalization001', normal, compile_and_run, [''])
test('hClose001', [], compile_and_run, [''])
test('hClose002', [normalise_win32_io_errors, js_broken(22261)], compile_and_run, [''])
test('hFileSize001',    normal, compile_and_run, [''])
test('hFileSize002',
     [omit_ghci], # different output
     compile_and_run, [''])
test('hFlush001', [], compile_and_run, [''])

test('hGetBuffering001',
	[omit_ghci, # can't use stdin
   set_stdin('hGetBuffering001.hs')],
	compile_and_run, [''])

test('hGetContentsS001', normal, compile_and_run, [''])
test('hGetChar001', normal, compile_and_run, [''])
test('hGetLine001', [set_stdin('hGetLine001.hs')], compile_and_run, ['-cpp'])
test('hGetLine002', normal, compile_and_run, [''])
test('hGetLine003', normal, compile_and_run, [''])
test('hGetPosn001', [], compile_and_run, ['-cpp'])
test('hIsEOF001',   normal, compile_and_run, [''])
test('hIsEOF002', [], compile_and_run, ['-cpp'])

test('hReady001', js_broken(22374), compile_and_run, ['-cpp'])

# hReady002 tests that hReady returns False for a pipe that has no
# data to read.  It relies on piping input from 'sleep 1', which doesn't
# work for the 'ghci' way because in that case we already pipe input from
# a script, so hence omit_ghci
test('hReady002',
     [cmd_prefix('sleep 1 |'),
      omit_ghci,
      multi_cpu_race,
      js_broken(22374),
      when(arch('wasm32'), fragile(23275))],
     compile_and_run, [''])

test('hSeek001', normal, compile_and_run, [''])
test('hSeek002', normal, compile_and_run, ['-cpp'])
test('hSeek003', normal, compile_and_run, ['-cpp'])
test('hSeek004', [], compile_and_run, ['-cpp'])

test('hSetBuffering002', [set_stdin('hSetBuffering002.hs')], compile_and_run, [''])

test('hSetBuffering003',
    [ omit_ghci # ghci's initial buffering setting is different, and this test checks that
    , set_stdin('hSetBuffering003.hs')
    , js_broken(22261)
    ],
	compile_and_run, [''])

test('hSetBuffering004', set_stdin('hSetBuffering004.hs'), compile_and_run, [''])

test('ioeGetErrorString001', normal, compile_and_run, ['-cpp'])
test('ioeGetFileName001',    normal, compile_and_run, ['-cpp'])
test('ioeGetHandle001',      normal, compile_and_run, ['-cpp'])
test('isEOF001', [extra_run_opts('</dev/null')], compile_and_run, [''])

test('misc001', [extra_run_opts('misc001.hs misc001.out')], compile_and_run,
     [''])

test('withFile001', [exit_code(1)], compile_and_run, [''])
test('withFile002', [exit_code(1)], compile_and_run, [''])
test('withFileBlocking001', [exit_code(1)], compile_and_run, [''])
test('withFileBlocking002', [exit_code(1)], compile_and_run, [''])
test('withBinaryFile001', [exit_code(1)], compile_and_run, [''])
test('withBinaryFile002', [exit_code(1)], compile_and_run, [''])

test('openFile001',  normal, compile_and_run, [''])
test('openFile002',  [exit_code(1), normalise_win32_io_errors], compile_and_run, [''])
test('openFile003', normalise_win32_io_errors, compile_and_run, [''])
test('openFile004', [], compile_and_run, [''])
test('openFile005', js_broken(22261), compile_and_run, [''])
test('openFile006', [], compile_and_run, [''])
test('openFile007', js_broken(22261), compile_and_run, [''])
test('openFile008', [cmd_prefix('ulimit -n 1024; ')], compile_and_run, [''])
test('openFile009', [], compile_and_run, [''])

test('putStr001',    normal, compile_and_run, [''])
test('readFile001', js_broken(22261), compile_and_run, [''])
test('readwrite001', [], compile_and_run, ['-cpp'])


test('readwrite002',
     [omit_ghci, # can't use stdin
      set_stdin('readwrite002.hs'),
      js_broken(22374)],
     compile_and_run, ['-cpp'])

test('readwrite003', [], compile_and_run, [''])

test('hGetBuf001',
     [ when(fast(), skip)
     , expect_fail_if_windows
     , js_broken(22374)
     , req_process
     ],
     compile_and_run, ['-package unix'])

test('hDuplicateTo001', [fragile_for(16819, concurrent_ways),
                         when(opsys('mingw32'), skip),
                         req_process], compile_and_run, [''])

test('countReaders001', js_broken(22261), compile_and_run, [''])

test('concio001', [normal, multi_cpu_race],
     makefile_test, ['test.concio001'])
test('concio001.thr', [extra_files(['concio001.hs']), multi_cpu_race],
     makefile_test, ['test.concio001.thr'])

test('T2122', [], compile_and_run, [''])
test('T3307', [], makefile_test, ['T3307-test'])
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
test('encoding001', [], compile_and_run, [''])

test('encoding002', normal, compile_and_run, [''])
test('encoding003', normal, compile_and_run, [''])
test('encoding004',
     [ extra_files(['encoded-data/']),
       js_broken(22374),
       # wasi-libc doesn't have cp936, see
       # https://gitlab.haskell.org/ghc/wasi-libc/-/blob/main/libc-top-half/musl/src/locale/iconv.c#L38
       # and
       # https://gitlab.haskell.org/ghc/wasi-libc/-/blob/main/libc-top-half/musl/src/locale/codepages.h
       # for locales supported by wasi-libc's iconv implementation
       when(arch('wasm32'), skip),
       # MacOS Sonoma iconv() has a regression that causes this test to fail on the
       # CP936 roundtrip. See the ticket for related issues in other projects.
       when(opsys('darwin'), fragile(24161)),
       when(opsys('freebsd'), expect_broken(22003))
     ], compile_and_run, [''])
test('encoding005', normal, compile_and_run, [''])

test('environment001', [], makefile_test, ['environment001-test'])

test('newline001', [], compile_and_run, [''])

test('openTempFile001', normal, compile_and_run, [''])

test('T4144', normal, compile_and_run, [''])

test('encodingerror001', normal, compile_and_run, [''])

# Requires use of the FD interface which is not supported under WINIO
test('T4808', [when(opsys('mingw32'), skip)
              ,fragile_for(16909, concurrent_ways), exit_code(1)]
              , compile_and_run, [''])
test('T4895', normal, compile_and_run, [''])
test('T7853', normal, compile_and_run, [''])
# Tests ability to perform >32-bit IO operations
test('T17414',
     [when(wordsize(32), skip),
      when(opsys('mingw32'), fragile(17453)),
      # It is common for tmpfs to be mounted to a small tmpfs on modern Linux
      # distributions. This test needs to create a large file which will exceed the
      # size of this filesystem consequently we must skip it (see #17459).
      when(opsys('linux'), skip),
      when(opsys('openbsd'), skip),
      high_memory_usage],
     compile_and_run, [''])
test('T17510', expect_broken(17510), compile_and_run, [''])
test('bytestringread001', extra_run_opts('test.data'), compile_and_run, [''])
test('T17912', [only_ways(['threaded1']), when(opsys('mingw32'),expect_broken(1))], compile_and_run, [''])
test('T18832', only_ways(['threaded1']), compile_and_run, [''])

test('mkdirExists', [exit_code(1), when(opsys('mingw32'), ignore_stderr)], compile_and_run, [''])
