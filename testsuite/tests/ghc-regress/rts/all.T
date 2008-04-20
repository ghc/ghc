test('testblockalloc', compose(c_src, 
                       compose(only_ways(['normal','threaded1']),
                               extra_run_opts('+RTS -I0'))), 
                       compile_and_run, [''])


# See bug #101, test requires +RTS -c (or equivalently +RTS -M<something>)
# only GHCi triggers the bug, but we run the test all ways for completeness.
test('bug1010', normal, compile_and_run, ['+RTS -c -RTS'])
test('derefnull',
             # SIGSEGV on Linux
     composes([exit_code(139),
             # SIGBUS on OX X
             if_platform('i386-apple-darwin',    exit_code(138)),
             if_platform('powerpc-apple-darwin', exit_code(138)),
             if_platform('i386-unknown-mingw32', exit_code(1))]),
     compile_and_run, [''])
test('divbyzero',
             # SIGFPE on Linux
     compose(exit_code(136),
             if_platform('i386-unknown-mingw32', exit_code(1))),
     compile_and_run, [''])

test('outofmem', normal, run_command, ['$MAKE -s --no-print-directory outofmem'])
test('outofmem2', extra_run_opts('+RTS -M5m -RTS'),
                  run_command, ['$MAKE -s --no-print-directory outofmem2'])

test('2047', compose(ignore_output, extra_run_opts('+RTS -c -RTS')),
             compile_and_run, ['-package containers'])
