test('testblockalloc', compose(c_src, 
                       compose(only_ways(['normal','threaded1']),
                               extra_run_opts('+RTS -I0'))), 
                       compile_and_run, [''])


# See bug #101, test requires +RTS -c (or equivalently +RTS -M<something>)
# only GHCi triggers the bug, but we run the test all ways for completeness.
test('bug1010', normal, compile_and_run, ['+RTS -c -RTS'])
test('derefnull',
     composes([
             # SIGSEGV on Linux (which we make the default)
             exit_code(139),
             # Apparently the output can be different on different
             # Linux setups, so just ignore it. As long as we get
             # the right exit code we're OK.
             if_os('linux', ignore_output),
             # SIGBUS on OX X
             if_os('darwin', exit_code(138)),
             if_platform('i386-unknown-mingw32', exit_code(1))]),
     compile_and_run, [''])
test('divbyzero',
     composes([
             # SIGFPE on Linux
             exit_code(136),
             # Apparently the output can be different on different
             # Linux setups, so just ignore it. As long as we get
             # the right exit code we're OK.
             if_os('linux', ignore_output),
             if_platform('i386-unknown-mingw32', exit_code(1))]),
     compile_and_run, [''])

test('outofmem', normal, run_command, ['$MAKE -s --no-print-directory outofmem'])
test('outofmem2', extra_run_opts('+RTS -M5m -RTS'),
                  run_command, ['$MAKE -s --no-print-directory outofmem2'])

test('2047', compose(ignore_output, extra_run_opts('+RTS -c -RTS')),
             compile_and_run, ['-package containers'])

# Blackhole-detection test.
# Skip GHCi due to #2786
test('2783', [ omit_ways(['ghci']), exit_code(1) ], compile_and_run, [''])

# Test the work-stealing deque implementation.  We run this test in
# both threaded1 (-threaded -debug) and threaded2 (-threaded) ways.
test('testwsdeque', [if_compiler_lt('ghc', '6.11', skip),
                    c_src, only_ways(['threaded1', 'threaded2'])],
                    compile_and_run, ['-I../../../../rts/parallel'])
