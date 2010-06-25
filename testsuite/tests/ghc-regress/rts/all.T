test('testblockalloc', compose(c_src, 
                       compose(only_ways(['normal','threaded1']),
                               extra_run_opts('+RTS -I0'))), 
                       compile_and_run, [''])


# See bug #101, test requires +RTS -c (or equivalently +RTS -M<something>)
# only GHCi triggers the bug, but we run the test all ways for completeness.
test('bug1010', normal, compile_and_run, ['+RTS -c -RTS'])
test('derefnull',
     composes([
             # LLVM Optimiser considers dereference of a null pointer
             # undefined and marks the code as unreachable which means
             # that later optimisations remove it altogether.
             omit_ways(['optllvm']),
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
test('testwsdeque', [unless_in_tree_compiler(skip),
                    c_src, only_ways(['threaded1', 'threaded2'])],
                    compile_and_run, ['-I../../../../rts'])

test('3236', [c_src, only_ways(['normal','threaded1']), exit_code(1)], compile_and_run, [''])

test('stack001', extra_run_opts('+RTS -K32m -RTS'), compile_and_run, [''])
test('stack002', extra_run_opts('+RTS -K32m -k4m -RTS'), compile_and_run, [''])

test('atomicinc', [ c_src, only_ways(['normal']) ], compile_and_run, [''])

test('3424', # it's slow:
             [ skip_if_fast, only_ways(['normal','threaded1','ghci']) ],
             compile_and_run, [''])

# Test for out-of-range heap size
test('rtsflags001', [ only_ways(['normal']), exit_code(1), extra_run_opts('+RTS -H0m -RTS') ], compile_and_run, [''])

# Test to see if linker scripts link properly to real ELF files
test('T2615',
     [ if_platform('i386-unknown-mingw32',skip),
       # OS X doesn't seem to support linker scripts
       if_os('darwin', skip),
       cmd_prefix('$MAKE T2615-prep && ' +
                  # Add current directory to dlopen search path
                  'LD_LIBRARY_PATH=$LD_LIBRARY_PATH:. '),
       extra_clean(['libfoo_T2615.so'])],
     compile_and_run,
     ['-package ghc'])

# omit dyn and profiling ways, because we don't build dyn_l or p_l
# variants of the RTS by default
test('traceEvent', [ omit_ways(['dyn'] + prof_ways), 
                     extra_run_opts('+RTS -ls -RTS') ], 
                   compile_and_run, ['-eventlog'])

test('T4059', normal, run_command, ['$MAKE -s --no-print-directory T4059'])
