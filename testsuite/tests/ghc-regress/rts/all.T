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
             exit_code_if_platform(138, 'i386-apple-darwin'),
             exit_code_if_platform(138, 'powerpc-apple-darwin'),
             exit_code_if_platform(1, 'i386-unknown-mingw32')]),
     compile_and_run, [''])
test('divbyzero',
             # SIGFPE on Linux
     compose(exit_code(136),
             exit_code_if_platform(1, 'i386-unknown-mingw32')),
     compile_and_run, [''])

test('outofmem', exit_code(1), compile_and_run, [''])
test('outofmem2', composes([exit_code(251),
                            expect_fail_if_compiler_lt('ghc','6.9'),
                            extra_run_opts('+RTS -M5m -RTS')]),
                  compile_and_run, [''])
