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
             # 5 on Windows - XXX why is that?
             exit_code_if_platform(5, 'i386-unknown-mingw32')]),
     compile_and_run, [''])
test('divbyzero',
             # SIGFPE on Linux
     compose(exit_code(136),
             # 5 on Windows - XXX why is that?
             exit_code_if_platform(5, 'i386-unknown-mingw32')),
     compile_and_run, [''])
