test('par001', only_ways(['threaded1', 'threaded2']),
                compile_and_run, ['-package parallel'])

test('par002', only_ways(['threaded2']),
                compile_and_run, ['-O0 -package parallel'])


test('par003', req_smp, run_command, ['$MAKE -s --no-print-directory par003'])

test('T2185', [when(fast(), skip), reqlib('parallel'),
              extra_run_opts('+RTS -M16m -RTS'),
              only_ways(['threaded1','threaded2'])],
        # threaded1 demonstrates the bug: sparks were treated as roots by GC
        multimod_compile_and_run, ['T2185',''])
