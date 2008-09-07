test('2185', [skip_if_fast, reqlib('parallel'), 
              expect_broken(2185),
              expect_broken(2570),
              extra_run_opts('+RTS -M16m -RTS'),
              only_ways(['threaded1','threaded2'])], 
        # threaded1 demonstrates the bug: sparks were treated as roots by GC
        multimod_compile_and_run, ['2185',''])
