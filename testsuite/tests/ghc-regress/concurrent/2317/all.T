test('2317',
     [skip_if_fast,
      expect_broken_for(2574, ['threaded2']),
      reqlib('parallel')],
     multimod_compile_and_run,
     ['2317',''])
