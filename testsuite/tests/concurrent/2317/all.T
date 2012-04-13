test('2317',
     [skip_if_fast,
      reqlib('parallel'), reqlib('random')],
     multimod_compile_and_run,
     ['2317',''])
