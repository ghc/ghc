test('T2317',
     [when(fast(), skip),
      javascript_failures,
      reqlib('parallel'), reqlib('random')],
     multimod_compile_and_run,
     ['T2317',''])
