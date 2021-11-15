test('T2317',
     [when(fast(), skip),
      when(opsys("OSJavaScript"), skip),
      reqlib('parallel'), reqlib('random')],
     multimod_compile_and_run,
     ['T2317',''])
