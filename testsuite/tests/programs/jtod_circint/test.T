
test('jtod_circint',
     [when(fast(), skip),
      extra_clean(['Bit.hi',    'Bit.o',
                   'LogFun.hi', 'LogFun.o',
                   'Main.hi',   'Main.o',
                   'Signal.hi', 'Signal.o'])],
     multimod_compile_and_run,
     ['Main', ''])

