
test('andy_cherry',
     [when(fast(), skip),
      extra_clean(['DataTypes.hi',   'DataTypes.o',
                   'GenUtils.hi',    'GenUtils.o',
                   'Interp.hi',      'Interp.o',
                   'InterpUtils.hi', 'InterpUtils.o',
                   'Main.hi',        'Main.o',
                   'Parser.hi',      'Parser.o',
                   'PrintTEX.hi',    'PrintTEX.o']),
      extra_run_opts('.')],
     multimod_compile_and_run,
     ['Main', '-cpp'])

