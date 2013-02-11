# Exhausts Hugs's heap (CAF leak)

test('barton-mangler-bug',
     [when(fast(), skip),
      extra_clean(['Basic.hi',             'Basic.o',
                   'Expected.hi',          'Expected.o',
                   'Main.hi',              'Main.o',
                   'Physical.hi',          'Physical.o',
                   'Plot.hi',              'Plot.o',
                   'PlotExample.hi',       'PlotExample.o',
                   'TypesettingTricks.hi', 'TypesettingTricks.o']),
      omit_compiler_types(['hugs'])],
     multimod_compile_and_run, ['Main', ''])

