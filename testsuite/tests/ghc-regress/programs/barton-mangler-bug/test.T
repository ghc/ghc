# Exhausts Hugs's heap (CAF leak)

test('barton-mangler-bug',
     [skip_if_fast,
      extra_clean(['Basic.hi',             'Basic.o',
                   'Expected.hi',          'Expected.o',
                   'Main.hi',              'Main.o',
                   'Physical.hi',          'Physical.o',
                   'Plot.hi',              'Plot.o',
                   'PlotExample.hi',       'PlotExample.o',
                   'TypesettingTricks.hi', 'TypesettingTricks.o']),
      omit_compiler_types(['hugs']),
      if_compiler_lt('ghc', '6.9', omit_ways(['profc']))],
     multimod_compile_and_run, ['Main', ''])

